#include "mpas_nuopc_macros.h"

#ifndef DEBUG
#define DEBUG .true.
#endif

module mpas_nuopc_field
  use esmf
  use nuopc
  use mpas_nuopc_geom, only: mesh_class
  use mpas_derived_types, only: RKIND
  use mpas_derived_types, only: domain_type
  use mpas_derived_types, only: mpas_pool_type
  use mpas_pool_routines, only: mpas_pool_get_subpool
  use mpas_pool_routines, only: mpas_pool_get_array
  use mpas_pool_routines, only: mpas_pool_get_dimension

  implicit none
  private

  type :: field_spec
    character(len=64) :: key
    character(len=64) :: subpool
    character(len=64) :: variable
    character(len=32) :: units
    logical           :: ungridded
  end type field_spec

  type(field_spec), parameter :: field_specs(*) = [ &
    field_spec("sfc_input_tslb", "sfc_input", "tslb", "K", .true.), &
    field_spec("diag_physics_t2m", "diag_physics", "t2m", "K", .false.), &
    field_spec("diag_physics_q2", "diag_physics", "q2", "kg kg-1", .false.), &
    field_spec("diag_physics_swdnb", "diag_physics", "swdnb", "W m-2", .false.), &
    field_spec("diag_physics_lwdnb", "diag_physics", "lwdnb", "W m-2", .false.), &
    field_spec("diag_physics_u10", "diag_physics", "u10", "m s-1", .false.), &
    field_spec("diag_physics_v10", "diag_physics", "v10", "m s-1", .false.), &
    field_spec("diag_surface_pressure", "diag", "surface_pressure", "Pa", .false.), &
    field_spec("diag_physics_ch", "diag_physics", "ch", "m s-1", .false.), &
    field_spec("diag_physics_chs2", "diag_physics", "chs2", "m s-1", .false.), &
    field_spec("diag_physics_cqs2", "diag_physics", "cqs2", "m s-1", .false.), &
    field_spec("diag_physics_sfc_emiss", "diag_physics", "sfc_emiss", "1", .false.), &
    field_spec("diag_physics_coszr", "diag_physics", "coszr", "1", .false.), &
    field_spec("sfc_input_xice", "sfc_input", "xice", "1", .false.), &
    field_spec("sfc_input_tmn", "sfc_input", "tmn", "K", .false.), &
    field_spec("diag_physics_rainncv", "diag_physics", "rainncv", "mm", .false.), &
    field_spec("diag_physics_raincv", "diag_physics", "raincv", "mm", .false.) &
  ]

  type, abstract :: field_data
  end type field_data

  type, extends(field_data) :: field_data_D1
    real(RKIND), pointer :: ptrD1(:) => null()
  end type field_data_D1

  type, extends(field_data) :: field_data_D2
    real(RKIND), pointer :: ptrD2(:,:) => null()
  end type field_data_D2

  type :: field_class
    character(len=64) :: key = ""
    character(len=ESMF_MAXSTR) :: sd_name = ""
    character(len=64) :: units = ""
    type(ESMF_TypeKind_Flag) :: typekind = ESMF_TYPEKIND_R4
    integer :: rank = 0
    integer :: ungriddedDimCount = 0
    logical :: advertised = .false.
    logical :: realized = .false.
    logical :: initialized = .false.
    type(ESMF_Field) :: efield
    class(field_data), allocatable :: data
    contains
      procedure :: initialize => field_initialize
      procedure :: advertise => field_advertise
      procedure :: realize => field_realize
      procedure :: destroy => field_destroy
      procedure :: stats => field_stats
      procedure :: set_ptr => field_set_ptr
  end type field_class

  interface new_field
  module procedure field_create
end interface new_field

  public :: field_class
  public :: new_field

contains

  !-----------------------------------------------------------------------
  ! index of key in field_specs, or 0 if not found
  pure integer function field_spec_index(key) result(index)
    character(*), intent(in) :: key
    integer :: i

    index = 0
    do i = 1, size(field_specs)
      if (trim(field_specs(i)%key) == trim(key)) then
        index = i
        return
      end if
    end do
  end function field_spec_index

  !-----------------------------------------------------------------------
  ! create ESMF_Field using MPAS pointer
  type(ESMF_Field) function create_efield_mpas_ptr(idx, domain, mesh, rc) result(esmffield)
    ! arguments
    integer, intent(in) :: idx
    type(domain_type), intent(in) :: domain
    type(mesh_class), intent(in) :: mesh
    integer, intent(out) :: rc
    ! local variables
    integer, pointer :: nCellsSolve
    type(mpas_pool_type), pointer :: subpool
    real(kind=RKIND), pointer :: ptrD1(:)
    real(kind=RKIND), pointer :: ptrD1_owned(:)
    real(kind=RKIND), pointer :: ptrD2(:,:)
    real(kind=RKIND), pointer :: ptrD2_owned(:,:)
    character(*), parameter :: method = "create_efield_mpas_ptr"

    ! fetch solver dimension and subpool pointers
    nullify(nCellsSolve)
    call mpas_pool_get_dimension(domain%blocklist%dimensions, 'nCellsSolve', nCellsSolve)
    if (.not. associated(nCellsSolve)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Failed to find nCellsSolve dimension", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if
    nullify(subpool)
    call mpas_pool_get_subpool(domain%blocklist%structs, trim(field_specs(idx)%subpool), subpool)
    if (.not. associated(subpool)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Failed to find subpool: "//trim(field_specs(idx)%subpool), &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    if (field_specs(idx)%ungridded) then

      ! create 2-dimensional field with ungridded layers
      nullify(ptrD2)
      call mpas_pool_get_array(subpool, trim(field_specs(idx)%variable), ptrD2)
      if (.not. associated(ptrD2)) then
        call ESMF_LogSetError( &
          ESMF_RC_OBJ_INIT, &
          msg="Failed to find: "//trim(field_specs(idx)%key), &
          line=__LINE__, &
          file=__FILE__, &
          method=method, &
          rcToReturn=rc &
        )
        return
      end if
      ptrD2_owned => ptrD2(:, 1:nCellsSolve)
      esmffield = ESMF_FieldCreate( &
        mesh=mesh%esmfmesh, &
        farrayPtr=ptrD2_owned, &
        meshloc=ESMF_MESHLOC_ELEMENT, &
        gridToFieldMap=(/2/), &
        name=field_specs(idx)%key, &
        rc=rc &
      )
      ESMF_ERR_RETURN(rc)

    else

      ! create 1-dimensional field
      nullify(ptrD1)
      call mpas_pool_get_array(subpool, trim(field_specs(idx)%variable), ptrD1)
      if (.not. associated(ptrD1)) then
        call ESMF_LogSetError( &
          ESMF_RC_OBJ_INIT, &
          msg="Failed to find: "//trim(field_specs(idx)%key), &
          line=__LINE__, &
          file=__FILE__, &
          method=method, &
          rcToReturn=rc &
        )
        return
      end if
      ptrD1_owned => ptrD1(1:nCellsSolve)
      esmffield = ESMF_FieldCreate( &
        mesh=mesh%esmfmesh, &
        farrayPtr=ptrD1_owned, &
        meshloc=ESMF_MESHLOC_ELEMENT, &
        name=field_specs(idx)%key, &
        rc=rc &
      )
      ESMF_ERR_RETURN(rc)

    end if

  end function create_efield_mpas_ptr

  !-----------------------------------------------------------------------
  ! initialize field
  subroutine field_initialize(self, domain, mesh, rc)
    ! arguments
    class(field_class), intent(inout) :: self
    type(domain_type), intent(in) :: domain
    type(mesh_class), intent(in) :: mesh
    integer, intent(out) :: rc
    ! local variables
    integer :: idx
    logical :: check
    character(*), parameter :: method = "field_initialize"

    ! get index of field spec for this key
    idx = field_spec_index(self%key)
    if (idx == 0) then
      call ESMF_LogSetError( &
        ESMF_RC_NOT_FOUND, &
        msg="MPAS field spec not found for key: "//trim(self%key), &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    ! set variable metadata
    self%units = trim(field_specs(idx)%units)
    if (RKIND == ESMF_KIND_R4) then
      self%typekind = ESMF_TYPEKIND_R4
    else if (RKIND == ESMF_KIND_R8) then
      self%typekind = ESMF_TYPEKIND_R8
    end if

    ! check if field is already created
    check = ESMF_FieldIsCreated(self%efield, rc=rc)
    ESMF_ERR_RETURN(rc)
    if (check) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Field already created: "//trim(self%key), &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    self%efield = create_efield_mpas_ptr(idx, domain, mesh, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_FieldGet( &
      field=self%efield, &
      rank=self%rank, &
      ungriddedDimCount=self%ungriddedDimCount, &
      rc=rc &
    )
    ESMF_ERR_RETURN(rc)

    ! set the pointer to the allocated data
    call self%set_ptr(rc=rc)
    ESMF_ERR_RETURN(rc)

    self%initialized = .true.
  end subroutine field_initialize

  !-----------------------------------------------------------------------
  ! destroy field instance - release memory
  subroutine field_destroy(self, rc)
    ! arguments
    class(field_class), intent(inout) :: self
    integer, intent(out) :: rc
    ! local variables
    logical :: is_created
    integer :: stat
    character(*), parameter :: method = "field_destroy"

    rc = ESMF_SUCCESS

    ! realized fields are owned by NUOPC state and
    ! should not be destroyed here
    if (.not. self%realized) then
      is_created = ESMF_FieldIsCreated(self%efield, rc=rc)
      ESMF_ERR_RETURN(rc)
      if (is_created) then
        call ESMF_FieldDestroy(self%efield, rc=rc)
        ESMF_ERR_RETURN(rc)
      end if
    end if

    if (allocated(self%data)) then
      deallocate(self%data, stat=stat)
      if (ESMF_LogFoundDeallocError( &
        statusToCheck=stat, &
        msg="Error deallocating field data", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc )) return
    end if
    self%advertised = .false.
    self%realized = .false.
    self%initialized = .false.

  end subroutine field_destroy

  !-----------------------------------------------------------------------
  ! set the  field pointer for the given MPAS field
  subroutine field_set_ptr(self, rc)
    ! arguments
    class(field_class), intent(inout) :: self
    integer, intent(out) :: rc
    ! local variables
    character(*), parameter :: method = "field_set_ptr"

    rc = ESMF_SUCCESS

    if (allocated(self%data)) deallocate(self%data)
    if (self%ungriddedDimCount == 0) then
      allocate(field_data_D1 :: self%data)
    else if (self%ungriddedDimCount == 1) then
      allocate(field_data_D2 :: self%data)
    else
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Unsupported ungriddedDimCount: "//trim(self%key), &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    ! set the pointer to the field data
    select type (data => self%data)
      type is (field_data_D1)
        call ESMF_FieldGet( &
          field=self%efield, &
          farrayPtr=data%ptrD1, &
          rc=rc &
        )
        ESMF_ERR_RETURN(rc)
      type is (field_data_D2)
        call ESMF_FieldGet( &
          field=self%efield, &
          farrayPtr=data%ptrD2, &
          rc=rc &
        )
        ESMF_ERR_RETURN(rc)
      class default
        call ESMF_LogSetError( &
          ESMF_RC_OBJ_INIT, &
          msg="Unsupported field data kind: "//trim(self%key), &
          line=__LINE__, &
          file=__FILE__, &
          method=method, &
          rcToReturn=rc &
        )
        return
      end select

  end subroutine field_set_ptr

  !-----------------------------------------------------------------------
  ! advertise field
  subroutine field_advertise(self, state, rc)
    ! arguments
    class(field_class), intent(inout) :: self
    type(ESMF_State), intent(inout) :: state
    integer, intent(out) :: rc
    ! local variables
    character(*), parameter :: method = "field_advertise"

    rc = ESMF_SUCCESS

    ! advertise the field with the NUOPC framework
    call NUOPC_Advertise(state, &
      standardname = self%sd_name, &
      units = self%units, &
      name = self%key, &
      rc = rc)
    ESMF_ERR_RETURN(rc)

    ! mark the field as advertised
    self%advertised = .true.

  end subroutine field_advertise

  !-----------------------------------------------------------------------
  ! Realize field in ESMF state
  subroutine field_realize(self, state, rc)
    ! arguments
    class(field_class), intent(inout) :: self
    type(ESMF_State), intent(inout) :: state
    integer, intent(out) :: rc
    ! local variables
    logical :: check_int
    character(*), parameter :: method = "field_realize"

    rc = ESMF_SUCCESS

    ! check if esmf field is created
    check_int = ESMF_FieldIsCreated(self%efield, rc=rc)
    ESMF_ERR_RETURN(rc)
    if (.not. check_int) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT,  &
        msg="Field is not created: "//trim(self%key), &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    ! realize field in the ESMF state
    call NUOPC_Realize(state, field=self%efield, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! mark the field as realized
    self%realized = .true.

  end subroutine field_realize

  !-----------------------------------------------------------------------
  ! compute field data statistics (count, min, max, mean)
  subroutine field_stats(self, rootPet, countv, minv, maxv, meanv, rc)
    class(field_class), intent(in) :: self
    integer, intent(in) :: rootPet
    integer(ESMF_KIND_I8), intent(out) :: countv
    real(ESMF_KIND_R8), intent(out) :: minv, maxv, meanv
    integer, intent(out) :: rc
    ! local variables
    type(ESMF_VM) :: vm
    logical :: has_data
    integer(ESMF_KIND_I8) :: localCount(1), globalCount(1)
    real(ESMF_KIND_R8) :: localSum(1), globalSum(1)
    real(ESMF_KIND_R8) :: localMin(1), globalMin(1)
    real(ESMF_KIND_R8) :: localMax(1), globalMax(1)
    character(*), parameter :: method = "field_stats"

    rc = ESMF_SUCCESS

    countv = 0_ESMF_KIND_I8
    minv = huge(0._ESMF_KIND_R8)
    maxv = -huge(0._ESMF_KIND_R8)
    meanv = 0._ESMF_KIND_R8

    if (.not. allocated(self%data)) then
      call ESMF_LogSetError( &
        ESMF_RC_ARG_BAD, &
        msg="Field data not allocated: "//trim(self%key), &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    localCount(1) = 0
    localSum(1) = 0._ESMF_KIND_R8
    localMin(1) = huge(0._ESMF_KIND_R8)
    localMax(1) = -huge(0._ESMF_KIND_R8)

    ! local data reductions
    select type (data => self%data)
      type is (field_data_D1)
        has_data = associated(data%ptrD1)
        if (has_data) then
          localCount(1) = size(data%ptrD1, kind=ESMF_KIND_I8)
          localSum(1) = sum(real(data%ptrD1, ESMF_KIND_R8))
          localMin(1) = real(minval(data%ptrD1), ESMF_KIND_R8)
          localMax(1) = real(maxval(data%ptrD1), ESMF_KIND_R8)
        end if
      type is (field_data_D2)
        has_data = associated(data%ptrD2)
        if (has_data) then
          localCount(1) = size(data%ptrD2, kind=ESMF_KIND_I8)
          localSum(1) = sum(real(data%ptrD2, ESMF_KIND_R8))
          localMin(1) = real(minval(data%ptrD2), ESMF_KIND_R8)
          localMax(1) = real(maxval(data%ptrD2), ESMF_KIND_R8)
        end if
      class default
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Unsupported field data kind: "//trim(self%key), &
          line=__LINE__, file=__FILE__, method=method, rcToReturn=rc)
        return
    end select

    if (.not. has_data) then
      call ESMF_LogSetError( &
        ESMF_RC_PTR_NOTALLOC, &
        msg="Field data pointer not associated: "//trim(self%key), &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    ! global reductions
    call ESMF_FieldGet(field=self%efield, vm=vm, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_VMReduce(vm, sendData=localCount, recvData=globalCount, &
      count=1, reduceflag=ESMF_REDUCE_SUM, rootPet=rootPet, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMReduce(vm, sendData=localSum, recvData=globalSum, &
      count=1, reduceflag=ESMF_REDUCE_SUM, rootPet=rootPet, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMReduce(vm, sendData=localMin, recvData=globalMin, &
      count=1, reduceflag=ESMF_REDUCE_MIN, rootPet=rootPet, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMReduce(vm, sendData=localMax, recvData=globalMax, &
      count=1, reduceflag=ESMF_REDUCE_MAX, rootPet=rootPet, rc=rc)
    ESMF_ERR_RETURN(rc)

    countv = globalCount(1)
    minv = globalMin(1)
    maxv = globalMax(1)
    if (globalCount(1) > 0) meanv = globalSum(1) / real(globalCount(1), ESMF_KIND_R8)

  end subroutine field_stats

  !-----------------------------------------------------------------------
  ! Field Constructors
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! create field
  pure type(field_class) function field_create(key, sd_name) result(field)
    ! arguments
    character(*), intent(in) :: key
    character(*), intent(in) :: sd_name

    field%key = key
    field%sd_name = sd_name
  end function field_create

end module mpas_nuopc_field
