#include "mpas_nuopc_macros.h"

#ifndef DEBUG
#define DEBUG .true.
#endif

module mpas_nuopc_fieldlist
  use esmf
  use nuopc
  use mpas_derived_types, only: domain_type
  use mpas_nuopc_field, only: field_class, new_field
  use mpas_nuopc_geom, only: mesh_class

  implicit none
  private

  type :: fieldlist_class
    type(field_class), allocatable :: fields(:)
    contains
      procedure :: initialize => fieldlist_initialize
      procedure :: advertise => fieldlist_advertise
      procedure :: realize => fieldlist_realize
      procedure :: finalize => fieldlist_finalize
      procedure :: data_initialize => fieldlist_data_initialize
      procedure :: dictionary_add => fieldlist_dictionary_add
      procedure :: print => fieldlist_print_list
      procedure :: print_stats => fieldlist_print_stats
  end type fieldlist_class

  interface new_fieldlist
    module procedure fieldlist_create_keys
    module procedure fieldlist_create_fields
  end interface new_fieldlist

  public :: fieldlist_class
  public :: new_fieldlist

contains

  !-----------------------------------------------------------------------
  ! print fieldlist
  subroutine fieldlist_print_list(self, writePet, rc)
    ! arguments
    class(fieldlist_class), intent(in) :: self
    integer, intent(in) :: writePet
    integer, intent(out) :: rc
    ! local variables
    type(ESMF_VM) :: vm
    integer :: localPet
    logical :: doWrite
    integer :: fld_i
    character(*), parameter :: fmt_hdr = "(A20,2(1X,A5),3(1X,A5))"
    character(*), parameter :: fmt_row = "(A20,2(1X,I5),3(1X,L5))"
    character(*), parameter :: method = "fieldlist_print_list"

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    ESMF_ERR_RETURN(rc)
    doWrite = (localPet == writePet)

    if (doWrite) then
      write(*, "(A)") "### Field List"
      write(*, fmt_hdr) "Name", "Rank", "Ungrd", "Init", "Adv", "Real"
      if (size(self%fields) == 0) then
        write(*, "(A)") "(fieldlist is empty)"
      end if
      ! print each field in the list
      do fld_i = 1, size(self%fields)
        write(*, fmt_row) &
          self%fields(fld_i)%key, &
          self%fields(fld_i)%rank, &
          self%fields(fld_i)%ungriddedDimCount, &
          self%fields(fld_i)%initialized, &
          self%fields(fld_i)%advertised, &
          self%fields(fld_i)%realized
      end do
    end if

  end subroutine fieldlist_print_list

  !-----------------------------------------------------------------------
  ! print field statistics
  subroutine fieldlist_print_stats(self, writePet, rc)
    ! arguments
    class(fieldlist_class), intent(in) :: self
    integer, intent(in) :: writePet
    integer, intent(out) :: rc
    ! local variables
    type(ESMF_VM) :: vm
    integer :: localPet
    logical :: doWrite
    integer :: fld_i
    integer(ESMF_KIND_I8) :: countv
    real(ESMF_KIND_R8) :: meanv, minv, maxv
    character(*), parameter :: fmt_hdr = "(A20,1(1X,A10),3(1X,A10  ))"
    character(*), parameter :: fmt_row = "(A20,1(1X,I10),3(1X,E10.3))"
    character(*), parameter :: method = "fieldlist_print_stats"

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    ESMF_ERR_RETURN(rc)
    doWrite = (localPet == writePet)
  
    if (doWrite) then
      write(*, "(A)") "### Field Statistic"
      write(*, fmt_hdr) "Name", "Count", "Min", "Max", "Mean"
      if (size(self%fields) == 0) then
        write(*, "(A)") "(fieldlist is empty)"
      end if
    end if
    ! print statistics for each field in the list
    do fld_i = 1, size(self%fields)
      call self%fields(fld_i)%stats(countv=countv, rootPet=writePet, &
        minv=minv, maxv=maxv, meanv=meanv, rc=rc)
      if (doWrite) write(*, fmt_row) &
        self%fields(fld_i)%key, &
        countv, &
        minv, &
        maxv, &
        meanv
    end do

  end subroutine fieldlist_print_stats

  !-----------------------------------------------------------------------
  ! initialize fieldlist
  subroutine fieldlist_initialize(self, domain, mesh, rc)
    ! arguments
    class(fieldlist_class), intent(inout) :: self
    type(domain_type), intent(in) :: domain
    type(mesh_class), intent(in) :: mesh
    integer, intent(out) :: rc
    ! local variables
    integer :: fld_i
    character(*), parameter :: method = "fieldlist_initialize"

    rc = ESMF_SUCCESS

    do fld_i = 1, size(self%fields)
      call self%fields(fld_i)%initialize(domain, mesh, rc=rc)
      ESMF_ERR_RETURN(rc)
    end do

  end subroutine fieldlist_initialize

  !-----------------------------------------------------------------------
  ! advertise fieldlist
  subroutine fieldlist_advertise(self, state, rc)
    ! arguments
    class(fieldlist_class), intent(inout) :: self
    type(ESMF_State), intent(inout) :: state
    integer, intent(out) :: rc
    ! local variables
    integer :: fld_i
    character(*), parameter :: method = "fieldlist_advertise"

    rc = ESMF_SUCCESS

    do fld_i = 1, size(self%fields)
      call self%fields(fld_i)%advertise(state, rc=rc)
      ESMF_ERR_RETURN(rc)
    end do

  end subroutine fieldlist_advertise

  !-----------------------------------------------------------------------
  ! realize the advertised fields
  subroutine fieldlist_realize(self, state, realizeAll, rc)
    ! arguments
    class(fieldlist_class), intent(inout) :: self
    type(ESMF_State), intent(inout) :: state
    logical, intent(in), optional :: realizeAll
    integer, intent(out) :: rc
    ! local variables
    integer :: fld_i
    logical :: do_realize
    logical :: local_realizeAll
    character(*), parameter :: method = "fieldlist_realize"

    rc = ESMF_SUCCESS

    if (present(realizeAll)) then
      local_realizeAll = realizeAll
    else
      local_realizeAll = .false.
    end if

    do fld_i=1, size(self%fields)

      if (.not. self%fields(fld_i)%advertised) then
        ! skip realization for fields that are not advertised
        self%fields(fld_i)%realized = .false.
        cycle
      else if (local_realizeAll) then
        ! realize all fields
        do_realize = .true.
      else
        ! realize if the field is connected
        do_realize = NUOPC_IsConnected( &
          state, &
          fieldName=self%fields(fld_i)%key, &
          rc=rc &
        )
        ESMF_ERR_RETURN(rc)
      end if

      ! realize field
      if (do_realize) then
        call self%fields(fld_i)%realize(state, rc=rc)
        ESMF_ERR_RETURN(rc)
      else
        call ESMF_StateRemove(state, (/self%fields(fld_i)%key/), rc=rc)
          ESMF_ERR_RETURN(rc)
        self%fields(fld_i)%realized = .false.
      end if
    end do

  end subroutine fieldlist_realize

  !-----------------------------------------------------------------------
  ! intialize field data in fieldlist
  subroutine fieldlist_data_initialize(self, clock, rc)
    ! arguments
    class(fieldlist_class), intent(inout) :: self
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc
    ! local variables
    type(ESMF_Time) :: currTime
    integer :: fld_i
    character(*), parameter :: method = "fieldlist_data_initialize"

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! initialize each field in the list
    do fld_i = 1, size(self%fields)
      call NUOPC_SetAttribute( &
        self%fields(fld_i)%efield, &
        name="Updated", &
        value="true", &
        rc=rc &
      )
      ESMF_ERR_RETURN(rc)
      call NUOPC_SetTimestamp( &
        self%fields(fld_i)%efield, &
        time=currTime, &
        rc=rc &
      )
      ESMF_ERR_RETURN(rc)
    end do

  end subroutine fieldlist_data_initialize

  !-----------------------------------------------------------------------
  ! finalize fieldlist
  subroutine fieldlist_finalize(self, rc)
    ! arguments
    class(fieldlist_class), intent(inout) :: self
    integer, intent(out) :: rc
    ! local variables
    integer :: fld_i
    character(*), parameter :: method = "fieldlist_finalize"

    rc = ESMF_SUCCESS

    do fld_i = 1, size(self%fields)
      call self%fields(fld_i)%destroy(rc=rc)
      ESMF_ERR_RETURN(rc)
    end do

    deallocate(self%fields)

  end subroutine fieldlist_finalize

  !-----------------------------------------------------------------------
  ! add entries to the NUOPC field dictionary
  subroutine fieldlist_dictionary_add(self, rc)
    ! arguments
    class(fieldlist_class), intent(inout) :: self
    integer, intent(out) :: rc
    ! local variables
    integer :: fld_i
    logical :: isPresent
    character(*), parameter :: method = "fieldlist_dictionary_add"

    rc = ESMF_SUCCESS

    do fld_i = 1, size(self%fields)
      isPresent = NUOPC_FieldDictionaryHasEntry( &
        self%fields(fld_i)%sd_name, rc=rc)
      ESMF_ERR_RETURN(rc)

      if (.not. isPresent) then
        call NUOPC_FieldDictionaryAddEntry( &
          StandardName=self%fields(fld_i)%sd_name, &
          canonicalUnits=self%fields(fld_i)%units, &
          rc=rc)
        ESMF_ERR_RETURN(rc)
      end if
    end do

  end subroutine fieldlist_dictionary_add

  !-----------------------------------------------------------------------
  ! FieldList Constructors
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! create fieldlist from keys and standard names
  pure type(fieldlist_class) function fieldlist_create_keys(keys, sd_names) result(fieldlist)
    character(*), intent(in) :: keys(:)
    character(*), intent(in) :: sd_names(:)
    ! local variables
    integer :: fld_i

    allocate(fieldlist%fields(size(keys)))
    do fld_i = 1, size(keys)
      fieldlist%fields(fld_i) = &
        new_field(keys(fld_i), sd_name=sd_names(fld_i))
    end do
  end function fieldlist_create_keys

  !-----------------------------------------------------------------------
  ! create fieldlist from list of mpas_nuopc_field objects
  type(fieldlist_class) function fieldlist_create_fields(fields) result(fieldlist)
    type(field_class), intent(in) :: fields(:)
    ! local variables
    integer :: fld_i

    allocate(fieldlist%fields(size(fields)))
    do fld_i = 1, size(fields)
      fieldlist%fields(fld_i) = fields(fld_i)
    end do
  end function fieldlist_create_fields

end module mpas_nuopc_fieldlist
