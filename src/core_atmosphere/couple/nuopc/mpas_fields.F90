module mpas_nuopc_fields
  use mpas_nuopc_utils, only: check, create_esmf_mesh
  use esmf
  use nuopc
  use nuopc_model, only: nuopc_modelget
  use mpas_io, only: MPAS_REAL_FILLVAL
  implicit none

  type cap_field_t
    sequence
    character(len=64)           :: sd_name   = "dummy" ! standard name
    character(len=64)           :: st_name   = "dummy" ! state name
    character(len=64)           :: units     = "-"     ! units
    logical                     :: ad_import = .FALSE. ! advertise import
    logical                     :: ad_export = .FALSE. ! advertise export
    real(ESMF_KIND_R8)          :: vl_fillv  = MPAS_REAL_FILLVAL ! default
    logical                     :: rl_import = .FALSE. ! realize import
    logical                     :: rl_export = .FALSE. ! realize export
  end type cap_field_t

  type memory_flag
     integer :: mem
  end type memory_flag

  ! integer, parameter :: num_field_vars = 20
  ! type(cap_field_t), target, dimension(num_field_vars) :: field_list
  type(cap_field_t), target, dimension(:), allocatable :: field_list
  logical :: initialized = .false.
  logical, parameter :: IMPORT_T = .true.
  logical, parameter :: IMPORT_F = .false.
  logical, parameter :: EXPORT_T = .true.
  logical, parameter :: EXPORT_F = .false.
  logical, parameter :: TMP_EXPORT_T = .false.
  logical, parameter :: TMP_EXPORT_TT = .false.
  logical, parameter :: TMP_IMPORT_T = .false.
  logical, parameter :: TMP_IMPORT_Q = .false.

  type(ESMF_Mesh) :: mesh_esmf
  logical :: mesh_esmf_initialized = .false.

contains

  function get_field_list(mpas) result(res)
    type(logical), intent(in), optional :: mpas
    type(cap_field_t), allocatable :: res(:)
    if (.not. initialized) then
       call field_init(mpas)
       initialized = .true.
    end if
    res = field_list
  end function get_field_list

  subroutine add_field_dictionary(fieldList, rc)
    type(cap_field_t), intent(in) :: fieldList(:)
    integer :: rc
    ! local variables
    integer :: n
    logical :: isPresent
    character(:), allocatable :: file
    file = __FILE__

    rc = ESMF_SUCCESS
    do n = lbound(fieldList,1), ubound(fieldList,1)
       isPresent = NUOPC_FieldDictionaryHasEntry( &
            fieldList(n)%sd_name, rc=rc)
       if (check(rc, __LINE__, file)) return

       if (.not.isPresent) then
          call NUOPC_FieldDictionaryAddEntry( &
               StandardName=trim(fieldList(n)%sd_name), &
               canonicalUnits=trim(fieldList(n)%units), &
               rc=rc)
          if (check(rc, __LINE__, file)) return
       end if
       ! print *, "field st name: ", trim(fieldList(n)%st_name)
    end do

  print *, "FIELD DICTIONARY ADDED"
  end subroutine add_field_dictionary


  subroutine field_init(mpas)
    type(logical), intent(in), optional :: mpas
    field_list = [ &
      add_field("inst_total_soil_moisture_content","smc", "m3 m-3", &
        TMP_IMPORT_T, TMP_EXPORT_T, 0.20d0), &
      add_field("inst_soil_moisture_content","slc", "m3 m-3", &
        TMP_IMPORT_T, TMP_EXPORT_T, 0.20d0), &
      add_field("inst_soil_temperature","stc", "K", &
        TMP_IMPORT_T, EXPORT_F, 288.d0), &
      add_field("liquid_fraction_of_soil_moisture_layer_1","sh2ox1", "m3 m-3", &
        IMPORT_T, EXPORT_T, 0.20d0), &
      add_field("liquid_fraction_of_soil_moisture_layer_2","sh2ox2", "m3 m-3", &
        IMPORT_T, EXPORT_T, 0.20d0), &
      add_field("liquid_fraction_of_soil_moisture_layer_3","sh2ox3", "m3 m-3", &
        IMPORT_T, EXPORT_T, 0.20d0), &
      add_field("liquid_fraction_of_soil_moisture_layer_4","sh2ox4", "m3 m-3", &
        IMPORT_T, EXPORT_T, 0.20d0), &
      add_field("soil_moisture_fraction_layer_1","smc1", "m3 m-3", &
        IMPORT_T, EXPORT_T, 0.20d0), &
      add_field("soil_moisture_fraction_layer_2","smc2", "m3 m-3", &
        IMPORT_T, EXPORT_T, 0.20d0), &
      add_field("soil_moisture_fraction_layer_3","smc3", "m3 m-3", &
        IMPORT_T, EXPORT_T, 0.20d0), &
      add_field("soil_moisture_fraction_layer_4","smc4", "m3 m-3", &
        IMPORT_T, EXPORT_T, 0.20d0), &
      add_field("soil_temperature_layer_1","stc1", "K", &
        IMPORT_T, EXPORT_T, 288.d0), &
      add_field("soil_temperature_layer_2","stc2", "K", &
        IMPORT_T, EXPORT_T, 288.d0), &
      add_field("soil_temperature_layer_3","stc3", "K", &
        IMPORT_T, EXPORT_T, 288.d0), &
      add_field("soil_temperature_layer_4","stc4", "K", &
        IMPORT_T, EXPORT_T, 288.d0), &
      add_field("soil_porosity","smcmax1", "1", &
        IMPORT_F, EXPORT_F, 0.45d0), &
      add_field("vegetation_type","vegtyp", "1", &
        IMPORT_F, EXPORT_F, 16.0d0), &
      add_field("surface_water_depth","sfchead", "mm", &
        IMPORT_F, EXPORT_F, 0.00d0), &
      add_field("time_step_infiltration_excess","infxsrt", "mm", &
        IMPORT_T, EXPORT_T, 0.00d0), &
      add_field("soil_column_drainage","soldrain", "mm", &
        IMPORT_T, EXPORT_T, 0.00d0) &
      ! ,add_field("surface_runoff_accumulated","sfcrunoff", "mm", &
      !   TMP_IMPORT_Q, EXPORT_T, 0.00d0), &
      ! add_field("subsurface_runoff_accumulated","udrunoff", "mm", &
      !   TMP_IMPORT_Q, EXPORT_T, 0.00d0) &
        ]

    ! add NoahMP Variables that MPAS provides
    ! if (present(mpas)) then
    !    if (mpas .eqv. .true.) then
    !       field_list = [field_list, &
    !            add_field("surface_runoff_accumulated","sfcrunoff", "mm", &
    !              TMP_IMPORT_Q, EXPORT_T, 0.00d0), &
    !            add_field("subsurface_runoff_accumulated","udrunoff", "mm", &
    !              TMP_IMPORT_Q, EXPORT_F, 0.00d0) &
    !            ]
    !    end if
    ! end if
  end subroutine field_init

  pure function add_field(sd_name, st_name, units, ad_import, ad_export, vl_fillv) result(f)
    character(len=*), intent(in) :: sd_name, st_name, units
    logical, intent(in)         :: ad_import, ad_export
    real(ESMF_KIND_R8), intent(in) :: vl_fillv
    type(cap_field_t) :: f
    f%sd_name   = sd_name
    f%st_name   = st_name
    f%units     = units
    f%ad_import = ad_import
    f%ad_export = ad_export
    f%vl_fillv  = vl_fillv
  end function add_field




!   subroutine advertise_fields_foo(domain)
!     use mpas_atmphys_vars, only: mpas_noahmp
!     use mpas_derived_types, only: domain_type, block_type
!     type(domain_type), pointer, intent(inout) :: domain
!     type(block_type), pointer :: block
!     real, pointer :: var(:)

!     type(ESMF_State)        :: importState, exportStaet

! ! From Noah-MP cap:
! ! Export from land:
! !   call state_setexport_1d(exportState, 'Sl_sfrac', noahmp%model%sncovr1, rc=rc) ! snow cover over land [fraction]
! !   call state_setexport_1d(exportState, 'Fall_lat', noahmp%model%evap, rc=rc) ! ! total latent heat flux [W/m2]
! !   call state_setexport_1d(exportState, 'Fall_sen', noahmp%model%hflx, rc=rc) ! ! sensible heat flux [W/m2]
! !   call state_setexport_1d(exportState, 'Fall_evap', noahmp%model%ep, rc=rc) ! ! potential evaporation [mm/s?]
! !   call state_setexport_1d(exportState, 'Sl_tref', noahmp%model%t2mmp, rc=rc) ! ! combined T2m from tiles
! !   call state_setexport_1d(exportState, 'Sl_qref', noahmp%model%q2mp, rc=rc) ! ! combined q2m from tiles
! !   call state_setexport_1d(exportState, 'Sl_q', noahmp%model%qsurf, rc=rc) ! ! specific humidity at sfc [kg/kg]
! !   call state_setexport_1d(exportState, 'Fall_gflx', noahmp%model%gflux, rc=rc) ! !  soil heat flux [W/m2]
! !   call state_setexport_1d(exportState, 'Fall_roff', noahmp%model%runoff, rc=rc) ! ! surface runoff [mm/s]
! !   call state_setexport_1d(exportState, 'Fall_soff', noahmp%model%drain, rc=rc) ! ! subsurface runoff [mm/s]
! !   call state_setexport_1d(exportState, 'Sl_cmm', noahmp%model%cmm, rc=rc) ! ! ! cm*U [m/s]
! !   call state_setexport_1d(exportState, 'Sl_chh', noahmp%model%chh, rc=rc) ! ! ch*U*rho [kg/m2/s]
! !   call state_setexport_1d(exportState, 'Sl_zvfun', noahmp%model%zvfun, rc=rc) !
! ! Export from land to mediator:
! !   call state_setexport_1d(exportState, 'Sl_lfrin', noahmp%domain%frac, rc=rc)

!     ! Fields in NoahMP
!     ! noahmp%water%flux%RunoffSurface ! subsurface runoff

!     !-------------------------------------------------------------------
!     ! NoahMP Variables
!     ! snowc               ! snow cover fraction []
!     !-------------------------------------------------------------------

!     ! check variables
!     call check_var(mpas_noahmp%snowc, "snowc")
!     ! call check_var(mpas_noahmp%qfx, "qfc latent heat flux")
!     ! call check_var(mpas_noahmp%lh, "lh latent heat flux")
!     ! call check_var(mpas_noahmp%xland, "xland")




!     ! query for exportState
!     ! exportable field: sea_surface_temperature
!     ! call NUOPC_Advertise(exportState, &
!     !      StandardName="sea_surface_temperature", name="sst", rc=rc)
!     ! if (check(rc, __LINE__, file)) return
!     ! ! exportable field: sea_surface_salinity
!     ! call NUOPC_Advertise(exportState, &
!     !      StandardName="sea_surface_salinity", name="sss", rc=rc)
!     ! if (check(rc, __LINE__, file)) return
!     ! ! exportable field: sea_surface_height_above_sea_level
!     ! call NUOPC_Advertise(exportState, &
!     !      StandardName="sea_surface_height_above_sea_level", &
!     !      name="ssh", rc=rc)
!     ! if (check(rc, __LINE__, file)) return

!   end subroutine advertise_fields_foo



  ! Allocate the data storage for the advertised variables in component’s
  ! ESMF_State (either import or export) so you can read/write values
  subroutine realize_fields(model, domain, fieldList, importState, &
       exportState, realizeAllImport, realizeAllExport, rc)
  ! subroutine realize_fields(fieldList, importState, exportState, grid, &
  ! did, realizeAllImport, realizeAllExport, memr_import, memr_export, rc)
    use mpas_derived_types, only: domain_type
    use mpas_atmphys_vars, only: mpas_noahmp, smois_p

    ! use mpas_atm_dimensions, only: nVertLevels, maxEdges, maxEdges2, num_scalars
    type(ESMF_GridComp), intent(inout) :: model
    type(domain_type), intent(in)    :: domain
    type(cap_field_t), intent(inout) :: fieldList(:)
    type(ESMF_State), intent(inout)   :: importState
    type(ESMF_State), intent(inout)   :: exportState
    integer, intent(out)              :: rc
    ! type(ESMF_Grid), intent(in)       :: grid
    ! integer, intent(in)               :: did
    logical, intent(in)               :: realizeAllImport
    logical, intent(in)               :: realizeAllExport
    ! type(memory_flag)                 :: memr_import
    ! type(memory_flag)                 :: memr_export

    ! local variables
    integer :: n
    logical :: realizeImport
    logical :: realizeExport
    type(ESMF_Field) :: import_field, export_field
    ! type(ESMF_Grid) :: grid


    character(:), allocatable :: file
    real(ESMF_KIND_R8), pointer :: fptr(:)
    integer :: numElements, numNodes
    integer, parameter :: did = 0
    ! integer, pointer :: nSoilLevels
    integer :: nSoilLevels
    file = __FILE__
    rc = ESMF_SUCCESS

    print *, "MPAS: entering realize fields"
    if (did /= 0) error stop "Not prepared for parallel yet"

    if (.not. mesh_esmf_initialized) then
       ! create ESMF mesh from MPAS mesh
       mesh_esmf = create_esmf_mesh(domain)
       mesh_esmf_initialized = .true.
    else
       stop "GOOD TO KNOW"
    end if

    ! ! Read in mesh rather than create it, just easier
    ! mesh_file = "x1.40962.esmf.nc"
    ! mesh_esmf = ESMF_MeshCreate(filename=mesh_file,
    ! fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    ! if (check(rc, __LINE__, file)) return
    ! print *, "Read in mesh file: ", mesh_file
    ! call ESMF_LogWrite("Read in " // mesh_file, ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_MeshGet(mesh_esmf, nodeCount=numNodes, &
         elementCount=numElements, rc=rc)
    print *, "Mesh element count: ", numElements
    print *, "Mesh node count: ", numNodes


    ! stop "halt and catch fire"

    ! ! Create Fields
    ! import_field = ESMF_FieldCreate(mesh_esmf, typekind=ESMF_TYPEKIND_R8, &
    !      meshloc=ESMF_MESHLOC_ELEMENT, name='temperature', rc=rc)
    ! if (check(rc, __LINE__, file)) return
    ! print *, "Fields created"
    ! ! Access pointer to underlying data array
    ! call ESMF_FieldGet(import_field, farrayPtr=fptr, rc=rc)
    ! nSoilLevels = 4
    ! ! Initialize field values
    ! fptr = 300.0d0   ! e.g., initialize temperature field to 300K

    ! Create field bundle

    ! windField = ESMF_FieldCreate(mpas_mesh, typekind=ESMF_TYPEKIND_R8, name='wind', rc=rc)
    ! ! Create FieldBundle and add Fields
    ! call ESMF_FieldBundleCreate(stateBundle, mpas_mesh, rc=rc)
    ! call ESMF_FieldBundleAdd(stateBundle, (/tempField, windField/), rc=rc)


    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (check(rc, __LINE__, file)) return

    call ESMF_StateLog(importState, logMsgFlag=ESMF_LOGMSG_INFO, rc=rc)
    call ESMF_StateLog(exportState, logMsgFlag=ESMF_LOGMSG_INFO, rc=rc)
    ! stop "MPAS: in realize printing states"

    do n=lbound(fieldList,1), ubound(fieldList,1)
      ! print *, "field ", trim(fieldList(n)%st_name), &
      !      "import ", fieldList(n)%ad_import

       ! check realize import
      if (fieldList(n)%ad_import) then
        if (realizeAllImport) then
          realizeImport = .true.
        else
          realizeImport = NUOPC_IsConnected(importState, &
               fieldName=trim(fieldList(n)%st_name), rc=rc)
          print*, "import field ", trim(fieldList(n)%st_name), " realized", &
               realizeImport
          if (check(rc, __LINE__, file)) return
        end if
      else
        realizeImport = .false.
      end if

      ! create import field
      if ( realizeImport ) then
         ! print*, "MPAS: importing field ", fieldList(n)%st_name
         ! import_field = field_create(fld_name=fieldList(n)%st_name, &
         !      grid=grid, did=did, memflg=memr_import, rc=rc)
         import_field = field_create(domain, fieldList(n)%st_name, &
             mesh_esmf, did, nSoilLevels, rc=rc)
         if (check(rc, __LINE__, file)) return

         call NUOPC_Realize(importState, field=import_field, rc=rc)
         if (check(rc, __LINE__, file)) return
         ! print*, "MPAS: importing field realized ", fieldList(n)%st_name
         fieldList(n)%rl_import = .true.
      else
         call ESMF_StateRemove(importState, (/fieldList(n)%st_name/), &
              relaxedflag=.true., rc=rc)
         if (check(rc, __LINE__, file)) return
         fieldList(n)%rl_import = .false.
      end if


      ! print *, "MPAS: export field ", trim(fieldList(n)%st_name), &
      !      " export ", fieldList(n)%ad_export
      ! --- exports ---
      ! check realize export
      if (fieldList(n)%ad_export) then
         ! print *, "MPAS: export field ", trim(fieldList(n)%st_name), &
         !   " export ", fieldList(n)%ad_export

         if (realizeAllExport) then
            realizeExport = .true.
         else
            realizeExport = NUOPC_IsConnected(exportState, &
                 fieldName = trim(fieldList(n)%st_name),rc=rc)
            if (check(rc, __LINE__, file)) return
         end if
      else
        realizeExport = .false.
      end if


      ! create export field
      if (realizeExport) then
        export_field = field_create(domain, fieldList(n)%st_name, &
             mesh_esmf, did, nSoilLevels, rc=rc)
             ! mesh, did, memr_export, rc=rc)
        if (check(rc, __LINE__, file)) return

        call NUOPC_Realize(exportState, field=export_field, rc=rc)
        if (check(rc, __LINE__, file)) return

        fieldList(n)%rl_export = .true.
        ! print*, "MPAS: realize exporting field ", fieldList(n)%st_name
        call ESMF_LogWrite("MPAS: exporting " // fieldList(n)%st_name, ESMF_LOGMSG_INFO, rc=rc)
      else
         ! print *, "MPAS: realize remove export ", trim(fieldList(n)%st_name),&
         !      " ---------------------------"
        call ESMF_StateRemove(exportState, (/fieldList(n)%st_name/), &
          relaxedflag=.true., rc=rc)
        if (check(rc, __LINE__, file)) return

        fieldList(n)%rl_export = .false.
      end if
    end do

    print *, "MPAS: exiting realize fields"
  end subroutine realize_fields


  ! Advertise the variables that can be provided or are needed
  subroutine advertise_fields(model,fieldList, importState, exportState, &
       transferOffer, rc)
    use NUOPC_Model, only : NUOPC_ModelGet
    type(ESMF_GridComp), intent(inout) :: model
    type(cap_field_t), intent(in) :: fieldList(:)
    type(ESMF_State), intent(inout) :: importState
    type(ESMF_State), intent(inout) :: exportState
    character(*), intent(in),optional :: transferOffer
    integer, intent(out) :: rc
    ! local variables
    character(:), allocatable :: file
    integer :: i, start, end
    ! type(ESMF_GridComp) :: model
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState_l
    type(ESMF_State) :: exportState_l

    file = __FILE__
    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (check(rc, __LINE__, file)) return

    start = lbound(fieldList,1)
    end = ubound(fieldList,1)

    do i = start, end
      ! if (fieldList(i)%ad_export .or. fieldList(i)%ad_import) then
      !    print *, "Advertising variable: ", trim(fieldList(i)%sd_name)," ", &
      !      trim(fieldList(i)%st_name), " Imp/Exp",  fieldList(i)%ad_import, &
      !      fieldList(i)%ad_export
      ! end if
      if (fieldList(i)%ad_import) then
        call NUOPC_Advertise(importState, &
          StandardName = fieldList(i)%sd_name, &
          Units = fieldList(i)%units, &
          TransferOfferGeomObject = transferOffer, &
          name = fieldList(i)%st_name, &
          rc = rc)
        if (check(rc, __LINE__, file)) return
        print *, "MPAS: advertise import ", trim(fieldList(i)%st_name)
      end if

      if (fieldList(i)%ad_export) then
        call NUOPC_Advertise(exportState, &
          StandardName = fieldList(i)%sd_name, &
          Units = fieldList(i)%units, &
          TransferOfferGeomObject = transferOffer, &
          name = fieldList(i)%st_name, &
          rc = rc)
        if (check(rc, __LINE__, file)) return
        ! print *, "MPAS: advertise export ", trim(fieldList(i)%st_name)
        ! print *, "MPAS: advertise export ", trim(fieldList(i)%sd_name), &
        !      trim(fieldList(i)%st_name), &
        !      trim(fieldList(i)%units), &
        !      trim(transferOffer)
      end if
    end do
    print *, "MPAS: Exiting Advertised Fields"
  end subroutine advertise_fields

  subroutine check_var(var, name)
    use machine
    real(kind=kind_noahmp), allocatable, dimension(:), intent(in) :: var
    character(len=*), intent(in) :: name
    if (allocated(var)) then
       print *, "allocated     ", trim(name)
       print *, " : ", var
    else
       print *, "not allocated ", trim(name)
    end if
  end subroutine check_var

  function field_create(rt_domain, fld_name, mesh, did, &
       nSoilLevels, rc) &
       result(field)
    use mpas_derived_types, only: domain_type
    use mpas_atmphys_vars, only: mpas_noahmp, smois_p
    ! arguments
    type(domain_type), intent(in) :: rt_domain
    ! type (domain_type), intent(in) :: rt_domain(:)
    character(*), intent(in) :: fld_name
    type(ESMF_Mesh), intent(in) :: mesh
    integer, intent(in) :: did
    integer, intent(in) :: nsoillevels
    ! type(memory_flag), intent(in) :: memflg
    integer,          intent(out) :: rc
    ! return value
    type(ESMF_Field) :: field
    ! local variables
    character(len=16)       :: cmemflg
    character(:), allocatable :: file
    real(ESMF_KIND_R8), allocatable, target :: test_array(:,:)

    file = __FILE__
    rc = ESMF_SUCCESS

    ! allocate(test_array(4, 40962))
    ! test_array(1,:) = 1
    ! test_array(2,:) = 2
    ! test_array(3,:) = 3
    ! test_array(4,:) = 4
    ! print *, "test_array shape", shape(test_array)

    ! print *, "allocated mpas_noahmp sfcrunoff =", allocated(mpas_noahmp%sfcrunoff)
    ! print *, "shape mpas_noahmp sfcrunoff =", shape(mpas_noahmp%sfcrunoff)



    ! if (memflg .eq. MEMORY_POINTER) then
      select case (trim(fld_name))
      case ('smc') ! soil moisture
         ! field = ESMF_FieldCreate(name=fld_name, grid=grid, &
         !    farray=rt_domain%smois(:,:,:), gridToFieldMap=(/1,2/), &
         !   ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
         !   indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         print *, "shape mpas_noahmp smc|smois =", shape(mpas_noahmp%smois)
         field = ESMF_FieldCreate( &
              name=fld_name, &
              farray=mpas_noahmp%smois(:,:), &
              mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              indexflag=ESMF_INDEX_DELOCAL, &
              gridToFieldMap=(/1/), &
              ungriddedLBound=(/1/), &
              ungriddedUBound=(/nSoilLevels/), rc=rc)
         ! indexflag=ESMF_INDEX_GLOBAL, &
         if (check(rc, __LINE__, file)) return
      case ('slc') ! liquid soil moisture
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              farray=mpas_noahmp%sh2o(:,:), &
              gridToFieldMap=(/1/), &
              ungriddedLBound=(/1/), &
              ungriddedUBound=(/nSoilLevels/), &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('stc')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              farray=mpas_noahmp%tslb(:,:), &
              gridToFieldMap=(/1/), &
              ungriddedLBound=(/1/), &
              ungriddedUBound=(/nSoilLevels/), &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('sh2ox1')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%sh2o(:,1), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('sh2ox2')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%sh2o(:,2), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('sh2ox3')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%sh2o(:,3), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('sh2ox4')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%sh2o(:,4), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('smc1')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%smois(:,1), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('smc2')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%smois(:,2), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('smc3')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%smois(:,3), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('smc4')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%smois(:,4), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('smcmax1')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%smcmax_table(:), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('stc1') ! soil temperature
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%tslb(:,1), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('stc2')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%tslb(:,2), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('stc3')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%tslb(:,3), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('stc4')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%tslb(:,4), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('vegtyp')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%ivgtyp(:), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('sfchead')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%sfcheadrt(:), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('infxsrt')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%infxsrt, &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('soldrain')
         print *, "soldrain allocated: ", allocated(mpas_noahmp%soldrain)
         print *, "soldrain shape: ", shape(mpas_noahmp%soldrain)
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%soldrain, &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('sfcrunoff')
         print *, "sfcrunoff allocated: ", allocated(mpas_noahmp%sfcrunoff)
         print *, "sfcrunoff shape: ", shape(mpas_noahmp%sfcrunoff)
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%sfcrunoff(:), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case ('udrunoff')
         field = ESMF_FieldCreate(name=fld_name, mesh=mesh, &
              meshloc=ESMF_MESHLOC_ELEMENT, &
              farray=mpas_noahmp%udrunoff(:), &
              indexflag=ESMF_INDEX_DELOCAL, rc=rc)
         if (check(rc, __LINE__, file)) return
      case default
         call ESMF_LogSetError(ESMF_FAILURE, &
              msg="Field hookup missing: "//trim(fld_name), &
              file=file, rcToReturn=rc)
         return  ! bail out
      end select
    ! elseif (memflg .eq. MEMORY_COPY) then
      ! select case (trim(fld_name))
      !   case ('smc','slc','stc')
      !     field = ESMF_FieldCreate(name=fld_name, grid=grid, &
      !       typekind=ESMF_TYPEKIND_FIELD, gridToFieldMap=(/1,2/), &
      !       ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
      !       indexflag=ESMF_INDEX_DELOCAL, rc=rc)
      !     if (check(rc, __LINE__, file)) return
      !   case default
      !     field = ESMF_FieldCreate(name=fld_name, grid=grid, &
      !       typekind=ESMF_TYPEKIND_FIELD, &
      !       indexflag=ESMF_INDEX_DELOCAL, rc=rc)
      !     if (check(rc, __LINE__, file)) return
      ! end select
      ! call ESMF_FieldFill(field, dataFillScheme="const", &
      !   const1=ESMF_MISSING_VALUE, rc=rc)
      ! if (check(rc, __LINE__, file)) return
    ! else
    !   cmemflg = memflg
    !   call ESMF_LogSetError(ESMF_FAILURE, &
    !        msg=": Field memory flag unknown: "//trim(cmemflg), &
    !        file=file, rcToReturn=rc)
    !   return  ! bail out
    ! endif

  end function field_create



end module mpas_nuopc_fields
