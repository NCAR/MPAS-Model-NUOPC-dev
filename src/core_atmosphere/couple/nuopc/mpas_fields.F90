module mpas_nuopc_fields
  use mpas_nuopc_utils, only: check
  use esmf
  use nuopc
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

  integer, parameter :: num_field_vars = 20
  type(cap_field_t), target, dimension(num_field_vars) :: field_list
  logical :: initialized = .false.

contains

  function get_field_list() result(res)
    type(cap_field_t), allocatable :: res(:)
    if (.not. initialized) then
       call field_init()
       initialized = .true.
    end if
    res = field_list
  end function get_field_list


  subroutine field_init()
    field_list = [ &
      add_field("inst_total_soil_moisture_content","smc", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("inst_soil_moisture_content","slc", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("inst_soil_temperature","stc", "K", &
        .TRUE., .FALSE., 288.d0), &
      add_field("liquid_fraction_of_soil_moisture_layer_1","sh2ox1", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("liquid_fraction_of_soil_moisture_layer_2","sh2ox2", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("liquid_fraction_of_soil_moisture_layer_3","sh2ox3", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("liquid_fraction_of_soil_moisture_layer_4","sh2ox4", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("soil_moisture_fraction_layer_1","smc1", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("soil_moisture_fraction_layer_2","smc2", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("soil_moisture_fraction_layer_3","smc3", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("soil_moisture_fraction_layer_4","smc4", "m3 m-3", &
        .TRUE., .TRUE., 0.20d0), &
      add_field("soil_temperature_layer_1","stc1", "K", &
        .TRUE., .FALSE., 288.d0), &
      add_field("soil_temperature_layer_2","stc2", "K", &
        .TRUE., .FALSE., 288.d0), &
      add_field("soil_temperature_layer_3","stc3", "K", &
        .TRUE., .FALSE., 288.d0), &
      add_field("soil_temperature_layer_4","stc4", "K", &
        .TRUE., .FALSE., 288.d0), &
      add_field("soil_porosity","smcmax1", "1", &
        .FALSE., .FALSE., 0.45d0), &
      add_field("vegetation_type","vegtyp", "1", &
        .FALSE., .FALSE., 16.0d0), &
      add_field("surface_water_depth","sfchead", "mm", &
        .FALSE., .TRUE., 0.00d0), &
      add_field("time_step_infiltration_excess","infxsrt", "mm", &
        .TRUE., .FALSE., 0.00d0), &
      add_field("soil_column_drainage","soldrain", "mm", &
        .TRUE., .FALSE., 0.00d0) &
      ]
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




!   subroutine advertise_fields(domain)
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
!     ! if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return
!     ! ! exportable field: sea_surface_salinity
!     ! call NUOPC_Advertise(exportState, &
!     !      StandardName="sea_surface_salinity", name="sss", rc=rc)
!     ! if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return
!     ! ! exportable field: sea_surface_height_above_sea_level
!     ! call NUOPC_Advertise(exportState, &
!     !      StandardName="sea_surface_height_above_sea_level", &
!     !      name="ssh", rc=rc)
!     ! if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return

!     print *, "end of advertise_fields"
!     stop "FOO"
!   end subroutine advertise_fields

  subroutine advertise_fields(fieldList, importState, exportState, &
       transferOffer, rc)
    type(cap_field_t), intent(in)    :: fieldList(:)
    type(ESMF_State), intent(inout)   :: importState
    type(ESMF_State), intent(inout)   :: exportState
    character(*), intent(in),optional :: transferOffer
    integer, intent(out)              :: rc
    ! local variables
    character(:), allocatable :: file
    integer :: n
    file = __FILE__
    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%ad_import) then
        call NUOPC_Advertise(importState, &
          StandardName=fieldList(n)%sd_name, &
          Units=fieldList(n)%units, &
          TransferOfferGeomObject=transferOffer, &
          name=fieldList(n)%st_name, &
          rc=rc)
        if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return
      end if
      if (fieldList(n)%ad_export) then
        call NUOPC_Advertise(exportState, &
          StandardName=fieldList(n)%sd_name, &
          Units=fieldList(n)%units, &
          TransferOfferGeomObject=transferOffer, &
          name=fieldList(n)%st_name, &
          rc=rc)
        if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return
      end if
    end do

  end subroutine

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
end module mpas_nuopc_fields
