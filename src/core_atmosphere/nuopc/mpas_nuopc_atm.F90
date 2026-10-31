#include "mpas_nuopc_macros.h"

module mpas_nuopc_atm
  !> MPAS NUOPC Cap for Atmosphere

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS => SetServices

#ifdef MPAS_USE_MPI_F08
  use mpi_f08, only : MPI_Comm
#endif

  ! MPAS modules
  use mpas_derived_types, only: core_type, domain_type, block_type, &
    mpas_pool_type, mpas_time_type
  use mpas_kind_types, only: rkind, r8kind, strkind
  use mpas_pool_routines, only: mpas_pool_get_config
  use mpas_subdriver, only: mpas_init, mpas_finalize
  use atm_core, only: atm_core_run_prepare, atm_core_run_step
  use mpas_nuopc_field, only: field_class, new_field
  use mpas_nuopc_fieldlist, only: fieldlist_class, new_fieldlist
  use mpas_nuopc_geom, only: mesh_class, new_mesh

  implicit none

  private

  ! MPAS state attached to NUOPC model
  type mpas_nuopc_atm_state
    integer :: verbosity = 0
    type(mesh_class) :: mesh
    type(fieldlist_class) :: export_fields
    type (core_type), pointer :: corelist => null()
    type (domain_type), pointer :: domain => null()
    real (kind=rkind), pointer :: dt => null()
    integer :: itimestep
    character (len=StrKIND), pointer :: config_restart_timestamp_name => null()
    logical, pointer :: config_apply_lbcs
  end type mpas_nuopc_atm_state

  type mpas_nuopc_atm_wrapper
    type(mpas_nuopc_atm_state), pointer :: mState => null()
  end type mpas_nuopc_atm_wrapper

  public SetVM, SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    !> Register model entry points:
    !>   Advertise: advertise import and export fields
    !>   Realize: realize connected fields
    !>   SetClock: initialize model clock
    !>   DataInitialize: initialize data in import and export states
    !>   Advance: advance model by a single time step
    !>   Finalize: finalize model and cleanup memory

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(mpas_nuopc_atm_wrapper) :: modelStateWrapper

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! specialize model entry points
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
      specRoutine=SetClock, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_Finalize, &
      specRoutine=Finalize, rc=rc)
    ESMF_ERR_RETURN(rc)

    allocate(modelStateWrapper%mState)
    call ESMF_GridCompSetInternalState(model, modelStateWrapper, rc)
    ESMF_ERR_RETURN(rc)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc)
    !> Advertise available export fields and desired import fields

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(mpas_nuopc_atm_wrapper) :: modelStateWrapper
    type(mpas_nuopc_atm_state), pointer :: mState
    type(ESMF_VM) :: vm
    type(ESMF_State) :: importState, exportState
    integer :: localPet
    integer :: int_mpic
#ifdef MPAS_USE_MPI_F08
    type(MPI_Comm) :: mpic
#endif
    character(len=*), parameter :: method = "Advertise"

    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(model, modelStateWrapper, rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_CompGet(model, verbosity=mState%verbosity, rc=rc)
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, &
      importState=importState, &
      exportState=exportState, &
      rc=rc &
    )
    ESMF_ERR_RETURN(rc)

    call ESMF_GridCompGet(model, vm=vm, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_VMGet(vm, localPet=localPet, mpiCommunicator=int_mpic, rc=rc)
    ESMF_ERR_RETURN(rc)

#ifdef MPAS_USE_MPI_F08
    mpic%mpi_val = int_mpic
    call mpas_init(corelist=mState%corelist, &
      domain_ptr=mState%domain, &
      external_comm=mpic)
#else
    call mpas_init(corelist=mState%corelist, &
      domain_ptr=mState%domain, &
      external_comm=int_mpic)
#endif

    if (localPet /= mState%domain%dminfo%my_proc_id) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="ESMF local PET must match the MPAS rank", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    mState%mesh = new_mesh(mState%domain, rc=rc)
    ESMF_ERR_RETURN(rc)
    if (mState%verbosity > 0) then
      call mState%mesh%print_mesh(writePet=0, rc=rc)
      ESMF_ERR_RETURN(rc)
    end if

    mState%export_fields = new_fieldlist([ &
      new_field("sfc_input_tslb","soil_temperature"), &
      new_field("diag_physics_t2m","surface_air_temperature"), &
      new_field("diag_physics_q2","surface_specific_humidity"), &
      new_field("diag_physics_swdnb","incident_shortwave"), &
      new_field("diag_physics_lwdnb","incident_longwave"), &
      new_field("diag_physics_u10","eastward_wind"), &
      new_field("diag_physics_v10","northward_wind"), &
      new_field("diag_surface_pressure","surface_pressure"), &
      new_field("diag_physics_ch","surface_exchange_coefficient_heat"), &
      new_field("diag_physics_chs2","surface_exchange_coefficient_heat_2m"), &
      new_field("diag_physics_cqs2","surface_exchange_coefficient_moisture_2m"), &
      new_field("diag_physics_sfc_emiss","surface_emissivity"), &
      new_field("diag_physics_coszr","cosine_zenith_angle"), &
      new_field("sfc_input_xice","sea_ice_area_fraction"), &
      new_field("sfc_input_tmn","soil_temperature_lower_boundary"), &
      new_field("diag_physics_rainncv","rainfall_accumulated"), &
      new_field("diag_physics_raincv","convective_rainfall_accumulated") &
    ])

    call mState%export_fields%initialize( &
      domain=mState%domain, &
      mesh=mState%mesh, &
      rc=rc &
    )
    ESMF_ERR_RETURN(rc)

    call mState%export_fields%advertise( &
      state=exportState, &
      rc=rc &
    )
    ESMF_ERR_RETURN(rc)

    if (mState%verbosity > 0) then
      call mState%export_fields%print(writePet=0, rc=rc)
      ESMF_ERR_RETURN(rc)
    end if

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    !> Check field connections and realize connected fields

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(mpas_nuopc_atm_wrapper) :: modelStateWrapper
    type(mpas_nuopc_atm_state), pointer :: mState
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(model, modelStateWrapper, rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, &
      driverClock=clock, &
      importState=importState, &
      exportState=exportState, &
      rc=rc &
    )
    ESMF_ERR_RETURN(rc)

    call mState%export_fields%realize(state=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call mState%export_fields%data_initialize(clock=clock, rc=rc)
    ESMF_ERR_RETURN(rc)

    if (mState%verbosity > 0) then
      call mState%export_fields%print(writePet=0, rc=rc)
      ESMF_ERR_RETURN(rc)
    end if

  end subroutine Realize

  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    !> Adjust model clock and time step during initialization

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(mpas_nuopc_atm_wrapper) :: modelStateWrapper
    type(mpas_nuopc_atm_state), pointer :: mState
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, currentTime, stopTime
    type(ESMF_State) :: importState, exportState
    integer(ESMF_KIND_I4) :: dt_i
    character(len=32) :: dateString

    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(model, modelStateWrapper, rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, &
      modelClock=clock, &
      importState=importState, &
      exportState=exportState, &
      rc=rc &
    )
    ESMF_ERR_RETURN(rc)

    ! initialize time step (dt)
    call mpas_pool_get_config(mState%domain%blocklist%configs, &
      key='config_dt', value=mState%dt)
    dt_i = int(mState%dt, kind=ESMF_KIND_I4)
    call ESMF_TimeIntervalSet(timeStep, s=dt_i, rc=rc) ! MPAS dt in seconds
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSetClock(model, clock, timeStep, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! log clock information
    call ESMF_ClockGet(clock, timeStep=timeStep, currTime=currentTime, &
      startTime=startTime, stopTime=stopTime, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_TimeIntervalGet(timeStep, timeStringISOFrac=dateString, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_LogWrite('ESMF timestep: ' // trim(dateString), &
      ESMF_LOGMSG_INFO, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_TimeGet(currentTime, timeStringISOFrac=dateString, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_LogWrite("ESMF current time: " // trim(dateString), &
      ESMF_LOGMSG_INFO, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_TimeGet(startTime, timeStringISOFrac=dateString, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_LogWrite("ESMF start time: " // trim(dateString), &
      ESMF_LOGMSG_INFO, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_TimeGet(stopTime, timeStringISOFrac=dateString, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_LogWrite("ESMF stop time: " // trim(dateString), &
      ESMF_LOGMSG_INFO, rc=rc)
    ESMF_ERR_RETURN(rc)

  end subroutine SetClock

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(model, rc)
    !> Initialize data in import and export states

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(mpas_nuopc_atm_wrapper) :: modelStateWrapper
    type(mpas_nuopc_atm_state), pointer :: mState
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState, exportState
    integer :: ierr

    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(model, modelStateWrapper, rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, modelClock=clock, &
      importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    ierr = atm_core_run_prepare(domain=mState%domain)
    if (ierr /= 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Error in atm_core_run_prepare", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn = rc)
      return
    end if
    mState%itimestep = 1
    call mpas_pool_get_config(mState%domain%blocklist%configs, &
      key='config_restart_timestamp_name', &
      value=mState%config_restart_timestamp_name)
    call mpas_pool_get_config(mState%domain%blocklist%configs, &
      key='config_apply_lbcs', &
      value=mState%config_apply_lbcs)

    call NUOPC_CompAttributeSet(model, &
      name="InitializeDataComplete", value="true", rc=rc)
    ESMF_ERR_RETURN(rc)

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    !> Advance model by a single time step

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(mpas_nuopc_atm_wrapper) :: modelStateWrapper
    type(mpas_nuopc_atm_state), pointer :: mState
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState, exportState
    integer :: ierr, stream_dir
    character(len=strkind) :: input_stream, read_time
    real (kind=r8kind) :: integ_start_time, integ_stop_time

    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(model, modelStateWrapper, rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, modelClock=clock, &
      importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    ierr = atm_core_run_step(domain=mState%domain, &
      itimestep=mState%itimestep, dt=mState%dt, &
      config_restart_timestamp_name=mState%config_restart_timestamp_name, &
      config_apply_lbcs=mState%config_apply_lbcs)
    if (ierr /= 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Error in atm_core_run_step", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn = rc)
      return
    end if

    if (mState%verbosity > 0) then
      call mState%export_fields%print_stats(writePet=0, rc=rc)
      ESMF_ERR_RETURN(rc)
    end if

  end subroutine Advance

  !-----------------------------------------------------------------------------

  subroutine Finalize(model, rc)
    !> Finalize model and cleanup memory allocations

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(mpas_nuopc_atm_wrapper) :: modelStateWrapper
    type(mpas_nuopc_atm_state), pointer :: mState
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(model, modelStateWrapper, rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, &
      importState=importState, &
      exportState=exportState, &
      rc=rc &
    )
    ESMF_ERR_RETURN(rc)

    call mState%export_fields%finalize(rc=rc)
    ESMF_ERR_RETURN(rc)

    call mpas_finalize(corelist=mState%corelist, domain_ptr=mState%domain)

    deallocate(modelStateWrapper%mState)

  end subroutine Finalize

  !-----------------------------------------------------------------------------

end module mpas_nuopc_atm
