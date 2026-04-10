#define ESMF_ERR_RETURN(rc) if (ESMF_LogFoundError( \
        rcToCheck=rc, \
        msg=ESMF_LOGERR_PASSTHRU, \
        line=__LINE__, \
        file=__FILE__) \
    ) return

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

  implicit none

  private
  ! MPAS state attached to NUOPC model
  type mpas_nuopc_atm_state
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
    call ESMF_InternalStateAdd(model, internalState=modelStateWrapper, rc=rc)
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
    integer :: int_mpic
#ifdef MPAS_USE_MPI_F08
    type(MPI_Comm) :: mpic
#endif

    rc = ESMF_SUCCESS

    call ESMF_InternalStateGet(model, internalState=modelStateWrapper, rc=rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_GridCompGet(model, vm=vm, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_VMGet(vm, &
      mpiCommunicator=int_mpic, rc=rc)
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
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    call ESMF_InternalStateGet(model, internalState=modelStateWrapper, rc=rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! (no fields to connect for initial implementation)

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

    call ESMF_InternalStateGet(model, internalState=modelStateWrapper, rc=rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, modelClock=clock, &
      importState=importState, &
      exportState=exportState, rc=rc)
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

    call ESMF_InternalStateGet(model, internalState=modelStateWrapper, rc=rc)
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

    call ESMF_InternalStateGet(model, internalState=modelStateWrapper, rc=rc)
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

    call ESMF_InternalStateGet(model, internalState=modelStateWrapper, rc=rc)
    mState => modelStateWrapper%mState
    ESMF_ERR_RETURN(rc)

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call mpas_finalize(corelist=mState%corelist, domain_ptr=mState%domain)

    deallocate(modelStateWrapper%mState)

  end subroutine Finalize

  !-----------------------------------------------------------------------------

end module mpas_nuopc_atm
