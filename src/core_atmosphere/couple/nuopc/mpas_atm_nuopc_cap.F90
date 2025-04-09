module mpas_atm_nuopc_cap

  ! use ESMF, only: ESMF_Comp, ESMF_State, ESMF_Clock
  use mpas_subdriver, only: mpas_init, mpas_run, mpas_finalize
  USE ESMF, only: ESMF_SUCCESS, ESMF_Clock, ESMF_State, ESMF_GridComp
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS        => SetServices, &
    model_label_DataInitialize => label_DataInitialize, &
    model_label_SetClock    => label_SetClock, &
    model_label_CheckImport => label_CheckImport, &
    model_label_Advance     => label_Advance, &
    model_label_Finalize    => label_Finalize

  ! use atm_import_export
  ! use nuopc_shr_methods, only: chkerr

  implicit none

  private
  ! public :: SetServices

contains
!! This cap specializes the cap configuration, initialization, advertised
!! fields, realized fields, data initialization, clock, run, and finalize.

!! @subsection SetServices Set Services (Register Subroutines)
!! Table summarizing the NUOPC specialized subroutines registered during
!! [SetServices] (@ref WRFHYDRO_NUOPC::SetServices).  The "Phase" column says
!! whether the subroutine is called during the initialization, run, or
!! finalize part of the coupled system run.

  ! subroutine SetServices(comp, rc)
  !   type(ESMF_Comp) :: comp
  !   integer, intent(out) :: rc
  !   rc = ESMF_SUCCESS

  !   call NUOPC_CompSpecialize(comp, specRoutine=InitializeP0, phase=0, rc=rc); call chkerr(rc)
  !   call NUOPC_CompSpecialize(comp, specRoutine=InitializeP1, phase=1, rc=rc); call chkerr(rc)
  !   call NUOPC_CompSpecialize(comp, specRoutine=InitializeP3, phase=3, rc=rc); call chkerr(rc)
  !   call NUOPC_CompSpecialize(comp, specRoutine=DataInitialize, phase=-1, rc=rc); call chkerr(rc)
  !   call NUOPC_CompSpecialize(comp, specRoutine=SetClock, phase=-1, rc=rc); call chkerr(rc)
  !   call NUOPC_CompSpecialize(comp, specRoutine=CheckImport, phase=-1, rc=rc); call chkerr(rc)
  !   call NUOPC_CompSpecialize(comp, specRoutine=ModelAdvance, phase=-1, rc=rc); call chkerr(rc)
  !   call NUOPC_CompSpecialize(comp, specRoutine=ModelFinalize, phase=-1, rc=rc); call chkerr(rc)
  ! end subroutine SetServices



  ! Set the Initialize Phase Definition (IPD). Configure model
  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    use mpas_subdriver, only: mpas_init
    use mpas_derived_types, only : core_type, domain_type
! #include "../../../framework/mpas_domain_types.inc"
    implicit none
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    type (core_type), pointer :: corelist => null()
    type (domain_type), pointer :: domain => null()

    rc = ESMF_SUCCESS
    ! Placeholder: Initialize VM, internal config
    ! Set clock, VM, etc.
    call mpas_init(corelist, domain)

  end subroutine InitializeP0

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    ! Advertise import/export fields
    ! call atm_advertise_fields(gcomp, exportState, rc); call chkerr(rc)
  end subroutine InitializeP1

  ! Initialize model.  Advertize import and export fields
  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    ! call atm_realize_fields(gcomp, importState, exportState, rc); call chkerr(rc)
  end subroutine InitializeP3

  ! Initialize import and export data
  subroutine DataInitialize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    ! call atm_import(gcomp, importState, rc); call chkerr(rc)
    ! call atm_export(gcomp, exportState, rc); call chkerr(rc)
  end subroutine DataInitialize

  ! Set model clock during initialization
  subroutine SetClock(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    ! Placeholder: Set MPAS-ATM internal time from clock
  end subroutine SetClock

  ! Check timestamp on import data.
  subroutine CheckImport(gcomp, importState, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    ! Placeholder: Timestamp logic or import validation
  end subroutine CheckImport

  ! Advances the model by a timestep
  subroutine ModelAdvance(gcomp, importState, exportState, clock, rc)
    use mpas_subdriver, only: mpas_run
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    ! call mpas_run(domain)
  end subroutine ModelAdvance

  ! Releases memory
  subroutine ModelFinalize(gcomp, rc)
    use mpas_subdriver, only: mpas_finalize
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    ! call mpas_finalize(corelist, domain)
  end subroutine ModelFinalize

end module mpas_atm_nuopc_cap
