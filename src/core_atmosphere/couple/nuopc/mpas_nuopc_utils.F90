module mpas_nuopc_utils
  use esmf
  implicit none
contains
  function gridCreate(rc) result(grid)
    integer, intent(out) :: rc
    type(ESMF_Grid) :: grid
    character(:), allocatable :: file
    rc = ESMF_SUCCESS
    file = __FILE__

    print *, "TODO: move reading in MPAS mesh to here"
    ! grid = ESMF_GridCreate(name='MPAS_Grid'
    !      distgrid=WRFHYDRO_DistGrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
    !      coordTypeKind=ESMF_TYPEKIND_COORD, &
    !      rc = rc)
    if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return
  end function gridCreate


  function check(rc, msg, line, file) result(res)
    integer, intent(in) :: rc
    character(len=*), intent(in) :: msg
    integer, intent(in) :: line
    character(len=*), intent(in) :: file
    logical :: res
    res = ESMF_LogFoundError(rcToCheck=rc, msg=msg, line=line, file=file)
  end function check
end module mpas_nuopc_utils
