module mpas_nuopc_utils
  use esmf, only: ESMF_LogFoundError
  implicit none
contains
  function check(rc, msg, line, file) result(res)
    integer, intent(in) :: rc
    character(len=*), intent(in) :: msg
    integer, intent(in) :: line
    character(len=*), intent(in) :: file
    logical :: res
    res = ESMF_LogFoundError(rcToCheck=rc, msg=msg, line=line, file=file)
  end function check
end module mpas_nuopc_utils
