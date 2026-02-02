module mpas_nuopc_utils
  use esmf
  use nuopc
  implicit none

  character(len=ESMF_MAXSTR), parameter :: file = __FILE__
  logical, parameter :: debug = .false.

contains
  subroutine hydroWeightGeneration()
    type(ESMF_Mesh) :: mesh, grid
    integer :: rc
    character(:), allocatable :: mesh_file, weight_file
    rc = ESMF_SUCCESS

    ! mesh_file = "x1.40962.esmf.nc"
    mesh_file = "frontrange.scrip.nc"
    mesh = ESMF_MeshCreate(filename=mesh_file, &
         fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    if (check(rc, __LINE__, file)) return
    print *, "Read in mesh file: ", mesh_file

    ! grid = ESMF.Grid(filename="wrfhydro_grid.nc", &
    !      filetype=ESMF.FileFormat.SCRIP, rc=rc)
    ! if (check(rc, __LINE__, file)) return
    ! print *, "Read in WRF-Hydro grid: ", mesh_file

    ! weight_file = "foo_weight.nc"
    ! call ESMF_RegridWeightGen(srcFile=mesh, dstFile=grid, &
    !      weightFile=weight_file, &
    !      srcFileType=ESMF_FILEFORMAT_ESMFMESH, &
    !      dstFileType=ESMF_FILEFORMAT_UGRID, &
    !      weightOnlyFlag=.true., verboseFlag=.true.)


    ! stop "fin in hydroWeightGeneration"
  end subroutine hydroWeightGeneration

  function gridCreate(rc) result(grid)
    integer, intent(out) :: rc
    type(ESMF_Grid) :: grid
    rc = ESMF_SUCCESS

    print *, "TODO gridCreate in utils: move reading in MPAS mesh to here"
    ! grid = ESMF_GridCreate(name='MPAS_Grid'
    !      distgrid=WRFHYDRO_DistGrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
    !      coordTypeKind=ESMF_TYPEKIND_COORD, &
    !      rc = rc)
    if (check(rc, __LINE__, file)) return
  end function gridCreate

  function create_esmf_mesh(domain) result(mesh)
    use mpas_derived_types, only: domain_type, mpas_pool_type
    use mpas_pool_routines, only: mpas_pool_get_subpool, mpas_pool_get_dimension
    type(domain_type), intent(in)    :: domain
    type (mpas_pool_type), pointer :: mpas_mesh
    type(ESMF_Mesh) :: mesh, mesh_in
    ! integer, pointer :: nCellsSolve, nSoilLevels

    character(:), allocatable :: file, mesh_file
    integer :: rc
    integer :: ncount, nelem
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_VM)       :: vm
    integer, allocatable :: gindex(:)
    character(len=256) :: iomsg, mpas_graph_file
    integer :: unit, iostat, irank, localCount
    integer :: rank, np
    integer :: idx, inode

    file = __FILE__
    rc = ESMF_SUCCESS

    ! add the following to make generic
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (check(rc, __LINE__, file)) return
    call ESMF_VMGet(vm, localPet=rank, petCount=np, rc=rc)
    if (check(rc, __LINE__, file)) return

    ! read
    if (np == 1) then
       mpas_graph_file = 'frontrange.graph.info'
    else
       write(mpas_graph_file, '(A,I0)') 'frontrange.graph.info.part.', np
    end if
    print *, "MPAS: mpas_graph_file =", mpas_graph_file
    open(newunit=unit, file=mpas_graph_file, &
         status='old', action='read', iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) then
       print *, trim(iomsg)
       stop "Error opening [casename].graph.info.part.[np]"
    end if

    localCount = 0
    do
       read(unit, *, iostat=iostat) irank
       if (iostat /= 0) exit
       if (irank == rank) localCount = localCount + 1
    end do

    allocate(gindex(localCount))

    print *, rank, "/", np, ": with localCount =", localCount

    ! setup grid distribution
    rewind(unit)
    idx   = 0
    inode = 0

    do
       read(unit, *, iostat=iostat) irank
       if (iostat /= 0) exit

       inode = inode + 1 ! inode = line number
       if (irank == rank) then
          idx = idx + 1
          gindex(idx) = inode ! seqIndex = global node id
       end if
    end do
    close(unit)


    distgrid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
    if (check(rc, __LINE__, file)) return

    ! call mpas_pool_get_subpool(domain%blocklist%structs, 'mesh', mpas_mesh)
    ! call mpas_pool_get_dimension(mpas_mesh,'nCellsSolve',nCellsSolve)
    ! call mpas_pool_get_dimension(mpas_mesh,'nSoilLevels',nSoilLevels)
    ! print *, "nCellsSolve =", nCellsSolve ! correct
    ! print *, "nSoilLevels =", nSoilLevels ! correct
    ! print *, "mpas_mesh size =", mpas_mesh%size
    ! print *, "nVertLevels, maxEdges, maxEdges2, num_scalars", nVertLevels,
    ! maxEdges, maxEdges2, num_scalars

    ! Reading in mesh file after converting frontrange.grid.nc to scrip format
    ! mpas-dev.github.io/MPAS-Tools/0.24.0/_modules/mpas_tools/scrip/from_mpas.html

    mesh_file = "frontrange.scrip.nc"
    print *, "todo: read mesh_file name from namelist, currently ", trim(mesh_file)
    mesh = ESMF_MeshCreate(filename=mesh_file, &
         elementDistgrid=distgrid, &
         fileformat=ESMF_FILEFORMAT_SCRIP, rc=rc)
    if (check(rc, __LINE__, file)) return

    if (debug) call ESMF_MeshWrite(mesh, "mpas_mesh", rc=rc)
    if (check(rc, __LINE__, file)) return

    ! Get dimensions/counts
    call ESMF_MeshGet(mesh, nodeCount=ncount, elementCount=nElem, rc=rc)
    if (check(rc, __LINE__, file)) return
    print *, "MPAS: ncount=", ncount, "nelem=", nelem

  end function create_esmf_mesh


  function check(rc, line, file_in) result(res)
    integer, intent(in) :: rc
    ! character(len=*), intent(in) :: msg
    integer, intent(in) :: line
    character(len=*), intent(in) :: file_in
    logical :: res
    res = ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=line, file=file_in)
    ! this won't work properly in parallel
    if (res .eqv. .true.) error stop "Bad Check, msg = " // ESMF_LOGERR_PASSTHRU
  end function check

  subroutine printa(msg)
    character(len=*), intent(in) :: msg
    integer :: rc
    logical :: rc_l
    print *, "MPAS: ", trim(msg)
    call ESMF_LogWrite("MPAS: "//trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    rc_l = check(rc, __LINE__, file)
    ! TODO: FIX RC_L TYPE
  end subroutine printa

  subroutine probe_connected_pair(expState, impState, name, rc)
    type(ESMF_State), intent(inout) :: expState, impState
    character(*), intent(in) :: name
    integer, intent(out) :: rc
    type(ESMF_Field) :: fe, fi
    logical :: ce, ci
    rc = ESMF_SUCCESS

    ce = NUOPC_IsConnected(expState, fieldName=trim(name), rc=rc)
    ci = NUOPC_IsConnected(impState, fieldName=trim(name), rc=rc)
    write(*,'(A,1X,A,1X,L1,1X,L1)') 'Connected(exp,imp):', trim(name), ce, ci
    if (ce) then
       call ESMF_StateGet(expState, itemName=trim(name), field=fe, rc=rc)
       if (check(rc, __LINE__, file)) return
       call ESMF_FieldValidate(fe, rc=rc)
       if (check(rc, __LINE__, file)) return
    end if
    if (ci) then
       call ESMF_StateGet(impState, itemName=trim(name), field=fi, rc=rc)
       if (check(rc, __LINE__, file)) return
       call ESMF_FieldValidate(fi, rc=rc)
       if (check(rc, __LINE__, file)) return
    end if
  end subroutine probe_connected_pair

end module mpas_nuopc_utils
