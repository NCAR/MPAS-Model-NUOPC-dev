module mpas_nuopc_utils
  use esmf
  implicit none
contains
  subroutine hydroWeightGeneration()
    type(ESMF_Mesh) :: mesh, grid
    integer :: rc
    character(:), allocatable :: file, mesh_file, weight_file
    file = __FILE__
    rc = ESMF_SUCCESS

    mesh_file = "x1.40962.esmf.nc"
    mesh = ESMF_MeshCreate(filename=mesh_file, &
         fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return
    print *, "Read in mesh file: ", mesh_file

    ! grid = ESMF.Grid(filename="wrfhydro_grid.nc", &
    !      filetype=ESMF.FileFormat.SCRIP, rc=rc)
    ! if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return
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
    character(:), allocatable :: file
    rc = ESMF_SUCCESS
    file = __FILE__

    print *, "TODO gridCreate in utils: move reading in MPAS mesh to here"
    ! grid = ESMF_GridCreate(name='MPAS_Grid'
    !      distgrid=WRFHYDRO_DistGrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
    !      coordTypeKind=ESMF_TYPEKIND_COORD, &
    !      rc = rc)
    if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return
  end function gridCreate

  function create_esmf_mesh(domain) result(mesh)
    use mpas_derived_types, only: domain_type, mpas_pool_type
    use mpas_pool_routines, only: mpas_pool_get_subpool, mpas_pool_get_dimension
    type(domain_type), intent(in)    :: domain
    type (mpas_pool_type), pointer :: mpas_mesh
    type(ESMF_Mesh) :: mesh
    integer, pointer :: nCellsSolve, nSoilLevels

    character(:), allocatable :: file, mesh_file
    integer :: rc
    file = __FILE__
    rc = ESMF_SUCCESS

    ! call mpas_pool_get_subpool(domain%blocklist%structs, 'mesh', mpas_mesh)
    ! call mpas_pool_get_dimension(mpas_mesh,'nCellsSolve',nCellsSolve)
    ! call mpas_pool_get_dimension(mpas_mesh,'nSoilLevels',nSoilLevels)
    ! print *, "nCellsSolve =", nCellsSolve ! correct
    ! print *, "nSoilLevels =", nSoilLevels ! correct
    ! print *, "mpas_mesh size =", mpas_mesh%size
    ! print *, "nVertLevels, maxEdges, maxEdges2, num_scalars", nVertLevels,
    ! maxEdges, maxEdges2, num_scalars

    ! Now create the ESMF Mesh
    ! mesh = ESMF_MeshCreate(parametricDim=2, spatialDim=2,                   &
    !      coordSys=ESMF_COORDSYS_SPH_RAD, rc=rc)

    ! call ESMF_MeshAddNodes(mesh, nodeCount=nVertices,                       &
    !      nodeIds=nodeIds, nodeCoords=nodeCoords, nodeOwners=nodeOwners, rc=rc)

    ! call ESMF_MeshAddElements(mesh, elemCount=nCells,                       &
    !      elementIds=elemIds, elementTypes=elemTypes,                        &
    !      elementConn=elemConn, elementConnIndex=elemConnIndex, rc=rc)

    ! call ESMF_MeshComplete(mesh, rc=rc)


    ! Reading in mesh file after converting frontrange.grid.nc to scrip format
    ! mpas-dev.github.io/MPAS-Tools/0.24.0/_modules/mpas_tools/scrip/from_mpas.html

    mesh_file = "frontrange.scrip.nc"
    mesh = ESMF_MeshCreate(filename=mesh_file, &
         fileformat=ESMF_FILEFORMAT_SCRIP, rc=rc)
    if (check(rc, ESMF_LOGERR_PASSTHRU, __LINE__, file)) return

  end function create_esmf_mesh


  function check(rc, msg, line, file) result(res)
    integer, intent(in) :: rc
    character(len=*), intent(in) :: msg
    integer, intent(in) :: line
    character(len=*), intent(in) :: file
    logical :: res
    res = ESMF_LogFoundError(rcToCheck=rc, msg=msg, line=line, file=file)
    if (res .eqv. .true.) error stop "Bad Check, msg = " // msg
  end function check
end module mpas_nuopc_utils
