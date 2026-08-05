#include "mpas_nuopc_macros.inc"

module mpas_nuopc_utils
  use mpi
  use esmf
  use netcdf
  use nuopc
  implicit none

  character(len=ESMF_MAXSTR), parameter :: file = __FILE__
  logical, parameter :: debug = .false.
  integer, allocatable :: gindex(:)

contains

  function create_esmf_mesh(domain, rc) result(mesh)
    !> Build the ESMF mesh matching the MPAS decomposition.
    !>
    !> Errors here abort the whole job rather than returning. Every failure point
    !> below sits next to a collective -- the barrier, or ESMF_MeshCreate, whose
    !> file conversion runs on PET0 while the others wait inside it -- so a local
    !> return would strand the remaining PETs and turn a clean error into a
    !> walltime kill. rc is still set so callers can react if that ever changes.
    use mpas_derived_types, only: domain_type
    type(domain_type), intent(in)    :: domain
    integer, intent(out) :: rc
    type(ESMF_Mesh) :: mesh

    character(len=:), allocatable :: mpas_grid_file, mesh_file
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_VM) :: vm
    integer :: ncount, nelem
    integer :: localPet
    ! ESMF_FILEFORMAT_* are a derived type, not integer constants
    type(ESMF_FileFormat_Flag) :: fileFormat
    logical :: exists, on_a_sphere

    rc = ESMF_SUCCESS

    mpas_grid_file = get_mpas_grid_filename(domain)

    ! SCRIP describes cells by corner lat/lon, so it can only carry a spherical
    ! mesh. A planar MPAS mesh keeps its coordinates in xCell/yCell and
    ! xVertex/yVertex, and goes out as an ESMF unstructured mesh in Cartesian
    ! (kilometre) coordinates instead.
    on_a_sphere = mpas_mesh_on_a_sphere(mpas_grid_file)
    if (on_a_sphere) then
       mesh_file  = mpas_mesh_tmp_filename(mpas_grid_file, '.tmp.scrip.nc')
       fileFormat = ESMF_FILEFORMAT_SCRIP
    else
       mesh_file  = mpas_mesh_tmp_filename(mpas_grid_file, '.tmp.esmfmesh.nc')
       fileFormat = ESMF_FILEFORMAT_ESMFMESH
    end if

    ! Only one PET generates the file. Every PET used to write the whole thing
    ! concurrently, which raced on the same bytes; the others wait here instead.
    ! Note get_mpas_dist_grid() still derives its rank from the *global* VM, so
    ! both assume the component owns every PET.
    call ESMF_VMGetCurrent(vm, rc=rc)
    ESMF_ERR_ABORT(rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    ESMF_ERR_ABORT(rc)

    if (localPet == 0) then
       inquire(file=trim(mesh_file), exist=exists)
       if (.not. exists) then
          if (on_a_sphere) then
             call mpas_to_scrip_mesh(mpas_grid_file, mesh_file)
          else
             call mpas_to_esmf_mesh(mpas_grid_file, mesh_file)
          end if
       end if
    end if
    call ESMF_VMBarrier(vm, rc=rc)
    ESMF_ERR_ABORT(rc)

    distgrid  = get_mpas_dist_grid(domain, rc)
    ESMF_ERR_ABORT(rc)
    mesh = ESMF_MeshCreate(filename=mesh_file, &
         elementDistgrid=distgrid, &
         fileformat=fileFormat, rc=rc)
    ESMF_ERR_ABORT(rc)

    if (debug) then
       call ESMF_MeshWrite(mesh, "mpas_mesh_from_file", rc=rc)
       ESMF_ERR_ABORT(rc)
    end if

    ! Get dimensions/counts
    call ESMF_MeshGet(mesh, nodeCount=ncount, elementCount=nElem, rc=rc)
    ESMF_ERR_ABORT(rc)
  end function create_esmf_mesh

  subroutine check_nf90(stat, func)
    integer, intent(in) :: stat
    character(len=*), intent(in) :: func
    if (stat /= nf90_noerr) then
       write(*,*) "NetCDF Error: ", trim(func), " failure"
       error stop
    end if
  end subroutine check_nf90

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

  function get_mpas_dist_grid(domain, rc) &
       result(dist_grid)
    use mpas_derived_types, only: domain_type, mpas_pool_type, StrKIND
    use mpas_pool_routines, only: mpas_pool_get_config
    type(domain_type), intent(in) :: domain
    integer, intent(inout) :: rc
    type(ESMF_DistGrid) :: dist_grid

    type(ESMF_VM)       :: vm
    character(len=256) :: mpas_graph_file, mpas_grid_file, iomsg
    character(len=StrKIND), pointer :: config_block_decomp_file_prefix
    integer :: unit, iostat, irank, localCount, ierr, tmp
    integer :: i, rank, np, idx, inode
    rc = ESMF_SUCCESS

    call ESMF_VMGetGlobal(vm, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMGet(vm, localPet=rank, petCount=np, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! get config_block_decomp_file_prefix
    call mpas_pool_get_config(domain % configs, &
         'config_block_decomp_file_prefix', config_block_decomp_file_prefix)

    ! open .graph.info when np==1 or .graph.info.np when np>1
    if (np == 1) then
       i = index(trim(config_block_decomp_file_prefix), '.part', back=.true.)
       mpas_graph_file = config_block_decomp_file_prefix(:i-1)
       print *, "MPAS: opening mpas_graph_file =", mpas_graph_file
       open(newunit=unit, file=mpas_graph_file, status="old", &
            action="read", iostat=ierr)
       if (ierr /= 0) error stop "Failed to open frontrange.graph.info"

       read(unit, *, iostat=ierr) localCount, tmp   ! reads: first_int second_int
       if (ierr /= 0) error stop &
            "Failed to read first line of frontrange.graph.info"
       close(unit)
    else
       write(mpas_graph_file, '(A,I0)') &
            trim(config_block_decomp_file_prefix), np
       print *, "MPAS: opening mpas_graph_file =", mpas_graph_file
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
    end if

    allocate(gindex(localCount))
    print *, rank, "/", np, ": with localCount =", localCount

    ! setup grid distribution
    if (np == 1) then
       inode = 1
       do inode = 1, localCount
          gindex(inode) = inode
       end do
    else
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
    end if
    close(unit)

    ! print *, rank,":gindex=", gindex(1:10)
    ! stop "here"
    dist_grid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
    ESMF_ERR_RETURN(rc)
  end function get_mpas_dist_grid

  function get_mpas_grid_filename(domain) &
       result(mpas_grid_file)
    use mpas_derived_types, only: domain_type, StrKIND, &
         MPAS_STREAM_PROPERTY_FILENAME, MPAS_STREAM_MGR_NOERR
    use mpas_stream_manager, only: MPAS_stream_mgr_get_property
    type(domain_type), intent(in) :: domain
    character(len=:), allocatable :: mpas_grid_file
    character (len=StrKIND) :: filename
    integer :: ierr

    ! The 'input' stream carries the mesh fields (latCell, lonCell,
    ! verticesOnCell, ...) that mpas_to_scrip_mesh needs, so ask the stream
    ! manager for its filename. Deriving a name from
    ! config_block_decomp_file_prefix only works for testcases that happen to
    ! name their mesh [testcase].grid.nc.
    filename = ''
    call MPAS_stream_mgr_get_property(domain % streamManager, 'input', &
         MPAS_STREAM_PROPERTY_FILENAME, filename, ierr=ierr)
    if (ierr /= MPAS_STREAM_MGR_NOERR .or. len_trim(filename) == 0) then
       print *, "Error: could not get filename of MPAS 'input' stream"
       error stop "Error: could not determine MPAS mesh file"
    end if
    mpas_grid_file = trim(filename)
  end function get_mpas_grid_filename

  function mpas_mesh_tmp_filename(mpas_grid_file, suffix) result(mesh_file)
    character(len=*), intent(in) :: mpas_grid_file
    character(len=*), intent(in) :: suffix
    character(len=:), allocatable :: mesh_file
    integer :: i
    i = index(trim(mpas_grid_file), '.nc', back=.true.)
    if (i == 0) then
       print *, "Error: mpas_grid_file ", trim(mpas_grid_file), &
            " does not have .nc suffix"
       error stop "Error: mpas_grid_file in incorrect format"
    end if
    mesh_file = mpas_grid_file(:i-1) // suffix
  end function mpas_mesh_tmp_filename

  function mpas_mesh_on_a_sphere(mpasFile) result(on_a_sphere)
    !> Read the 'on_a_sphere' global attribute of an MPAS mesh file. Read-only,
    !> so every PET may call it concurrently.
    character(len=*), intent(in) :: mpasFile
    logical :: on_a_sphere

    character(len=:), allocatable :: attval
    integer :: stat, fin, attlen

    stat = nf90_open(trim(mpasFile), nf90_nowrite, fin)
    call check_nf90(stat, 'nf90_open('//trim(mpasFile)//')')

    stat = nf90_inquire_attribute(fin, nf90_global, 'on_a_sphere', len=attlen)
    call check_nf90(stat, 'nf90_inquire_attribute(on_a_sphere)')
    allocate(character(len=attlen) :: attval)
    stat = nf90_get_att(fin, nf90_global, 'on_a_sphere', attval)
    call check_nf90(stat, 'nf90_get_att(on_a_sphere)')

    stat = nf90_close(fin)
    call check_nf90(stat, 'nf90_close('//trim(mpasFile)//')')

    ! Some MPAS files pad the attribute, e.g. 'NO              '
    on_a_sphere = (trim(adjustl(attval)) /= 'NO')
  end function mpas_mesh_on_a_sphere

  ! Convert a *planar* MPAS mesh to the ESMF unstructured grid format
  ! (ESMF_FILEFORMAT_ESMFMESH) in Cartesian coordinates.
  !
  ! SCRIP cannot represent a planar mesh: it stores only corner lat/lon, which a
  ! planar MPAS file leaves at zero. The ESMF format instead takes explicit node
  ! coordinates, and flags them as Cartesian by setting units='kilometers'
  ! ('degrees' being the only other value ESMF recognises).
  !
  ! Periodic meshes need care. MPAS stores every coordinate inside the domain, so
  ! a cell straddling the wrap seam has corners on the far side and would come out
  ! spanning the whole domain. Each corner is therefore shifted by whole periods
  ! until it sits next to its own cell centre, and corners that had to be shifted
  ! become new nodes -- i.e. the torus is cut open along the seam. Cells still
  ! share nodes everywhere except across the cut.
  !
  ! Only PET0 calls this, so all NetCDF access here is serial.
  subroutine mpas_to_esmf_mesh(mpasFile, meshFile)
    use netcdf
    implicit none

    character(len=*), intent(in) :: mpasFile
    character(len=*), intent(in) :: meshFile

    ! metres -> kilometres, the Cartesian unit ESMF understands
    real(ESMF_KIND_R8), parameter :: M2KM = 1.0e-3_ESMF_KIND_R8
    ! a shifted corner must land at most this multiple of the largest ordinary
    ! centre-to-corner offset away, otherwise the inferred period is wrong
    real(ESMF_KIND_R8), parameter :: UNWRAP_TOL = 1.5_ESMF_KIND_R8

    integer :: stat, fin, fout, dimid, varid
    integer :: nCells, nVertices, maxEdges
    integer :: iCell, k, v, sx, sy, iNode, nNodes, nc, nReversed

    ! unwrapped corners of the cell in hand, used to fix the winding
    real(ESMF_KIND_R8), allocatable :: cx(:), cy(:)
    integer(ESMF_KIND_I4), allocatable :: nodeIdx(:)

    real(ESMF_KIND_R8), allocatable :: xCell(:), yCell(:)
    real(ESMF_KIND_R8), allocatable :: xVertex(:), yVertex(:)
    integer(ESMF_KIND_I4), allocatable :: verticesOnCell(:,:), nEdgesOnCell(:)

    real(ESMF_KIND_R8), allocatable :: nodeCoords(:,:), centerCoords(:,:)
    integer(ESMF_KIND_I4), allocatable :: elementConn(:,:), numElementConn(:)
    ! nodeMap(sx+2, sy+2, v) -> node index for vertex v shifted by (sx,sy) periods
    integer(ESMF_KIND_I4), allocatable :: nodeMap(:,:,:)

    real(ESMF_KIND_R8) :: xPeriod, yPeriod, rMaxX, rMaxY, resid
    logical :: xPeriodic, yPeriodic

    integer :: dim_nodeCount, dim_elementCount, dim_maxNodePElement, dim_coordDim
    integer :: var_nodeCoords, var_elementConn, var_numElementConn
    integer :: var_centerCoords

    stat = nf90_open(trim(mpasFile), nf90_nowrite, fin)
    call check_nc(stat, 'nf90_open('//trim(mpasFile)//')')

    stat = nf90_inq_dimid(fin, 'nCells', dimid)
    call check_nc(stat, 'nf90_inq_dimid(nCells)')
    stat = nf90_inquire_dimension(fin, dimid, len=nCells)
    call check_nc(stat, 'nf90_inquire_dimension(nCells)')

    stat = nf90_inq_dimid(fin, 'nVertices', dimid)
    call check_nc(stat, 'nf90_inq_dimid(nVertices)')
    stat = nf90_inquire_dimension(fin, dimid, len=nVertices)
    call check_nc(stat, 'nf90_inquire_dimension(nVertices)')

    stat = nf90_inq_dimid(fin, 'maxEdges', dimid)
    call check_nc(stat, 'nf90_inq_dimid(maxEdges)')
    stat = nf90_inquire_dimension(fin, dimid, len=maxEdges)
    call check_nc(stat, 'nf90_inquire_dimension(maxEdges)')

    allocate(xCell(nCells), yCell(nCells))
    allocate(xVertex(nVertices), yVertex(nVertices))
    allocate(nEdgesOnCell(nCells), verticesOnCell(maxEdges, nCells))

    call get_r8(fin, 'xCell', xCell)
    call get_r8(fin, 'yCell', yCell)
    call get_r8(fin, 'xVertex', xVertex)
    call get_r8(fin, 'yVertex', yVertex)

    stat = nf90_inq_varid(fin, 'nEdgesOnCell', varid)
    call check_nc(stat, 'nf90_inq_varid(nEdgesOnCell)')
    stat = nf90_get_var(fin, varid, nEdgesOnCell)
    call check_nc(stat, 'nf90_get_var(nEdgesOnCell)')

    stat = nf90_inq_varid(fin, 'verticesOnCell', varid)
    call check_nc(stat, 'nf90_inq_varid(verticesOnCell)')
    stat = nf90_get_var(fin, varid, verticesOnCell)
    call check_nc(stat, 'nf90_get_var(verticesOnCell)')

    stat = nf90_close(fin)
    call check_nc(stat, 'nf90_close(input)')

    if (any(nEdgesOnCell < 3) .or. any(nEdgesOnCell > maxEdges)) then
       error stop 'Error: nEdgesOnCell outside [3, maxEdges]'
    end if
    if (any(verticesOnCell < 1) .or. any(verticesOnCell > nVertices)) then
       error stop 'Error: verticesOnCell outside [1, nVertices]'
    end if

    ! MPAS records x_period/y_period, but older mesh files (and anything derived
    ! from them) leave them at zero even when the mesh really is periodic, so the
    ! periods are recovered from the geometry instead. An ordinary corner sits
    ! within one cell radius of its centre; a corner across the seam sits nearly a
    ! full period away. The gap between those two populations is enormous, so
    ! splitting on half the coordinate span separates them cleanly, and
    !     period = (smallest wrapped offset) + (largest ordinary offset)
    ! recovers the period using only min/max over large samples.
    call infer_period(xCell, xVertex, xPeriod, rMaxX, xPeriodic)
    call infer_period(yCell, yVertex, yPeriod, rMaxY, yPeriodic)

    if (xPeriodic) write(*,'(A,ES14.6,A)') &
         ' -- mesh is periodic in x, inferred x_period = ', xPeriod, ' m'
    if (yPeriodic) write(*,'(A,ES14.6,A)') &
         ' -- mesh is periodic in y, inferred y_period = ', yPeriod, ' m'

    ! Assign nodes, duplicating any corner that had to be shifted across the seam.
    allocate(nodeMap(3, 3, nVertices))
    allocate(elementConn(maxEdges, nCells), numElementConn(nCells))
    allocate(cx(maxEdges), cy(maxEdges), nodeIdx(maxEdges))
    nodeMap = 0
    nNodes = 0
    nReversed = 0

    do iCell = 1, nCells
       nc = nEdgesOnCell(iCell)
       numElementConn(iCell) = nc

       do k = 1, nc
          v  = verticesOnCell(k, iCell)
          sx = period_shift(xVertex(v), xCell(iCell), xPeriod, xPeriodic)
          sy = period_shift(yVertex(v), yCell(iCell), yPeriod, yPeriodic)

          cx(k) = xVertex(v) + real(sx, ESMF_KIND_R8)*xPeriod
          cy(k) = yVertex(v) + real(sy, ESMF_KIND_R8)*yPeriod

          ! the shift must actually bring the corner home, or the period is wrong
          resid = abs(cx(k) - xCell(iCell))
          if (resid > UNWRAP_TOL*rMaxX) then
             write(*,'(A,I0,A,ES14.6)') ' -- ERROR: cell ', iCell, &
                  ' still has an x corner offset of ', resid
             error stop 'Error: inferred x_period does not unwrap the mesh'
          end if
          resid = abs(cy(k) - yCell(iCell))
          if (resid > UNWRAP_TOL*rMaxY) then
             write(*,'(A,I0,A,ES14.6)') ' -- ERROR: cell ', iCell, &
                  ' still has a y corner offset of ', resid
             error stop 'Error: inferred y_period does not unwrap the mesh'
          end if

          if (nodeMap(sx+2, sy+2, v) == 0) then
             nNodes = nNodes + 1
             nodeMap(sx+2, sy+2, v) = nNodes
          end if
          nodeIdx(k) = nodeMap(sx+2, sy+2, v)
       end do

       ! ESMF wants element corners counter-clockwise. MPAS lists verticesOnCell
       ! clockwise in the x-y plane of a planar mesh, so flip whenever the signed
       ! area comes out negative rather than assuming one convention.
       if (signed_area(cx(1:nc), cy(1:nc)) < 0.0_ESMF_KIND_R8) then
          do k = 1, nc
             elementConn(k, iCell) = nodeIdx(nc + 1 - k)
          end do
          nReversed = nReversed + 1
       else
          elementConn(1:nc, iCell) = nodeIdx(1:nc)
       end if

       ! ESMF reads only numElementConn entries; pad the rest with _FillValue
       do k = nc+1, maxEdges
          elementConn(k, iCell) = -1
       end do
    end do

    allocate(nodeCoords(2, nNodes))
    do v = 1, nVertices
       do sy = -1, 1
          do sx = -1, 1
             iNode = nodeMap(sx+2, sy+2, v)
             if (iNode > 0) then
                nodeCoords(1, iNode) = &
                     (xVertex(v) + real(sx, ESMF_KIND_R8)*xPeriod) * M2KM
                nodeCoords(2, iNode) = &
                     (yVertex(v) + real(sy, ESMF_KIND_R8)*yPeriod) * M2KM
             end if
          end do
       end do
    end do

    allocate(centerCoords(2, nCells))
    centerCoords(1, :) = xCell * M2KM
    centerCoords(2, :) = yCell * M2KM

    write(*,'(A,I0,A,I0,A,I0,A)') ' -- ESMF mesh: ', nCells, ' elements, ', &
         nNodes, ' nodes (', nNodes - nVertices, ' duplicated along the seam)'
    write(*,'(A,I0,A,I0)') ' -- corner order reversed to counter-clockwise for ', &
         nReversed, ' cells of ', nCells

    ! Write the ESMF unstructured grid file. NetCDF declares dimensions
    ! slowest-varying first, so the dimid arrays below are the reverse of the
    ! shape as it appears in ncdump.
    stat = nf90_create(trim(meshFile), ior(nf90_clobber, nf90_netcdf4), fout)
    call check_nc(stat, 'nf90_create('//trim(meshFile)//')')

    stat = nf90_def_dim(fout, 'nodeCount', nNodes, dim_nodeCount)
    call check_nc(stat, 'nf90_def_dim(nodeCount)')
    stat = nf90_def_dim(fout, 'elementCount', nCells, dim_elementCount)
    call check_nc(stat, 'nf90_def_dim(elementCount)')
    stat = nf90_def_dim(fout, 'maxNodePElement', maxEdges, dim_maxNodePElement)
    call check_nc(stat, 'nf90_def_dim(maxNodePElement)')
    stat = nf90_def_dim(fout, 'coordDim', 2, dim_coordDim)
    call check_nc(stat, 'nf90_def_dim(coordDim)')

    stat = nf90_def_var(fout, 'nodeCoords', nf90_double, &
         (/dim_coordDim, dim_nodeCount/), var_nodeCoords)
    call check_nc(stat, 'nf90_def_var(nodeCoords)')
    stat = nf90_put_att(fout, var_nodeCoords, 'units', 'kilometers')
    call check_nc(stat, 'nf90_put_att(nodeCoords:units)')

    stat = nf90_def_var(fout, 'elementConn', nf90_int, &
         (/dim_maxNodePElement, dim_elementCount/), var_elementConn)
    call check_nc(stat, 'nf90_def_var(elementConn)')
    stat = nf90_put_att(fout, var_elementConn, 'long_name', &
         'Node indices that define the element connectivity')
    call check_nc(stat, 'nf90_put_att(elementConn:long_name)')
    stat = nf90_put_att(fout, var_elementConn, '_FillValue', -1)
    call check_nc(stat, 'nf90_put_att(elementConn:_FillValue)')

    stat = nf90_def_var(fout, 'numElementConn', nf90_int, &
         (/dim_elementCount/), var_numElementConn)
    call check_nc(stat, 'nf90_def_var(numElementConn)')
    stat = nf90_put_att(fout, var_numElementConn, 'long_name', &
         'Number of nodes per element')
    call check_nc(stat, 'nf90_put_att(numElementConn:long_name)')

    stat = nf90_def_var(fout, 'centerCoords', nf90_double, &
         (/dim_coordDim, dim_elementCount/), var_centerCoords)
    call check_nc(stat, 'nf90_def_var(centerCoords)')
    stat = nf90_put_att(fout, var_centerCoords, 'units', 'kilometers')
    call check_nc(stat, 'nf90_put_att(centerCoords:units)')

    ! elementArea and elementMask are optional; ESMF derives areas from the
    ! geometry unless addUserArea is requested, so neither is written here.
    stat = nf90_put_att(fout, nf90_global, 'gridType', 'unstructured mesh')
    call check_nc(stat, 'nf90_put_att(gridType)')
    stat = nf90_put_att(fout, nf90_global, 'version', '0.9')
    call check_nc(stat, 'nf90_put_att(version)')
    stat = nf90_put_att(fout, nf90_global, 'inputFile', trim(mpasFile))
    call check_nc(stat, 'nf90_put_att(inputFile)')

    stat = nf90_enddef(fout)
    call check_nc(stat, 'nf90_enddef')

    stat = nf90_put_var(fout, var_nodeCoords, nodeCoords)
    call check_nc(stat, 'nf90_put_var(nodeCoords)')
    stat = nf90_put_var(fout, var_elementConn, elementConn)
    call check_nc(stat, 'nf90_put_var(elementConn)')
    stat = nf90_put_var(fout, var_numElementConn, numElementConn)
    call check_nc(stat, 'nf90_put_var(numElementConn)')
    stat = nf90_put_var(fout, var_centerCoords, centerCoords)
    call check_nc(stat, 'nf90_put_var(centerCoords)')

    stat = nf90_close(fout)
    call check_nc(stat, 'nf90_close(output)')

  contains

    subroutine check_nc(status, where)
      integer, intent(in) :: status
      character(len=*), intent(in) :: where
      if (status /= nf90_noerr) then
         write(*,'(A)') 'NetCDF error in '//trim(where)//': '//trim(nf90_strerror(status))
         error stop 1
      end if
    end subroutine check_nc

    subroutine get_r8(ncid, name, values)
      !> MPAS planar coordinates are stored as float; NetCDF converts on read.
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: values(:)
      integer :: vid, st
      st = nf90_inq_varid(ncid, name, vid)
      call check_nc(st, 'nf90_inq_varid('//name//')')
      st = nf90_get_var(ncid, vid, values)
      call check_nc(st, 'nf90_get_var('//name//')')
    end subroutine get_r8

    subroutine infer_period(cellCoord, vertCoord, period, rMax, isPeriodic)
      real(ESMF_KIND_R8), intent(in)  :: cellCoord(:), vertCoord(:)
      real(ESMF_KIND_R8), intent(out) :: period, rMax
      logical, intent(out) :: isPeriodic

      real(ESMF_KIND_R8) :: half, d, wrapMin
      integer :: ic, kk, vv

      half    = 0.5_ESMF_KIND_R8 * (maxval(vertCoord) - minval(vertCoord))
      rMax    = 0.0_ESMF_KIND_R8
      wrapMin = huge(1.0_ESMF_KIND_R8)

      do ic = 1, nCells
         do kk = 1, nEdgesOnCell(ic)
            vv = verticesOnCell(kk, ic)
            d  = abs(vertCoord(vv) - cellCoord(ic))
            if (d > half) then
               wrapMin = min(wrapMin, d)   ! corner sits across the seam
            else
               rMax    = max(rMax, d)      ! ordinary centre-to-corner offset
            end if
         end do
      end do

      isPeriodic = (wrapMin < huge(1.0_ESMF_KIND_R8))
      if (isPeriodic) then
         period = wrapMin + rMax
      else
         period = 0.0_ESMF_KIND_R8
      end if

      ! guard against a mesh so coarse that no corner counts as "ordinary"
      if (rMax <= 0.0_ESMF_KIND_R8) then
         error stop 'Error: could not measure cell size while inferring period'
      end if
    end subroutine infer_period

    pure function signed_area(px, py) result(a)
      !> shoelace formula; positive when the corners run counter-clockwise
      real(ESMF_KIND_R8), intent(in) :: px(:), py(:)
      real(ESMF_KIND_R8) :: a
      integer :: kk, nk
      nk = size(px)
      a  = 0.0_ESMF_KIND_R8
      do kk = 1, nk
         a = a + px(kk)*py(mod(kk, nk) + 1) - px(mod(kk, nk) + 1)*py(kk)
      end do
      a = 0.5_ESMF_KIND_R8 * a
    end function signed_area

    pure function period_shift(vertCoord, cellCoord, period, isPeriodic) result(s)
      !> whole periods to add to vertCoord to bring it alongside cellCoord
      real(ESMF_KIND_R8), intent(in) :: vertCoord, cellCoord, period
      logical, intent(in) :: isPeriodic
      integer :: s
      if (isPeriodic .and. period > 0.0_ESMF_KIND_R8) then
         s = -nint((vertCoord - cellCoord) / period)
      else
         s = 0
      end if
    end function period_shift

  end subroutine mpas_to_esmf_mesh

  ! Convert MPAS mesh to scrip format. Based on
  ! mpas-dev.github.io/MPAS-Tools/0.24.0/_modules/mpas_tools/scrip/from_mpas.html
  subroutine mpas_to_scrip_mesh(mpasFile, scripFile, useLandIceMask)
    use netcdf
    use iso_fortran_env, only : real64, int32
    implicit none

    character(len=*), intent(in) :: mpasFile
    character(len=*), intent(in) :: scripFile
    logical, intent(in), optional :: useLandIceMask

    logical :: doLandIceMask
    integer :: stat
    integer :: fin, fout
    integer :: dimid, nCells, nVertices, maxVertices
    integer :: varid

    real(ESMF_KIND_R8), parameter :: SHR_CONST_REARTH = 6.37122e6

    ! input arrays
    real(ESMF_KIND_R8), allocatable :: latCell(:), lonCell(:)
    real(ESMF_KIND_R8), allocatable :: latVertex(:), lonVertex(:)
    real(ESMF_KIND_R8), allocatable :: areaCell(:)
    integer(ESMF_KIND_I4), allocatable :: verticesOnCell(:,:)
    integer(ESMF_KIND_I4), allocatable :: nEdgesOnCell(:)
    integer(ESMF_KIND_I4), allocatable :: landIceMask1d(:)
    integer(ESMF_KIND_I4), allocatable :: landIceMask2d(:,:)

    ! output arrays
    real(ESMF_KIND_R8), allocatable :: grid_corner_lat(:,:), grid_corner_lon(:,:)
    real(ESMF_KIND_R8), allocatable :: grid_area(:)
    integer(ESMF_KIND_I4), allocatable :: grid_imask(:)
    integer(ESMF_KIND_I4) :: grid_dims(1)

    ! output variable ids
    integer :: dim_grid_size, dim_grid_corners, dim_grid_rank
    integer :: var_grid_center_lat, var_grid_center_lon
    integer :: var_grid_corner_lat, var_grid_corner_lon
    integer :: var_grid_area, var_grid_imask, var_grid_dims

    ! helpers
    real(ESMF_KIND_R8) :: sphereRadius
    real(ESMF_KIND_R8) :: pi
    character(len=:), allocatable :: on_a_sphere
    integer :: attlen
    integer :: iCell, iVertex, lastValidVertex
    integer :: ndims_landIceMask, dimids_landIceMask(NF90_MAX_VAR_DIMS)

    doLandIceMask = .false.
    if (present(useLandIceMask)) doLandIceMask = useLandIceMask

    if (doLandIceMask) then
       write(*,'(A)') ' -- Landice Masks are enabled'
    else
       write(*,'(A)') ' -- Landice Masks are disabled'
    end if
    write(*,*)

    ! the literal must carry the kind, or this is single-precision pi widened to
    ! real64 -- it now sets the modulo above, not just a range check
    pi = acos(-1.0_ESMF_KIND_R8)

    stat = nf90_open(trim(mpasFile), nf90_nowrite, fin)
    call check_nc(stat, 'nf90_open('//trim(mpasFile)//')')

    stat = nf90_inq_dimid(fin, 'nCells', dimid)
    call check_nc(stat, 'nf90_inq_dimid(nCells)')
    stat = nf90_inquire_dimension(fin, dimid, len=nCells)
    call check_nc(stat, 'nf90_inquire_dimension(nCells)')

    stat = nf90_inq_dimid(fin, 'nVertices', dimid)
    call check_nc(stat, 'nf90_inq_dimid(nVertices)')
    stat = nf90_inquire_dimension(fin, dimid, len=nVertices)
    call check_nc(stat, 'nf90_inquire_dimension(nVertices)')

    stat = nf90_inq_dimid(fin, 'maxEdges', dimid)
    call check_nc(stat, 'nf90_inq_dimid(maxEdges)')
    stat = nf90_inquire_dimension(fin, dimid, len=maxVertices)
    call check_nc(stat, 'nf90_inquire_dimension(maxEdges)')

    allocate(latCell(nCells), lonCell(nCells))
    allocate(latVertex(nVertices), lonVertex(nVertices))
    allocate(areaCell(nCells))
    allocate(verticesOnCell(maxVertices, nCells))
    allocate(nEdgesOnCell(nCells))

    stat = nf90_inq_varid(fin, 'latCell', varid)
    call check_nc(stat, 'nf90_inq_varid(latCell)')
    stat = nf90_get_var(fin, varid, latCell)
    call check_nc(stat, 'nf90_get_var(latCell)')

    stat = nf90_inq_varid(fin, 'lonCell', varid)
    call check_nc(stat, 'nf90_inq_varid(lonCell)')
    stat = nf90_get_var(fin, varid, lonCell)
    call check_nc(stat, 'nf90_get_var(lonCell)')

    stat = nf90_inq_varid(fin, 'latVertex', varid)
    call check_nc(stat, 'nf90_inq_varid(latVertex)')
    stat = nf90_get_var(fin, varid, latVertex)
    call check_nc(stat, 'nf90_get_var(latVertex)')

    stat = nf90_inq_varid(fin, 'lonVertex', varid)
    call check_nc(stat, 'nf90_inq_varid(lonVertex)')
    stat = nf90_get_var(fin, varid, lonVertex)
    call check_nc(stat, 'nf90_get_var(lonVertex)')

    stat = nf90_inq_varid(fin, 'verticesOnCell', varid)
    call check_nc(stat, 'nf90_inq_varid(verticesOnCell)')
    stat = nf90_get_var(fin, varid, verticesOnCell)
    call check_nc(stat, 'nf90_get_var(verticesOnCell)')

    stat = nf90_inq_varid(fin, 'nEdgesOnCell', varid)
    call check_nc(stat, 'nf90_inq_varid(nEdgesOnCell)')
    stat = nf90_get_var(fin, varid, nEdgesOnCell)
    call check_nc(stat, 'nf90_get_var(nEdgesOnCell)')

    stat = nf90_inq_varid(fin, 'areaCell', varid)
    call check_nc(stat, 'nf90_inq_varid(areaCell)')
    stat = nf90_get_var(fin, varid, areaCell)
    call check_nc(stat, 'nf90_get_var(areaCell)')

    stat = nf90_get_att(fin, nf90_global, 'sphere_radius', sphereRadius)
    call check_nc(stat, 'nf90_get_att(sphere_radius)')

    stat = nf90_inquire_attribute(fin, nf90_global, 'on_a_sphere', len=attlen)
    call check_nc(stat, 'nf90_inquire_attribute(on_a_sphere)')
    allocate(character(len=attlen) :: on_a_sphere)
    stat = nf90_get_att(fin, nf90_global, 'on_a_sphere', on_a_sphere)
    call check_nc(stat, 'nf90_get_att(on_a_sphere)')

    ! MPAS is not consistent about longitude convention, and can even mix the two
    ! within a single file: the SGP regional mesh stores lonCell in [0, 2pi) but
    ! lonVertex in [-pi, pi]. SCRIP needs one convention, and leaving cell centres
    ! and their own corners half a turn apart would be worse than either. Fold
    ! both into [0, 2pi) rather than rejecting the file -- Fortran's modulo()
    ! takes the sign of its second argument, so negative longitudes come back
    ! positive.
    if (any(lonCell < 0.0_ESMF_KIND_R8) .or. any(lonVertex < 0.0_ESMF_KIND_R8)) then
       write(*,'(A)') ' -- folding negative longitudes into [0, 2pi)'
    end if
    lonCell   = modulo(lonCell,   2.0_ESMF_KIND_R8*pi)
    lonVertex = modulo(lonVertex, 2.0_ESMF_KIND_R8*pi)

    if (sphereRadius <= 0.0) then
       sphereRadius = SHR_CONST_REARTH
       write(*,'(A,ES24.16)') ' -- WARNING: sphereRadius<=0 so setting sphereRadius = ', &
            SHR_CONST_REARTH
    end if

    if (trim(on_a_sphere) == 'NO') then
       write(*,'(A)') " -- WARNING: 'on_a_sphere' attribute is 'NO', which means there may be some disagreement regarding area between the planar (source) and spherical (target) mesh"
       error stop "Error: planar MPAS mesh cannot be converted to SCRIP format"
    end if

    if (doLandIceMask) then
       stat = nf90_inq_varid(fin, 'landIceMask', varid)
       call check_nc(stat, 'nf90_inq_varid(landIceMask)')

       stat = nf90_inquire_variable(fin, varid, ndims=ndims_landIceMask, dimids=dimids_landIceMask)
       call check_nc(stat, 'nf90_inquire_variable(landIceMask)')

       if (ndims_landIceMask == 1) then
          allocate(landIceMask1d(nCells))
          stat = nf90_get_var(fin, varid, landIceMask1d)
          call check_nc(stat, 'nf90_get_var(landIceMask 1D)')
       else if (ndims_landIceMask == 2) then
          allocate(landIceMask2d(1, nCells))
          stat = nf90_get_var(fin, varid, landIceMask2d, start=(/1,1/), count=(/1,nCells/))
          call check_nc(stat, 'nf90_get_var(landIceMask 2D first slice)')
       else
          error stop 'landIceMask has unsupported rank'
       end if
    end if

    ! allocate(grid_corner_lat(nCells, maxVertices))
    ! allocate(grid_corner_lon(nCells, maxVertices))
    allocate(grid_area(nCells))
    allocate(grid_imask(nCells))

    ! grid_corner_lat = 0.0
    ! grid_corner_lon = 0.0
    grid_area = areaCell / (sphereRadius*sphereRadius)
    grid_dims(1) = nCells


    allocate(grid_corner_lat(maxVertices, nCells))
    allocate(grid_corner_lon(maxVertices, nCells))

    grid_corner_lat = 0.0
    grid_corner_lon = 0.0

    do iCell = 1, nCells
       lastValidVertex = verticesOnCell(nEdgesOnCell(iCell), iCell)

       do iVertex = 1, maxVertices
          if (iVertex <= nEdgesOnCell(iCell)) then
             grid_corner_lat(iVertex, iCell) = latVertex(verticesOnCell(iVertex, iCell))
             grid_corner_lon(iVertex, iCell) = lonVertex(verticesOnCell(iVertex, iCell))
          else
             grid_corner_lat(iVertex, iCell) = latVertex(lastValidVertex)
             grid_corner_lon(iVertex, iCell) = lonVertex(lastValidVertex)
          end if
       end do
    end do


    ! do iCell = 1, nCells
    !   lastValidVertex = verticesOnCell(nEdgesOnCell(iCell), iCell)

    !   do iVertex = 1, maxVertices
    !     if (iVertex <= nEdgesOnCell(iCell)) then
    !       grid_corner_lat(iCell, iVertex) = latVertex(verticesOnCell(iVertex, iCell))
    !       grid_corner_lon(iCell, iVertex) = lonVertex(verticesOnCell(iVertex, iCell))
    !     else
    !       grid_corner_lat(iCell, iVertex) = latVertex(lastValidVertex)
    !       grid_corner_lon(iCell, iVertex) = lonVertex(lastValidVertex)
    !     end if
    !   end do
    ! end do

    if (doLandIceMask) then
       if (allocated(landIceMask1d)) then
          grid_imask = 1 - landIceMask1d
       else
          grid_imask = 1 - landIceMask2d(1,:)
       end if
    else
       grid_imask = 1
    end if

    ! Serial create: only PET0 reaches here now. Creating with comm=MPI_COMM_WORLD
    ! is collective and would hang the other PETs, which are sitting on the
    ! barrier in create_esmf_mesh().
    stat = nf90_create(trim(scripFile), ior(nf90_clobber, nf90_netcdf4), fout)
    call check_nc(stat, 'nf90_create('//trim(scripFile)//', NETCDF4)')

    stat = nf90_def_dim(fout, 'grid_size',    nCells,      dim_grid_size)
    call check_nc(stat, 'nf90_def_dim(grid_size)')
    stat = nf90_def_dim(fout, 'grid_corners', maxVertices, dim_grid_corners)
    call check_nc(stat, 'nf90_def_dim(grid_corners)')
    stat = nf90_def_dim(fout, 'grid_rank',    1,           dim_grid_rank)
    call check_nc(stat, 'nf90_def_dim(grid_rank)')

    stat = nf90_def_var(fout, 'grid_center_lat', nf90_double, (/dim_grid_size/), var_grid_center_lat)
    call check_nc(stat, 'nf90_def_var(grid_center_lat)')
    stat = nf90_put_att(fout, var_grid_center_lat, 'units', 'radians')
    call check_nc(stat, 'nf90_put_att(grid_center_lat:units)')

    stat = nf90_def_var(fout, 'grid_center_lon', nf90_double, (/dim_grid_size/), var_grid_center_lon)
    call check_nc(stat, 'nf90_def_var(grid_center_lon)')
    stat = nf90_put_att(fout, var_grid_center_lon, 'units', 'radians')
    call check_nc(stat, 'nf90_put_att(grid_center_lon:units)')

    stat = nf90_def_var(fout, 'grid_corner_lat', nf90_double, &
         &(/dim_grid_corners,dim_grid_size /), var_grid_corner_lat)
    call check_nc(stat, 'nf90_def_var(grid_corner_lat)')
    stat = nf90_put_att(fout, var_grid_corner_lat, 'units', 'radians')
    call check_nc(stat, 'nf90_put_att(grid_corner_lat:units)')

    stat = nf90_def_var(fout, 'grid_corner_lon', nf90_double, &
         &(/dim_grid_corners,dim_grid_size/), var_grid_corner_lon)
    call check_nc(stat, 'nf90_def_var(grid_corner_lon)')
    stat = nf90_put_att(fout, var_grid_corner_lon, 'units', 'radians')
    call check_nc(stat, 'nf90_put_att(grid_corner_lon:units)')

    stat = nf90_def_var(fout, 'grid_area', nf90_double, (/dim_grid_size/), var_grid_area)
    call check_nc(stat, 'nf90_def_var(grid_area)')
    stat = nf90_put_att(fout, var_grid_area, 'units', 'radian^2')
    call check_nc(stat, 'nf90_put_att(grid_area:units)')

    stat = nf90_def_var(fout, 'grid_imask', nf90_int, (/dim_grid_size/), var_grid_imask)
    call check_nc(stat, 'nf90_def_var(grid_imask)')
    stat = nf90_put_att(fout, var_grid_imask, 'units', 'unitless')
    call check_nc(stat, 'nf90_put_att(grid_imask:units)')

    stat = nf90_def_var(fout, 'grid_dims', nf90_int, (/dim_grid_rank/), var_grid_dims)
    call check_nc(stat, 'nf90_def_var(grid_dims)')

    stat = nf90_enddef(fout)
    call check_nc(stat, 'nf90_enddef')

    stat = nf90_put_var(fout, var_grid_center_lat, latCell)
    call check_nc(stat, 'nf90_put_var(grid_center_lat)')
    stat = nf90_put_var(fout, var_grid_center_lon, lonCell)
    call check_nc(stat, 'nf90_put_var(grid_center_lon)')
    stat = nf90_put_var(fout, var_grid_corner_lat, grid_corner_lat)
    call check_nc(stat, 'nf90_put_var(grid_corner_lat)')
    stat = nf90_put_var(fout, var_grid_corner_lon, grid_corner_lon)
    call check_nc(stat, 'nf90_put_var(grid_corner_lon)')
    stat = nf90_put_var(fout, var_grid_area, grid_area)
    call check_nc(stat, 'nf90_put_var(grid_area)')
    stat = nf90_put_var(fout, var_grid_imask, grid_imask)
    call check_nc(stat, 'nf90_put_var(grid_imask)')
    stat = nf90_put_var(fout, var_grid_dims, grid_dims)
    call check_nc(stat, 'nf90_put_var(grid_dims)')

    stat = nf90_close(fin)
    call check_nc(stat, 'nf90_close(input)')
    stat = nf90_close(fout)
    call check_nc(stat, 'nf90_close(output)')

  contains

    subroutine check_nc(status, where)
      integer, intent(in) :: status
      character(len=*), intent(in) :: where
      if (status /= nf90_noerr) then
         write(*,'(A)') 'NetCDF error in '//trim(where)//': '//trim(nf90_strerror(status))
         error stop 1
      end if
    end subroutine check_nc

  end subroutine mpas_to_scrip_mesh

end module mpas_nuopc_utils
