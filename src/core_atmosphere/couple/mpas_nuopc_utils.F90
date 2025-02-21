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
    use mpas_derived_types, only: domain_type
    type(domain_type), intent(in)    :: domain
    type(ESMF_Mesh) :: mesh

    character(len=:), allocatable :: mpas_grid_file, scrip_mesh_file
    type(ESMF_DistGrid) :: distgrid
    integer :: rc
    integer :: ncount, nelem
    logical :: exists

    rc = ESMF_SUCCESS

    ! convert mesh from mpas to scrip format
    mpas_grid_file = get_mpas_grid_filename(domain)
    scrip_mesh_file = mpas_to_scrip_filename(mpas_grid_file)

    inquire(file=trim(scrip_mesh_file), exist=exists)
    if (.not. exists) then
       call mpas_to_scrip_mesh(mpas_grid_file, scrip_mesh_file)
    end if

    distgrid  = get_mpas_dist_grid(domain, rc)
    mesh = ESMF_MeshCreate(filename=scrip_mesh_file, &
         elementDistgrid=distgrid, &
         fileformat=ESMF_FILEFORMAT_SCRIP, rc=rc)
    if (check(rc, __LINE__, file)) return

    if (debug) then
       call ESMF_MeshWrite(mesh, "mpas_mesh_from_scrip", rc=rc)
       if (check(rc, __LINE__, file)) return
    end if

    ! Get dimensions/counts
    call ESMF_MeshGet(mesh, nodeCount=ncount, elementCount=nElem, rc=rc)
    if (check(rc, __LINE__, file)) return
    print *, "MPAS: ncount=", ncount, "nelem=", nelem
  end function create_esmf_mesh

  subroutine check_nf90(stat, func)
    integer, intent(in) :: stat
    character(len=*), intent(in) :: func
    if (stat /= nf90_noerr) &
         error stop "NetCDF Error: " // func // "failure"
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
    if (check(rc, __LINE__, file)) return
    call ESMF_VMGet(vm, localPet=rank, petCount=np, rc=rc)
    if (check(rc, __LINE__, file)) return

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

    print *, rank,":gindex=", gindex(1:10)
    ! stop "here"
    dist_grid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
    if (check(rc, __LINE__, file)) return
  end function get_mpas_dist_grid

  function get_mpas_grid_filename(domain) &
       result(mpas_grid_file)
    use mpas_derived_types, only: domain_type, StrKIND
    use mpas_pool_routines, only: mpas_pool_get_config
    type(domain_type), intent(in) :: domain
    character(len=:), allocatable :: mpas_grid_file
    character (len=StrKIND), pointer :: config_block_decomp_file_prefix
    integer :: i

    ! config_block_decomp_file_prefix returns [testcase].graph.info.part
    call mpas_pool_get_config(domain % configs, &
         'config_block_decomp_file_prefix', config_block_decomp_file_prefix)
    i = index(trim(config_block_decomp_file_prefix), 'graph.info.part', &
         back=.true.)
    mpas_grid_file = config_block_decomp_file_prefix(:i-1) // "grid.nc"
  end function get_mpas_grid_filename

  function mpas_to_scrip_filename(mpas_grid_file) result(scrip_mesh_file)
    character(len=*), intent(in) :: mpas_grid_file
    character(len=:), allocatable :: scrip_mesh_file
    integer :: i
    i = index(trim(mpas_grid_file), '.grid.nc', back=.true.)
    if (i == 0) then
       print *, "Error: mpas_grid_file ", trim(mpas_grid_file), &
            " does not have .grid.nc suffix"
       error stop "Error: mpas_grid_file in incorrect format"
    end if
    scrip_mesh_file = mpas_grid_file(:i-1) // ".tmp.scrip.nc"
  end function mpas_to_scrip_filename

  function mpas_to_esmf_mesh_2(distGrid, mpasFile, rc) &
       result(mesh)
    type(ESMF_DistGrid), intent(in)  :: distGrid
    character(len=*), intent(in)  :: mpasFile
    integer, intent(out) :: rc
    type(ESMF_Mesh) :: mesh

    integer :: stat, ncid, varid
    integer :: nCells, nVertices, maxEdges
    integer :: deCount, localDeCount
    integer :: de, lde, deId, deCol
    integer, allocatable :: localDeToDeMap(:)
    integer, allocatable :: minIndexPDe(:,:), maxIndexPDe(:,:)

    real(ESMF_KIND_R8), allocatable :: lonCell(:), latCell(:)
    real(ESMF_KIND_R8), allocatable :: lonVertex(:), latVertex(:)
    real(ESMF_KIND_R8), allocatable :: areaCell(:)

    integer, allocatable :: nEdgesOnCell(:)
    integer, allocatable :: verticesOnCell(:,:)   ! expected as (maxEdges, nCells)

    integer, allocatable :: localCellIds(:)
    integer, allocatable :: nodeIds(:)
    integer, allocatable :: elementIds(:), elementTypes(:), elementConn(:,:)
    integer, allocatable :: vertexToLocal(:)
    logical, allocatable :: vertexUsed(:)

    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
    real(ESMF_KIND_R8), allocatable :: elementCoords(:)
    real(ESMF_KIND_R8), allocatable :: elementArea(:)

    real(ESMF_KIND_R8) :: sphereRadius
    logical :: haveArea

    integer :: i, j, c, v, n, ne, nlen, ncon
    integer :: nLocalCells, nLocalNodes
    integer :: p, nconn, connSize

    type(ESMF_VM)       :: vm
    integer :: rank, np

    !
    integer :: dimid, elementCount, nodeCount
    integer, allocatable :: elementMask(:)
    integer, allocatable :: numElementConn(:)
    ! are these correct?
    integer, parameter :: coordDim = 2
    integer, parameter :: maxNodePElement = 6 ! QU
    ! input attribute
    real(ESMF_KIND_R8) :: radius, areafac, r2d

    rc = ESMF_SUCCESS
    ncid = -1
    haveArea = .false.

    call ESMF_VMGetGlobal(vm, rc=rc)
    if (check(rc, __LINE__, file)) return
    call ESMF_VMGet(vm, localPet=rank, petCount=np, rc=rc)
    if (check(rc, __LINE__, file)) return

    stat = nf90_open(trim(mpasFile), nf90_nowrite, ncid)
    call check_nf90(stat, "open")
    stat = nf90_inq_dimid(ncid,'nCells', dimid)
    call check_nf90(stat, "inq_dimid nCells")
    stat = nf90_inquire_dimension(ncid, dimid, len=elementCount)
    call check_nf90(stat, "inquire_dimension nCells")
    stat = nf90_inq_dimid(ncid,'nVertices', dimid)
    call check_nf90(stat, "inq_dimid nVertices")
    stat = nf90_inquire_dimension(ncid, dimid, len=nodeCount)
    call check_nf90(stat, "inquire_dimension nVertices")

    allocate(latCell(elementCount))
    allocate(lonCell(elementCount))
    allocate(areaCell(elementCount))
    allocate(nEdgesOnCell(elementCount))
    allocate(latVertex(nodeCount))
    allocate(lonVertex(nodeCount))
    allocate(verticesOnCell(maxNodePElement, elementCount))

    allocate(nodeCoords(coordDim*nodeCount))
    allocate(elementCoords(coordDim*elementCount))
    allocate(elementArea(elementCount))
    allocate(elementMask(elementCount))
    allocate(numElementConn(elementCount))
    allocate(elementConn(maxNodePElement, elementCount))

    ! read
    stat = nf90_inq_varid(ncid,'latCell',varid)
    stat = nf90_get_var(ncid, varid, latCell)
    call check_nf90(stat, "get latCell")

    stat = nf90_inq_varid(ncid,'lonCell',varid)
    stat = nf90_get_var(ncid, varid, lonCell)
    call check_nf90(stat, "get lonCell")

    stat = nf90_inq_varid(ncid,'latVertex',varid)
    stat = nf90_get_var(ncid, varid, latVertex)
    call check_nf90(stat, "get latVertex")

    stat = nf90_inq_varid(ncid,'lonVertex',varid)
    stat = nf90_get_var(ncid, varid, lonVertex)
    call check_nf90(stat, "get lonVertex")

    stat = nf90_inq_varid(ncid,'verticesOnCell',varid)
    do n = 1,elementCount,10000
       ne = min(elementCount,n+9999)
       nlen = min(10000,elementCount+1-n)
       stat = nf90_get_var(ncid, varid, verticesOnCell(:,n:ne),(/1,n/),(/maxNodePElement,nlen/))
       call check_nf90(stat, "get verticesOnCell")
    enddo
    stat = nf90_inq_varid(ncid,'areaCell',varid)
    stat = nf90_get_var(ncid, varid, areaCell)
    call check_nf90(stat, "get areaCell")
    stat = nf90_inq_varid(ncid,'nEdgesOnCell',varid)
    stat = nf90_get_var(ncid, varid, nEdgesOnCell)
    call check_nf90(stat, "get nEdgesOnCell")
    stat = nf90_get_att(ncid, nf90_global, 'sphere_radius',radius)
    call check_nf90(stat, "get sphere_radius")

    ! copy, convert units and compute numElementConn
    r2d = 180._8 / (4._8 * atan(1._8))
    areafac = 1. / (radius**2)
    print*,'radius ',radius
    do i = 1,nodeCount
       nodeCoords(i*2-1) = lonVertex(i) * r2d
       nodeCoords(i*2) = latVertex(i) * r2d
    enddo

    do i = 1,elementCount
       elementCoords(i*2-1) = lonCell(i) * r2d
       elementCoords(i*2) = latCell(i) * r2d
       elementArea(i) = areaCell(i) * areafac
       elementMask(i) = 1
       ncon = 0

       !! find n pole
       !if(centerCoords(2,i) > 89.9) then
       !  print*,'n. pole at cell ',i
       !  do m = 1,6
       !    print*,'node ',verticesoncell(m,i),nodecoords(:,verticesoncell(m,i))
       !  enddo
       !endif
       !nodeunique = .true.
       do n = 1,nEdgesOnCell(i)
          !if( verticesOnCell(n,i) <= nodeCount ) then
          ncon = ncon + 1
          elementConn(ncon,i) = verticesOnCell(n,i)
          !endif
          !   !uniqueness test
          !   do m = 1,n-1
          !      if(verticesOnCell(n,i) == verticesOnCell(m,i)) nodeunique = .false.
          !   enddo
       enddo
       !if(.not.nodeunique) print*,'nodes not unique for cell ',i,'nodes ',verticesOnCell(:,i)
       do n = nEdgesOnCell(i)+1,maxNodePElement
          elementConn(n,i) = -1
       enddo
       numElementConn(i) = ncon
    enddo

    ! mesh = ESMF_MeshCreate( &
    !      parametricDim = 2, &
    !      spatialDim    = 2, &
    !      ! nodeIds       = nodeIds, &
    !      nodeIds       = gindex, &
    !      nodeCoords    = nodeCoords, &
    !      elementIds    = elementIds, &
    !      elementTypes  = elementTypes, &
    !      elementConn   = elementConn, &
    !      elementMask   = elementMask, &
    !      elementArea   = elementArea, &
    !      elementCoords = elementCoords, &
    !      elementDistgrid = distGrid, &
    !      coordSys      = ESMF_COORDSYS_SPH_RAD, &
    !      name          = "MPAS mesh from arrays", &
    !      rc            = rc)

  end function mpas_to_esmf_mesh_2


function mpas_to_esmf_mesh_3(distGrid, mpasFile, rc) result(mesh)
  implicit none

  type(ESMF_DistGrid), intent(in) :: distGrid
  character(len=*),    intent(in) :: mpasFile
  integer,             intent(out) :: rc
  type(ESMF_Mesh) :: mesh

  integer :: stat, ncid, varid, dimid
  integer :: nCells, nVertices, maxEdges
  integer :: deCount, localDeCount
  integer :: lde, deId, deCol
  integer :: i, j, c, v, p, nconn
  integer :: localElementCount, localNodeCount, connSize
  integer :: localrc

  integer, allocatable :: localDeToDeMap(:)
  integer, allocatable :: minIndexPDe(:,:), maxIndexPDe(:,:)

  real(ESMF_KIND_R8), allocatable :: latCell(:), lonCell(:)
  real(ESMF_KIND_R8), allocatable :: latVertex(:), lonVertex(:)
  real(ESMF_KIND_R8), allocatable :: areaCell(:)

  integer, allocatable :: nEdgesOnCell(:)
  integer, allocatable :: verticesOnCell(:,:)   ! (maxEdges, nCells)

  integer, allocatable :: localCellIds(:)
  logical, allocatable :: vertexUsed(:)
  integer, allocatable :: vertexToLocal(:)

  integer, allocatable :: nodeIds(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)

  integer, allocatable :: elementIds(:)
  integer, allocatable :: elementTypes(:)
  integer, allocatable :: elementConn(:)
  integer, allocatable :: elementMask(:)
  real(ESMF_KIND_R8), allocatable :: elementCoords(:)
  real(ESMF_KIND_R8), allocatable :: elementArea(:)

  real(ESMF_KIND_R8) :: sphere_radius

  rc = ESMF_FAILURE
  ncid = -1

  !--------------------------------------------------------------
  ! 1. Get local element ids from the caller-provided DistGrid.
  !    Assumes the DistGrid sequence indices are the MPAS cell ids.
  !--------------------------------------------------------------
  call ESMF_DistGridGet(distGrid, deCount=deCount, localDeCount=localDeCount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) then
    rc = localrc
    return
  end if

  allocate(localDeToDeMap(localDeCount))
  allocate(minIndexPDe(1, deCount))
  allocate(maxIndexPDe(1, deCount))

  call ESMF_DistGridGet(distGrid, &
       localDeToDeMap=localDeToDeMap, &
       minIndexPDe=minIndexPDe, &
       maxIndexPDe=maxIndexPDe, &
       rc=localrc)
  if (localrc /= ESMF_SUCCESS) then
    rc = localrc
    return
  end if

  localElementCount = 0
  do lde = 1, localDeCount
    deId  = localDeToDeMap(lde)   ! ESMF DE id value
    deCol = deId + 1              ! convert to normal Fortran column index
    localElementCount = localElementCount + &
         (maxIndexPDe(1,deCol) - minIndexPDe(1,deCol) + 1)
  end do

  allocate(localCellIds(localElementCount))

  p = 0
  do lde = 1, localDeCount
    deId  = localDeToDeMap(lde)
    deCol = deId + 1
    do c = minIndexPDe(1,deCol), maxIndexPDe(1,deCol)
      p = p + 1
      localCellIds(p) = c
    end do
  end do

  !--------------------------------------------------------------
  ! 2. Open MPAS mesh file and read global mesh arrays.
  !--------------------------------------------------------------
  stat = nf90_open(trim(mpasFile), nf90_nowrite, ncid)
  if (stat /= nf90_noerr) then
    write(*,*) 'nf90_open failed: ', trim(nf90_strerror(stat))
    return
  end if

  stat = nf90_inq_dimid(ncid, 'nCells', dimid)
  if (stat /= nf90_noerr) then
    write(*,*) 'inq_dimid nCells failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if
  stat = nf90_inquire_dimension(ncid, dimid, len=nCells)
  if (stat /= nf90_noerr) then
    write(*,*) 'inquire_dimension nCells failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_inq_dimid(ncid, 'nVertices', dimid)
  if (stat /= nf90_noerr) then
    write(*,*) 'inq_dimid nVertices failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if
  stat = nf90_inquire_dimension(ncid, dimid, len=nVertices)
  if (stat /= nf90_noerr) then
    write(*,*) 'inquire_dimension nVertices failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_inq_dimid(ncid, 'maxEdges', dimid)
  if (stat /= nf90_noerr) then
    write(*,*) 'inq_dimid maxEdges failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if
  stat = nf90_inquire_dimension(ncid, dimid, len=maxEdges)
  if (stat /= nf90_noerr) then
    write(*,*) 'inquire_dimension maxEdges failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  allocate(latCell(nCells), lonCell(nCells), areaCell(nCells))
  allocate(nEdgesOnCell(nCells))
  allocate(latVertex(nVertices), lonVertex(nVertices))
  allocate(verticesOnCell(maxEdges, nCells))

  stat = nf90_inq_varid(ncid, 'latCell', varid)
  stat = nf90_get_var(ncid, varid, latCell)
  if (stat /= nf90_noerr) then
    write(*,*) 'get latCell failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_inq_varid(ncid, 'lonCell', varid)
  stat = nf90_get_var(ncid, varid, lonCell)
  if (stat /= nf90_noerr) then
    write(*,*) 'get lonCell failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_inq_varid(ncid, 'latVertex', varid)
  stat = nf90_get_var(ncid, varid, latVertex)
  if (stat /= nf90_noerr) then
    write(*,*) 'get latVertex failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_inq_varid(ncid, 'lonVertex', varid)
  stat = nf90_get_var(ncid, varid, lonVertex)
  if (stat /= nf90_noerr) then
    write(*,*) 'get lonVertex failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_inq_varid(ncid, 'verticesOnCell', varid)
  stat = nf90_get_var(ncid, varid, verticesOnCell)
  if (stat /= nf90_noerr) then
    write(*,*) 'get verticesOnCell failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_inq_varid(ncid, 'areaCell', varid)
  stat = nf90_get_var(ncid, varid, areaCell)
  if (stat /= nf90_noerr) then
    write(*,*) 'get areaCell failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_inq_varid(ncid, 'nEdgesOnCell', varid)
  stat = nf90_get_var(ncid, varid, nEdgesOnCell)
  if (stat /= nf90_noerr) then
    write(*,*) 'get nEdgesOnCell failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_get_att(ncid, nf90_global, 'sphere_radius', sphere_radius)
  if (stat /= nf90_noerr) then
    write(*,*) 'get sphere_radius failed: ', trim(nf90_strerror(stat))
    stat = nf90_close(ncid)
    return
  end if

  stat = nf90_close(ncid)
  if (stat /= nf90_noerr) then
    write(*,*) 'nf90_close failed: ', trim(nf90_strerror(stat))
    return
  end if
  ncid = -1

  !--------------------------------------------------------------
  ! 3. Build the PET-local node list from PET-local cells.
  !    nodeIds are global MPAS vertex ids.
  !    elementConn must point to LOCAL positions in nodeIds.
  !--------------------------------------------------------------
  allocate(vertexUsed(nVertices))
  allocate(vertexToLocal(nVertices))
  vertexUsed    = .false.
  vertexToLocal = 0

  connSize = 0
  do i = 1, localElementCount
    c = localCellIds(i)
    if (c < 1 .or. c > nCells) then
      write(*,*) 'local cell id out of range: ', c
      return
    end if

    connSize = connSize + nEdgesOnCell(c)

    do j = 1, nEdgesOnCell(c)
      v = verticesOnCell(j, c)   ! MPAS file is already 1-based for Fortran
      if (v < 1 .or. v > nVertices) then
        write(*,*) 'vertex id out of range: cell=', c, ' edge=', j, ' vertex=', v
        return
      end if
      vertexUsed(v) = .true.
    end do
  end do

  localNodeCount = count(vertexUsed)

  allocate(nodeIds(localNodeCount))
  allocate(nodeCoords(2*localNodeCount))

  ! print *, rank, ": localNodeCount", localNodeCount



  ! stop "hi"
  p = 0
  do v = 1, nVertices
    if (vertexUsed(v)) then
      p = p + 1
      nodeIds(p)        = v
      nodeCoords(2*p-1) = lonVertex(v)
      nodeCoords(2*p  ) = latVertex(v)
      vertexToLocal(v)  = p
    end if
  end do

  !--------------------------------------------------------------
  ! 4. Build PET-local element arrays.
  !--------------------------------------------------------------
  allocate(elementIds(localElementCount))
  allocate(elementTypes(localElementCount))
  allocate(elementMask(localElementCount))
  allocate(elementArea(localElementCount))
  allocate(elementCoords(2*localElementCount))
  allocate(elementConn(connSize))

  nconn = 0
  do i = 1, localElementCount
    c = localCellIds(i)

    elementIds(i)        = c
    elementTypes(i)      = nEdgesOnCell(c)
    elementMask(i)       = 1
    elementArea(i)       = areaCell(c) / (sphere_radius * sphere_radius)
    elementCoords(2*i-1) = lonCell(c)
    elementCoords(2*i  ) = latCell(c)

    do j = 1, nEdgesOnCell(c)
      v = verticesOnCell(j, c)
      nconn = nconn + 1
      elementConn(nconn) = vertexToLocal(v)
    end do
  end do

  !--------------------------------------------------------------
  ! 5. Create the mesh.
  !--------------------------------------------------------------
  mesh = ESMF_MeshCreate( &
       parametricDim  = 2, &
       spatialDim     = 2, &
       ! nodeIds        = nodeIds, &
       nodeIds        = gindex, &
       ! nodeCoords     = nodeCoords, &
       nodeCoords     = elementCoords, &
       ! nodalDistgrid = distGrid, &
       elementIds     = elementIds, &
       elementTypes   = elementTypes, &
       elementConn    = elementConn, &
       elementMask    = elementMask, &
       elementArea    = elementArea, &
       ! elementCoords  = elementCoords, &
       ! elementDistgrid = distGrid, &
       coordSys       = ESMF_COORDSYS_SPH_RAD, &
       name           = 'MPAS mesh from arrays', &
       rc             = localrc)

  rc = localrc
  print *, "localcellid", localcellids(1:10)
  print *, "gindex shape", shape(gindex)

end function mpas_to_esmf_mesh_3



  function mpas_to_esmf_mesh(cellDistGrid, mpasFile, rc) &
       result(mesh)
    type(ESMF_DistGrid), intent(in)  :: cellDistGrid
    character(len=*), intent(in)  :: mpasFile
    integer, intent(out) :: rc
    type(ESMF_Mesh) :: mesh

    integer :: status, ncid
    integer :: nCells, nVertices, maxEdges
    integer :: deCount, localDeCount
    integer :: de, lde, deId, deCol
    integer, allocatable :: localDeToDeMap(:)
    integer, allocatable :: minIndexPDe(:,:), maxIndexPDe(:,:)

    real(ESMF_KIND_R8), allocatable :: lonCell(:), latCell(:)
    real(ESMF_KIND_R8), allocatable :: lonVertex(:), latVertex(:)
    real(ESMF_KIND_R8), allocatable :: areaCell(:)

    integer, allocatable :: nEdgesOnCell(:)
    integer, allocatable :: verticesOnCell(:,:)   ! expected as (maxEdges, nCells)

    integer, allocatable :: localCellIds(:)
    integer, allocatable :: nodeIds(:)
    integer, allocatable :: elementIds(:), elementTypes(:), elementConn(:)
    integer, allocatable :: vertexToLocal(:)
    logical, allocatable :: vertexUsed(:)

    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
    real(ESMF_KIND_R8), allocatable :: elementCoords(:)
    real(ESMF_KIND_R8), allocatable :: elementArea(:)

    real(ESMF_KIND_R8) :: sphereRadius
    logical :: haveArea

    integer :: i, j, c, v
    integer :: nLocalCells, nLocalNodes
    integer :: p, nconn, connSize

    type(ESMF_VM)       :: vm
    integer :: rank, np

    rc = ESMF_SUCCESS
    ncid = -1
    haveArea = .false.

    call ESMF_VMGetGlobal(vm, rc=rc)
    if (check(rc, __LINE__, file)) return
    call ESMF_VMGet(vm, localPet=rank, petCount=np, rc=rc)
    if (check(rc, __LINE__, file)) return


    ! 1. Query the caller-provided DistGrid.
    !    Assumption: 1-D DistGrid over MPAS cell ids 1:nCells.
    call ESMF_DistGridGet(cellDistGrid, deCount=deCount, &
                          localDeCount=localDeCount, rc=rc)
    if (check(rc, __LINE__, file)) return

    allocate(localDeToDeMap(localDeCount))
    allocate(minIndexPDe(1, deCount))
    allocate(maxIndexPDe(1, deCount))

    call ESMF_DistGridGet(cellDistGrid,                  &
                          localDeToDeMap=localDeToDeMap, &
                          minIndexPDe=minIndexPDe,       &
                          maxIndexPDe=maxIndexPDe,       &
                          rc=rc)
    if (check(rc, __LINE__, file)) return

    print *, rank, ": decount", decount
    print *, rank, ": localdecount", localdecount
    print *, rank, ": localdetodemap", localDeToDeMap
    print *, rank, ": minIndexPDe", minIndexPDe
    print *, rank, ": maxIndexPDe", maxIndexPDe

    stop "is it right"

    nLocalCells = 0
    do lde = 1, localDeCount
      deId  = localDeToDeMap(lde)   ! ESMF DE id value
      deCol = deId + 1              ! convert to 1-based Fortran column index
      nLocalCells = nLocalCells + (maxIndexPDe(1,deCol) - minIndexPDe(1,deCol) + 1)
    end do

    allocate(localCellIds(nLocalCells))
    p = 0
    do lde = 1, localDeCount
      deId  = localDeToDeMap(lde)
      deCol = deId + 1
      do c = minIndexPDe(1,deCol), maxIndexPDe(1,deCol)
        p = p + 1
        localCellIds(p) = c
      end do
    end do

    ! 2. Read MPAS mesh arrays from file.
    status = nf90_open(trim(mpasFile), nf90_nowrite, ncid)
    if (status /= nf90_noerr) then
       print *, "Error: unable to open ", trim(mpasFile)
       error stop "Error: unable to open NetCDF file"
    end if


    call get_dim(ncid, "nCells", nCells, status)
    if (status /= nf90_noerr) error stop "Error: get dim nCells"

    call get_dim(ncid, "nVertices", nVertices, status)
    if (status /= nf90_noerr) error stop "Error: get dim nVertices"

    call get_dim(ncid, "maxEdges", maxEdges, status)
    if (status /= nf90_noerr) error stop "Error: get dim maxEdges"

    if (nLocalCells > 0) then
      if (maxval(localCellIds) > nCells .or. minval(localCellIds) < 1) then
        write(*,*) "mpas_to_esmf_mesh: DistGrid cell ids inconsistent with nCells"
        rc = ESMF_FAILURE
        return
      end if
    end if

    allocate(lonCell(nCells), latCell(nCells))
    allocate(lonVertex(nVertices), latVertex(nVertices))
    allocate(areaCell(nCells))
    allocate(nEdgesOnCell(nCells))
    allocate(verticesOnCell(maxEdges, nCells))

    call read_var_1d_r8(ncid, "lonCell", lonCell, status)
    if (status /= nf90_noerr) error stop "Error: read lonCell"

    call read_var_1d_r8(ncid, "latCell", latCell, status)
    if (status /= nf90_noerr) error stop "Error: read latCell"

    call read_var_1d_r8(ncid, "lonVertex", lonVertex, status)
    if (status /= nf90_noerr) error stop "Error: read lonVertex"

    call read_var_1d_r8(ncid, "latVertex", latVertex, status)
    if (status /= nf90_noerr) error stop "Error: read latVertex"

    call read_var_1d_i4(ncid, "nEdgesOnCell", nEdgesOnCell, status)
    if (status /= nf90_noerr) error stop "Error: read nEdgesOnCell"

    call read_var_2d_i4(ncid, "verticesOnCell", verticesOnCell, status)
    if (status /= nf90_noerr) error stop "Error: read verticesOnCell"

    call try_read_var_1d_r8(ncid, "areaCell", areaCell, status)
    if (status == nf90_noerr) then
      status = nf90_get_att(ncid, nf90_global, "sphere_radius", sphereRadius)
      if (status == nf90_noerr .and. sphereRadius > 0.0_ESMF_KIND_R8) then
        haveArea = .true.
      else
        haveArea = .false.
      end if
    else
      haveArea = .false.
    end if

    status = nf90_close(ncid)
    if (status /= nf90_noerr) error stop "Error: calling nf90_close"
    ncid = -1

    ! 3. Build PET-local node list from PET-local cells.
    allocate(vertexUsed(nVertices))
    allocate(vertexToLocal(nVertices))
    vertexUsed    = .false.
    vertexToLocal = 0

    connSize = 0
    do i = 1, nLocalCells
       c = localCellIds(i)
       connSize = connSize + nEdgesOnCell(c)

       do j = 1, nEdgesOnCell(c)
          v = verticesOnCell(j, c)
          if (v < 1 .or. v > nVertices) then
             write(*,*) "mpas_to_esmf_mesh: bad vertex id:", &
                  " cell=", c, " edge=", j, " vertex=", v
             rc = ESMF_FAILURE
             return
          end if
          vertexUsed(v) = .true.
       end do
    end do

    nLocalNodes = count(vertexUsed)

    allocate(nodeIds(nLocalNodes))
    allocate(nodeCoords(2*nLocalNodes))

    p = 0
    do v = 1, nVertices
       if (vertexUsed(v)) then
          p = p + 1
          nodeIds(p)        = v
          nodeCoords(2*p-1) = lonVertex(v)
          nodeCoords(2*p)   = latVertex(v)
          vertexToLocal(v)  = p
       end if
    end do

    ! 4. Build PET-local element arrays.
    allocate(elementIds(nLocalCells))
    allocate(elementTypes(nLocalCells))
    allocate(elementCoords(2*nLocalCells))
    allocate(elementConn(connSize))
    if (haveArea) allocate(elementArea(nLocalCells))

    nconn = 0
    do i = 1, nLocalCells
       c = localCellIds(i)

       elementIds(i)        = c
       elementTypes(i)      = nEdgesOnCell(c)
       elementCoords(2*i-1) = lonCell(c)
       elementCoords(2*i)   = latCell(c)

       if (haveArea) elementArea(i) = areaCell(c) / (sphereRadius*sphereRadius)

       do j = 1, nEdgesOnCell(c)
          v = verticesOnCell(j, c)
          nconn = nconn + 1
          elementConn(nconn) = vertexToLocal(v)
       end do
    end do

    ! 5. Create the ESMF mesh.
    mesh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, &
         coordSys=ESMF_COORDSYS_SPH_RAD, &
         name="MPAS mesh", rc=rc)
    if (check(rc, __LINE__, file)) return

    call ESMF_MeshAddNodes(mesh, &
         nodeIds=nodeIds, &
         nodeCoords=nodeCoords, &
         rc=rc)
    if (check(rc, __LINE__, file)) return

    if (haveArea) then
       call ESMF_MeshAddElements(mesh, &
            elementIds=elementIds, &
            elementTypes=elementTypes, &
            elementConn=elementConn, &
            elementArea=elementArea, &
            elementCoords=elementCoords, &
            elementDistgrid=cellDistGrid, &
            rc=rc)
       if (check(rc, __LINE__, file)) return
    else
       call ESMF_MeshAddElements(mesh, &
            elementIds=elementIds, &
            elementTypes=elementTypes, &
            elementConn=elementConn, &
            elementCoords=elementCoords, &
            elementDistgrid=cellDistGrid, &
            rc=rc)
       if (check(rc, __LINE__, file)) return
    end if

  contains

    subroutine get_dim(ncid, name, n, status)
      integer,          intent(in)  :: ncid
      character(len=*), intent(in)  :: name
      integer,          intent(out) :: n
      integer,          intent(out) :: status
      integer :: dimid

      status = nf90_inq_dimid(ncid, trim(name), dimid)
      if (status /= nf90_noerr) return
      status = nf90_inquire_dimension(ncid, dimid, len=n)
    end subroutine get_dim

    subroutine read_var_1d_r8(ncid, name, a, status)
      integer,          intent(in)  :: ncid
      character(len=*), intent(in)  :: name
      real(ESMF_KIND_R8), intent(out) :: a(:)
      integer,          intent(out) :: status
      integer :: varid

      status = nf90_inq_varid(ncid, trim(name), varid)
      if (status /= nf90_noerr) return
      status = nf90_get_var(ncid, varid, a)
    end subroutine read_var_1d_r8

    subroutine try_read_var_1d_r8(ncid, name, a, status)
      integer,          intent(in)  :: ncid
      character(len=*), intent(in)  :: name
      real(ESMF_KIND_R8), intent(out) :: a(:)
      integer,          intent(out) :: status
      integer :: varid

      status = nf90_inq_varid(ncid, trim(name), varid)
      if (status /= nf90_noerr) return
      status = nf90_get_var(ncid, varid, a)
    end subroutine try_read_var_1d_r8

    subroutine read_var_1d_i4(ncid, name, a, status)
      integer,          intent(in)  :: ncid
      character(len=*), intent(in)  :: name
      integer,          intent(out) :: a(:)
      integer,          intent(out) :: status
      integer :: varid

      status = nf90_inq_varid(ncid, trim(name), varid)
      if (status /= nf90_noerr) return
      status = nf90_get_var(ncid, varid, a)
    end subroutine read_var_1d_i4

    subroutine read_var_2d_i4(ncid, name, a, status)
      integer,          intent(in)  :: ncid
      character(len=*), intent(in)  :: name
      integer,          intent(out) :: a(:,:)
      integer,          intent(out) :: status
      integer :: varid

      status = nf90_inq_varid(ncid, trim(name), varid)
      if (status /= nf90_noerr) return
      status = nf90_get_var(ncid, varid, a)
    end subroutine read_var_2d_i4

  end function mpas_to_esmf_mesh




  ! subroutine scrip_from_mpas(mpasFile, scripFile, useLandIceMask)
  !   character(len=*), intent(in) :: mpasFile
  !   character(len=*), intent(in) :: scripFile
  !   logical, intent(in), optional :: useLandIceMask

  !   logical :: doLandIceMask
  !   integer :: stat
  !   integer :: fin, fout
  !   integer :: dimid, nCells, nVertices, maxVertices
  !   integer :: varid

  !   ! input arrays
  !   real(ESMF_KIND_R8), allocatable :: latCell(:), lonCell(:)
  !   real(ESMF_KIND_R8), allocatable :: latVertex(:), lonVertex(:)
  !   real(ESMF_KIND_R8), allocatable :: areaCell(:)
  !   integer(ESMF_KIND_I4), allocatable :: verticesOnCell(:,:)
  !   integer(ESMF_KIND_I4), allocatable :: nEdgesOnCell(:)
  !   integer(ESMF_KIND_I4), allocatable :: landIceMask1d(:)
  !   integer(ESMF_KIND_I4), allocatable :: landIceMask2d(:,:)

  !   ! output arrays
  !   real(ESMF_KIND_R8), allocatable :: grid_corner_lat(:,:), grid_corner_lon(:,:)
  !   real(ESMF_KIND_R8), allocatable :: grid_area(:)
  !   integer(ESMF_KIND_I4), allocatable :: grid_imask(:)
  !   integer(ESMF_KIND_I4) :: grid_dims(1)

  ! real(ESMF_KIND_R8), parameter :: SHR_CONST_REARTH = 6.37122e6_rk

  !   ! output variable ids
  !   integer :: dim_grid_size, dim_grid_corners, dim_grid_rank
  !   integer :: var_grid_center_lat, var_grid_center_lon
  !   integer :: var_grid_corner_lat, var_grid_corner_lon
  !   integer :: var_grid_area, var_grid_imask, var_grid_dims

  !   ! attributes / helpers
  !   real(ESMF_KIND_R8) :: sphereRadius
  !   real(ESMF_KIND_R8) :: pi
  !   character(len=:), allocatable :: on_a_sphere
  !   integer :: attlen
  !   integer :: iCell, iVertex, lastValidVertex
  !   integer :: ndims_landIceMask, dimids_landIceMask(NF90_MAX_VAR_DIMS)

  !   doLandIceMask = .false.
  !   if (present(useLandIceMask)) doLandIceMask = useLandIceMask

  !   if (doLandIceMask) then
  !     write(*,'(A)') ' -- Landice Masks are enabled'
  !   else
  !     write(*,'(A)') ' -- Landice Masks are disabled'
  !   end if
  !   write(*,*)

  !   pi = acos(-1.0_rk)

  !   !---------------------------------------
  !   ! Open input MPAS file
  !   !---------------------------------------
  !   stat = nf90_open(trim(mpasFile), nf90_nowrite, fin)
  !   call check_nc(stat, 'nf90_open('//trim(mpasFile)//')')

  !   stat = nf90_inq_dimid(fin, 'nCells', dimid)
  !   call check_nc(stat, 'nf90_inq_dimid(nCells)')
  !   stat = nf90_inquire_dimension(fin, dimid, len=nCells)
  !   call check_nc(stat, 'nf90_inquire_dimension(nCells)')

  !   stat = nf90_inq_dimid(fin, 'nVertices', dimid)
  !   call check_nc(stat, 'nf90_inq_dimid(nVertices)')
  !   stat = nf90_inquire_dimension(fin, dimid, len=nVertices)
  !   call check_nc(stat, 'nf90_inquire_dimension(nVertices)')

  !   stat = nf90_inq_dimid(fin, 'maxEdges', dimid)
  !   call check_nc(stat, 'nf90_inq_dimid(maxEdges)')
  !   stat = nf90_inquire_dimension(fin, dimid, len=maxVertices)
  !   call check_nc(stat, 'nf90_inquire_dimension(maxEdges)')

  !   allocate(latCell(nCells), lonCell(nCells))
  !   allocate(latVertex(nVertices), lonVertex(nVertices))
  !   allocate(areaCell(nCells))
  !   allocate(verticesOnCell(maxVertices, nCells))
  !   allocate(nEdgesOnCell(nCells))

  !   stat = nf90_inq_varid(fin, 'latCell', varid)
  !   call check_nc(stat, 'nf90_inq_varid(latCell)')
  !   stat = nf90_get_var(fin, varid, latCell)
  !   call check_nc(stat, 'nf90_get_var(latCell)')

  !   stat = nf90_inq_varid(fin, 'lonCell', varid)
  !   call check_nc(stat, 'nf90_inq_varid(lonCell)')
  !   stat = nf90_get_var(fin, varid, lonCell)
  !   call check_nc(stat, 'nf90_get_var(lonCell)')

  !   stat = nf90_inq_varid(fin, 'latVertex', varid)
  !   call check_nc(stat, 'nf90_inq_varid(latVertex)')
  !   stat = nf90_get_var(fin, varid, latVertex)
  !   call check_nc(stat, 'nf90_get_var(latVertex)')

  !   stat = nf90_inq_varid(fin, 'lonVertex', varid)
  !   call check_nc(stat, 'nf90_inq_varid(lonVertex)')
  !   stat = nf90_get_var(fin, varid, lonVertex)
  !   call check_nc(stat, 'nf90_get_var(lonVertex)')

  !   stat = nf90_inq_varid(fin, 'verticesOnCell', varid)
  !   call check_nc(stat, 'nf90_inq_varid(verticesOnCell)')
  !   stat = nf90_get_var(fin, varid, verticesOnCell)
  !   call check_nc(stat, 'nf90_get_var(verticesOnCell)')

  !   stat = nf90_inq_varid(fin, 'nEdgesOnCell', varid)
  !   call check_nc(stat, 'nf90_inq_varid(nEdgesOnCell)')
  !   stat = nf90_get_var(fin, varid, nEdgesOnCell)
  !   call check_nc(stat, 'nf90_get_var(nEdgesOnCell)')

  !   stat = nf90_inq_varid(fin, 'areaCell', varid)
  !   call check_nc(stat, 'nf90_inq_varid(areaCell)')
  !   stat = nf90_get_var(fin, varid, areaCell)
  !   call check_nc(stat, 'nf90_get_var(areaCell)')

  !   stat = nf90_get_att(fin, nf90_global, 'sphere_radius', sphereRadius)
  !   call check_nc(stat, 'nf90_get_att(sphere_radius)')

  !   stat = nf90_inquire_attribute(fin, nf90_global, 'on_a_sphere', len=attlen)
  !   call check_nc(stat, 'nf90_inquire_attribute(on_a_sphere)')
  !   allocate(character(len=attlen) :: on_a_sphere)
  !   stat = nf90_get_att(fin, nf90_global, 'on_a_sphere', on_a_sphere)
  !   call check_nc(stat, 'nf90_get_att(on_a_sphere)')

  !   !---------------------------------------
  !   ! Validate / warn
  !   !---------------------------------------
  !   if (any(lonCell < 0.0_rk .or. lonCell > 2.0_rk*pi)) then
  !     error stop 'lonCell is not in the desired range (0, 2pi)'
  !   end if

  !   if (any(lonVertex < 0.0_rk .or. lonVertex > 2.0_rk*pi)) then
  !     error stop 'lonVertex is not in the desired range (0, 2pi)'
  !   end if

  !   if (sphereRadius <= 0.0_rk) then
  !     sphereRadius = SHR_CONST_REARTH
  !     write(*,'(A,ES24.16)') ' -- WARNING: sphereRadius<=0 so setting sphereRadius = ', &
  !                             SHR_CONST_REARTH
  !   end if

  !   if (trim(on_a_sphere) == 'NO') then
  !     write(*,'(A)') " -- WARNING: 'on_a_sphere' attribute is 'NO', which means there may be some disagreement regarding area between the planar (source) and spherical (target) mesh"
  !   end if

  !   !---------------------------------------
  !   ! Optional landIceMask
  !   !---------------------------------------
  !   if (doLandIceMask) then
  !     stat = nf90_inq_varid(fin, 'landIceMask', varid)
  !     call check_nc(stat, 'nf90_inq_varid(landIceMask)')

  !     stat = nf90_inquire_variable(fin, varid, ndims=ndims_landIceMask, dimids=dimids_landIceMask)
  !     call check_nc(stat, 'nf90_inquire_variable(landIceMask)')

  !     if (ndims_landIceMask == 1) then
  !       allocate(landIceMask1d(nCells))
  !       stat = nf90_get_var(fin, varid, landIceMask1d)
  !       call check_nc(stat, 'nf90_get_var(landIceMask 1D)')
  !     else if (ndims_landIceMask == 2) then
  !       allocate(landIceMask2d(1, nCells))
  !       stat = nf90_get_var(fin, varid, landIceMask2d, start=(/1,1/), count=(/1,nCells/))
  !       call check_nc(stat, 'nf90_get_var(landIceMask 2D first slice)')
  !     else
  !       error stop 'landIceMask has unsupported rank'
  !     end if
  !   end if

  !   !---------------------------------------
  !   ! Build SCRIP arrays
  !   !---------------------------------------
  !   allocate(grid_corner_lat(nCells, maxVertices))
  !   allocate(grid_corner_lon(nCells, maxVertices))
  !   allocate(grid_area(nCells))
  !   allocate(grid_imask(nCells))

  !   grid_corner_lat = 0.0_rk
  !   grid_corner_lon = 0.0_rk

  !   grid_area = areaCell / (sphereRadius*sphereRadius)
  !   grid_dims(1) = nCells

  !   do iCell = 1, nCells
  !     lastValidVertex = verticesOnCell(nEdgesOnCell(iCell), iCell)

  !     do iVertex = 1, maxVertices
  !       if (iVertex <= nEdgesOnCell(iCell)) then
  !         grid_corner_lat(iCell, iVertex) = latVertex(verticesOnCell(iVertex, iCell))
  !         grid_corner_lon(iCell, iVertex) = lonVertex(verticesOnCell(iVertex, iCell))
  !       else
  !         grid_corner_lat(iCell, iVertex) = latVertex(lastValidVertex)
  !         grid_corner_lon(iCell, iVertex) = lonVertex(lastValidVertex)
  !       end if
  !     end do
  !   end do

  !   if (doLandIceMask) then
  !     if (allocated(landIceMask1d)) then
  !       grid_imask = 1 - landIceMask1d
  !     else
  !       grid_imask = 1 - landIceMask2d(1, :)
  !     end if
  !   else
  !     grid_imask = 1
  !   end if

  !   !---------------------------------------
  !   ! Create output SCRIP file
  !   !---------------------------------------
  !   stat = nf90_create(trim(scripFile), nf90_clobber, fout)
  !   call check_nc(stat, 'nf90_create('//trim(scripFile)//')')

  !   stat = nf90_def_dim(fout, 'grid_size',    nCells,       dim_grid_size)
  !   call check_nc(stat, 'nf90_def_dim(grid_size)')
  !   stat = nf90_def_dim(fout, 'grid_corners', maxVertices,  dim_grid_corners)
  !   call check_nc(stat, 'nf90_def_dim(grid_corners)')
  !   stat = nf90_def_dim(fout, 'grid_rank',    1,            dim_grid_rank)
  !   call check_nc(stat, 'nf90_def_dim(grid_rank)')

  !   stat = nf90_def_var(fout, 'grid_center_lat', nf90_double, (/dim_grid_size/), var_grid_center_lat)
  !   call check_nc(stat, 'nf90_def_var(grid_center_lat)')
  !   stat = nf90_put_att(fout, var_grid_center_lat, 'units', 'radians')
  !   call check_nc(stat, 'nf90_put_att(grid_center_lat:units)')

  !   stat = nf90_def_var(fout, 'grid_center_lon', nf90_double, (/dim_grid_size/), var_grid_center_lon)
  !   call check_nc(stat, 'nf90_def_var(grid_center_lon)')
  !   stat = nf90_put_att(fout, var_grid_center_lon, 'units', 'radians')
  !   call check_nc(stat, 'nf90_put_att(grid_center_lon:units)')

  !   stat = nf90_def_var(fout, 'grid_corner_lat', nf90_double, (/dim_grid_size, dim_grid_corners/), var_grid_corner_lat)
  !   call check_nc(stat, 'nf90_def_var(grid_corner_lat)')
  !   stat = nf90_put_att(fout, var_grid_corner_lat, 'units', 'radians')
  !   call check_nc(stat, 'nf90_put_att(grid_corner_lat:units)')

  !   stat = nf90_def_var(fout, 'grid_corner_lon', nf90_double, (/dim_grid_size, dim_grid_corners/), var_grid_corner_lon)
  !   call check_nc(stat, 'nf90_def_var(grid_corner_lon)')
  !   stat = nf90_put_att(fout, var_grid_corner_lon, 'units', 'radians')
  !   call check_nc(stat, 'nf90_put_att(grid_corner_lon:units)')

  !   stat = nf90_def_var(fout, 'grid_area', nf90_double, (/dim_grid_size/), var_grid_area)
  !   call check_nc(stat, 'nf90_def_var(grid_area)')
  !   stat = nf90_put_att(fout, var_grid_area, 'units', 'radian^2')
  !   call check_nc(stat, 'nf90_put_att(grid_area:units)')

  !   stat = nf90_def_var(fout, 'grid_imask', nf90_int, (/dim_grid_size/), var_grid_imask)
  !   call check_nc(stat, 'nf90_def_var(grid_imask)')
  !   stat = nf90_put_att(fout, var_grid_imask, 'units', 'unitless')
  !   call check_nc(stat, 'nf90_put_att(grid_imask:units)')

  !   stat = nf90_def_var(fout, 'grid_dims', nf90_int, (/dim_grid_rank/), var_grid_dims)
  !   call check_nc(stat, 'nf90_def_var(grid_dims)')

  !   stat = nf90_enddef(fout)
  !   call check_nc(stat, 'nf90_enddef')

  !   stat = nf90_put_var(fout, var_grid_center_lat, latCell)
  !   call check_nc(stat, 'nf90_put_var(grid_center_lat)')

  !   stat = nf90_put_var(fout, var_grid_center_lon, lonCell)
  !   call check_nc(stat, 'nf90_put_var(grid_center_lon)')

  !   stat = nf90_put_var(fout, var_grid_corner_lat, grid_corner_lat)
  !   call check_nc(stat, 'nf90_put_var(grid_corner_lat)')

  !   stat = nf90_put_var(fout, var_grid_corner_lon, grid_corner_lon)
  !   call check_nc(stat, 'nf90_put_var(grid_corner_lon)')

  !   stat = nf90_put_var(fout, var_grid_area, grid_area)
  !   call check_nc(stat, 'nf90_put_var(grid_area)')

  !   stat = nf90_put_var(fout, var_grid_imask, grid_imask)
  !   call check_nc(stat, 'nf90_put_var(grid_imask)')

  !   stat = nf90_put_var(fout, var_grid_dims, grid_dims)
  !   call check_nc(stat, 'nf90_put_var(grid_dims)')

  !   write(*,'(A,1X,ES24.16,1X,ES24.16)') 'Input latCell min/max values (radians):', &
  !        minval(latCell), maxval(latCell)
  !   write(*,'(A,1X,ES24.16,1X,ES24.16)') 'Input lonCell min/max values (radians):', &
  !        minval(lonCell), maxval(lonCell)
  !   write(*,'(A,1X,ES24.16,1X,ES24.16)') 'Calculated grid_center_lat min/max values (radians):', &
  !        minval(latCell), maxval(latCell)
  !   write(*,'(A,1X,ES24.16,1X,ES24.16)') 'Calculated grid_center_lon min/max values (radians):', &
  !        minval(lonCell), maxval(lonCell)
  !   write(*,'(A,1X,ES24.16,1X,ES24.16)') 'Calculated grid_area min/max values (sq radians):', &
  !        minval(grid_area), maxval(grid_area)

  !   stat = nf90_close(fin)
  !   call check_nc(stat, 'nf90_close(input)')

  !   stat = nf90_close(fout)
  !   call check_nc(stat, 'nf90_close(output)')

  !   write(*,'(A)') 'Creation of SCRIP file is complete.'

  ! contains

  !   subroutine check_nc(status, where)
  !     integer, intent(in) :: status
  !     character(len=*), intent(in) :: where

  !     if (status /= nf90_noerr) then
  !       write(*,'(A)') 'NetCDF error in '//trim(where)//': '//trim(nf90_strerror(status))
  !       error stop 1
  !     end if
  !   end subroutine check_nc

  ! end subroutine scrip_from_mpas


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

  pi = acos(-1.0)

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

  if (any(lonCell < 0.0 .or. lonCell > 2.0*pi)) then
    error stop 'lonCell is not in the desired range (0, 2pi)'
  end if

  if (any(lonVertex < 0.0 .or. lonVertex > 2.0*pi)) then
    error stop 'lonVertex is not in the desired range (0, 2pi)'
  end if

  if (sphereRadius <= 0.0) then
    sphereRadius = SHR_CONST_REARTH
    write(*,'(A,ES24.16)') ' -- WARNING: sphereRadius<=0 so setting sphereRadius = ', &
                            SHR_CONST_REARTH
  end if

  if (trim(on_a_sphere) == 'NO') then
    write(*,'(A)') " -- WARNING: 'on_a_sphere' attribute is 'NO', which means there may be some disagreement regarding area between the planar (source) and spherical (target) mesh"
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

  stat = nf90_create(trim(scripFile), ior(nf90_clobber, nf90_netcdf4), &
       fout, &
       comm=MPI_COMM_WORLD,                                      &
       info=MPI_INFO_NULL)
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
