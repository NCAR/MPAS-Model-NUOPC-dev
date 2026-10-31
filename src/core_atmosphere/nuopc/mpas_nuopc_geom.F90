#include "mpas_nuopc_macros.h"

#ifndef DEBUG
#define DEBUG .true.
#endif

module mpas_nuopc_geom
  use esmf
  use nuopc
  use mpas_kind_types, only: RKIND
  use mpas_derived_types, only: mpas_pool_type
  use mpas_derived_types, only: domain_type
  use mpas_derived_types, only: mpas_exchange_list
  use mpas_pool_routines, only: mpas_pool_get_subpool
  use mpas_pool_routines, only: mpas_pool_get_dimension
  use mpas_pool_routines, only: mpas_pool_get_array
  use mpas_pool_routines, only: mpas_pool_get_config
  use mpas_dmpar, only: mpas_dmpar_sum_int
  use mpas_dmpar, only: mpas_dmpar_max_int
  use mpas_dmpar, only: mpas_dmpar_min_real
  use mpas_dmpar, only: mpas_dmpar_max_real

  implicit none
  private

  type :: mesh_class
    type(ESMF_DistGrid) :: esmfdistgrid
    type(ESMF_Mesh) :: esmfmesh
    logical :: on_a_sphere
    integer :: ownElem
    integer :: nNodes
    integer :: nShiftedNodes
    integer :: nReversed
    integer, allocatable :: elementMask(:)
    logical, allocatable :: landMask(:)
    logical, allocatable :: oceanMask(:)
    contains
      procedure :: destroy => mesh_destroy
      procedure :: print_distgrid => distgrid_print
      procedure :: print_mesh => mesh_print
  end type mesh_class

  interface new_mesh
    module procedure mesh_create
  end interface new_mesh

  public :: mesh_class
  public :: new_mesh

contains

  !-----------------------------------------------------------------------
  ! determine if the mesh is on a sphere (on_a_sphere)
  function mesh_onSphere(meshPool, rc) result(isOnSphere)
    ! arguments
    type(mpas_pool_type), pointer, intent(in) :: meshPool
    integer, intent(out) :: rc
    ! return value
    logical :: isOnSphere
    ! local variables
    logical, pointer :: on_a_sphere
    character(*), parameter :: method = "mesh_onSphere"

    rc = ESMF_SUCCESS
    isOnSphere = .true.

    nullify(on_a_sphere)
    call mpas_pool_get_config(meshPool, 'on_a_sphere', on_a_sphere)
    if (.not. associated(on_a_sphere)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Failed to find on_a_sphere config in mesh subpool", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if
    isOnSphere = on_a_sphere
  end function mesh_onSphere

  !-----------------------------------------------------------------------
  ! determine number of owned elements (nCellsSolve)
  function mesh_ownedElements(meshPool, rc) result(nOwned)
    type(mpas_pool_type), pointer, intent(in) :: meshPool
    integer, intent(out) :: rc
    ! return value
    integer :: nOwned
    ! local variables
    integer, pointer :: nCellsSolve
    character(*), parameter :: method = "mesh_ownedElements"

    nOwned = -1
    nullify(nCellsSolve)
    call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
    if (.not. associated(nCellsSolve)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Failed to find nCellsSolve dimension", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if
    nOwned = nCellsSolve
  end function mesh_ownedElements

  !-----------------------------------------------------------------------
  ! create distributed grid for the mesh
  function mesh_distGridCreate(meshPool, nOwned, rc) result(distGrid)
    ! arguments
    type(mpas_pool_type), pointer, intent(in) :: meshPool
    integer, intent(in) :: nOwned
    integer, intent(inout) :: rc
    ! return value
    type(ESMF_DistGrid) :: distGrid
    ! local variables
    integer, dimension(:), pointer :: indexToCellID
    integer, allocatable :: seqIndex(:)
    character(*), parameter :: method = "mesh_distGridCreate"

    rc = ESMF_SUCCESS

    nullify(indexToCellID)
    call mpas_pool_get_array(meshPool, 'indexToCellID', indexToCellID)
    if (.not. associated(indexToCellID)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Failed to find indexToCellID array", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if
    allocate(seqIndex(nOwned))
    seqIndex(1:nOwned) = indexToCellID(1:nOwned)
    distGrid = ESMF_DistGridCreate(arbSeqIndexList=seqIndex, rc=rc)
    ESMF_ERR_RETURN(rc)
  end function mesh_distGridCreate

  !-----------------------------------------------------------------------
  ! create an element mask for the surface
  function sfc_elementMask(sfcPool, nOwned, rc) result(elementMask)
    type(mpas_pool_type), pointer, intent(in) :: sfcPool
    integer, intent(in) :: nOwned
    integer, intent(out) :: rc
    ! return value
    integer, allocatable :: elementMask(:)
    ! local variables
    integer, pointer :: landmask(:)
    integer, parameter :: LAND = 1
    integer, parameter :: OCEAN = 0
    character(len=*), parameter :: method = "sfc_elementMask"

    rc = ESMF_SUCCESS

    nullify(landmask)
    call mpas_pool_get_array(sfcPool, 'landmask', landmask)
    if (.not. associated(landmask)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Failed to find landmask array in sfc_input subpool", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    allocate(elementMask(nOwned))
    elementMask = landmask(1:nOwned)
  end function sfc_elementMask

  !-----------------------------------------------------------------------
  ! create a land mask from the element mask
  function sfc_landMask(elementMask, rc) result(landMask)
    integer, intent(in) :: elementMask(:)
    integer, intent(out) :: rc
    ! return value
    logical, allocatable :: landMask(:)
    ! local variables
    integer, parameter :: LAND = 1
    character(len=*), parameter :: method = "sfc_landMask"

    rc = ESMF_SUCCESS

    allocate(landMask(size(elementMask)))
    landMask = (elementMask == LAND)
  end function sfc_landMask

  !-----------------------------------------------------------------------
  ! create an ocean mask from the element mask
  function sfc_oceanMask(elementMask, rc) result(oceanMask)
    integer, intent(in) :: elementMask(:)
    integer, intent(out) :: rc
    ! return value
    logical, allocatable :: oceanMask(:)
    ! local variables
    integer, parameter :: OCEAN = 0
    character(len=*), parameter :: method = "sfc_oceanMask"

    rc = ESMF_SUCCESS

    allocate(oceanMask(size(elementMask)))
    oceanMask = (elementMask == OCEAN)
  end function sfc_oceanMask

  !-----------------------------------------------------------------------
  ! Owning MPI rank of every local cell, owned and halo
  subroutine mpas_cell_owners(domain, nCells, nCellsSolve, cellOwner)
    !> Owned cells belong to this rank. A halo cell is received from its owner,
    !> so the cell receive lists already name it: no communication needed.
    ! arguments
    type(domain_type), intent(in) :: domain
    integer, intent(in) :: nCells, nCellsSolve
    integer, intent(out) :: cellOwner(:)
    ! local variables
    type(mpas_exchange_list), pointer :: exchList
    integer :: iHalo, i

    cellOwner = -1
    cellOwner(1:nCellsSolve) = domain%dminfo%my_proc_id

    do iHalo = 1, size(domain%blocklist%parinfo%cellsToRecv%halos)
      exchList => domain%blocklist%parinfo%cellsToRecv%halos(iHalo)%exchList
      do while (associated(exchList))
        do i = 1, exchList%nlist
          if (exchList%destList(i) >= 1 .and. exchList%destList(i) <= nCells) then
            cellOwner(exchList%destList(i)) = exchList%endPointID
          end if
        end do
        exchList => exchList%next
      end do
    end do
  end subroutine mpas_cell_owners

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

  pure function sphere_winding(plon, plat, clon, clat) result(w)
    !> sum of (p_k x p_k+1) . c over the corners of a spherical polygon around
    !> centre c (all lon/lat in radians); positive when the corners run
    !> counter-clockwise seen from outside the sphere
    real(ESMF_KIND_R8), intent(in) :: plon(:), plat(:), clon, clat
    real(ESMF_KIND_R8) :: w
    real(ESMF_KIND_R8) :: c(3), p(3), q(3)
    integer :: kk, nk
    nk = size(plon)
    c = unit_vector(clon, clat)
    w = 0.0_ESMF_KIND_R8
    do kk = 1, nk
      p = unit_vector(plon(kk), plat(kk))
      q = unit_vector(plon(mod(kk, nk) + 1), plat(mod(kk, nk) + 1))
      w = w + c(1)*(p(2)*q(3) - p(3)*q(2)) &
            + c(2)*(p(3)*q(1) - p(1)*q(3)) &
            + c(3)*(p(1)*q(2) - p(2)*q(1))
    end do
  end function sphere_winding

  pure function unit_vector(lon, lat) result(u)
    real(ESMF_KIND_R8), intent(in) :: lon, lat
    real(ESMF_KIND_R8) :: u(3)
    u = (/ cos(lat)*cos(lon), cos(lat)*sin(lon), sin(lat) /)
  end function unit_vector

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

  pure integer function shift_code(sx, sy) result(code)
    !> 0 for the unshifted node, 1..8 for the eight seam shifts
    integer, intent(in) :: sx, sy
    code = (sx + 1) + 3*(sy + 1)
    if (code == 4) then
      code = 0
    else if (code < 4) then
      code = code + 1
    end if
  end function shift_code

  !-----------------------------------------------------------------------
  ! print distributed grid information
  subroutine distgrid_print(self, writePet, rc)
    ! arguments
    class(mesh_class), intent(in) :: self
    integer, intent(in) :: writePet
    integer, intent(out) :: rc
    ! local variables
    logical :: doWrite
    integer :: de_i
    type(ESMF_VM) :: vm
    integer :: localPet
    integer :: deCount
    integer(ESMF_KIND_I8), allocatable :: elementCountPDeI8(:)
    character(*), parameter :: fmt_hdr = "(A10,1(1X,A10))"
    character(*), parameter :: fmt_inf = "(I10,1(1X,I10))"
    character(len=*), parameter :: method = "distgrid_print"

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    ESMF_ERR_RETURN(rc)
    doWrite = (localPet == writePet)

    if (doWrite) then
      call ESMF_DistGridGet(distgrid=self%esmfdistgrid, deCount=deCount, rc=rc)
      ESMF_ERR_RETURN(rc)
      allocate(elementCountPDeI8(deCount))
      call ESMF_DistGridGet(distgrid=self%esmfdistgrid, &
        elementCountPDeI8=elementCountPDeI8, rc=rc)
      ESMF_ERR_RETURN(rc)

      write(*, "(A)") "### DistGrid Information"
      write(*, fmt_hdr) "DE", "ElemCount"
      do de_i = 1, deCount
        write(*, fmt_inf) &
          de_i, &
          elementCountPDeI8(de_i)
      end do
      deallocate(elementCountPDeI8)
    end if

  end subroutine distgrid_print

  !-----------------------------------------------------------------------
  ! print mesh information
  subroutine mesh_print(self, writePet, rc)
    class(mesh_class), intent(in) :: self
    integer, intent(in) :: writePet
    integer, intent(out) :: rc
    ! local variables
    integer :: pet_i
    logical :: doWrite
    type(ESMF_VM) :: vm
    integer :: localPet
    integer :: petCount
    integer :: localOwnNde(1)
    integer :: localOwnElm(1)
    integer :: localLndCnt(1)
    integer :: localOcnCnt(1)
    integer, allocatable :: allOwnNde(:)
    integer, allocatable :: allOwnElm(:)
    integer, allocatable :: allLndCnt(:)
    integer, allocatable :: allOcnCnt(:)
    character(*), parameter :: fmt_hdr = "(A10,1(1X,A10),4(1X,A10))"
    character(*), parameter :: fmt_inf = "(I10,1(1X,L10),4(1X,I10))"
    character(*), parameter :: fmt_ttl = "(A10,1(1X,L10),4(1X,I10))"
    character(len=*), parameter :: method = "mesh_print"

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    ESMF_ERR_RETURN(rc)
    doWrite = (localPet == writePet)

    ! get local mesh information
    call ESMF_MeshGet(self%esmfmesh, &
      numOwnedNodes=localOwnNde(1), &
      rc=rc &
    )
    localOwnElm(1) = self%ownElem
    if (allocated(self%landMask)) then
      localLndCnt = count(self%landMask)
    else
      localLndCnt = 0
    end if
    if (allocated(self%oceanMask)) then
      localOcnCnt = count(self%oceanMask)
    else
      localOcnCnt = 0
    end if

    if (doWrite) then
      allocate(allOwnNde(0:petCount-1))
      allocate(allOwnElm(0:petCount-1))
      allocate(allLndCnt(0:petCount-1))
      allocate(allOcnCnt(0:petCount-1))
    end if

    ! gather mesh information from all PETs to the write PET
    call ESMF_VMGather(vm, sendData=localOwnNde, recvData=allOwnNde, &
      count=1, rootPet=writePet, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMGather(vm, sendData=localOwnElm, recvData=allOwnElm, &
      count=1, rootPet=writePet, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMGather(vm, sendData=localLndCnt, recvData=allLndCnt, &
      count=1, rootPet=writePet, rc=rc)
    ESMF_ERR_RETURN(rc)
    call ESMF_VMGather(vm, sendData=localOcnCnt, recvData=allOcnCnt, &
      count=1, rootPet=writePet, rc=rc)
    ESMF_ERR_RETURN(rc)

    if (doWrite) then
      write(*, "(A)") "### Mesh Information"
      write(*, fmt_hdr) "PET", "onSphere", "ownedNodes", "ownedElems", "landCells", "oceanCells"
      do pet_i = 0, petCount-1
        write(*, fmt_inf) &
          pet_i, &
          self%on_a_sphere, &
          allOwnNde(pet_i), &
          allOwnElm(pet_i), &
          allLndCnt(pet_i), &
          allOcnCnt(pet_i)
      end do
      write(*, fmt_ttl) &
        "TOTAL", &
        self%on_a_sphere, &
        sum(allOwnNde), &
        sum(allOwnElm), &
        sum(allLndCnt), &
        sum(allOcnCnt)
      deallocate(allOwnNde)
      deallocate(allOwnElm)
      deallocate(allLndCnt)
      deallocate(allOcnCnt)
    end if
  end subroutine mesh_print

  !-----------------------------------------------------------------------
  ! destroy mesh and associated resources
  subroutine mesh_destroy(self, rc)
    class(mesh_class), intent(inout) :: self
    integer, intent(out) :: rc
    ! local variables
    logical :: distGridCheck
    logical :: meshCheck

    rc = ESMF_SUCCESS

    distGridCheck = ESMF_DistGridIsCreated(self%esmfdistgrid, rc=rc)
    ESMF_ERR_RETURN(rc)
    if (distGridCheck) then
      call ESMF_DistGridDestroy(self%esmfdistgrid, rc=rc)
      ESMF_ERR_RETURN(rc)
    end if

    meshCheck = ESMF_MeshIsCreated(self%esmfmesh, rc=rc)
    if (meshCheck) then
      call ESMF_MeshDestroy(self%esmfmesh, rc=rc)
      ESMF_ERR_RETURN(rc)
    end if

    self%ownElem = -1
    self%nNodes = -1
    self%nShiftedNodes = -1
    self%nReversed = -1

    if (allocated(self%elementMask)) deallocate(self%elementMask)
    if (allocated(self%landMask)) deallocate(self%landMask)
    if (allocated(self%oceanMask)) deallocate(self%oceanMask)

  end subroutine mesh_destroy

  !-----------------------------------------------------------------------
  ! Mesh Constructors
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! Create an ESMF mesh from the MPAS mesh pool
  type(mesh_class) function mesh_create(domain, rc) result(mesh)
    !> Build the ESMF mesh matching the MPAS decomposition, in memory.
    !>
    !> Each PET contributes its owned cells (1:nCellsSolve) as elements, in the
    !> same order as the distgrid and the field pointers, and the MPAS vertices
    !> around them as nodes. Everything comes from the 'mesh' pool, so no grid
    !> file is read or written.
    !>
    !> Errors here abort the whole job rather than returning. The dmpar
    !> reductions and ESMF_MeshCreate are collectives, and a failure is often
    !> seen by one PET only, so a local return would strand the remaining PETs
    !> and turn a clean error into a walltime kill.
    ! arguments
    type(domain_type), intent(in) :: domain
    integer, intent(out) :: rc
    ! local variables
    ! metres -> kilometres, the Cartesian unit used for planar meshes
    real(ESMF_KIND_R8), parameter :: M2KM = 1.0e-3_ESMF_KIND_R8
    ! a shifted corner must land at most this multiple of the largest ordinary
    ! centre-to-corner offset away, otherwise the period is wrong
    real(ESMF_KIND_R8), parameter :: UNWRAP_TOL = 1.5_ESMF_KIND_R8
    ! used when the mesh carries no sphere_radius
    real(ESMF_KIND_R8), parameter :: REARTH = 6.37122e6_ESMF_KIND_R8
    character(*), parameter :: method = "mesh_create"

    type(ESMF_VM) :: vm
    integer :: localPet
    type(mpas_pool_type), pointer :: meshPool, sfcPool
    integer, pointer :: nCells, nVertices, maxEdges, vertexDegree
    integer, dimension(:), pointer :: indexToCellID, indexToVertexID
    integer, dimension(:), pointer :: nEdgesOnCell, landmask
    integer, dimension(:,:), pointer :: verticesOnCell, cellsOnVertex
    real(RKIND), dimension(:), pointer :: areaCell
    real(RKIND), dimension(:), pointer :: cellA_r, cellB_r, vertA_r, vertB_r
    real(RKIND), pointer :: sphere_radius, x_period, y_period

    ! cell/vertex coordinates in R8: (x,y) in metres on a plane, (lon,lat) in
    ! radians on a sphere
    real(ESMF_KIND_R8), allocatable :: cellA(:), cellB(:), vertA(:), vertB(:)
    integer, allocatable :: cellOwner(:)

    real(ESMF_KIND_R8) :: xPeriod, yPeriod, rMaxX, rMaxY, resid, radius, rad2deg
    logical :: xPeriodic, yPeriodic

    ! nodeMap(sx+2, sy+2, v) -> local node for vertex v shifted by (sx,sy) periods
    integer, allocatable :: nodeMap(:,:,:)
    integer, allocatable :: nodeVert(:), nodeSx(:), nodeSy(:)
    integer, allocatable :: nodeIdx(:)
    real(ESMF_KIND_R8), allocatable :: cx(:), cy(:)

    integer, allocatable :: nodeIds(:), nodeOwners(:)
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
    integer, allocatable :: elementTypes(:), elementConn(:)
    real(ESMF_KIND_R8), allocatable :: elementCoords(:), elementArea(:)
    ! left unallocated, and so absent in ESMF_MeshCreate, without a landmask
    type(ESMF_CoordSys_Flag) :: coordSys

    integer :: iCell, iNode, k, v, sx, sy, nc, pos
    integer :: nNodes, nShiftedNodes, nReversed, nLandCells
    integer :: nVertGlobal, localMaxVertID

    rc = ESMF_SUCCESS

    if (associated(domain%blocklist%next)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="More than one MPAS block per rank is not supported", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    nullify(meshPool)
    call mpas_pool_get_subpool(domain%blocklist%structs, 'mesh', meshPool)
    if (.not. associated(meshPool)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Failed to find mesh subpool", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    ! get the subpool for surface input
    nullify(sfcPool)
    call mpas_pool_get_subpool(domain%blocklist%structs, 'sfc_input', sfcPool)
    if (.not. associated(sfcPool)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="Failed to find sfc_input subpool", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if

    mesh%on_a_sphere = mesh_onSphere(meshPool, rc=rc)
    ESMF_ERR_ABORT(rc)

    mesh%ownElem = mesh_ownedElements(meshPool, rc=rc)
    ESMF_ERR_ABORT(rc)

    mesh%esmfdistgrid = mesh_distGridCreate(meshPool, mesh%ownElem, rc=rc)
    ESMF_ERR_ABORT(rc)

    mesh%elementMask = sfc_elementMask(sfcPool, mesh%ownElem, rc=rc)
    ESMF_ERR_ABORT(rc)

    mesh%landMask = sfc_landMask(mesh%elementMask, rc=rc)
    ESMF_ERR_ABORT(rc)

    mesh%oceanMask = sfc_oceanMask(mesh%elementMask, rc=rc)
    ESMF_ERR_ABORT(rc)

    call mpas_pool_get_dimension(meshPool, 'nCells', nCells)
    call mpas_pool_get_dimension(meshPool, 'nVertices', nVertices)
    call mpas_pool_get_dimension(meshPool, 'maxEdges', maxEdges)
    call mpas_pool_get_dimension(meshPool, 'vertexDegree', vertexDegree)
    call mpas_pool_get_array(meshPool, 'indexToCellID', indexToCellID)
    call mpas_pool_get_array(meshPool, 'indexToVertexID', indexToVertexID)
    call mpas_pool_get_array(meshPool, 'nEdgesOnCell', nEdgesOnCell)
    call mpas_pool_get_array(meshPool, 'verticesOnCell', verticesOnCell)
    call mpas_pool_get_array(meshPool, 'cellsOnVertex', cellsOnVertex)
    call mpas_pool_get_array(meshPool, 'areaCell', areaCell)
    call mpas_pool_get_config(meshPool, 'sphere_radius', sphere_radius)
    call mpas_pool_get_config(meshPool, 'x_period', x_period)
    call mpas_pool_get_config(meshPool, 'y_period', y_period)

    if (mesh%on_a_sphere) then
      call mpas_pool_get_array(meshPool, 'lonCell', cellA_r)
      call mpas_pool_get_array(meshPool, 'latCell', cellB_r)
      call mpas_pool_get_array(meshPool, 'lonVertex', vertA_r)
      call mpas_pool_get_array(meshPool, 'latVertex', vertB_r)
      coordSys = ESMF_COORDSYS_SPH_DEG
    else
      ! A planar mesh keeps its coordinates in xCell/yCell and xVertex/yVertex
      ! and goes to ESMF in Cartesian (kilometre) coordinates.
      call mpas_pool_get_array(meshPool, 'xCell', cellA_r)
      call mpas_pool_get_array(meshPool, 'yCell', cellB_r)
      call mpas_pool_get_array(meshPool, 'xVertex', vertA_r)
      call mpas_pool_get_array(meshPool, 'yVertex', vertB_r)
      coordSys = ESMF_COORDSYS_CART
    end if

    allocate(cellA(nCells), cellB(nCells), vertA(nVertices), vertB(nVertices))
    cellA = real(cellA_r(1:nCells), ESMF_KIND_R8)
    cellB = real(cellB_r(1:nCells), ESMF_KIND_R8)
    vertA = real(vertA_r(1:nVertices), ESMF_KIND_R8)
    vertB = real(vertB_r(1:nVertices), ESMF_KIND_R8)

    if (any(nEdgesOnCell(1:mesh%ownElem) < 3) .or. &
        any(nEdgesOnCell(1:mesh%ownElem) > maxEdges)) then
      call ESMF_LogSetError( &
        ESMF_RC_OBJ_INIT, &
        msg="nEdgesOnCell outside [3, maxEdges]", &
        line=__LINE__, &
        file=__FILE__, &
        method=method, &
        rcToReturn=rc &
      )
      return
    end if
    do iCell = 1, mesh%ownElem
      nc = nEdgesOnCell(iCell)
      if (any(verticesOnCell(1:nc, iCell) < 1) .or. &
          any(verticesOnCell(1:nc, iCell) > nVertices)) then
        call ESMF_LogSetError( &
          ESMF_RC_OBJ_INIT, &
          msg="verticesOnCell of an owned cell is not a local vertex", &
          line=__LINE__, &
          file=__FILE__, &
          method=method, &
          rcToReturn=rc &
        )
        return
      end if
    end do

    allocate(cellOwner(nCells))
    call mpas_cell_owners(domain, nCells, mesh%ownElem, cellOwner)

    ! Periodicity only applies to planar meshes. Every PET takes part in the
    ! reductions inside infer_period, so all of them see the same periods.
    if (mesh%on_a_sphere) then
      xPeriodic = .false.
      yPeriodic = .false.
      xPeriod = 0.0_ESMF_KIND_R8
      yPeriod = 0.0_ESMF_KIND_R8
      rMaxX = huge(1.0_ESMF_KIND_R8)
      rMaxY = huge(1.0_ESMF_KIND_R8)
    else
      call infer_period(cellA, vertA, real(x_period, ESMF_KIND_R8), &
        xPeriod, rMaxX, xPeriodic, rc=rc)
      ESMF_ERR_RETURN(rc)
      call infer_period(cellB, vertB, real(y_period, ESMF_KIND_R8), &
        yPeriod, rMaxY, yPeriodic, rc=rc)
      ESMF_ERR_RETURN(rc)
      if (xPeriodic .and. localPet == 0) write(*,'(A,A,A,ES14.6,A)') &
        "MPAS: ", method, " mesh is periodic in x, x_period = ", xPeriod, " m"
      if (yPeriodic .and. localPet == 0) write(*,'(A,A,A,ES14.6,A)') &
        "MPAS: ", method, " mesh is periodic in y, y_period = ", yPeriod, " m"
    end if

    ! Assign local nodes: one per (vertex, period shift) pair used by an owned
    ! cell. On a periodic plane a corner across the seam is shifted next to its
    ! own cell centre and becomes a node of its own, i.e. the torus is cut
    ! open along the seam; cells still share nodes everywhere else.
    allocate(nodeMap(3, 3, nVertices))
    allocate(nodeVert(mesh%ownElem*maxEdges))
    allocate(nodeSx(mesh%ownElem*maxEdges), nodeSy(mesh%ownElem*maxEdges))
    allocate(elementTypes(mesh%ownElem))
    allocate(elementConn(sum(nEdgesOnCell(1:mesh%ownElem))))
    allocate(cx(maxEdges), cy(maxEdges), nodeIdx(maxEdges))
    nodeMap = 0
    nNodes = 0
    nReversed = 0
    pos = 0

    do iCell = 1, mesh%ownElem
      nc = nEdgesOnCell(iCell)
      elementTypes(iCell) = nc

      do k = 1, nc
        v  = verticesOnCell(k, iCell)
        sx = period_shift(vertA(v), cellA(iCell), xPeriod, xPeriodic)
        sy = period_shift(vertB(v), cellB(iCell), yPeriod, yPeriodic)

        cx(k) = vertA(v) + real(sx, ESMF_KIND_R8)*xPeriod
        cy(k) = vertB(v) + real(sy, ESMF_KIND_R8)*yPeriod

        ! the shift must actually bring the corner home, or the period is wrong
        if (.not. mesh%on_a_sphere) then
          resid = abs(cx(k) - cellA(iCell))
          if (resid > UNWRAP_TOL*rMaxX) then
            write(*,'(A,I0,A,ES14.6)') "MPAS: cell ", indexToCellID(iCell), &
              " still has an x corner offset of ", resid
            call ESMF_LogSetError( &
              ESMF_RC_OBJ_INIT, &
              msg="x_period does not unwrap the mesh", &
              line=__LINE__, &
              file=__FILE__, &
              method=method, &
              rcToReturn=rc &
            )
            return
          end if
          resid = abs(cy(k) - cellB(iCell))
          if (resid > UNWRAP_TOL*rMaxY) then
            write(*,'(A,I0,A,ES14.6)') "MPAS: cell ", indexToCellID(iCell), &
              " still has a y corner offset of ", resid
            call ESMF_LogSetError( &
              ESMF_RC_OBJ_INIT, &
              msg="y_period does not unwrap the mesh", &
              line=__LINE__, &
              file=__FILE__, &
              method=method, &
              rcToReturn=rc &
            )
            return
          end if
        end if

        if (nodeMap(sx+2, sy+2, v) == 0) then
          nNodes = nNodes + 1
          nodeMap(sx+2, sy+2, v) = nNodes
          nodeVert(nNodes) = v
          nodeSx(nNodes) = sx
          nodeSy(nNodes) = sy
        end if
        nodeIdx(k) = nodeMap(sx+2, sy+2, v)
      end do

      ! ESMF wants element corners counter-clockwise (seen from outside the
      ! sphere). Flip whenever the winding comes out negative rather than
      ! assuming MPAS's convention.
      if (mesh%on_a_sphere) then
        resid = sphere_winding(cx(1:nc), cy(1:nc), cellA(iCell), cellB(iCell))
      else
        resid = signed_area(cx(1:nc), cy(1:nc))
      end if
      if (resid < 0.0_ESMF_KIND_R8) then
        do k = 1, nc
          elementConn(pos + k) = nodeIdx(nc + 1 - k)
        end do
        nReversed = nReversed + 1
      else
        elementConn(pos+1:pos+nc) = nodeIdx(1:nc)
      end if
      pos = pos + nc
    end do

    ! Node IDs: the MPAS global vertex ID for an unshifted node, offset by a
    ! multiple of the global vertex count for each of the 8 seam shifts.
    localMaxVertID = maxval(indexToVertexID(1:nVertices))
    call mpas_dmpar_max_int(domain%dminfo, localMaxVertID, nVertGlobal)

    rad2deg = 180.0_ESMF_KIND_R8 / acos(-1.0_ESMF_KIND_R8)
    allocate(nodeIds(nNodes), nodeOwners(nNodes), nodeCoords(2*nNodes))
    nShiftedNodes = 0
    do iNode = 1, nNodes
      v  = nodeVert(iNode)
      sx = nodeSx(iNode)
      sy = nodeSy(iNode)
      nodeIds(iNode) = indexToVertexID(v) + nVertGlobal*shift_code(sx, sy)
      nodeOwners(iNode) = node_owner(v, sx, sy, rc=rc)
      ESMF_ERR_ABORT(rc)
      if (sx /= 0 .or. sy /= 0) nShiftedNodes = nShiftedNodes + 1
      if (mesh%on_a_sphere) then
        nodeCoords(2*iNode-1) = modulo(vertA(v)*rad2deg, 360.0_ESMF_KIND_R8)
        nodeCoords(2*iNode)   = vertB(v)*rad2deg
      else
        nodeCoords(2*iNode-1) = (vertA(v) + real(sx, ESMF_KIND_R8)*xPeriod) * M2KM
        nodeCoords(2*iNode)   = (vertB(v) + real(sy, ESMF_KIND_R8)*yPeriod) * M2KM
      end if
    end do

    ! Element centres and areas. ESMF does no unit conversion on areas, so
    ! give them in its own units: square radians on a sphere, square
    ! kilometres (the coordinate unit) on a plane.
    allocate(elementCoords(2*mesh%ownElem), elementArea(mesh%ownElem))
    if (mesh%on_a_sphere) then
      radius = real(sphere_radius, ESMF_KIND_R8)
      if (radius <= 0.0_ESMF_KIND_R8) radius = REARTH
      do iCell = 1, mesh%ownElem
        elementCoords(2*iCell-1) = modulo(cellA(iCell)*rad2deg, 360.0_ESMF_KIND_R8)
        elementCoords(2*iCell)   = cellB(iCell)*rad2deg
      end do
      elementArea = real(areaCell(1:mesh%ownElem), ESMF_KIND_R8) / (radius*radius)
    else
      do iCell = 1, mesh%ownElem
        elementCoords(2*iCell-1) = cellA(iCell) * M2KM
        elementCoords(2*iCell)   = cellB(iCell) * M2KM
      end do
      elementArea = real(areaCell(1:mesh%ownElem), ESMF_KIND_R8) * M2KM * M2KM
    end if

    mesh%esmfmesh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, &
      nodeIds=nodeIds, nodeCoords=nodeCoords, nodeOwners=nodeOwners, &
      elementIds=indexToCellID(1:mesh%ownElem), elementTypes=elementTypes, &
      elementConn=elementConn, elementMask=mesh%elementMask, &
      elementArea=elementArea, elementCoords=elementCoords, &
      elementDistgrid=mesh%esmfdistgrid, coordSys=coordSys, rc=rc)
    ESMF_ERR_ABORT(rc)

    mesh%nNodes = nNodes
    mesh%nShiftedNodes = nShiftedNodes
    mesh%nReversed = nReversed

    contains

    subroutine infer_period(cellCoord, vertCoord, cfgPeriod, period, rMax, isPeriodic, rc)
      !> Period of a planar mesh along one axis, and the largest ordinary
      !> centre-to-corner offset (used as the unwrap tolerance).
      !>
      !> MPAS records x_period/y_period, but older mesh files (and anything
      !> derived from them) leave them at zero even when the mesh really is
      !> periodic, so a zero period is recovered from the geometry. An
      !> ordinary corner sits within one cell radius of its centre; a corner
      !> across the seam sits nearly a full period away. Splitting on half the
      !> global coordinate span separates the two cleanly, and
      !>     period = (smallest wrapped offset) + (largest ordinary offset)
      !> needs only global min/max reductions.
      real(ESMF_KIND_R8), intent(in)  :: cellCoord(:), vertCoord(:), cfgPeriod
      real(ESMF_KIND_R8), intent(out) :: period, rMax
      logical, intent(out) :: isPeriodic
      integer, intent(out) :: rc
      ! local variables
      real(RKIND) :: lo, hi, red
      real(ESMF_KIND_R8) :: half, d, wrapMin, noWrap
      integer :: ic, kk, vv
      character(len=*), parameter :: method = "infer_period"

      rc = ESMF_SUCCESS

      noWrap = real(huge(1.0_RKIND), ESMF_KIND_R8)

      call mpas_dmpar_min_real(domain%dminfo, real(minval(vertCoord), RKIND), lo)
      call mpas_dmpar_max_real(domain%dminfo, real(maxval(vertCoord), RKIND), hi)
      half = 0.5_ESMF_KIND_R8 * (real(hi, ESMF_KIND_R8) - real(lo, ESMF_KIND_R8))

      rMax    = 0.0_ESMF_KIND_R8
      wrapMin = noWrap
      do ic = 1, mesh%ownElem
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

      call mpas_dmpar_max_real(domain%dminfo, real(rMax, RKIND), red)
      rMax = real(red, ESMF_KIND_R8)
      call mpas_dmpar_min_real(domain%dminfo, real(wrapMin, RKIND), red)
      wrapMin = real(red, ESMF_KIND_R8)

      ! guard against a mesh so coarse that no corner counts as "ordinary"
      if (rMax <= 0.0_ESMF_KIND_R8) then
        call ESMF_LogSetError( &
          ESMF_RC_OBJ_INIT, &
          msg="Could not measure cell size while inferring period", &
          line=__LINE__, &
          file=__FILE__, &
          method=method, &
          rcToReturn=rc &
        )
        return
      end if

      if (cfgPeriod > 0.0_ESMF_KIND_R8) then
        isPeriodic = .true.
        period = cfgPeriod
      else
        isPeriodic = (wrapMin < noWrap)
        if (isPeriodic) then
          period = wrapMin + rMax
        else
          period = 0.0_ESMF_KIND_R8
        end if
      end if
    end subroutine infer_period

    integer function node_owner(vv, ssx, ssy, rc) result(owner)
      !> Owner of node (vertex vv, shift ssx/ssy): the owner of the cell with
      !> the lowest global ID among the cells around vv that use this same
      !> shift. Those cells are all owned or in halo layer 1, so every PET that
      !> holds the node picks the same owner, and the owner always has the node
      !> in one of its own elements, as ESMF requires.
      integer, intent(in) :: vv, ssx, ssy
      integer, intent(out) :: rc
      integer :: j, c, best

      rc = ESMF_SUCCESS

      owner = -1
      best = huge(0)
      do j = 1, vertexDegree
        c = cellsOnVertex(j, vv)
        if (c < 1 .or. c > nCells) cycle
        if (period_shift(vertA(vv), cellA(c), xPeriod, xPeriodic) /= ssx) cycle
        if (period_shift(vertB(vv), cellB(c), yPeriod, yPeriodic) /= ssy) cycle
        if (indexToCellID(c) < best) then
          best = indexToCellID(c)
          owner = cellOwner(c)
        end if
      end do
      if (owner < 0) then
        call ESMF_LogSetError( &
          ESMF_RC_OBJ_INIT, &
          msg="No owner found for vertex", &
          line=__LINE__, &
          file=__FILE__, &
          method=method, &
          rcToReturn=rc &
        )
      end if
    end function node_owner

  end function mesh_create

end module mpas_nuopc_geom
