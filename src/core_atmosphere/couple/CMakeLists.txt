set( MPAS_ATM_NUOPC_MODDIR
  ${CMAKE_CURRENT_BINARY_DIR}/include)

add_library(mpas_atm_nuopc_obj
  OBJECT
  mpas_nuopc_utils.F90
  mpas_atm_nuopc.F90
  mpas_fields.F90
)

set_target_properties(mpas_atm_nuopc_obj PROPERTIES
  Fortran_MODULE_DIRECTORY ${MPAS_ATM_NUOPC_MODDIR}
)

target_link_libraries(mpas_atm_nuopc_obj
  PRIVATE
  core_atmosphere
  PIO::PIO_Fortran
)


add_library(mpas_atm_nuopc
  STATIC
  $<TARGET_OBJECTS:core_atmosphere>
  $<TARGET_OBJECTS:framework>
  $<TARGET_OBJECTS:operators>
  # $<TARGET_OBJECTS:core_init_atmosphere>
  $<TARGET_OBJECTS:mpas_atm_nuopc_obj>
  $<TARGET_OBJECTS:ezxml>
  )

target_include_directories(mpas_atm_nuopc
  PUBLIC
    $<BUILD_INTERFACE:${MPAS_ATM_NUOPC_MODDIR}>
    $<INSTALL_INTERFACE:include>
)
# link_directories(${CMAKE_INSTALL_LIBDIR})

target_link_libraries(mpas_atm_nuopc
  PUBLIC
  ESMF
  PIO::PIO_Fortran
  PIO::PIO_C
  PnetCDF::PnetCDF_Fortran
  )

install(TARGETS mpas_atm_nuopc
  EXPORT mpas_atm_nuopc-config
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin
)

install(DIRECTORY ${MPAS_ATM_NUOPC_MODDIR}/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
)

install(EXPORT mpas_atm_nuopc-config
  NAMESPACE MPAS::
  DESTINATION lib/cmake/mpas_atm_nuopc
)
