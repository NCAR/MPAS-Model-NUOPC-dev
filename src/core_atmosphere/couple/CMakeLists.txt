set(ESMX_DATA_INCLUDE_DIR ${CMAKE_BINARY_DIR}/ESMX_Data/include)
file(MAKE_DIRECTORY ${ESMX_DATA_INCLUDE_DIR})

add_library(mpas_atm_nuopc_obj
  OBJECT
  mpas_nuopc_utils.F90
  mpas_atm_nuopc.F90
  mpas_fields.F90
  )
set_target_properties(mpas_atm_nuopc_obj PROPERTIES
  Fortran_MODULE_DIRECTORY ${ESMX_DATA_INCLUDE_DIR}
)
target_include_directories(mpas_atm_nuopc_obj
  PRIVATE
  ${ESMX_DATA_INCLUDE_DIR}
  ${CMAKE_BINARY_DIR}/mpas_atm_nuopc/ESMX_Data/include/
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


link_directories(${CMAKE_INSTALL_LIBDIR})

target_link_libraries(mpas_atm_nuopc
  PUBLIC
  MPI::MPI_Fortran
  ESMF
  PIO::PIO_Fortran
  PIO::PIO_C
  PnetCDF::PnetCDF_Fortran
  )

install(TARGETS mpas_atm_nuopc
        EXPORT mpas_atm_nuopc-config
        LIBRARY DESTINATION lib
)
install(DIRECTORY ${ESMX_DATA_INCLUDE_DIR}/
        DESTINATION include
        FILES_MATCHING PATTERN "*.mod"
        PATTERN "CMakeFiles" EXCLUDE
)
install(EXPORT mpas_atm_nuopc-config DESTINATION lib/cmake)
