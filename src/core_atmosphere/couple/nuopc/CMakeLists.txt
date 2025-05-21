add_library(mpas_atm_nuopc_obj
  OBJECT
  mpas_atm_nuopc.F90
  )
target_link_libraries(mpas_atm_nuopc_obj
  PRIVATE
  core_atmosphere
)

add_library(mpas_atm_nuopc
  STATIC
  $<TARGET_OBJECTS:core_atmosphere>
  # $<TARGET_OBJECTS:core_init_atmosphere>
  $<TARGET_OBJECTS:mpas_atm_nuopc_obj>
  )
target_link_libraries(mpas_atm_nuopc
  PUBLIC
  ESMF)

install(TARGETS mpas_atm_nuopc
        EXPORT mpas_atm_nuopc-config
        LIBRARY DESTINATION lib
)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
        DESTINATION include
        FILES_MATCHING PATTERN "*.mod"
        PATTERN "CMakeFiles" EXCLUDE
)
install(EXPORT mpas_atm_nuopc-config DESTINATION lib/cmake)

# message(FATAL_ERROR "BUILDING MPAS_ATM_NUOPC")
