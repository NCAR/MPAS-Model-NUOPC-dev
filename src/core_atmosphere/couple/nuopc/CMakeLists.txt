add_library(mpas_atm_nuopc
  mpas_atm_nuopc.F90)
target_link_libraries(mpas_atm_nuopc ESMF)

install(TARGETS mpas_atm_nuopc
        EXPORT mpas_atm_nuopc
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        INCLUDES DESTINATION mod
)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
        DESTINATION mod
        FILES_MATCHING
        PATTERN "*.mod"
        PATTERN "CMakeFiles" EXCLUDE
)
install(EXPORT mpas_atm_nuopc DESTINATION cmake)
# message(FATAL_ERROR "BUILDING MPAS_ATM_NUOPC")
