cmake_minimum_required(VERSION 3.10)

project(fortran_approx LANGUAGES Fortran)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_INSTALL_PREFIX "." CACHE PATH "Default install prefix" FORCE)
endif()

include(GNUInstallDirs)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib CACHE PATH "Output directory for archives")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib CACHE PATH "Output directory for libraries")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin CACHE PATH "Output directory for runtime binaries")
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules CACHE PATH "Output directory for Fortran modules")

set(SOURCES
    src/approx.F90
)

add_library(approx ${SOURCES})

# set_target_properties(approx PROPERTIES
#     Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules
# )

target_include_directories(approx
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

install(TARGETS approx
    EXPORT approxTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(DIRECTORY ${CMAKE_BINARY_DIR}/modules/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

enable_testing()

add_executable(approx_example example/main.f90)
target_link_libraries(approx_example PRIVATE approx)

add_test(NAME RunExample COMMAND approx_example)
