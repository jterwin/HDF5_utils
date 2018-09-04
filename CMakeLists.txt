# Specify the minimum version for CMake

cmake_minimum_required(VERSION 3.0.2)

enable_language(Fortran)

# Project's name
project(hdf5_utils)

set( CMAKE_VERBOSE_MAKEFILE on )

# Set the output folder where your program will be created
set(CMAKE_BINARY_DIR ${CMAKE_SOURCE_DIR}/bin)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR})

# search for HDF5
find_package(HDF5 COMPONENTS Fortran REQUIRED)
include_directories(${HDF5_INCLUDE_DIRS})
MESSAGE (STATUS "HDF5 PATH:" ${HDF5_LIBRARIES})

# create binary
set(SOURCES ${PROJECT_SOURCE_DIR}/main.f90 ${PROJECT_SOURCE_DIR}/constants.f90 ${PROJECT_SOURCE_DIR}/HDF5_utils.f90)
add_executable(hdf5_test ${SOURCES})

# linking
set( CMAKE_VERBOSE_MAKEFILE on )
target_link_libraries(hdf5_test ${HDF5_LIBRARIES})

add_library(hdf5_utils STATIC ${PROJECT_SOURCE_DIR}/HDF5_utils.f90)
install(TARGETS hdf5_utils DESTINATION bin)
