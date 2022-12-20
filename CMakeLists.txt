cmake_minimum_required(VERSION 3.24)

project(amspipe VERSION 0.1
                DESCRIPTION "a library that implements the worker side of the AMS pipe protocol"
                LANGUAGES CXX C)

option(BUILD_SHARED_LIBS "Build shared library (.dll/.so) instead of static ones (.lib/.a)" OFF)

function(enable_warnings target)
   target_compile_options(${target} PRIVATE
     $<$<CXX_COMPILER_ID:MSVC>:/W4>
     $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra -Wpedantic>
   )
endfunction()

# amspipe library

add_library(amspipe)

target_include_directories(amspipe
   PUBLIC
      include
)

target_sources(amspipe
   PRIVATE
      src/amspipe.cpp
      src/amspipe_c.cpp
      src/ubjson.cpp
      src/ubjson.hpp
   PUBLIC FILE_SET HEADERS
   BASE_DIRS include
   FILES
      include/amspipe.hpp
      include/amspipe.h
      include/amspipe.F90
)

set_target_properties(amspipe PROPERTIES
    CXX_STANDARD 11
    CXX_STANDARD_REQUIRED YES
    CXX_EXTENSIONS NO
)
enable_warnings(amspipe)

install(TARGETS amspipe FILE_SET HEADERS)

# demo application for C++

add_executable(demo_cpp
   demo/demo.cpp
)

set_target_properties(demo_cpp PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    CXX_EXTENSIONS NO
)
enable_warnings(demo_cpp)

target_link_libraries(demo_cpp PRIVATE amspipe)

# demo application for C

add_executable(demo_c
   demo/demo.c
)

enable_warnings(demo_c)

target_link_libraries(demo_c PRIVATE amspipe)

# demo application for Fortran

ENABLE_LANGUAGE(Fortran)

add_executable(demo_F90
   demo/demo.F90
)

enable_warnings(demo_F90)

target_link_libraries(demo_F90 PRIVATE amspipe)
