cmake_minimum_required(VERSION 3.24)

project(amspipe VERSION 0.1
                DESCRIPTION "a library that implements the worker side of the AMS pipe protocol"
                LANGUAGES CXX)

option(BUILD_SHARED_LIBS "Build shared library (.dll/.so) instead of static ones (.lib/.a)" ON)

# amspipe library

add_library(amspipe
   src/amspipe.cpp
   src/ubjson.cpp
   src/ubjson.hpp
   include/amspipe.hpp
)

target_include_directories(amspipe
   PUBLIC
      include
)

set_target_properties(amspipe PROPERTIES
    CXX_STANDARD 11
    CXX_STANDARD_REQUIRED YES
    CXX_EXTENSIONS NO
)

# demo application for C++

add_executable(demo
   demo/demo.cpp
)

set_target_properties(demo PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    CXX_EXTENSIONS NO
)

target_link_libraries(demo PRIVATE amspipe)

# TODO: demo application for Fortran
