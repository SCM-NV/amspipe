cmake_minimum_required(VERSION 3.24)

project(amspipe VERSION 0.1
                DESCRIPTION "a library that implements the worker side of the AMS pipe protocol"
                LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

option(BUILD_SHARED_LIBS "Build shared library (.dll/.so) instead of static ones (.lib/.a)" ON)

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

add_executable(demo
   demo/demo.cpp
)

target_link_libraries(demo PRIVATE amspipe)
