cmake_minimum_required(VERSION 3.24)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

project(amspipe)

add_executable(demo
   demo.cpp
   src/amspipe.cpp
   include/amspipe.hpp
)

target_include_directories(demo
   PUBLIC
      include
)
