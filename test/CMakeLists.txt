cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

project(HotUICpp VERSION 0.1.0 LANGUAGES C CXX)

set(GEODE_NO_GEODE_FILE On)

set(SOURCE_FILES test.cpp)
include(../sdk/Geode.cmake)

target_precompile_headers(${PROJECT_NAME} PUBLIC ../mod/src/parse/Managed.hpp)

target_link_libraries(${PROJECT_NAME} "${CMAKE_SOURCE_DIR}/build/RelWithDebInfo/GDML.lib")
