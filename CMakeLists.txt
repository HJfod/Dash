cmake_minimum_required(VERSION 3.10 FATAL_ERROR)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")
find_package(LibClang REQUIRED)

project(GDMLv4 VERSION 0.0.1)
set(BINARY_NAME "gmdbrew")

file(GLOB SOURCES
    src/*.cpp
    src/compiler/*.cpp
    src/parser/*.cpp
    src/utils/*.cpp
    src/cli/*.cpp
)
configure_file(src/utils/about.hpp.in ${CMAKE_CURRENT_SOURCE_DIR}/src/utils/about.hpp)

add_compile_definitions(GDML_DEBUG)

add_executable(${PROJECT_NAME} ${SOURCES})
set_target_properties(${PROJECT_NAME} PROPERTIES OUTPUT_NAME ${BINARY_NAME})

target_include_directories(${PROJECT_NAME} PUBLIC
    src
    ${LibClang_INCLUDE_DIRS}
)
target_link_libraries(${PROJECT_NAME} PUBLIC
    ${LibCLang_LIBRARIES}
)

add_subdirectory(test)
