cmake_minimum_required(VERSION 3.10 FATAL_ERROR)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(GDMLTestMod VERSION 0.0.1)

add_library(${PROJECT_NAME} SHARED main.cpp)
set_target_properties(${PROJECT_NAME} PROPERTIES PREFIX "")

add_dependencies(${PROJECT_NAME} GDMLv4)

include(../GDML.cmake)

process_gdml_files(${PROJECT_NAME}
    MyLayer.gdml
)

add_subdirectory(K:/GeodeSDK/geode K:/GeodeSDK/geode/build)

target_link_libraries(${PROJECT_NAME} geode-sdk)
create_geode_file(${PROJECT_NAME})
