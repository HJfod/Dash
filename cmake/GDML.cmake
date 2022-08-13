if (NOT DEFINED GDML_CLI)
    find_program(
        GDML_CLI
        NAMES gmdbrew.exe gmdbrew gdml-cli.exe gdml
        REQUIRED
    )
endif()

if (GDML_CLI STREQUAL "GDML_CLI-NOTFOUND")
	message(STATUS "Unable to find GDML Compiler")
else()
    message(STATUS "Found GDML Compiler: ${GDML_CLI}")
endif()

function(process_gdml_files proname)
    message(STATUS "Processing GDML files for ${proname}: ${ARGN}")

    if(GDML_CLI STREQUAL "GDML_CLI-NOTFOUND")
        message(WARNING "process_gdml_files called, but the GDML Compiler was not found")
    else()

        foreach(file ${ARGN})
            list(APPEND SourceFiles
                ${CMAKE_CURRENT_SOURCE_DIR}/${file}
            )
            list(APPEND SourceFilesWithDest
                "${CMAKE_CURRENT_SOURCE_DIR}/${file}->${CMAKE_CURRENT_BINARY_DIR}/${file}.cpp"
            )
            list(APPEND GeneratedFiles
                ${CMAKE_CURRENT_BINARY_DIR}/${file}.cpp
            )
        endforeach()

        add_custom_command(
            OUTPUT ${GeneratedFiles}
            COMMAND ${GDML_CLI} ${SourceFilesWithDest}
            COMMENT "Processing GDML files"
            DEPENDS ${SourceFiles}
            VERBATIM
        )

        target_sources(${proname} PUBLIC ${GeneratedFiles})

    endif()

endfunction()
