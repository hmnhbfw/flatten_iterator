no_in_source_builds_guard()
cxx_standard_guard()


add_project_library(generator INTERFACE)
target_link_libraries(${generator} INTERFACE ${cxx_standard})
target_include_directories(${generator}
  INTERFACE
    "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/generator/include>"
)
