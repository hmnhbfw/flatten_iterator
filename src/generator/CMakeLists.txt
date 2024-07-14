no_in_source_builds_guard()
cxx_standard_guard()


add_project_library(generator INTERFACE)
target_link_libraries(flatten_iterator_generator
  INTERFACE
    flatten_iterator_cxx_standard
)
target_include_directories(flatten_iterator_generator
  INTERFACE
    "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/generator/include>"
)
