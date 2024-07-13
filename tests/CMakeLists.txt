no_in_source_builds_guard()
cxx_standard_guard()


add_project_executable(flatten_iterator_test flatten_iterator_test.cpp)
target_link_libraries(${flatten_iterator_test}
  PRIVATE
    flatten_iterator_flatten_iterator
    flatten_iterator_generator
    ${cxx_warning_options}
    ${cxx_language_options}
    ${cxx_diagnostic_options}
    GTest::gtest_main
)
target_compile_options(${flatten_iterator_test} PRIVATE -Wundef -Wno-undef)
gtest_discover_tests(${flatten_iterator_test})
