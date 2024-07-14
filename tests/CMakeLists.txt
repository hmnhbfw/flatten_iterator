no_in_source_builds_guard()
cxx_standard_guard()


add_project_executable(tests flatten_iterator_test.cpp)
target_link_libraries(flatten_iterator_tests
  PRIVATE
    flatten_iterator_flatten_iterator
    flatten_iterator_generator
    ${CXX_OPTIONS}
    GTest::gtest_main
)
gtest_discover_tests(flatten_iterator_tests)
