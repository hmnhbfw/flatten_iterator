no_in_source_builds_guard()
cxx_standard_guard()


add_project_executable(basic_range_traits_test basic_range_traits_test.cpp)
target_link_libraries(flatten_view_basic_range_traits_test
  PRIVATE
    flatten_view::flatten_view
    ${CXX_OPTIONS}
    GTest::gtest_main
)
gtest_discover_tests(flatten_view_basic_range_traits_test)

add_project_executable(flatten_view_test flatten_view_test.cpp)
target_link_libraries(flatten_view_flatten_view_test
  PRIVATE
    flatten_view::flatten_view
    flatten_view::generator
    ${CXX_OPTIONS}
    GTest::gtest_main
)
gtest_discover_tests(flatten_view_flatten_view_test)
