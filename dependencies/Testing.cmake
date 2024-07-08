include_guard(GLOBAL)
after_project_guard()
cxx_standard_guard()


enable_if_project_variable_is_set(ENABLE_TESTING)

enable_testing()

#[=============================================================================[
  TODO(?): specify other testing library if you want so.
  Otherwise, you may also want to specify a particular version of GTest
  in the `find_package(GTest <version>)` command and use other GIT_TAG
  in the `FetchContent_Declare()` command.
#]=============================================================================]

use_cxx_standard_at_least(14)

find_package(GTest)
if (NOT GTest_FOUND)
  can_install_locally(GTest)

  include(FetchContent)
  FetchContent_Declare(
    GTest
    GIT_REPOSITORY https://github.com/google/googletest
    GIT_TAG f8d7d77c06936315286eb55f8de22cd23c188571 # v1.14.0
  )

  # Prevent overriding the parent project's compiler/linker settings
  set(gtest_force_shared_crt ON)

  set(INSTALL_GTEST OFF)

  # TODO(?): set `ON` if testing your project uses GMock
  set(BUILD_GMOCK OFF)

  FetchContent_MakeAvailable(GTest)
endif()

# Get the `gtest_add_tests()` and `gtest_discover_tests()` command
include(GoogleTest)

add_subdirectory(tests)
