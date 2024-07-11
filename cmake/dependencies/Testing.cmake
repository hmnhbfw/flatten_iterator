#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Testing library
  ------------------------------------------------------------------------------
  Specify other testing library if you want so. Otherwise, you may also want to
  specify a particular version of GTest in the `find_package(GTest <version>)`
  command and use other GIT_TAG in the `FetchContent_Declare()` command.

  Enable the `${NAMESPACE_UPPER}_ENABLE_DEVELOPER_MODE` or
  `${NAMESPACE_UPPER}_ENABLE_TESTING` project option to turn testing on.
  See `../Variables.cmake` for details.
#]=============================================================================]

include_guard(GLOBAL)
cxx_standard_guard()


enable_if_project_variable_is_set(ENABLE_TESTING)

enable_testing()

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

  # Set `ON` if testing your project uses GMock
  set(BUILD_GMOCK OFF)

  FetchContent_MakeAvailable(GTest)
endif(NOT GTest_FOUND)

# Get the `gtest_add_tests()` and `gtest_discover_tests()` command
include(GoogleTest)

add_subdirectory(tests)
