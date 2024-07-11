#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Dependency management
  ------------------------------------------------------------------------------
  Prefer to get any dependencies by the `find_package()` command first.
  If it failed, then use the `FetchContent` module.
  It would be nice if you let users decide whether they want to use
  `FetchContent` or not. See `${NAMESPACE_UPPER}_INSTALL_EXTERNALS_LOCALLY`
  in '../Variables.cmake' and, if necessary, set there your own option named by
  `${NAMESPACE_UPPER}_INSTALL_<dependency>_LOCALLY`, where `dependency` is
  a package case-sensitive name using in the `find_package` command.
  Use the `can_install_locally()` function to check this out.

  Usage:

  # File: dependencies/Testing.cmake

    # ...

    find_package(GTest)
    if (NOT GTest_FOUND)
      can_install_locally(GTest)
      include(FetchContent)
      # fetch GTest from its github repo via the `FetchContent` module
      # ...
    endif()

    # ...

#]=============================================================================]

include_guard(GLOBAL)
no_in_source_builds_guard()
__variable_init_guard()


############################ Dependency management #############################

# Check if `dependency_name` can be installed locally
function(can_install_locally dependency_name)
  if (DEFINED ${NAMESPACE_UPPER}_INSTALL_${dependency_name}_LOCALLY)
    set(allowed ${${NAMESPACE_UPPER}_INSTALL_${dependency_name}_LOCALLY})
  else()
    set(allowed ${${NAMESPACE_UPPER}_INSTALL_EXTERNALS_LOCALLY})
  endif()

  if (NOT allowed)
    message(FATAL_ERROR
      "\n"
      "'${dependency_name}' is not allowed to install locally.\n"
      "Pass `-D${NAMESPACE_UPPER}_INSTALL_${dependency_name}_LOCALLY=ON` if you want otherwise.\n"
      "Passing `-D${NAMESPACE_UPPER}_INSTALL_EXTERNALS_LOCALLY=ON` allows that for all of the external dependencies, except for those that are already set to `OFF`.\n"
    )
  endif()
endfunction()
