#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Installation commands
#]=============================================================================]

include_guard(GLOBAL)
no_in_source_builds_guard()
__variable_init_guard()


enable_if_project_variable_is_set(ENABLE_INSTALL)


# Helper functions for creating config files that can be included by other
# projects to find and use a package.
# See: https://cmake.org/cmake/help/latest/module/CMakePackageConfigHelpers.html
include(CMakePackageConfigHelpers)


############################# Installation commands ############################

#[=============================================================================[
  Create and install CMake configuration files for using the `find_package`
  command.

    install_cmake_configs(
      [PATH_VARS <var1> <var2> ... <varN>]
      [NO_SET_AND_CHECK_MACRO]
      [NO_CHECK_REQUIRED_COMPONENTS_MACRO]
      [VERSION <major.minor.patch>]
      COMPATIBILITY <version_compatibility>
      [ARCH_INDEPENDENT]
    )

  This function internally calls the `configure_package_config_file`,
  `write_basic_package_version_file`, and `install(FILES)` commands. Some of
  the parameters of these commands may be passed to this function.

  `INSTALL_DESTINATION` is always `${INSTALL_CMAKE_DIR}`, see the project
  variables in `cmake/Variables.cmake`. `INSTALL_CMAKE_DIR` is also appended to
  the `PATH_VARS` list.

  `VERSION` is `${PROJECT_VERSION}` by default.

  `COMPATIBILITY` is either `AnyNewerVersion`, or `SameMajorVersion`, or
  `SameMinorVersion`, or `ExactVersion`.

  Set `ARCH_INDEPENDENT` for header-only libraries.
#]=============================================================================]
function(install_cmake_configs)
  set(options
    NO_SET_AND_CHECK_MACRO
    NO_CHECK_REQUIRED_COMPONENTS_MACRO
    ARCH_INDEPENDENT
  )
  set(one_value_keywords
    VERSION
    COMPATIBILITY
  )
  set(multi_value_keywords PATH_VARS)
  cmake_parse_arguments(PARSE_ARGV 0 "ARGS"
    "${options}"
    "${one_value_keywords}"
    "${multi_value_keywords}"
  )

  list(APPEND ARGS_PATH_VARS "INSTALL_CMAKE_DIR")

  if (ARGS_NO_SET_AND_CHECK_MACRO)
    set(no_set_and_check_macro "NO_SET_AND_CHECK_MACRO")
  endif()

  if (ARGS_NO_CHECK_REQUIRED_COMPONENTS_MACRO)
    set(no_check_required_components_macro "NO_CHECK_REQUIRED_COMPONENTS_MACRO")
  endif()

  set(config_file "${PACKAGE_NAME}Config.cmake")
  set(config_version_file "${PACKAGE_NAME}ConfigVersion.cmake")

  configure_package_config_file (
    "${PROJECT_SOURCE_DIR}/cmake/install/Config.cmake.in"
    "${PROJECT_BINARY_DIR}/${config_file}"
    INSTALL_DESTINATION "${INSTALL_CMAKE_DIR}"
    PATH_VARS ${ARGS_PATH_VARS}
    ${no_set_and_check_macro}
    ${no_check_required_components_macro}
  )

  if (NOT ARGS_VERSION)
    set(ARGS_VERSION "${PROJECT_VERSION}")
  endif()

  if (ARGS_ARCH_INDEPENDENT)
    set(arch_independent "ARCH_INDEPENDENT")
  endif()

  write_basic_package_version_file(
    "${PROJECT_BINARY_DIR}/${config_version_file}"
    VERSION ${ARGS_VERSION}
    COMPATIBILITY ${ARGS_COMPATIBILITY}
    ${arch_independent}
  )

  install(FILES
      "${PROJECT_BINARY_DIR}/${config_file}"
      "${PROJECT_BINARY_DIR}/${config_version_file}"
    DESTINATION "${INSTALL_CMAKE_DIR}"
    COMPONENT "${namespace}_configs"
  )
endfunction()


# Install the LICENSE file
macro(install_license)
  install(FILES
      "${PROJECT_SOURCE_DIR}/LICENSE"
    DESTINATION "${INSTALL_LICENSE_DIR}"
  )
endmacro()
