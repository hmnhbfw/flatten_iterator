include_guard(GLOBAL)
after_project_guard()


# By default, it is `${PROJECT_NAME}` in uppercase/lowercase letters with
# underscores (for variables/targets accordingly).
# Otherwise, call the `define_project_namespace(<namespace>)` command.
define_project_namespace()


############################### Project options ################################

#[=============================================================================[
  Prefer that the install rules are available if the project is on the top
  level, e.g. a regular project clone, building, and installing or using
  `ExternalProject`.
  Otherwise, neither when using `add_subdirectory` nor when using `FetchContent`
  is usually expected to generate install rules.
#]=============================================================================]
project_option(ENABLE_INSTALL "Enable the library installation"
  ON IF (PROJECT_IS_TOP_LEVEL AND (NOT CMAKE_SKIP_INSTALL_RULES))
  AUTHOR_WARNING
    "\n"
    "Installation is not expected when `PROJECT_IS_TOP_LEVEL` is set to `OFF` and `CMAKE_SKIP_INSTALL_RULES` is set to `ON`. But:\n"
    "PROJECT_IS_TOP_LEVEL=${PROJECT_IS_TOP_LEVEL}\n"
    "CMAKE_SKIP_INSTALL_RULES=${CMAKE_SKIP_INSTALL_RULES}\n"
)

#[=============================================================================[
  Allow to install all external dependencies locally (e.g. using `FetchContent`,
  or `ExternalProject` and downloading external sources into the binary
  directory), except to those that is not allowed explicitly by setting
  `${NAMESPACE_UPPER}_INSTALL_<dependency-name>_LOCALLY`.
  `<dependency-name>` is just a name using by the `find_package` command.
#]=============================================================================]
project_option(INSTALL_EXTERNALS_LOCALLY "Install external dependencies locally"
  OFF
)

project_option(ENABLE_DEVELOPER_MODE "Enable developer mode"
  OFF WEAK IF (NOT PROJECT_IS_TOP_LEVEL)
  AUTHOR_WARNING
    "Developer mode is intended for developers of \"${PROJECT_NAME}\".\n"
)

# Developer options
project_dev_option(ENABLE_TESTING "Enable testing")
project_dev_option(ENABLE_BENCHMARKING "Enable benchmarking")
project_dev_option(ENABLE_COVERAGE "Enable code coverage testing")
project_dev_option(ENABLE_FORMATTING "Enable code formatting")
# TODO: implement other developer options

project_option(ENABLE_DOCS "Enable creating documentation" OFF)

# TODO(?): For small projects it is useless to enable `ccache`
project_option(ENABLE_CCACHE "Enable ccache" OFF)

#[=============================================================================[
  Enable treating the project's include directories as system via passing the
  `SYSTEM` option to the `target_include_directories` command. This may have
  effects such as suppressing warnings or skipping the contained headers in
  dependency calculations (see compiler documentation).
#]=============================================================================]
project_option(ENABLE_TREATING_INCLUDES_AS_SYSTEM
  "Use the `SYSTEM` option for the project's includes, compilers may disable warnings"
  ON IF (NOT PROJECT_IS_TOP_LEVEL)
)


##################### Project non-boolean cached variables #####################

if (ENABLE_INSTALL)
  # Provides install directory variables as defined by the GNU Coding Standards.
  # See: https://www.gnu.org/prep/standards/html_node/Directory-Variables.html
  include(GNUInstallDirs)

  project_cached_variable(PACKAGE_NAME
    ${namespace_lower} STRING
    "The package name used by the `find_package` command"
  )

  project_cached_variable(INSTALL_INCLUDE_DIR
    "${CMAKE_INSTALL_INCLUDEDIR}" PATH
    "Installation directory for public headers"
  )

  project_cached_variable(INSTALL_CMAKE_DIR
    "${CMAKE_INSTALL_DATAROOTDIR}/${PACKAGE_NAME}/cmake" PATH
    "Installation directory for CMake configuration files"
  )
  
  project_cached_variable(INSTALL_LICENSE_DIR
    "${CMAKE_INSTALL_DATAROOTDIR}/${PACKAGE_NAME}" PATH
    "Installation directory for the LICENSE file"
  )

  if (ENABLE_DOCS)
    project_cached_variable(INSTALL_DOC_DIR
      "${CMAKE_INSTALL_DATAROOTDIR}/${PACKAGE_NAME}/doc" PATH
      "Installation directory for documentation"
    )
  endif()
endif(ENABLE_INSTALL)


############################ Variable init guard ###############################

# Prevent processing listfiles before including `Variables`
macro(variable_init_guard)
  # Do nothing, just check if this macro exists
endmacro()
