#[=============================================================================[
  Project namespace, options, and cached variables.
  ------------------------------------------------------------------------------
  This listfile is intended for editing existing cached variables or/and adding
  additional ones.
#]=============================================================================]

include_guard(GLOBAL)


# By default, it is `${PROJECT_NAME}` in uppercase/lowercase letters with
# underscores (for variables/targets accordingly).
# Otherwise, call the `define_project_namespace(<namespace>)` command.
define_project_namespace()


############################### Project options ################################

#[=============================================================================[
  Allow to install all external dependencies locally (e.g. using `FetchContent`,
  or `ExternalProject` and downloading external sources into the binary
  directory), except to those that is not allowed explicitly by setting
  `${NAMESPACE}_INSTALL_<dependency-name>_LOCALLY`.
  `<dependency-name>` is just a name using by the `find_package` command.
#]=============================================================================]
project_option(INSTALL_EXTERNALS_LOCALLY "Install external dependencies locally" OFF)

#[=============================================================================[
  Prefer that the install rules are available if the project is on the top
  level, e.g. a regular project clone, building, and installing or using
  `ExternalProject`.
  Otherwise, neither when using `add_subdirectory` nor when using `FetchContent`
  is usually expected to generate install rules.
#]=============================================================================]
project_option(ENABLE_INSTALL "Enable the library installation"
  ON IF (PROJECT_IS_TOP_LEVEL AND (NOT CMAKE_SKIP_INSTALL_RULES))
)

project_option(ENABLE_DOCS "Enable creating documentation" OFF)

# For small projects it is useless to enable `ccache`
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
if (ENABLE_TREATING_INCLUDES_AS_SYSTEM)
  project_cached_variable(WARNING_GUARD "SYSTEM" STRING "Warning guard")
endif()


############################## Developer options ###############################

project_option(ENABLE_DEVELOPER_MODE "Enable developer mode" OFF)

if ((NOT PROJECT_IS_TOP_LEVEL) AND ENABLE_DEVELOPER_MODE)
  message(AUTHOR_WARNING "Developer mode is intended for developers of \"${PROJECT_NAME}\".")
endif()

project_dev_option(ENABLE_TESTING "Enable testing")
project_cached_variable(TEST_DIR "tests" PATH "Testing directory")

project_dev_option(ENABLE_BENCHMARKING "Enable benchmarking")
project_cached_variable(BENCHMARK_DIR "benchmarks" PATH "Benchmarking directory")

project_dev_option(ENABLE_COVERAGE "Enable code coverage testing")

project_dev_option(ENABLE_FORMATTING "Enable code formatting")

# TODO: implement other developer options


##################### Project non-boolean cached variables #####################

if (ENABLE_INSTALL)
  # Provides install directory variables as defined by the GNU Coding Standards.
  # See: https://www.gnu.org/prep/standards/html_node/Directory-Variables.html
  include(GNUInstallDirs)

  project_cached_variable(PACKAGE_NAME
    ${namespace} STRING
    "The package name used by the `find_package` command"
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

# For internal use: prevent processing listfiles before including `Variables`
function(__variable_init_guard)
  # Do nothing, just check if this function exists
endfunction()
