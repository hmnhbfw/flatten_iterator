#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  'flatten_iterator' header-only library
#]=============================================================================]

cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

project(flatten_iterator
  VERSION 0.1.0
  DESCRIPTION "TODO: add brief description"
  HOMEPAGE_URL "https://github.com/tsayukov/flatten_iterator"
  LANGUAGES CXX
)

include("${PROJECT_SOURCE_DIR}/cmake/Common.cmake")
no_in_source_builds_guard()

use_cxx_standard_at_least(17)
disable_cxx_extensions()

include_project_module(Externals)

add_project_library(flatten_iterator INTERFACE)
target_link_libraries(${flatten_iterator} INTERFACE ${cxx_standard})
target_include_directories(${flatten_iterator}
  ${WARNING_GUARD}
  INTERFACE
    "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>"
)

include_project_module(DeveloperMode)

if (ENABLE_INSTALL)
  install_project_cmake_configs(COMPATIBILITY "SameMajorVersion" ARCH_INDEPENDENT)
  install_project_license()
  install_project_headers("include")
  install_project_targets(TARGETS ${flatten_iterator} ${cxx_standard} HEADER_ONLY)

  set(CPACK_PACKAGE_VENDOR "Pavel Tsayukov")
  set(CPACK_PACKAGE_FILE_NAME "${PACKAGE_NAME}-${PROJECT_VERSION}")
  set(CPACK_PACKAGE_CHECKSUM "SHA256")
  include(CPack)
endif()
