#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Modules that should be located using the `${CMAKE_MODULE_PATH}` list, e.g.
  `Find<package>.cmake` in order to use the `find_package(<package>)` command.
#]=============================================================================]

include_guard(GLOBAL)
no_in_source_builds_guard()
__variable_init_guard()


list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake/modules")
