#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Interface libraries with different compiler options
#]=============================================================================]

include_guard(GLOBAL)


if (MSVC)
  include_project_module(compiler/Msvc.CxxOptions)
else()
  include_project_module(compiler/Gnu.CxxOptions)
endif()

# Include `CXX_ERROR_OPTIONS` explicitly
list(APPEND CXX_OPTIONS
  ${CXX_WARNING_OPTIONS}
  ${CXX_LANGUAGE_OPTIONS}
  ${CXX_DIAGNOSTIC_OPTIONS}
)
