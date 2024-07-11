#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Formatting
  ------------------------------------------------------------------------------
  Enable the `${NAMESPACE}_ENABLE_DEVELOPER_MODE` or
  `${NAMESPACE}_ENABLE_FORMATTING` project option to turn formatting on.
  See `../Variables.cmake` for details.
#]=============================================================================]

include_guard(GLOBAL)


enable_if_project_variable_is_set(ENABLE_FORMATTING)

# TODO: implement
