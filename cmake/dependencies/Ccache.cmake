#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Ccache support for fast recompilation.
  ------------------------------------------------------------------------------
  Enable the `${NAMESPACE_UPPER}_ENABLE_CCACHE` project option to turn ccache on.
  See `Variables.cmake` for details.

  See supported languages:
    - https://ccache.dev/
    - https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_COMPILER_LAUNCHER.html
#]=============================================================================]

include_guard(GLOBAL)
after_project_guard()


# Set `CMAKE_${LANG}_COMPILER_LAUNCHER` if `ccache` is enabled and found
function(use_ccache_if_enabled_for LANG)
  enable_if_project_variable_is_set(ENABLE_CCACHE)

  if (NOT LANG MATCHES "^((OBJ)?C(XX)?|ASM.*|CUDA)$")
    message(AUTHOR_WARNING "${LANG} is not supported by ccache.")
    return()
  endif()

  if (CCACHE_PATH)
    set(CMAKE_${LANG}_COMPILER_LAUNCHER
      "${CCACHE_PATH}" CACHE FILEPATH "${LANG} compiler launcher" FORCE
    )
  endif()
endfunction()


enable_if_project_variable_is_set(ENABLE_CCACHE)

find_program(CCACHE_PATH ccache)
if (NOT CCACHE_PATH)
  message(AUTHOR_WARNING
    "\n"
    "Ccache is not found, that will increase the re-compilation time.\n"
    "Pass `-DCCACHE_PATH=path/to/bin` to specify the path to the `ccache` binary file.\n"
  )
endif()
