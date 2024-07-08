include_guard(GLOBAL)
after_project_guard()


#[=============================================================================[
  Set `CMAKE_${language}_COMPILER_LAUNCHER` if `ccache` is enabled and found.
  See supported languages:
    - https://ccache.dev/
    - https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_COMPILER_LAUNCHER.html
#]=============================================================================]
function(use_ccache_if_enabled_for language)
  enable_if_project_variable_is_set(ENABLE_CCACHE)
  if (CCACHE_PATH)
    set(CMAKE_${language}_COMPILER_LAUNCHER
      "${CCACHE_PATH}" CACHE FILEPATH "${language} compiler launcher"
    )
  endif()
endfunction()


enable_if_project_variable_is_set(ENABLE_CCACHE)

if (NOT CCACHE_PATH)
  find_program(CCACHE_PATH ccache)
endif()

if (NOT CCACHE_PATH)
  message(AUTHOR_WARNING
    "\n"
    "Ccache is not found, that will increase the re-compilation time.\n"
    "Pass `-DCCACHE_PATH=path/to/bin` to specify the path to the `ccache` binary file.\n"
  )
endif()
