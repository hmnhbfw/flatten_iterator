#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Define commands, variables, and targets to work with enabled languages.
  ------------------------------------------------------------------------------
  Commands for language `${lang}` (e.g. `c`, `cxx`, in lower case):
    * Common commands:
      - get_${lang}_compiler_version
    * C/C++ standard:
      - use_${lang}_standard_at_least
      - ${lang}_standard_guard
    * C/C++ extensions toggle:
      - enable_${lang}_extensions
      - disable_${lang}_extensions
    * Interface C/C++ libraries (use `target_link_libraries` to link against
      them):
      - ${${lang}_standard}
      - ${cxx_options}
      - ${cxx_warning_options}
      - ${cxx_error_options}
      - ${cxx_language_options}
      - ${cxx_diagnostic_options}

  Usage:

  # File: CMakeLists.txt
    cmake_minimum_required(VERSION 3.14 FATAL_ERROR)
    project(my_project_name CXX) # enable C++ language

    include("${PROJECT_SOURCE_DIR}/cmake/Common.cmake")
    no_in_source_builds_guard()

    use_cxx_standard_at_least(20)
    disable_cxx_extensions()

    add_subdirectory(externals)

    add_project_library(my_library)
    target_link_libraries(${my_library} PUBLIC ${cxx_standard})
    target_link_libraries(${my_library} PRIVATE ${cxx_options})
    # other target commands

  # File: externals/CMakeLists.txt
    no_in_source_builds_guard()
    cxx_standard_guard()

    use_cxx_standard_at_least(17) # okay
    # include an external library that requires at least the standard C++17

    use_cxx_standard_at_least(20) # okay
    # include an external library that requires at least the standard C++20

    use_cxx_standard_at_least(23) # error!
    # include an external library that requires at least the standard C++23

  Setting `CMAKE_CXX_STANDARD` to `23` before the first calling of the
  `use_cxx_standard_at_least` command can eliminate that error, but probably
  this is not what you want. If an external library your project depends on
  requires at least the standard C++23, your project should require the same.

  Still, toggling between different `CMAKE_CXX_STANDARD` for different builds is
  a good way to test your project against different standard, no less than the
  least required.
#]=============================================================================]

include_guard(GLOBAL)
no_in_source_builds_guard()
__variable_init_guard()


# For internal use: this macro must be called at the end of the current listfile.
# It defines commands and variables to work with enabled languages.
macro(__init_compiler_common)
  include_project_module(dependencies/Ccache)

  get_property(__languages GLOBAL PROPERTY ENABLED_LANGUAGES)
  foreach (__lang IN LISTS __languages)
    string(TOUPPER "${__lang}" __LANG)
    string(TOLOWER "${__lang}" __lang)

    __include_compiler_commands(${__lang} ${__LANG})
    __include_compiler_options(${__lang} ${__LANG})
    use_ccache_if_enabled_for(${__LANG})
  endforeach()
endmacro()


# For internal use: define common commands to work with `${lang}` compiler
macro(__include_compiler_commands lang LANG)
  ############################# Common commands ################################

  #[===========================================================================[
    Helper function for optional getting the major, minor, or/and patch version
    of `${compiler_id}` that has to be the current `${LANG}` compiler.
    Raise an error if there is no such `${compiler_id}`.

      # E.g. `${lang}` is `cxx`
      get_cxx_compiler_version(<compiler_id> [MAJOR] [MINOR] [PATCH])

    Set a variable called by `${compiler_id}_(major|minor|patch)_version` in
    a parent scope.

    See supported compilers: https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_COMPILER_ID.html
    See supported languages: https://cmake.org/cmake/help/latest/command/project.html
  #]===========================================================================]
  function(get_${lang}_compiler_version compiler_id)
    set(options MAJOR MINOR PATCH)
    set(one_value_keywords "")
    set(multi_value_keywords "")
    cmake_parse_arguments(PARSE_ARGV 1 "ARGS"
      "${options}"
      "${one_value_keywords}"
      "${multi_value_keywords}"
    )

    if (NOT CMAKE_${LANG}_COMPILER_ID STREQUAL "${compiler_id}")
      message(FATAL_ERROR
        "\n"
        "${compiler_id} is either a wrong name or not the current ${LANG} compiler.\n"
        "Hint: the current ${LANG} compiler is \"${CMAKE_${LANG}_COMPILER_ID}\".\n"
      )
    endif()

    string(REPLACE "." ";" compiler_version_list "${CMAKE_${LANG}_COMPILER_VERSION}")
    list(LENGTH compiler_version_list size)

    if (ARGS_MAJOR AND (size GREATER_EQUAL "1"))
      list(GET compiler_version_list 0 compiler_major_version)
      set(${compiler_id}_major_version "${compiler_major_version}" PARENT_SCOPE)
    endif()

    if (ARGS_MINOR AND (size GREATER_EQUAL "2"))
      list(GET compiler_version_list 1 compiler_minor_version)
      set(${compiler_id}_minor_version "${compiler_minor_version}" PARENT_SCOPE)
    endif()

    if (ARGS_PATCH AND (size GREATER_EQUAL "3"))
      list(GET compiler_version_list 2 compiler_patch_version)
      set(${compiler_id}_patch_version "${compiler_patch_version}" PARENT_SCOPE)
    endif()
  endfunction()

  ############################ Only C/C++ support ##############################

  if ("${LANG}" MATCHES "^C(XX)?$")
    ############################# C/C++ standard ###############################

    #[=========================================================================[
      Check if `CMAKE_${LANG}_STANDARD` is set to at least `standard` and
      in the very first call define the `${${lang}_standard}` target as
      an interface library with the corresponding standard which other targets
      can link against. `${${lang}_standard}` should be treated as the
      least supported standard of this library. The first time this function
      should be called in the root listfile to define `${${lang}_standard}`.
      Use its next calls to check if some dependency's requirement is not
      greater than `${${lang}_standard}`.
    #]=========================================================================]
    function(use_${lang}_standard_at_least standard)
      # Helper comparison of C/C++ standards
      function(__compare_standards lhs rhs)
        if (NOT lhs MATCHES "^9[0-9]$")
          set(lhs ${lhs}00)
        endif()
        if (NOT rhs MATCHES "^9[0-9]$")
          set(rhs ${rhs}00)
        endif()

        if (lhs LESS rhs)
          set(result "LESS")
        elseif (lhs GREATER rhs)
          set(result "GREATER")
        else() # lhr EQUAL rhs
          set(result "EQUAL")
        endif()

        set(__compare_standards_result ${result} PARENT_SCOPE)
      endfunction()

      set(current_standard ${CMAKE_${LANG}_STANDARD})
      if (NOT current_standard)
        set(current_standard ${standard})
      endif()

      __compare_standards(${current_standard} ${standard})
      if (__compare_standards_result STREQUAL "LESS")
        message(FATAL_ERROR
          "The library requires the ${LANG} standard ${current_standard}, but got: ${standard}."
        )
      endif()

      set(target_alias ${lang}_standard)
      if (NOT TARGET ${${target_alias}})
        add_project_library(${target_alias} INTERFACE)
        set(target ${${target_alias}})
        set(${target_alias} ${target} PARENT_SCOPE)
        target_compile_features(${target}
          INTERFACE
            ${lang}_std_${current_standard}
        )
        set(CMAKE_${LANG}_STANDARD_REQUIRED ON PARENT_SCOPE)
      endif()

      set(CMAKE_${LANG}_STANDARD ${current_standard} PARENT_SCOPE)
    endfunction()

    # Raise an error if the `${${lang}_standard}` target is not defined
    function(${lang}_standard_guard)
      if (NOT TARGET ${${lang}_standard})
        message(FATAL_ERROR
          "\n"
          "The `${namespace_lower}_${lang}_standard` target must be defined.\n"
          "Hint: call the `use_${lang}_standard_at_least(<standard>)` command in the root listfile of the current library before this code has been processed.\n"
        )
      endif()
    endfunction()


    ######################### C/C++ extensions toggle ##########################

    function(enable_${lang}_extensions)
      set_target_properties(${${lang}_standard} PROPERTIES ${LANG}_EXTENSIONS ON)
      set(CMAKE_${LANG}_EXTENSIONS ON PARENT_SCOPE)
    endfunction()

    function(disable_${lang}_extensions)
      set_target_properties(${${lang}_standard} PROPERTIES ${LANG}_EXTENSIONS OFF)
      set(CMAKE_${LANG}_EXTENSIONS OFF PARENT_SCOPE)
    endfunction()

  endif("${LANG}" MATCHES "^C(XX)?$")

endmacro()


macro(__include_compiler_options lang LANG)
  # So far, support only C++ options
  if ("${LANG}" STREQUAL "CXX")
    include_project_module(compiler/CxxOptions)
  endif()

  # TODO: add C options support
endmacro()


########################### The end of the listfile ############################

__init_compiler_common()
