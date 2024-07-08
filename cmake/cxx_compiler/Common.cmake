include_guard(GLOBAL)
after_project_guard()
no_in_source_builds_guard()
variable_init_guard()


################################ C++ standard ##################################

#[=============================================================================[
  Check if `CMAKE_CXX_STANDARD` is set to at least `standard` and in the very
  first call define the `${cxx_standard}` target as an interface library with
  the corresponding standard which other targets can link against.
  The `${cxx_standard}` should be treated as the least supported standard of
  this library. The first time this macro should be called in the root listfile
  to define the `${cxx_standard}`. Use its next calls to check if some
  dependency's requirement is not greater than the `${cxx_standard}`.
#]=============================================================================]
macro(use_cxx_standard_at_least standard)
  if (NOT CMAKE_CXX_STANDARD)
    set(CMAKE_CXX_STANDARD ${standard})
  endif()

  cmake_cxx_standard_is_not_less_than(${standard})

  if (NOT TARGET ${cxx_standard})
    add_project_library(cxx_standard INTERFACE)
    target_compile_features(${cxx_standard}
      INTERFACE
        cxx_std_${CMAKE_CXX_STANDARD}
    )
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
  endif()
endmacro()

# Raise an error if the `${cxx_standard}` target is not defined
macro(cxx_standard_guard)
  if (NOT TARGET ${cxx_standard})
    message(FATAL_ERROR
      "\n"
      "The `${cxx_standard}` target must be defined.\n"
      "Hint: call the `use_cxx_standard_at_least(<standard>)` command in the root listfile of the current library before this code has been processed.\n"
    )
  endif()
endmacro()


############################ C++ extensions toggle #############################

macro(enable_cxx_extensions)
  set_target_properties(${cxx_standard}
    PROPERTIES
      CXX_EXTENSIONS ON
  )
  set(CMAKE_CXX_EXTENSIONS ON)
endmacro()

macro(disable_cxx_extensions)
  set_target_properties(${cxx_standard}
    PROPERTIES
      CXX_EXTENSIONS OFF
  )
  set(CMAKE_CXX_EXTENSIONS OFF)
endmacro()


######################### Helper functions and macros ##########################

# Helper comparison
function(cmake_cxx_standard_is_not_less_than value)
  set(lhs ${CMAKE_CXX_STANDARD})
  set(rhs ${value})

  if (lhs EQUAL "98")
    set(lhs 9)
  endif()
  if (rhs EQUAL "98")
    set(rhs 9)
  endif()

  if (lhs LESS rhs)
    message(FATAL_ERROR
      "\n"
      "The library requires the C++ standard ${CMAKE_CXX_STANDARD}. Got: ${value}.\n"
    )
  endif()
endfunction()

#[=============================================================================[
  Helper function for optional getting the major, minor, or/and patch version of
  `${compiler_id}` that has to be the current `${lang}` compiler. The default
  language is `CXX`. Raise an error if there is no such `${compiler_id}`.

    get_compiler_version(<compiler_id> [MAJOR] [MINOR] [PATCH]
                         [LANGUAGE <lang>])

  Set a variable called by `${compiler_id}_(major|minor|patch)_version` in
  a parent scope.

  See supported compilers: https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_COMPILER_ID.html
  See supported languages: https://cmake.org/cmake/help/latest/command/project.html
#]=============================================================================]
function(get_compiler_version compiler_id)
  set(options MAJOR MINOR PATCH)
  set(one_value_keywords LANGUAGE)
  set(multi_value_keywords "")
  cmake_parse_arguments(PARSE_ARGV 1 "args"
    "${options}"
    "${one_value_keywords}"
    "${multi_value_keywords}"
  )

  if (NOT args_LANGUAGE)
    set(args_LANGUAGE CXX)
  endif()

  if (NOT CMAKE_${args_LANGUAGE}_COMPILER_ID STREQUAL "${compiler_id}")
    message(FATAL_ERROR
      "\n"
      "${compiler_id} is either a wrong name or not the current ${args_LANGUAGE} compiler.\n"
      "Hint: the current ${args_LANGUAGE} compiler is \"${CMAKE_${args_LANGUAGE}_COMPILER_ID}\".\n"
    )
  endif()

  string(REPLACE "." ";" compiler_version_list "${CMAKE_${args_LANGUAGE}_COMPILER_VERSION}")
  list(LENGTH compiler_version_list size)

  if (args_MAJOR AND (size GREATER_EQUAL "1"))
    list(GET compiler_version_list 0 compiler_major_version)
    set(${compiler_id}_major_version "${compiler_major_version}" PARENT_SCOPE)
  endif()

  if (args_MINOR AND (size GREATER_EQUAL "2"))
    list(GET compiler_version_list 1 compiler_minor_version)
    set(${compiler_id}_minor_version "${compiler_minor_version}" PARENT_SCOPE)
  endif()

  if (args_PATCH AND (size GREATER_EQUAL "3"))
    list(GET compiler_version_list 2 compiler_patch_version)
    set(${compiler_id}_patch_version "${compiler_patch_version}" PARENT_SCOPE)
  endif()
endfunction()
