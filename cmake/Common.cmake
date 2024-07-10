#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  Include common variables and commands.
  ------------------------------------------------------------------------------
  Commands:
    * Guards:
      - no_in_source_builds_guard
    * Watchers:
      - mark_command_as_deprecated (3.18+, see details)
      - mark_normal_variable_as_deprecated
    * Project related commands:
      - define_project_namespace
      - include_project_module
      - enable_if_project_variable_is_set
      - project_option
      - project_dev_option
      - project_cached_variable
      - add_project_library
      - add_project_executable
      - get_project_target_property
      - set_project_target_property
    * Host information
      - get_n_physical_cores
      - get_n_logical_cores
    * Miscellaneous
      - xor
    * Debugging
      - print
      - print_var
      - print_var_with
    * TODO: add other modules

  Usage:

  # File: CMakeLists.txt
    cmake_minimum_required(VERSION 3.14 FATAL_ERROR)
    project(my_project_name)

    include("${PROJECT_SOURCE_DIR}/cmake/Common.cmake")
    no_in_source_builds_guard()

#]=============================================================================]

include_guard(GLOBAL)


#[=============================================================================[
  For internal use: this macro must be called at the end of the current listfile.
  It checks if the `project` command is already called, prevents in-source
  builds inside the 'cmake' directory, and initialize some common variables,
  project options, and project cached variables.
#]=============================================================================]
macro(__init_common)
  # This guard should be at the beginning
  after_project_guard()

  if(CMAKE_VERSION LESS "3.14")
    message(FATAL_ERROR
      "Requires a CMake version not lower than 3.14, but got ${CMAKE_VERSION}."
    )
  endif()

  #[===========================================================================[
    The `PROJECT_IS_TOP_LEVEL` is set by the `project` command in CMake 3.21+.
    Otherwise, the custom version of that variable is used that works
    in the same way as described in the `PROJECT_IS_TOP_LEVEL` documentation.
    See: https://cmake.org/cmake/help/latest/variable/PROJECT_IS_TOP_LEVEL.html
  #]===========================================================================]
  if (CMAKE_VERSION LESS "3.21")
    string(COMPARE EQUAL
      "${CMAKE_SOURCE_DIR}" "${PROJECT_SOURCE_DIR}"
      PROJECT_IS_TOP_LEVEL
    )
  endif()

  # Init the project cached variables
  include_project_module(Variables)

  # Include other Commons after Variables
  include_project_module(compiler/Common)
  include_project_module(dependencies/Common)
  include_project_module(install/Common)
  include_project_module(modules/Common)

  # And this guard should be at the end
  no_in_source_builds_guard()
endmacro()


#################################### Guards ####################################

# For internal use: prevent including any listfiles before the `project` command
# in the current project root listfile.
function(after_project_guard)
  if (NOT PROJECT_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    get_filename_component(file_name "${CMAKE_CURRENT_LIST_FILE}" NAME)
    message(FATAL_ERROR
      "'${file_name}' must be included in the current listfile after the `project` command."
    )
  endif()
endfunction()

#[=============================================================================[
  Prevent in-source builds. Prefer to start each listfile with this function.
  Although, if this project is included as a subproject, the outer project
  is allowed to build wherever it wants.
#]=============================================================================]
function(no_in_source_builds_guard)
  if (PROJECT_IS_TOP_LEVEL AND (CMAKE_CURRENT_LIST_DIR STREQUAL CMAKE_BINARY_DIR))
    message(FATAL_ERROR
      "\n"
      "In-source builds are not allowed. Instead, provide a path to build tree like so:\n"
      "cmake -B <binary-directory>\n"
      "Or use presets with an out-of-source build configuration like so:\n"
      "cmake --preset <preset-name>\n"
      "To remove files you accidentally created execute:\n"
      "NOTE: be careful if you had you own directory and files with same names! Use your version control system to restore your data.\n"
      "Linux: rm -rf CMakeFiles CMakeCache.txt cmake_install.cmake\n"
      "Windows (PowerShell): Remove-Item CMakeFiles, CMakeCache.txt, cmake_install.cmake -force -recurse\n"
      "Windows (Command Prompt): rmdir CMakeFiles /s /q && del /q CMakeCache.txt cmake_install.cmake\n"
      "NOTE: Build generator files may also remain, that is, 'Makefile', 'build.ninja' and so forth.\n"
    )
  endif()
endfunction()


################################### Watchers ###################################

#[=============================================================================[
  For CMake version 3.18+, mark the `${command}` command as deprecated because
  of "${reason}".
  For earlier versions, a corresponding macro should be written manually after
  each definition of a deprecated command:

    # E.g. `foo` is a deprecated command now
    macro(foo)
      message(DEPRECATION "The `foo` command is deprecated.")
      _foo(${ARGV})
    endmacro()

#]=============================================================================]
macro(mark_command_as_deprecated command reason)
  cmake_language(EVAL CODE "
    macro(${command})
      message(DEPRECATION \"The `${command}` command is deprecated because of: ${reason}.\")
      _${command}(" [=[ ${ARGV} ]=] ")
    endmacro()"
  )
endmacro()

#[=============================================================================[
  Mark the `${variable}` normal variable as deprecated because of "${reason}".
  Even though this macro can be used only for normal variables, since all
  project cached variables have aliases, these aliases can be used as normal
  variables in this macro.
#]=============================================================================]
macro(mark_normal_variable_as_deprecated variable reason)
  function(__watcher_deprecated_${variable} variable access)
    if (access STREQUAL "READ_ACCESS")
      message(DEPRECATION "The `${variable}` normal variable is deprecated because of: ${reason}")
    endif()
  endfunction()

  variable_watch(${variable} __watcher_deprecated_${variable})
endmacro()


################################################################################
#[=============================================================================[
                            Project related commands

  To prevent name clashes all project cached variables, targets, and properties
  defined by functions and macros below have a prefix followed by underscore.
  This prefix is defined by the `define_project_namespace([<prefix>])` command.
  It is recommended to name cached variables and properties in uppercase letters
  with underscores. On the contrary, it is recommended to name targets in
  lowercase letters with underscore. The prefix, aka namespace, format is
  selected accordingly, in uppercase or lowercase letters with underscores.
  In addition, normal variables are defined for all project cached variables
  and targets. For project cached variables, these variables are set to the
  value of the corresponding project cached variable. For project targets, these
  variables are set to the true target name.

  E.g. the `project_option(ENABLE_FEATURE "Enable a cool feature" ON)` command
  is trying to set an option, aka boolean cached variable, named by
  `${NAMESPACE_UPPER}_ENABLE_FEATURE`. A short alias named by `ENABLE_FEATURE`
  is also defined and set to `ON`.

  E.g. the `add_project_library(my_library INTERFACE)` command defines
  an interface library named by `${namespace_lower}_my_library`. A short alias
  named by `my_library` is also defined and set to the true target name.
  Typical usage: `target_compile_features(${my_library} INTERFACE cxx_std_20)`.

  The `EXPORT_NAME` property is also added to the target and set to `my_library`.
  It is nessecary in order to use the `install(TARGETS ${my_library} ...)` and
  then `install(EXPORT ... NAMESCAPE ${namespace_lower}::)` to export the target
  as `${namespace_lower}::my_library`.

  An alias target named by `${namespace_lower}::my_library` is also defined.
  When a consuming project gets this project via the `find_package` command
  it uses exported targets, e.g. `${namespace_lower}::my_library`, that defined
  by `install(EXPORT ... NAMESPACE ${namespace_lower}::)`.
  But if a consuming project gets this project via the `FetchContent` module or
  `add_subdirectory` command it has to use `${my_library}` until this project
  adds an alias defenition:

    add_library(${namespace_lower}::my_library ALIAS ${my_library})

  Summing up, changing the method of getting this project won't cause
  to change `target_link_libraries(<consuming_target> ... <namespace>::<target>)`
  usage in any of these cases.
#]=============================================================================]

# Define the namespace, by default it is `${PROJECT_NAME}` in the appropriate
# format, see `NAMESPACE_UPPER` and `namespace_lower` variables.
function(define_project_namespace)
  if (ARGC EQUAL "0")
    set(namespace ${PROJECT_NAME})
  else()
    set(namespace ${ARGV0})
  endif()

  string(REGEX REPLACE "[- ]" "_" namespace ${namespace})

  string(TOUPPER ${namespace} namespace)
  set(NAMESPACE_UPPER ${namespace} PARENT_SCOPE)

  string(TOLOWER ${namespace} namespace)
  set(namespace_lower ${namespace} PARENT_SCOPE)
endfunction()

#[=============================================================================[
  Include the `${module}.cmake` file located in the `cmake` directory of
  the current project. It let us include listfiles by name, preventing
  name collisions by using `include(${module})` when a module with
  the same name is defined in the outer score, e.g. the outer project
  sets its own `CMAKE_MODULE_PATH`.
#]=============================================================================]
macro(include_project_module module)
  include("${PROJECT_SOURCE_DIR}/cmake/${module}.cmake")
endmacro()

# Enable the rest of a listfile if the project cached variable is set
macro(enable_if_project_variable_is_set SUFFIX)
  if (NOT ${NAMESPACE_UPPER}_${SUFFIX})
    return()
  endif()
endmacro()

#[=============================================================================[
  Set a project option named `${NAMESPACE_UPPER}_${variable_alias}` if there is
  no such normal or cached variable set before (see CMP0077 for details). Other
  parameters of the `option` command are passed after the `variable_alias`
  parameter, except that the `value` parameter is required now.
  Use the short alias `${ALIAS}` to get the option's value where `ALIAS` is
  `${variable_alias}`.

    project_option(<variable_alias> "<help_text>" <value> [IF <condition>])

  The `condition` parameter after the `IF` keyword is a condition that is used
  inside the `if (<condition>)` command. If it is true, the project option will
  try to set to `${value}`, otherwise, to the opposite one.
#]=============================================================================]
function(project_option variable_alias help_text value)
  set(options "")
  set(one_value_keywords "")
  set(multi_value_keywords IF)
  cmake_parse_arguments(PARSE_ARGV 3 "ARGS"
    "${options}"
    "${one_value_keywords}"
    "${multi_value_keywords}"
  )

  set(variable ${NAMESPACE_UPPER}_${variable_alias})

  if (value)
    set(not_value OFF)
  else()
    set(not_value ON)
  endif()

  if (DEFINED ARGS_IF)
    if (${ARGS_IF})
      option(${variable} "${help_text}" ${value})
    else()
      option(${variable} "${help_text}" ${not_value})
    endif()
  else()
    option(${variable} "${help_text}" ${value})
  endif()

  set(${variable_alias} ${${variable}} PARENT_SCOPE)
endfunction()

#[=============================================================================[
  Set a project option named by `${NAMESPACE_UPPER}_${variable_alias}` to `ON`
  if the developer mode is enable, e.g. by passing
  `-D${NAMESPACE_UPPER}_ENABLE_DEVELOPER_MODE=ON`.
  Note, if this project option is already set, this macro has no effect.
  Use the short alias `${ALIAS}` to get the option's value where `ALIAS`
  is `${variable_alias}`.
#]=============================================================================]
macro(project_dev_option variable_alias help_text)
  project_option(${variable_alias} "${help_text}" ${ENABLE_DEVELOPER_MODE})
endmacro()

#[=============================================================================[
  Set a project cached variable named by `${NAMESPACE_UPPER}_${variable_alias}`
  if there is no such cached variable set before (see CMP0126 for details).
  Use the short alias `${ALIAS}` to get the cached variable's value where
  `ALIAS` is `${variable_alias}`.
#]=============================================================================]
function(project_cached_variable variable_alias value type docstring)
  set(${NAMESPACE_UPPER}_${variable_alias} ${value} CACHE ${type}
    "${docstring}" ${ARGN}
  )
  set(${variable_alias} ${${NAMESPACE_UPPER}_${variable_alias}} PARENT_SCOPE)
endfunction()

# Add a project library target called by `${namespace_lower}_${target_alias}`.
# All parameters of the `add_library` command are passed after `target_alias`.
function(add_project_library target_alias)
  set(${target_alias} ${namespace_lower}_${target_alias})
  add_library(${${target_alias}} ${ARGN})
  add_library(${namespace_lower}::${target_alias} ALIAS ${${target_alias}})
  set_target_properties(${${target_alias}} PROPERTIES EXPORT_NAME ${target_alias})
  set(${target_alias} ${${target_alias}} PARENT_SCOPE)
endfunction()

# Add an executable target called by `${namespace_lower}_${target_alias}`.
# All parameters of the `add_executable` command are passed after `target_alias`.
function(add_project_executable target_alias)
  set(${target_alias} ${namespace_lower}_${target_alias})
  add_executable(${${target_alias}} ${ARGN})
  add_executable(${namespace_lower}::${target_alias} ALIAS ${${target_alias}})
  set_target_properties(${${target_alias}} PROPERTIES EXPORT_NAME ${target_alias})
  set(${target_alias} ${${target_alias}} PARENT_SCOPE)
endfunction()

macro(get_project_target_property variable target project_property)
  get_target_property("${variable}" ${target} ${NAMESPACE_UPPER}_${project_property})
endmacro()

macro(set_project_target_property target project_property value)
  set_target_properties(${target}
    PROPERTIES
      ${NAMESPACE_UPPER}_${project_property} "${value}"
  )
endmacro()

#[=============================================================================[
  Add a header only library as a project interface library.

    add_project_header_only_library(
      <target_alias> [WARNING_GUARD]
      [INCLUDE_DIR <include_dir>]
    )

  `<include_dir>` is the target's include directory. It is `include` by default
  and should be relative to `${PROJECT_SOURCE_DIR}` or be a subdirectory of
  `${PROJECT_SOURCE_DIR}`.

  `WARNING_GUARD` enables treating the target's include directory as system
  via passing the `SYSTEM` option to the `target_include_directories` command.
  By default, it is controlled by the `${ENABLE_TREATING_INCLUDES_AS_SYSTEM}`
  project option.
#]=============================================================================]
function(add_project_header_only_library target_alias)
  set(options WARNING_GUARD)
  set(one_value_keywords INCLUDE_DIR)
  set(multi_value_keywords "")
  cmake_parse_arguments(PARSE_ARGV 1 "args"
    "${options}"
    "${one_value_keywords}"
    "${multi_value_keywords}"
  )

  if (args_WARNING_GUARD OR ENABLE_TREATING_INCLUDES_AS_SYSTEM)
    set(warning_guard "SYSTEM")
  endif()

  if (NOT args_INCLUDE_DIR)
    set(args_INCLUDE_DIR "include")
  endif()

  file(REAL_PATH "${args_INCLUDE_DIR}" include_dir_path
    BASE_DIRECTORY "${PROJECT_SOURCE_DIR}"
  )
  if (NOT EXISTS "${include_dir_path}")
    message(FATAL_ERROR "\"${include_dir_path}\" doesn't exist.")
  endif()

  add_project_library(${target_alias} INTERFACE)
  set(target ${${target_alias}})
  set(${target_alias} ${target} PARENT_SCOPE)

  target_link_libraries(${target} INTERFACE ${cxx_standard})

  target_include_directories(${target}
    ${warning_guard} INTERFACE "$<BUILD_INTERFACE:${include_dir_path}>"
  )
endfunction()


############################### Host information ###############################

# Use the `n_physical_cores` variable to get the number of physical processor
# cores after calling this macro
macro(get_n_physical_cores)
  cmake_host_system_information(RESULT n_physical_cores QUERY NUMBER_OF_PHYSICAL_CORES)
endmacro()

# Use the `n_logical_cores` variable to get the number of logical processor
# cores after calling this macro
macro(get_n_logical_cores)
  cmake_host_system_information(RESULT n_logical_cores QUERY NUMBER_OF_LOGICAL_CORES)
endmacro()


################################ Miscellaneous #################################

# Exclusive OR. Use the `xor_result` variable to get the result.
function(xor lhs rhs)
  if (((NOT "${lhs}") AND "${rhs}") OR ("${lhs}" AND (NOT "${rhs}")))
    set(xor_result ON PARENT_SCOPE)
  else()
    set(xor_result OFF PARENT_SCOPE)
  endif()
endfunction()


################################## Debugging ###################################

macro(print text)
  message("${text}")
endmacro()

macro(print_var variable)
  message("${variable} = \"${${variable}}\"")
endmacro()

macro(print_var_with variable hint)
  message("${hint}: ${variable} = \"${${variable}}\"")
endmacro()


########################### The end of the listfile ############################

__init_common()
