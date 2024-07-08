include_guard(GLOBAL)
after_project_guard()
no_in_source_builds_guard()
variable_init_guard()


############################ Dependency management #############################

# Check if `dependency_name` can be installed locally
function(can_install_locally dependency_name)
  if (DEFINED ${NAMESPACE_UPPER}_INSTALL_${dependency_name}_LOCALLY)
    set(allowed ${${NAMESPACE_UPPER}_INSTALL_${dependency_name}_LOCALLY})
  else()
    set(allowed ${${NAMESPACE_UPPER}_INSTALL_EXTERNALS_LOCALLY})
  endif()

  if (NOT allowed)
    message(FATAL_ERROR
      "\n"
      "'${dependency_name}' is not allowed to install locally.\n"
      "Pass `-D${NAMESPACE_UPPER}_INSTALL_${dependency_name}_LOCALLY=ON` if you want otherwise.\n"
      "Passing `-D${NAMESPACE_UPPER}_INSTALL_EXTERNALS_LOCALLY=ON` allows that for all of the external dependencies, except for those that are already set to `OFF`.\n"
    )
  endif()
endfunction()
