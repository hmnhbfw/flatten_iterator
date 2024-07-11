include_guard(GLOBAL)
no_in_source_builds_guard()
__variable_init_guard()


# Modules that should be located using the `${CMAKE_MODULE_PATH}` list, e.g.
# `Find<package>.cmake` to use the `find_package(<package>)` command.
list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake/modules")
