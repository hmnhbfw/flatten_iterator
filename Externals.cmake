include_guard(GLOBAL)
after_project_guard()
cxx_standard_guard()


include_project_module(dependencies/Docs)
add_docs_if_enabled(docs FORMAT html INPUTS include README.md)

#[=============================================================================[
  TODO(?) Put configurations of your external dependencies here.

  NOTE: these dependencies are not related to library development.

  1. Prefer to get any dependencies by the `find_package()` command first.
  2. If it failed, then use the `FetchContent` module.
  2a. It would be nice if you let users decide whether they want to use
    `FetchContent` or not.
    See `${NAMESPACE_UPPER}_INSTALL_EXTERNALS_LOCALLY` and
    `${NAMESPACE_UPPER}_INSTALL_<dependency>_LOCALLY` in 'Common.cmake'.
    Use the `can_install_locally()` function to check this out.
    Also see the example in 'Testing.cmake'.
#]=============================================================================]
