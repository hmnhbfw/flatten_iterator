#[=============================================================================[
  Developer mode dependencies
  ------------------------------------------------------------------------------
  It's orthogonal to `Externals.cmake`. These dependencies are only enabled
  under the developer mode and are not used directly by the current project.
  It's expected that this listfile will be included after definitions of the
  project targets, because some of these dependencies may link against the
  project targets.
#]=============================================================================]

include_guard(GLOBAL)


include_project_module(dependencies/Testing)
include_project_module(dependencies/Benchmarking)
include_project_module(dependencies/Coverage)
include_project_module(dependencies/Formatting)
