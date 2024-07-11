include_guard(GLOBAL)
cxx_standard_guard()


enable_if_project_variable_is_set(ENABLE_BENCHMARKING)

#[=============================================================================[
  TODO(?): specify other benchmarking library if you want so.
  Otherwise, you may also want to specify a particular version of the google
  benchmark library in the `find_package(benchmark <version>)` command and use
  other GIT_TAG in the `FetchContent_Declare()` command.
#]=============================================================================]

use_cxx_standard_at_least(11)

find_package(benchmark)
if (NOT benchmark_FOUND)
  can_install_locally(benchmark)
  use_cxx_standard_at_least(14)

  include(FetchContent)
  FetchContent_Declare(
    benchmark
    GIT_REPOSITORY https://github.com/google/benchmark
    GIT_TAG a4cf155615c63e019ae549e31703bf367df5b471 # v1.8.4
  )

  set(BENCHMARK_ENABLE_INSTALL OFF)
  set(BENCHMARK_INSTALL_DOCS OFF)
  set(BENCHMARK_ENABLE_GTEST_TESTS OFF)

  FetchContent_MakeAvailable(benchmark)
endif()

add_subdirectory(benchmarks)
