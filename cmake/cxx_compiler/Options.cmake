include_guard(GLOBAL)
after_project_guard()


if (MSVC)
  include_project_module(cxx_compiler/Msvc.Options)
else()
  include_project_module(cxx_compiler/Gnu.Options)
endif()

add_project_library(cxx_warning_options INTERFACE)
target_compile_options(${cxx_warning_options} INTERFACE ${CXX_WARNING_OPTIONS})

add_project_library(cxx_error_options INTERFACE)
target_compile_options(${cxx_error_options} INTERFACE ${CXX_ERROR_OPTIONS})

add_project_library(cxx_language_options INTERFACE)
target_compile_options(${cxx_language_options} INTERFACE ${CXX_LANGUAGE_OPTIONS})

add_project_library(cxx_diagnostic_options INTERFACE)
target_compile_options(${cxx_diagnostic_options} INTERFACE ${CXX_DIAGNOSTIC_OPTIONS})

add_project_library(cxx_options INTERFACE)
target_link_libraries(${cxx_options}
  INTERFACE
    ${cxx_warning_options}
    ${cxx_error_options}
    ${cxx_language_options}
    ${cxx_diagnostic_options}
)
