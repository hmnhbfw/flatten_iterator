include_guard(GLOBAL)
after_project_guard()


#[=============================================================================[
  If the documentation is enabled, add the target for building the documentation
  of files that can be recursively found in the `INPUTS` parameters and to place
  the result in the `OUTPUT` parameter.

    add_docs_if_enabled(<target_alias>
                        FORMAT <format>
                        INPUTS <files or directories> ...
                        [OUTPUT <output_directory>])

  The true target name is `${namespace_lower}_${target_alias}` with defined
  `${target_alias}` variable set to the target name.

  See supported formats: https://www.doxygen.nl/manual/starting.html#step2.

  By default, `<output_directory>` is `${DOXYGEN_OUTPUT_DIRECTORY}/<format>`.

  Also generate install rules for documentation if `${ENABLE_INSTALL}` is set.
#]=============================================================================]
function(add_docs_if_enabled target_alias)
  set(options "")
  set(one_value_keywords FORMAT OUTPUT)
  set(multi_value_keywords INPUTS)
  cmake_parse_arguments(PARSE_ARGV 1 "args"
    "${options}"
    "${one_value_keywords}"
    "${multi_value_keywords}"
  )

  enable_if_project_variable_is_set(ENABLE_DOCS)

  if (NOT args_OUTPUT)
    string(TOLOWER ${args_FORMAT} args_OUTPUT)
  endif()

  if (IS_ABSOLUTE "${args_OUTPUT}")
    set(output "${args_OUTPUT}")
  else()
    set(output "${DOXYGEN_OUTPUT_DIRECTORY}/${args_OUTPUT}")
  endif()

  string(TOUPPER ${args_FORMAT} args_FORMAT)

  set(DOXYGEN_GENERATE_${args_FORMAT} YES)
  set(DOXYGEN_${args_FORMAT}_OUTPUT "${output}")

  set(target ${namespace_lower}_${target_alias})
  doxygen_add_docs(
    ${target} ${args_INPUTS}
    WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
    COMMENT "Generate ${args_FORMAT} documentation"
  )
  set(${target_alias} ${target} PARENT_SCOPE)

  enable_if_project_variable_is_set(ENABLE_INSTALL)

  install(DIRECTORY
      "${output}"
    DESTINATION
      "${INSTALL_DOC_DIR}"
    COMPONENT "docs" EXCLUDE_FROM_ALL
  )
endfunction()


enable_if_project_variable_is_set(ENABLE_DOCS)


################## Initialization the documentation generator ##################

find_package(Doxygen REQUIRED OPTIONAL_COMPONENTS dot)
# Get the path to the executable file
get_target_property(doxygen Doxygen::doxygen IMPORTED_LOCATION)

find_path(doxygen_awesome_SOURCE_DIR
    doxygen-awesome.css
  PATH_SUFFIXES
    share/doxygen-awesome-css
)
mark_as_advanced(FORCE doxygen_awesome_SOURCE_DIR)
if (NOT doxygen_awesome_SOURCE_DIR)
  can_install_locally(doxygen_awesome)
  include(FetchContent)
  FetchContent_Declare(doxygen_awesome
    GIT_REPOSITORY https://github.com/jothepro/doxygen-awesome-css
    GIT_TAG 40e9b25b6174dd3b472d8868f63323a870dfeeb8 # v2.3.3
  )
  FetchContent_MakeAvailable(doxygen_awesome)
endif()


######################### Doxygen template generation ##########################

set(doxygen_template_header "header.html")
set(doxygen_template_footer "footer.html")
set(doxygen_template_custom_css "customdoxygen.html")
set(doxygen_template_dir "${PROJECT_BINARY_DIR}/docs/template")
file(MAKE_DIRECTORY "${doxygen_template_dir}")

execute_process(COMMAND
    ${doxygen} -w html
      ${doxygen_template_header}
      ${doxygen_template_footer}
      ${doxygen_template_custom_css}
  WORKING_DIRECTORY "${doxygen_template_dir}"
  ERROR_VARIABLE doxygen_generation_error
)
if (doxygen_generation_error)
  message(FATAL_ERROR
    "\n"
    "An error occurred while generating Doxygen template files.\n"
    "${doxygen_generation_error}\n"
  )
endif()


############################ Configuration options #############################

# See: https://www.doxygen.nl/manual/config.html

# Project related configuration options
set(DOXYGEN_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/docs")

# Build related configuration options
set(DOXYGEN_EXTRACT_ALL YES)

# Configuration options related to the input files
set(DOXYGEN_RECURSIVE YES)
set(DOXYGEN_USE_MDFILE_AS_MAINPAGE
  "${PROJECT_SOURCE_DIR}/README.md"
)

#Configuration options related to the HTML output
set(DOXYGEN_GENERATE_TREEVIEW YES)
set(DOXYGEN_DISABLE_INDEX NO)
set(DOXYGEN_FULL_SIDEBAR NO)
set(DOXYGEN_HTML_COLORSTYLE "LIGHT")

set(DOXYGEN_HTML_HEADER
  "${doxygen_template_dir}/${doxygen_template_header}"
)

set(DOXYGEN_HTML_EXTRA_STYLESHEET
  "${doxygen_awesome_SOURCE_DIR}/doxygen-awesome.css"
  "${doxygen_awesome_SOURCE_DIR}/doxygen-awesome-sidebar-only.css"
  "${doxygen_awesome_SOURCE_DIR}/doxygen-awesome-sidebar-only-darkmode-toggle.css"
)

# See: https://jothepro.github.io/doxygen-awesome-css/md_docs_2extensions.html#extension-dark-mode-toggle
set(DOXYGEN_HTML_EXTRA_FILES
  "${doxygen_awesome_SOURCE_DIR}/doxygen-awesome-darkmode-toggle.js"
)
file(APPEND "${doxygen_template_dir}/header.html"
[=[
<!-- Dark Mode Toggle -->
<script type="text/javascript" src="$relpath^doxygen-awesome-darkmode-toggle.js"></script>
<script type="text/javascript">
  DoxygenAwesomeDarkModeToggle.init()
</script>
]=]
)

# Configuration options related to diagram generator tools
if (Doxygen_dot_FOUND)
  set(DOXYGEN_HAVE_DOT YES)
  set(DOXYGEN_DOT_IMAGE_FORMAT "svg")
  set(DOXYGEN_DOT_TRANSPARENT YES)
endif()
