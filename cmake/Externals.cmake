include_guard(GLOBAL)
cxx_standard_guard()


include_project_module(dependencies/Docs)
add_docs_if_enabled(docs FORMAT html
  INPUTS
    include
    README.md
)
