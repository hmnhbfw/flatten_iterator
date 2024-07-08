include_guard(GLOBAL)


set(CXX_WARNING_OPTIONS
  # This enables all the warnings about constructions that some users consider questionable.
  # See: https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-Wall
  -Wall

  # This enables some extra warning flags that are not enabled by -Wall.
  # See: https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-W
  -Wextra

  # Issue all the warnings demanded by strict ISO C and ISO C++.
  # See: https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-pedantic-1
  -Wpedantic

  # Specific options, as well as those that is not included in Clang by default:

  -Wcast-align
  -Wcast-function-type
  -Wcast-qual
  -Wconversion
  -Wctor-dtor-privacy
  -Wdouble-promotion
  -Wextra-semi
  -Wfloat-equal
  -Wimplicit-fallthrough
  -Wnon-virtual-dtor
  -Wnull-dereference
  -Wold-style-cast
  -Woverloaded-virtual
  -Wshadow
  -Wsign-compare
  -Wsign-conversion
  -Wsign-promo
  -Wtype-limits
  -Wundef
  -Wzero-as-null-pointer-constant
)

set(CXX_ERROR_OPTIONS
  # Make all warnings into errors.
  # See: https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-Werror
  -Werror

  # Give an error whenever the base standard (see -Wpedantic) requires a diagnostic.
  # See: https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-pedantic-errors-1
  -pedantic-errors

  # Suppress specific errors:

  -Wno-error=non-virtual-dtor
)

set(CXX_DIAGNOSTIC_OPTIONS
  # See: https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Message-Formatting-Options.html

  # -fno-elide-type
  -fdiagnostics-show-template-tree
)
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # For GNU compiler; Clang does diagnostics in color by default
  list(APPEND CXX_DIAGNOSTIC_OPTIONS -fdiagnostics-color)
endif()
