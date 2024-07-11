#[=============================================================================[
  Author: Pavel Tsayukov
  Distributed under the MIT License. See accompanying file LICENSE or
  https://opensource.org/license/mit for details.
  ------------------------------------------------------------------------------
  MSVC compiler options
#]=============================================================================]

include_guard(GLOBAL)


set(CXX_WARNING_OPTIONS
  # Displays level 1, 2, 3, and 4 warnings, that aren't off by default.
  /W4

  # Enable some warnings that are turned off by default.
  # See: https://learn.microsoft.com/en-us/cpp/preprocessor/compiler-warnings-that-are-off-by-default

  /w14311
  /w14545
  /w14546
  /w14547
  /w14549
  /w14555
  /w14905
  /w14906
  /w14928
  /w15038
  /w15262

  /w24826

  /w34265
  /w34287
  /w34619

  /w44062
  /w44242
  /w44254
  /w44263
  /w44296
  /w44365
  /w44388
  /w44464
  /w44837
  /w45038
  /w45259
  /w45263
)

set(CXX_ERROR_OPTIONS
  # Treats all compiler warnings as errors.
  /WX
)

set(CXX_LANGUAGE_OPTIONS
  # See: https://learn.microsoft.com/en-us/cpp/build/reference/compiler-options-listed-by-category#language

  # Specify standards conformance mode to the compiler.
  /permissive-

  # Enable updated `__cplusplus` macro
  /Zc:__cplusplus

  # Enable `enum` type deduction; previously, the Microsoft compiler always used `int`.
  /Zc:enumTypes

  # Enable extern constexpr variables.
  /Zc:externConstexpr

  # Functions declared `inline` must have a definition available in the same translation unit if they're used.
  /Zc:inline

  # Enable preprocessor conformance mode.
  /Zc:preprocessor

  # Assume operator new throws; skip checks for a null pointer return.
  /Zc:throwingNew
)

set(CXX_DIAGNOSTIC_OPTIONS
  # See: https://learn.microsoft.com/en-us/cpp/build/reference/compiler-options-listed-by-category#diagnostics

  /diagnostics:caret
)
