include_guard(GLOBAL)
after_project_guard()


enable_if_project_variable_is_set(ENABLE_INSTALL)


install_cmake_configs(COMPATIBILITY "SameMajorVersion" ARCH_INDEPENDENT)
install_header_only_library(TARGETS ${my_header_only_library})
install_license()

#[=============================================================================[
  TODO(?): set CPack variables.
  See: https://cmake.org/cmake/help/latest/module/CPack.html
  See the `cpack` command options:
  https://cmake.org/cmake/help/latest/manual/cpack.1.html

  Common usage: `cpack -G ZIP [-B <package_directory>]` will pack all files that
  have been installed via the `install` command to a zip file named
  `${CPACK_PACKAGE_FILE_NAME}` and located in `<package_directory>`, or
  `${CPACK_PACKAGE_DIRECTORY}`, by default, it is the binary directory.
#]=============================================================================]
set(CPACK_PACKAGE_VENDOR "Your [company] name")
set(CPACK_PACKAGE_NAME "${PACKAGE_NAME}")
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}")
set(CPACK_PACKAGE_CHECKSUM "SHA256")
include(CPack)
