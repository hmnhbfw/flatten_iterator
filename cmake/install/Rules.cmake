#[=============================================================================[
  Install rules
  ------------------------------------------------------------------------------
  This listfile is expected to be included at the very last.
#]=============================================================================]

include_guard(GLOBAL)


enable_if_project_variable_is_set(ENABLE_INSTALL)

# Install configuration files

install_cmake_configs(COMPATIBILITY "SameMajorVersion" ARCH_INDEPENDENT)

# Install public headers

install(DIRECTORY
    "${PROJECT_SOURCE_DIR}/include/"
  DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
  COMPONENT "${namespace}_headers"
  FILES_MATCHING
    PATTERN "*.h"
    PATTERN "*.hpp"
)

# Generate and install `${PACKAGE_EXPORT_TARGET_NAME}.cmake` file

install(TARGETS
    ${flatten_iterator}
    ${cxx_standard}
  EXPORT "${PACKAGE_EXPORT_TARGET_NAME}"
  INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)

install(EXPORT
    "${PACKAGE_EXPORT_TARGET_NAME}"
  NAMESPACE ${namespace}::
  DESTINATION "${INSTALL_CMAKE_DIR}"
  COMPONENT "${namespace}_configs"
)

# Install extra files

install_license()


#[=============================================================================[
  CPack variables
  ------------------------------------------------------------------------------
  See: https://cmake.org/cmake/help/latest/module/CPack.html
  See the `cpack` command options:
  https://cmake.org/cmake/help/latest/manual/cpack.1.html

  Common usage: `cpack -G ZIP [-B <package_directory>]` will pack all files that
  have been installed via the `install` command to a zip file named
  `${CPACK_PACKAGE_FILE_NAME}` and located in `<package_directory>`, or
  `${CPACK_PACKAGE_DIRECTORY}`, by default, it is the binary directory.
#]=============================================================================]
set(CPACK_PACKAGE_VENDOR "Pavel Tsayukov")
set(CPACK_PACKAGE_NAME "${PACKAGE_NAME}")
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}")
set(CPACK_PACKAGE_CHECKSUM "SHA256")
include(CPack)
