cmake_minimum_required(VERSION 3.16.3 FATAL_ERROR)
option(WINDOWS "Set the target build system to windows" OFF)

project(crypto LANGUAGES C)

set(BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Include relevant libraries and directories based on platform
if(${WINDOWS})
    message("Windows Build Initiated")
    find_library(ws2_32 lib REQUIRED)
    find_library(crypt32 lib REQUIRED)
    set(INCLUDE_DIR "${BASE_DIR}/dist/windows/include")
    set(LIB_CRYPTO_DIR "${BASE_DIR}/dist/windows/lib/libcrypto.a")
    set(LIB_SSL_DIR "${BASE_DIR}/dist/windows/lib/libssl.a")
    set(INSTALL_DIR "${BASE_DIR}/lib/windows")
else()
    message("Host build initiated")
    set(INCLUDE_DIR "${BASE_DIR}/dist/host/include")
    set(LIB_CRYPTO_DIR "${BASE_DIR}/dist/host/lib/libcrypto.a")
    set(LIB_SSL_DIR "${BASE_DIR}/dist/host/lib/libssl.a")
    set(INSTALL_DIR "${BASE_DIR}/lib/host")
endif()

message("SSL Includes: " ${INCLUDE_DIR})
message("libcrypto.a: " ${LIB_CRYPTO_DIR})
message("libssl.a: " ${LIB_SSL_DIR})
message("Install Dir: " ${INSTALL_DIR})


set(CMAKE_INSTALL_PREFIX ${INSTALL_DIR})
add_library(ssl_crypto STATIC IMPORTED GLOBAL)
set_property(TARGET ssl_crypto PROPERTY IMPORTED_LOCATION ${LIB_CRYPTO_DIR})

add_library(ssl64 STATIC IMPORTED GLOBAL)
set_property(TARGET ssl64 PROPERTY IMPORTED_LOCATION ${LIB_SSL_DIR})

include_directories(${INCLUDE_DIR})
add_library(crypto SHARED crypto.c)

if(${WINDOWS})
    target_link_libraries(crypto 
                          PUBLIC ssl64 
                          PUBLIC ssl_crypto
                          PRIVATE ws2_32
                          PRIVATE crypt32)
else()
    target_link_libraries(crypto 
                          PUBLIC ssl64 
                          PUBLIC ssl_crypto)
endif()

install(TARGETS crypto)