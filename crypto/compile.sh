#!/usr/bin/env bash


for i in "$@"
do
case $i in
    -t=*|--target=*)
    TARGET="${i#*=}"
    shift
    ;;
    *)
    echo "Unkown param passed"
    exit 1
    ;;
esac
done

CWD=$PWD
CROSS_COMPILE_SSL="--cross-compile-prefix="

if [[ -z "$TARGET" ]]
then
    TARGET="host"
    CROSS_COMPILE_SSL=""
elif [[ $TARGET == "windows" ]]
then
    COMPILER="x86_64-w64-mingw32-"
    CROSS_COMPILE_SSL="mingw64 "$CROSS_COMPILE_SSL$COMPILER
    CMAKE_TOOLCHAIN_FILE="-DCMAKE_TOOLCHAIN_FILE=${CWD}/mingw_toolchain.cmake"
elif [[ $TARGET == "osx" ]]
then
    COMPILER=""
    echo "Cross Compilation for OSX is not supported!"
    exit 1
else
    echo "Selected target is unknown"
    exit 1
fi

BUILD_DIR=${CWD}/dist/${TARGET}
LIB_CRYPTO_BUILD_DIR=${CWD}/build/${TARGET}

echo "CWD=$CWD"
echo "Build Directory=$BUILD_DIR"
echo "Selected Target=$TARGET"
echo "Final Build Dir=$LIB_CRYPTO_BUILD_DIR"
if [[ ! -z $COMPILER ]]
then
    echo "Selected Compiler=$COMPILER"
    echo "OpenSSL Params=$CROSS_COMPILE_SSL"
fi


read -p "Continue? Y/n " -n 1 -r
echo   # move to new line to prevent ugly exit
if [[ $REPLY =~ ^[Yy]$ || -z "$REPLY" ]]
then
    if [[ ! -d "openssl" ]]
    then
        git clone \
		--recursive \
		--jobs 4 \
		--branch master \
		git://git.openssl.org/openssl.git \
		openssl
    fi
    
    cd openssl && \
	git fetch && \
	git checkout 3f987381929ee725daf4746591144dde18f313e1
    read -p "Reconfigure openssl makefile? Y/n " -n 1 -r
    echo   # move to new line to prevent ugly exit
    if [[ $REPLY =~ ^[Yy]$ || -z "$REPLY" ]]
    then
        perl Configure ${CROSS_COMPILE_SSL} \
		--prefix=${BUILD_DIR} \
		--openssldir=${BUILD_DIR} \
        -static
		-DPEDANTIC \
		-DOPENSSL_USE_IPV6=0 \
		no-comp \
		no-dtls \
		no-nextprotoneg \
		no-psk \
		no-srp \
		no-ssl2 \
		no-ssl3 \
		no-weak-ssl-ciphers \
		no-shared
    fi

    read -p "Compile openssl? Y/n " -n 1 -r
    echo   # move to new line to prevent ugly exit
    if [[ $REPLY =~ ^[Yy]$ || -z "$REPLY" ]]
    then
	    make && make install_sw install_ssldirs
    fi

    read -p "Compile libcrypto for racket? Y/n " -n 1 -r
    echo   # move to new line to prevent ugly exit
    if [[ $REPLY =~ ^[Yy]$ || -z "$REPLY" ]]
    then
        mkdir -p ${LIB_CRYPTO_BUILD_DIR}
        cd ${LIB_CRYPTO_BUILD_DIR}
        echo "Working Dir: $(pwd)"
        echo "CMAKE TOOLCHAIN: ${CMAKE_TOOLCHAIN_FILE}"
        if [[ ! -z $CMAKE_TOOLCHAIN_FILE ]]
        then
            if [[ $TARGET == "windows" ]]
            then
                cmake ${CMAKE_TOOLCHAIN_FILE} -DWINDOWS=ON ${CWD}
            fi
        else
            cmake ${CWD}
        fi
        make && make install && make clean
    fi
    cd ${CWD}/openssl
    make clean
fi
