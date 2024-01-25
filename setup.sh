#!/bin/sh

set -e # exit on error

usage()
{
  echo "ICAR Setup Script"
  echo ""
  echo "USAGE:"
  echo "./setup.sh [--help|-h] | [-p|--prefix=PREFIX]"
  echo ""
  echo " --help             Display this help text"
  echo " --prefix=PREFIX    Install binary in 'PREFIX/bin'"
  echo "                    Default prefix='\$HOME/.local/bin'"
  echo ""
}

PREFIX="$HOME/.local"

while [ "$1" != "" ]; do
  PARAM=$(echo "$1" | awk -F= '{print $1}')
  VALUE=$(echo "$1" | awk -F= '{print $2}')
  case $PARAM in
    -h | --help)
      usage
      exit
      ;;
    -p | --prefix)
      PREFIX=$VALUE
      ;;
    *)
      echo "ERROR: unknown parameter \"$PARAM\""
      usage
      exit 1
      ;;
  esac
  shift
done

set -u # error on use of undefined variable

if ! command -v brew > /dev/null ; then
  if ! command -v curl > /dev/null ; then
    echo "Please install curl and then rerun ./setup.sh"
    exit 1
  fi
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  if [ $(uname) = "Linux" ]; then
    if [ -z "$PATH" ]; then
      PATH=/home/linuxbrew/.linuxbrew/bin/
    else
      PATH=/home/linuxbrew/.linuxbrew/bin/:"$PATH"
    fi
  fi
fi


brew install cmake netcdf netcdf-fortran fftw pkg-config coreutils # coreutils supports `realpath` below

PREFIX=`realpath $PREFIX`

GIT_VERSION=`git describe --long --dirty --all --always | sed -e's/heads\///'`
FFTW_INCLUDE_PATH="`brew --prefix fftw`/include"
NETCDF_LIB_PATH="`brew --prefix netcdf`/lib"
NETCDFF_LIB_PATH="`brew --prefix netcdf-fortran`/lib"
HDF5_LIB_PATH="`brew --prefix hdf5`/lib"
FFTW_LIB_PATH="`brew --prefix fftw`/lib"

FPM_FLAG="-cpp -DUSE_ASSERTIONS=.true."
FPM_FLAG=" $FPM_FLAG -I$FFTW_INCLUDE_PATH"
FPM_FLAG=" $FPM_FLAG -fallow-argument-mismatch -ffree-line-length-none"
FPM_FLAG=" $FPM_FLAG -DVERSION=\\\'$GIT_VERSION\\\'"
FPM_FLAG=" $FPM_FLAG -L$NETCDF_LIB_PATH -L$FFTW_LIB_PATH -L$HDF5_LIB_PATH -L$NETCDFF_LIB_PATH"
FPM_CC="`which mpicc`"
FPM_CXX="`which mpicxx`"
FPM_FC="`which caf`"
FPM_RUNNER=`which cafrun`
FPM=`which fpm`

PKG_CONFIG_PATH="$PREFIX"/lib/pkgconfig

if [ ! -d $PKG_CONFIG_PATH ]; then
  mkdir -p $PKG_CONFIG_PATH
fi
cd "$PKG_CONFIG_PATH"
  echo "ICAR_FPM_CXX=\"$FPM_CXX\""       >  icar.pc
  echo "ICAR_FPM_CC=\"$FPM_CC\""         >> icar.pc
  echo "ICAR_FPM_FC=\"$FPM_FC\""         >> icar.pc
  echo "ICAR_FPM_FLAG=\"$FPM_FLAG\""     >> icar.pc
  echo "ICAR_FPM_RUNNER=\"$FPM_RUNNER\"" >> icar.pc
  echo "Name: icar"                      >> icar.pc
  echo "Description: Intermediate Complexity Atmospheric Research (ICAR)" >> icar.pc
  echo "URL: https://github.com/ncar/icar"                                >> icar.pc
  echo "Version: 2.0.0"                                                   >> icar.pc
cd -

export PKG_CONFIG_PATH
mkdir -p build
cd build
  cp ../src/run-fpm.sh-header ./run-fpm.sh
  echo "export PKG_CONFIG_PATH"                                            >> run-fpm.sh
  echo "$FPM \$fpm_arguments --verbose \\"                                 >> run-fpm.sh
  echo "--profile release \\"                                              >> run-fpm.sh
  echo "--c-compiler \"`pkg-config icar --variable=ICAR_FPM_CC`\" \\"      >> run-fpm.sh
  echo "--compiler \"`pkg-config icar --variable=ICAR_FPM_FC`\" \\"        >> run-fpm.sh
  echo "--flag \"`pkg-config icar --variable=ICAR_FPM_FLAG`\" \\"          >> run-fpm.sh
  echo "\$program_arguments"                                               >> run-fpm.sh
  chmod u+x run-fpm.sh
cd -

export PKG_CONFIG_PATH
./build/run-fpm.sh test

echo ""
echo "________________ ICAR has been installed! ____________________" 
