#!/bin/dash

set -e # exit on error

usage()
{
  echo "ICAR Installation Script"
  echo ""
  echo "USAGE:"
  echo "./install.sh [--help | [--prefix=PREFIX]"
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
    --prefix)
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

# if brew not found, install homebrew
if ! command -v brew > /dev/null ; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

GCC_VER="11"
brew install cmake netcdf fftw gcc@$GCC_VER pkg-config opencoarrays coreutils # coreutils supports `realpath` below

PREFIX=`realpath $PREFIX`

if command -v curl > /dev/null 2>&1; then
    FETCH="curl -L"
elif command -v wget > /dev/null 2>&1; then
    FETCH="wget -O -"
else
    echo "No download mechanism found. Please install curl or wget first."
    exit 1
fi

mkdir -p build/dependencies
git clone https://github.com/Unidata/netcdf-fortran.git build/dependencies/netcdf-fortran
mkdir -p build/dependencies/netcdf-fortran/build
cd build/dependencies/netcdf-fortran/build
  export FC=gfortran-${GCC_VER} CC=gcc-${GCC_VER} CXX=g++-${GCC_VER}
  NETCDF_PREFIX="`brew --prefix netcdf`"
  cmake .. \
    -DNETCDF_C_LIBRARY="$NETCDF_PREFIX/lib" \
    -DNETCDF_C_INCLUDE_DIR="$NETCDF_PREFIX/include"
  make install
cd -

GIT_VERSION=`git describe --long --dirty --all --always | sed -e's/heads\///'`
FFTW_INCLUDE_PATH="`brew --prefix fftw`/include"
NETCDF_LIB_PATH="`brew --prefix netcdf`/lib"
FFTW_LIB_PATH="`brew --prefix fftw`/lib"
export PKG_CONFIG_PATH="$PREFIX"/lib/pkgconfig

FPM_FLAG="-cpp -DUSE_ASSERTIONS=.true."
FPM_FLAG=" $FPM_FLAG -I$FFTW_INCLUDE_PATH"
FPM_FLAG=" $FPM_FLAG -fallow-argument-mismatch -ffree-line-length-none"
FPM_FLAG=" $FPM_FLAG -DVERSION=\\\'$GIT_VERSION\\\'"
FPM_FLAG=" $FPM_FLAG -L$NETCDF_LIB_PATH -L$FFTW_LIB_PATH"
FPM_FC="caf"
FPM_CC="$CC"

cd "$PKG_CONFIG_PATH"
  echo "ICAR_FPM_CXX=\"$CXX\""       >  icar.pc
  echo "ICAR_FPM_CC=\"$FPM_CC\""     >> icar.pc
  echo "ICAR_FPM_FC=\"$FPM_FC\""     >> icar.pc
  echo "ICAR_FPM_FLAG=\"$FPM_FLAG\"" >> icar.pc
  echo "Name: icar"                  >> icar.pc
  echo "Description: Intermediate Complexity Atmospheric Research (ICAR)" >> icar.pc
  echo "URL: https://github.com/ncar/icar"                                >> icar.pc
  echo "Version: 1.0.0"                                                   >> icar.pc
cd -

cd build
  echo "#!/bin/sh"                                                    >  run-fpm.sh
  echo "#-- DO NOT EDIT -- created by icar/install.sh"                >> run-fpm.sh
  echo "\"${PREFIX}\"/bin/fpm \$@ \\"                                 >> run-fpm.sh
  echo "--profile debug \\"                                           >> run-fpm.sh
  echo "--c-compiler \"`pkg-config icar --variable=ICAR_FPM_CC`\" \\" >> run-fpm.sh
  echo "--compiler \"`pkg-config icar --variable=ICAR_FPM_FC`\" \\"   >> run-fpm.sh
  echo "--flag \"`pkg-config icar --variable=ICAR_FPM_FLAG`\""        >> run-fpm.sh
  chmod u+x run-fpm.sh
cd -

./build/run-fpm.sh test

echo ""
echo "________________ ICAR has been installed! ____________________" 
echo ""
echo "To rebuild or to run tests or examples via the Fortran Package"
echo "Manager (fpm) with the required compiler/linker flags, pass a"
echo "fpm command to the build/run-fpm.sh script. For example, run"
echo ""
echo "./build/run-fpm.sh run"
