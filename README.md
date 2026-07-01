# Fortran parquet library for WAVES/4HS

Library to read/write parquet files and handle MAML files.

## Prerequisites

The code compiles successfully with the following compilers and libraries. It might compile with previous or later versions as well but this is not tested.

- Fortran compiler:
    - Intel Fortran (ifx) v2025.3.0
    - Gfortran v15.2.0
- FPM ([Fortran Package Manager](https://fpm.fortran-lang.org/))
- apache-arrow (C++ library for reading/writing parquet files)

Unit testing is handled using test-drive, which is automatically installed by FPM.

### Environment variables

To build the code with intel compiles, the following environment variables should be set:

- LIBRARY_PATH should point to the Healpix and cfitsio lib directories.
- CPATH should point to the Healpix and cfitsio include directories.
- LD_LIBRARY_PATH should point to the cfitsio lib directory
- FPM_FC can be used to set fortran compiler for FPM (e.g. FPM_FC=ifx)

In bash you can initialise them as follows:

    export PKG_CONFIG_PATH=path_healpix/lib/pkgconfig:$PKG_CONFIG_PATH
    export LIBRARY_PATH=path_healpix/lib:path_cfitsio/lib:$LIBRARY_PATH
    export CPATH=path_healpix/include:path_cfitsio/include:$CPATH
    export LD_LIBRARY_PATH=path_cfitsio/lib:$LD_LIBRARY_PATH
    export FPM_FC=ifx

NB! It might depend on the operating system and fortran compiler what environment variables are needed.

## Building and installing instructions

To test the code:

    fpm test

To clean the build directory:

    fpm clean
    
To clean the build directory and all dependencies:

    fpm clean --all

To generate the executable:

    fpm install --prefix my_path

The executable is placed in the my_path/bin directory.

## Reading parquet files from your fortran code

## Writing parquet files from your fortran code

The parquet file metadata can be defined using the [MAML-format](https://github.com/asgr/MAML-Format).

