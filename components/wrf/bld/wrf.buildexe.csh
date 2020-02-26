#! /bin/csh -V

set OBJROOT = "."
set CODEROOT = "."
set MACH = ""
set COMPILER = ""
set CASE = ""
set DEBUGDIR = ""

while ($# > 0)
    echo "argv[1]=$argv[1]"
    switch( $argv[1] )
	case "-objroot":
	    set OBJROOT = $2
	    echo "objroot=$OBJROOT"
	    shift
	    breaksw
	case "-coderoot":
	    set CODEROOT = $2
	    echo "coderoot=$CODEROOT"
	    shift
	    breaksw
	case "-mach":
	    set MACH = $2
	    echo "mach=$MACH"
	    shift
	    breaksw
	case "-comp":
	    set COMPILER = $2
	    echo "comp=$COMPILER"
	    shift
	    breaksw
	case "-case":
	    set CASE = $2
	    echo "case=$CASE"
	    shift
	    breaksw
	case "-dbgdir":
	    set DEBUGDIR = $2
	    echo "debugdir=$DEBUGDIR"
	    shift
	    breaksw
	default:
	    echo "NO MATCH"
	    shift
	    breaksw
    endsw
    shift
end

# enter the code directory
set objdir = $OBJROOT
set srcdir = $OBJROOT/../src
mkdir -p $srcdir
mkdir -p $objdir
cd $srcdir
cp -p -r $CODEROOT/components/wrf .
cd ./wrf

# modify CCSMCASE in Registry to case name

cd ./Registry
rm -f *.hold >&! /dev/null
foreach file (Registry.* registry.*)
  mv -f ${file} ${file}.hold
  sed -e "s#CCSMCASE#$CASE#" < ${file}.hold > ${file}
end
cd $srcdir/wrf

setenv WRF_EM_CORE	1
setenv WRF_NMM_CORE	0
setenv WRF_COAMPS_CORE	0
setenv WRF_EXP_CORE	0
setenv DM_FC mpif90
setenv DM_CC mpicc
setenv DEBUGDIR $DEBUGDIR
echo $DEBUGDIR

# for atm_mct_comp compiler

if (-f configure.wrf.${MACH}_${COMPILER}) then
  cp -f configure.wrf.${MACH}_${COMPILER} configure.wrf
else if (-f configure.wrf.${MACH}) then
  cp -f configure.wrf.${MACH} configure.wrf
else
  echo "configure.wrf for ${MACH} ${COMPILER} not found"
  exit 0
endif
./compile em_real $DEBUGDIR
echo $DEBUG

# for ccsm compiler
cd $srcdir/wrf/external/esmf_time_f90
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_netcdf
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/fftpack/fftpack5
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib_share
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib1
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib1/grib1_util
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib1/MEL_grib1
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib1/WGRIB
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/RSL_LITE
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_int
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/frame
ar ru $srcdir/wrf/main/libwrflib.a module_internal_header_util.o pack_utils.o

cd $srcdir/wrf/main
ar ru $srcdir/wrf/main/libwrflib.a atm_io_tools.o atm_radiation_mod.o atm_files_mod.o atm_cpl_indices.o atm_grid_mod.o atm_comp_mct.o module_wrf_top.o atm_instance.o

# esmf_wrf
cd $srcdir/wrf/external/esmf_time_f90

cp *.mod $LIBROOT/include

cd $srcdir/wrf

cp -p *.mod $objdir/
cp -p */*.mod $objdir/
cp -p */*/*.mod $objdir/
cp -p */*/*/*.mod $objdir/
cp -p main/atm_comp*.mod  $LIBROOT/include
cp -p main/libwrflib.a  $LIBROOT/libatm.a


