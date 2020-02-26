#! /bin/csh  -x

#
# ROMS CESM build script
# 
# Apr 18, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - revised
#

set OBJROOT = "."
set CODEROOT = "."
set OCN_GRID = ""

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
	case "-ocn_grid":
	    set OCN_GRID = $2
	    echo "ocn_grid=$OCN_GRID"
	    shift
	    breaksw
	case "-gmakej":
	    set GMAKE_J = $2
	    echo "gmakej=$GMAKE_J"
	    shift
	    breaksw
	case "-casetools":
	    set CASETOOLS = $2
	    echo "casetools=$CASETOOLS"
	    shift
	    breaksw
	case "-caseroot":
	    set CASEROOT = $2
	    echo "caseroot=$CASEROOT"
	    shift
	    breaksw
	case "-mach":
	    set MACH = $2
	    echo "mach=$MACH"
	    shift
	    breaksw
	default:
	    echo "NO MATCH"
	    shift
	    breaksw
    endsw
    shift
end

set objdir = $OBJROOT
set srcdir = $OBJROOT/../src
mkdir -p $srcdir
mkdir -p $objdir
ln -sf $CODEROOT/components/roms/* $srcdir/

# Set the CPP option defining the particular application. This will                           
# determine the name of the ".h" header file with the application                             
# CPP definitions.                                                                            
set ROMS_APPLICATION = UPWELLING
# Set a local environmental variable to define the path to the directories              
# where all this project's files are kept.                
set MY_ROOT_DIR = $CODEROOT
set MY_PROJECT_DIR = $CODEROOT

# The path to the user's local current ROMS source code.                                      
#                                                                                             
# If using svn locally, this would be the user's Working Copy Path (WCPATH).                  
# Note that one advantage of maintaining your source code locally with svn                    
# is that when working simultaneously on multiple machines (e.g. a local                      
# workstation, a local cluster and a remote supercomputer) you can checkout                   
# the latest release and always get an up-to-date customized source on each                   
# machine. This script is designed to more easily allow for differing paths                   
# to the code and inputs on differing machines.                                                
#setenv MY_ROMS_SRC          ${MY_ROOT_DIR}/branches/arango                                   
set MY_ROMS_SRC = $srcdir

# Set path of the directory containing makefile configuration (*.mk) files.                   
# The user has the option to specify a customized version of these files                      
# in a different directory than the one distributed with the source code,                     
# ${MY_ROMS_SRC}/Compilers. If this is the case, the you need to keep                         
# these configurations files up-to-date.                                                       
set COMPILERS = ${MY_ROMS_SRC}/Compilers

# Other user defined environmental variables. See the ROMS makefile for                       
# details on other options the user might want to set here. Be sure to                        
# leave the switches meant to be off set to an empty string or commented                      
# out. Any string value (including off) will evaluate to TRUE in                              
# conditional if-statements.                                                                  
 set USE_MPI = on          # distributed-memory parallelism                      
 set USE_MPIF90 = on          # compile with mpif90 script                          
 set which_MPI = mpich       # compile with MPICH library                          
#setenv which_MPI           mpich2      # compile with MPICH2 library                        
#setenv which_MPI           openmpi     # compile with OpenMPI library    
 set FORT  = ifort
 set USE_LARGE = on          # activate 64-bit compilation 
#setenv USE_DEBUG           on          # use Fortran debugging flags        

# The rest of this script sets the path to the users header file and                          
# analytical source files, if any. See the templates in User/Functionals.                     
#                                                                                             
# If applicable, use the MY_ANALYTICAL_DIR directory to place your                            
# customized biology model header file (like fennel.h, nemuro.h, ecosim.h,                    
# etc).                                                                                      
 set MY_HEADER_DIR = ${objdir}
 set MY_ANALYTICAL_DIR = ${objdir}
# Put the binary to execute in the following directory.                                       
 set BINDIR = ${objdir}
# Put the f90 files in a project specific Build directory to avoid conflict                   
# with other projects.                                                                        
 set SCRATCH_DIR = ${objdir}

set ROMS_BUILD_DIR = ${objdir}
set ROMS_DIR = ${srcdir}
set ROMS_APPLICATION = ${OCN_GRID}
set ROMS_APPL_SM = `echo ${ROMS_APPLICATION} | tr A-Z a-z`
set ROMS_HEADFILE = ${ROMS_APPL_SM}.h
set ROMS_HEADER_DIR = ${srcdir}/Apps/${OCN_GRID}
#set ROMS_MCT_CPPDEFS = "-DMPI -DROMS_HEADER=${ROMS_HEADER_DIR}/${ROMS_HEADFILE}"
set ROMS_MCT_CPPDEFS = '-DMPI -DROMS_HEADER=\"'${ROMS_HEADER_DIR}'/'${ROMS_HEADFILE}'\"  '
echo $ROMS_MCT_CPPDEFS
if (${COMPILER} == 'intel') then
    setenv ROMS_FORT     ifort
else
    setenv ROMS_FORT     ${COMPILER}
endif
setenv NETCDF_INCDIR     `nc-config --includedir`

mkdir -p ${ROMS_BUILD_DIR}


env | grep ROMS

cd ${ROMS_DIR}
gmake -j ${GMAKE_J} libraries ROMS_APPLICATION=${ROMS_APPLICATION} MY_HEADER_DIR=${ROMS_HEADER_DIR} SCRATCH_DIR=${ROMS_BUILD_DIR} FORT=${ROMS_FORT} COMPILERS=${ROMS_DIR}/Compilers
cp -f -p ${ROMS_DIR}/ROMS/Include/cppdefs.h ${ROMS_BUILD_DIR}
cp -f -p ${ROMS_DIR}/ROMS/Include/globaldefs.h ${ROMS_BUILD_DIR}

cd ${objdir}

set comp = 'mct'
#if ($COMP_INTERFACE == 'MCT' ) set comp = mct
#if ($COMP_INTERFACE == 'ESMF') set comp = esmf

cat >! Filepath <<EOF
$CODEROOT/components/roms/drivers/cpl_$comp
$CODEROOT/components/roms/drivers/cpl_share
EOF

gmake complib -j ${GMAKE_J} MODEL=roms COMPLIB=libroms_drv.a USER_INCLDIR="-I ${ROMS_BUILD_DIR}" CPPDEFS="${ROMS_MCT_CPPDEFS}" -f $CASETOOLS/Makefile MACFILE=$CASEROOT/Macros.make || exit 2

ar crv ${LIBROOT}/libocn.a ${objdir}/*.o ${ROMS_BUILD_DIR}/*.o
ranlib ${LIBROOT}/libocn.a
