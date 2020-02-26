#! /bin/csh -f 

#
# ROMS CESM input data script
# 
# Apr 18, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - revised
#

set ocn_dir = components/roms
set NTASKS_OCN = 1
set DIN_LOC_ROOT = "."
set domainfilepath = "."
set INST_STR = ""
set CONTINUE_RUN = "FALSE"
set RUN_STARTDATE = ""

while ($# > 0)

    echo "argv[1]=$argv[1]"
    switch( $argv[1] )
	case "-ntasks":
	    set NTASKS_OCN = $2
	    echo "ntasks=$NTASKS_OCN"
	    shift
	    breaksw
	case "-continue":
	    set CONTINUE_RUN = $2
	    echo "continue=$CONTINUE_RUN"
	    shift
	    breaksw
	case "-startdate":
	    set RUN_STARTDATE = $2
	    echo "startdate=$RUN_STARTDATE"
	    shift
	    breaksw
	case "-starttod":
	    set RUN_STARTTOD = $2
	    echo "run_starttod=$RUN_STARTTOD"
	    shift
	    breaksw
	case "-ocngrid":
	    set OCN_GRID = $2
	    echo "ocngrid=$OCN_GRID"
	    shift
	    breaksw
	case "-coderoot":
	    set CODEROOT = $2
	    echo "coderoot=$CODEROOT"
	    shift
	    breaksw
	case "-rundir":
	    set RUNDIR = $2
	    echo "rundir=$RUNDIR"
	    shift
	    breaksw
	case "-dinlocroot":
	    set DIN_LOC_ROOT = $2
	    echo "dinlocroot=$DIN_LOC_ROOT"
	    shift
	    breaksw
	case "-inst_string":
	    set INST_STR = $2
	    echo "inst_string=$INST_STR"
	    shift
	    breaksw
	case "-domfile":
	    set domainfilepath = $2
	    echo "domainfilepath=$domainfilepath"
	    shift
	    breaksw
	default:
	    echo "NO MATCH"
	    shift
	    breaksw
    endsw
    shift

end

## To calculate decomp, set up Ocn_nx and Ocn_ny based on the 
## domain grid
if ($OCN_GRID =~ gom3) then
    set OCN_NX = 655
    set OCN_NY = 489
else if ($OCN_GRID =~ gom3x) then
    set OCN_NX = 856
    set OCN_NY = 811
else
    echo "Unknown OCN grid \[$OCN_GRID\], unable to calculate ROMS decomposition"
    exit 1
endif
    

# compute roms ntilei and ntilej
# shoot for square blocks
# would like bfac to be a real, since it isn't, use bfacr as proxy

set ntif = -1
set ntjf = -1
set bnxf = -1
set bnyf = -1
set bfacf = -1
set bfacrf = -1
set nti = 1
while ($nti <= ${NTASKS_OCN})
  @ ntmod = ${NTASKS_OCN} % $nti
  if ($ntmod == 0) then
     @ ntj = ${NTASKS_OCN} / $nti
#     echo "nti ntj $nti $ntj ${NTASKS_OCN}"
     @ bnx = ${OCN_NX} / $nti
     if (${OCN_NX} % $nti > 0) @ bnx = $bnx + 1
     @ bny = ${OCN_NY} / $ntj
     if (${OCN_NY} % $ntj > 0) @ bny = $bny + 1

     @ bfac1 = $bnx / $bny
     @ bfac2 = $bny / $bnx
     if ($bfac1 > $bfac2) then
        set bfac = $bfac1
        @ bfacr = $bnx % $bny
     else
        set bfac = $bfac2
        @ bfacr = $bny % $bnx
     endif
#     echo "bnx bny $bnx $bny $bfac"
     set update = "no"
     if ($ntif < 0) then
        set update = "yes"
     else
        if ($bfac < $bfacf) set update = "yes"
        if ($bfac == $bfacf && $bfacr < $bfacrf) set update = "yes"
     endif
#     echo "bfac $bfac $bfacf $bfacr $bfacrf $update"
     if ($update == "yes") then
        set ntif = $nti
        set ntjf = $ntj
        set bnxf = $bnx
        set bnyf = $bny
        set bfacf = $bfac
        set bfacrf = $bfacr
     endif
  endif
  @ nti = $nti + 1
end

if ($ntif < 0) then
  echo "roms decomp not found for $NTASKS_OCN"
  exit -2
else
  echo "roms decomp for ${NTASKS_OCN} tasks is $ntif $ntjf"
endif

set oinfile = "$CODEROOT/${ocn_dir}/Apps/${OCN_GRID}/ocean.in"
set ofile = ${RUNDIR}/ocean.in
set input_data_dir = ${DIN_LOC_ROOT}/ocn/roms/gom3/

if !(-e $oinfile) then
  echo "$oinfile roms input file not found"
  exit -2
endif

set tfile = tmpfile1
cat $oinfile | sed "s/\(^\s*NtileI\s*==\s*\)[0-9]*/\1 $ntif /g" >! $tfile
cat $tfile   | sed "s/\(^\s*NtileJ\s*==\s*\)[0-9]*/\1 $ntjf /g" >! $ofile
rm -f $tfile

cp -f $CODEROOT/${ocn_dir}/Apps/${OCN_GRID}/ocn_in ${RUNDIR} || exit -2
cp -f $CODEROOT/${ocn_dir}/ROMS/External/varinfo.dat ${RUNDIR} || exit -2

# Calculate start date and if this run is a restart.
set contrunupper = ` echo $CONTINUE_RUN | tr "[a-z]" "[A-Z]" `
echo "contrunupper = $contrunupper"
if ( $contrunupper == 'TRUE' ) then
   set rest_flag = '.true.'
   set date = `cat $RUNDIR/rpointer.drv${INST_STR} | sed "s/\.nc//; s/^.*\.r\.//;"`
else
   set rest_flag = '.false.'
   set date = ${RUN_STARTDATE}-${RUN_STARTTOD}
endif
set st_year = `echo $date | cut -d "-" -f 1`
set st_mon  = `echo $date | cut -d "-" -f 2`
set st_day  = `echo $date | cut -d "-" -f 3`
set st_tod  = `echo $date | cut -d "-" -f 4`

@ st_hr = ${st_tod} / 3600
@ st_min = ( ${st_tod} % 3600 ) / 60
@ st_sec = ( ${st_tod} % 3600 ) % 60

sed -i "s/CASE_ST_YR/${st_year}/" ${RUNDIR}/ocn_in
sed -i "s/CASE_ST_MO/${st_mon}/" ${RUNDIR}/ocn_in
sed -i "s/CASE_ST_DA/${st_day}/" ${RUNDIR}/ocn_in
sed -i "s/CASE_ST_HR/${st_hr}/" ${RUNDIR}/ocn_in
sed -i "s/CASE_ST_MI/${st_min}/" ${RUNDIR}/ocn_in
sed -i "s/CASE_ST_SE/${st_sec}/" ${RUNDIR}/ocn_in


### Move over grid and boundary data for a given compset
if (($OCN_GRID == gom3)||($OCN_GRID == gom3x)) then
    echo "gom3 or gom3x boundary data copied to run dir"
    cp -f ${input_data_dir}/gom03_grd_N050_md15m.nc ${RUNDIR} || exit -2
    cp -f ${input_data_dir}/gom03_N050_md15m_bry_HYCOM_GBL_19p1_2010_01.nc ${RUNDIR} || exit -2
    cp -f ${input_data_dir}/gom03_N050_md15m_ini_HYCOM_GBL_19p1_201001.nc ${RUNDIR} || exit -2
    cp -f ${input_data_dir}/gom03_N050_md15m_nudg_HYCOM_GBL_19p1_201001.nc ${RUNDIR} || exit -2
endif
if ($OCN_GRID == gom3x) then
    echo "docn support data copied to run dir"
    cp -f ${input_data_dir}/gom03_xroms_sstice.nc ${RUNDIR}
    cp -f ${input_data_dir}/docn.streams.txt.prescribed ${RUNDIR}
    cp -f $CODEROOT/${ocn_dir}/Apps/${OCN_GRID}/docn_in ${RUNDIR}/docn_in${INST_STR}
    cp -f $CODEROOT/${ocn_dir}/Apps/${OCN_GRID}/docn_ocn_in ${RUNDIR}/docn_ocn_in${INST_STR}
    sed -i "s/docn_ocn_in/docn_ocn_in${INST_STR}/" ${RUNDIR}/docn_in${INST_STR}
    sed -i "s%replacedomainfile%${domainfilepath}%" ${RUNDIR}/docn_ocn_in${INST_STR}
endif

#cp -f /pic/scratch/tcraig/IRESM/inputdata/ocn/roms/${OCN_GRID}/* ${RUNDIR} || exit -2
# ^^^ What grid files need to be copied into the run directory, and how should I go about that?
