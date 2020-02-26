#! /bin/csh -x

#echo " "
#echo "*************************************************"
#echo "You should do the following manually before running the CCSM build script:"
#echo "  setenv NETCDF /usr/local/pkg/netcdf/netcdf-3.6.1.pgi (for midnight)"
#echo "  cd models/atm/wrf"
#echo "  ./clean -a"
#echo "  ./configure"
#echo "  choose 4 (for midnight)"
#echo "  choose 1"
#echo "*************************************************"
#echo " "

set RUNDIR = "."
set CONTINUE_RUN = "FALSE"
set CALENDAR = "GREGORIAN"
set RUN_STARTDATE = ""
set STOP_OPTION = ""
set STOP_N = ""
set ATM_GRID = ""
set DIN_LOC_ROOT = "."
set INST_STR = ""

while ($# > 0)

    echo "argv[1]=$argv[1]"
    switch( $argv[1] )
	case "-rundir":
	    set RUNDIR = $2
	    echo "rundir=$RUNDIR"
	    shift
	    breaksw
	case "-continue":
	    set CONTINUE_RUN = $2
	    echo "continue_run=$CONTINUE_RUN"
	    shift
	    breaksw
	case "-startdate":
	    set RUN_STARTDATE = $2
	    echo "run_startdate=$RUN_STARTDATE"
	    shift
	    breaksw
	case "-starttod":
	    set RUN_STARTTOD = $2
	    echo "run_starttod=$RUN_STARTTOD"
	    shift
	    breaksw
	case "-stopoption":
	    set STOP_OPTION = $2
	    echo "stop_option=$STOP_OPTION"
	    shift
	    breaksw
	case "-stopn":
	    set STOP_N = $2
	    echo "stop_n=$STOP_N"
	    shift
	    breaksw
	case "-atmgrid":
	    set ATM_GRID = $2
	    echo "atm_grid=$ATM_GRID"
	    shift
	    breaksw
	case "-dinlocroot":
	    set DIN_LOC_ROOT = $2
	    echo "DIN_LOC_ROOT = $DIN_LOC_ROOT"
	    shift
	    breaksw
        case "-inst_string":
            set INST_STR = $2
            echo "inst_string=$INST_STR"
            shift
            breaksw
	default:
	    echo "NO MATCH"
	    shift
	    breaksw
    endsw
    shift

end

# prepare namelist and other parameters
cd $RUNDIR

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
echo "startdate = $date"
set st_year = `echo $date | cut -d "-" -f 1`
set st_mon  = `echo $date | cut -d "-" -f 2`
set st_day  = `echo $date | cut -d "-" -f 3`
set st_tod  = `echo $date | cut -d "-" -f 4`

@ st_hr = ${st_tod} / 3600
@ st_min = ( ${st_tod} % 3600 ) / 60
@ st_sec = ( ${st_tod} % 3600 ) % 60

@ y4   = ($st_year / 4) * 4
@ y100 = ($st_year / 100) * 100
@ y400 = ($st_year / 400) * 400

set leap = 0
if ($st_year == $y4  ) set leap = 1
if ($st_year == $y100) set leap = 0
if ($st_year == $y400) set leap = 1

#echo $y4 $y100 $y400 $leap

if ($CALENDAR == 'GREGORIAN' && $leap == 1) then
  set days_in_month = ('31' '29' '31' '30' '31' '30' '31' '31' '30' '31' '30' '31')
  set months   = ('01' '02' '03' '04' '05' '06' '07' '08' '09' '10' '11' '12')
else
  set days_in_month = ('31' '28' '31' '30' '31' '30' '31' '31' '30' '31' '30' '31')
  set months   = ('01' '02' '03' '04' '05' '06' '07' '08' '09' '10' '11' '12')
endif

# Calculate the number of days we should run by setting run_days in WRF namelist.
# Note: run_days is the number of days to run after WRF start_year, start_month, and start_day
# It is NOT the number of days to run after CCSM run_startdate.  So, if we are running one
# month jobs, run_days will never be more than the number of days in the month.

if ($STOP_OPTION == 'ndays') then
    set run_days = $STOP_N
    # This will be a problem if the number of run days takes us into the next month
    # because our WRF lateral boundary conditions are on a month-by-month basis.
    @ wrf_end_date = $run_days + $st_day - 1
    if ($wrf_end_date > $days_in_month[$st_mon]) then
        echo "You are asking to run RACM past available WRF lateral boundary conditions."
        echo "Set stop_option in env_run.xml to nmonths rather than ndays to accomodate this."
        exit 0
    endif
endif

if ($STOP_OPTION == 'nmonths') then
    if ($STOP_N != '1') then
        echo "WRF lateral bounday conditions exist on a month-by-month basis. Please run one month at a time."
        exit 0
    endif
    echo $days_in_month[$st_mon]
    @ run_days = 1 + $days_in_month[$st_mon] - $st_day
endif

if ($STOP_OPTION != 'ndays' && $STOP_OPTION != 'nmonths') then
    echo "Please set stop_option in env_run.xml to either ndays or nmonths."
    exit 0
endif


# Turn on or off spectral nudging here.
# 0 = no spectral nudging; 1 = turn on spectral nudging
set spectral_nudging = 0
if ($ATM_GRID =~ us20) then
  set spectral_nudging = 1
endif

if ($spectral_nudging) then
    set nudging_switch = 2
else
    set nudging_switch = 0
endif

#--------------------------------------------------------------------------
# ATM_GRID is wr50a
#--------------------------------------------------------------------------

if ($ATM_GRID =~ wr50a) then
set link_data = 0
if ($link_data) then
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/CAM_ABS_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/CAM_AEROPT_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ETAMPNEW_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ETAMPNEW_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTM_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTM_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_SW_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_SW_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_LW_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_LW_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ozone.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ozone_lat.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/ozone_plev.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/GENPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/LANDUSE.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/SOILPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/URBPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/VEGPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/tr49t67 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/tr49t85 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/tr67t85 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/co2_trans .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/gribmap.txt .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/data/grib2map.tbl .

  foreach yp (0 1 2 3)
  foreach mon (01 02 03 04 05 06 07 08 09 10 11 12)
     @ yr = ${st_year} + ${yp}
     ln -sf $DIN_LOC_ROOT/atm/wrf/boundary/wrfbdy_d01_${yr}${mon}01 wrfbdy_d01_${yr}-${mon}-01_00:00:00
  end
  end
  ln -sf   ./wrfbdy_d01_${st_year}-${st_mon}-01_00:00:00 wrfbdy_d01

  if (${CONTINUE_RUN} == 'FALSE') then
    ln -sf   $DIN_LOC_ROOT/atm/wrf/boundary/wrfinput_d01_${st_year}${st_mon}01  wrfinput_d01
  endif
  if ($spectral_nudging) then
    foreach yp (0 1 2 3)
    foreach mon (01 02 03 04 05 06 07 08 09 10 11 12)
      @ yr = ${st_year} + ${yp}
      ln -sf $DIN_LOC_ROOT/atm/wrf/boundary/wrffdda_d01_${yr}${mon}01 wrffdda_d01_${yr}-${mon}-01_00:00:00
    end
    end
    ln -sf   ./wrffdda_d01_${st_year}-${st_mon}-01_00:00:00  wrffdda_d01
  endif
else
  cp   $DIN_LOC_ROOT/atm/wrf/data/CAM_ABS_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/CAM_AEROPT_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ETAMPNEW_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ETAMPNEW_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTM_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTM_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_SW_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_SW_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_LW_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/data/RRTMG_LW_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ozone.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ozone_lat.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/data/ozone_plev.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/data/GENPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/LANDUSE.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/SOILPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/URBPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/VEGPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/data/tr49t67 .
  cp   $DIN_LOC_ROOT/atm/wrf/data/tr49t85 .
  cp   $DIN_LOC_ROOT/atm/wrf/data/tr67t85 .
  cp   $DIN_LOC_ROOT/atm/wrf/data/co2_trans .
  cp   $DIN_LOC_ROOT/atm/wrf/data/gribmap.txt .
  cp   $DIN_LOC_ROOT/atm/wrf/data/grib2map.tbl .
  cp   $DIN_LOC_ROOT/atm/wrf/boundary/wrfbdy_d01_${st_year}${st_mon}01 wrfbdy_d01
  if (${CONTINUE_RUN} == 'FALSE') then
    usr/bin/rcp   $DIN_LOC_ROOT/atm/wrf/boundary/wrfinput_d01_${st_year}${st_mon}01  wrfinput_d01
  endif
  if ($spectral_nudging) then
    usr/bin/rcp   $DIN_LOC_ROOT/atm/wrf/boundary/wrffdda_d01_${st_year}${st_mon}01  wrffdda_d01
  endif
  chmod u+w *
endif

cat >! namelist.input <<EOF
 &time_control
 start_year                          = ${st_year}
 start_month                         = ${st_mon}
 start_day                           = ${st_day}
 start_hour                          = ${st_hr}
 start_minute                        = ${st_min}
 start_second                        = ${st_sec}
 run_days                            = ${run_days}
 interval_seconds                    = 21600
 input_from_file                     = .true.
 history_interval                    = 180
 frames_per_outfile                  = 1
 restart                             = ${rest_flag}
 restart_interval                    = 1440
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 /

 &domains
 time_step                           = 150
 time_step_fract_num                 = 0
 time_step_fract_den                 = 1
 max_dom                             = 1
 s_we                                = 1
 e_we                                = 276
 s_sn                                = 1
 e_sn                                = 206
 e_vert                              = 40
 eta_levels                          =  1.00000, 0.99667, 0.99268, 0.98738, 0.98077,
                                        0.97223, 0.96179, 0.94884, 0.93346, 0.91447,
                                        0.89203, 0.86633, 0.83640, 0.80141, 0.76188,
                                        0.71838, 0.67156, 0.62208, 0.57068, 0.51720,
                                        0.46214, 0.40769, 0.35800, 0.31274, 0.27161,
                                        0.23431, 0.20056, 0.17106, 0.14553, 0.12291,
                                        0.10287, 0.08512, 0.06940, 0.05547, 0.04313,
                                        0.03220, 0.02251, 0.01394, 0.00634, 0.00000,
 p_top_requested                     = 5000
 num_metgrid_levels                  = 30
 num_metgrid_soil_levels             = 4
 dx                                  = 50000
 dy                                  = 50000
 grid_id                             = 1
 parent_id                           = 0
 i_parent_start                      = 1
 j_parent_start                      = 1
 parent_grid_ratio                   = 1
 parent_time_step_ratio              = 1
 feedback                            = 1
 nproc_x                             = -1,
 nproc_y                             = -1,
 smooth_option                       = 0
 /

 &physics
 mp_physics                          = 7,
 gsfcgce_hail                        = 0,
 gsfcgce_2ice                        = 0,
 co2tf                               = 1,
 ra_lw_physics                       = 3,
 ra_sw_physics                       = 3,
 radt                                = 20,
 sf_sfclay_physics                   = 1,
 bl_pbl_physics                      = 1,
 sf_surface_physics                  = 2,
 bldt                                = 0,
 cu_physics                          = 3,
 cudt                                = 5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 pxlsm_smois_init                    = 1,
 usemonalb                           = .false.
 fractional_seaice                   = 1
 sf_urban_physics                    = 0,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 slope_rad                           = 0,
 topo_shading                        = 0,
 cam_abs_freq_s                      = 21600
 cam_abs_dim2                        = 40
 cam_abs_dim1                        = 4
 paerlev                             = 29
 levsiz                              = 59
 /

 &fdda
 grid_fdda                           = ${nudging_switch}
 gfdda_inname                        = "wrffdda_d<domain>",
 gfdda_interval_m                    = 360,
 fgdt                                = 0,
 gfdda_end_h                         = 9999999,
 if_no_pbl_nudging_uv                = 0,
 if_no_pbl_nudging_t                 = 0,
 if_no_pbl_nudging_ph                = 0,
 if_zfac_uv                          = 1,
 if_zfac_t                           = 1,
 if_zfac_ph                          = 1,
 k_zfac_uv                           = 20,
 k_zfac_t                            = 20,
 k_zfac_ph                           = 50,
 guv                                 = 0.0003,
 gt                                  = 0.0003,
 gph                                 = 0.0003,
 dk_zfac_uv                          = 30
 dk_zfac_t                           = 30
 dk_zfac_ph                          = 30
 xwavenum                            = 2,
 ywavenum                            = 2,
 /

 &dynamics
 rk_ord                              = 3,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,
 diff_6th_factor                     = 0.12,
 w_damping                           = 1,
 base_temp                           = 268.
 damp_opt                            = 0,
 zdamp                               = 5000.,
 dampcoef                            = 0.01,
 khdif                               = 0,
 kvdif                               = 0,
 smdiv                               = 0.1,
 emdiv                               = 0.01,
 epssm                               = 0.1,
 non_hydrostatic                     = .true.,
 h_mom_adv_order                     = 5,
 v_mom_adv_order                     = 3,
 h_sca_adv_order                     = 5,
 v_sca_adv_order                     = 3,
 time_step_sound                     = 4,
 moist_adv_opt                       = 1,
 scalar_adv_opt                      = 1,
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true.,
 nested                              = .false.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF
endif

#--------------------------------------------------------------------------
# ATM_GRID is wus12
#--------------------------------------------------------------------------

if ($ATM_GRID =~ wus12) then
set link_data = 1
if ($link_data) then
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/RRTM_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/RRTM_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/CAM_ABS_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/CAM_AEROPT_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ozone.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ozone_lat.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ozone_plev.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/GENPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/LANDUSE.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/SOILPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/urban_param.tbl .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/VEGPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr49t67 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr49t85 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr67t85 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/gribmap.txt .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/grib2map.tbl .

  ln -sf   $DIN_LOC_ROOT/atm/wrf/wus12/wrfbdy_d01_${st_year}${st_mon} wrfbdy_d01
  if (${CONTINUE_RUN} == 'FALSE') then
    ln -sf   $DIN_LOC_ROOT/atm/wrf/wus12/wrfinput_d01_${st_year}${st_mon}0100  wrfinput_d01
  endif
  if ($spectral_nudging) then
    ln -sf   $DIN_LOC_ROOT/atm/wrf/wus12/wrffdda_d01_${st_year}${st_mon}01  wrffdda_d01
  endif
else
  cp   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/RRTM_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/run/RRTM_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/CAM_ABS_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/run/CAM_AEROPT_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/run/ozone.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/run/ozone_lat.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf//run/ozone_plev.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/run/GENPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/LANDUSE.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/SOILPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/urban_param.tbl .
  cp   $DIN_LOC_ROOT/atm/wrf/run/VEGPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/tr49t67 .
  cp   $DIN_LOC_ROOT/atm/wrf/run/tr49t85 .
  cp   $DIN_LOC_ROOT/atm/wrf/run/tr67t85 .
  cp   $DIN_LOC_ROOT/atm/wrf/run/gribmap.txt .
  cp   $DIN_LOC_ROOT/atm/wrf/run/grib2map.tbl .

  cp   $DIN_LOC_ROOT/atm/wrf/wus12/wrfbdy_d01_${st_year}${st_mon} wrfbdy_d01
  if (${CONTINUE_RUN} == 'FALSE') then
    cp   $DIN_LOC_ROOT/atm/wrf/wus12/wrfinput_d01_${st_year}${st_mon}0100  wrfinput_d01
  endif
  if ($spectral_nudging) then
    cp   $DIN_LOC_ROOT/atm/wrf/wus12/wrffdda_d01_${st_year}${st_mon}01  wrffdda_d01
  endif
  chmod u+w *
endif


cat >! namelist.input <<EOF
 &time_control
 run_days                            = ${run_days},
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = ${st_year}
 start_month                         = ${st_mon}
 start_day                           = ${st_day}
 start_hour                          = ${st_hr}
 start_minute                        = ${st_min}
 start_second                        = ${st_sec}
 end_year                            = 2003, 
 end_month                           = 11,   
 end_day                             = 01,   
 end_hour                            = 00,  
 end_minute                          = 00,  
 end_second                          = 00,   
 interval_seconds                    = 10800
 input_from_file                     = .true.,
 history_interval                    = 360,  
 frames_per_outfile                  = 1,   
 restart                             = ${rest_flag}
 restart_interval                    = 999999999
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 /

 &domains
 time_step                           = 60,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,    
 e_we                                = 341,  
 s_sn                                = 1,  
 e_sn                                = 337,  
 s_vert                              = 1,   
 e_vert                              = 45,  
 num_metgrid_levels                  = 30
 dx                                  = 12000, 
 dy                                  = 12000, 
 grid_id                             = 1,    
 parent_id                           = 0,    
 i_parent_start                      = 1,     
 j_parent_start                      = 1,  
 parent_grid_ratio                   = 1,  
 parent_time_step_ratio              = 1,    
 feedback                            = 1,
 smooth_option                       = 0
 /

 &physics
 mp_physics                          = 6,   
 ra_lw_physics                       = 3,   
 ra_sw_physics                       = 3, 
 radt                                = 15,    
 sf_sfclay_physics                   = 1,    
 sf_surface_physics                  = 2,    
 bl_pbl_physics                      = 1,   
 bldt                                = 0,    
 cu_physics                          = 1,   
 cudt                                = 15,     
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 sf_urban_physics                    = 0,
 mp_zero_out                         = 2,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 cam_abs_freq_s                      = 21600,
 cam_abs_dim2                        = 45,
 cam_abs_dim1                        = 4,
 paerlev                              = 29,
 levsiz                               = 59
 /

 &fdda
 /

 &dynamics
 w_damping                           = 0,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,
 diff_6th_factor                     = 0.12,
 base_temp                           = 290.
 damp_opt                            = 0,
 zdamp                               = 5000., 
 dampcoef                            = 0.2, 
 khdif                               = 0,    
 kvdif                               = 0,   
 non_hydrostatic                     = .true., 
 moist_adv_opt                       = 1, 
 scalar_adv_opt                      = 1, 
 use_baseparam_fr_nml                = .true.
 /

 &bdy_control
 spec_bdy_width                      = 10,
 spec_zone                           = 1,
 relax_zone                          = 9,
 specified                           = .true., 
 nested                              = .false., 
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF
endif

#--------------------------------------------------------------------------
# ATM_GRID is txlw9k
#--------------------------------------------------------------------------

if ($ATM_GRID =~ txlw9k) then
    set link_data = 1
    if ($link_data) then
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA_DBL .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/RRTMG_LW_DATA .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/RRTMG_LW_DATA_DBL .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/RRTMG_SW_DATA .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/RRTMG_SW_DATA_DBL .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/CAM_ABS_DATA .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/CAM_AEROPT_DATA .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ozone.formatted .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ozone_lat.formatted .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ozone_plev.formatted .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/GENPARM.TBL .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/LANDUSE.TBL .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/SOILPARM.TBL .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/urban_param.tbl .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/VEGPARM.TBL .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr49t67 .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr49t85 .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr67t85 .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/gribmap.txt .
    ln -sf   $DIN_LOC_ROOT/atm/wrf/run/grib2map.tbl .

    ln -sf   $DIN_LOC_ROOT/atm/wrf/txlw9k/wrfbdy_d01_${st_year}${st_mon}0100 wrfbdy_d01
#--    if (${CONTINUE_RUN} == 'FALSE') then
    ln -sf  $DIN_LOC_ROOT/atm/wrf/txlw9k/wrfinput_d01_${st_year}${st_mon}0100  wrfinput_d01
#--    endif
    else
    cp   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA .
    cp   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA_DBL .
    cp   $DIN_LOC_ROOT/atm/wrf/run/RRTMG_LW_DATA .
    cp   $DIN_LOC_ROOT/atm/wrf/run/RRTMG_LW_DATA_DBL .
    cp   $DIN_LOC_ROOT/atm/wrf/run/RRTMG_SW_DATA .
    cp   $DIN_LOC_ROOT/atm/wrf/run/RRTMG_SW_DATA_DBL .
    cp   $DIN_LOC_ROOT/atm/wrf/run/CAM_ABS_DATA .
    cp   $DIN_LOC_ROOT/atm/wrf/run/CAM_AEROPT_DATA .
    cp   $DIN_LOC_ROOT/atm/wrf/run/ozone.formatted .
    cp   $DIN_LOC_ROOT/atm/wrf/run/ozone_lat.formatted .
    cp   $DIN_LOC_ROOT/atm/wrf//run/ozone_plev.formatted .
    cp   $DIN_LOC_ROOT/atm/wrf/run/GENPARM.TBL .
    cp   $DIN_LOC_ROOT/atm/wrf/run/LANDUSE.TBL .
    cp   $DIN_LOC_ROOT/atm/wrf/run/SOILPARM.TBL .
    cp   $DIN_LOC_ROOT/atm/wrf/run/urban_param.tbl .
    cp   $DIN_LOC_ROOT/atm/wrf/run/VEGPARM.TBL .
    cp   $DIN_LOC_ROOT/atm/wrf/run/tr49t67 .
    cp   $DIN_LOC_ROOT/atm/wrf/run/tr49t85 .
    cp   $DIN_LOC_ROOT/atm/wrf/run/tr67t85 .
    cp   $DIN_LOC_ROOT/atm/wrf/run/gribmap.txt .
    cp   $DIN_LOC_ROOT/atm/wrf/run/grib2map.tbl .

    cp   $DIN_LOC_ROOT/atm/wrf/txlw9k/wrfbdy_d01_${st_year}${st_mon}0100 wrfbdy_d01
    if (${CONTINUE_RUN} == 'FALSE') then
	cp   $DIN_LOC_ROOT/atm/wrf/txlw9k/wrfinput_d01_${st_year}${st_mon}0100  wrfinput_d01
    endif
    if ($spectral_nudging) then
	cp   $DIN_LOC_ROOT/atm/wrf/wus12/wrffdda_d01_${st_year}${st_mon}01  wrffdda_d01
    endif
    chmod u+w *
    endif

    cat >! namelist.input <<EOF
    &time_control
    run_days                            = ${run_days},
    run_hours                           = 0,
    run_minutes                         = 0,
    run_seconds                         = 0,
    start_year                          = ${st_year},
    start_month                         = ${st_mon},
    start_day                           = ${st_day},
    start_hour                          = ${st_hr},
    start_minute                        = ${st_min},
    start_second                        = ${st_sec},
    end_year                            = 2010, 
    end_month                           = 01,   
    end_day                             = 04,   
    end_hour                            = 00,  
    end_minute                          = 00,  
    end_second                          = 00,   
    interval_seconds                    = 21600,
    input_from_file                     = .true.,
    history_interval                    = 180,  
    frames_per_outfile                  = 8,   
    restart                             = ${rest_flag},
    restart_interval_d                  = 1,
    io_form_history                     = 2,
    io_form_restart                     = 2,
    io_form_input                       = 2,
    io_form_boundary                    = 2,
    debug_level                         = 0,
    auxinput1_inname                    = "met_em.d<domain>.<date>"
    auxinput4_inname                    = "wrflowinp_d<domain>"
    auxinput4_interval                  = 360 
    io_form_auxinput4                   = 2
    nocolons                            = .true.
    output_diagnostics                  = 0
    /

    &domains
    time_step                           = 30,
    time_step_fract_num                 = 0,
    time_step_fract_den                 = 1,
    max_dom                             = 1,
    e_we                                = 286,
    e_sn                                = 271,
    e_vert                              = 35,
    p_top_requested                     = 5000,
    num_metgrid_levels                  = 38,
    num_metgrid_soil_levels             = 4,
    dx                                  = 9000, 
    dy                                  = 9000, 
    grid_id                             = 1,    
    parent_id                           = 0,    
    i_parent_start                      = 1,     
    j_parent_start                      = 1,  
    parent_grid_ratio                   = 1,  
    parent_time_step_ratio              = 1,    
    feedback                            = 1,
    smooth_option                       = 0,
    interp_type                         = 1,
    lowest_lev_from_sfc                 = .false.,
    lagrange_order                      = 1,
    force_sfc_in_vinterp                = 1,
    zap_close_levels                    = 500,
    sfcp_to_sfcp                        = .false.,
    adjust_heights                      = .false.,
    eta_levels                          = 1.000, 0.993, 0.983, 0.970, 0.954,
					0.934, 0.909, 0.880, 0.845, 0.807,
					0.765, 0.719, 0.672, 0.622, 0.571,
					0.520, 0.468, 0.420, 0.376, 0.335,
					0.298, 0.263, 0.231, 0.202, 0.175,
					0.150, 0.127, 0.106, 0.088, 0.070,
					0.055, 0.040, 0.026, 0.013, 0.000
    /

    &physics
    mp_physics                          = 2,   
    ra_lw_physics                       = 4,   
    ra_sw_physics                       = 4, 
    radt                                = 3,  
    ra_call_offset                      = 0
    cam_abs_freq_s                      = 21600,
    levsiz                              = 59,
    paerlev                             = 29,
    cam_abs_dim1                        = 4,
    cam_abs_dim2                        = 35,
    sf_sfclay_physics                   = 1,
    sf_surface_physics                  = 2,
    bl_pbl_physics                      = 1,   
    bldt                                = 0,    
    cu_physics                          = 0,   
    cudt                                = 5,     
    isfflx                              = 1,
    ifsnow                              = 0,
    icloud                              = 1,
    surface_input_source                = 1,
    num_soil_layers                     = 4,
    sf_urban_physics                    = 0,
    maxiens                             = 1,
    maxens                              = 3,
    maxens2                             = 3,
    maxens3                             = 16,
    ensdim                              = 144,
    sst_update                          = 0,
    usemonalb                           = .true.
    fractional_seaice                   = 0
    /

    &fdda
    /

    &dynamics
    w_damping                           = 0,
    diff_opt                            = 1,
    km_opt                              = 4,
    diff_6th_opt                        = 0,  
    diff_6th_factor                     = 0.12, 
    damp_opt                            = 0,
    zdamp                               = 5000., 
    dampcoef                            = 0.2,  
    khdif                               = 0,   
    kvdif                               = 0,   
    non_hydrostatic                     = .true.,
    moist_adv_opt                       = 1,
    scalar_adv_opt                      = 1,
    iso_temp                            = 0
    /

    &bdy_control
    spec_bdy_width                      = 5,
    spec_zone                           = 1,
    relax_zone                          = 4,
    specified                           = .true., 
    nested                              = .false., 
    /

    &grib2
    /

    &namelist_quilt
    nio_tasks_per_group = 0,
    nio_groups = 1,
    /
    
EOF
endif

#--------------------------------------------------------------------------
# ATM_GRID is us20
#--------------------------------------------------------------------------

if ($ATM_GRID =~ us20) then
set link_data = 1
if ($link_data) then
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/RRTM_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/RRTM_DATA_DBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/CAM_ABS_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/CAM_AEROPT_DATA .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ozone.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/ozone_lat.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf//run/ozone_plev.formatted .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/GENPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/LANDUSE.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/SOILPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/urban_param.tbl .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/VEGPARM.TBL .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr49t67 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr49t85 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/tr67t85 .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/gribmap.txt .
  ln -sf   $DIN_LOC_ROOT/atm/wrf/run/grib2map.tbl .

  ln -sf   $DIN_LOC_ROOT/atm/wrf/CCSMtoWRF.us20km.wrfinput.2005-2100/wrfbdy_d01_${st_year}${st_mon} wrfbdy_d01
  if (${CONTINUE_RUN} == 'FALSE') then
    ln -sf   $DIN_LOC_ROOT/atm/wrf/CCSMtoWRF.us20km.wrfinput.2005-2100/wrfinput_d01_${st_year}${st_mon}  wrfinput_d01
  endif
  if ($spectral_nudging) then
    ln -sf   $DIN_LOC_ROOT/atm/wrf/CCSMtoWRF.us20km.wrfinput.2005-2100/wrffdda_d01_${st_year}${st_mon}  wrffdda_d01
  endif
else
  cp   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/run/ETAMPNEW_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/RRTM_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/run/RRTM_DATA_DBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/CAM_ABS_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/run/CAM_AEROPT_DATA .
  cp   $DIN_LOC_ROOT/atm/wrf/run/ozone.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/run/ozone_lat.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf//run/ozone_plev.formatted .
  cp   $DIN_LOC_ROOT/atm/wrf/run/GENPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/LANDUSE.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/SOILPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/urban_param.tbl .
  cp   $DIN_LOC_ROOT/atm/wrf/run/VEGPARM.TBL .
  cp   $DIN_LOC_ROOT/atm/wrf/run/tr49t67 .
  cp   $DIN_LOC_ROOT/atm/wrf/run/tr49t85 .
  cp   $DIN_LOC_ROOT/atm/wrf/run/tr67t85 .
  cp   $DIN_LOC_ROOT/atm/wrf/run/gribmap.txt .
  cp   $DIN_LOC_ROOT/atm/wrf/run/grib2map.tbl .

  cp   $DIN_LOC_ROOT/atm/wrf/CCSMtoWRF.us20km.wrfinput.2005-2100/wrfbdy_d01_${st_year}${st_mon} wrfbdy_d01
  if (${CONTINUE_RUN} == 'FALSE') then
    cp   $DIN_LOC_ROOT/atm/wrf/CCSMtoWRF.us20km.wrfinput.2005-2100/wrfinput_d01_${st_year}${st_mon}  wrfinput_d01
  endif
  if ($spectral_nudging) then
    cp   $DIN_LOC_ROOT/atm/wrf/CCSMtoWRF.us20km.wrfinput.2005-2100/wrffdda_d01_${st_year}${st_mon}  wrffdda_d01
  endif
  chmod u+w *
endif


cat >! namelist.input <<EOF
 &time_control
 run_days                            = ${run_days},
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = ${st_year}, 2001, 2001
 start_month                         = ${st_mon}, 06, 06
 start_day                           = ${st_day}, 11, 11
 start_hour                          = 00,   12,   12,
 start_minute                        = 00,   00,   00,
 start_second                        = 00,   00,   00,
 end_year                            = 2100, 2001, 2001,
 end_month                           = 11,   06,   06,
 end_day                             = 01,   12,   12,
 end_hour                            = 00,   12,   12,
 end_minute                          = 00,   00,   00,
 end_second                          = 00,   00,   00,
 interval_seconds                    = 21600
 input_from_file                     = .true.,.true.,.true.,
 history_interval                    = 360,   60,   60,
 frames_per_outfile                  = 1,    1,    1,
 auxhist2_interval                   = 60,   60,  60,
 frames_per_auxhist2                 = 24,   24,  24,
 io_form_auxhist2                    = 2
 restart                             = ${rest_flag}
 restart_interval                    = 86400
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 /

 &domains
 time_step                           = 30,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,     1,     1,
 e_we                                = 351,   112,   94,
 s_sn                                = 1,     1,     1,
 e_sn                                = 231,   97,    91,
 s_vert                              = 1,     1,     1,
 e_vert                              = 35,    35,    35,
 num_metgrid_levels                  = 18
 dx                                  = 20000,  1333.33, 444.44,
 dy                                  = 20000,  1333.33, 444.44,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 1,     31,    30,
 j_parent_start                      = 1,     17,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
 /

 &physics
 mp_physics                          = 4,    6,     6,
 ra_lw_physics                       = 3,     1,     1,
 ra_sw_physics                       = 3,     1,     1,
 radt                                = 15,    10,    10,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 2,     2,     2,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 3,     0,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 sf_urban_physics                    = 0,
 mp_zero_out                         = 2,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 cam_abs_freq_s                      = 21600,
 cam_abs_dim2                        = 35,
 cam_abs_dim1                        = 4,
 paerlev                             = 29,
 levsiz                              = 59
 /

 &fdda
 grid_fdda                           = 2
 xwavenum                            = 3
 ywavenum                            = 3
 gfdda_inname                        = "wrffdda_d<domain>",
 gfdda_interval_m                    = 360,   360,   360,
 gfdda_end_h                         = 7200000,    24,    24,
 if_no_pbl_nudging_uv                = 1,     0,     1,
 if_no_pbl_nudging_t                 = 1,     0,     1,
 if_no_pbl_nudging_q                 = 1
 guv                                 = 0.0003,     0.0003,     0.0003,
 gt                                  = 0.000000,     0.0003,     0.0003,
 gq                                  = 0.000000,     0.0003,     0.0003,
 if_ramping                          = 1,
 dtramp_min                          = 60.0,
 io_form_gfdda                       = 2,
 /

 &dynamics
 w_damping                           = 0,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,
 diff_6th_factor                     = 0.12,
 base_temp                           = 290.
 damp_opt                            = 0,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.2,    0.2,    0.2
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 non_hydrostatic                     = .true., .true., .true.,
 moist_adv_opt                       = 1,      1,      1,
 scalar_adv_opt                      = 1,      1,      1,
 use_baseparam_fr_nml                = .true.
 /

 &bdy_control
 spec_bdy_width                      = 15,
 spec_zone                           = 1,
 relax_zone                          = 14,
 specified                           = .true., .false.,.false.,
 nested                              = .false., .true., .true.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF
endif

