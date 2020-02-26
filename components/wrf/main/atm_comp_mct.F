!BOP
!   !MODULE: atm_comp_mct
!   !INTERFACE:

module atm_comp_mct

!   !DESCRIPTION:
!       Main driver module for the Weather Research and Forecasting (WRF) Model
!
!   !REVISION HISTORY:
!       Mar 06, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

    use mct_mod
    use esmf

    use shr_kind_mod,   only : r8 => SHR_KIND_R8, &
                               cl => SHR_KIND_CL, &
                               cs => SHR_KIND_CS
    use shr_const_mod,  only : g => SHR_CONST_G, &
                               shr_const_stebol, &
                               shr_const_rdair,  &
                               shr_const_zvir
    use shr_sys_mod,    only : shr_sys_abort

    use seq_cdata_mod
    use seq_comm_mct,   only : seq_comm_iamroot
    use seq_flds_mod
    use seq_infodata_mod
    use seq_timemgr_mod

    use perf_mod,       only : t_startf, &
                               t_stopf

    use atm_instance
    use atm_cpl_indices
    use atm_radiation_mod
    use atm_grid_mod
    use atm_io_tools
    use atm_files_mod

    use module_wrf_top, only : wrf_init, &
                               wrf_run,  &
                               wrf_finalize
    use module_domain,  only : domain,               &
                               head_grid,            &
                               get_ijk_from_grid,    &
                               domain_get_time_step, &
                               domain_get_current_time
    use module_dm
    use module_configure
    use module_integrate
    use module_utility
    use module_radiation_driver,  only : eccen,  &
                                         lambm0, &
                                         mvelpp, &
                                         obliqr
    use module_streams,           only : RESTART_ALARM
    use module_model_constants,   only : t0,      &
                                         p1000mb, &
                                         rcp,     &
                                         r_d
    use module_state_description, only : P_QV,           &
                                         CAMLWSCHEME,    &
                                         CAMUWPBLSCHEME, &
                                         SFCLAYSCHEME

    implicit none

    private

!   --- Public interfaces

    public :: atm_init_mct, &
              atm_run_mct,  &
              atm_final_mct

!   --- Private data

    type (ESMF_Clock),        pointer :: atm_EClock
    type (seq_infodata_type), pointer :: infodata

    type (domain),            pointer :: atm_grid
    type (grid_config_rec_type)       :: config_flags

    logical :: iamroot

    integer :: atm_cpl_dt

    integer :: atm_start_type

    integer, parameter :: ATM_DO_INIT    = 1, &
                          ATM_DO_RESTART = 2, &
                          ATM_DO_BRANCH  = 3


!   Time average of flux fields:
!   . counter
    integer :: avg_count
!   . instantaneous and total field vectors
    type (mct_aVect) :: a2x_a_SNP, a2x_a_SUM

contains

    subroutine atm_init_mct( EClock, cdata_a, x2a_a, a2x_a, NLFilename )

        type (ESMF_Clock),             intent(inout) :: EClock
        type (seq_cdata),              intent(inout) :: cdata_a
        type (mct_aVect),              intent(inout) :: x2a_a
        type (mct_aVect),              intent(inout) :: a2x_a
        character (len = *), optional, intent(in)    :: NLFilename

        ! --- Local variables ---

        integer  :: ATMID,       &
                    mpicom_a,    &
                    lsize,       &
                    nx, ny

        integer  :: id, max_dom
        integer  :: lbnum, rc
        integer  :: phase
        integer  :: nl_integer
        integer  :: atm_start_ymd, &
                    atm_start_tod, &
                    atm_stop_ymd,  &
                    atm_stop_tod,  &
                        start_ymd, &
                        start_tod, &
                        stop_ymd,  &
                        stop_tod

        integer  :: atm_int_dt, &
                    time_step_fract_num, &
                    time_step_fract_den

        logical  :: lnd_present, &
                    ocn_present

        logical  :: nl_logical

        !real(r8) :: eccen, mvelpp, lambm0, obliqr
        real(r8) :: nextsw_cday

        type (mct_gsMap), pointer :: gsMap_a
        type (mct_gGrid), pointer :: dom_a
        type (ESMF_TimeInterval)  :: time_step

        character(cs) :: caseid
        character(cl) :: caltype
        character(cl) :: starttype

        character (len = *), parameter :: subname = 'atm_init_mct'

        ! --- Begin

        ! --- Set pointers to cdata for grid and decomp
        call seq_cdata_setptrs(cdata_a,       ID=ATMID , mpicom=mpicom_a, &
                               gsMap=gsMap_a, dom=dom_a, infodata=infodata)

#if (defined _MEMTRACE)
        iamroot = seq_comm_iamroot(ATMID)
        if ( iamroot ) then
           lbnum = 1
           call memmon_dump_fort('memmon.out',trim(subname) // ':start::',lbnum)
        endif
#endif

        ! --- Get initialization phase ---
        call seq_infodata_GetData(infodata, atm_phase=phase)

        select case ( phase )
        case ( 1 )  ! --- Init: Phase 1

            nullify( atm_grid )

            ! --- Initialize WRF instance
            call atm_instance_init( ATMID )

            ! --- Initialize and set log unit for shared output
            call atm_log_init()

            ! --- Set WRF MPI communicator
            call wrf_set_dm_communicator( mpicom_a )

            ! --- Set attribute vector indices ---
            call atm_cpl_indices_set()

            ! --- Performs consistency check on CO2 flux (translate from CAM to WRF)
            !        if (co2_readFlux_ocn .and. index_x2a_Faoo_fco2_ocn /= 0) then
            !           call atm_log(subname, 'error co2_readFlux_ocn and index_x2a_Faoo_fco2_ocn cannot both be active')
            !           call shr_sys_abort()
            !        end if

            ! --- Get infodata ---
            ! --- Use orbital parameters and solar constants from coupler, if possible
            call seq_infodata_GetData( infodata,                                           &
                                       case_name=caseid, start_type=starttype,                                   &
                                       orb_eccen=eccen, orb_mvelpp=mvelpp, orb_lambm0=lambm0, orb_obliqr=obliqr, &
                                       lnd_present=lnd_present, ocn_present=ocn_present )

            !
            ! --- Get startup type from infodata ---
            !
            if (     trim(starttype) == trim(seq_infodata_start_type_start)) then
                atm_start_type = ATM_DO_INIT
            else if (trim(starttype) == trim(seq_infodata_start_type_cont) ) then
                atm_start_type = ATM_DO_RESTART
            else if (trim(starttype) == trim(seq_infodata_start_type_brnch)) then
                atm_start_type = ATM_DO_BRANCH
            else
                call atm_log(subname, 'ERROR: unknown starttype')
                call shr_sys_abort()
            end if

            ! --- Initialize WRF ---
            ! --- Start timer
            call t_startf('wrf_init')
            ! --- We initialize phase 1 and 2 separately to set the calendar type
            ! --- consistently with ESMF
            ! --- Init WRF: step 1
            call init_modules( 1 )

            call seq_timemgr_EClockGetData(EClock, calendar=caltype)

            ! --- Initialize WRF clock using correct calendar type ---
            if (     trim(caltype) == trim(seq_timemgr_noleap   )) then
                call WRFU_Initialize( defaultCalendar=WRFU_CALKIND_NOLEAP )
            else if (trim(caltype) == trim(seq_timemgr_gregorian)) then
                call WRFU_Initialize( defaultCalendar=WRFU_CALKIND_GREGORIAN )
            else
                call atm_log(subname, 'ERROR: unknown caltype: ' // trim(caltype))
                call shr_sys_abort()
            end if

            ! --- Init WRF: step 2
            call wrf_init( no_init1=.true. )

            ! --- Check if WRF config is compatible with coupled simulations ---
            ! --- NOTE: Only top domain is coupled at the moment
            atm_grid => head_grid

            ! --- Check if sw radiation is enabled and compatible ---
            call nl_get_ra_sw_physics ( atm_grid % id , nl_integer )
            if ( nl_integer == 0 ) then
                call atm_log(subname, 'ERROR: WRF shortwave radiation physics (ra_sw_physics) must be enabled')
                call shr_sys_abort()
#ifndef CCSM_USE_ATM_SW_NET
            else if ((nl_integer /= CAMSWSCHEME) .and. (nl_integer /= RRTMG_SWSCHEME)) then
                call atm_log(subname, 'ERROR: WRF shortwave radiation schemes (ra_sw_physics) must be either CAM or RRTMG')
                call shr_sys_abort()
#endif
            end if
            ! --- Check if lw radiation is enabled and compatible ---
#if (defined (CCSM_USE_WRF_LM) && defined (CCSM_USE_ATM_AOFLUX))
            ! --- Using WRF's surface schemes for land and ocean, therefore lw scheme is unrestricted
#else
            call nl_get_ra_lw_physics ( atm_grid % id , nl_integer )
            if ((nl_integer /= CAMLWSCHEME) .and. (nl_integer /= RRTMG_LWSCHEME)) then
                call atm_log(subname, 'ERROR: WRF longwave radiation schemes (ra_lw_physics) must be either CAM or RRTMG')
                call shr_sys_abort()
            end if
#endif

            ! --- Get WRF clock ---

            atm_EClock => atm_grid % domain_clock

            ! --- Check that cpl and WRF clocks have same start/stop time
            if ( atm_start_type == ATM_DO_INIT ) then
                call seq_timemgr_EClockGetData(EClock, &
                                               start_ymd=start_ymd, start_tod=start_tod, &
                                               stop_ymd=stop_ymd,   stop_tod=stop_tod)
            else
                call seq_timemgr_EClockGetData(EClock, &
                                               curr_ymd=start_ymd,  curr_tod=start_tod, &
                                               stop_ymd=stop_ymd,   stop_tod=stop_tod)
            end if
            call seq_timemgr_EClockGetData(atm_EClock, &
                                           start_ymd=atm_start_ymd, start_tod=atm_start_tod, &
                                           stop_ymd=atm_stop_ymd,   stop_tod=atm_stop_tod)

            if (start_ymd /= atm_start_ymd) then
                call atm_log(subname, 'ERROR: WRF domain start date different from coupler')
                write( atm_log_unit, * ) 'drv: start ymd = ',start_ymd
                write( atm_log_unit, * ) 'atm: start ymd = ',atm_start_ymd
                call shr_sys_abort()
            end if
            if (start_tod /= atm_start_tod) then
                call atm_log(subname, 'ERROR: WRF domain start time different from coupler')
                write( atm_log_unit, * ) 'drv: start tod = ',start_tod
                write( atm_log_unit, * ) 'atm: start tod = ',atm_start_tod
                call shr_sys_abort()
            end if
            if (stop_ymd /= atm_stop_ymd) then
                call atm_log(subname, 'ERROR: WRF domain stop date different from coupler')
                write( atm_log_unit, * ) 'drv: stop ymd = ',stop_ymd
                write( atm_log_unit, * ) 'atm: stop ymd = ',atm_stop_ymd
                call shr_sys_abort()
            end if
            if (stop_tod /= atm_stop_tod) then
                call atm_log(subname, 'ERROR: WRF domain stop time different from coupler')
                write( atm_log_unit, * ) 'drv: stop tod = ',stop_tod
                write( atm_log_unit, * ) 'atm: stop tod = ',atm_stop_tod
                call shr_sys_abort()
            end if

            ! --- Make sure that coupling and integration time steps are compatible ---
            ! --- Stop if adaptive time step is used in WRF
            call nl_get_use_adaptive_time_step ( atm_grid % id, nl_logical )
            if ( nl_logical ) then
                call atm_log(subname, 'ERROR: WRF adaptive time step incompatible with coupled mode')
                call shr_sys_abort()
            end if

            ! --- Stop if fractional time step is used in WRF
            time_step = domain_get_time_step( atm_grid )
            call ESMF_TimeIntervalGet(time_step, s=atm_int_dt, Sn=time_step_fract_num, Sd=time_step_fract_den, rc=rc)
            if ( rc /= ESMF_SUCCESS ) then
                call atm_log(subname, trim(subname) // ': error retrieving WRF fractional time step')
                call shr_sys_abort()
            end if
            if (time_step_fract_num > 0) then
                call atm_log(subname, 'ERROR: WRF fractional time step incompatible with coupled mode')
                call shr_sys_abort()
            end if

            ! --- Check if coupling time step is multiple of WRF time step
            call seq_timemgr_EClockGetData(Eclock, dtime=atm_cpl_dt)
            if (atm_cpl_dt < atm_int_dt) then
                call atm_log(subname, 'ERROR: coupling time step must be >= WRF integration time step')
                call shr_sys_abort()
            else if (mod(atm_cpl_dt, atm_int_dt) /= 0) then
                call atm_log(subname, 'ERROR: coupling time step must be multiple of WRF integration time step')
                call shr_sys_abort()
            end if

            ! --- Checks surface and PBL schemes, and makes sure they are set for coupling mode
!#ifndef CCSM_USE_WRF_LM
#if (!defined(CCSM_USE_WRF_LM) && !defined(CCSM_USE_ATM_LOWBC) && !defined(CCSM_USE_WRF_SEAICE))
            call nl_get_io_form_auxinput4 ( atm_grid % id, nl_integer )
            if ( nl_integer /= 0 ) then
                call atm_log(subname, 'WARNING: io_form_auxinput4 set to 0 in this coupled mode.')
                call nl_set_io_form_auxinput4 ( atm_grid % id, 0 )
                call nl_get_max_dom ( atm_grid % id, max_dom )
                do id = 1, max_dom
                    call nl_set_auxinput4_interval   ( id, 0 )
                    call nl_set_auxinput4_interval_y ( id, 0 )
                    call nl_set_auxinput4_interval_d ( id, 0 )
                    call nl_set_auxinput4_interval_h ( id, 0 )
                    call nl_set_auxinput4_interval_m ( id, 0 )
                    call nl_set_auxinput4_interval_s ( id, 0 )
                end do
            end if
            call nl_get_sst_update ( atm_grid % id, nl_integer )
            if ( nl_integer /= 0 ) then
                call atm_log(subname, 'WARNING: sst_update not allowed in this coupled mode. Option disabled')
                call nl_set_sst_update ( atm_grid % id, 0 )
            end if
#endif
            call nl_get_sst_skin ( atm_grid % id, nl_integer )
            if ( nl_integer /= 0 ) then
                call atm_log(subname, 'WARNING: sst_skin not allowed in coupled mode. Option disabled')
                call nl_set_sst_skin ( atm_grid % id, 0 )
            end if
            call nl_get_scm_force_flux ( atm_grid % id, nl_integer )
            if ( nl_integer /= 0 ) then
                call atm_log(subname, 'ERROR: scm_force_flux not allowed in coupled mode')
                call shr_sys_abort()
            end if
            call nl_get_scm_force_skintemp ( atm_grid % id, nl_integer )
            if ( nl_integer /= 0 ) then
                call atm_log(subname, 'ERROR: scm_force_skintemp not allowed in coupled mode')
                call shr_sys_abort()
            end if
            call nl_get_fractional_seaice ( atm_grid % id, nl_integer )
            if ( nl_integer /= 0 ) then
                call atm_log(subname, 'ERROR: fractional_seaice not implemented in coupled mode')
                call shr_sys_abort()
            end if
#ifdef CCSM_USE_WRF_LM
            ! Check if surface scheme is compatible when one of WRF's built-in land models is used
            call nl_get_sf_sfclay_physics ( atm_grid % id, nl_integer )
            select case ( nl_integer )
                case (SFCLAYSCHEME, SFCLAYREVSCHEME, PXSFCSCHEME)
                    ! Ok - surface scheme works with WRF's land model when coupled
                case default
                    call atm_log(subname, 'ERROR: surface scheme not implemented in coupled mode')
                    call shr_sys_abort()
            end select
#endif

            ! --- Set history and restart file names and types ---
            call atm_history_outfile_set( atm_grid % id, caseid )
            call atm_restart_outfile_set( atm_grid % id, caseid )

            ! --- Disable all restart alarms, as CPL7 will control restart ---
            call nl_set_restart_interval   ( atm_grid % id, 0 )
            call nl_set_restart_interval_d ( atm_grid % id, 0 )
            call nl_set_restart_interval_h ( atm_grid % id, 0 )
            call nl_set_restart_interval_m ( atm_grid % id, 0 )
            call nl_set_restart_interval_s ( atm_grid % id, 0 )
            call nl_set_override_restart_timers ( atm_grid % id, .true. )

            if ( atm_grid % alarms_created( RESTART_ALARM ) ) then
                call WRFU_AlarmDisable( atm_grid % alarms( RESTART_ALARM ) )
            end if

            ! --- Set restart flag according to coupler ---
            ! --- Allow for WRF restart even in 'startup' cases
            if ( atm_start_type /= ATM_DO_INIT ) then
                call nl_set_restart( atm_grid % id, .true. )
            end if

            ! --- Read initial data
            call atm_init_read( atm_grid )

            ! --- Stop timer
            call t_stopf('wrf_init')

            ! --- Initialize MCT attribute vectors ---

            ! --- Start timer ---
            call t_startf('wrf_mct_init')

            ! --- Set decomposition ---
            call atm_setgsMap_mct( mpicom_a, ATMID, atm_grid, gsMap_a, lx=nx, ly=ny )

            ! --- Get local number of grid cells ---
            lsize = mct_gsMap_lsize( gsMap_a, mpicom_a )

            ! --- Set grid
            call atm_domain_mct( atm_grid, lsize, gsMap_a, dom_a )

            ! --- Initialize import/export vectors
            call mct_aVect_init(a2x_a, rList=seq_flds_a2x_fields, lsize=lsize)
            call mct_aVect_zero(a2x_a)

            call mct_aVect_init(x2a_a, rList=seq_flds_x2a_fields, lsize=lsize)
            call mct_aVect_zero(x2a_a)

            ! --- Initialize counter and vectors for field averages
            avg_count = 0

            call mct_aVect_init(a2x_a_SNP, rList=a2x_avg_flds, lsize=lsize)
            call mct_aVect_zero(a2x_a_SNP)

            call mct_aVect_init(a2x_a_SUM, rList=a2x_avg_flds, lsize=lsize)
            call mct_aVect_zero(a2x_a_SUM)

            ! --- Stop timer ---
            call t_stopf('wrf_mct_init')

            ! --- Export initial data to coupler ---
            call atm_export_mct( atm_grid, a2x_a )

            ! --- Set prognostic info ---
            call seq_infodata_PutData(infodata, atm_present=.true.)
            call seq_infodata_PutData(infodata, atm_prognostic=.true.)
            call seq_infodata_PutData(infodata, atm_nx=nx, atm_ny=ny)
            ! CLM requires the atmosphere model to send aerosols,
            ! therefore we hardcoded this to .true.
            call seq_infodata_PutData(infodata, atm_aero=.true.)

            ! --- Set nextsw_calday to start calday on init ---
            if (atm_start_type == ATM_DO_INIT) then
                nextsw_cday = atm_sw_calday_get( atm_grid, now = .true. )
                call seq_infodata_PutData( infodata, nextsw_cday=nextsw_cday )
            end if


        case ( 2 )  ! --- Init: Phase 2

            if (atm_start_type /= ATM_DO_INIT) then
                ! --- Read restart file name from pointer file
                call atm_restart_infile_set( atm_grid % id )
!               ! --- Read coupling fields from surface restart file
                ! Not implemented -- not sure if needed, since the driver will overwrite exported fields
            end if

            ! --- Import state from coupler
            call atm_import_mct( atm_grid, x2a_a )

            ! --- Ocean/Atmosphere fluxes are not computed yet.
            ! --- If CAM lw radiation scheme is used, need to provide lwups
            call nl_get_ra_lw_physics ( atm_grid % id, nl_integer )
            if ( nl_integer == CAMLWSCHEME ) call atm_lw_get( atm_grid )

            ! --- Run radiation based on input albedo from model components ---
            atm_grid % start_subtime = domain_get_current_time( atm_grid )
            atm_grid %  stop_subtime = atm_grid % start_subtime + domain_get_time_step( atm_grid )

            call integrate( atm_grid, phase = -1 )

            ! --- Export state to coupler
            call atm_export_mct( atm_grid, a2x_a )

            ! --- Set nextsw_cday
            nextsw_cday = atm_sw_calday_get( atm_grid, nextTimeInterval = atm_cpl_dt )
            call seq_infodata_PutData( infodata, nextsw_cday=nextsw_cday )

        case default

            call atm_log(subname, 'Only phase 1 and 2 are available')
            call shr_sys_abort()

        end select

        ! --- Reset log unit
        call atm_log_flush()
        call atm_log_reset()

#if (defined _MEMTRACE)
        if ( iamroot ) then
           lbnum = 1
           call memmon_dump_fort('memmon.out', trim(subname) // ':end::',lbnum)
           call memmon_reset_addr()
        endif
#endif

    end subroutine atm_init_mct

    subroutine atm_run_mct( EClock, cdata_a, x2a_a, a2x_a )

    ! --- Run the atmospheric model until driver's clock current time

        type (ESMF_Clock), intent(inout) :: EClock
        type (seq_cdata),  intent(inout) :: cdata_a
        type (mct_aVect),  intent(inout) :: x2a_a
        type (mct_aVect),  intent(inout) :: a2x_a

        ! --- Local variables
        integer                  :: lbnum, ymd, tod, rc
        logical                  :: restart_now, stop_now
        real(r8)                 :: nextsw_cday
        type (ESMF_Time)         :: atm_CurrTime, drv_CurrTime
        type (ESMF_TimeInterval) :: time_step
        character(len = 80)      :: msg
        character(len = *), parameter :: subname = 'atm_run_mct'


        ! --- Begin
#if (defined _MEMTRACE)
        if ( iamroot ) then
           lbnum = 1
           call memmon_dump_fort('memmon.out',trim(subname) // ':start::',lbnum)
        endif
#endif

        ! --- Set shared output to log file
        call atm_log_set()

        ! --- Get updated orbital data from coupler ---
        call seq_infodata_GetData( infodata,                             &
                                   orb_eccen=eccen,   orb_mvelpp=mvelpp, &
                                   orb_lambm0=lambm0, orb_obliqr=obliqr )

        ! --- Get coupling end time ---
        call seq_timemgr_EClockGetData( EClock, ECurrTime = drv_CurrTime )

        ! --- Get atmosphere start time ---
        atm_CurrTime = domain_get_current_time( atm_grid )
        time_step = domain_get_time_step( atm_grid )

        do while ( atm_CurrTime < drv_CurrTime )
            ! --- Prepare atmospheric model for integration over next time step ---
            atm_grid % start_subtime = atm_CurrTime
            atm_grid % stop_subtime  = atm_grid % start_subtime + time_step
            ! --- Perform I/O before integration
            call integrate( atm_grid, phase = 1 )
            ! --- Import surface fluxes from coupler
            call atm_import_mct( atm_grid, x2a_a )
            ! --- Continue integration & advance atmosphere clock
            call integrate( atm_grid, phase = 2 )
            ! --- Export surface fields to data structure for coupling
            call atm_export_mct( atm_grid, a2x_a )
#ifndef CCSM_AOFLUX_NOAVERAGE
            ! --- Accumulate fields to be averaged, skipping 1st & 2nd coupling step
            ! --- for consistency with ccsm3 when flxave is off
            ! --- Legacy code: disabled ---
!           if ( atm_grid % itimestep <= 2 ) then
!              call mct_aVect_copy( a2x_a, a2x_a_SUM )
!              avg_count = 1
!           else
               call mct_aVect_copy ( a2x_a, a2x_a_SNP )
               call mct_avect_accum( aVin = a2x_a_SNP, aVout = a2x_a_SUM )
               avg_count = avg_count + 1
!           end if
#endif
            atm_CurrTime = domain_get_current_time( atm_grid )
        end do

#ifndef CCSM_AOFLUX_NOAVERAGE
        ! --- Average fields and copy into coupling vector
        call mct_aVect_avg ( a2x_a_SUM, avg_count )
        call mct_aVect_copy( a2x_a_SUM, a2x_a     )
        call mct_aVect_zero( a2x_a_SUM )
        avg_count = 0
#endif

        ! --- Set nextsw_cday
        ! --- Get time of next radiation calculation (nextsw_cday)
        ! --- Albedos will need to be calculated by each surface model at this time

        nextsw_cday = atm_sw_calday_get( atm_grid, nextTimeInterval = atm_cpl_dt )
        call seq_infodata_PutData( infodata, nextsw_cday=nextsw_cday )

        ! --- Check if this is last coupling period and/or restart is required at end of cpl period ---
        stop_now    = seq_timemgr_StopAlarmIsOn   ( EClock )
        restart_now = seq_timemgr_RestartAlarmIsOn( EClock )

        ! --- Write merged surface data restart file if appropriate

        ! --- Write restart file if necessary
        if ( restart_now ) call atm_restart_write ( atm_grid )

        ! --- Set domain stop time if last coupling interval
        if ( stop_now ) then
            call atm_last_write( atm_grid )
            call ESMF_ClockSet( atm_grid % domain_clock, stopTime = atm_CurrTime, rc = rc )
            if ( rc /= ESMF_SUCCESS ) then
                call atm_log(subname, trim(subname) // ': error setting domain stop time')
                call shr_sys_abort()
            end if
        end if

        ! --- WRF's internal clock should now be consistent with driver's clock
        if ( atm_CurrTime /= drv_CurrTime ) then
           call seq_timemgr_EClockGetData(atm_grid % domain_clock, curr_ymd=ymd, curr_tod=tod )
           msg = ''
           write(msg, '(" WRF ymd=",i9,"  WRF tod=",i6)') ymd, tod
           call atm_log(subname, msg)
           call seq_timemgr_EClockGetData(EClock, curr_ymd=ymd, curr_tod=tod )
           msg = ''
           write(msg, '("sync ymd=",i9," sync tod=",i6)') ymd, tod
           call atm_log(subname, msg)
           call shr_sys_abort( subname//': WRF clock is not in sync with master Sync Clock' )
        end if

        ! --- Reset log unit
        call atm_log_flush()
        call atm_log_reset()

#if (defined _MEMTRACE)
        if ( iamroot ) then
           lbnum = 1
           call memmon_dump_fort('memmon.out',trim(subname) // ':end::',lbnum)
        endif
#endif

    end subroutine atm_run_mct

    subroutine atm_final_mct( EClock, cdata_a, x2a_a, a2x_a )

        type (ESMF_Clock), intent(inout) :: EClock
        type (seq_cdata),  intent(inout) :: cdata_a
        type (mct_aVect),  intent(inout) :: x2a_a
        type (mct_aVect),  intent(inout) :: a2x_a

        character(len = *), parameter :: subname = 'atm_final_mct'

        ! --- Set share output to log file
        call atm_log_set()

        ! --- Finalize WRF
        call wrf_finalize( no_shutdown = .true. )

        ! --- Clean mct vectors ---
        ! TBD

        ! --- Reset and close log unit
        call atm_log_finalize()

    end subroutine atm_final_mct

!==============================================================================
!BOP
!   !ROUTINE: atm_setgsMap_mct
!
!   !DESCRIPTION:
!       Sets MCT global seg map for the domain decomposition
!
!   !REVISION HISTORY:
!       Mar 06, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !INTERFACE:

    subroutine atm_setgsMap_mct( mpicom_a, ATMID, grid, gsMap_a, lx, ly )

        implicit none

!   !INPUT PARAMETERS:

        integer,          intent(in)    :: mpicom_a
        integer,          intent(in)    :: ATMID
        type (domain),    intent(in)    :: grid

!   !OUTPUT PARAMETERS:

        type (mct_gsMap),  intent(inout) :: gsMap_a
        integer, optional, intent(out)   :: lx, ly
!EOP
!BOC
        ! --- Local variables ---

        integer :: ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe

        integer :: gsize, lsize
        integer :: i, j, n, nx, ny, offset, rc

        integer, dimension(:), allocatable :: gindex

        character(len = *), parameter :: subname = 'atm_setgsMap_mct'

        ! --- Begin

        call get_ijk_from_grid( grid,                         &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe )

        ipe = min(ipe, ide-1)
        jpe = min(jpe, jde-1)

        ! --- coupling on unstaggered grid (mass-points).
        ! --- ide, jde indexes are on staggered grid, therefore nx = (ide-1)-ids+1 = ide-ids
        nx = ide - ids
        ny = jde - jds
        gsize = nx * ny

        lsize = (ipe-ips+1) * (jpe-jps+1)

        allocate(gindex(lsize), stat=rc)
        if (rc /= 0) then
            call atm_log(subname, 'Failed to allocate temporary workspace')
            call shr_sys_abort()
        end if


        n = 0
        offset = (jps-1) * nx
        do j = jps, jpe
           do i = ips, ipe
              n = n + 1
              gindex(n) = i + offset
           end do
           offset = offset + nx
        end do

        call mct_gsMap_init( gsMap_a, gindex, mpicom_a, ATMID, lsize, gsize )

        deallocate(gindex, stat=rc)
        if (rc /= 0) then
            call atm_log(subname, 'Failed to deallocate temporary workspace')
            call shr_sys_abort()
        end if

        if (present(lx)) lx = nx
        if (present(ly)) ly = ny

!EOC
    end subroutine atm_setgsMap_mct

!===============================================================================

    subroutine atm_domain_mct( grid, lsize, gsMap_a, dom_a )

        ! --- Arguments ---

        type (domain)  , intent(in)   :: grid
        integer        , intent(in)   :: lsize
        type(mct_gsMap), intent(in)   :: gsMap_a
        type(mct_gGrid), intent(inout):: dom_a

        ! --- Local variables ---

        integer  :: i, j, n
        integer  :: rc
        integer  :: ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    ips, ipe, jps, jpe, kps, kpe

        integer,  dimension(:), pointer :: idata
        real(r8), dimension(:), pointer :: data

        character (len = *), parameter :: subname = 'atm_domain_mct'
        !-------------------------------------------------------------------
        !
        ! Initialize mct atm domain
        !
        call mct_gGrid_init( GGrid=dom_a,                         &
                             CoordChars=trim(seq_flds_dom_coord), &
                             OtherChars=trim(seq_flds_dom_other), &
                             lsize=lsize )
        !
        ! Allocate memory
        !
        allocate(data(lsize), stat=rc)
        if (rc /= 0) then
            call atm_log(subname, 'Failed to allocate temporary workspace')
            call shr_sys_abort()
        end if
        !
        ! Initialize attribute vector with special value
        !
        call mct_gsMap_orderedPoints( gsMap_a, mytask, idata )
        call mct_gGrid_importIAttr( dom_a, 'GlobGridNum', idata, lsize )
        !
        ! Determine domain (numbering scheme is: West to East and South to North to South pole)
        ! Initialize attribute vector with special value
        !
        data(:) = -9999.0_r8
        call mct_gGrid_importRAttr( dom_a, "lat"  , data, lsize )
        call mct_gGrid_importRAttr( dom_a, "lon"  , data, lsize )
        call mct_gGrid_importRAttr( dom_a, "area" , data, lsize )
        call mct_gGrid_importRAttr( dom_a, "aream", data, lsize )
        data(:) = 0.0_r8
        call mct_gGrid_importRAttr( dom_a, "mask" , data, lsize )
        data(:) = 1.0_r8
        call mct_gGrid_importRAttr( dom_a, "frac" , data, lsize )
        !
        ! Fill in correct values for domain components
        !
        call get_ijk_from_grid( grid,                         &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe )

        ! --- coupling on unstaggered grid (mass-points).
        ! --- ide, jde indexes are on staggered grid
        ide = ide-1
        jde = jde-1
        ipe = min(ipe, ide)
        jpe = min(jpe, jde)

        n=0
        do j = jps, jpe
           do i = ips, ipe
              n = n + 1
              data(n) = grid % xlat(i,j)
           end do
        end do
        call mct_gGrid_importRAttr( dom_a, "lat", data, lsize )

        n=0
        do j = jps, jpe
           do i = ips, ipe
              n = n + 1
              data(n) = grid % xlong(i,j)
           end do
        end do
        call mct_gGrid_importRAttr( dom_a, "lon", data, lsize )

        n=0
        do j = jps, jpe
           do i = ips, ipe
              n = n + 1
              data(n) = atm_grid_area( grid, i, j )
!             data(n) = atm_grid_cell_area(i, j, ide, jde,   &
!                                          grid % xlong,     &
!                                          grid % xlat,      &
!                                          ims, ime, jms, jme)
           end do
        end do
        call mct_gGrid_importRAttr( dom_a, "area", data, lsize )

        data = 1._r8
        call mct_gGrid_importRAttr( dom_a, "mask", data, lsize )

        deallocate(data, stat=rc)
        if (rc /= 0) then
            call atm_log(subname, 'Failed to deallocate temporary workspace')
            call shr_sys_abort()
        end if

    end subroutine atm_domain_mct

!===============================================================================

    subroutine atm_export_mct( grid, a2x_a )

        type (domain),    intent(in)  :: grid
        type (mct_aVect), intent(out) :: a2x_a

        integer  :: i, ip1, j, jp1, n
        integer  :: ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    ips, ipe, jps, jpe, kps, kpe
        integer  :: kb, km, kt
        real(r8) :: dens, pbot, pslv, ptem, qbot, tbot, zbot, w1, w2
        real(r8) :: earth_u, earth_v, ubot, vbot
        real(r8) :: rainc, rainl, snowl

        real(r8), dimension(3) :: phi

        !
        integer,  parameter :: kbot = 1             ! bottom layer index
        real(r8), parameter :: co2vmr = 3.55e-04_r8 ! CAM value for CO2 volume mixing ratio
        character (len = *), parameter :: subname = 'atm_export_mct'

        ! --- Begin

        ! --- Set share output to log file
        call atm_log_set()

        call get_ijk_from_grid( grid,                         &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe )

        ipe = min(ipe, ide-1)
        jpe = min(jpe, jde-1)

        kb = kbot
        km = kb + 1
        kt = km + 1

! Halo exchange required for u & v if not already done
!#ifdef DM_PARALLEL
!#     include "HALO_EM_PHYS_A.inc"
!#endif

        phi = 0._r8

        n = 0
        do j = jps, jpe
           jp1 = j + 1
           do i = ips, ipe
              n = n + 1
              ip1 = i + 1
              ! --- Geopotential at lowest 3 levels
              phi = grid % ph_2(i,kb:kt,j) + grid % phb(i,kb:kt,j)
              ! --- Pressure at bottom layer (Pa)
              pbot = grid % p(i,kbot,j) + grid % pb(i,kbot,j)
              ! --- Potential temperature at bottom layer (K)
              ptem = grid % t_2(i,kbot,j) + t0
              ! --- Temperature at bottom layer (K)
              tbot = ptem * (pbot/p1000mb) ** rcp
              ! --- Height of bottom layer (m)
              zbot = 0.5_r8 * ( phi(2) - phi(1) ) / g
              ! --- Air density (kg/m3) & specific humidity (kg/kg) at bottom layer
              qbot = max( 0.0, grid % moist(i,kbot,j,P_QV) )
              dens = 1._r8 + qbot
              qbot = qbot / dens
              dens = dens / grid % alt(i,kb,j)
              ! --- Extrapolate pressure to surface linearly for now (as in WRF)
              w2 = ( phi(2) - phi(1) ) / ( phi(1) - phi(3) )
              w1 = 1._r8 - w2
              pslv = w1 * pbot + w2 * ( grid % p(i,km,j) + grid % pb(i,km,j) )

              ! --- Alternative formulation for surface density in WRF
              ! --- surface layer schemes: MM5, MM5 rev., PX
              !! dens = pslv / (r_d * tbot * (1._r8 + ep_1 * qbot))
!             qbot = max( 0.0, grid % moist(i,kbot,j,P_QV) )
!             dens = pslv / (shr_const_rdair * tbot * (1._r8 + shr_const_zvir * qbot))
!             qbot = qbot / (1._r8 + qbot)

              ! --- Rotate u & v at bottom layer to Earth reference frame ---
#if (defined(CCSM_USE_ATM_AOFLUX) && defined(CCSM_USE_COARE_AOFLUX))
              if ( grid % xland(i,j) >= 1.5 ) then
                 ubot = grid % u10(i,j) + grid % u_frame
                 vbot = grid % v10(i,j) + grid % v_frame
              else
                 ubot = 0.5_r8 * ( grid % u_2(i,kbot,j) + grid % u_2(ip1,kbot,j) ) + grid % u_frame
                 vbot = 0.5_r8 * ( grid % v_2(i,kbot,j) + grid % v_2(i,kbot,jp1) ) + grid % v_frame
              end if
#else
              ubot = 0.5_r8 * ( grid % u_2(i,kbot,j) + grid % u_2(ip1,kbot,j) ) + grid % u_frame
              vbot = 0.5_r8 * ( grid % v_2(i,kbot,j) + grid % v_2(i,kbot,jp1) ) + grid % v_frame
#endif
              earth_u = ubot * grid % cosa(i,j) - vbot * grid % sina(i,j)
              earth_v = vbot * grid % cosa(i,j) + ubot * grid % sina(i,j)

              ! --- Compute precipitation (kg m-2 s-1) ---
              ! --- Perhaps replace these instantaneous values with averages
              ! over coupling time step computed from accumulations (crcm) --
              ! although a2x is averaged afterwards...
              ! --- liquid, "convective"
              rainc = real(grid % raincv(i,j) / grid % dt, kind = r8)
              ! --- liquid, "large-scale"
              snowl = real( grid % snowncv(i,j) + grid % graupelncv(i,j) + grid % hailncv(i,j), kind = r8 )
              rainl = ( grid % rainncv(i,j) - snowl ) / grid % dt
              snowl = snowl / grid % dt

              a2x_a % rAttr(index_a2x_Sa_pslv   , n) = pslv
              a2x_a % rAttr(index_a2x_Sa_z      , n) = zbot
              a2x_a % rAttr(index_a2x_Sa_u      , n) = earth_u
              a2x_a % rAttr(index_a2x_Sa_v      , n) = earth_v
              a2x_a % rAttr(index_a2x_Sa_tbot   , n) = tbot
#ifdef CCSM_USE_COARE_AOFLUX
              a2x_a % rAttr(index_a2x_Sa_ptem   , n) = real(grid % pblh (i,j), kind = r8) !! temporary
#else
              a2x_a % rAttr(index_a2x_Sa_ptem   , n) = ptem
#endif
              a2x_a % rAttr(index_a2x_Sa_pbot   , n) = pbot
              a2x_a % rAttr(index_a2x_Sa_shum   , n) = qbot
              a2x_a % rAttr(index_a2x_Sa_dens   , n) = dens
              a2x_a % rAttr(index_a2x_Faxa_swnet, n) = real(grid % gsw  (i,j), kind = r8)
              a2x_a % rAttr(index_a2x_Faxa_lwdn , n) = real(grid % glw  (i,j), kind = r8)
              a2x_a % rAttr(index_a2x_Faxa_rainc, n) = rainc
              a2x_a % rAttr(index_a2x_Faxa_rainl, n) = rainl
              a2x_a % rAttr(index_a2x_Faxa_snowc, n) = 0._r8    ! No convective frozen precip available
              a2x_a % rAttr(index_a2x_Faxa_snowl, n) = snowl
              a2x_a % rAttr(index_a2x_Faxa_swndr, n) = real(grid % swndr(i,j), kind = r8)
              a2x_a % rAttr(index_a2x_Faxa_swvdr, n) = real(grid % swvdr(i,j), kind = r8)
              a2x_a % rAttr(index_a2x_Faxa_swndf, n) = real(grid % swndf(i,j), kind = r8)
              a2x_a % rAttr(index_a2x_Faxa_swvdf, n) = real(grid % swvdf(i,j), kind = r8)

!              ! aerosol deposition fluxes -- set to zero for now
              a2x_a%rAttr(index_a2x_Faxa_bcphidry, n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_bcphodry, n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_bcphiwet, n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_ocphidry, n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_ocphodry, n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_ocphiwet, n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_dstwet1 , n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_dstdry1 , n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_dstwet2 , n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_dstdry2 , n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_dstwet3 , n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_dstdry3 , n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_dstwet4 , n) = 0._r8
              a2x_a%rAttr(index_a2x_Faxa_dstdry4 , n) = 0._r8

              ! --- The following values are set to a constant vmr, but will need to be updated
              if (index_a2x_Sa_co2prog /= 0) then
                 a2x_a%rAttr(index_a2x_Sa_co2prog, n) = co2vmr ! atm prognostic co2
              end if
              if (index_a2x_Sa_co2diag /= 0) then
                 a2x_a%rAttr(index_a2x_Sa_co2diag, n) = co2vmr ! atm diagnostic co2
              end if

#ifdef CCSM_USE_ATM_AOFLUX
              a2x_a%rAttr(index_a2x_Sa_tref  , n) =  0._r8
              a2x_a%rAttr(index_a2x_Sa_qref  , n) =  0._r8
              a2x_a%rAttr(index_a2x_Sa_ustar , n) =  0._r8
              a2x_a%rAttr(index_a2x_Faxa_sen , n) =  0._r8
              a2x_a%rAttr(index_a2x_Faxa_lat , n) =  0._r8
              a2x_a%rAttr(index_a2x_Faxa_evap, n) =  0._r8

# ifdef CCSM_USE_WRF_LM
              if ( grid % xland(i,j) < 1.5 ) then
                 a2x_a%rAttr(index_a2x_Sa_tref  , n) =  real(grid % t2 (i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Sa_qref  , n) =  real(grid % q2 (i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Sa_ustar , n) =  real(grid % ust(i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Faxa_sen , n) = -real(grid % hfx(i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Faxa_lat , n) = -real(grid % lh (i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Faxa_evap, n) = -real(grid % qfx(i,j), kind = r8)
              endif
# endif

              if ( grid % xland(i,j) >= 1.5 ) then
                 a2x_a%rAttr(index_a2x_Sa_tref  , n) =  real(grid % t2 (i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Sa_qref  , n) =  real(grid % q2 (i,j), kind = r8)
# ifndef CCSM_USE_COARE_AOFLUX
                 a2x_a%rAttr(index_a2x_Sa_ustar , n) =  real(grid % ust(i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Faxa_sen , n) = -real(grid % hfx(i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Faxa_lat , n) = -real(grid % lh (i,j), kind = r8)
                 a2x_a%rAttr(index_a2x_Faxa_evap, n) = -real(grid % qfx(i,j), kind = r8)
# endif
              end if
#endif
           end do
        end do

        ! --- Reset log unit
        call atm_log_flush()
        call atm_log_reset()

    end subroutine atm_export_mct

!===============================================================================

    subroutine atm_import_mct( grid, x2a_a )

        type(domain),     intent(inout) :: grid
        type (mct_aVect), intent(in)    :: x2a_a

        logical  :: import_flux
        integer  :: i, j, n
        integer  :: ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    ips, ipe, jps, jpe, kps, kpe

        real     :: tsk
        real(r8) :: fcorr
        real(r8) :: frac, rbot, taux, tauy, ts
        real(r8) :: avsdr, anidr, avsdf, anidf,   &
                    swvdr, swndr, swvdf, swndf, swdwn

        integer,  parameter :: kbot = 1             ! bottom layer index
        real,     parameter :: albedo_nighttime = -1.0
        real(r8), parameter :: daylight_threshold = 1.e-05_r8, &
                               logz0_min = log(0.01_r8),       &
                               bulk_dens_snow = 250._r8           ! bulk density snow in CLM (kg m-3)
                                                                  ! NOTE: WRF uses 200 kg m-3

        character (len = *), parameter :: subname = 'atm_import_mct'

        ! --- Begin

        ! NOTE: shr_flux_atmOcn sets missing values to 0.0
        ! However, shr_flux_atmIce uses shr_const_spval (1.e+30)
        ! A through check for missing values needs to be implemented for ice

        ! --- Set share output to log file
        call atm_log_set()

        call get_ijk_from_grid( grid,                         &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe )

        ipe = min(ipe, ide-1)
        jpe = min(jpe, jde-1)

        n = 0
        do j = jps, jpe
           do i = ips, ipe
              n = n + 1
              ! --- Update land & ice fractions ---
#ifndef CCSM_USE_WRF_LM
              grid % xland(i,j) = 2.0 - x2a_a % rAttr(index_x2a_Sf_lfrac, n)    ! surface land fraction
#endif
#ifndef CCSM_USE_WRF_SEAICE
              grid % xice (i,j) = x2a_a % rAttr(index_x2a_Sf_ifrac, n)          ! surface ice fraction
#endif
              ! --- Radiation ---
              !  +  Shortwave
#ifdef CCSM_USE_ATM_SW_NET
              !     Use WRF's standard formula
              grid % swdown(i,j) = grid % gsw(i,j) / ( 1. - grid % albedo(i,j) )
#else
              !     Use spectral components to compute the downwelling total shortwave radiation (SWDOWN)
              swvdr = real(grid % swvdr(i,j), kind = r8)        ! visibile direct  incident radiation
              swndr = real(grid % swndr(i,j), kind = r8)        ! near-IR  direct  incident radiation
              swvdf = real(grid % swvdf(i,j), kind = r8)        ! visibile diffuse incident radiation
              swndf = real(grid % swndf(i,j), kind = r8)        ! near-IR  diffuse incident radiation
              swdwn = swvdr + swvdf + swndr + swndf
              grid % swdown(i,j) = real(swdwn)                  ! write total shortwave down WRF variable
              if ( grid % xland(i,j) >= 1.5 ) then
                  !     Compute ALBEDO using spectral components over ocean.
                  !     (a) save previous spectral albedos
                  avsdr = real(grid % avsdr(i,j), kind = r8)        ! albedo, visible, direct
                  anidr = real(grid % anidr(i,j), kind = r8)        ! albedo, near-ir, direct
                  avsdf = real(grid % avsdf(i,j), kind = r8)        ! albedo, visible, diffuse
                  anidf = real(grid % anidf(i,j), kind = r8)        ! albedo, near-ir, diffuse
                  !     (b) load new spectral albedos from the coupler.
                  grid % avsdr(i,j) = x2a_a % rAttr(index_x2a_Sx_avsdr, n)        ! albedo, visible, direct
                  grid % anidr(i,j) = x2a_a % rAttr(index_x2a_Sx_anidr, n)        ! albedo, near-ir, direct
                  grid % avsdf(i,j) = x2a_a % rAttr(index_x2a_Sx_avsdf, n)        ! albedo, visible, diffuse
                  grid % anidf(i,j) = x2a_a % rAttr(index_x2a_Sx_anidf, n)        ! albedo, near-ir, diffuse
                  !     Calculate albedo by a weighted combination of the spectral shortwave values from
                  !     WRF and spectral albedos from the coupler. 
                  !     This needs to be done using previous values for albedos so that the net ALBEDO
                  !     variable corresponds to the weighted average albedo using the spectral albedos used
                  !     with the spectral shortwave components in the internal WRF calculations.
                  !     Make sure to account for when there is no sunlight (nswi ~ 0) as the model will blow up otherwise.
                  if ( swdwn < daylight_threshold ) then            ! nighttime - threshold from module_surface_driver.F
                     grid % albedo(i,j) = albedo_nighttime          ! set a fill value if no shortwave
                  else
                     grid % albedo(i,j) = ( avsdr * swvdr + anidr * swndr + avsdf * swvdf + anidf * swndf ) / swdwn
                  end if
              end if
#endif

              fcorr = 1._r8 
              import_flux = .false.

              if ( grid % xland(i,j) < 1.5 ) then
                 ! --- Over land
#ifdef CCSM_USE_WRF_LM
                 ts = grid % tsk(i,j)
                 grid % lwups(i,j) = grid % emiss(i,j) * shr_const_stebol * ts * ts * ts * ts
#else
                 ! Import fluxes from coupler
                 import_flux = .true.

                 ! --- Skin ---
                 grid % tsk(i,j) = x2a_a % rAttr(index_x2a_Sx_t, n)            ! surface temperature

                 ! Set snow water equivalent and physical depth according to CLM
                 grid % snow (i,j) = 1.e+03_r8 * x2a_a % rAttr(index_x2a_Sl_snowh, n)  ! mm snow water in CLM
                 grid % snowh(i,j) = x2a_a % rAttr(index_x2a_Sl_snowh, n) / bulk_dens_snow
                 ! Recomputing u* from taux and tauy, since land & ice models do not provide it
                 ! WARNING: There seem to be some confusion between the air density at bottom layer
                 ! and the air density at the surface. Here we use the value at the lowest layer for
                 ! consistency with CPL7 fluxes over the ocean, but we *should* use the value of
                 ! air density at the surface. Needs to be revisited.
                 rbot = max( 0.0, grid % moist(i,kbot,j,P_QV) )
                 rbot = ( 1._r8 + rbot ) / grid % alt(i,kbot,j)             ! approximate moist air density at bottom layer
                 taux = -x2a_a % rAttr(index_x2a_Faxx_taux, n)       ! wind stress, zonal
                 tauy = -x2a_a % rAttr(index_x2a_Faxx_tauy, n)       ! wind stress, meridional
                 grid % ust(i,j) = sqrt(sqrt(taux * taux + tauy * tauy) / rbot)
#endif
              else
                 ! --- Over ocean

#ifdef CCSM_USE_ATM_AOFLUX
                 ! Only the following fields are needed when using built-in surface layer scheme over ocean
# ifdef CCSM_USE_ATM_LOWBC
                 ! Pad missing SSTs with WRF lower boundary data
                 if ( x2a_a % rAttr(index_x2a_So_t, n) > 0._r8 ) then
                     grid % sst(i,j) = x2a_a % rAttr(index_x2a_So_t, n)         ! sea surface temperature
                 else
                     if ( grid % lowbcsst(i,j) > 0. ) grid % sst(i,j) = grid % lowbcsst(i,j)  ! sea surface temperature from lower bc file
                 endif
# else
                 grid % sst(i,j) = x2a_a % rAttr(index_x2a_So_t, n)         ! sea surface temperature
# endif
                 ! --- Upward longwave radiation flux 
!                grid % lwups(i,j) = -fcorr * grid % emiss(i,j) * x2a_a % rAttr(index_x2a_Faxx_lwup, n)
                 ts = grid % sst(i,j)
                 grid % lwups(i,j) = grid % emiss(i,j) * shr_const_stebol * ts * ts * ts * ts
#else
                 ! Import fluxes from coupler
                 import_flux = .true.
# ifdef CCSM_USE_WRF_LM
                 ! Fix merged fields from coupler if WRF land model is used
                 fcorr = 1._r8 / max( 1.e-03_r8, 1._r8 - x2a_a % rAttr(index_x2a_Sf_lfrac, n) )
# endif

                 ! --- Sea Surface Temperature ---
                 grid % sst(i,j) = x2a_a % rAttr(index_x2a_So_t, n)         ! sea surface temperature

                 ! --- Skin ---
                 grid % tsk(i,j) = fcorr * x2a_a % rAttr(index_x2a_Sx_t, n)            ! surface temperature

                 grid % ust  (i,j) = x2a_a % rAttr(index_x2a_So_ustar, n)    ! surface friction velocity
                 grid % snow (i,j) = 0.0                                    ! surface snow depth over water
                 grid % snowh(i,j) = 0.0
#endif

                 ! Update skin and soil temperature according to WRF's scheme
                 if (((grid % sst(i,j) .lt. 350.0) .and. (grid % sst(i,j) .gt. 250.)) &
                     .or. (grid % tsk(i,j) .eq. 0.)) then
                    grid % tsk(i,j)    = grid % sst(i,j)
                    grid % tslb(i,1,j) = grid % sst(i,j)
                 end if
              end if
 
              ! --- Radiation ---
              ! --- Upward longwave ---
            ! grid % lwups(i,j) = -x2a_a % rAttr(index_x2a_Faxx_lwup, n)       ! upward longwave heat flux

              if ( import_flux ) then
                 ! --- Reference states (2m & 10m)
                 grid % t2(i,j)    =  fcorr * x2a_a % rAttr(index_x2a_Sx_tref, n)         ! 2m reference temperature
                 grid % q2(i,j)    =  fcorr * x2a_a % rAttr(index_x2a_Sx_qref, n)         ! 2m reference specific humidity

                 ! --- Turbulent layer
                 grid % lh(i,j)    = -fcorr * x2a_a % rAttr(index_x2a_Faxx_lat,  n)       ! latent          heat flux
                 grid % hfx(i,j)   = -fcorr * x2a_a % rAttr(index_x2a_Faxx_sen,  n)       ! sensible        heat flux
                 grid % qfx(i,j)   = -fcorr * x2a_a % rAttr(index_x2a_Faxx_evap, n)       ! evaporation    water flux
                 !! grid % znt(i,j)   =  fcorr * exp(max(logz0_min, x2a_a % rAttr(index_x2a_Sx_logz0, n)))   ! roughness length (m)

                 ! --- Upward longwave radiation
                 grid % lwups(i,j) = -fcorr * x2a_a % rAttr(index_x2a_Faxx_lwup, n)

!                ts = x2a_a % rAttr(index_x2a_Sx_t, n) * fcorr
!                grid % emiss(i,j) = -fcorr * x2a_a % rAttr(index_x2a_Faxx_lwup, n)/(shr_const_stebol * ts * ts * ts * ts)  ! emissivity

                 ! Compute surface emissivity
                 tsk = grid % tsk(i,j)
                 grid % emiss(i,j) = grid % lwups(i,j) / (shr_const_stebol * tsk * tsk * tsk * tsk)  ! emissivity
#if (NMM_CORE==1)
                 taux = -fcorr * x2a_a % rAttr(index_x2a_Faxx_taux, n)       ! wind stress, zonal
                 tauy = -fcorr * x2a_a % rAttr(index_x2a_Faxx_tauy, n)       ! wind stress, meridional
                 ! Rotate back from Earth's (coupler) to WRF's grid
                 grid % taux(i,j) = taux * grid % cosa(i,j) + tauy * grid % sina(i,j)       ! wind stress, zonal
                 grid % tauy(i,j) = tauy * grid % cosa(i,j) - taux * grid % sina(i,j)       ! wind stress, meridional
#else
                 ! WRF does not use taux, tauy
#endif
              endif

           end do
        end do

        ! --- Reset log unit
        call atm_log_flush()
        call atm_log_reset()

    end subroutine atm_import_mct

end module atm_comp_mct
