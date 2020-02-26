!BOP
!   !MODULE: rocn_comp_mct
!   !INTERFACE:

module rocn_comp_mct

!   !DESCRIPTION:
!       This is the main driver for the Regional Ocean Modeling System (ROMS)
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!       Aug 09, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - added wind/velocities rotation
!       Apr 08, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - included modifications by Tony Craig
!       Jun 05, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - improved consistency check, history/restart files
!
!   !USES:
    use mct_mod
    use esmf

    use seq_cdata_mod
    use seq_flds_mod
    use seq_infodata_mod
    use seq_timemgr_mod
    use perf_mod
    use shr_file_mod
    use shr_sys_mod,  only : shr_sys_flush
    use shr_kind_mod, only : shr_kind_cs, &
                             shr_kind_cl


    ! ROMS modules
    use mod_kinds        , only : r8

    use mod_parallel,      only : ocn_root => MyMaster, &
                                  ocn_tile => MyRank,   &
                                  log_task => OutThread

    use mod_iounits,       only : stdout

    use mod_scalars,       only : ROMS_exit_flag => exit_flag, &
                                  ROMS_noerror   => NoError,   &
                                  LdefRST, LwrtRST, LcycleRST, &
                                  LastRec, nrrec
    use ocn_cplindices
    use ocn_instance

    ! additional modules to interface with ROMS
    use roms_run_mod
    use ocn_io_tools
    use ocn_utils
    use ocn_timemgr_mod
    use ocn_files

    implicit none
!
!   !PUBLIC MEMBER FUNCTIONS:

    public :: rocn_init_mct
    public :: rocn_run_mct
    public :: rocn_final_mct

!   !PUBLIC DATA:
!
!
!EOP

!   !Save all variables in this scoping unit ????
!    save

!   !Private by default
    private

!   !PRIVATE MODULE FUNCTIONS:

    ! --- Other parameters ---
    integer, parameter :: ocn_start_init = 1, & ! startup run
                          ocn_start_cont = 2, & ! restart run
                          ocn_start_brnc = 3    ! branch run

    integer, parameter :: ocn_Success =  0, &   ! ocean success return code
                          ocn_Failure = -1      ! ocean failure return code

    character(len = *), parameter :: myModelName = 'roms'   !local model name

    ! --- Private variables ---

    integer :: ng       ! ROMS grid number
    integer :: nx_global, ny_global

    type(seq_infodata_type), pointer :: infodata

    character(len = shr_kind_cs) :: caseid

!   Time average of flux fields:
!   . counter
    integer :: avg_count
!   . instantaneous and total field vectors
    type (mct_aVect) :: o2x_SNP, o2x_SUM
!   . list of flux fields to be averaged
    character(*), parameter :: o2x_avg_flds = "So_t:So_u:So_v"

contains

!==============================================================================
!BOP
!   !ROUTINE: rocn_init_mct
!   !INTERFACE:

    subroutine rocn_init_mct(EClock, cdata, x2o, o2x, NLFilename)

!   !DESCRIPTION:
!       Initialize ROMS ocean model
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!       Jun 05, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - added start time check, CESM-style restart/history
!
!   !USES:

        use mod_param,   only : Lm, Mm, Ngrids
        use mod_scalars, only : time_ref

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        type(ESMF_Clock)            , intent(inout) :: EClock
        type(seq_cdata)             , intent(inout) :: cdata
        type(mct_aVect)             , intent(inout) :: x2o, o2x
        character(len = *), optional, intent(in)    :: NLFilename

!EOP
!BOC
        !--- Local variables ---

        integer :: OCNID,         &
                   mpicom_o,      &
                   lsize,         &
                   ocn_cpl_dt,    &
                   ocn_int_dt,    &
                   runtype,       &
                   drv_start_ymd, &
                   drv_start_tod, &
                   ocn_start_ymd, &
                   ocn_start_tod, &
                   errorCode

        character (len = shr_kind_cl)  :: starttype

        type(mct_gsMap), pointer :: gsMap_o

        type(mct_gGrid), pointer :: dom_o

        character(len = *), parameter :: subname = 'rocn_init_mct'

        !--- Begin

        ! --- Set cdata pointers ---

        call seq_cdata_setptrs(cdata,         ID=OCNID,  mpicom=mpicom_o, &
                               gsMap=gsMap_o, dom=dom_o, infodata=infodata)

        ! --- Initialize ROMS instance ---

        call ocn_instance_init( OCNID )

        call roms_init(mpicom_o)

        ! --- Set shr logging to my log file ---

        call ocn_log_init(mpicom_o, OCNID)
        call ocn_set_logging(stdout)

        ! --- Set field index values ---

        call ocn_cplindicesSet()

        ! --- Set start_type: initial, continue, branch ---

        call seq_infodata_GetData(infodata, case_name=caseid, start_type=starttype)

        if (     trim(starttype) == trim(seq_infodata_start_type_start)) then
           runtype = ocn_start_init
        else if (trim(starttype) == trim(seq_infodata_start_type_cont) ) then
           runtype = ocn_start_cont
        else if (trim(starttype) == trim(seq_infodata_start_type_brnch)) then
           runtype = ocn_start_brnc
        else
           write(stdout,*) 'rocn_comp_mct ERROR: unknown starttype'
           call ocn_abort(sigAbort, ' rocn_comp_mct ERROR: unknown starttype')
        end if

        ! --- Start timer ---

        call t_startf('roms_init')

        ! --- Start up ROMS ---

        call roms_startup(nmlfile='ocn_in')
        if (ROMS_exit_flag /= ROMS_noerror) &
           call ocn_abort(sigAbort, 'ERROR in roms_init_state')

        ! --- Read time parameters & initialize internal clock ---

        call ocn_timemgr_InitClock(mpicom_o, nmlfile = 'ocn_in')

        ! --- Reset number of ocean grids to 1 (to be extended) --

        Ngrids = 1      ! total grids
        ng     = 1      ! current grid #

        ! --- Set global grid size ---

        nx_global = Lm(ng)+2
        ny_global = Mm(ng)+2


        ! --- Check for consistency between ROMS and the driver
        ! (perhaps not necessary: could set ROMS init time = cpl init time.)

        if ( runtype == ocn_start_init ) then
            call seq_timemgr_EClockGetData(EClock, start_ymd=drv_start_ymd, start_tod=drv_start_tod)
        else
            call seq_timemgr_EClockGetData(EClock, curr_ymd=drv_start_ymd,  curr_tod=drv_start_tod)
        end if
        call ocn_timemgr_getClock(start_ymd=ocn_start_ymd, start_tod=ocn_start_tod)

        if (drv_start_ymd /= ocn_start_ymd) then
           write(stdout,*) ' drv: start ymd = ', drv_start_ymd, &
                           ' ocn: start ymd = ', ocn_start_ymd
           call ocn_abort(sigAbort,'ERROR: ocean start date different from coupler')
        end if

        if (drv_start_tod /= ocn_start_tod) then
           write(stdout,*) ' drv: start tod = ', drv_start_tod, &
                           ' ocn: start tod = ', ocn_start_tod
           call ocn_abort(sigAbort,'ERROR: ocean start time different from coupler')
        end if

        ! --- Check if coupling time step is multiple of ROMS baroclinic time step

        call seq_timemgr_EClockGetData(EClock, dtime=ocn_cpl_dt)
        call ocn_timemgr_getClock( dtime=ocn_int_dt )
        if (ocn_cpl_dt < ocn_int_dt) then
            write(stdout,*) ' drv: coupling   timestep = ',ocn_cpl_dt, &
                            ' ocn: baroclinic timestep = ',ocn_int_dt
            call ocn_abort(sigAbort,subname // 'ERROR: coupling time step must be >= ROMS baroclinic time step (DT)')
        else if (mod(ocn_cpl_dt, ocn_int_dt) /= 0) then
            write(stdout,*) ' drv: coupling   timestep = ',ocn_cpl_dt, &
                            ' ocn: baroclinic timestep = ',ocn_int_dt
            call ocn_abort(sigAbort,subname // 'ERROR: coupling time step must be multiple of ROMS baroclinic time step (DT)')
        end if

        ! --- Set reference time to internal clock start time (not for now) ---

        !time_ref = real(ocn_start_ymd, kind = r8) + ocn_start_tod / 86400._r8

        ! --- Clear up input settings for restart, since the coupler will control restart
        LdefRST(ng)   = .false.
        LwrtRST(ng)   = .false.
        LcycleRST(ng) = .false.
        nrrec(ng)     = -1      ! restart from the most recent time record
        LastRec(ng)   = .true.
        call ocn_pfile_set( OCNID )
        call ocn_history_outfile_set( ng, caseid )
        if ( runtype /= ocn_start_init ) call ocn_restart_infile_set( ng )

        ! --- Read ROMS initial conditions and observations ---
        call roms_ini_data()
        if (ROMS_exit_flag /= ROMS_noerror) &
           call ocn_abort(sigAbort, 'ERROR in roms_ini_data')

        ! --- Stop timer ---

        call t_stopf('roms_init')

        ! --- Initialize MCT attribute vectors and indices ---

        call t_startf('roms_mct_init')

        call rocn_SetGSMap_mct(GSMap_o, mpicom_o, OCNID, nx_global, ny_global)
        lsize = mct_gsMap_lsize(gsMap_o, mpicom_o)

        ! --- Initialize mct ocn domain ---

        call ocn_domain_mct(lsize, gsMap_o, dom_o)

        ! --- Initialize coupling info ---

        ! --- Initialize mct attribute vectors ---

        call mct_aVect_init(x2o, rList=seq_flds_x2o_fields, lsize=lsize)
        call mct_aVect_zero(x2o)

        call mct_aVect_init(o2x, rList=seq_flds_o2x_fields, lsize=lsize)
        call mct_aVect_zero(o2x)

        ! --- Initialize counter and vectors for field averages
        avg_count = 0

        call mct_aVect_init(o2x_SNP, rList=o2x_avg_flds, lsize=lsize)
        call mct_aVect_zero(o2x_SNP)

        call mct_aVect_init(o2x_SUM, rList=o2x_avg_flds, lsize=lsize)
        call mct_aVect_zero(o2x_SUM)

        ! --- Send initial state to driver ---
        call ocn_export_mct(o2x, errorCode)
        if (errorCode /= ocn_Success) &
           call ocn_abort(sigAbort, 'ERROR in ocn_export_mct')

        ! --- Copy coupling data to buffer for averages ---
        ! --- Not used ---
        ! call mct_aVect_copy( o2x, o2x_SUM )
        ! avg_count = 1


        call t_stopf('roms_mct_init')

        call seq_infodata_PutData( infodata,           &
                                   ocn_nx = nx_global, &
                                   ocn_ny = ny_global )

        call seq_infodata_PutData( infodata,                 &
                                   ocn_prognostic   =.true., &
                                   ocnrof_prognostic=.true. )

        ! --- Reset shr logging to original units ---

        call ocn_reset_logging

        ! --- Output delimiter to log file ---

        call ocn_log_delim (" End of initialization")

!EOC
    end subroutine rocn_init_mct

!==============================================================================
!BOP
!   !ROUTINE: rocn_run_mct
!
!   !DESCRIPTION:
!       Run ROMS for a coupling interval
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!       Apr 09, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - rewrote set/reset logging
!
!   !INTERFACE:

    subroutine rocn_run_mct(EClock, cdata, x2o, o2x)

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        type(ESMF_Clock)            , intent(inout) :: EClock
        type(seq_cdata)             , intent(inout) :: cdata
        type(mct_aVect)             , intent(inout) :: x2o, o2x

!EOP
!BOC
        !--- Local variables ---

        integer :: drv_ymd, drv_tod, drv_year, drv_month, drv_day

        integer :: ymd, tod

        integer :: errorCode, rc

        real(r8) :: drv_curr_time,    &
                    ocn_run_interval, &
                    roms_StepTime

        character(len = 256 ) :: msg, timestr

        type(ESMF_Time) :: drv_CurrTime, ocn_CurrTime

        character(len = *), parameter :: subname = 'rocn_run_mct'


        !--- Begin

        ! --- Reset shr logging to my log file ---

        call ocn_set_logging(stdout)

        ! --- Get driver clock

        call ESMF_ClockGet(EClock,  CurrTime=drv_CurrTime, rc=rc)
        call ocn_timemgr_CheckRC(rc, clock_name='driver', callsub_name=subname)

        call ESMF_ClockGet(o_Clock, CurrTime=ocn_CurrTime, rc=rc)
        call ocn_timemgr_CheckRC(rc, clock_name='ocean ', callsub_name=subname)

        if (ocn_CurrTime > drv_CurrTime) then
           timestr = ''
           call ESMF_TimeGet( drv_CurrTime, timeString=timestr )
           call ocn_log (subname, 'driver current time', trim(timestr))
           timestr = ''
           call ESMF_TimeGet( ocn_CurrTime, timeString=timestr )
           call ocn_log (subname, 'ocean current time', trim(timestr))
           call ocn_abort(sigAbort, '('//subname//') ERROR: overshot coupling time')
        end if

        ! --- Advance the model in time over coupling interval

        step_loop: do while (ocn_CurrTime < drv_CurrTime)

            call roms_advance_step
            if (ROMS_exit_flag /= ROMS_noerror) then
               msg = 'roms_advance_step'
               exit
            end if

            call ocn_timemgr_advanceClock
            if (ROMS_exit_flag /= ROMS_noerror) then
               msg = 'ocn_timemgr_advanceClock'
               exit
            end if

            call roms_set_state
            if (ROMS_exit_flag /= ROMS_noerror) then
               msg = 'roms_set_state'
               exit
            end if

            call roms_radiation_stress
            if (ROMS_exit_flag /= ROMS_noerror) then
               msg = 'roms_radiation_stress'
               exit
            end if

            call ocn_import_mct(x2o, errorCode)
            if (errorCode /= ocn_Success) then
               msg = 'ocn_import_mct'
               ROMS_exit_flag = 4
               exit
            end if

            call roms_step
            if (ROMS_exit_flag /= ROMS_noerror) then
               msg = 'roms_step'
               exit
            end if

            call ocn_export_mct(o2x, errorCode)
            if (errorCode /= ocn_Success) then
               msg = 'ocn_export_mct'
               ROMS_exit_flag = 4
               exit
            end if

#ifndef CCSM_AOFLUX_NOAVERAGE
            call mct_aVect_copy ( o2x, o2x_SNP )
            call mct_avect_accum( aVin = o2x_SNP, aVout = o2x_SUM )
            avg_count = avg_count + 1
#endif

            call ESMF_ClockGet(o_Clock, CurrTime=ocn_CurrTime, rc=rc)
            call ocn_timemgr_CheckRC(rc, clock_name='ocean ', callsub_name=subname)

        end do step_loop

        ! --- Check for error codes ---
        if (ROMS_exit_flag /= ROMS_noerror) then
            ! --- Save latest state into restart file if blowing up ---
            if (ROMS_exit_flag == 1) then
                rc = ROMS_exit_flag
                ROMS_exit_flag = ROMS_noerror
                call ocn_restart_write( ng, caseid )
                ROMS_exit_flag = rc
            end if
            call ocn_abort(sigAbort, '('//subname//')::('//trim(msg)//'):: ERROR detected')
        end if

#ifndef CCSM_AOFLUX_NOAVERAGE
        ! --- Average fields and copy into coupling vector
        call mct_aVect_avg ( o2x_SUM, avg_count )
        call mct_aVect_copy( o2x_SUM, o2x       )
        call mct_aVect_zero( o2x_SUM )
        avg_count = 0
#endif

        ! --- Check if restart is needed
        if ( seq_timemgr_RestartAlarmIsOn( EClock ) ) then
            call ocn_restart_write( ng, caseid )
        end if
        ! --- Check that internal clock is in sync with master clock

        call ocn_timemgr_checkInSyncClock(EClock, name_SyncClock='driver')

        ! --- Reset shr logging to original units ---

        call ocn_reset_logging

!EOC
    end subroutine rocn_run_mct

!==============================================================================
!BOP
!   !ROUTINE: rocn_final_mct
!
!   !DESCRIPTION:
!       Properly finalize ROMS
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!       Apr 10, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - added arguments
!
!   !INTERFACE:

    subroutine rocn_final_mct(EClock, cdata, x2o, o2x)

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        type(ESMF_Clock)            , intent(inout) :: EClock
        type(seq_cdata)             , intent(inout) :: cdata
        type(mct_aVect)             , intent(inout) :: x2o, o2x

!EOP
        ! --- Local variables ---

        character(len = *), parameter :: subname = 'rocn_final_mct'

!BOC
        ! --- Finalize model ---
        call roms_finalize
!EOC
    end subroutine rocn_final_mct


!==============================================================================
!BOP
!   !ROUTINE: ocn_tile_bounds
!
!   !DESCRIPTION:
!       Get ROMS tile boundaries
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !INTERFACE:

    subroutine ocn_tile_bounds( ng, tile,           &
                                ibs, ibe, jbs, jbe, &
                                ids, ide, jds, jde, &
                                its, ite, jts, jte  )

        use mod_param, only : BOUNDS

        implicit none

!   !INPUT/OUTPUT PARAMETERS:
        integer, intent(in)  :: ng, tile
        integer, intent(out) :: ibs, ibe, jbs, jbe, &
                                ids, ide, jds, jde, &
                                its, ite, jts, jte

!EOP
        ! --- Local variables ---

        character(len = *), parameter :: subname = 'ocn_tile_bounds'

        !--- Begin

        ! --- Tile lower/upper bounds (include ghost points) ---

        ibs = BOUNDS(ng)%LBi(tile)
        ibe = BOUNDS(ng)%UBi(tile)
        jbs = BOUNDS(ng)%LBj(tile)
        jbe = BOUNDS(ng)%UBj(tile)

        ! --- Extended tile bounds ---
        ! --- Include boundary points if adjacent to physical boundary ---

        ids = BOUNDS(ng)%IstrR(tile)
        ide = BOUNDS(ng)%IendR(tile)
        jds = BOUNDS(ng)%JstrR(tile)
        jde = BOUNDS(ng)%JendR(tile)

        ! --- Interior tile bounds, used for computation ---

        its = BOUNDS(ng)%Istr(tile)
        ite = BOUNDS(ng)%Iend(tile)
        jts = BOUNDS(ng)%Jstr(tile)
        jte = BOUNDS(ng)%Jend(tile)


    end subroutine ocn_tile_bounds

!==============================================================================
!BOP
!   !ROUTINE: rocn_SetGSMap_mct
!
!   !DESCRIPTION:
!       Sets MCT global seg map for the domain decomposition
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!       Mar 23, 2015 - Raffaele Montuoro <rmontuoro@tamu.edu> - Add offset,nx,ny
!
!   !INTERFACE:

    subroutine rocn_SetGSMap_mct( gsMap_ocn, mpicom_ocn, OCNID, nx, ny, ioffset, joffset )

        implicit none

        type(mct_gsMap)  , intent(inout) :: gsMap_ocn
        integer          , intent(in)    :: mpicom_ocn
        integer          , intent(in)    :: OCNID
        integer          , intent(in)    :: nx, ny
        integer, optional, intent(in)    :: ioffset, joffset

        ! --- Local variables ---

        integer  :: i, j, lsize, gsize, n
        integer  :: ibs, ibe, jbs, jbe, &
                    ids, ide, its, ite, &
                    jds, jde, jts, jte

        integer, dimension(:), allocatable :: gindex

        character(len = *), parameter :: subname = 'rocn_SetGSMap_mct'

        ! --- Begin

        call ocn_tile_bounds( ng, ocn_tile, &
                              ibs, ibe, jbs, jbe, &
                              ids, ide, jds, jde, &
                              its, ite, jts, jte )

        lsize = (ide-ids+1) * (jde-jds+1)

        allocate(gindex(lsize))

        gsize = nx * ny

        if (present(ioffset)) then
           ids = ids + ioffset
           ide = ide + ioffset
        end if
 
        if (present(joffset)) then
           jds = jds + joffset
           jde = jde + joffset
        end if

        n=0
        do j=jds, jde
           do i=ids, ide
              n=n+1
              gindex(n) = i + 1 + j * nx
           end do
        end do

        call mct_gsMap_init( gsMap_ocn,  gindex, mpicom_ocn,  OCNID, lsize, gsize )

        deallocate(gindex)

    end subroutine rocn_SetGSMap_mct

!==============================================================================
!BOP
!   !ROUTINE: ocn_domain_mct
!   !INTERFACE:

    subroutine ocn_domain_mct( lsize, gsMap_ocn, dom_ocn )

!   !DESCRIPTION:
!       Sets MCT global seg map for the domain decomposition
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
        use mod_grid,      only : GRID
        use shr_const_mod, only : SHR_CONST_PI, &
                                  SHR_CONST_REARTH

        implicit none

!   !INPUT PARAMETERS:

        integer        , intent(in)    :: lsize
        type(mct_gsMap), intent(in)    :: gsMap_ocn

!   !OUTPUT PARAMETERS:

        type(mct_gGrid), intent(inout) :: dom_ocn

!EOP
!BOC
        !--- Local variables ---
        integer :: i, ierr, imax, j, jmax, n
        integer :: ibs, ibe, jbs, jbe, &
                   ids, ide, its, ite, &
                   jds, jde, jts, jte

        integer , dimension(:), pointer :: idata
        real(r8), dimension(:), pointer :: data

        real(r8) :: dlon, latn, lats

        real(r8), parameter :: RAD_PER_DEG = SHR_CONST_PI/180.0_r8, &
                               REARTHINVSQ = 1.0_r8/(SHR_CONST_REARTH*SHR_CONST_REARTH)

        character (len = *), parameter :: subname = 'ocn_domain_mct'

        !--- Initialize mct domain type, lat/lon & mask

        call mct_gGrid_init( GGrid=dom_ocn,                         &
                             CoordChars=trim(seq_flds_dom_coord),   &
                             OtherChars=trim(seq_flds_dom_other),   &
                             lsize=lsize )

        call mct_aVect_zero( dom_ocn%data )
        allocate(data(lsize), stat=ierr)
        if (ierr > 0) call ocn_abort( sigAbort, &
           '('//trim(subname)//') ERROR: failed to allocate temporary workspace')

        !--- Determine global gridpoint number attribute, GlobGridNum

        call mct_gsMap_orderedPoints( gsMap_ocn, ocn_tile, idata )
        call mct_gGrid_importIAttr( dom_ocn, 'GlobGridNum', idata, lsize )

        !--- Initialize attribute vector with special value ---

        data(:) = -9999.0_r8
        call mct_gGrid_importRAttr( dom_ocn, "lat",   data, lsize )
        call mct_gGrid_importRAttr( dom_ocn, "lon",   data, lsize )
        call mct_gGrid_importRAttr( dom_ocn, "area",  data, lsize )
        call mct_gGrid_importRAttr( dom_ocn, "aream", data, lsize )

        data(:) = 0.0_r8
        call mct_gGrid_importRAttr( dom_ocn, "mask",  data, lsize )
        call mct_gGrid_importRAttr( dom_ocn, "frac",  data, lsize )

        !--- Fill in correct values ---

        call ocn_tile_bounds( ng, ocn_tile,       &
                              ibs, ibe, jbs, jbe, &
                              ids, ide, jds, jde, &
                              its, ite, jts, jte  )

        ! --- Latitude ---

        n=0
        do j=jds,jde
           do i=ids,ide
              n=n+1
              data(n) = GRID(ng)%latr(i,j)
           end do
        end do
        call mct_gGrid_importRAttr( dom_ocn, "lat",  data, lsize )

        ! --- Longitude ---

        n=0
        do j=jds,jde
           do i=ids,ide
              n=n+1
              data(n) = GRID(ng)%lonr(i,j)
           end do
        end do
        call mct_gGrid_importRAttr( dom_ocn, "lon",  data, lsize )

        ! --- Masking ---

        n=0
        do j=jds,jde
           do i=ids,ide
              n=n+1
              data(n) = GRID(ng)%rmask(i,j)
           end do
        end do
        call mct_gGrid_importRAttr( dom_ocn, "mask", data, lsize )

        ! --- Grid cells area ---

        n=0
        do j=jds,jde
           do i=ids,ide
              n=n+1
              data(n) = REARTHINVSQ / ( GRID(ng)%pm(i,j) * GRID(ng)%pn(i,j) )
           end do
        end do

        call mct_gGrid_importRAttr( dom_ocn, "area", data, lsize )

        ! --- Grid cell fraction (temporary) ---

        n=0
        do j=jds,jde
           do i=ids,ide
              n=n+1
              data(n) = GRID(ng)%rmask(i,j)
           end do
        end do

        call mct_gGrid_importRAttr( dom_ocn, "frac", data, lsize )

        deallocate(idata, data, stat=ierr )
        if (ierr > 0) call ocn_abort( sigAbort, &
           '('//trim(subname)//') ERROR: failed to deallocate temporary workspace')

!EOC

    end subroutine ocn_domain_mct

!==============================================================================
!BOP
!   !ROUTINE: ocn_import_mct
!
!   !INTERFACE:
    subroutine ocn_import_mct(x2o, errorCode)

!   !DESCRIPTION:
!       This routine sets ocean surface fluxes received from coupler for ROMS
!
!       Fluxes used:
!
!       o pslv  -- sea-level pressure                   (Pa)
!       o lwdn  -- longwave radiation (down)            (W/m2)
!       o lwup  -- longwave radiation (up)              (W/m2)
!       o swnet -- net shortwave heat flux              (W/m2)
!       o lat   -- latent heat flux                     (W/m2)
!       o sen   -- sensible heat flux                   (W/m2)
!       o melth -- heat flux from snow & ice melt       (W/m2)
!       o salt  -- salt flux                            (kg(salt)/m2/s)
!       o snow  -- water flux due to snow               (kg/m2/s)
!       o rain  -- water flux due to rain               (kg/m2/s)
!       o evap  -- evaporation flux                     (kg/m2/s)
!       o meltw -- snow melt flux                       (kg/m2/s)
!       o roff  -- river runoff flux                    (kg/m2/s)
!       o ioff  -- ice runoff flux                      (kg/m2/s)
!       o taux  -- zonal wind stress                    (Pa)
!       o tauy  -- meridional wind stress               (Pa)
!
!       All quantities are considered positive downward
!       e.g.: net heat flux = swnet + lwdn + lwup + sen + lat
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:
        use mod_forces
        use mod_scalars,   only : isalt, itemp
        use shr_const_mod, only : SHR_CONST_CPSW,  &
                                  SHR_CONST_RHOFW, &
                                  SHR_CONST_RHOSW

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        type(mct_aVect), intent(inout) :: x2o

!   !OUTPUT PARAMETERS:

        integer        , intent(out)   :: errorCode       ! returned error code

!EOP
!BOC
        ! --- Local variables ---

        integer :: i, ierr, imax, j, jmax, n
        integer :: ibs, ibe, jbs, jbe, &
                   ids, ide, its, ite, &
                   jds, jde, jts, jte

        real(r8), dimension(:,:), allocatable :: taux, tauy

        real(r8), parameter :: conv_pair        = 0.01_r8,                                &
                               conv_heat_flux   = 1._r8/(SHR_CONST_RHOSW*SHR_CONST_CPSW), &
                               conv_water_flux  = -1._r8/SHR_CONST_RHOFW,                 &
                               conv_wind_stress = 0.5_r8/SHR_CONST_RHOSW

        character( len = * ), parameter :: subname = 'ocn_import_mct'

        ! --- Begin

        errorCode = ocn_Success        ! temporary

        ! --- Set surface fluxes for ROMS

        ! --- Get tile boundaries ---

        call ocn_tile_bounds( ng, ocn_tile,       &
                              ibs, ibe, jbs, jbe, &
                              ids, ide, jds, jde, &
                              its, ite, jts, jte )


        ! --- Initialize flux arrays ---

        ierr = 0
        allocate(taux(ibs:ibe,jbs:jbe), &
                 tauy(ibs:ibe,jbs:jbe), stat=ierr)
        if (ierr > 0) then
           errorCode = ocn_Failure
           call ocn_task_log( subname, 'ERROR: failed to allocate temporary workspace')
           return
        end if

        taux = 0.0_r8
        tauy = 0.0_r8

        FORCES(ng) % Pair (ibs:ibe,jbs:jbe)       = 0.0_r8
        FORCES(ng) % lrflx(ibs:ibe,jbs:jbe)       = 0.0_r8
        FORCES(ng) % srflx(ibs:ibe,jbs:jbe)       = 0.0_r8
        FORCES(ng) % lhflx(ibs:ibe,jbs:jbe)       = 0.0_r8
        FORCES(ng) % shflx(ibs:ibe,jbs:jbe)       = 0.0_r8
        FORCES(ng) % sustr(ibs:ibe,jbs:jbe)       = 0.0_r8
        FORCES(ng) % svstr(ibs:ibe,jbs:jbe)       = 0.0_r8
        FORCES(ng) % stflx(ibs:ibe,jbs:jbe,isalt) = 0.0_r8
        FORCES(ng) % stflx(ibs:ibe,jbs:jbe,itemp) = 0.0_r8


        ! --- Fields not yet used ---
        !
        !    x2o % rAttr(index_x2o_Foxx_melth, :)
        !    x2o % rAttr(index_x2o_Foxx_salt , :)
        !    x2o % rAttr(index_x2o_Foxx_snow , :)
        !    x2o % rAttr(index_x2o_Foxx_meltw, :)
        !    x2o % rAttr(index_x2o_Forr_roff , :)
        !    x2o % rAttr(index_x2o_Forr_ioff , :)


        n=0
        do j=jds,jde
           do i=ids,ide
              n=n+1

              ! --- surface air pressure (mb)
              FORCES(ng) % Pair (i,j) = conv_pair * x2o % rAttr(index_x2o_Sa_pslv, n)

              ! --- longwave net radiation flux (Celsius m/s)
              FORCES(ng) % lrflx(i,j) = conv_heat_flux * &
                                        (x2o % rAttr(index_x2o_Faxa_lwdn, n) + &
                                         x2o % rAttr(index_x2o_Foxx_lwup, n))

              ! --- shortwave net radiation flux (Celsius m/s)
              FORCES(ng) % srflx(i,j) = conv_heat_flux * x2o % rAttr(index_x2o_Foxx_swnet, n)

              ! --- latent net heat flux (Celsius m/s)
              FORCES(ng) % lhflx(i,j) = conv_heat_flux * x2o % rAttr(index_x2o_Foxx_lat, n)

              ! --- sensible net heat flux (Celsius m/s)
              FORCES(ng) % shflx(i,j) = conv_heat_flux * x2o % rAttr(index_x2o_Foxx_sen, n)

              ! --- surface salt flux (m/s), converted to psu m/s in set_vbc.F
              ! --- Note that fresh water flux is positive out of the ocean in ROMS
              ! --- Foxx_salt is the salt flux from melting ice (add later?)
              ! --- Foxx_prec is snow+rain (later?)
              ! --- Using evap-rain ---
              ! --- add prec(?)
              FORCES(ng) % rain (i,j) = x2o % rAttr(index_x2o_Faxa_rain, n)
              FORCES(ng) % stflx(i,j,isalt) = conv_water_flux * &
                                              (x2o % rAttr(index_x2o_Faxa_rain, n) + &
                                               x2o % rAttr(index_x2o_Foxx_evap, n))

              ! --- temperature kinematic surface flux (Celsius m/s)
              FORCES(ng) % stflx(i,j,itemp) = FORCES(ng) % srflx(i,j) + &
                                              FORCES(ng) % lrflx(i,j) + &
                                              FORCES(ng) % lhflx(i,j) + &
                                              FORCES(ng) % shflx(i,j)

              ! --- zonal & meridional wind stress (Pa)
              taux(i,j) = x2o % rAttr(index_x2o_Foxx_taux, n)
              tauy(i,j) = x2o % rAttr(index_x2o_Foxx_tauy, n)

           end do
        end do

        ! --- rotate wind stress from coupler to ROMS grid, if necessary ---
        call roms_rotate_vector(ng, ibs, ibe, jbs, jbe, taux, tauy, rot = -1.0_r8)

        ! --- performs halo update on rho grid points for wind stress ---
        call roms_halo_update_r2d(ng, ocn_tile,       &
                                  ibs, ibe, jbs, jbe, &
                                  taux, tauy)
        if (ROMS_exit_flag /= ROMS_noerror) then
           errorCode = ocn_Failure
           call ocn_task_log( subname, 'Error in wind stress halo update')
           return
        end if

        do j=jds,jde
           do i=its,ide
              FORCES(ng) % sustr(i,j) = conv_wind_stress * (taux(i-1,j)+taux(i,j))
           end do
        end do

        do j=jts,jde
           do i=ids,ide
              FORCES(ng) % svstr(i,j) = conv_wind_stress * (tauy(i,j-1)+tauy(i,j))
           end do
        end do

        deallocate(taux, tauy, stat=ierr)
        if (ierr > 0) then
           errorCode = ocn_Failure
           call ocn_task_log( subname, 'ERROR: failed to deallocate temporary workspace')
           return
        end if


        ! --- Update halo for all fluxes ---
        call roms_halo_update_fluxes(ng, ocn_tile,       &
                                     ibs, ibe, jbs, jbe, &
                                     FORCES(ng) % Pair , &
                                     FORCES(ng) % lrflx, &
                                     FORCES(ng) % srflx, &
                                     FORCES(ng) % lhflx, &
                                     FORCES(ng) % shflx, &
                                     FORCES(ng) % stflx, &
                                     FORCES(ng) % rain , &
                                     FORCES(ng) % sustr, &
                                     FORCES(ng) % svstr)

        if (ROMS_exit_flag /= ROMS_noerror) then
           call ocn_task_log( subname, 'Error in flux halo update')
           errorCode = ocn_Failure
           return
        end if

        ! --- Diagnostic
        if (diag_level > 0) call ocn_log(subname, ': received fluxes from coupler')
!EOC

    end subroutine ocn_import_mct


!==============================================================================
!BOP
!   !ROUTINE: ocn_export_mct
!
    subroutine ocn_export_mct(o2x, errorCode)

!   !DESCRIPTION:
!       This routine sends ROMS fields to the CPL7 coupler
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

        use mod_ocean,     only : OCEAN
        use mod_scalars,   only : itemp
        use mod_param,     only : N
        use mod_stepping,  only : nrhs
        use shr_const_mod, only : t0_kelvin => SHR_CONST_TKFRZ  ! T0 Kelvin

!
!   !INPUT PARAMETERS:

        type(mct_aVect), intent(inout) :: o2x

!   !OUTPUT PARAMETERS:

        integer        , intent(out)   :: errorCode       ! returned error code

!EOP
!BOC
        ! --- Local variables ---

        integer :: i, ierr, j, k, lu, lv
        integer :: ibs, ibe, jbs, jbe, &
                   ids, ide, jds, jde, &
                   its, ite, jts, jte, &
                   iue, iuw, jvn, jvs

        real(r8), dimension(:,:), allocatable :: u, v

        character( len = * ), parameter :: subname = 'ocn_export_mct'

        ! --- Begin

        errorCode = ocn_Success

        call ocn_tile_bounds( ng, ocn_tile,       &
                              ibs, ibe, jbs, jbe, &
                              ids, ide, jds, jde, &
                              its, ite, jts, jte )

        ! --- Surface temperature ---

        k=0
        do j=jds,jde
           do i=ids,ide
              k=k+1
              o2x % rAttr(index_o2x_So_t,k) = OCEAN(ng) % t(i,j,N(ng),nrhs(ng),itemp) &
                                            + t0_kelvin
           end do
        end do

        ! --- Surface zonal, meridional velocities ---

        ! --- allocate temporary workspace for u, v ---

        ierr = 0
        allocate(u(ids:ide,jds:jde), &
                 v(ids:ide,jds:jde), stat=ierr)
        if (ierr > 0) then
           errorCode = ocn_Failure
           call ocn_task_log( subname, 'ERROR: failed to allocate temporary workspace')
           return
        end if

        u = 0.0_r8
        v = 0.0_r8

        lu = nx_global-2
        lv = ny_global-2

        do j=jds,jde
           jvs = max(j,1)
           jvn = min(lv,j)+1
           do i=ids,ide
              iuw = max(i,1)
              iue = min(lu,i)+1
              u(i,j) = 0.5_r8 * (OCEAN(ng) % u(iuw,j,N(ng),2) + &
                                 OCEAN(ng) % u(iue,j,N(ng),2))
              v(i,j) = 0.5_r8 * (OCEAN(ng) % v(i,jvs,N(ng),2) + &
                                 OCEAN(ng) % v(i,jvn,N(ng),2))
           end do
        end do

        ! --- rotate ocean surface velocities to coupler grid, if necessary ---
        call roms_rotate_vector(ng, ids, ide, jds, jde, u, v, rot = +1.0_r8)

        ! --- exports ocean surface velocities ---

        k=0
        do j=jds,jde
           do i=ids,ide
              k=k+1
              o2x % rAttr(index_o2x_So_u,k) = u(i,j)
              o2x % rAttr(index_o2x_So_v,k) = v(i,j)
           end do
        end do

        ierr = 0
        deallocate(u, v, stat=ierr)
        if (ierr > 0) then
           errorCode = ocn_Failure
           call ocn_task_log( subname, 'ERROR: failed to deallocate temporary workspace')
           return
        end if

        ! --- Diagnostic ---
        if (diag_level > 0 ) call ocn_log(subname, ': sent fluxes to coupler')

!EOC

    end subroutine ocn_export_mct

!
end module rocn_comp_mct
