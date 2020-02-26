!BOP
!   !MODULE: ocn_timemgr_mod
!
!   !INTERFACE:

module ocn_timemgr_mod

!   !DESCRIPTION:
!   This module contains time management routines for ROMS' internal clock
!
!   !REVISION HISTORY:
!   Apr 19, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial version
!   Apr 09, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - upgraded ocn_timemgr_setClock
!
!   !USES:

    use ESMF

    use ocn_io_tools
    use ocn_utils,    only : ocn_abort, sigAbort

    use mod_parallel, only : inp_task => InpThread
    use mod_scalars,  only : iic, ntstart, dt
    use diag_mod,     only : date_time

    use shr_mpi_mod,  only : shr_mpi_bcast

    use shr_file_mod, only : shr_file_getUnit,  &
                             shr_file_freeUnit

    implicit none

!   !PUBLIC MEMBER FUNCTIONS:

    ! --- Public interface ---

    public :: ocn_timemgr_setClock
    public :: ocn_timemgr_getClock
    public :: ocn_timemgr_InitClock
    public :: ocn_timemgr_advanceClock
    public :: ocn_timemgr_CheckInSyncClock
    public :: ocn_timemgr_CheckRC

!   !PUBLIC DATA:

    public :: date_str_len
    public :: o_Clock
!
!
!EOP

    private

    integer, parameter :: str_len      = 1024
    integer, parameter :: date_str_len = 19

    integer, parameter :: ng = 1 ! only grid #1

    type(ESMF_Clock),    save :: o_Clock
    type(ESMF_Calendar), save :: esmf_cal


    character( len = 9 ) :: calendar

    integer :: cpl_dt_s

    integer ::              &
             start_year,    &
             start_month,   &
             start_day,     &
             start_hour,    &
             start_minute,  &
             start_second,  &
             start_ymd,     &
             start_tod

    namelist /ocn_timemgr/ &
             calendar,      &
             start_year,    &
             start_month,   &
             start_day,     &
             start_hour,    &
             start_minute,  &
             start_second,  &
             start_ymd,     &
             start_tod


contains

!==============================================================================
!BOP
!   !ROUTINE: ocn_timemgr_setClock
!
!   !INTERFACE:

    subroutine ocn_timemgr_setClock(year, month, day, hour, minute, second, ymd, tod, step, calkind)

!   !DESCRIPTION:
!       Set ocean model clock
!
!   !REVISION HISTORY:
!       Apr 19, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial version
!       Apr 09, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - upgraded ESMF interface
!                      according to T. Craig's changes
!
        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        integer,             optional, intent(in) :: year, month, day, hour, minute, second, ymd, tod, step

        character (len = *), optional, intent(in) :: calkind

!EOP
!BOC
        ! --- Local variables ---

        integer :: rc
        integer :: yy, mm, dd, s

        type(ESMF_CalKind_Flag) :: esmf_calkind
        type(ESMF_Time)         :: StartTime, CurrTime, RefTime, StopTime
        type(ESMF_TimeInterval) :: TimeStep

        character (len = *), parameter :: subname = 'ocn_timemgr_setClock'

        ! --- Begin

        s = 0
        if (present(tod)) then
           s=tod
        else
           if (present(hour)  ) s =   3600 * hour
           if (present(minute)) s = s + 60 * minute
           if (present(second)) s = s +      second
        end if

        if (present(ymd)) then
           yy = ymd / 10000
           mm = (ymd - 10000 * yy) / 100
           dd = mod(ymd, 100)
        else if (present(year) .and. present(month) .and. present(day)) then
           yy = year
           mm = month
           dd = day
        else
           call ocn_abort(sigAbort, 'ocn_setClock ERROR: no year, month, day specified')
        end if

        if (present(calkind)) then
           select case (trim(calkind))
              case('NO_LEAP')
                 esmf_calkind = ESMF_CALKIND_NOLEAP
              case('GREGORIAN')
                 esmf_calkind = ESMF_CALKIND_GREGORIAN
              case default
                 call ocn_abort(sigAbort, 'ocn_setClock ERROR: calendar type '//trim(calkind)//' not recognized')
           end select
        else
           esmf_calkind = ESMF_CALKIND_NOLEAP
        end if

        esmf_cal = ESMF_CalendarCreate( name="roms_calendar", calkindflag=esmf_calkind, rc=rc )
        call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR setting calendar type')

        call ESMF_TimeSet( StartTime, yy=yy, mm=mm, dd=dd, s=s, calendar=esmf_cal, rc=rc )
        call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR setting start time')

        call ESMF_TimeSet( RefTime , yy=yy, mm=mm, dd=dd, s=s, calendar=esmf_cal, rc=rc )
        call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR setting reference time')

        call ESMF_TimeSet( CurrTime , yy=yy, mm=mm, dd=dd, s=s, calendar=esmf_cal, rc=rc )
        call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR setting current time')

        call ESMF_TimeIntervalSet( TimeStep, s=step, rc=rc)
        call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR setting internal timestep')

        ! ------ Create ESMF Clock with input characteristics -------------------
        ! --- NOTE: StopTime is required in interface but not used, so set  -----
        ! --- to something arbitrary.                                       -----

        call ESMF_TimeSet(StopTime, yy=9999, mm=01, dd=01, s=0, calendar=esmf_cal, rc=rc)
        call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR setting stop time')

        o_Clock = ESMF_ClockCreate(name='roms_clock', TimeStep=TimeStep, startTime=StartTime, &
                                   StopTime=StopTime, RefTime=RefTime, rc=rc)
        call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR setting clock create')

        call ESMF_ClockSet( o_Clock, CurrTime=CurrTime, rc=rc )
        call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR setting clock current time')

!EOC
    end subroutine ocn_timemgr_setClock

!==============================================================================
!BOP
!   !ROUTINE: ocn_timemgr_getClock
!
!   !INTERFACE:

    subroutine ocn_timemgr_getClock(dtime, timeString, currTime, startTime, start_ymd, start_tod)

!   !DESCRIPTION:
!       Get ocean model time
!
!   !REVISION HISTORY:
!       Apr 19, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial version
!       Jun 05, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - added curr & start times, error check
!
        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        integer,            optional, intent(out) :: dtime
        character(len = *), optional, intent(out) :: timeString
        type(ESMF_Time),    optional, intent(out) :: startTime, currTime
        integer,            optional, intent(out) :: start_ymd, start_tod

!EOP
!BOC
        ! --- Local variables

        integer                 :: rc, year, month, day, sec, ymd, tod
        type(ESMF_Time)         :: sTime
        type(ESMF_TimeInterval) :: TimeStep
        character (len = *), parameter :: subname = 'ocn_timemgr_getClock'

        ! --- Begin

        rc = 0
        if (present(dtime)) then
            call ESMF_ClockGet( o_Clock, TimeStep=TimeStep, rc=rc )
            call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR getting time step interval')
            call ESMF_TimeIntervalGet( TimeStep, s=dtime, rc=rc )
            call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR getting time step')
        end if

        if (present(timeString)) then
            call ESMF_TimeGet( o_Clock%clockint%CurrTime, timeString=timeString, rc=rc )
            call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR getting current time string')
        end if

        if (present(currTime)) then
            call ESMF_ClockGet( o_Clock, currTime=currTime, rc=rc )
            call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR getting current time')
        end if

        if (present(startTime) .or. present(start_ymd) .or. present(start_tod)) then
            call ESMF_ClockGet( o_Clock, startTime=sTime, rc=rc )
            call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR getting start time')
            if (present(startTime)) startTime = sTime
        end if

        if (present(start_ymd) .or. present(start_tod)) then
            call ESMF_TimeGet( sTime, yy=year, mm=month, dd=day, s=sec, rc=rc )
            call ocn_timemgr_CheckRC( rc, callsub_name=subname, msg='ERROR getting start ymd/tod')
            if (present(start_ymd)) start_ymd = 10000 * year + 100 * month + day
            if (present(start_tod)) start_tod = sec
        end if

!EOC
    end subroutine ocn_timemgr_getClock

!==============================================================================
!BOP
!   !ROUTINE: ocn_timemgr_InitClock
!
!   !INTERFACE:

    subroutine ocn_timemgr_InitClock(mpicom, nmlfile)

!   !DESCRIPTION:
!       Initialize ocean model clock
!
!   !REVISION HISTORY:
!       Apr 19, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial version
!
        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        integer,                       intent(in) :: mpicom
        character (len = *), optional, intent(in) :: nmlfile

!EOP
!BOC
        ! --- Local variables ---

        integer :: yy, mm, dd, s
        integer :: ierr, iunit

        character (len = str_len) :: nml_inpfile

        character (len = *), parameter :: subname = 'ocn_timemgr_InitClock'

        ! --- Begin

        calendar = ""
        start_year   = 0
        start_month  = 0
        start_day    = 0
        start_hour   = 0
        start_minute = 0
        start_second = 0
        start_ymd    = 0
        start_tod    = 0

        if (present(nmlfile)) then
           nml_inpfile = nmlfile
        else
           nml_inpfile = 'ocn_in'
        end if

        ! --- Read time parameters on input task ---

        if (inp_task) then

           ! --- Check if namelist file exists ---
           iunit = shr_file_getUnit()
           open(unit = iunit, file = trim(nml_inpfile), status = 'old')
           rewind(unit = iunit)
           read(unit = iunit, nml = ocn_timemgr, iostat = ierr)
           close(unit = iunit)
           call shr_file_freeUnit( iunit )

        end if

        call shr_mpi_bcast(ierr, mpicom)

        if (ierr /= 0) call ocn_abort(sigAbort, &
              '('//trim(subname) // &
              ') ERROR reading namelist ocn_timemgr from '//trim(nml_inpfile))

        if (inp_task) then

           if (start_ymd > 0) then
              yy = start_ymd / 10000
              mm = (start_ymd - 10000 * yy) / 100
              dd = mod(start_ymd, 100)
           else if (start_year > 0 .and. start_month > 0 .and. start_day > 0) then
              yy = start_year
              mm = start_month
              dd = start_day
           else
              ierr = -1
           end if

        end if

        call shr_mpi_bcast(ierr, mpicom)

        if (ierr /= 0) call ocn_abort(sigAbort, &
                   '('//trim(subname) // &
                   ') ERROR reading year, month, day from '//trim(nml_inpfile))

        if (inp_task) then

           if (start_tod > 0) then
              s = start_tod
           else
              s = 3600 * start_hour + 60 * start_minute + start_second
           end if

        end if

        ! --- Broadcast time parameters ---

        call shr_mpi_bcast(yy,       mpicom)
        call shr_mpi_bcast(mm,       mpicom)
        call shr_mpi_bcast(dd,       mpicom)
        call shr_mpi_bcast(s,        mpicom)
        call shr_mpi_bcast(calendar, mpicom)

        ! --- Warning: internal clock will be advanced by INT(dt), not dt as originally in ROMS

        call ocn_timemgr_SetClock(year=yy, month=mm, day=dd, tod=s, step=int(dt(ng)), calkind=calendar)

!EOC
    end subroutine ocn_timemgr_InitClock

!==============================================================================
!BOP
!   !ROUTINE: ocn_timemgr_advanceClock
!
!   !INTERFACE:

    subroutine ocn_timemgr_advanceClock

!   !DESCRIPTION:
!       Advance ocean model clock by one preset timestep
!
!   !REVISION HISTORY:
!       Apr 19, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial version
!
        implicit none

!EOP
!BOC
        ! --- Begin

        if (iic(ng) > ntstart(ng)) then
           o_Clock % clockint % CurrTime = o_Clock % clockint % CurrTime &
                                         + o_Clock % clockint % TimeStep

           o_Clock % clockint % AdvanceCount = o_Clock % clockint % AdvanceCount + 1
        end if

        call ESMF_TimeGet( o_Clock%clockint%CurrTime, timeString=date_time)

!EOC
    end subroutine ocn_timemgr_advanceClock

!==============================================================================
!BOP
!   !ROUTINE: ocn_timemgr_checkInSyncClock
!
!   !INTERFACE:

    subroutine ocn_timemgr_checkInSyncClock(SyncClock, name_SyncClock)

!   !DESCRIPTION:
!       Check if ocean clock is in sync with external clock
!
!   !REVISION HISTORY:
!       Apr 19, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial version
!
        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        type(ESMF_Clock),              intent(in) :: SyncClock
        character (len = *), optional, intent(in) :: name_SyncClock

!EOP
!BOC

        ! --- Local variables ---

        integer :: rc
        type(ESMF_Time) :: SyncTime, ocn_Time
        character (len = str_len) :: timestr, clockname

        character (len = *), parameter :: subname = 'ocn_timemgr_checkInSyncClock'

        ! --- Begin

        if (present(name_SyncClock)) then
           clockname = trim(name_SyncClock)
        else
           clockname = 'reference'
        end if

        call ESMF_ClockGet(SyncClock,  CurrTime=SyncTime, rc=rc)
        call ocn_timemgr_CheckRC(rc, clock_name=clockname, callsub_name=subname)

        call ESMF_ClockGet(o_Clock, CurrTime=ocn_Time, rc=rc)
        call ocn_timemgr_CheckRC(rc, clock_name='ocean', callsub_name=subname)

        if (ocn_Time /= SyncTime) then
           timestr = ''
           call ESMF_TimeGet( SyncTime, timeString=timestr )
           call ocn_log ('ocn_run_mct', trim(clockname) // ' current time', trim(timestr))
           timestr = ''
           call ESMF_TimeGet( ocn_Time, timeString=timestr )
           call ocn_log ('ocn_run_mct', 'ocean current time', trim(timestr))
           call ocn_abort(sigAbort, trim(subname) // &
                ' ERROR: ocean clock not in sync with ' // trim(clockname) // ' clock')
        end if

!EOC
    end subroutine ocn_timemgr_checkInSyncClock

!==============================================================================
!BOP
!   !ROUTINE: ocn_timemgr_CheckRC
!
!   !INTERFACE:

    subroutine ocn_timemgr_CheckRC(rc, clock_name, callsub_name, msg)

!   !DESCRIPTION:
!       Check return code from ESMF routine
!
!   !REVISION HISTORY:
!       Apr 19, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial version
!
        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        integer,                      intent(in) :: rc
        character(len = *), optional, intent(in) :: clock_name, callsub_name, msg

!EOP
!BOC
        character(len = *), parameter :: subname = 'ocn_timemgr_CheckRC'

        character(len = str_len) :: message


        if (rc /= ESMF_SUCCESS) then

           if (present(callsub_name)) write(message,'("(",a,")")') trim(callsub_name)

           if (present(msg)) then
              message = trim(message) // msg

           else

              message = ' ERROR: cannot get'

              if (present(clock_name)) &
                  message = trim(message) // ' ' // trim(clock_name)

              message = trim(message) // ' current time'

           end if

           call ocn_abort(sigAbort, message)

        end if

!EOC
    end subroutine ocn_timemgr_CheckRC

end module ocn_timemgr_mod
