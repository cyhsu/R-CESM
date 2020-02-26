!
!        in WRF:
!        itimestep = 0
!        do while (isstoptime)
!
!            write history and restart
!            read bdy
!            solve_domain:
!               itimestep = itimestep + 1
!               first_rk_step_part1:
!                   radiation_driver( itimestep )
!               ...
!            ...
!            advance_domain_clock by dt
!        done
!        write last I/O: history, restart
!
!        so itimestep is ahead of clock by dt when radiation_driver
!        checks if sw needs to be computed:
!
!        curr_time:    0  t 2t 3t          nt
!                      +--+--+--+--+-   -+--+
!        itimestep:     1  2  3       n-1 n

module atm_radiation_mod

    use shr_kind_mod,  only : r8 => SHR_KIND_R8
    use shr_const_mod, only : SHR_CONST_STEBOL
    use module_domain, only : domain,                  &
                              get_ijk_from_grid,       &
                              domain_get_current_time, &
                              domain_get_stop_time,    &
                              domain_get_time_step

    use seq_timemgr_mod
    use module_utility
    use atm_io_tools
    use shr_sys_mod

    implicit none

    private

    public :: atm_sw_calday_get, &
              atm_lw_get

contains

    real(r8) function atm_sw_calday_get( grid, nextTimeInterval, now )

    ! --- Returns Greenwich calendar day (1.00 -> 365.99) of
    ! --- next shortwave calculation within the nextTimeInterval (if present)
    ! --- or until integration stops for input grid.
    ! --- If no sw calculation is found, returns -1._r8
    ! --- NOTES:
    !       a) shr_orb needs calday starting from 1.0 (Jan 1, 0Z)
    !       b) shr_orb_decl needs calday within the 1.xx-365.xx range
    !       c) This function only works after a call to phy_init() in start_domain
    !
        type (domain),           intent(in) :: grid
        integer,       optional, intent(in) :: nextTimeInterval
        logical,       optional, intent(in) :: now

        logical  :: dosw
        integer  :: itimestep, ra_sw_physics, rc
        integer  :: ymd, tod
        real(r8) :: calday, dayspy
        type (WRFU_Time)         :: curr_time, next_time, stop_time
        type (WRFU_TimeInterval) :: time_step, time_intv

        character (len = *), parameter :: subname = '(atm_sw_calday_get)'

        atm_sw_calday_get = -1._r8
        dosw = .false.

        ! --- Check if sw radiation is computed
        call nl_get_ra_sw_physics ( grid % id , ra_sw_physics )
        if (ra_sw_physics == 0) return

        curr_time = domain_get_current_time( grid )
        stop_time = domain_get_stop_time( grid )
        time_step = domain_get_time_step( grid )

        if (present(now)) then
            dosw = now
        else if (present(nextTimeInterval)) then
            call WRFU_TimeIntervalSet( time_intv, s=nextTimeInterval, rc=rc )
            if ( rc /= WRFU_SUCCESS ) then
                call atm_log( subname, 'Error in WRFU_TimeIntervalSet' )
                call shr_sys_abort()
            end if
            next_time = curr_time + time_intv
            if ( next_time < stop_time ) stop_time = next_time
        end if

        itimestep = grid % itimestep
        do while ( curr_time < stop_time )
            itimestep = itimestep + 1
            dosw = do_sw_calc( grid, itimestep )
            if ( dosw ) exit
            curr_time = curr_time + time_step
        end do

        if (dosw) then
            call WRFU_TimeGet( curr_time, dayOfYear_r8=calday, rc=rc )
            if ( rc /= WRFU_SUCCESS ) then
                call atm_log( subname, 'Error in WRFU_TimeGet' )
                call shr_sys_abort()
            end if
            ! --- NOTE: Gregorian calendar is incompatible with shr_orb_decl,
            ! --- which assumes calday to be within 1.xx and 365.xx
            ! --- We fake day 366 reusing day 365
            ! --- Based on hack by Dani Bundy-Coleman and Erik Kluzek Aug/2008 (CLM)
            dayspy = 366._r8
!           if (defaultCal % Type % caltype == WRFU_CALKIND_GREGORIAN % caltype) dayspy = dayspy + 1
            if (curr_time % Calendar % Type % caltype == WRFU_CALKIND_GREGORIAN % caltype) dayspy = dayspy + 1
            if ((calday < 1._r8) .or. (calday >= dayspy)) then
                call atm_log( subname, 'Error in WRFU_TimeGet: calday out of bounds' )
                call shr_sys_abort()
            else if (calday >= 366._r8) then
                calday = calday - 1._r8
            end if
            atm_sw_calday_get = calday
        end if

    end function atm_sw_calday_get


    logical function do_sw_calc( grid, itimestep )

        integer,       intent(in) :: itimestep
        type (domain), intent(in) :: grid

        logical :: dosw
        integer :: ra_call_offset

        dosw = ( itimestep == 1 )
        if ( .not.dosw ) dosw = ( grid % radt == 0.) .or. ( grid % stepra .eq. 1 )
        if ( .not.dosw ) then
           dosw = mod( itimestep, grid % stepra ) .eq. (1 + grid % ra_call_offset)
        end if

        do_sw_calc = dosw

    end function do_sw_calc


    subroutine atm_lw_get( grid )

        type (domain), intent(inout) :: grid

        integer :: ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe

        integer :: i, j

        character(len = *), parameter :: subname = '(atm_lw_get)'

        ! --- Begin

        call get_ijk_from_grid( grid,                         &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe )

        ipe = min(ipe, ide-1)
        jpe = min(jpe, jde-1)

        do j = jps, jpe
           do i = ips, ipe
              if ( grid % xland(i,j) >= 1.5 ) then
                 grid % lwups(i,j) = SHR_CONST_STEBOL  &
                                   * grid % emiss(i,j) &
                                   * grid % tsk(i,j) * grid % tsk(i,j) &
                                   * grid % tsk(i,j) * grid % tsk(i,j)
              end if
           end do
        end do

    end subroutine atm_lw_get


end module atm_radiation_mod
