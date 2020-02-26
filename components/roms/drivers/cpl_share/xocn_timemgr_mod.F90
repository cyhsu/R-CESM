module xocn_timemgr_mod

    use ESMF
    use shr_sys_mod, only : shr_sys_abort

    implicit none

    private

    public :: xocn_timemgr_timeString_len
    public :: xocn_timemgr_getClock

    integer, parameter :: xocn_timemgr_timeString_len = 19

contains

    subroutine xocn_timemgr_getClock( EClock, timeString )

        implicit none

        type( ESMF_Clock ), intent(in) :: EClock
        character( len = xocn_timemgr_timeString_len ), optional, intent(out) :: timeString

        integer :: rc
        type( ESMF_Time ) :: currTime
        character( len = * ), parameter :: subName = '(xocn_timemgr_getClock)'

        if ( present( timeString ) ) then
            call ESMF_ClockGet( EClock, currTime=currTime, rc=rc )
            if ( rc /= ESMF_SUCCESS ) call shr_sys_abort( subName // ':: ERROR retrieving clock' )
            call ESMF_TimeGet( currTime, timeString=timeString, rc=rc )
            if ( rc /= ESMF_SUCCESS ) call shr_sys_abort( subName // ':: ERROR retrieving start time' )
        end if

    end subroutine xocn_timemgr_getClock

end module xocn_timemgr_mod
