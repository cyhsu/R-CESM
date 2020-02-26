!BOP
!   !MODULE: ocn_utils
!   !INTERFACE:

module ocn_utils

!   !DESCRIPTION:
!   Auxiliary routines for ocean model driver
!
!   !REVISION HISTORY:
!   Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

    use ocean_control_mod, only : ROMS_finalize
    use mod_iounits,       only : stdout
    use mod_parallel,      only : OutThread
    use shr_sys_mod,       only : shr_sys_flush, &
                                  shr_sys_abort

    implicit none

    private

!   !PRIVATE DATA:

    integer, parameter ::  &
             sigAbort = -1, &
             sigExit  = 0

    integer, parameter :: msg_len = 1024


!   !PUBLIC MEMBER DATA:

    public :: sigAbort, sigExit

!   !PUBLIC MEMBER FUNCTIONS:

    public :: ocn_abort

contains

!==============================================================================
!BOP
!   !ROUTINE: ocn_abort
!   !INTERFACE:

    subroutine ocn_abort( exit_signal, exit_message, out_unit )

!   !DESCRIPTION:
!       Aborts ROMS properly
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        integer,            intent(in) :: exit_signal
        character(len = *), intent(in) :: exit_message
        integer,  optional, intent(in) :: out_unit

!EOP
!BOC

        ! --- Local constants & variables ---
        integer :: o_unit
        character(len = msg_len) :: msg

        ! --- Begin ---

        if (present(out_unit)) then
           o_unit = out_unit
        else
           o_unit = stdout
        end if

        select case(exit_signal)
           case(sigExit)
              write(msg,'(a)') 'ROMS exiting...'
           case(sigAbort)
              write(msg,'(a)') 'ROMS aborting...'
           case default
              write(msg,'(a)') 'ROMS exiting with unknown signal...'
        end select

        write(o_unit,'(/72("-")/)')
        write(o_unit,'(a)') trim(exit_message)
        write(o_unit,'(a)') trim(msg)
        write(o_unit,'(/72("=")/)')

        call shr_sys_flush(o_unit)

        ! --- Finalize ROMS ---
        call ROMS_finalize

        ! --- Stops the entire MPI environment consistently ---
        call shr_sys_abort(msg)

!EOC
    end subroutine ocn_abort

end module ocn_utils
