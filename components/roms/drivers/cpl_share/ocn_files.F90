#include "cppdefs.h"
!BOP
!   !MODULE: ocn_files
!   !INTERFACE:

module ocn_files

!   !DESCRIPTION:
!       Includes routines for generating/handling history and restart files according to CESM/CPL7 rules
!
!   !REVISION HISTORY:
!       Jun 05, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

    use shr_kind_mod
    use shr_file_mod
    use shr_mpi_mod,   only : shr_mpi_bcast

    use mod_parallel,  only : OCN_COMM_WORLD,         &
                              inp_task  => InpThread, &
                              log_task  => OutThread
    use mod_param,     only : iNLM
    use mod_iounits,   only : HIS, INI, RST
    use mod_scalars,   only : nrrec,     &
                              exit_flag, &
                              NoError

    use ocn_io_tools
    use ocn_instance
    use ocn_utils,     only : ocn_abort, sigAbort
    use ocn_timemgr_mod

    implicit none

    private

!
!   !PUBLIC MEMBER FUNCTIONS:

    public :: ocn_history_outfile_set, &
              ocn_restart_outfile_set, &
              ocn_restart_infile_set,  &
              ocn_pfile_set,           &
              ocn_restart_write

!   !PUBLIC DATA:
!
!
!EOP

!   !PRIVATE MODULE FUNCTIONS:

    character(shr_kind_cl) :: ocn_pfile_name

    character( len = * ), parameter :: restart_pfile = 'rpointer.ocn'

contains

    subroutine ocn_history_outfile_set( ng, case_id )

        integer,                intent(in) :: ng
        character(shr_kind_cs), intent(in) :: case_id

        HIS( ng ) % base = trim(case_id) // '.ocn' // trim(inst_suffix) // '.hi'

    end subroutine ocn_history_outfile_set


    subroutine ocn_restart_outfile_set( ng, case_id )

        integer,                intent(in)    :: ng
        character(shr_kind_cs), intent(in) :: case_id

        character(shr_kind_cs) :: time_str

        call ocn_timemgr_getClock( timeString = time_str )
        RST( ng ) % name = trim(case_id) // '.ocn' // trim(inst_suffix) // '.r.' // trim(time_str) // '.nc'

    end subroutine ocn_restart_outfile_set


    subroutine ocn_restart_infile_set( ng )

        integer, intent(in) :: ng

        integer :: rc
        character(shr_kind_cl) :: rst_inname

        character( len = * ), parameter :: subname = 'ocn_restart_infile_set'

        call ocn_pfile_read( rst_inname, rc )
        if ( rc /= 0 ) then
            call ocn_log  ( subname, 'Error reading restart file name in '//trim(ocn_pfile_name) )
            call ocn_abort( sigAbort,'Error reading restart file name in '//trim(ocn_pfile_name) )
        end if

        INI(ng) % name = trim(rst_inname)

    end subroutine ocn_restart_infile_set


    subroutine ocn_pfile_set( ocnid, name )

        integer,                        intent(in) :: ocnid
        character( len = * ), optional, intent(in) :: name

        if ( present(name) ) then
            ocn_pfile_name = trim(name) // trim(inst_suffix)
        else
            ocn_pfile_name = trim(restart_pfile) // trim(inst_suffix)
        end if

    end subroutine ocn_pfile_set


    subroutine ocn_pfile_read( rstname, rc )

        character(shr_kind_cl), intent(out) :: rstname
        integer,                intent(out) :: rc

        integer :: pfunit

        character( len = * ), parameter :: subname = 'ocn_pfile_read'

        rc = 0
        if ( inp_task ) then
            pfunit = shr_file_getUnit()
            open( unit = pfunit, file = trim(ocn_pfile_name), form = 'formatted', action = 'read', iostat = rc )
            if ( rc == 0 ) then
                rewind ( unit = pfunit )
                read ( unit = pfunit, fmt = '(a)', iostat = rc ) rstname
                if ( rc /= 0 ) call ocn_log( subname, 'Error reading from file '//trim(ocn_pfile_name) )
                close( unit = pfunit )
            else
                call ocn_log( subname, 'Error opening file '//trim(ocn_pfile_name) )
            end if
            call shr_file_freeUnit( pfunit )
        end if

        call shr_mpi_bcast( rc, OCN_COMM_WORLD )
        if ( rc /= 0 ) call shr_mpi_bcast( rstname, OCN_COMM_WORLD )

    end subroutine ocn_pfile_read

    subroutine ocn_pfile_write( rstname )

        character( len = * ), intent(in) :: rstname

        integer :: pfunit, rc

        character( len = * ), parameter :: subname = 'ocn_pfile_write'

        rc = 0
        if ( log_task ) then
            pfunit = shr_file_getUnit()
            open( unit = pfunit, file = trim(ocn_pfile_name), form = 'formatted', status = 'unknown', iostat = rc )
            if ( rc == 0 ) then
                rewind ( unit = pfunit )
                write ( unit = pfunit, fmt = '(a)', iostat = rc ) trim(rstname)
                if ( rc /= 0 ) call ocn_log( subname, 'Error writing to file '//trim(ocn_pfile_name) )
                close( unit = pfunit )
            else
                call ocn_log( subname, 'Error opening file '//trim(ocn_pfile_name) )
            end if
            call shr_file_freeUnit( pfunit )
        end if

    end subroutine ocn_pfile_write

    subroutine ocn_restart_write( ng, case_id )

        integer,                intent(in) :: ng
        character(shr_kind_cs), intent(in) :: case_id

        character( len = * ), parameter :: subname = 'ocn_restart_write'

        ! --- Begin
# ifdef PROFILE
        call wclock_on (ng, iNLM, 8)
# endif

        ! --- Create restart file name
        call ocn_restart_outfile_set( ng, case_id )

!-----------------------------------------------------------------------
!  If appropriate, process restart NetCDF file.
!-----------------------------------------------------------------------
!
!  Create output restart NetCDF file or prepare existing file to
!  append new data to it.
!
        call def_rst( ng )
        if ( exit_flag .ne. NoError ) return
!
!  Write out data into restart NetCDF file.
!
        call wrt_rst( ng )
        if ( exit_flag .ne. NoError ) return

!  Write restart filename to pointer file
        call ocn_pfile_write( trim(RST(ng) % name) )

# ifdef PROFILE
        call wclock_off (ng, iNLM, 8)
# endif

    end subroutine ocn_restart_write

end module ocn_files
