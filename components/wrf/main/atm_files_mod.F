module atm_files_mod

    use shr_kind_mod
    use shr_file_mod
    use shr_sys_mod,    only : shr_sys_abort

    use atm_instance,   only : inst_suffix
    use atm_io_tools

    use module_domain,  only : domain,                &
                               domain_clock_get,      &
                               domain_get_start_time, &
                               domain_get_stop_time
    use module_io_domain
    use module_configure
    use module_timing
    use module_utility
    use module_wrf_top, only : alloc_doms_for_dfi

    implicit none

    private

    public :: atm_history_outfile_set, &
              atm_restart_outfile_set, &
              atm_restart_infile_set,  &
              atm_restart_write,       &
              atm_last_write,          &
              atm_init_read

    character(len = shr_kind_cl) :: atm_pfile_name

    integer, parameter :: io_form_netcdf  = 2, &
                          io_form_pnetcdf = 11
    integer, parameter :: io_form_default = io_form_netcdf  ! NetCDF serial

    character( len = * ), parameter :: restart_pfile = 'rpointer.atm'

contains

    subroutine atm_history_outfile_set( grid_id, case_id )

        integer,                      intent(in) :: grid_id
        character(len = shr_kind_cs), intent(in) :: case_id

        integer                :: io_form
        character(len = shr_kind_cl) :: file_name

        file_name = trim(case_id) // '.atm' // trim(inst_suffix) // '.hi.<date>.nc'
        call nl_set_history_outname ( grid_id, file_name )

        call nl_get_io_form_history ( grid_id, io_form )
        if ( io_form /= io_form_netcdf .and. io_form /= io_form_pnetcdf ) then
            ! Set file format to default
            call nl_set_io_form_history ( grid_id, io_form_default )
        end if

    end subroutine atm_history_outfile_set


    subroutine atm_restart_outfile_set( grid_id, case_id )

        integer,                      intent(in) :: grid_id
        character(len = shr_kind_cs), intent(in) :: case_id

        integer                      :: io_form
        character(len = shr_kind_cl) :: file_name

        file_name = trim(case_id) // '.atm' // trim(inst_suffix) // '.r.<date>.nc'
        call nl_set_rst_outname ( grid_id, file_name )

        call nl_get_io_form_restart ( grid_id, io_form )
        if ( io_form /= io_form_netcdf .and. io_form /= io_form_pnetcdf ) then
            ! Set file format to default
            call nl_set_io_form_restart ( grid_id, io_form_default )
        end if

        ! --- Set pointer file name for restart
        call atm_pfile_set()

    end subroutine atm_restart_outfile_set


    subroutine atm_restart_infile_set( grid_id )

        integer, intent(in) :: grid_id

        integer                      :: rc
        character(len = shr_kind_cl) :: rst_inname

        character( len = * ), parameter :: subname = 'atm_restart_infile_set'

        call atm_pfile_read( rst_inname, rc )
        if ( rc /= 0 ) then
            call atm_log( subname, 'Error reading restart file name in '//trim(atm_pfile_name) )
            call shr_sys_abort()
        end if

        call nl_set_rst_inname( grid_id, rst_inname )

    end subroutine atm_restart_infile_set


    subroutine atm_pfile_set( name )

        character( len = * ), optional, intent(in) :: name

        if ( present(name) ) then
            atm_pfile_name = trim(name) // trim(inst_suffix)
        else
            atm_pfile_name = trim(restart_pfile) // trim(inst_suffix)
        end if

    end subroutine atm_pfile_set


    subroutine atm_pfile_read( rstname, rc )

        character(len = shr_kind_cl), intent(out) :: rstname
        integer,                      intent(out) :: rc

        integer :: pfunit
        logical, external :: wrf_dm_on_monitor

        character( len = * ), parameter :: subname = 'atm_pfile_read'

        rc = 0
        rstname = ''
        if ( wrf_dm_on_monitor() ) then
            pfunit = shr_file_getUnit()
            open( unit = pfunit, file = trim(atm_pfile_name), form = 'formatted', action = 'read', iostat = rc )
            if ( rc == 0 ) then
                rewind ( unit = pfunit )
                read ( unit = pfunit, fmt = '(a)', iostat = rc ) rstname
                if ( rc /= 0 ) call atm_log( subname, 'Error reading from file '//trim(atm_pfile_name) )
                close( unit = pfunit )
            else
                call atm_log( subname, 'Error opening file '//trim(atm_pfile_name) )
            end if
            call shr_file_freeUnit( pfunit )
        end if

        call wrf_dm_bcast_integer( rc, 1 )
        if ( rc == 0 ) call wrf_dm_bcast_string( rstname, len(rstname) )

    end subroutine atm_pfile_read

    subroutine atm_pfile_write( rstname )

        character( len = * ), intent(in) :: rstname

        integer :: pfunit, rc
        logical, external :: wrf_dm_on_monitor

        character( len = * ), parameter :: subname = 'atm_pfile_write'

        rc = 0
        if ( wrf_dm_on_monitor() ) then
            pfunit = shr_file_getUnit()
            open( unit = pfunit, file = trim(atm_pfile_name), form = 'formatted', status = 'unknown', iostat = rc )
            if ( rc == 0 ) then
                rewind ( unit = pfunit )
                write ( unit = pfunit, fmt = '(a)', iostat = rc ) trim(rstname)
                if ( rc /= 0 ) call atm_log( subname, 'Error writing to file '//trim(atm_pfile_name) )
                close( unit = pfunit )
            else
                call atm_log( subname, 'Error opening file '//trim(atm_pfile_name) )
            end if
            call shr_file_freeUnit( pfunit )
        end if

    end subroutine atm_pfile_write

    subroutine atm_restart_write( grid )

        type ( domain ), intent(in) :: grid

        integer                      :: ierr, rid
        character(len = shr_kind_cs) :: timestr
        character(len = shr_kind_cl) :: rstname, message
        type (grid_config_rec_type)  :: config_flags

        logical, external            :: wrf_dm_on_monitor

        character( len = * ), parameter :: subname = 'atm_restart_write'

        ! --- Begin
        if ( wrf_dm_on_monitor() ) call start_timing

        ! --- Copy configuration into local data structure
        call model_to_grid_config_rec ( grid % id , model_config_rec , config_flags )

        ! --- Create restart file name
        call domain_clock_get( grid, current_timestr=timestr )
        call construct_filename2a( rstname, config_flags % rst_outname, grid % id, 2, timestr )

        ! --- Open restart dataset
        write( message , '(a,": opening ",A," for writing")' ) subname, trim( rstname )
        call wrf_debug( 1 , message )
        call open_w_dataset ( rid, trim(rstname), grid , &
                              config_flags , output_restart , "DATASET=RESTART", ierr )
        if ( ierr /= 0 ) then
            call wrf_message( message )
        end if
        ! --- Write dataset
        call output_restart( rid, grid, config_flags, ierr )
        if ( wrf_dm_on_monitor() ) then
            write( message, FMT = '("Writing restart for domain ",I8)' ) grid % id
            call end_timing( trim(message) )
        end if
        ! --- Close dataset
        call close_dataset( rid, config_flags, "DATASET=RESTART" )

        ! --- Write restart filename to pointer file
        call atm_pfile_write( trim(rstname) )

    end subroutine atm_restart_write


    subroutine atm_last_write( grid )

        type ( domain ), intent(in) :: grid

        type (grid_config_rec_type)  :: config_flags

        character( len = * ), parameter :: subname = 'atm_last_write'

        ! --- Begin

        ! --- Copy configuration into local data structure
        call model_to_grid_config_rec ( grid % id , model_config_rec , config_flags )

        ! --- Create restart file name
        CALL med_last_solve_io ( grid , config_flags )

    end subroutine atm_last_write


    subroutine atm_init_read( grid )

        type ( domain ), pointer, intent(inout) :: grid

        logical                     :: restart
        type (grid_config_rec_type) :: config_flags

        character( len = * ), parameter :: subname = 'atm_init_read'

        ! --- Begin

        ! --- If restart, set restart file name
        call nl_get_restart( grid % id, restart )
        if ( restart ) call atm_restart_infile_set( grid % id )

        ! --- Copy configuration into local data structure
        call model_to_grid_config_rec ( grid % id , model_config_rec , config_flags )

        ! --- Read initial data
        CALL med_initialdata_input( grid , config_flags )

        IF ( config_flags%write_restart_at_0h ) THEN
           CALL med_restart_out ( grid, config_flags )
        END IF

        ! set default values for subtimes
        grid%start_subtime = domain_get_start_time ( grid )
        grid%stop_subtime  = domain_get_stop_time  ( grid )

        !  For EM (but not DA), if this is a DFI run, we can allocate some space.  We are
        !  not allowing anyting tricky for nested DFI.  If there are any nested domains,
        !  they all need to start at the same time.  Otherwise, why even do the DFI?  If
        !  the domains do not all start at the same time, then there will be inconsistencies,
        !  which is what DFI is supposed to address.

#if (EM_CORE == 1)
        IF ( grid%dfi_opt .NE. DFI_NODFI ) THEN
           CALL alloc_doms_for_dfi ( grid )
        END IF
#endif

    end subroutine atm_init_read

end module atm_files_mod
