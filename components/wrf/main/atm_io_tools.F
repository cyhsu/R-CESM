module atm_io_tools

    use shr_file_mod, only : shr_file_getUnit,     &
                             shr_file_freeUnit,    &
                             shr_file_setIO,       &
                             shr_file_getLogUnit,  &
                             shr_file_setLogUnit,  &
                             shr_file_getLogLevel, &
                             shr_file_setLogLevel

    use shr_sys_mod , only : shr_sys_flush

    use atm_instance, only : inst_isroot, &
                             inst_suffix

    implicit none

    private

    public :: atm_log_init,     &
              atm_log_flush,    &
              atm_log_set,      &
              atm_log_reset,    &
              atm_log_finalize, &
              atm_log_unit,     &
              atm_log

    interface atm_log
        module procedure atm_log_char
    end interface

    integer,             parameter :: filename_len = 256
    integer,             parameter :: std_output = 6
    character (len = *), parameter :: modelio_nml = 'atm_modelio.nml'

    integer       :: atm_log_unit
    integer, save :: shrloglev, shrlogunit

contains

    subroutine atm_log_set(unit)

        integer, optional, intent(in) :: unit

        if (present(unit)) atm_log_unit = unit

        call shr_file_getLogUnit (shrlogunit)
        call shr_file_getLogLevel(shrloglev)
        call shr_file_setLogUnit (atm_log_unit)

    end subroutine atm_log_set

    subroutine atm_log_reset

        call shr_file_setLogUnit (shrlogunit)
        call shr_file_setLogLevel(shrloglev)

    end subroutine atm_log_reset


    subroutine atm_log_init(ionml)

        character (len = *), optional, intent(in) :: ionml

        logical :: exists
        character (len = filename_len) :: io_nml

        if (present(ionml)) then
           io_nml = trim(ionml)
        else
           io_nml = trim(modelio_nml) // trim(inst_suffix)
        end if

        if (inst_isroot) then
           inquire(file = trim(io_nml), exist = exists)
           if (exists) then
              atm_log_unit = shr_file_getUnit()
              call shr_file_setIO(trim(io_nml), atm_log_unit)
           else
              atm_log_unit = std_output
           end if
        else
            atm_log_unit = std_output
        end if

        call atm_log_set

    end subroutine atm_log_init

    subroutine atm_log_finalize

        call atm_log_flush()
        call atm_log_reset()

        if ( atm_log_unit /= std_output ) then
            close ( unit = atm_log_unit )
            call shr_file_freeUnit( atm_log_unit )
            atm_log_unit = std_output
        end if

    end subroutine atm_log_finalize

    subroutine atm_log_flush(unit)

        integer, optional, intent(in) :: unit

        if (present(unit)) then
           call shr_sys_flush(unit)
        else
           call shr_sys_flush(atm_log_unit)
        end if

    end subroutine atm_log_flush

    subroutine atm_log_char(subname, message, string)

        character (len = *),           intent(in) :: subname
        character (len = *),           intent(in) :: message
        character (len = *), optional, intent(in) :: string

        character (len = *), parameter :: fmt1 = "(  5x, '(',a,') ',a)"
        character (len = *), parameter :: fmt2 = "(  5x, '(',a,') ',a,' = ',a)"

        if (inst_isroot) then
           if (present(string)) then
              write(atm_log_unit,fmt2) trim(subname), trim(message), trim(string)
           else
              write(atm_log_unit,fmt1) trim(subname), trim(message)
           end if
           call atm_log_flush
        end if

    end subroutine atm_log_char

end module atm_io_tools
