module xocn_log_mod

    use shr_kind_mod, only : r8 => SHR_KIND_R8, &
                             SHR_KIND_CL

    use shr_file_mod, only : shr_file_getUnit,     &
                             shr_file_freeUnit,    &
                             shr_file_getLogUnit,  &
                             shr_file_setLogUnit,  &
                             shr_file_getLogLevel, &
                             shr_file_setLogLevel
  
    use shr_sys_mod,  only : shr_sys_abort, &
                             shr_sys_flush, &
                             shr_sys_system
    use ocn_instance, only : inst_suffix

    use xocn_file_mod

    implicit none

    integer :: shrlogunit
    integer :: shrloglev
    integer :: xocn_logunit 
    logical :: xocn_logging_on 
    logical :: xocn_logfile_on 
    character(SHR_KIND_CL) :: xocn_logfile
    character(SHR_KIND_CL) :: xocn_nml_logfile
    character(SHR_KIND_CL) :: xocn_data_logfile
    character(SHR_KIND_CL) :: xocn_actv_logfile
    character(SHR_KIND_CL) :: xocn_inpdir
    character(SHR_KIND_CL) :: xocn_logdir

    integer, parameter :: xocn_model_data = 1
    integer, parameter :: xocn_model_actv = 2

    character( len = * ), parameter :: nml_modelio = 'ocn_modelio.nml'
    character( len = * ), parameter :: xocn_label_driv = 'main'
    character( len = * ), parameter :: xocn_label_data = 'data'
    character( len = * ), parameter :: xocn_label_actv = 'roms'

    character( len = * ), parameter :: xrst_comm_pfile = 'rpointer.ocn'
    character( len = * ), parameter :: xrst_data_pfile = 'rpointer.docn'
    character( len = * ), parameter :: xrst_actv_pfile = 'rpointer.roms'

    
    private

    public :: xocn_logging_on
    public :: xocn_model_data
    public :: xocn_model_actv
    public :: xocn_log_init
    public :: xocn_log_finalize
    public :: xocn_log_set
    public :: xocn_log_reset
    public :: xocn_log
    public :: xocn_log_setfile
    public :: xocn_log_setrestart

    interface xocn_log
        module procedure xocn_log_char
        module procedure xocn_log_char_int
        module procedure xocn_log_char_int2
        module procedure xocn_log_char_int3
        module procedure xocn_log_char_real
        module procedure xocn_log_char_real2
    end interface xocn_log

contains

    subroutine xocn_log_set()

        implicit none
      
        character( len = * ), parameter :: subName = '(xocn_log_set) '

        ! --- set log unit ---
        call shr_file_getLogUnit (shrlogunit)
        call shr_file_getLogLevel(shrloglev)
        call shr_file_setLogUnit (xocn_logunit)

    end subroutine xocn_log_set


    subroutine xocn_log_reset()

        implicit none
      
        character( len = * ), parameter :: subName = '(xocn_log_set) '

        ! --- reset log unit ---
        call shr_file_setLogUnit (shrlogunit)
        call shr_file_setLogLevel(shrloglev)
        call shr_sys_flush(xocn_logunit)

    end subroutine xocn_log_reset


    subroutine xocn_log_init(mpicom)

        use shr_mpi_mod, only : shr_mpi_commrank

        implicit none

        integer, optional, intent(in) :: mpicom

        integer :: iunit, my_rank, rc, l
        logical :: exists

        character(SHR_KIND_CL) :: diri
        character(SHR_KIND_CL) :: diro
        character(SHR_KIND_CL) :: logfile

        namelist / modelio / diri, diro, logfile

        character( len = * ), parameter :: subName = '(xocn_log_init) '

        xocn_logfile = ''
        xocn_nml_logfile = ''
        xocn_logfile_on = .false.

        if (present(mpicom)) then
           call shr_mpi_commrank( mpicom, my_rank, subName )
           xocn_logging_on = ( my_rank == 0 )
        else
           xocn_logging_on = .true.
        end if

        if ( xocn_logging_on ) then
           call shr_file_getLogUnit (shrlogunit)
           inquire( file = nml_modelio // inst_suffix, exist = exists )
           if ( exists ) then
              diri = '.'
              diro = '.'
              logfile = ''
              iunit = shr_file_getUnit()
              open( unit = iunit, file = nml_modelio // inst_suffix, action = 'read', position = 'rewind', iostat = rc )
              if ( rc == 0 ) read( unit = iunit, nml = modelio, iostat = rc )
              close( unit = iunit, iostat = rc )
              call shr_file_freeUnit( iunit )
              if ( rc /= 0 ) then
                 write( shrlogunit, '(4a,i0)' ) subName, ':: ERROR reading ', nml_modelio // inst_suffix, ', iostat = ',rc
                 call shr_sys_flush( shrlogunit )
                 call shr_sys_abort( subName // ':: ERROR reading ' // nml_modelio // inst_suffix )
              end if
              call xocn_file_setdir( diro, xocn_logdir )
              call xocn_file_setdir( diri, xocn_inpdir )

              xocn_nml_logfile  = trim(xocn_logdir) // trim(logfile)
              xocn_data_logfile = trim(xocn_logdir) // trim(xocn_label_data) // '.' // trim(logfile)
              xocn_actv_logfile = trim(xocn_logdir) // trim(xocn_label_actv) // '.' // trim(logfile)
              xocn_logfile      = trim(xocn_logdir) // trim(xocn_label_driv) // '.' // trim(logfile)
              xocn_logfile_on = .true.

              xocn_logunit = shr_file_getUnit()
              open( unit = xocn_logunit, file = trim(xocn_logfile), form = 'formatted', status = 'unknown', position = 'rewind', iostat = rc )
              if ( rc /= 0 ) then
                 write( shrlogunit, '(4a,i0)' ) subName, ':: ERROR opening log file ', trim(xocn_logfile), ', iostat = ', rc
                 call shr_sys_flush( shrlogunit )
                 call shr_sys_abort( subName // ':: ERROR opening log file ' // trim(xocn_logfile) )
              end if
           else
              write( shrlogunit, '(4a)' ) subName, ':: ', nml_modelio // inst_suffix, ' not found. Using shared log unit.'
              xocn_logunit = shrlogunit
           end if
        end if

        call xocn_log_set()
        
    end subroutine xocn_log_init

    subroutine xocn_log_finalize()

        implicit none

        integer :: rc

        character( len = * ), parameter :: subName = '(xocn_log_finalize) '

        if ( xocn_logging_on ) close( unit = xocn_logunit, iostat = rc )

        call xocn_log_reset()

    end subroutine xocn_log_finalize

    subroutine xocn_log_setfile( model, init )

        implicit none

        integer,           intent(in) :: model
        logical, optional, intent(in) :: init

        logical :: initfile

        if ( xocn_logging_on .and. xocn_logfile_on ) then
           if ( present(init) ) then
              initfile = init
           else
              initfile = .false.
           end if
           select case ( model )
              case ( xocn_model_data )
                 call xocn_file_link( xocn_data_logfile, xocn_nml_logfile, init = initfile )
              case ( xocn_model_actv )
                 call xocn_file_link( xocn_actv_logfile, xocn_nml_logfile, init = initfile )
           end select
        end if

    end subroutine xocn_log_setfile

    subroutine xocn_log_setrestart( model, action )

        implicit none

        integer,             intent(in) :: model
        character( len = *), intent(in) :: action

        logical :: initfile

        character( SHR_KIND_CL ) :: dirp
        character( SHR_KIND_CL ) :: restartp

        character( len = * ), parameter :: subName = '(xocn_log_setrestart) '

        if ( xocn_logging_on ) then

           dirp = xocn_logdir

           if ( trim(action) == 'read' .xor. trim(action) == 'READ' ) then
              dirp = xocn_logdir
              initfile = .false.
           else if ( trim(action) == 'write' .xor. trim(action) == 'WRITE' ) then
              dirp = xocn_logdir
              initfile = .true.
           else
              call shr_sys_abort( subName // ':: ERROR - action not recognized: ' // trim(action) )
           end if

           select case ( model )
              case ( xocn_model_data )
                 restartp = trim( dirp ) // trim( xrst_data_pfile ) // inst_suffix
              case ( xocn_model_actv )
                 restartp = trim( dirp ) // trim( xrst_actv_pfile ) // inst_suffix
              case default
                 call shr_sys_abort( subName // ':: ERROR - model type not recognized' )
           end select

           call xocn_file_link( restartp, trim(dirp) // trim(xrst_comm_pfile) // inst_suffix, init = initfile )

           call xocn_log( subName, 'restart pointer set to: ' // trim(restartp) )

        end if

    end subroutine xocn_log_setrestart


!   subroutine xocn_log_ln(name,init)

!       character(len=*), intent(in) :: name
!       logical, intent(in) :: init

!       integer :: rc

!       if ( xocn_logging_on ) then
!          if ( init ) then
!             call shr_sys_system( 'echo "--- data ocean log ---" > data.docn.log', rc )
!             call shr_sys_system( 'echo "--- roms model log ---" > roms.docn.log', rc )
!          end if
!          call shr_sys_system( 'ln -sf ' // trim(name) // '.docn.log docn.log', rc )
!       end if

!   end subroutine xocn_log_ln

    subroutine xocn_log_char( label, message )

        implicit none

        character( len = * ), intent(in) :: label, message

        if ( xocn_logging_on ) then
           write(xocn_logunit, '(a," :: ",a)') trim(label), trim(message)
           call shr_sys_flush(xocn_logunit)
        end if

    end subroutine xocn_log_char

    subroutine xocn_log_char_int( label, message, int )

        implicit none

        character( len = * ), intent(in) :: label, message
        integer, intent(in) :: int

        if ( xocn_logging_on ) then
           write(xocn_logunit, '(a," :: ",a,2x,i0)') trim(label), trim(message), int
           call shr_sys_flush(xocn_logunit)
        end if

    end subroutine xocn_log_char_int

    subroutine xocn_log_char_int2( label, message, int1, int2 )

        implicit none

        character( len = * ), intent(in) :: label, message
        integer, intent(in) :: int1, int2

        if ( xocn_logging_on ) then
           write(xocn_logunit, '(a," :: ",a,2(2x,i0))') trim(label), trim(message), int1, int2
           call shr_sys_flush(xocn_logunit)
        end if

    end subroutine xocn_log_char_int2

    subroutine xocn_log_char_int3( label, message, int1, int2, int3 )

        implicit none

        character( len = * ), intent(in) :: label, message
        integer, intent(in) :: int1, int2, int3

        if ( xocn_logging_on ) then
           write(xocn_logunit, '(a," :: ",a,3(2x,i0))') trim(label), trim(message), int1, int2, int3
           call shr_sys_flush(xocn_logunit)
        end if

    end subroutine xocn_log_char_int3


    subroutine xocn_log_char_real( label, message, real )

        implicit none

        character( len = * ), intent(in) :: label, message
        real(r8), intent(in) :: real

        if ( xocn_logging_on ) then
           write(xocn_logunit, '(a," :: ",a,2x,g22.14)') trim(label), trim(message), real
           call shr_sys_flush(xocn_logunit)
        end if

    end subroutine xocn_log_char_real

    subroutine xocn_log_char_real2( label, message, real1, real2 )

        implicit none

        character( len = * ), intent(in) :: label, message
        real(r8), intent(in) :: real1, real2

        if ( xocn_logging_on ) then
           write(xocn_logunit, '(a," :: ",a,2(2x,g22.14))') trim(label), trim(message), real1, real2
           call shr_sys_flush(xocn_logunit)
        end if

    end subroutine xocn_log_char_real2


end module xocn_log_mod
