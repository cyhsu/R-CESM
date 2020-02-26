module xocn_file_mod

    use shr_kind_mod, only : SHR_KIND_CL
    use shr_sys_mod,  only : shr_sys_abort, &
                             shr_sys_system

    use shr_file_mod, only : shr_file_getUnit, &
                             shr_file_freeUnit

    implicit none

    private

    public :: xocn_file_setdir
    public :: xocn_file_link

contains

    subroutine xocn_file_setdir( dirin, dirou )

         implicit none

         character( SHR_KIND_CL ), intent(in)  :: dirin
         character( SHR_KIND_CL ), intent(out) :: dirou

         integer :: l

         character( len = * ), parameter :: subName = '(xocn_file_setdir) '

         l = len( trim(dirin) )
         if ( l == 0 ) then
            dirou = './'
         else if ( dirin( l:l ) == '/' ) then
            dirou = trim(dirin)
         else
            dirou = trim(dirin) // '/'
         end if
 
    end subroutine xocn_file_setdir

    subroutine xocn_file_link( target, name, init )

        implicit none

        character( len = * ), intent(in) :: target
        character( len = * ), intent(in) :: name
        logical, optional,    intent(in) :: init

        logical :: create, exists
        integer :: unit, rc

        character( len = * ), parameter :: subName = '(xocn_file_link) '

        if ( present(init) ) then
           create = init
        else
           create = .false.
        end if

        inquire( file = trim(target), exist = exists )
        if ( .not. exists ) then
           if ( create ) then
              unit = shr_file_getUnit()
              open( unit = unit, file = trim(target), form = 'formatted', action = 'write', status = 'new' )
              write( unit, '(1x)' ) 
              close( unit = unit )
              call shr_file_freeUnit( unit )
           else
              call shr_sys_abort( subName // ' :: ERROR - target file not found: ' // trim(target) )
           end if
        end if

        call shr_sys_system( 'ln -sf ' // trim(target) // ' ' // trim(name), rc )

    end subroutine xocn_file_link

end module xocn_file_mod
