!BOP
!   !MODULE: ocn_comp_mct
!   !INTERFACE:

module rxocn_comp_mct

!   !DESCRIPTION:
!       This is the main driver for the Extended Regional Ocean Modeling System (xROMS)
!       -- ROMS embedded into a data ocean model
!
!   !REVISION HISTORY:
!       Mar 23, 2015 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

  use shr_kind_mod,  only : r8 => SHR_KIND_R8

  use seq_cdata_mod
  use esmf
  use mct_mod
 
  use shr_sys_mod,   only : shr_sys_abort, &
                            shr_sys_flush

  use perf_mod,      only : t_startf, &
                            t_stopf

  use docn_comp_mod
  use rocn_comp_mct, only : rocn_comp_init  => rocn_init_mct, &
                            rocn_comp_run   => rocn_run_mct,  &
                            rocn_comp_final => rocn_final_mct

  use ocn_instance
  use xocn_log_mod
  use xocn_timemgr_mod

! !PUBLIC TYPES:
  implicit none

  private ! except

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: rxocn_init_mct
  public :: rxocn_run_mct
  public :: rxocn_final_mct

!EOP
!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

  integer, parameter :: ocn_root_rank = 0

  logical, dimension(:), pointer :: domerge

  type(seq_cdata) :: cdata_r
  type(mct_aVect) :: x2o_r, o2x_r
  type(mct_rearr) :: rearr_x2o, rearr_o2x

  character(len = xocn_timemgr_timeString_len) :: ocnTimeString

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CONTAINS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: rxocn_init_mct
!
! !DESCRIPTION:
!     initialize data + ROMS ocean models
!
! !REVISION HISTORY:
!       Mar 23, 2015 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
! !INTERFACE: ------------------------------------------------------------------

subroutine rxocn_init_mct( EClock, cdata, x2o, o2x, NLFilename )

    use shr_kind_mod,     only : shr_kind_cl, &
                                 shr_kind_cs
    use seq_infodata_mod, only : seq_infodata_type, &
                                 seq_infodata_start_type_start, &
                                 seq_infodata_start_type_cont,  &
                                 seq_infodata_start_type_brnch, &
                                 seq_infodata_GetData, &
                                 seq_infodata_PutData

    implicit none

! !INPUT/OUTPUT PARAMETERS:

    type(ESMF_Clock)            , intent(inout) :: EClock
    type(seq_cdata)             , intent(inout) :: cdata
    type(mct_aVect)             , intent(inout) :: x2o, o2x
    character(len=*), optional  , intent(in)    :: NLFilename ! Namelist filename

!EOP

    ! --- Local variables ---
    integer :: ioffset, joffset
    integer :: nx_o, ny_o
    integer :: nx_r, ny_r
    integer :: mpicom_o, &
               OCNID
    integer :: rc

    logical :: restart

    real(r8) :: eps_ogrid

    character(shr_kind_cl) :: start_type
    character(shr_kind_cs) :: case_name

    type(mct_gGrid), pointer :: dom_o
    type(mct_gsMap), pointer :: gsMap_o

    type(mct_gGrid), target  :: dom_r
    type(mct_gsMap), target  :: gsMap_r

    type(mct_gsMap)          :: gsMap_x


    type(seq_infodata_type), pointer :: infodata
    type(seq_infodata_type), target  :: infodata_r

    character(len = *), parameter :: subName = "(rxocn_init_mct) "
!-------------------------------------------------------------------------------

    ! --- Set cdata pointers (cdata name set by coupler) ---
    call seq_cdata_setptrs(cdata, ID=OCNID, dom=dom_o, gsMap=gsMap_o, infodata=infodata, &
                           mpicom=mpicom_o)

    call ocn_instance_init( OCNID )
    ! --- set model's log unit ---
    call xocn_log_init( mpicom = mpicom_o )

    call xocn_log( subName, 'Starting eXtended Regional Ocean Modeling System (xROMS)' )

    ! --- Set logfile to data model ---
    call xocn_log_setfile( model = xocn_model_data, init = .true. )

    ! --- Get infodata ---
    call seq_infodata_GetData(infodata, case_name=case_name, start_type=start_type)

    call xocn_log( subName, 'case_name  : ' // trim(case_name ) )
    call xocn_log( subName, 'start_type : ' // trim(start_type) )

    if      ( trim(start_type) == trim(seq_infodata_start_type_start) ) then
       restart = .false.
       call xocn_log( subName, 'models set to initialize' )
    else if ( trim(start_type) == trim(seq_infodata_start_type_cont ) ) then
       restart = .true.
    else if ( trim(start_type) == trim(seq_infodata_start_type_brnch) ) then
       restart = .true.
    else
       call shr_sys_abort( subName // ':: ERROR - start_type not implemented: ' // trim(start_type) )
    end if

    ! --- Set restart pointer file if necessary ---
    if ( restart ) call xocn_log_setrestart( model = xocn_model_data, action = 'read' )

    ! --- Initialize data model ---
    call xocn_log( subName, 'start data ocean' )

    call t_startf('docn_init')

    if (present(NLFilename)) then
       call docn_comp_init( EClock, cdata, x2o, o2x, NLFilename )
    else
       call docn_comp_init( EClock, cdata, x2o, o2x  )
    end if

    call t_stopf ('docn_init')

    call xocn_log( subName, 'data ocean ready' )


    ! --- Copy info from coupler to ROMS ---
    call seq_infodata_PutData(infodata_r, case_name=case_name, start_type=start_type)

    ! --- Setup cdata info for ROMS ---
    call seq_cdata_init(cdata_r, ID=OCNID, dom=dom_r, gsMap=gsMap_r, infodata=infodata_r, &
                        name='Embedded ROMS')

    ! --- Set logfile to active model ---
    call xocn_log_setfile( model = xocn_model_actv, init = .true. )

    ! --- Set restart pointer file if necessary ---
    if ( restart ) call xocn_log_setrestart( model = xocn_model_actv, action = 'read' )

    ! --- Initialize ROMS model ---
    call xocn_log( subName, 'start active ocean' )

    call t_startf('rocn_comp_init')

    if (present(NLFilename)) then
       call rocn_comp_init( EClock, cdata_r, x2o_r, o2x_r, NLFilename )
    else
       call rocn_comp_init( EClock, cdata_r, x2o_r, o2x_r )
    endif

    call t_stopf ('rocn_comp_init')

    call xocn_log( subName, 'active ocean ready' )

    ! --- Set extended ocean model prognostics ---
    call seq_infodata_PutData(infodata, ocn_prognostic = .true., ocnrof_prognostic = .true.)

    ! --- Output cdata info ---
    !call seq_cdata_info(cdata)
    !call seq_cdata_info(cdata_r)

    ! --- Setup MCT rearrange ---
    call seq_infodata_GetData(infodata, ocn_nx=nx_o, ocn_ny=ny_o, eps_ogrid=eps_ogrid)

    call xocn_log( subName, 'data: nx, ny = ', nx_o, ny_o )
    call xocn_log( subName, 'data: gsize = ', mct_gsMap_gsize(gsMap_o) )
    call xocn_log( subName, 'data: eps_ogrid = ', eps_ogrid )

    call seq_infodata_GetData(infodata_r, ocn_nx=nx_r, ocn_ny=ny_r)

    call xocn_log( subName, 'roms: nx, ny =', nx_r, ny_r )
    call xocn_log( subName, 'roms: gsize = ', mct_gsMap_gsize(gsMap_r) )

    call ocn_setRearrGSMap_mct( mpicom_o, OCNID, dom_r, gsMap_r, nx_r, dom_o, gsMap_o, nx_o, eps_ogrid, gsMap_x )

    call xocn_log( subName, 'set gsMap_x: gsize = ', mct_gsMap_gsize(gsMap_x) )

    call xocn_log( subName, 'setting rearrangers: coupler -> active, active -> coupler' )
    call mct_rearr_init(gsMap_o, gsMap_x, mpicom_o, rearr_x2o)
    call mct_rearr_init(gsMap_x, gsMap_o, mpicom_o, rearr_o2x)

    call xocn_log( subName, 'setting active/data -> coupler merging mask')
    call ocn_setMergeMask_mct( mpicom_o, dom_r, gsMap_r, gsMap_o, rearr_o2x, domerge )
    call ocn_merge_mct( o2x_r, o2x, rearr_o2x, domerge )

    call mct_gsMap_clean( gsMap_x, rc )

    ! --- get current model time ---
    call xocn_timemgr_getClock( EClock, timeString=ocnTimeString )
    call xocn_log( subName, 'xROMS init time: ' // trim(ocnTimeString) )

    call xocn_log( subName, 'xROMS ready' )

    ! --- reset log unit ---
    call xocn_log_reset()

end subroutine rxocn_init_mct

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: rxocn_run_mct
!
! !DESCRIPTION:
!     run method for data + ROMS ocean model
!
! !REVISION HISTORY:
!       Mar 23, 2015 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
! !INTERFACE: ------------------------------------------------------------------

subroutine rxocn_run_mct( EClock, cdata,  x2o, o2x )

    use seq_timemgr_mod, only : seq_timemgr_RestartAlarmIsOn

    implicit none

! !INPUT/OUTPUT PARAMETERS:

    type(ESMF_Clock)            ,intent(inout) :: EClock
    type(seq_cdata)             ,intent(inout) :: cdata
    type(mct_aVect)             ,intent(inout) :: x2o
    type(mct_aVect)             ,intent(inout) :: o2x

!EOP

    logical :: restart_now
    character(len = xocn_timemgr_timeString_len) :: endTimeString

    character(*), parameter :: subName = "(rxocn_run_mct) "
!-------------------------------------------------------------------------------

    ! --- Set model's log unit ---
    call xocn_log_set()

    ! --- Print clock info ---
    call xocn_log( subName, 'Advancing data and active components' )

    call xocn_timemgr_getClock( EClock, timeString=endTimeString )

    call xocn_log( subName, 'Start Time: ' // trim(ocnTimeString) )
    call xocn_log( subName, '  End Time: ' // trim(endTimeString) )
    ocnTimeString = endTimeString

    ! --- Set logfile to data model ---
    call xocn_log_setfile( model = xocn_model_data )

    ! --- Check if restart is written at the end of run time interval ---
    restart_now = seq_timemgr_RestartAlarmIsOn( EClock )

    if ( restart_now ) call xocn_log_setrestart( model = xocn_model_data, action = 'write' )

    ! --- Run data ocean on coupling grid ---
    call xocn_log( subName, 'running data ocean...')
    call t_startf('docn_comp_run')
    call docn_comp_run( EClock, cdata, x2o, o2x )
    call t_stopf ('docn_comp_run')
    call xocn_log( subName, 'data ocean done' )

    ! --- Rearrange surface quantities from coupler to embedded ROMS ---
    call mct_rearr_rearrange( x2o, x2o_r, rearr_x2o )

    call xocn_log( subName, 'rearranged x2o: coupler -> active' )

    ! --- Set logfile to active model ---
    call xocn_log_setfile( model = xocn_model_actv )

    ! --- Set restart pointer if necessary ---
    if ( restart_now ) call xocn_log_setrestart( model = xocn_model_actv, action = 'write' )

    ! --- Run embedded ROMS ---
    call xocn_log( subName, 'running active ocean...')
    call t_startf('rocn_comp_run')
    call rocn_comp_run( EClock, cdata_r, x2o_r, o2x_r )
    call t_stopf ('rocn_comp_run')
    call xocn_log( subName, 'active ocean done' )

    ! --- Merge surface quantities from embedded ROMS to coupling grid ---
    call ocn_merge_mct( o2x_r, o2x, rearr_o2x, domerge )

    call xocn_log( subName, 'merged o2x: active/data -> coupler' )

    ! --- reset log unit ---
    call xocn_log_reset()
 
end subroutine rxocn_run_mct

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: rxocn_final_mct
!
! !DESCRIPTION:
!     finalize method for data + ROMS ocean model
!
! !REVISION HISTORY:
!       Mar 23, 2015 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
! 
! !INTERFACE: ------------------------------------------------------------------
!
subroutine rxocn_final_mct( EClock, cdata, x2o, o2x )

    implicit none

    !----- arguments -----

    type(ESMF_Clock)            ,intent(inout) :: EClock
    type(seq_cdata)             ,intent(inout) :: cdata
    type(mct_aVect)             ,intent(inout) :: x2o
    type(mct_aVect)             ,intent(inout) :: o2x

!EOP

    !--- local variables ---
    integer :: rc

    !--- formats ---
    character(*), parameter :: subName = "(rxocn_final_mct) "
!-------------------------------------------------------------------------------

    ! --- Set model's log unit ---
    call xocn_log_set()

    call xocn_log( subName, 'Finalizing xROMS...' )

    if (associated(domerge)) deallocate(domerge, stat = rc)

    call mct_rearr_clean( rearr_x2o )
    call mct_rearr_clean( rearr_o2x )

    call mct_aVect_clean( x2o_r )
    call mct_aVect_clean( o2x_r )

    !call seq_cdata_clean( cdata_r )

    call docn_comp_final()
    call xocn_log( subName, 'data ocean finalized' )

    call rocn_comp_final( EClock, cdata, x2o, o2x )
    call xocn_log( subName, 'active ocean finalized' )

    call xocn_log( subName, 'xROMS done' )

    ! --- reset log unit ---
    call xocn_log_finalize()

end subroutine rxocn_final_mct

!===============================================================================
!BOP
!   !ROUTINE: ocn_getGridOffset_mct
!   !INTERFACE:

subroutine ocn_setRearrGSMap_mct( mpicom, OCNID, dom_a, gsMap_a, nx_a, dom_b, gsMap_b, nx_b, match_thrs, gsMap_x )

!   !DESCRIPTION:
!       Locate domain A (dom_a) within domain B (dom_b) and compute grid point
!       offsets. Based on POP ocn_coffset_mct() subroutine 
!
!   !REVISION HISTORY:
!       Mar 23, 2015 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

    use shr_mpi_mod,  only : shr_mpi_bcast,   &
                             shr_mpi_commrank

    implicit none

!   !INPUT/OUTPUT PARAMETERS:

    integer,         intent(in)    :: mpicom
    integer,         intent(in)    :: OCNID
    type(mct_gGrid), intent(in)    :: dom_a
    type(mct_gsMap), intent(in)    :: gsMap_a
    integer,         intent(in)    :: nx_a
    type(mct_gGrid), intent(in)    :: dom_b
    type(mct_gsMap), intent(in)    :: gsMap_b
    integer,         intent(in)    :: nx_b
    real(r8),        intent(in)    :: match_thrs
    type(mct_gsMap), intent(inout) :: gsMap_x

!EOP
!BOC

    ! -- Local variables --

    integer :: kx, ky
    integer :: lsize, lsize_a, lsize_b, gsize_a, gsize_b
    integer :: ij, ij_offset, ij_offset_g
    integer :: i, ioffset, j, joffset
    integer :: my_rank
    integer,  dimension(:), pointer :: ipoints, gindex
    real(r8) :: x, y, dx2, dy2, dr2, dr2_min, dr2_min_g
    real(r8), dimension(2) :: p

    type(mct_aVect) :: aV_a, aV_local

    integer, parameter :: stdout = 6
    character( len = * ), parameter :: subName = '(ocn_getGridOffset_mct) '

    ! -- Begin

    ioffset = 0
    joffset = 0

    lsize_a = mct_gsMap_lsize( gsMap_a, mpicom )
    lsize_b = mct_gsMap_lsize( gsMap_b, mpicom )

!   -- perhaps use these functions:
!   lsize_a = mct_gGrid_lsize( dom_a )
!   lsize_b = mct_gGrid_lsize( dom_b )

    call mct_aVect_init( aV_local, rList='lon:lat', lsize=lsize_a )
    call mct_aVect_copy( dom_a % data, aV_local, rList='lon:lat' )
    call mct_aVect_gather( aV_local, aV_a, gsMap_a, ocn_root_rank, mpicom )
    call mct_aVect_clean( aV_local )

    call shr_mpi_commrank( mpicom, my_rank, subName )

    if ( my_rank == ocn_root_rank ) then
       kx   = mct_aVect_indexRA( aV_a, 'lon', dieWith=subName // '_aV_a' )
       ky   = mct_aVect_indexRA( aV_a, 'lat', dieWith=subName // '_aV_a' )
       p(1) = mod(aV_a % rAttr(kx, 1) + 360._r8, 360._r8)
       p(2) =     aV_a % rAttr(ky, 1)
    end if

    call mct_aVect_clean( aV_a )

    call shr_mpi_bcast( p, mpicom )

    call mct_gsMap_orderedPoints( gsMap_b, my_rank, ipoints )

    if (size(ipoints) /= lsize_b) then
       call xocn_log( subName,' size ipoints = ', size(ipoints), lsize_b )
       call shr_sys_abort(subName//' :: error size of ipoints')
    endif

    kx = mct_aVect_indexRA( dom_b % data,'lon', dieWith=subName//'_dom_b')
    ky = mct_aVect_indexRA( dom_b % data,'lat', dieWith=subName//'_dom_b')

    dr2_min = huge(1._r8)
    ij_offset = -1

    do ij = 1, lsize_b
       x = mod( dom_b % data % rAttr(kx,ij) + 360._r8, 360._r8 )
       y =      dom_b % data % rAttr(ky,ij)
       dx2 = x-p(1)
       dx2 = dx2 * dx2
       dy2 = y-p(2)
       dy2 = dy2 * dy2
       dr2 = dx2 + dy2
       if ( dr2 < dr2_min ) then
          dr2_min = dr2
          ij_offset = ipoints(ij)
       end if
       dx2 = x-p(1)-360._r8
       dx2 = dx2 * dx2
       dr2 = dx2 + dy2
       if ( dr2 < dr2_min ) then
          dr2_min = dr2
          ij_offset = ipoints(ij)
       end if
       dx2 = x-p(1)+360._r8
       dx2 = dx2 * dx2
       dr2 = dx2 + dy2
       if ( dr2 < dr2_min ) then
          dr2_min = dr2
          ij_offset = ipoints(ij)
       end if
    end do

    deallocate(ipoints)

    call shr_mpi_min( dr2_min, dr2_min_g, mpicom, 'dr2_min_g', all=.true. )

    x = sqrt(dr2_min_g)
    if ( x > match_thrs ) then
       call xocn_log( subName, 'min. distance > threshold:', x, match_thrs )
       call shr_sys_abort(subName//' :: matching threshold exceeded')
    end if

    if ( dr2_min_g /= dr2_min ) then
       ij_offset = -1
    endif

    call shr_mpi_max(ij_offset, ij_offset_g, mpicom, 'ij_offset_g', all=.true. )

    if (ij_offset_g < 1) call shr_sys_abort(subName//' :: min. ij offset < 1')

    call ocn_getGSMap_ij( ij_offset_g, nx_b, ioffset, joffset )

    call xocn_log( subName, 'active ocean grid origin: lon,lat = ',p(1),p(2) )
    call xocn_log( subName, 'data ocean grid location: i, j, n = ',ioffset, joffset, ij_offset_g )

    ioffset = ioffset - 1
    joffset = joffset - 1

    ! --- check grid bounds ---
    gsize_a = mct_gsMap_gsize( gsMap_a )
    gsize_b = mct_gsMap_gsize( gsMap_b )
    if ( my_rank == ocn_root_rank ) then
       i = ioffset + nx_a 
       if ( i > nx_b ) then
          call xocn_log( subName, 'active ocean grid beyond data ocean Eastern boundary: active,data = ',i,nx_b )
          call shr_sys_abort(subName//' :: incompatible active/ocean grids')
       end if
       ij = gsize_b/nx_b
       j = joffset + gsize_a/nx_a 
       if ( j > ij ) then
          call xocn_log( subName, 'active ocean grid beyond data ocean Northern boundary: active,data = ',j,ij )
          call shr_sys_abort(subName//' :: incompatible active/ocean grids')
       end if
    end if

    ! --- setup new map using only active ocean points from inner domain A ---
    call mct_gsMap_orderedPoints( gsMap_a, my_rank, ipoints )

    do ij = 1, lsize_a
       call ocn_getGSMap_ij( ipoints(ij), nx_a, i, j )
       i = i + ioffset
       j = j + joffset
       ipoints(ij) = i + (j-1) * nx_b
    end do

    call mct_gsMap_init( gsMap_x, ipoints, mpicom, OCNID, lsize_a, gsize_b )

    deallocate(ipoints)

end subroutine ocn_setRearrGSMap_mct

subroutine ocn_setMergeMask_mct( mpicom, dom_a, gsMap_a, gsMap_b, rearr, domerge )

    implicit none

    integer,         intent(in)    :: mpicom
    type(mct_gGrid), intent(in)    :: dom_a
    type(mct_gsMap), intent(in)    :: gsMap_a
    type(mct_gsMap), intent(in)    :: gsMap_b
    type(mct_rearr), intent(in)    :: rearr
    logical, dimension(:), pointer :: domerge

    integer :: imask, lsize_a, lsize_b, rc

    type(mct_aVect) :: av_a, av_b

    character( len = * ), parameter :: subName = '(ocn_setMergeMask_mct) '

    lsize_a = mct_gsMap_lsize( gsMap_a, mpicom )
    lsize_b = mct_gsMap_lsize( gsMap_b, mpicom )

    call mct_aVect_init( av_a, rList='mask', lsize=lsize_a )
    call mct_aVect_copy( dom_a % data, av_a, rList='mask'  )
    call mct_aVect_init( av_b, rList='mask', lsize=lsize_b )
    call mct_aVect_zero( av_b )

    call mct_rearr_rearrange( av_a, av_b, rearr )

    imask = mct_aVect_indexRA( av_b, 'mask', dieWith=subName//'_dom_mask' )

    if (associated( domerge )) deallocate( domerge, stat = rc )
    allocate( domerge( lsize_b ) )

    domerge = ( av_b % rAttr( imask, : ) == 0._r8 )

    call mct_aVect_clean( av_a )
    call mct_aVect_clean( av_b )

end subroutine ocn_setMergeMask_mct

subroutine ocn_merge_mct( av_a, av_b, rearr, domerge )

    implicit none
    
    type(mct_aVect),       intent(in)    :: av_a
    type(mct_aVect),       intent(inout) :: av_b
    type(mct_rearr),       intent(in)    :: rearr
    logical, dimension(:), intent(in)    :: domerge

    ! --- Local variables ---
    integer :: lsize, n
    type(mct_aVect) :: av

    character( len = * ), parameter :: subName = '(ocn_merge_mct) '

    lsize = mct_aVect_lsize( av_b )

    if (lsize /= size(domerge)) then
       call xocn_log( subName, 'lsize(aV_b) /= size(domerge): ', lsize, size(domerge) )
       call shr_sys_abort(subName//' :: lsize(aV_b) /= size(domerge)')
    end if

    call mct_aVect_init( av, av_b, lsize )
    call mct_aVect_zero( av )
    call mct_rearr_rearrange( av_a, av, rearr )

    if (associated(av % iAttr)) then
       do n = 1, lsize
          if (domerge(n)) av % iAttr(:,n) = av_b % iAttr(:,n)
       end do
    end if

    if (associated(av % rAttr)) then
       do n = 1, lsize
          if (domerge(n)) av % rAttr(:,n) = av_b % rAttr(:,n)
       end do
    end if

    call mct_aVect_copy( av, av_b )
    call mct_aVect_clean( av )
    
end subroutine ocn_merge_mct

subroutine ocn_getGSMap_ij( n, nx, i, j )

    implicit none
  
    integer, intent(in)  :: n
    integer, intent(in)  :: nx
    integer, intent(out) :: i
    integer, intent(out) :: j

    character( len = * ), parameter :: subName = '(ocn_getGSMap_ij) '

    i = mod(n-1, nx) + 1
    j = (n - i) / nx   + 1

end subroutine ocn_getGSMap_ij

end module rxocn_comp_mct
