#define XROMS = 1

module ocn_comp_mct

! !USES:
#ifdef XROMS
  use rxocn_comp_mct, only : ocn_init_mct  => rxocn_init_mct, &
                             ocn_run_mct   => rxocn_run_mct,  &
                             ocn_final_mct => rxocn_final_mct
#else
  use rocn_comp_mct, only : ocn_init_mct  => rocn_init_mct, &
                            ocn_run_mct   => rocn_run_mct,  &
                            ocn_final_mct => rocn_final_mct
#endif

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: ocn_init_mct
  public :: ocn_run_mct
  public :: ocn_final_mct

end module ocn_comp_mct
