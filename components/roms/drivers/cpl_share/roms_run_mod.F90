#include "cppdefs.h"
module roms_run_mod

#if defined NONLINEAR && defined SOLVE3D

!BOP
!   !MODULE: roms_run_mod
!   !INTERFACE:

!   !DESCRIPTION:
!   This module contains all the necessary subroutines to run nonlinear
!   ROMS/TOMS when configurated as a full 3D baroclinic ocean model.
!   It advances forward the primitive equations for all nested grids, if any.
!
!   !REVISION HISTORY:
!   Sep 21, 2011 - Original ROMS code, revision 568
!   Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!   Aug 09, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - added wind/velocities rotation
!
!   !USES:
    USE mod_param
    USE mod_parallel
    USE mod_iounits
    USE mod_scalars
    USE mod_stepping

# ifdef ANA_VMIX
    USE analytical_mod, ONLY : ana_vmix
# endif
# ifdef BIOLOGY
    USE biology_mod, ONLY : biology
# endif
# ifdef BBL_MODEL
    USE bbl_mod, ONLY : bblm
# endif
# if defined BULK_FLUXES && !defined CCSMCOUPLED
    USE bulk_flux_mod, ONLY : bulk_flux
# endif
# ifdef BVF_MIXING
    USE bvf_mix_mod, ONLY : bvf_mix
# endif
    USE diag_mod, ONLY : diag
# ifdef TLM_CHECK
    USE dotproduct_mod, ONLY : nl_dotproduct
# endif
# if defined W4DPSAS || defined NLM_OUTER || \
     defined W4DPSAS_SENSITIVITY
    USE forcing_mod, ONLY : forcing
# endif
# if defined ADJUST_STFLUX || defined ADJUST_WSTRESS
    USE frc_adjust_mod, ONLY : frc_adjust
# endif
# ifdef GLS_MIXING
    USE gls_corstep_mod, ONLY : gls_corstep
    USE gls_prestep_mod, ONLY : gls_prestep
# endif
# if defined DIFF_3DCOEF || defined VISC_3DCOEF
    USE hmixing_mod, ONLY : hmixing
# endif
    USE ini_fields_mod, ONLY : ini_fields, ini_zeta
# ifdef LMD_MIXING
    USE lmd_vmix_mod, ONLY : lmd_vmix
# endif
# ifdef MY25_MIXING
    USE my25_corstep_mod, ONLY : my25_corstep
    USE my25_prestep_mod, ONLY : my25_prestep
# endif
# ifdef NESTING
    USE nesting_mod, ONLY : nesting
# endif
# if defined ADJUST_BOUNDARY
    USE obc_adjust_mod, ONLY : obc_adjust, load_obc
# endif
    USE omega_mod, ONLY : omega
# ifdef NEARSHORE_MELLOR
    USE radiation_stress_mod, ONLY : radiation_stress
# endif
# ifndef TS_FIXED
    USE rho_eos_mod, ONLY : rho_eos
# endif
    USE rhs3d_mod, ONLY : rhs3d
# ifdef SEDIMENT
    USE sediment_mod, ONLY : sediment
# endif
# ifdef AVERAGES
    USE set_avg_mod, ONLY : set_avg
# endif
    USE set_depth_mod, ONLY : set_depth
    USE set_massflux_mod, ONLY : set_massflux
# if defined SSH_TIDES || defined UV_TIDES
    USE set_tides_mod, ONLY : set_tides
# endif
    USE set_vbc_mod, ONLY : set_vbc
    USE set_zeta_mod, ONLY : set_zeta
    USE step2d_mod, ONLY : step2d
# ifndef TS_FIXED
    USE step3d_t_mod, ONLY : step3d_t
# endif
    USE step3d_uv_mod, ONLY : step3d_uv
# ifdef FLOATS
    USE step_floats_mod, ONLY : step_floats
# endif
    USE wvelocity_mod, ONLY : wvelocity

    use ocean_control_mod, only : roms_finalize => ROMS_finalize

    use ocn_io_tools, only : ocn_read_drv_params

    use ocn_files, only : ocn_history_outfile_set

    implicit none

    interface roms_halo_update_r2d

        module procedure roms_halo_update_r2d_ab

    end interface roms_halo_update_r2d

!   !PUBLIC MEMBER FUNCTIONS:

    public :: roms_init,                &
              roms_startup,             &
              roms_ini_data,            &
              roms_advance_step,        &
              roms_set_state,           &
              roms_radiation_stress,    &
              roms_step,                &
              roms_halo_update_r2d,     &
              roms_halo_update_fluxes,  &
              roms_finalize,            &
              roms_rotate_vector

!EOP
    private

!
!  Local variable declarations.
!
    integer :: my_iif, next_indx1, subs, tile, thread

contains

!==============================================================================
!BOP
!   !ROUTINE: roms_init
!   !INTERFACE:

    subroutine roms_init(mpicom)

!   !DESCRIPTION:
!       Initialize ROMS instance
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!       Apr 09, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - new shortened version,
!                      remaining part of original routine moved to roms_setup()
!
!   !USES:

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        integer, intent(in) :: mpicom

!EOP
!BOC
        ! --- Local variables ---
        integer :: MyError, MySize

        ! --- Set MPI communicator ---

        OCN_COMM_WORLD = mpicom
        call mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)
        call mpi_comm_size (OCN_COMM_WORLD, MySize, MyError)

        ! --- Initialize parallel control switches. These scalars switches are
        !     independent from standard input parameters.

        call initialize_parallel

!EOC
    end subroutine roms_init

!==============================================================================
!BOP
!   !ROUTINE: roms_startup
!   !INTERFACE:

    subroutine roms_startup(nmlfile)

!   !DESCRIPTION:
!       Start up ROMS
!
!   !REVISION HISTORY:
!       Apr 09, 2013 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release,
!                      from the original version of roms_init()
!       Jun 11, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - removed IC reading
!
!   !USES:

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        character(len = *), optional, intent(in) :: nmlfile

!EOP
!BOC
        ! --- Local variables ---
        logical :: allocate_vars = .true.
        integer :: ng, thread
        integer :: irank,isize,ierr

        ! --- Read driver parameters from namelist file ---

        if (present(nmlfile)) then
           call ocn_read_drv_params(OCN_COMM_WORLD, nmlfile = nmlfile)
        else
           call ocn_read_drv_params(OCN_COMM_WORLD)
        end if

        !
        !  Read in model tunable parameters from input script file. Allocate and
        !  initialize variables in several modules after the number of nested
        !  grids and dimension parameters are known.
        !
        CALL inp_par (iNLM)
        IF (exit_flag.ne.NoError) RETURN

        !
        !  Initialize internal wall clocks. Notice that the timings does not
        !  includes processing standard input because several parameters are
        !  needed to allocate clock variables.
        !
        call mpi_comm_rank (OCN_COMM_WORLD, irank, ierr)
        call mpi_comm_size (OCN_COMM_WORLD, isize, ierr)
        print *, 'MPI task: ',irank,'/',isize

        IF (Master) THEN
          WRITE (stdout,10)
 10       FORMAT (/,' Process Information:',/)
        END IF

        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread) SHARED(numthreads)
          DO thread=0,numthreads-1
            CALL wclock_on (ng, iNLM, 0)
          END DO
!$OMP END PARALLEL DO
        END DO
        !
        !  Allocate and initialize all model state arrays.
        !
        CALL mod_arrays (allocate_vars)

!EOC
    end subroutine roms_startup

!==============================================================================
!BOP
!   !ROUTINE: roms_startup
!   !INTERFACE:

    subroutine roms_ini_data

!   !DESCRIPTION:
!       Reads initial data and observations
!
!   !REVISION HISTORY:
!       Jun 11, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release,
!                      from the original version of roms_init()
!   !USES:

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

!EOP
!BOC
        ! --- Local variables ---
        integer :: ng, out
        character (len=256) :: fname
        logical, external :: find_file

        character(len = *), parameter :: subname = 'roms_ini_data'

        !
        !-----------------------------------------------------------------------
        !  Report output/input files and check availability of input files.
        !-----------------------------------------------------------------------
        !
        IF (Master) THEN
          out = stdout
          DO ng=1,Ngrids
#ifdef INI_FILE
# ifdef NONLINEAR
          fname=INI(ng)%name
          IF (.not.find_file(ng, fname)) GO TO 30
          WRITE (out,230) '  Input Nonlinear Initial File:  ',          &
     &                    TRIM(fname)
# endif
# if defined TANGENT && \
    !(defined FOUR_DVAR         || defined IS4DVAR_SENSITIVITY || \
      defined OPT_OBSERVATIONS  || defined SANITY_CHECK        || \
      defined SENSITIVITY_4DVAR || defined TLM_CHECK)
          fname=ITL(ng)%name
          IF (.not.find_file(ng, fname)) GO TO 30
          WRITE (out,230) '    Input Tangent Initial File:  ',          &
     &                    TRIM(fname)
# endif
# if defined WEAK_CONSTRAINT && \
    !(defined W4DPSAS        || defined W4DPSAS_SENSITIVITY)
          fname=IRP(ng)%name
          IF (.not.find_file(ng, fname)) GO TO 30
          WRITE (out,230) 'Input Representer Initial File:  ',          &
     &                    TRIM(fname)
# endif
# if defined ADJOINT && \
    !(defined AD_SENSITIVITY      || defined FOUR_DVAR         || \
      defined IS4DVAR_SENSITIVITY || defined FORCING_SV        || \
      defined OPT_OBSERVATIONS    || defined OPT_PERTURBATION  || \
      defined SANITY_CHECK        || defined SENSITIVITY_4DVAR || \
      defined SO_SEMI)
          fname=IAD(ng)%name
          IF (.not.find_file(ng, fname)) GO TO 30
          WRITE (out,230) '    Input Adjoint Initial File:  ',          &
     &                    TRIM(fname)
# endif
#endif

          WRITE (out,230) '           Output Restart File:  ',          &
     &                    TRIM(RST(ng)%name)
          IF (LdefHIS(ng)) THEN
            IF (ndefHIS(ng).eq.0) THEN
              WRITE (out,230) '           Output History File:  ',      &
     &                        TRIM(HIS(ng)%name)
            ELSE
              WRITE (out,230) '      Prefix for History Files:  ',      &
     &                        TRIM(HIS(ng)%base)
            END IF
          END IF
          GO TO 40
  30      IF (Master) WRITE (out,270) subname, TRIM(fname)
          exit_flag=4
          RETURN
  40      CONTINUE
          END DO
        ENDIF

#ifdef VERIFICATION
        !
        !  Allocate and initialize observation arrays.
        !
        CALL initialize_fourdvar
#endif

        !-----------------------------------------------------------------------
        !  Initialize nonlinear model state variables over all nested grids,
        !  if applicable.
        !-----------------------------------------------------------------------

        DO ng=1,Ngrids
          CALL initial (ng)
          IF (exit_flag.ne.NoError) RETURN
        END DO
        !
        !  Initialize run or ensemble counter.
        !
        Nrun=1

#ifdef VERIFICATION
        !
        !  Create out NetCDF file containing model solution at observation
        !  locations.
        !
        IF (Nrun.eq.1) THEN
          DO ng=1,Ngrids
            LdefMOD(ng)=.TRUE.
            wrtNLmod(ng)=.TRUE.
            CALL def_mod (ng)
            IF (exit_flag.ne.NoError) RETURN
          END DO
        END IF
#endif
   230  FORMAT (2x,a,a)
   270  FORMAT (/,'(',a,'): could not find input file:  ',a)
!EOC
    end subroutine roms_ini_data

!==============================================================================
!BOP
!   !ROUTINE: roms_advance_step
!   !INTERFACE:

    subroutine roms_advance_step

!   !DESCRIPTION:
!       Advance ROMS nonlinear model integration time by one timestep
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

        implicit none

!EOP
!BOC
        integer :: ng

        !
        !  Set time indices and time clock.
        !
        DO ng=1,Ngrids
            iic(ng)=iic(ng)+1
            nstp(ng)=1+MOD(iic(ng)-ntstart(ng),2)
            nnew(ng)=3-nstp(ng)
            nrhs(ng)=nstp(ng)
            time(ng)=time(ng)+dt(ng)
            tdays(ng)=time(ng)*sec2day
            CALL time_string (time(ng), time_code(ng))
        END DO

!EOC
    end subroutine roms_advance_step

!==============================================================================
!BOP
!   !ROUTINE: roms_set_state
!   !INTERFACE:

    subroutine roms_set_state

!   !DESCRIPTION:
!       Setup nonlinear model state before integrating to the next timestep
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

        implicit none

!EOP
!BOC
        ! --- Local variables ---

        integer :: ng, subs, tile, thread
!
!-----------------------------------------------------------------------
!  Read in required data, if any, from input NetCDF files.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          CALL get_data (ng)
          IF (exit_flag.ne.NoError) RETURN
        END DO
!
!-----------------------------------------------------------------------
!  If applicable, process input data: time interpolate between data
!  snapshots.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*thread,subs*(thread+1)-1,+1
              CALL set_data (ng, TILE)
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
        IF (exit_flag.ne.NoError) RETURN

# if defined W4DPSAS || defined NLM_OUTER || \
     defined W4DPSAS_SENSITIVITY
!
!-----------------------------------------------------------------------
!  If appropriate, add convolved adjoint solution impulse forcing to
!  the nonlinear model solution. Notice that the forcing is only needed
!  after finishing all the inner loops. The forcing is continuous.
!  That is, it is time interpolated at every time-step from available
!  snapshots (FrequentImpulse=TRUE).
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          IF (FrequentImpulse(ng)) THEN
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
            DO thread=0,numthreads-1
              subs=NtileX(ng)*NtileE(ng)/numthreads
              DO tile=subs*thread,subs*(thread+1)-1,+1
                CALL forcing (ng, TILE, kstp(ng), nstp(ng))
              END DO
            END DO
!$OMP END PARALLEL DO
          END IF
        END DO
# endif
!
!-----------------------------------------------------------------------
!  Initialize all time levels and compute other initial fields.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          IF (iic(ng).eq.ntstart(ng)) THEN
!
!  Initialize free-surface and compute initial level thicknesses and
!  depths.
!
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(ng,numthreads)
            DO thread=0,numthreads-1
              subs=NtileX(ng)*NtileE(ng)/numthreads
              DO tile=subs*thread,subs*(thread+1)-1,+1
                CALL ini_zeta (ng, TILE, iNLM)
                CALL set_depth (ng, TILE)
              END DO
            END DO
!$OMP END PARALLEL DO
!
!  Initialize other state variables.
!
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(ng,numthreads)
            DO thread=0,numthreads-1
              subs=NtileX(ng)*NtileE(ng)/numthreads
              DO tile=subs*(thread+1)-1,subs*thread,-1
                CALL ini_fields (ng, TILE, iNLM)
              END DO
            END DO
!$OMP END PARALLEL DO
          END IF
        END DO
!
!-----------------------------------------------------------------------
!  Compute horizontal mass fluxes (Hz*u/n and Hz*v/m), density related
!  quatities and report global diagnostics.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*thread,subs*(thread+1)-1,+1
              CALL set_massflux (ng, TILE)
# ifndef TS_FIXED
              CALL rho_eos (ng, TILE)
# endif
              CALL diag (ng, TILE)
# ifdef TLM_CHECK
              CALL nl_dotproduct (ng, TILE, Lnew(ng))
# endif
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
        IF (exit_flag.ne.NoError) RETURN
# ifdef NESTING
        CALL nesting (5)
# endif

!EOC
    end subroutine roms_set_state

!==============================================================================
!BOP
!   !ROUTINE: roms_radiation_stress
!   !INTERFACE:

    subroutine roms_radiation_stress

!   !DESCRIPTION:
!       Compute radiation stress on the ocean
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

        implicit none

!EOP
!BOC
        ! --- Local variables ---

        integer :: ng, subs, tile, thread

# ifdef NEARSHORE_MELLOR
!
!-----------------------------------------------------------------------
!  Compute radiation stress terms.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*(thread+1)-1,subs*thread,-1
              CALL radiation_stress (ng, TILE)
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
#  ifdef NESTING
        CALL nesting (8)
#  endif
# endif

!EOC
    end subroutine roms_radiation_stress

!==============================================================================
!BOP
!   !ROUTINE: roms_step
!   !INTERFACE:

    subroutine roms_step

!   !DESCRIPTION:
!       Performs ROMS nonlinear model integration step
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

        implicit none

!EOP
!BOC
        ! --- Local variables ---

        integer :: my_iif, next_indx1, ng, subs, tile, thread

# ifdef FLOATS
        integer :: Lend, Lstr, chunk_size
# endif

!-----------------------------------------------------------------------
!  Set fields for vertical boundary conditions. Process tidal forcing,
!  if any.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*thread,subs*(thread+1)-1,+1
# if defined BULK_FLUXES && !defined CCSMCOUPLED
#  if defined FOUR_DVAR && defined NL_BULK_FLUXES
              IF (Nrun.eq.1) CALL bulk_flux (ng, TILE)
#  else
              CALL bulk_flux (ng, TILE)
#  endif
# endif
# ifdef BBL_MODEL
              CALL bblm (ng, TILE)
# endif
              CALL set_vbc (ng, TILE)
# if defined SSH_TIDES || defined UV_TIDES
              CALL set_tides (ng, TILE)
# endif
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
# ifdef NESTING
        CALL nesting (9)
# endif

# ifdef ADJUST_BOUNDARY
!
!-----------------------------------------------------------------------
!  Interpolate open boundary increments and adjust open boundary.
!  Load open boundary into storage arrays. Skip the last output
!  timestep.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          IF (iic(ng).lt.(ntend(ng)+1)) THEN
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(ng,numthreads)
            DO thread=0,numthreads-1
              subs=NtileX(ng)*NtileE(ng)/numthreads
              DO tile=subs*thread,subs*(thread+1)-1,+1
                CALL obc_adjust (ng, TILE, Lbinp(ng))
                CALL load_obc (ng, TILE, Lbout(ng))
              END DO
            END DO
!$OMP END PARALLEL DO
          END IF
        END DO
# endif

# if defined ADJUST_STFLUX || defined ADJUST_WSTRESS
!
!-----------------------------------------------------------------------
!  Interpolate surface forcing increments and adjust surface forcing.
!  Load surface forcing into storage arrays. Skip the last output
!  timestep.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          IF (iic(ng).lt.(ntend(ng)+1)) THEN
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(ng,numthreads)
            DO thread=0,numthreads-1
              subs=NtileX(ng)*NtileE(ng)/numthreads
              DO tile=subs*thread,subs*(thread+1)-1,+1
                CALL frc_adjust (ng, TILE, Lfinp(ng))
              END DO
            END DO
!$OMP END PARALLEL DO
          END IF
        END DO
# endif
!
!-----------------------------------------------------------------------
!  Compute time-dependent vertical/horizontal mixing coefficients for
!  momentum and tracers. Compute S-coordinate vertical velocity,
!  diagnostically from horizontal mass divergence.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile)                             &
!$OMP&            SHARED(nstp,numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*(thread+1)-1,subs*thread,-1
# if defined ANA_VMIX
              CALL ana_vmix (ng, TILE, iNLM)
# elif defined LMD_MIXING
              CALL lmd_vmix (ng, TILE)
# elif defined BVF_MIXING
              CALL bvf_mix (ng, TILE)
# endif
# if defined DIFF_3DCOEF || defined VISC_3DCOEF
              CALL hmixing (ng, TILE)
# endif
              CALL omega (ng, TILE)
              CALL wvelocity (ng, TILE, nstp(ng))
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
# ifdef NESTING
        CALL nesting (10)
# endif
!
!-----------------------------------------------------------------------
!  Set free-surface to it time-averaged value.  If applicable,
!  accumulate time-averaged output data which needs a irreversible
!  loop in shared-memory jobs.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*thread,subs*(thread+1)-1,+1      ! irreversible
              CALL set_zeta (ng, TILE)
#  ifdef DIAGNOSTICS
              CALL set_diags (ng, TILE)
#  endif
#  ifdef AVERAGES
              CALL set_avg (ng, TILE)
#  endif
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
#  ifdef NESTING
        CALL nesting (11)
#  endif
!
!-----------------------------------------------------------------------
!  If appropriate, write out fields into output NetCDF files.  Notice
!  that IO data is written in delayed and serial mode.  Exit if last
!  time step.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          CALL output (ng)
          IF ((exit_flag.ne.NoError).or.                                &
     &        ((iic(ng).eq.(ntend(ng)+1)).and.(ng.eq.Ngrids))) RETURN
        END DO
!
!-----------------------------------------------------------------------
!  Compute right-hand-side terms for 3D equations.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*(thread+1)-1,subs*thread,-1
              CALL rhs3d (ng, TILE)
#  ifdef MY25_MIXING
              CALL my25_prestep (ng, TILE)
#  elif defined GLS_MIXING
              CALL gls_prestep (ng, TILE)
#  endif
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
#  ifdef NESTING
        CALL nesting (12)
#  endif
!
!-----------------------------------------------------------------------
!  Solve the vertically integrated primitive equations for the
!  free-surface and barotropic momentum components.
!-----------------------------------------------------------------------
!
        LOOP_2D : DO my_iif=1,MAXVAL(nfast)+1
!
!  Set time indices for predictor step. The PREDICTOR_2D_STEP switch
!  it is assumed to be false before the first time-step.
!
          DO ng=1,Ngrids
            next_indx1=3-indx1(ng)
            IF (.not.PREDICTOR_2D_STEP(ng).and.                         &
     &          my_iif.le.(nfast(ng)+1)) THEN
              PREDICTOR_2D_STEP(ng)=.TRUE.
              iif(ng)=my_iif
              IF (FIRST_2D_STEP) THEN
                kstp(ng)=indx1(ng)
              ELSE
                kstp(ng)=3-indx1(ng)
              END IF
              knew(ng)=3
              krhs(ng)=indx1(ng)
            END IF
!
!  Predictor step - Advance barotropic equations using 2D time-step
!  ==============   predictor scheme.  No actual time-stepping is
!  performed during the auxiliary (nfast+1) time-step. It is needed
!  to finalize the fast-time averaging of 2D fields, if any, and
!  compute the new time-evolving depths.
!
            IF (my_iif.le.(nfast(ng)+1)) THEN
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(ng,numthreads)
              DO thread=0,numthreads-1
                subs=NtileX(ng)*NtileE(ng)/numthreads
                DO tile=subs*(thread+1)-1,subs*thread,-1
                  CALL step2d (ng, TILE)
                END DO
              END DO
!$OMP END PARALLEL DO
            END IF
          END DO
# ifdef NESTING
          CALL nesting (14)
# endif
!
!  Set time indices for corrector step.
!
          DO ng=1,Ngrids
            IF (PREDICTOR_2D_STEP(ng)) THEN
              PREDICTOR_2D_STEP(ng)=.FALSE.
              knew(ng)=next_indx1
              kstp(ng)=3-knew(ng)
              krhs(ng)=3
              IF (iif(ng).lt.(nfast(ng)+1)) indx1(ng)=next_indx1
            END IF
!
!  Corrector step - Apply 2D time-step corrector scheme.  Notice that
!  ==============   there is not need for a corrector step during the
!  auxiliary (nfast+1) time-step.
!
            IF (iif(ng).lt.(nfast(ng)+1)) THEN
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(ng,numthreads)
              DO thread=0,numthreads-1
                subs=NtileX(ng)*NtileE(ng)/numthreads
                DO tile=subs*thread,subs*(thread+1)-1,+1
                  CALL step2d (ng, TILE)
                END DO
              END DO
!$OMP END PARALLEL DO
            END IF
          END DO
# ifdef NESTING
          CALL nesting (15)
# endif
        END DO LOOP_2D
# ifdef NESTING
        CALL nesting (21)
# endif
!
!-----------------------------------------------------------------------
!  Recompute depths and thicknesses using the new time filtered
!  free-surface.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*(thread+1)-1,subs*thread,-1
              CALL set_depth (ng, TILE)
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
!
!-----------------------------------------------------------------------
!  Time-step 3D momentum equations.
!-----------------------------------------------------------------------
!
!  Time-step 3D momentum equations and couple with vertically
!  integrated equations.
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*(thread+1)-1,subs*thread,-1
              CALL step3d_uv (ng, TILE)
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
# ifdef NESTING
        CALL nesting (16)
# endif
!
!-----------------------------------------------------------------------
!  Time-step vertical mixing turbulent equations and passive tracer
!  source and sink terms, if applicable.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(nnew,numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*thread,subs*(thread+1)-1,+1
              CALL omega (ng, TILE)
# ifdef MY25_MIXING
              CALL my25_corstep (ng, TILE)
# elif defined GLS_MIXING
              CALL gls_corstep (ng, TILE)
# endif
# ifdef BIOLOGY
              CALL biology (ng, TILE)
# endif
# ifdef SEDIMENT
              CALL sediment (ng, TILE)
# endif
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
# ifdef NESTING
        CALL nesting (17)
# endif

# ifndef TS_FIXED
!
!-----------------------------------------------------------------------
!  Time-step tracer equations.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP PARALLEL DO PRIVATE(thread,subs,tile) SHARED(numthreads)
          DO thread=0,numthreads-1
            subs=NtileX(ng)*NtileE(ng)/numthreads
            DO tile=subs*(thread+1)-1,subs*thread,-1
              CALL step3d_t (ng, TILE)
            END DO
          END DO
!$OMP END PARALLEL DO
        END DO
#  ifdef NESTING
        CALL nesting (18)
#  endif
# endif

# ifdef FLOATS
!
!-----------------------------------------------------------------------
!  Compute Lagrangian drifters trajectories.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          IF (Lfloats(Ng)) THEN
!$OMP PARALLEL DO PRIVATE(thread,chunk_size,Lstr,Lend)                  &
!$OMP&            SHARED(numthreads,Nfloats)
            DO thread=0,numthreads-1
              chunk_size=(Nfloats(ng)+numthreads-1)/numthreads
              Lstr=1+thread*chunk_size
              Lend=MIN(Nfloats(ng),Lstr+chunk_size-1)
              CALL step_floats (ng, Lstr, Lend)
            END DO
!$OMP END PARALLEL DO
!
!  Shift floats time indices.
!
            nfp1(ng)=MOD(nfp1(ng)+1,NFT+1)
            nf  (ng)=MOD(nf  (ng)+1,NFT+1)
            nfm1(ng)=MOD(nfm1(ng)+1,NFT+1)
            nfm2(ng)=MOD(nfm2(ng)+1,NFT+1)
            nfm3(ng)=MOD(nfm3(ng)+1,NFT+1)
          END IF
        END DO
# endif

!EOC
    end subroutine roms_step

!==============================================================================
!BOP
!   !ROUTINE: roms_halo_update_fluxes
!   !INTERFACE:

    subroutine roms_rotate_vector(ng,                  &
                                  LBi, UBi, LBj, UBj,  &
                                  u, v, rot)

!   !DESCRIPTION:
!       Rotate a vector field defined on ROMS rho grid to/from the coupler's
!       (lon,lat) grid according to the sign of optional argument rot:
!       * rot > 0:  Rotate from ROMS to coupler (xi,eta) -> (lon,lat)
!       * rot < 0:  Rotate from coupler to ROMS (lon,lat) -> (xi,eta)
!
!   !REVISION HISTORY:
!       Aug 08, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:
!
        use mod_grid, only : GRID

!   !INPUT/OUTPUT PARAMETERS:

        integer,            intent(in)    :: ng
        integer,            intent(in)    :: LBi, UBi, LBj, UBj
        real(r8),           intent(inout) :: u(LBi:UBi,LBj:UBj), &
                                             v(LBi:UBi,LBj:UBj)
        real(r8), optional, intent(in)    :: rot

!EOP
!BOC
# ifdef CURVGRID

        ! --- Local variables ---

        integer :: i, j
        real(r8) :: rs, sa, us, vs

        if (present(rot)) then
           rs = sign(1._r8, real(rot, kind = r8))
        else
           rs = 1._r8
        end if

        do j = LBj, UBj
           do i = LBi, UBi
              us = u(i,j)
              vs = v(i,j)
              sa = sign(GRID(ng) % SinAngler(i,j), rs)
              u(i,j) = us * GRID(ng) % CosAngler(i,j) - vs * sa
              v(i,j) = vs * GRID(ng) % CosAngler(i,j) + us * sa
           end do
        end do

# endif
!EOC
    end subroutine roms_rotate_vector

!==============================================================================
!BOP
!   !ROUTINE: roms_halo_update_fluxes
!   !INTERFACE:

    subroutine roms_halo_update_fluxes(ng, tile,            &
                                       LBi, UBi, LBj, UBj,  &
                                       Pair,                &
                                       lrflx, srflx,        &
                                       lhflx, shflx,        &
                                       stflx, rain,         &
                                       sustr, svstr)

!   !DESCRIPTION:
!       Performs halo update for ROMS surface fluxes
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:
!
!-----------------------------------------------------------------------
!  Exchange boundary data and distribute over processors
!-----------------------------------------------------------------------
!
        use mod_scalars, only : isalt, itemp
        use mod_param,   only : iNLM, NghostPoints, NT

# if defined EW_PERIODIC || defined NS_PERIODIC
        use exchange_2d_mod
# endif
# ifdef DISTRIBUTE
        use mp_exchange_mod, only : mp_exchange2d
# endif

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        integer, intent(in) :: ng, tile
        integer, intent(in) :: LBi, UBi, LBj, UBj

# ifdef ASSUMED_SHAPE
        real(r8), intent(inout) :: Pair (LBi:,LBj:)
        real(r8), intent(inout) :: lrflx(LBi:,LBj:)
        real(r8), intent(inout) :: srflx(LBi:,LBj:)
        real(r8), intent(inout) :: lhflx(LBi:,LBj:)
        real(r8), intent(inout) :: shflx(LBi:,LBj:)
        real(r8), intent(inout) :: stflx(LBi:,LBj:,:)
        real(r8), intent(inout) :: rain (LBi:,LBj:)
        real(r8), intent(inout) :: sustr(LBi:,LBj:)
        real(r8), intent(inout) :: svstr(LBi:,LBj:)
# else
        real(r8), intent(inout) :: Pair (LBi:UBi,LBj:UBj)
        real(r8), intent(inout) :: lrflx(LBi:UBi,LBj:UBj)
        real(r8), intent(inout) :: srflx(LBi:UBi,LBj:UBj)
        real(r8), intent(inout) :: lhflx(LBi:UBi,LBj:UBj)
        real(r8), intent(inout) :: shflx(LBi:UBi,LBj:UBj)
        real(r8), intent(inout) :: stflx(LBi:UBi,LBj:UBj,NT(ng))
        real(r8), intent(inout) :: rain (LBi:UBi,LBj:UBj)
        real(r8), intent(inout) :: sustr(LBi:UBi,LBj:UBj)
        real(r8), intent(inout) :: svstr(LBi:UBi,LBj:UBj)
# endif

!EOP
!BOC

        ! --- Local variables ---

# ifdef DISTRIBUTE
#  ifdef EW_PERIODIC
        logical :: EWperiodic=.TRUE.
#  else
        logical :: EWperiodic=.FALSE.
#  endif
#  ifdef NS_PERIODIC
        logical :: NSperiodic=.TRUE.
#  else
        logical :: NSperiodic=.FALSE.
#  endif
# endif


#  if defined EW_PERIODIC || defined NS_PERIODIC
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, Pair )
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, lrflx)
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, srflx)
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, lhflx)
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, shflx)
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, stflx(:,:,itemp))
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, stflx(:,:,isalt))
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, rain )
        call exchange_u2d_tile (ng, tile, LBi, UBi, LBj, UBj, sustr)
        call exchange_v2d_tile (ng, tile, LBi, UBi, LBj, UBj, svstr)
#  endif
#  ifdef DISTRIBUTE
        call mp_exchange2d (ng, tile, iNLM, 4,                            &
                            LBi, UBi, LBj, UBj,                           &
                            NghostPoints, EWperiodic, NSperiodic,         &
                            Pair, lrflx, srflx, rain)
        if (exit_flag.ne.NoError) return

        call mp_exchange2d (ng, tile, iNLM, 4,                            &
                            LBi, UBi, LBj, UBj,                           &
                            NghostPoints, EWperiodic, NSperiodic,         &
                            lhflx, shflx, stflx(:,:,itemp), stflx(:,:,isalt))
        if (exit_flag.ne.NoError) return

        call mp_exchange2d (ng, tile, iNLM, 2,                            &
                            LBi, UBi, LBj, UBj,                           &
                            NghostPoints, EWperiodic, NSperiodic,         &
                            sustr, svstr)
#  endif

!EOC
    end subroutine roms_halo_update_fluxes

!==============================================================================
!BOP
!   !ROUTINE: roms_halo_update_r2d_ab
!   !INTERFACE: roms_halo_update_r2d

    subroutine roms_halo_update_r2d_ab( ng, tile,          &
                                        LBi, UBi, LBj, UBj, &
                                        A,   B )

!   !DESCRIPTION:
!       Performs halo update for ROMS 2D arrays on rho-points
!
!   !REVISION HISTORY:
!       Apr 23, 2012 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

        use mod_param,   only : iNLM, NghostPoints

# if defined EW_PERIODIC || defined NS_PERIODIC
        use exchange_2d_mod
# endif
# ifdef DISTRIBUTE
        use mp_exchange_mod, only : mp_exchange2d
# endif

        implicit none

!   !INPUT/OUTPUT PARAMETERS:

        integer, intent(in) :: ng, tile
        integer, intent(in) :: LBi, UBi, LBj, UBj

# ifdef ASSUMED_SHAPE
        real(r8), dimension(LBi:   ,LBj:   ), intent(inout) :: A, B
# else
        real(r8), dimension(LBi:UBi,LBj:UBj), intent(inout) :: A, B
# endif

!EOP
!BOC
        ! --- Local variables ---
# ifdef DISTRIBUTE
#  ifdef EW_PERIODIC
        logical :: EWperiodic = .true.
#  else
        logical :: EWperiodic = .false.
#  endif
#  ifdef NS_PERIODIC
        logical :: NSperiodic = .true.
#  else
        logical :: NSperiodic = .false.
#  endif
# endif

        ! --- Begin

# if defined EW_PERIODIC || defined NS_PERIODIC
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, A)
        call exchange_r2d_tile (ng, tile, LBi, UBi, LBj, UBj, B)
# endif

# ifdef DISTRIBUTE
        call mp_exchange2d (ng, tile, iNLM, 2,                      &
                            LBi, UBi, LBj, UBj,                     &
                            NghostPoints, EWperiodic, NSperiodic,   &
                            A, B)
#  endif

!EOC
    end subroutine roms_halo_update_r2d_ab

#endif

end module roms_run_mod
