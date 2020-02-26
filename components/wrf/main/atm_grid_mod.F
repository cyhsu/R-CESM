!BOP

module atm_grid_mod

!   !DESCRIPTION:
!       Auxiliary routines for WRF grid
!
!   !REVISION HISTORY:
!       Apr 23, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !USES:

    use shr_kind_mod,  only : r8 => SHR_KIND_R8
    use shr_const_mod, only : SHR_CONST_PI, &
                              SHR_CONST_REARTH

    use module_domain, only : domain

    implicit none

    private

    public :: atm_grid_cell_area
    public :: atm_grid_area

contains

!==============================================================================
!BOP
!   !ROUTINE: atm_grid_cell_area
!
!   !DESCRIPTION:
!       Compute WRF grid cell area
!
!   !REVISION HISTORY:
!       Apr 23, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !INTERFACE:

    real(r8) function atm_grid_cell_area(i, j, ide, jde, lon, lat, ims, ime, jms, jme)

!   !INPUT PARAMETERS:

        integer,                             intent(in) :: i, j, ide, jde, &
                                                           ims, ime, jms, jme
        real   , dimension(ims:ime,jms:jme), intent(in) :: lon, lat

!EOP
!BOC
        ! --- Local variables ---
        integer :: ib, ic, ie, is, jb, jc, je, js, k
        integer :: sym
        real(r8), dimension(0:3) :: x, y
        real(r8), parameter :: cv2rad = SHR_CONST_PI/720._r8

        ! --- Begin
        atm_grid_cell_area = 0._r8
        x = 0._r8
        y = 0._r8

        ib = max(i-1,1)
        ie = min(i+1,ide)
        is = ie - ib

        jb = max(j-1,1)
        je = min(j+1,jde)
        js = je - jb

        ! --- Compute symmetry # to estimate grid cell area at grid edges ---
        sym = (3-is)*(3-js)

        ! --- Compute grid cell corner on a counterclockwise path ---
        k = 0
        do jc = jb, je, js
           do ic = ib, ie, is
              x(k) = lon(i,j) + lon(ic,j) + lon(i,jc) + lon(ic,jc)
              if (abs(lon(ic,j )-lon(i,j)) > 180._r8) x(k) = x(k) + 360._r8
              if (abs(lon(i ,jc)-lon(i,j)) > 180._r8) x(k) = x(k) + 360._r8
              if (abs(lon(ic,jc)-lon(i,j)) > 180._r8) x(k) = x(k) + 360._r8
              x(k) = cv2rad * x(k)
              y(k) = cv2rad * (lat(i,j) + lat(ic,j) + lat(i,jc) + lat(ic,jc))
              k = k + 1
           end do
           ic = ib
           ib = ie
           ie = ic
           is = -is
        end do

        ! --- Compute grid cell area using line integrals ---
        do ib = 0, 3
           ie = mod(ib+1,4)
           atm_grid_cell_area = atm_grid_cell_area &
                              + (x(ib)-x(ie)) * (sin(y(ib)) + sin(y(ie)))
        end do
        atm_grid_cell_area = 0.5_r8 * sym * atm_grid_cell_area
!EOC
    end function atm_grid_cell_area

!==============================================================================
!BOP
!   !ROUTINE: atm_grid_area
!
!   !DESCRIPTION:
!       Compute WRF grid cell area using map factors
!
!   !REVISION HISTORY:
!       Apr 23, 2014 - Raffaele Montuoro <rmontuoro@tamu.edu> - initial release
!
!   !INTERFACE:

    real(r8) function atm_grid_area(grid, i, j)

!   !INPUT PARAMETERS:

        type(domain), intent(in) :: grid
        integer,      intent(in) :: i, j
!EOP
!BOC
        ! --- Local variables ---
        real(r8), parameter :: rearthinvsq = 1._r8/(SHR_CONST_REARTH*SHR_CONST_REARTH)

        ! --- Begin
        atm_grid_area = rearthinvsq * grid % dx * grid % dy &
                        / ( grid % msftx(i,j) * grid % msfty(i,j) )
!EOC
    end function atm_grid_area
!EOB
end module atm_grid_mod
