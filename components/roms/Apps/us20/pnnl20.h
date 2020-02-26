/*
** svn $Id: ias.h 8 2007-02-06 19:00:29Z arango $
*******************************************************************************
** Copyright (c) 2002-2007 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for CplAtl Application.
**
*/

# define INLINE_2DIO
!# define UV_SADVECTION
!# define TS_FIXED
# define AVERAGES
!# define AVERAGES_AKV
!# define AVERAGES_AKT
!# define AVERAGES_AKS
!# define AVERAGES_FLUXES
!# define STATIONS
!# define FLOATS
!# define DIAGNOSTICS_TS
!# define DIAGNOSTICS_UV
!# define AVERAGES_QUADRATIC

!# define RST_SINGLE
!# define QCORRECTION
!# define SCORRECTION
!# define SRELAXATION
!# define SPONGE

# define UV_ADV
# define DJ_GRADPS
# define UV_COR
# define UV_QDRAG
# define UV_VIS2
!# define UV_VIS4
# define MIX_S_UV
!# define TS_U3HADVECTION
!# define TS_SVADVECTION
# define TS_MPDATA
# define TS_DIF2
!# define TS_DIF4
# define MIX_GEO_TS

# define SOLVE3D
# define SALINITY
# define NONLIN_EOS
!# define CURVGRID
# define SPLINES            
# define MASKING

/* define River flows */
!#define  UV_PSOURCE
!#define  TS_PSOURCE

/* surface forcing */
# define BULK_FLUXES
# ifdef BULK_FLUXES
#  define LONGWAVE_OUT
!# define LONGWAVE
#  define EMINUSP
#  define SOLAR_SOURCE
!#  define DIURNAL_SRFLUX
# else
#  define ANA_SSFLUX
#  define ANA_SMFLUX
#  define ANA_STFLUX
# endif

/* horizontal mixing option */
# define VISC_GRID
# define DIFF_GRID

/* turbulence mixing scheme */
# define LMD_MIXING
# ifdef LMD_MIXING
#  define LMD_RIMIX
#  define LMD_CONVEC
#  define LMD_SKPP
#  define LMD_BKPP
#  define LMD_NONLOCAL
# endif

!# define MY25_MIXING
# ifdef MY25_MIXING
#  define N2S2_HORAVG
#  define KANTHA_CLAYSON
# endif

!# define  GLS_MIXING
# ifdef GLS_MIXING
#  define KANTHA_CLAYSON
#  undef  CANUTO_A
#  define N2S2_HORAVG
# endif

/* Select  Biological model option */
!# define RIVER_BIOLOGY

# undef NPZD_POWELL
# ifdef NPZD_POWELL
#  define ANA_SPFLUX
#  define ANA_BPFLUX
#  undef CONST_PAR
#  define SPITZ
# endif

/*define boundary conditon */
# define RADIATION_2D
!# define EASTERN_WALL
# define WESTERN_WALL

# define NORTH_FSCHAPMAN
# define NORTH_M2FLATHER
# define NORTH_M3RADIATION
# define NORTH_M3NUDGING
# define NORTH_TRADIATION
# define NORTH_TNUDGING
# define NORTH_VOLCONS

# define SOUTH_FSCHAPMAN
# define SOUTH_M2FLATHER
# define SOUTH_M3RADIATION
# define SOUTH_M3NUDGING
# define SOUTH_TRADIATION
# define SOUTH_TNUDGING
# define SOUTH_VOLCONS

# define EAST_FSCHAPMAN
# define EAST_M2FLATHER
# define EAST_M3RADIATION
# define EAST_M3NUDGING
# define EAST_TRADIATION
# define EAST_TNUDGING
# define EAST_VOLCONS

# define WEST_FSCHAPMAN
# define WEST_M2FLATHER
# define WEST_M3RADIATION
# define WEST_M3NUDGING
# define WEST_TRADIATION
# define WEST_TNUDGING
# define WEST_VOLCONS

# define ANA_BSFLUX
# define ANA_BTFLUX

/* define water mass relaxation */
!# define TCLIMATOLOGY
!# define TCLM_NUDGING

!#define RELATIVE_WIND
