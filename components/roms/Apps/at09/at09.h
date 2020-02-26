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

/* define coupling within CESM/CCSM */
#define CCSMCOUPLED

#ifdef CCSMCOUPLED
# define ATM_PRESS
# define BULK_FLUXES
# define SHORTWAVE
# define SOLAR_SOURCE
#endif

# undef T_PASSIVE
# define INLINE_2DIO
# undef UV_SADVECTION
# undef TS_FIXED
# undef AVERAGES	/* define if averaged fields needed */
# undef STATIONS
# undef FLOATS

# undef RST_SINGLE
# undef QCORRECTION
# undef SCORRECTION
# undef SRELAXATION
# undef SPONGE

# define UV_ADV
# define DJ_GRADPS
# define UV_COR
# define UV_QDRAG
# define UV_VIS2
# undef UV_VIS4
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
# undef  UV_PSOURCE
# undef  TS_PSOURCE

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

/* bottom surface ANA Flux */
# define ANA_BSFLUX
# define ANA_BTFLUX

/* define water mass relaxation */
!# define TCLIMATOLOGY
!# define TCLM_NUDGING

!#define RELATIVE_WIND
