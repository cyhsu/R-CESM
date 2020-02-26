/*
** svn $Id: tamu.h 523 2011-01-05 03:21:38Z arango $
*******************************************************************************
** Copyright (c) 2002-2011 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for running ROMS within CESM/CCSM.
**
** Application flag:   CCSM
** Input script:       ccsm.in
*/

# define INLINE_2DIO
# define UV_ADV
# define DJ_GRADPS
# define UV_COR
# define UV_QDRAG
# define UV_VIS2
# define MIX_S_UV
# define TS_MPDATA
# define TS_DIF2
# define MIX_GEO_TS

# define SOLVE3D
# define SALINITY
# define NONLIN_EOS
# define SPLINES            
# define MASKING

/* define coupling within CESM/CCSM */
# define CCSMCOUPLED

# ifdef CCSMCOUPLED
#  define ATM_PRESS
#  define BULK_FLUXES
#  define EMINUSP
#  define SHORTWAVE
#  define SOLAR_SOURCE
# endif

/* surface forcing */
# ifdef BULK_FLUXES
#  define LONGWAVE_OUT
#  define EMINUSP
#  define SOLAR_SOURCE
#  define DIURNAL_SRFLUX
# else
#  define ANA_SSFLUX
#  define ANA_SMFLUX
#  define ANA_STFLUX
# endif

/* horizontal mixing option */
# define VISC_GRID
# define DIFF_GRID

/* turbulence mixing scheme */
# ifdef LMD_MIXING
#  define LMD_RIMIX
#  define LMD_CONVEC
#  define LMD_SKPP
#  define LMD_NONLOCAL
# endif

# define MY25_MIXING
# ifdef MY25_MIXING
#  define N2S2_HORAVG
#  define KANTHA_CLAYSON
# endif

# ifdef GLS_MIXING
#  define KANTHA_CLAYSON
#  undef  CANUTO_A
#  define N2S2_HORAVG
# endif

/* Select  Biological model option */
# undef NPZD_POWELL
# ifdef NPZD_POWELL
#  define ANA_SPFLUX
#  define ANA_BPFLUX
#  undef CONST_PAR
#  define SPITZ
# endif

/*define boundary conditon */
# define RADIATION_2D
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

# define ANA_BSFLUX
# define ANA_BTFLUX

