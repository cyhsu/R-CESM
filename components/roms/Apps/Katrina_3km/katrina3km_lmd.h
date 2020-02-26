/*
** svn $Id: Katrina_3km.h 25 2007-04-09 23:43:58Z jcwarner $
*******************************************************************************
** Copyright (c) 2002-2007 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Katrina3km
**
** Application flag:   Katrina3km_lmd
*/

/* define coupling within CESM/CCSM */
#define CCSMCOUPLED

#ifdef CCSMCOUPLED
# define ATM_PRESS
# define BULK_FLUXES
# define SHORTWAVE
# define SOLAR_SOURCE
#endif

#define UV_ADV
#define UV_COR
#define UV_VIS2
#define MIX_S_UV
#define UV_QDRAG
#define TS_C4HADVECTION
#define TS_C4VADVECTION


#define DJ_GRADPS
#define TS_DIF2
#define MIX_GEO_TS

#define SALINITY
#define SOLVE3D
#define SPLINES
#define NONLIN_EOS
#define MASKING

/* horizontal mixing option  */
# define VISC_GRID
# define DIFF_GRID

/* Turbulence closure */
#undef GLS_MIXING
#undef MY25_MIXING
#define LMD_MIXING

#if defined GLS_MIXING || defined MY25_MIXING
# define KANTHA_CLAYSON 
# define N2S2_HORAVG
#endif

# ifdef LMD_MIXING
#  define LMD_RIMIX
#  define LMD_CONVEC
#  define LMD_BKPP
#  define LMD_SKPP
#  define LMD_NONLOCAL
# endif


/* Boundary condition */
# define RADIATION_2D

#define WESTERN_WALL
#define NORTHERN_WALL

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

/* Biological module */
#undef  NPZD_POWELL

#if defined NPZD_POWELL
# define ANA_BIOLOGY
# define ANA_SPFLUX
# define ANA_BPFLUX
# define ANA_SRFLUX
#endif

#define ANA_BTFLUX
#define ANA_BSFLUX
