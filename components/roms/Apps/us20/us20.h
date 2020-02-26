/*
**
** ROMS definitions for CRESM/US20
** Aug 09, 2012
**
** Application flag:   US20
*/

/* define coupling within CESM/CCSM */
#define CCSMCOUPLED

#ifdef CCSMCOUPLED
# define ATM_PRESS
# define BULK_FLUXES
# define SHORTWAVE
# define SOLAR_SOURCE
#endif

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

/* curvilinear grid */
#define CURVGRID


#define UV_ADV
#define UV_COR
#define UV_VIS2
#define MIX_S_UV
#define UV_QDRAG
#define TS_MPDATA   /* Using MPDATA instead of C4 ADVECTION to reduce checkerboard pattern */


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
#undef  GLS_MIXING
#define MY25_MIXING
#undef  LMD_MIXING

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

# define SOUTH_FSCHAPMAN
# define SOUTH_M2FLATHER
# define SOUTH_M3RADIATION
# define SOUTH_M3NUDGING
# define SOUTH_TRADIATION
# define SOUTH_TNUDGING
# define SOUTH_VOLCONS

# define NORTH_FSCHAPMAN
# define NORTH_M2FLATHER
# define NORTH_M3RADIATION
# define NORTH_M3NUDGING
# define NORTH_TRADIATION
# define NORTH_TNUDGING
# define NORTH_VOLCONS

# define WEST_FSCHAPMAN
# define WEST_M2FLATHER
# define WEST_M3RADIATION
# define WEST_M3NUDGING
# define WEST_TRADIATION
# define WEST_TNUDGING
# define WEST_VOLCONS

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
