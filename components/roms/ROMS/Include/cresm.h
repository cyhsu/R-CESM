/*
**
** ROMS additional settings for CRESM
** R. Montuoro <rmontuoro@tamu.edu>
** Mar 13, 2015
**
*/

/* define coupling within CESM/CCSM */
#define CCSMCOUPLED

#ifdef CCSMCOUPLED
# define ATM_PRESS
# define BULK_FLUXES
# define EMINUSP
# define SHORTWAVE
# define SOLAR_SOURCE
# define SOLVE3D
#endif
