{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/global.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Application interface library for the ALSA driver
 *}
{*
 *   This library is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU Lesser General Public License as
 *   published by the Free Software Foundation; either version 2.1 of
 *   the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 *}

//#ifndef __ALSA_GLOBAL_H_
//#define __ALSA_GLOBAL_H_

///* for timeval and timespec */
//#include <time.h>

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup Global Global defines and functions
 *  Global defines and functions.
 *  \par
 *  The ALSA library implementation uses these macros and functions.
 *  Most applications probably do not need them.
 *  \{
 *)

function snd_asoundlib_version: PChar; cdecl; external libasound;

{#ifndef ATTRIBUTE_UNUSED
/** do not print warning (gcc) when function parameter is not used */
#define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#endif}

(*
#ifdef PIC /* dynamic build */

/** \hideinitializer \brief Helper macro for #SND_DLSYM_BUILD_VERSION. */
#define __SND_DLSYM_VERSION(name, version) _ ## name ## version
/**
 * \hideinitializer
 * \brief Appends the build version to the name of a versioned dynamic symbol.
 */
#define SND_DLSYM_BUILD_VERSION(name, version) char __SND_DLSYM_VERSION(name, version);

#else /* static build */

struct snd_dlsym_link {
	struct snd_dlsym_link *next;
	const char *dlsym_name;
	const void *dlsym_ptr;
};

extern struct snd_dlsym_link *snd_dlsym_start;

/** \hideinitializer \brief Helper macro for #SND_DLSYM_BUILD_VERSION. */
#define __SND_DLSYM_VERSION(prefix, name, version) _ ## prefix ## name ## version
/**
 * \hideinitializer
 * \brief Appends the build version to the name of a versioned dynamic symbol.
 */
#define SND_DLSYM_BUILD_VERSION(name, version) \
  static struct snd_dlsym_link __SND_DLSYM_VERSION(snd_dlsym_, name, version); \
  void __SND_DLSYM_VERSION(snd_dlsym_constructor_, name, version) (void) __attribute__ ((constructor)); \
  void __SND_DLSYM_VERSION(snd_dlsym_constructor_, name, version) (void) { \
    __SND_DLSYM_VERSION(snd_dlsym_, name, version).next = snd_dlsym_start; \
    __SND_DLSYM_VERSION(snd_dlsym_, name, version).dlsym_name = # name; \
    __SND_DLSYM_VERSION(snd_dlsym_, name, version).dlsym_ptr = (void * )&name; \
    snd_dlsym_start = &__SND_DLSYM_VERSION(snd_dlsym_, name, version); \
  }

#endif
*)

{#ifndef __STRING
/** \brief Return 'x' argument as string */
#define __STRING(x)     #x
#endif

/** \brief Returns the version of a dynamic symbol as a string. */
#define SND_DLSYM_VERSION(version) __STRING(version)
}

function snd_dlopen(_file: PChar; mode: cint): Pointer; cdecl; external libasound;
function snd_dlsym(handle: Pointer; name, version: PChar): Pointer; cdecl; external libasound;
function snd_dlclose(handle: Pointer): cint; cdecl; external libasound;


///** \brief alloca helper macro. */
//#define __snd_alloca(ptr,type) do { *ptr = (type##_t *) alloca(type##_sizeof()); memset(*ptr, 0, type##_sizeof()); } while (0)

(**
 * \brief Internal structure for an async notification client handler.
 *
 * The ALSA library uses a pointer to this structure as a handle to an async
 * notification object. Applications don't access its contents directly.
 *)
type
  PPsnd_async_handler_t = ^Psnd_async_handler_t;
  Psnd_async_handler_t = ^snd_async_handler_t;
  snd_async_handler_t = record
  end;

(**
 * \brief Async notification callback.
 *
 * See the #snd_async_add_handler function for details.
 *)
//typedef void (*snd_async_callback_t)(snd_async_handler_t *handler);
  snd_async_callback_t = procedure(handler: Psnd_async_handler_t); cdecl;

function snd_async_add_handler(handler: PPsnd_async_handler_t; fd: cint;
                               callback: snd_async_callback_t; private_data: Pointer): cint; cdecl; external libasound;
function snd_async_del_handler(handler: Psnd_async_handler_t): cint; cdecl; external libasound;
function snd_async_handler_get_fd(handler: Psnd_async_handler_t): cint; cdecl; external libasound;
function snd_async_handler_get_signo(handler: Psnd_async_handler_t): cint; cdecl; external libasound;
function snd_async_handler_get_callback_private(handler: Psnd_async_handler_t): Pointer; cdecl; external libasound;

type
  Psnd_shm_area = ^snd_shm_area;
  snd_shm_area = record
  end;

function snd_shm_area_create(shmid: cint; ptr: Pointer): Psnd_shm_area; cdecl; external libasound;
function snd_shm_area_share(area: Psnd_shm_area): Psnd_shm_area; cdecl; external libasound;
function snd_shm_area_destroy(area: Psnd_shm_area): cint; cdecl; external libasound;

function snd_user_file(_file: PChar; result: PPChar): cint; cdecl; external libasound;

(*#if !defined(_POSIX_C_SOURCE) && !defined(_POSIX_SOURCE)
struct timeval {
	time_t		tv_sec;		/* seconds */
	long		tv_usec;	/* microseconds */
};

struct timespec {
	time_t		tv_sec;		/* seconds */
	long		tv_nsec;	/* nanoseconds */
};
#endif*)

type
{ Timestamp }
  Psnd_timestamp_t = ^snd_timestamp_t;
  snd_timestamp_t = timeval;
{ Hi-res timestamp }
  Psnd_htimestamp_t = ^snd_htimestamp_t;
  snd_htimestamp_t = timespec;

//* \} */

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_GLOBAL_H */
