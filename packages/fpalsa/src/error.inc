{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/error.h
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

//#ifndef __ALSA_ERROR_H
//#define __ALSA_ERROR_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup Error Error handling
 *  Error handling macros and functions.
 *  \{
 *)

const
  SND_ERROR_BEGIN                = 500000;               { Lower boundary of sound error codes. }
  SND_ERROR_INCOMPATIBLE_VERSION = SND_ERROR_BEGIN + 0;  { Kernel/library protocols are not compatible. }
  SND_ERROR_ALISP_NIL            = SND_ERROR_BEGIN + 1;  { Lisp encountered an error during acall. }

function snd_strerror(errnum: cint): PChar; cdecl; external libasound;

(**
 * \brief Error handler callback.
 * \param file Source file name.
 * \param line Line number.
 * \param function Function name.
 * \param err Value of \c errno, or 0 if not relevant.
 * \param fmt \c printf(3) format.
 * \param ... \c printf(3) arguments.
 *
 * A function of this type is called by the ALSA library when an error occurs.
 * This function usually shows the message on the screen, and/or logs it.
 *)
type
  snd_lib_error_handler_t = procedure(_file: PChar; line: cint; _function: PChar; err: cint; fmt: PChar; dot_dot_dot: array of const) { __attribute__ ((format (printf, 5, 6))) }; cdecl;

var
  snd_lib_error: snd_lib_error_handler_t; external libasound;

function snd_lib_error_set_handler(handler: snd_lib_error_handler_t): cint; cdecl; external libasound;

{#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 95)
#define SNDERR(...) snd_lib_error(__FILE__, __LINE__, __FUNCTION__, 0, __VA_ARGS__) /**< Shows a sound error message. */
#define SYSERR(...) snd_lib_error(__FILE__, __LINE__, __FUNCTION__, errno, __VA_ARGS__) /**< Shows a system error message (related to \c errno). */
#else
#define SNDERR(args...) snd_lib_error(__FILE__, __LINE__, __FUNCTION__, 0, ##args) /**< Shows a sound error message. */
#define SYSERR(args...) snd_lib_error(__FILE__, __LINE__, __FUNCTION__, errno, ##args) /**< Shows a system error message (related to \c errno). */
#endif}

///** \} */

//#ifdef __cplusplus
//}
//#endif

{ Local error handler function type }
type
  snd_local_error_handler_t = procedure (_file: PChar; line: cint;
					 func: PChar; err: cint;
{$INFO va_list ???}
					 fmt: PChar; arg: va_list); cdecl;

function snd_lib_error_set_local(func: snd_local_error_handler_t): snd_local_error_handler_t; cdecl; external libasound;

//#endif /* __ALSA_ERROR_H */
