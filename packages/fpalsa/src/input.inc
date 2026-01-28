{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/input.h
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

//#ifndef __ALSA_INPUT_H
//#define __ALSA_INPUT_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup Input Input Interface
 *
 *  The input functions present an interface similar to the stdio functions
 *  on top of different underlying input sources.
 *
 *  The #snd_config_load function uses such an input handle to be able to
 *  load configurations not only from standard files but also from other
 *  sources, e.g. from memory buffers.
 *
 *  \{
 *)

(**
 * \brief Internal structure for an input object.
 *
 * The ALSA library uses a pointer to this structure as a handle to an
 * input object. Applications don't access its contents directly.
 *)
type
  PPsnd_input_t = ^Psnd_input_t;
  Psnd_input_t = ^snd_input_t;
  snd_input_t = record
  end;

{ Input type. }
  snd_input_type_t = (
    { Input from a stdio stream. }
    SND_INPUT_STDIO,
    { Input from a memory buffer. }
    SND_INPUT_BUFFER);

function snd_input_stdio_open(inputp: PPsnd_input_t; _file, mode: PChar): cint; cdecl; external libasound;
{$INFO FILE* ???}
function snd_input_stdio_attach(inputp: PPsnd_input_t; {FILE *}fp: Pointer; _close: cint): cint; cdecl; external libasound;
function snd_input_buffer_open(inputp: PPsnd_input_t; buffer: PChar; size: ssize_t): cint; cdecl; external libasound;
function snd_input_close(input: Psnd_input_t): cint; cdecl; external libasound;
function snd_input_scanf(input: Psnd_input_t; format: PChar; dot_dot_dot: array of const): cint
{#ifndef DOC_HIDDEN
	__attribute__ ((format (scanf, 2, 3)))
#endif}
	; cdecl; external libasound;
function snd_input_gets(input: Psnd_input_t; str: PChar; size: size_t): PChar; cdecl; external libasound;
function snd_input_getc(input: Psnd_input_t): cint; cdecl; external libasound;
function snd_input_ungetc(input: Psnd_input_t; c: cint): cint; cdecl; external libasound;

///** \} */

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_INPUT_H */
