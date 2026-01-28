{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/rawmidi.h
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

//#ifndef __ALSA_RAWMIDI_H
//#define __ALSA_RAWMIDI_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup RawMidi RawMidi Interface
 *  The RawMidi Interface. See \ref rawmidi page for more details.
 *  \{
 *)

(** dlsym version for interface entry callback *)
//#define SND_RAWMIDI_DLSYM_VERSION	_dlsym_rawmidi_001

type
{ RawMidi information container }
  PPsnd_rawmidi_info_t = ^Psnd_rawmidi_info_t;
  Psnd_rawmidi_info_t = ^snd_rawmidi_info_t;
  snd_rawmidi_info_t = record
  end;
{ RawMidi settings container }
  PPsnd_rawmidi_params_t = ^Psnd_rawmidi_params_t;
  Psnd_rawmidi_params_t = ^snd_rawmidi_params_t;
  snd_rawmidi_params_t = record
  end;
{ RawMidi status container }
  PPsnd_rawmidi_status_t = ^Psnd_rawmidi_status_t;
  Psnd_rawmidi_status_t = ^snd_rawmidi_status_t;
  snd_rawmidi_status_t = record
  end;

{ RawMidi stream (direction) }
  Psnd_rawmidi_stream_t = ^snd_rawmidi_stream_t;
  snd_rawmidi_stream_t = (
	{ Output stream }
	SND_RAWMIDI_STREAM_OUTPUT = 0,
	{ Input stream }
	SND_RAWMIDI_STREAM_INPUT,
	SND_RAWMIDI_STREAM_LAST = SND_RAWMIDI_STREAM_INPUT);

const
{ Append (flag to open mode) }
  SND_RAWMIDI_APPEND   = $0001;
{ Non blocking mode (flag to open mode) }
  SND_RAWMIDI_NONBLOCK = $0002;
{ Write sync mode (Flag to open mode) }
  SND_RAWMIDI_SYNC     = $0004;

type
{ RawMidi handle }
  PPsnd_rawmidi_t = ^Psnd_rawmidi_t;
  Psnd_rawmidi_t = ^snd_rawmidi_t;
  snd_rawmidi_t = record
  end;

{ RawMidi type }
  Psnd_rawmidi_type_t = ^snd_rawmidi_type_t;
  snd_rawmidi_type_t = (
	{ Kernel level RawMidi }
	SND_RAWMIDI_TYPE_HW,
	{ Shared memory client RawMidi (not yet implemented) }
	SND_RAWMIDI_TYPE_SHM,
	{ INET client RawMidi (not yet implemented) }
	SND_RAWMIDI_TYPE_INET,
	{ Virtual (sequencer) RawMidi }
	SND_RAWMIDI_TYPE_VIRTUAL);

function snd_rawmidi_open(in_rmidi: PPsnd_rawmidi_t; out_rmidi: PPsnd_rawmidi_t;
                          name: PChar; mode: cint): cint; cdecl; external libasound;
function snd_rawmidi_open_lconf(in_rmidi: PPsnd_rawmidi_t; out_rmidi: PPsnd_rawmidi_t;
                                name: PChar; mode: cint; lconf: Psnd_config_t): cint; cdecl; external libasound;
function snd_rawmidi_close(rmidi: Psnd_rawmidi_t): cint; cdecl; external libasound;
function snd_rawmidi_poll_descriptors_count(rmidi: Psnd_rawmidi_t): cint; cdecl; external libasound;
{$INFO pollfd?}
function snd_rawmidi_poll_descriptors(rmidi: Psnd_rawmidi_t; pfds: {struct pollfd*}Pointer; space: cuint): cint; cdecl; external libasound;
{$INFO pollfd?}
function snd_rawmidi_poll_descriptors_revents(rawmidi: Psnd_rawmidi_t; pfds: {struct pollfd*}Pointer; nfds: cuint; revent: Pcushort): cint; cdecl; external libasound;
{$INFO snd_rawmidi_nonblock renamed to snd_rawmidi_set_nonblock, because pascal isn't case sensitive}
function snd_rawmidi_set_nonblock(rmidi: Psnd_rawmidi_t; nonblock: cint): cint; cdecl; external libasound Name 'snd_rawmidi_nonblock';
function snd_rawmidi_info_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_rawmidi_info_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_rawmidi_info_alloca(ptr) __snd_alloca(ptr, snd_rawmidi_info)
function snd_rawmidi_info_malloc(ptr: PPsnd_rawmidi_info_t): cint; cdecl; external libasound;
procedure snd_rawmidi_info_free(obj: Psnd_rawmidi_info_t); cdecl; external libasound;
procedure snd_rawmidi_info_copy(dst: Psnd_rawmidi_info_t; src: Psnd_rawmidi_info_t); cdecl; external libasound;
function snd_rawmidi_info_get_device(obj: Psnd_rawmidi_info_t): cuint; cdecl; external libasound;
function snd_rawmidi_info_get_subdevice(obj: Psnd_rawmidi_info_t): cuint; cdecl; external libasound;
function snd_rawmidi_info_get_stream(obj: Psnd_rawmidi_info_t): snd_rawmidi_stream_t; cdecl; external libasound;
function snd_rawmidi_info_get_card(obj: Psnd_rawmidi_info_t): cint; cdecl; external libasound;
function snd_rawmidi_info_get_flags(obj: Psnd_rawmidi_info_t): cuint; cdecl; external libasound;
function snd_rawmidi_info_get_id(obj: Psnd_rawmidi_info_t): PChar; cdecl; external libasound;
function snd_rawmidi_info_get_name(obj: Psnd_rawmidi_info_t): PChar; cdecl; external libasound;
function snd_rawmidi_info_get_subdevice_name(obj: Psnd_rawmidi_info_t): PChar; cdecl; external libasound;
function snd_rawmidi_info_get_subdevices_count(obj: Psnd_rawmidi_info_t): cuint; cdecl; external libasound;
function snd_rawmidi_info_get_subdevices_avail(obj: Psnd_rawmidi_info_t): cuint; cdecl; external libasound;
procedure snd_rawmidi_info_set_device(obj: Psnd_rawmidi_info_t; val: cuint); cdecl; external libasound;
procedure snd_rawmidi_info_set_subdevice(obj: Psnd_rawmidi_info_t; val: cuint); cdecl; external libasound;
procedure snd_rawmidi_info_set_stream(obj: Psnd_rawmidi_info_t; val: snd_rawmidi_stream_t); cdecl; external libasound;
function snd_rawmidi_info(rmidi: Psnd_rawmidi_t; info: Psnd_rawmidi_info_t): cint; cdecl; external libasound;
function snd_rawmidi_params_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_rawmidi_params_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_rawmidi_params_alloca(ptr) __snd_alloca(ptr, snd_rawmidi_params)
function snd_rawmidi_params_malloc(ptr: PPsnd_rawmidi_params_t): cint; cdecl; external libasound;
procedure snd_rawmidi_params_free(obj: Psnd_rawmidi_params_t); cdecl; external libasound;
procedure snd_rawmidi_params_copy(dst: Psnd_rawmidi_params_t; src: Psnd_rawmidi_params_t); cdecl; external libasound;
function snd_rawmidi_params_set_buffer_size(rmidi: Psnd_rawmidi_t; params: Psnd_rawmidi_params_t; val: size_t): cint; cdecl; external libasound;
function snd_rawmidi_params_get_buffer_size(params: Psnd_rawmidi_params_t): size_t; cdecl; external libasound;
function snd_rawmidi_params_set_avail_min(rmidi: Psnd_rawmidi_t; params: Psnd_rawmidi_params_t; val: size_t): cint; cdecl; external libasound;
function snd_rawmidi_params_get_avail_min(params: Psnd_rawmidi_params_t): size_t; cdecl; external libasound;
function snd_rawmidi_params_set_no_active_sensing(rmidi: Psnd_rawmidi_t; params: Psnd_rawmidi_params_t; val: cint): cint; cdecl; external libasound;
function snd_rawmidi_params_get_no_active_sensing(params: Psnd_rawmidi_params_t): cint; cdecl; external libasound;
function snd_rawmidi_params(rmidi: Psnd_rawmidi_t; params: Psnd_rawmidi_params_t): cint; cdecl; external libasound;
function snd_rawmidi_params_current(rmidi: Psnd_rawmidi_t; params: Psnd_rawmidi_params_t): cint; cdecl; external libasound;
function snd_rawmidi_status_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_rawmidi_status_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_rawmidi_status_alloca(ptr) __snd_alloca(ptr, snd_rawmidi_status)
function snd_rawmidi_status_malloc(ptr: PPsnd_rawmidi_status_t): cint; cdecl; external libasound;
procedure snd_rawmidi_status_free(obj: Psnd_rawmidi_status_t); cdecl; external libasound;
procedure snd_rawmidi_status_copy(dst: Psnd_rawmidi_status_t; src: Psnd_rawmidi_status_t); cdecl; external libasound;
procedure snd_rawmidi_status_get_tstamp(obj: Psnd_rawmidi_status_t; ptr: Psnd_htimestamp_t); cdecl; external libasound;
function snd_rawmidi_status_get_avail(obj: Psnd_rawmidi_status_t): size_t; cdecl; external libasound;
function snd_rawmidi_status_get_xruns(obj: Psnd_rawmidi_status_t): size_t; cdecl; external libasound;
function snd_rawmidi_status(rmidi: Psnd_rawmidi_t; status: Psnd_rawmidi_status_t): cint; cdecl; external libasound;
function snd_rawmidi_drain(rmidi: Psnd_rawmidi_t): cint; cdecl; external libasound;
function snd_rawmidi_drop(rmidi: Psnd_rawmidi_t): cint; cdecl; external libasound;
function snd_rawmidi_write(rmidi: Psnd_rawmidi_t; buffer: Pointer; size: size_t): ssize_t; cdecl; external libasound;
function snd_rawmidi_read(rmidi: Psnd_rawmidi_t; buffer: Pointer; size: size_t): ssize_t; cdecl; external libasound;
function snd_rawmidi_name(rmidi: Psnd_rawmidi_t): PChar; cdecl; external libasound;
function snd_rawmidi_type(rmidi: Psnd_rawmidi_t): snd_rawmidi_type_t; cdecl; external libasound;
function snd_rawmidi_stream(rawmidi: Psnd_rawmidi_t): snd_rawmidi_stream_t; cdecl; external libasound;

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __RAWMIDI_H */
