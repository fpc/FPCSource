{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/hwdep.h
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

//#ifndef __ALSA_HWDEP_H
//#define __ALSA_HWDEP_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup HwDep Hardware Dependant Interface
 *  The Hardware Dependant Interface.
 *  \{
 *)

///** dlsym version for interface entry callback */
//#define SND_HWDEP_DLSYM_VERSION		_dlsym_hwdep_001

type
{ HwDep information container }
  PPsnd_hwdep_info_t = ^Psnd_hwdep_info_t;
  Psnd_hwdep_info_t = ^snd_hwdep_info_t;
  snd_hwdep_info_t = record
  end;

{ HwDep DSP status container }
  PPsnd_hwdep_dsp_status_t = ^Psnd_hwdep_dsp_status_t;
  Psnd_hwdep_dsp_status_t = ^snd_hwdep_dsp_status_t;
  snd_hwdep_dsp_status_t = record
  end;

{ HwDep DSP image container }
  PPsnd_hwdep_dsp_image_t = ^Psnd_hwdep_dsp_image_t;
  Psnd_hwdep_dsp_image_t = ^snd_hwdep_dsp_image_t;
  snd_hwdep_dsp_image_t = record
  end;

{ HwDep interface }
  Psnd_hwdep_iface_t = ^snd_hwdep_iface_t;
  snd_hwdep_iface_t = (
	SND_HWDEP_IFACE_OPL2 = 0,	{ OPL2 raw driver }
	SND_HWDEP_IFACE_OPL3,		{ OPL3 raw driver }
	SND_HWDEP_IFACE_OPL4,		{ OPL4 raw driver }
	SND_HWDEP_IFACE_SB16CSP,	{ SB16CSP driver }
	SND_HWDEP_IFACE_EMU10K1,	{ EMU10K1 driver }
	SND_HWDEP_IFACE_YSS225,		{ YSS225 driver }
	SND_HWDEP_IFACE_ICS2115,	{ ICS2115 driver }
	SND_HWDEP_IFACE_SSCAPE,		{ Ensoniq SoundScape ISA card (MC68EC000) }
	SND_HWDEP_IFACE_VX,		{ Digigram VX cards }
	SND_HWDEP_IFACE_MIXART,		{ Digigram miXart cards }
	SND_HWDEP_IFACE_USX2Y,		{ Tascam US122, US224 & US428 usb }
	SND_HWDEP_IFACE_EMUX_WAVETABLE,	{ EmuX wavetable }
	SND_HWDEP_IFACE_BLUETOOTH,	{ Bluetooth audio }
	SND_HWDEP_IFACE_USX2Y_PCM,	{ Tascam US122, US224 & US428 raw USB PCM }
	SND_HWDEP_IFACE_PCXHR,		{ Digigram PCXHR }
	SND_HWDEP_IFACE_SB_RC,		{ SB Extigy/Audigy2NX remote control }
	SND_HWDEP_IFACE_HDA,		{ HD-audio }
	SND_HWDEP_IFACE_USB_STREAM,	{ direct access to usb stream }
	SND_HWDEP_IFACE_FW_DICE,	{ TC DICE FireWire device }
	SND_HWDEP_IFACE_FW_FIREWORKS,	{ Echo Audio Fireworks based device }
	SND_HWDEP_IFACE_FW_BEBOB,	{ BridgeCo BeBoB based device }
	SND_HWDEP_IFACE_FW_OXFW,	{ Oxford OXFW970/971 based device }
	SND_HWDEP_IFACE_FW_DIGI00X,	{ Digidesign Digi 002/003 family }
	SND_HWDEP_IFACE_FW_TASCAM,	{ TASCAM FireWire series }

	SND_HWDEP_IFACE_LAST = SND_HWDEP_IFACE_FW_TASCAM  { last known hwdep interface }
);

const
{ open for reading }
  SND_HWDEP_OPEN_READ     = O_RDONLY;
{ open for writing }
  SND_HWDEP_OPEN_WRITE    = O_WRONLY;
{ open for reading and writing }
  SND_HWDEP_OPEN_DUPLEX   = O_RDWR;
{ open mode flag: open in nonblock mode }
  SND_HWDEP_OPEN_NONBLOCK = O_NONBLOCK;

type
{ HwDep handle type }
  Psnd_hwdep_type_t = ^snd_hwdep_type_t;
  snd_hwdep_type_t = (
	{ Kernel level HwDep }
	SND_HWDEP_TYPE_HW,
	{ Shared memory client HwDep (not yet implemented) }
	SND_HWDEP_TYPE_SHM,
	{ INET client HwDep (not yet implemented) }
	SND_HWDEP_TYPE_INET
);

{ HwDep handle }
  PPsnd_hwdep_t = ^Psnd_hwdep_t;
  Psnd_hwdep_t = ^snd_hwdep_t;
  snd_hwdep_t = record
  end;

function snd_hwdep_open(hwdep: PPsnd_hwdep_t; name: PChar; mode: cint): cint; cdecl; external libasound;
function snd_hwdep_close(hwdep: Psnd_hwdep_t): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_hwdep_poll_descriptors(hwdep: Psnd_hwdep_t; pfds: {struct pollfd*}Pointer; space: cuint): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_hwdep_poll_descriptors_revents(hwdep: Psnd_hwdep_t; pfds: {struct pollfd*}Pointer; nfds: cuint; revents: Pcushort): cint; cdecl; external libasound;
function snd_hwdep_nonblock(hwdep: Psnd_hwdep_t; nonblock: cint): cint; cdecl; external libasound;
function snd_hwdep_info(hwdep: Psnd_hwdep_t; info: Psnd_hwdep_info_t): cint; cdecl; external libasound;
function snd_hwdep_dsp_status(hwdep: Psnd_hwdep_t; status: Psnd_hwdep_dsp_status_t): cint; cdecl; external libasound;
function snd_hwdep_dsp_load(hwdep: Psnd_hwdep_t; block: Psnd_hwdep_dsp_image_t): cint; cdecl; external libasound;
function snd_hwdep_ioctl(hwdep: Psnd_hwdep_t; request: cuint; arg: Pointer): cint; cdecl; external libasound;
function snd_hwdep_write(hwdep: Psnd_hwdep_t; buffer: Pointer; size: size_t): ssize_t; cdecl; external libasound;
function snd_hwdep_read(hwdep: Psnd_hwdep_t; buffer: Pointer; size: size_t): ssize_t; cdecl; external libasound;

function snd_hwdep_info_sizeof: size_t; cdecl; external libasound;
{ allocate #snd_hwdep_info_t container on stack }
//#define snd_hwdep_info_alloca(ptr) __snd_alloca(ptr, snd_hwdep_info)
function snd_hwdep_info_malloc(ptr: PPsnd_hwdep_info_t): cint; cdecl; external libasound;
procedure snd_hwdep_info_free(obj: Psnd_hwdep_info_t); cdecl; external libasound;
procedure snd_hwdep_info_copy(dst: Psnd_hwdep_info_t; src: Psnd_hwdep_info_t); cdecl; external libasound;

function snd_hwdep_info_get_device(obj: Psnd_hwdep_info_t): cuint; cdecl; external libasound;
function snd_hwdep_info_get_card(obj: Psnd_hwdep_info_t): cint; cdecl; external libasound;
function snd_hwdep_info_get_id(obj: Psnd_hwdep_info_t): PChar; cdecl; external libasound;
function snd_hwdep_info_get_name(obj: Psnd_hwdep_info_t): PChar; cdecl; external libasound;
function snd_hwdep_info_get_iface(obj: Psnd_hwdep_info_t): snd_hwdep_iface_t; cdecl; external libasound;
procedure snd_hwdep_info_set_device(obj: Psnd_hwdep_info_t; val: cuint); cdecl; external libasound;

function snd_hwdep_dsp_status_sizeof: size_t; cdecl; external libasound;
{ allocate #snd_hwdep_dsp_status_t container on stack }
//#define snd_hwdep_dsp_status_alloca(ptr) __snd_alloca(ptr, snd_hwdep_dsp_status)
function snd_hwdep_dsp_status_malloc(ptr: PPsnd_hwdep_dsp_status_t): cint; cdecl; external libasound;
procedure snd_hwdep_dsp_status_free(obj: Psnd_hwdep_dsp_status_t); cdecl; external libasound;
procedure snd_hwdep_dsp_status_copy(dst: Psnd_hwdep_dsp_status_t; src: Psnd_hwdep_dsp_status_t); cdecl; external libasound;

function snd_hwdep_dsp_status_get_version(obj: Psnd_hwdep_dsp_status_t): cuint; cdecl; external libasound;
function snd_hwdep_dsp_status_get_id(obj: Psnd_hwdep_dsp_status_t): PChar; cdecl; external libasound;
function snd_hwdep_dsp_status_get_num_dsps(obj: Psnd_hwdep_dsp_status_t): cuint; cdecl; external libasound;
function snd_hwdep_dsp_status_get_dsp_loaded(obj: Psnd_hwdep_dsp_status_t): cuint; cdecl; external libasound;
function snd_hwdep_dsp_status_get_chip_ready(obj: Psnd_hwdep_dsp_status_t): cuint; cdecl; external libasound;

function snd_hwdep_dsp_image_sizeof: size_t; cdecl; external libasound;
{ allocate #snd_hwdep_dsp_image_t container on stack }
//#define snd_hwdep_dsp_image_alloca(ptr) __snd_alloca(ptr, snd_hwdep_dsp_image)
function snd_hwdep_dsp_image_malloc(ptr: PPsnd_hwdep_dsp_image_t): cint; cdecl; external libasound;
procedure snd_hwdep_dsp_image_free(obj: Psnd_hwdep_dsp_image_t); cdecl; external libasound;
procedure snd_hwdep_dsp_image_copy(dst: Psnd_hwdep_dsp_image_t; src: Psnd_hwdep_dsp_image_t); cdecl; external libasound;

function snd_hwdep_dsp_image_get_index(obj: Psnd_hwdep_dsp_image_t): cuint; cdecl; external libasound;
function snd_hwdep_dsp_image_get_name(obj: Psnd_hwdep_dsp_image_t): PChar; cdecl; external libasound;
function snd_hwdep_dsp_image_get_image(obj: Psnd_hwdep_dsp_image_t): Pointer; cdecl; external libasound;
function snd_hwdep_dsp_image_get_length(obj: Psnd_hwdep_dsp_image_t): size_t; cdecl; external libasound;

procedure snd_hwdep_dsp_image_set_index(obj: Psnd_hwdep_dsp_image_t; _index: cuint); cdecl; external libasound;
procedure snd_hwdep_dsp_image_set_name(obj: Psnd_hwdep_dsp_image_t; name: PChar); cdecl; external libasound;
procedure snd_hwdep_dsp_image_set_image(obj: Psnd_hwdep_dsp_image_t; buffer: Pointer); cdecl; external libasound;
procedure snd_hwdep_dsp_image_set_length(obj: Psnd_hwdep_dsp_image_t; length: size_t); cdecl; external libasound;

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_HWDEP_H */
