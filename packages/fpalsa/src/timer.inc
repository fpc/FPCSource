{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/timer.h
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

//#ifndef __ALSA_TIMER_H
//#define __ALSA_TIMER_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup Timer Timer Interface
 *  Timer Interface. See \ref timer page for more details.
 *  \{
 *)

///** dlsym version for interface entry callback */
//#define SND_TIMER_DLSYM_VERSION		_dlsym_timer_001
///** dlsym version for interface entry callback */
//#define SND_TIMER_QUERY_DLSYM_VERSION	_dlsym_timer_query_001

type
{ timer identification structure }
  PPsnd_timer_id_t = ^Psnd_timer_id_t;
  Psnd_timer_id_t = ^snd_timer_id_t;
  snd_timer_id_t = record
  end;
{ timer global info structure }
  PPsnd_timer_ginfo_t = ^Psnd_timer_ginfo_t;
  Psnd_timer_ginfo_t = ^snd_timer_ginfo_t;
  snd_timer_ginfo_t = record
  end;
{ timer global params structure }
  PPsnd_timer_gparams_t = ^Psnd_timer_gparams_t;
  Psnd_timer_gparams_t = ^snd_timer_gparams_t;
  snd_timer_gparams_t = record
  end;
{ timer global status structure }
  PPsnd_timer_gstatus_t = ^Psnd_timer_gstatus_t;
  Psnd_timer_gstatus_t = ^snd_timer_gstatus_t;
  snd_timer_gstatus_t = record
  end;
{ timer info structure }
  PPsnd_timer_info_t = ^Psnd_timer_info_t;
  Psnd_timer_info_t = ^snd_timer_info_t;
  snd_timer_info_t = record
  end;
{ timer params structure }
  PPsnd_timer_params_t = ^Psnd_timer_params_t;
  Psnd_timer_params_t = ^snd_timer_params_t;
  snd_timer_params_t = record
  end;
{ timer status structure }
  PPsnd_timer_status_t = ^Psnd_timer_status_t;
  Psnd_timer_status_t = ^snd_timer_status_t;
  snd_timer_status_t = record
  end;
{ timer master class }
  Psnd_timer_class_t = ^snd_timer_class_t;
  snd_timer_class_t = (
	SND_TIMER_CLASS_NONE = -1,	{ invalid }
	SND_TIMER_CLASS_SLAVE = 0,	{ slave timer }
	SND_TIMER_CLASS_GLOBAL,		{ global timer }
	SND_TIMER_CLASS_CARD,		{ card timer }
	SND_TIMER_CLASS_PCM,		{ PCM timer }
	SND_TIMER_CLASS_LAST = SND_TIMER_CLASS_PCM	{ last timer });

{ timer slave class }
  Psnd_timer_slave_class_t = ^snd_timer_slave_class_t;
  snd_timer_slave_class_t = (
	SND_TIMER_SCLASS_NONE = 0,		{ none }
	SND_TIMER_SCLASS_APPLICATION,		{ for internal use }
	SND_TIMER_SCLASS_SEQUENCER,		{ sequencer timer }
	SND_TIMER_SCLASS_OSS_SEQUENCER,		{ OSS sequencer timer }
	SND_TIMER_SCLASS_LAST = SND_TIMER_SCLASS_OSS_SEQUENCER	{ last slave timer });

{ timer read event identification }
  Psnd_timer_event_t = ^snd_timer_event_t;
  snd_timer_event_t = (
	SND_TIMER_EVENT_RESOLUTION = 0,	{ val = resolution in ns }
	SND_TIMER_EVENT_TICK,		{ val = ticks }
	SND_TIMER_EVENT_START,		{ val = resolution in ns }
	SND_TIMER_EVENT_STOP,		{ val = 0 }
	SND_TIMER_EVENT_CONTINUE,	{ val = resolution in ns }
	SND_TIMER_EVENT_PAUSE,		{ val = 0 }
	SND_TIMER_EVENT_EARLY,		{ val = 0 }
	SND_TIMER_EVENT_SUSPEND,	{ val = 0 }
	SND_TIMER_EVENT_RESUME,		{ val = resolution in ns }
	{ master timer events for slave timer instances }
	SND_TIMER_EVENT_MSTART = Integer(SND_TIMER_EVENT_START) + 10,
	SND_TIMER_EVENT_MSTOP = Integer(SND_TIMER_EVENT_STOP) + 10,
	SND_TIMER_EVENT_MCONTINUE = Integer(SND_TIMER_EVENT_CONTINUE) + 10,
	SND_TIMER_EVENT_MPAUSE = Integer(SND_TIMER_EVENT_PAUSE) + 10,
	SND_TIMER_EVENT_MSUSPEND = Integer(SND_TIMER_EVENT_SUSPEND) + 10,
	SND_TIMER_EVENT_MRESUME = Integer(SND_TIMER_EVENT_RESUME) + 10);

{ timer read structure }
  Psnd_timer_read_t = ^snd_timer_read_t;
  snd_timer_read_t = record
	resolution: cuint;	{ tick resolution in nanoseconds }
        ticks: cuint;		{ count of happened ticks }
  end;

{ timer tstamp + event read structure }
  Psnd_timer_tread_t = ^snd_timer_tread_t;
  snd_timer_tread_t = record
	event: snd_timer_event_t;	{ Timer event }
	tstamp: snd_htimestamp_t;	{ Time stamp of each event }
	val: cuint;		        { Event value }
  end;

const
{ global timer - system }
  SND_TIMER_GLOBAL_SYSTEM  = 0;
{ global timer - RTC }
  SND_TIMER_GLOBAL_RTC     = 1;
{ global timer - HPET }
  SND_TIMER_GLOBAL_HPET    = 2;
{ global timer - HRTIMER }
  SND_TIMER_GLOBAL_HRTIMER = 3;

{ timer open mode flag - non-blocking behaviour }
  SND_TIMER_OPEN_NONBLOCK = 1 shl 0;
{ use timestamps and event notification - enhanced read }
  SND_TIMER_OPEN_TREAD    = 1 shl 1;

type
{ timer handle type }
  PPsnd_timer_type_t = ^Psnd_timer_type_t;
  Psnd_timer_type_t = ^snd_timer_type_t;
  snd_timer_type_t = (
	{ Kernel level HwDep }
	SND_TIMER_TYPE_HW = 0,
	{ Shared memory client timer (not yet implemented) }
	SND_TIMER_TYPE_SHM,
	{ INET client timer (not yet implemented) }
	SND_TIMER_TYPE_INET);

{ timer query handle }
  PPsnd_timer_query_t = ^Psnd_timer_query_t;
  Psnd_timer_query_t = ^snd_timer_query_t;
  snd_timer_query_t = record
  end;
{ timer handle }
  PPsnd_timer_t = ^Psnd_timer_t;
  Psnd_timer_t = ^snd_timer_t;
  snd_timer_t = record
  end;


function snd_timer_query_open(handle: PPsnd_timer_query_t; name: PChar; mode: cint): cint; cdecl; external libasound;
function snd_timer_query_open_lconf(handle: PPsnd_timer_query_t; name: PChar; mode: cint; lconf: Psnd_config_t): cint; cdecl; external libasound;
function snd_timer_query_close(handle: Psnd_timer_query_t): cint; cdecl; external libasound;
function snd_timer_query_next_device(handle: Psnd_timer_query_t; tid: Psnd_timer_id_t): cint; cdecl; external libasound;
function snd_timer_query_info(handle: Psnd_timer_query_t; info: Psnd_timer_ginfo_t): cint; cdecl; external libasound;
function snd_timer_query_params(handle: Psnd_timer_query_t; params: Psnd_timer_gparams_t): cint; cdecl; external libasound;
function snd_timer_query_status(handle: Psnd_timer_query_t; status: Psnd_timer_gstatus_t): cint; cdecl; external libasound;

function snd_timer_open(handle: PPsnd_timer_t; name: PChar; mode: cint): cint; cdecl; external libasound;
function snd_timer_open_lconf(handle: PPsnd_timer_t; name: PChar; mode: cint; lconf: Psnd_config_t): cint; cdecl; external libasound;
function snd_timer_close(handle: Psnd_timer_t): cint; cdecl; external libasound;
function snd_async_add_timer_handler(handler: PPsnd_async_handler_t; timer: Psnd_timer_t;
				     callback: snd_async_callback_t; private_data: Pointer): cint; cdecl; external libasound;
function snd_async_handler_get_timer(handler: Psnd_async_handler_t): Psnd_timer_t; cdecl; external libasound;
function snd_timer_poll_descriptors_count(handle: Psnd_timer_t): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_timer_poll_descriptors(handle: Psnd_timer_t; pfds{struct pollfd*}: Pointer; space: cuint): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_timer_poll_descriptors_revents(timer: Psnd_timer_t; pfds{struct pollfd*}: Pointer; nfds: cuint; revents: Pcushort): cint; cdecl; external libasound;
function snd_timer_info(handle: Psnd_timer_t; timer: Psnd_timer_info_t): cint; cdecl; external libasound;
function snd_timer_params(handle: Psnd_timer_t; params: Psnd_timer_params_t): cint; cdecl; external libasound;
function snd_timer_status(handle: Psnd_timer_t; status: Psnd_timer_status_t): cint; cdecl; external libasound;
function snd_timer_start(handle: Psnd_timer_t): cint; cdecl; external libasound;
function snd_timer_stop(handle: Psnd_timer_t): cint; cdecl; external libasound;
function snd_timer_continue(handle: Psnd_timer_t): cint; cdecl; external libasound;
function snd_timer_read(handle: Psnd_timer_t; buffer: Pointer; size: size_t): ssize_t; cdecl; external libasound;

function snd_timer_id_sizeof: size_t; cdecl; external libasound;
{ allocate #snd_timer_id_t container on stack }
//#define snd_timer_id_alloca(ptr) __snd_alloca(ptr, snd_timer_id)
function snd_timer_id_malloc(ptr: PPsnd_timer_id_t): cint; cdecl; external libasound;
procedure snd_timer_id_free(obj: Psnd_timer_id_t); cdecl; external libasound;
procedure snd_timer_id_copy(dst: Psnd_timer_id_t; src: Psnd_timer_id_t); cdecl; external libasound;

procedure snd_timer_id_set_class(id: Psnd_timer_id_t; dev_class: cint); cdecl; external libasound;
function snd_timer_id_get_class(id: Psnd_timer_id_t): cint; cdecl; external libasound;
procedure snd_timer_id_set_sclass(id: Psnd_timer_id_t; dev_sclass: cint); cdecl; external libasound;
function snd_timer_id_get_sclass(id: Psnd_timer_id_t): cint; cdecl; external libasound;
procedure snd_timer_id_set_card(id: Psnd_timer_id_t; card: cint); cdecl; external libasound;
function snd_timer_id_get_card(id: Psnd_timer_id_t): cint; cdecl; external libasound;
procedure snd_timer_id_set_device(id: Psnd_timer_id_t; device: cint); cdecl; external libasound;
function snd_timer_id_get_device(id: Psnd_timer_id_t): cint; cdecl; external libasound;
procedure snd_timer_id_set_subdevice(id: Psnd_timer_id_t; subdevice: cint); cdecl; external libasound;
function snd_timer_id_get_subdevice(id: Psnd_timer_id_t): cint; cdecl; external libasound;

function snd_timer_ginfo_sizeof: size_t; cdecl; external libasound;
{ allocate #snd_timer_ginfo_t container on stack }
//#define snd_timer_ginfo_alloca(ptr) __snd_alloca(ptr, snd_timer_ginfo)
function snd_timer_ginfo_malloc(ptr: PPsnd_timer_ginfo_t): cint; cdecl; external libasound;
procedure snd_timer_ginfo_free(obj: Psnd_timer_ginfo_t); cdecl; external libasound;
procedure snd_timer_ginfo_copy(dst: Psnd_timer_ginfo_t; src: Psnd_timer_ginfo_t); cdecl; external libasound;

function snd_timer_ginfo_set_tid(obj: Psnd_timer_ginfo_t; tid: Psnd_timer_id_t): cint; cdecl; external libasound;
function snd_timer_ginfo_get_tid(obj: Psnd_timer_ginfo_t): Psnd_timer_id_t; cdecl; external libasound;
function snd_timer_ginfo_get_flags(obj: Psnd_timer_ginfo_t): cuint; cdecl; external libasound;
function snd_timer_ginfo_get_card(obj: Psnd_timer_ginfo_t): cint; cdecl; external libasound;
function snd_timer_ginfo_get_id(obj: Psnd_timer_ginfo_t): PChar; cdecl; external libasound;
function snd_timer_ginfo_get_name(obj: Psnd_timer_ginfo_t): PChar; cdecl; external libasound;
function snd_timer_ginfo_get_resolution(obj: Psnd_timer_ginfo_t): culong; cdecl; external libasound;
function snd_timer_ginfo_get_resolution_min(obj: Psnd_timer_ginfo_t): culong; cdecl; external libasound;
function snd_timer_ginfo_get_resolution_max(obj: Psnd_timer_ginfo_t): culong; cdecl; external libasound;
function snd_timer_ginfo_get_clients(obj: Psnd_timer_ginfo_t): cuint; cdecl; external libasound;

function snd_timer_info_sizeof: size_t; cdecl; external libasound;
{ allocate #snd_timer_info_t container on stack }
//#define snd_timer_info_alloca(ptr) __snd_alloca(ptr, snd_timer_info)
function snd_timer_info_malloc(ptr: PPsnd_timer_info_t): cint; cdecl; external libasound;
procedure snd_timer_info_free(obj: Psnd_timer_info_t); cdecl; external libasound;
procedure snd_timer_info_copy(dst: Psnd_timer_info_t; src: Psnd_timer_info_t); cdecl; external libasound;

function snd_timer_info_is_slave(info: Psnd_timer_info_t): cint; cdecl; external libasound;
function snd_timer_info_get_card(info: Psnd_timer_info_t): cint; cdecl; external libasound;
function snd_timer_info_get_id(info: Psnd_timer_info_t): PChar; cdecl; external libasound;
function snd_timer_info_get_name(info: Psnd_timer_info_t): PChar; cdecl; external libasound;
function snd_timer_info_get_resolution(info: Psnd_timer_info_t): clong; cdecl; external libasound;

function snd_timer_params_sizeof: size_t; cdecl; external libasound;
{ allocate #snd_timer_params_t container on stack }
//#define snd_timer_params_alloca(ptr) __snd_alloca(ptr, snd_timer_params)
function snd_timer_params_malloc(ptr: PPsnd_timer_params_t): cint; cdecl; external libasound;
procedure snd_timer_params_free(obj: Psnd_timer_params_t); cdecl; external libasound;
procedure snd_timer_params_copy(dst: Psnd_timer_params_t; src: Psnd_timer_params_t); cdecl; external libasound;

function snd_timer_params_set_auto_start(params: Psnd_timer_params_t; auto_start: cint): cint; cdecl; external libasound;
function snd_timer_params_get_auto_start(params: Psnd_timer_params_t): cint; cdecl; external libasound;
function snd_timer_params_set_exclusive(params: Psnd_timer_params_t; exclusive: cint): cint; cdecl; external libasound;
function snd_timer_params_get_exclusive(params: Psnd_timer_params_t): cint; cdecl; external libasound;
function snd_timer_params_set_early_event(params: Psnd_timer_params_t; early_event: cint): cint; cdecl; external libasound;
function snd_timer_params_get_early_event(params: Psnd_timer_params_t): cint; cdecl; external libasound;
procedure snd_timer_params_set_ticks(params: Psnd_timer_params_t; ticks: clong); cdecl; external libasound;
function snd_timer_params_get_ticks(params: Psnd_timer_params_t): clong; cdecl; external libasound;
procedure snd_timer_params_set_queue_size(params: Psnd_timer_params_t; queue_size: clong); cdecl; external libasound;
function snd_timer_params_get_queue_size(params: Psnd_timer_params_t): clong; cdecl; external libasound;
procedure snd_timer_params_set_filter(params: Psnd_timer_params_t; filter: cuint); cdecl; external libasound;
function snd_timer_params_get_filter(params: Psnd_timer_params_t): cuint; cdecl; external libasound;

function snd_timer_status_sizeof: size_t; cdecl; external libasound;
{ allocate #snd_timer_status_t container on stack }
//#define snd_timer_status_alloca(ptr) __snd_alloca(ptr, snd_timer_status)
function snd_timer_status_malloc(ptr: PPsnd_timer_status_t): cint; cdecl; external libasound;
procedure snd_timer_status_free(obj: Psnd_timer_status_t); cdecl; external libasound;
procedure snd_timer_status_copy(dst: Psnd_timer_status_t; src: Psnd_timer_status_t); cdecl; external libasound;

function snd_timer_status_get_timestamp(status: Psnd_timer_status_t): snd_htimestamp_t; cdecl; external libasound;
function snd_timer_status_get_resolution(status: Psnd_timer_status_t): clong; cdecl; external libasound;
function snd_timer_status_get_lost(status: Psnd_timer_status_t): clong; cdecl; external libasound;
function snd_timer_status_get_overrun(status: Psnd_timer_status_t): clong; cdecl; external libasound;
function snd_timer_status_get_queue(status: Psnd_timer_status_t): clong; cdecl; external libasound;

{ deprecated functions, for compatibility }
function snd_timer_info_get_ticks(info: Psnd_timer_info_t): clong; cdecl; external libasound;

///** \} */

//#ifdef __cplusplus
//}
//#endif

//#endif /** __ALSA_TIMER_H */
