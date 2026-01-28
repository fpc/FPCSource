{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

(**
 * \file include/seq.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *)
(*
 * Application interface library for the ALSA driver
 *
 *
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
 *)

//#ifndef __ALSA_SEQ_H
//#define __ALSA_SEQ_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup Sequencer MIDI Sequencer
 *  MIDI Sequencer Interface.
 *  See \ref seq page for more details.
 *  \{
 *)

(** dlsym version for interface entry callback *)
//#define SND_SEQ_DLSYM_VERSION		_dlsym_seq_001

(** Sequencer handle *)
type
  PPsnd_seq_t = ^Psnd_seq_t;
  Psnd_seq_t = ^snd_seq_t;
  snd_seq_t = record
  end;

(**
 * sequencer opening stream types
 *)
const
  SND_SEQ_OPEN_OUTPUT = 1;  (**< open for output (write) *)
  SND_SEQ_OPEN_INPUT  = 2;  (**< open for input (read) *)
  SND_SEQ_OPEN_DUPLEX = (SND_SEQ_OPEN_OUTPUT or SND_SEQ_OPEN_INPUT);  (**< open for both input and output (read/write) *)

(**
 * sequencer opening mode
 *)

 SND_SEQ_NONBLOCK = $0001;  (**< non-blocking mode (flag to open mode) *)

(** sequencer handle type *)
type
  Psnd_seq_type_t = ^snd_seq_type_t;
  snd_seq_type_t = (
	SND_SEQ_TYPE_HW,		(**< hardware *)
	SND_SEQ_TYPE_SHM,		(**< shared memory (NYI) *)
	SND_SEQ_TYPE_INET		(**< network (NYI) *)
  );

(** special client (port) ids *)
const
  SND_SEQ_ADDRESS_UNKNOWN     = 253;  (**< unknown source *)
  SND_SEQ_ADDRESS_SUBSCRIBERS = 254;  (**< send event to all subscribed ports *)
  SND_SEQ_ADDRESS_BROADCAST   = 255;  (**< send event to all queues/clients/ports/channels *)

(** known client numbers *)
  SND_SEQ_CLIENT_SYSTEM       = 0;  (**< system client *)

(*
 *)
function snd_seq_open(handle: PPsnd_seq_t; name: PChar; streams: cint; mode: cint): cint; cdecl; external libasound;
function snd_seq_open_lconf(handle: PPsnd_seq_t; name: PChar; streams: cint; mode: cint; lconf: Psnd_config_t): cint; cdecl; external libasound;
function snd_seq_name(seq: Psnd_seq_t): PChar; cdecl; external libasound;
function snd_seq_type(seq: Psnd_seq_t): snd_seq_type_t; cdecl; external libasound;
function snd_seq_close(handle: Psnd_seq_t): cint; cdecl; external libasound;
function snd_seq_poll_descriptors_count(handle: Psnd_seq_t; events: cshort): cint; cdecl; external libasound;
{$INFO pollfd?}
function snd_seq_poll_descriptors(handle: Psnd_seq_t; pfds: {struct pollfd*}Pointer; space: cuint; events: cshort): cint; cdecl; external libasound;
{$INFO pollfd?}
function snd_seq_poll_descriptors_revents(seq: Psnd_seq_t; pfds: {struct pollfd*}Pointer; nfds: cuint; revents: Pcushort): cint; cdecl; external libasound;
{$INFO snd_seq_nonblock renamed to snd_seq_set_nonblock, because pascal isn't case sensitive}
function snd_seq_set_nonblock(handle: Psnd_seq_t; nonblock: cint): cint; cdecl; external libasound Name 'snd_seq_nonblock';
function snd_seq_client_id(handle: Psnd_seq_t): cint; cdecl; external libasound;

function snd_seq_get_output_buffer_size(handle: Psnd_seq_t): size_t; cdecl; external libasound;
function snd_seq_get_input_buffer_size(handle: Psnd_seq_t): size_t; cdecl; external libasound;
function snd_seq_set_output_buffer_size(handle: Psnd_seq_t; size: size_t): cint; cdecl; external libasound;
function snd_seq_set_input_buffer_size(handle: Psnd_seq_t; size: size_t): cint; cdecl; external libasound;

(** system information container *)
type
  PPsnd_seq_system_info_t = ^Psnd_seq_system_info_t;
  Psnd_seq_system_info_t = ^snd_seq_system_info_t;
  snd_seq_system_info_t = record
  end;

function snd_seq_system_info_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_system_info_t container on stack *)
//#define snd_seq_system_info_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_system_info)
function snd_seq_system_info_malloc(ptr: PPsnd_seq_system_info_t): cint; cdecl; external libasound;
procedure snd_seq_system_info_free(ptr: Psnd_seq_system_info_t); cdecl; external libasound;
procedure snd_seq_system_info_copy(dst: Psnd_seq_system_info_t; src: Psnd_seq_system_info_t); cdecl; external libasound;

function snd_seq_system_info_get_queues(info: Psnd_seq_system_info_t): cint; cdecl; external libasound;
function snd_seq_system_info_get_clients(info: Psnd_seq_system_info_t): cint; cdecl; external libasound;
function snd_seq_system_info_get_ports(info: Psnd_seq_system_info_t): cint; cdecl; external libasound;
function snd_seq_system_info_get_channels(info: Psnd_seq_system_info_t): cint; cdecl; external libasound;
function snd_seq_system_info_get_cur_clients(info: Psnd_seq_system_info_t): cint; cdecl; external libasound;
function snd_seq_system_info_get_cur_queues(info: Psnd_seq_system_info_t): cint; cdecl; external libasound;

function snd_seq_system_info(handle: Psnd_seq_t; info: Psnd_seq_system_info_t): cint; cdecl; external libasound;

(** \} *)


(**
 *  \defgroup SeqClient Sequencer Client Interface
 *  Sequencer Client Interface
 *  \ingroup Sequencer
 *  \{
 *)

(** client information container *)
type
  PPsnd_seq_client_info_t = ^Psnd_seq_client_info_t;
  Psnd_seq_client_info_t = ^snd_seq_client_info_t;
  snd_seq_client_info_t = record
  end;

(** client types *)
  Psnd_seq_client_type_t = ^snd_seq_client_type_t;
  snd_seq_client_type_t = (
	SND_SEQ_USER_CLIENT     = 1,	(**< user client *)
	SND_SEQ_KERNEL_CLIENT   = 2	(**< kernel client *)
  );

function snd_seq_client_info_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_client_info_t container on stack *)
//#define snd_seq_client_info_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_client_info)
function snd_seq_client_info_malloc(ptr: PPsnd_seq_client_info_t): cint; cdecl; external libasound;
procedure snd_seq_client_info_free(ptr: Psnd_seq_client_info_t); cdecl; external libasound;
procedure snd_seq_client_info_copy(dst: Psnd_seq_client_info_t; src: Psnd_seq_client_info_t); cdecl; external libasound;

function snd_seq_client_info_get_client(info: Psnd_seq_client_info_t): cint; cdecl; external libasound;
function snd_seq_client_info_get_type(info: Psnd_seq_client_info_t): snd_seq_client_type_t; cdecl; external libasound;
function snd_seq_client_info_get_name(info: Psnd_seq_client_info_t): PChar; cdecl; external libasound;
function snd_seq_client_info_get_broadcast_filter(info: Psnd_seq_client_info_t): cint; cdecl; external libasound;
function snd_seq_client_info_get_error_bounce(info: Psnd_seq_client_info_t): cint; cdecl; external libasound;
function snd_seq_client_info_get_event_filter(info: Psnd_seq_client_info_t): Pcuchar; cdecl; external libasound;
function snd_seq_client_info_get_num_ports(info: Psnd_seq_client_info_t): cint; cdecl; external libasound;
function snd_seq_client_info_get_event_lost(info: Psnd_seq_client_info_t): cint; cdecl; external libasound;

procedure snd_seq_client_info_set_client(info: Psnd_seq_client_info_t; client: cint); cdecl; external libasound;
procedure snd_seq_client_info_set_name(info: Psnd_seq_client_info_t; name: PChar); cdecl; external libasound;
procedure snd_seq_client_info_set_broadcast_filter(info: Psnd_seq_client_info_t; val: cint); cdecl; external libasound;
procedure snd_seq_client_info_set_error_bounce(info: Psnd_seq_client_info_t; val: cint); cdecl; external libasound;
procedure snd_seq_client_info_set_event_filter(info: Psnd_seq_client_info_t; filter: Pcuchar); cdecl; external libasound;

procedure snd_seq_client_info_event_filter_clear(info: Psnd_seq_client_info_t); cdecl; external libasound;
procedure snd_seq_client_info_event_filter_add(info: Psnd_seq_client_info_t; event_type: cint); cdecl; external libasound;
procedure snd_seq_client_info_event_filter_del(info: Psnd_seq_client_info_t; event_type: cint); cdecl; external libasound;
function snd_seq_client_info_event_filter_check(info: Psnd_seq_client_info_t; event_type: cint): cint; cdecl; external libasound;

function snd_seq_get_client_info(handle: Psnd_seq_t; info: Psnd_seq_client_info_t): cint; cdecl; external libasound;
function snd_seq_get_any_client_info(handle: Psnd_seq_t; client: cint; info: Psnd_seq_client_info_t): cint; cdecl; external libasound;
function snd_seq_set_client_info(handle: Psnd_seq_t; info: Psnd_seq_client_info_t): cint; cdecl; external libasound;
function snd_seq_query_next_client(handle: Psnd_seq_t; info: Psnd_seq_client_info_t): cint; cdecl; external libasound;

(*
 *)

(** client pool information container *)
type
  PPsnd_seq_client_pool_t = ^Psnd_seq_client_pool_t;
  Psnd_seq_client_pool_t = ^snd_seq_client_pool_t;
  snd_seq_client_pool_t = record
  end;

function snd_seq_client_pool_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_client_pool_t container on stack *)
//#define snd_seq_client_pool_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_client_pool)
function snd_seq_client_pool_malloc(ptr: PPsnd_seq_client_pool_t): cint; cdecl; external libasound;
procedure snd_seq_client_pool_free(ptr: Psnd_seq_client_pool_t); cdecl; external libasound;
procedure snd_seq_client_pool_copy(dst: Psnd_seq_client_pool_t; src: Psnd_seq_client_pool_t); cdecl; external libasound;

function snd_seq_client_pool_get_client(info: Psnd_seq_client_pool_t): cint; cdecl; external libasound;
function snd_seq_client_pool_get_output_pool(info: Psnd_seq_client_pool_t): size_t; cdecl; external libasound;
function snd_seq_client_pool_get_input_pool(info: Psnd_seq_client_pool_t): size_t; cdecl; external libasound;
function snd_seq_client_pool_get_output_room(info: Psnd_seq_client_pool_t): size_t; cdecl; external libasound;
function snd_seq_client_pool_get_output_free(info: Psnd_seq_client_pool_t): size_t; cdecl; external libasound;
function snd_seq_client_pool_get_input_free(info: Psnd_seq_client_pool_t): size_t; cdecl; external libasound;
procedure snd_seq_client_pool_set_output_pool(info: Psnd_seq_client_pool_t; size: size_t); cdecl; external libasound;
procedure snd_seq_client_pool_set_input_pool(info: Psnd_seq_client_pool_t; size: size_t); cdecl; external libasound;
procedure snd_seq_client_pool_set_output_room(info: Psnd_seq_client_pool_t; size: size_t); cdecl; external libasound;

function snd_seq_get_client_pool(handle: Psnd_seq_t; info: Psnd_seq_client_pool_t): cint; cdecl; external libasound;
function snd_seq_set_client_pool(handle: Psnd_seq_t; info: Psnd_seq_client_pool_t): cint; cdecl; external libasound;


(** \} *)


(**
 *  \defgroup SeqPort Sequencer Port Interface
 *  Sequencer Port Interface
 *  \ingroup Sequencer
 *  \{
 *)

(** port information container *)
type
  PPsnd_seq_port_info_t = ^Psnd_seq_port_info_t;
  Psnd_seq_port_info_t = ^snd_seq_port_info_t;
  snd_seq_port_info_t = record
  end;

(** known port numbers *)
const
  SND_SEQ_PORT_SYSTEM_TIMER    = 0;  (**< system timer port *)
  SND_SEQ_PORT_SYSTEM_ANNOUNCE = 1;  (**< system announce port *)

(** port capabilities (32 bits) *)
  SND_SEQ_PORT_CAP_READ        = 1 shl 0;  (**< readable from this port *)
  SND_SEQ_PORT_CAP_WRITE       = 1 shl 1;  (**< writable to this port *)

  SND_SEQ_PORT_CAP_SYNC_READ   = 1 shl 2;  (**< allow read subscriptions *)
  SND_SEQ_PORT_CAP_SYNC_WRITE  = 1 shl 3;  (**< allow write subscriptions *)

  SND_SEQ_PORT_CAP_DUPLEX      = 1 shl 4;  (**< allow read/write duplex *)

  SND_SEQ_PORT_CAP_SUBS_READ   = 1 shl 5;  (**< allow read subscription *)
  SND_SEQ_PORT_CAP_SUBS_WRITE  = 1 shl 6;  (**< allow write subscription *)
  SND_SEQ_PORT_CAP_NO_EXPORT   = 1 shl 7;  (**< routing not allowed *)

(* port type *)
(** Messages sent from/to this port have device-specific semantics. *)
  SND_SEQ_PORT_TYPE_SPECIFIC      = 1 shl 0;
(** This port understands MIDI messages. *)
  SND_SEQ_PORT_TYPE_MIDI_GENERIC  = 1 shl 1;
(** This port is compatible with the General MIDI specification. *)
  SND_SEQ_PORT_TYPE_MIDI_GM       = 1 shl 2;
(** This port is compatible with the Roland GS standard. *)
  SND_SEQ_PORT_TYPE_MIDI_GS       = 1 shl 3;
(** This port is compatible with the Yamaha XG specification. *)
  SND_SEQ_PORT_TYPE_MIDI_XG       = 1 shl 4;
(** This port is compatible with the Roland MT-32. *)
  SND_SEQ_PORT_TYPE_MIDI_MT32     = 1 shl 5;
(** This port is compatible with the General MIDI 2 specification. *)
  SND_SEQ_PORT_TYPE_MIDI_GM2      = 1 shl 6;
(** This port understands SND_SEQ_EVENT_SAMPLE_xxx messages
    (these are not MIDI messages). *)
  SND_SEQ_PORT_TYPE_SYNTH         = 1 shl 10;
(** Instruments can be downloaded to this port
    (with SND_SEQ_EVENT_INSTR_xxx messages sent directly). *)
  SND_SEQ_PORT_TYPE_DIRECT_SAMPLE = 1 shl 11;
(** Instruments can be downloaded to this port
    (with SND_SEQ_EVENT_INSTR_xxx messages sent directly or through a queue). *)
  SND_SEQ_PORT_TYPE_SAMPLE        = 1 shl 12;
(** This port is implemented in hardware. *)
  SND_SEQ_PORT_TYPE_HARDWARE      = 1 shl 16;
(** This port is implemented in software. *)
  SND_SEQ_PORT_TYPE_SOFTWARE      = 1 shl 17;
(** Messages sent to this port will generate sounds. *)
  SND_SEQ_PORT_TYPE_SYNTHESIZER   = 1 shl 18;
(** This port may connect to other devices
    (whose characteristics are not known). *)
  SND_SEQ_PORT_TYPE_PORT          = 1 shl 19;
(** This port belongs to an application, such as a sequencer or editor. *)
  SND_SEQ_PORT_TYPE_APPLICATION   = 1 shl 20;


function snd_seq_port_info_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_port_info_t container on stack *)
//#define snd_seq_port_info_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_port_info)
function snd_seq_port_info_malloc(ptr: PPsnd_seq_port_info_t): cint; cdecl; external libasound;
procedure snd_seq_port_info_free(ptr: Psnd_seq_port_info_t); cdecl; external libasound;
procedure snd_seq_port_info_copy(dst: Psnd_seq_port_info_t; src: Psnd_seq_port_info_t); cdecl; external libasound;

function snd_seq_port_info_get_client(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_port(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_addr(info: Psnd_seq_port_info_t): Psnd_seq_addr_t; cdecl; external libasound;
function snd_seq_port_info_get_name(info: Psnd_seq_port_info_t): PChar; cdecl; external libasound;
function snd_seq_port_info_get_capability(info: Psnd_seq_port_info_t): cuint; cdecl; external libasound;
function snd_seq_port_info_get_type(info: Psnd_seq_port_info_t): cuint; cdecl; external libasound;
function snd_seq_port_info_get_midi_channels(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_midi_voices(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_synth_voices(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_read_use(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_write_use(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_port_specified(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_timestamping(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_timestamp_real(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_port_info_get_timestamp_queue(info: Psnd_seq_port_info_t): cint; cdecl; external libasound;

procedure snd_seq_port_info_set_client(info: Psnd_seq_port_info_t; client: cint); cdecl; external libasound;
procedure snd_seq_port_info_set_port(info: Psnd_seq_port_info_t; port: cint); cdecl; external libasound;
procedure snd_seq_port_info_set_addr(info: Psnd_seq_port_info_t; addr: Psnd_seq_addr_t); cdecl; external libasound;
procedure snd_seq_port_info_set_name(info: Psnd_seq_port_info_t; name: PChar); cdecl; external libasound;
procedure snd_seq_port_info_set_capability(info: Psnd_seq_port_info_t; capability: cuint); cdecl; external libasound;
procedure snd_seq_port_info_set_type(info: Psnd_seq_port_info_t; _type: cuint); cdecl; external libasound;
procedure snd_seq_port_info_set_midi_channels(info: Psnd_seq_port_info_t; channels: cint); cdecl; external libasound;
procedure snd_seq_port_info_set_midi_voices(info: Psnd_seq_port_info_t; voices: cint); cdecl; external libasound;
procedure snd_seq_port_info_set_synth_voices(info: Psnd_seq_port_info_t; voices: cint); cdecl; external libasound;
procedure snd_seq_port_info_set_port_specified(info: Psnd_seq_port_info_t; val: cint); cdecl; external libasound;
procedure snd_seq_port_info_set_timestamping(info: Psnd_seq_port_info_t; enable: cint); cdecl; external libasound;
procedure snd_seq_port_info_set_timestamp_real(info: Psnd_seq_port_info_t; realtime: cint); cdecl; external libasound;
procedure snd_seq_port_info_set_timestamp_queue(info: Psnd_seq_port_info_t; queue: cint); cdecl; external libasound;

function snd_seq_create_port(handle: Psnd_seq_t; info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_delete_port(handle: Psnd_seq_t; port: cint): cint; cdecl; external libasound;
function snd_seq_get_port_info(handle: Psnd_seq_t; port: cint; info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_get_any_port_info(handle: Psnd_seq_t; client: cint; port: cint; info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_set_port_info(handle: Psnd_seq_t; port: cint; info: Psnd_seq_port_info_t): cint; cdecl; external libasound;
function snd_seq_query_next_port(handle: Psnd_seq_t; info: Psnd_seq_port_info_t): cint; cdecl; external libasound;

(** \} *)


(**
 *  \defgroup SeqSubscribe Sequencer Port Subscription
 *  Sequencer Port Subscription
 *  \ingroup Sequencer
 *  \{
 *)

(** port subscription container *)
type
  PPsnd_seq_port_subscribe_t = ^Psnd_seq_port_subscribe_t;
  Psnd_seq_port_subscribe_t = ^snd_seq_port_subscribe_t;
  snd_seq_port_subscribe_t = record
  end;

function snd_seq_port_subscribe_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_port_subscribe_t container on stack *)
//#define snd_seq_port_subscribe_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_port_subscribe)
function snd_seq_port_subscribe_malloc(ptr: PPsnd_seq_port_subscribe_t): cint; cdecl; external libasound;
procedure snd_seq_port_subscribe_free(ptr: Psnd_seq_port_subscribe_t); cdecl; external libasound;
procedure snd_seq_port_subscribe_copy(dst: Psnd_seq_port_subscribe_t; src: Psnd_seq_port_subscribe_t); cdecl; external libasound;

function snd_seq_port_subscribe_get_sender(info: Psnd_seq_port_subscribe_t): Psnd_seq_addr_t; cdecl; external libasound;
function snd_seq_port_subscribe_get_dest(info: Psnd_seq_port_subscribe_t): Psnd_seq_addr_t; cdecl; external libasound;
function snd_seq_port_subscribe_get_queue(info: Psnd_seq_port_subscribe_t): cint; cdecl; external libasound;
function snd_seq_port_subscribe_get_exclusive(info: Psnd_seq_port_subscribe_t): cint; cdecl; external libasound;
function snd_seq_port_subscribe_get_time_update(info: Psnd_seq_port_subscribe_t): cint; cdecl; external libasound;
function snd_seq_port_subscribe_get_time_real(info: Psnd_seq_port_subscribe_t): cint; cdecl; external libasound;

procedure snd_seq_port_subscribe_set_sender(info: Psnd_seq_port_subscribe_t; addr: Psnd_seq_addr_t); cdecl; external libasound;
procedure snd_seq_port_subscribe_set_dest(info: Psnd_seq_port_subscribe_t; addr: Psnd_seq_addr_t); cdecl; external libasound;
procedure snd_seq_port_subscribe_set_queue(info: Psnd_seq_port_subscribe_t; q: cint); cdecl; external libasound;
procedure snd_seq_port_subscribe_set_exclusive(info: Psnd_seq_port_subscribe_t; val: cint); cdecl; external libasound;
procedure snd_seq_port_subscribe_set_time_update(info: Psnd_seq_port_subscribe_t; val: cint); cdecl; external libasound;
procedure snd_seq_port_subscribe_set_time_real(info: Psnd_seq_port_subscribe_t; val: cint); cdecl; external libasound;

function snd_seq_get_port_subscription(handle: Psnd_seq_t; sub: Psnd_seq_port_subscribe_t): cint; cdecl; external libasound;
function snd_seq_subscribe_port(handle: Psnd_seq_t; sub: Psnd_seq_port_subscribe_t): cint; cdecl; external libasound;
function snd_seq_unsubscribe_port(handle: Psnd_seq_t; sub: Psnd_seq_port_subscribe_t): cint; cdecl; external libasound;

(*
 *)

(** subscription query container *)
type
  PPsnd_seq_query_subscribe_t = ^Psnd_seq_query_subscribe_t;
  Psnd_seq_query_subscribe_t = ^snd_seq_query_subscribe_t;
  snd_seq_query_subscribe_t = record
  end;

(** type of query subscription *)
  Psnd_seq_query_subs_type_t = ^snd_seq_query_subs_type_t;
  snd_seq_query_subs_type_t = (
	SND_SEQ_QUERY_SUBS_READ,	(**< query read subscriptions *)
	SND_SEQ_QUERY_SUBS_WRITE	(**< query write subscriptions *)
  );

function snd_seq_query_subscribe_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_query_subscribe_t container on stack *)
//#define snd_seq_query_subscribe_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_query_subscribe)
function snd_seq_query_subscribe_malloc(ptr: PPsnd_seq_query_subscribe_t): cint; cdecl; external libasound;
procedure snd_seq_query_subscribe_free(ptr: Psnd_seq_query_subscribe_t); cdecl; external libasound;
procedure snd_seq_query_subscribe_copy(dst: Psnd_seq_query_subscribe_t; src: Psnd_seq_query_subscribe_t); cdecl; external libasound;

function snd_seq_query_subscribe_get_client(info: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;
function snd_seq_query_subscribe_get_port(info: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;
function snd_seq_query_subscribe_get_root(info: Psnd_seq_query_subscribe_t): Psnd_seq_addr_t; cdecl; external libasound;
function snd_seq_query_subscribe_get_type(info: Psnd_seq_query_subscribe_t): snd_seq_query_subs_type_t; cdecl; external libasound;
function snd_seq_query_subscribe_get_index(info: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;
function snd_seq_query_subscribe_get_num_subs(info: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;
function snd_seq_query_subscribe_get_addr(info: Psnd_seq_query_subscribe_t): Psnd_seq_addr_t; cdecl; external libasound;
function snd_seq_query_subscribe_get_queue(info: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;
function snd_seq_query_subscribe_get_exclusive(info: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;
function snd_seq_query_subscribe_get_time_update(info: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;
function snd_seq_query_subscribe_get_time_real(info: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;

procedure snd_seq_query_subscribe_set_client(info: Psnd_seq_query_subscribe_t; client: cint); cdecl; external libasound;
procedure snd_seq_query_subscribe_set_port(info: Psnd_seq_query_subscribe_t; port: cint); cdecl; external libasound;
procedure snd_seq_query_subscribe_set_root(info: Psnd_seq_query_subscribe_t; addr: Psnd_seq_addr_t); cdecl; external libasound;
procedure snd_seq_query_subscribe_set_type(info: Psnd_seq_query_subscribe_t; _type: snd_seq_query_subs_type_t); cdecl; external libasound;
procedure snd_seq_query_subscribe_set_index(info: Psnd_seq_query_subscribe_t; _index: cint); cdecl; external libasound;

function snd_seq_query_port_subscribers(seq: Psnd_seq_t; subs: Psnd_seq_query_subscribe_t): cint; cdecl; external libasound;

(** \} *)


(**
 *  \defgroup SeqQueue Sequencer Queue Interface
 *  Sequencer Queue Interface
 *  \ingroup Sequencer
 *  \{
 *)

type
(** queue information container *)
  PPsnd_seq_queue_info_t = ^Psnd_seq_queue_info_t;
  Psnd_seq_queue_info_t = ^snd_seq_queue_info_t;
  snd_seq_queue_info_t = record
  end;
(** queue status container *)
  PPsnd_seq_queue_status_t = ^Psnd_seq_queue_status_t;
  Psnd_seq_queue_status_t = ^snd_seq_queue_status_t;
  snd_seq_queue_status_t = record
  end;
(** queue tempo container *)
  PPsnd_seq_queue_tempo_t = ^Psnd_seq_queue_tempo_t;
  Psnd_seq_queue_tempo_t = ^snd_seq_queue_tempo_t;
  snd_seq_queue_tempo_t = record
  end;
(** queue timer information container *)
  PPsnd_seq_queue_timer_t = ^Psnd_seq_queue_timer_t;
  Psnd_seq_queue_timer_t = ^snd_seq_queue_timer_t;
  snd_seq_queue_timer_t = record
  end;

const
(** special queue ids *)
  SND_SEQ_QUEUE_DIRECT = 253;  (**< direct dispatch *)

function snd_seq_queue_info_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_queue_info_t container on stack *)
//#define snd_seq_queue_info_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_queue_info)
function snd_seq_queue_info_malloc(ptr: PPsnd_seq_queue_info_t): cint; cdecl; external libasound;
procedure snd_seq_queue_info_free(ptr: Psnd_seq_queue_info_t); cdecl; external libasound;
procedure snd_seq_queue_info_copy(dst: Psnd_seq_queue_info_t; src: Psnd_seq_queue_info_t); cdecl; external libasound;

function snd_seq_queue_info_get_queue(info: Psnd_seq_queue_info_t): cint; cdecl; external libasound;
function snd_seq_queue_info_get_name(info: Psnd_seq_queue_info_t): PChar; cdecl; external libasound;
function snd_seq_queue_info_get_owner(info: Psnd_seq_queue_info_t): cint; cdecl; external libasound;
function snd_seq_queue_info_get_locked(info: Psnd_seq_queue_info_t): cint; cdecl; external libasound;
function snd_seq_queue_info_get_flags(info: Psnd_seq_queue_info_t): cuint; cdecl; external libasound;

procedure snd_seq_queue_info_set_name(info: Psnd_seq_queue_info_t; name: PChar); cdecl; external libasound;
procedure snd_seq_queue_info_set_owner(info: Psnd_seq_queue_info_t; owner: cint); cdecl; external libasound;
procedure snd_seq_queue_info_set_locked(info: Psnd_seq_queue_info_t; locked: cint); cdecl; external libasound;
procedure snd_seq_queue_info_set_flags(info: Psnd_seq_queue_info_t; flags: cuint); cdecl; external libasound;

function snd_seq_create_queue(seq: Psnd_seq_t; info: Psnd_seq_queue_info_t): cint; cdecl; external libasound;
function snd_seq_alloc_named_queue(seq: Psnd_seq_t; name: PChar): cint; cdecl; external libasound;
function snd_seq_alloc_queue(handle: Psnd_seq_t): cint; cdecl; external libasound;
function snd_seq_free_queue(handle: Psnd_seq_t; q: cint): cint; cdecl; external libasound;
function snd_seq_get_queue_info(seq: Psnd_seq_t; q: cint; info: Psnd_seq_queue_info_t): cint; cdecl; external libasound;
function snd_seq_set_queue_info(seq: Psnd_seq_t; q: cint; info: Psnd_seq_queue_info_t): cint; cdecl; external libasound;
function snd_seq_query_named_queue(seq: Psnd_seq_t; name: PChar): cint; cdecl; external libasound;

function snd_seq_get_queue_usage(handle: Psnd_seq_t; q: cint): cint; cdecl; external libasound;
function snd_seq_set_queue_usage(handle: Psnd_seq_t; q: cint; used: cint): cint; cdecl; external libasound;

(*
 *)
function snd_seq_queue_status_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_queue_status_t container on stack *)
//#define snd_seq_queue_status_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_queue_status)
function snd_seq_queue_status_malloc(ptr: PPsnd_seq_queue_status_t): cint; cdecl; external libasound;
procedure snd_seq_queue_status_free(ptr: Psnd_seq_queue_status_t); cdecl; external libasound;
procedure snd_seq_queue_status_copy(dst: Psnd_seq_queue_status_t; src: Psnd_seq_queue_status_t); cdecl; external libasound;

function snd_seq_queue_status_get_queue(info: Psnd_seq_queue_status_t): cint; cdecl; external libasound;
function snd_seq_queue_status_get_events(info: Psnd_seq_queue_status_t): cint; cdecl; external libasound;
function snd_seq_queue_status_get_tick_time(info: Psnd_seq_queue_status_t): snd_seq_tick_time_t; cdecl; external libasound;
function snd_seq_queue_status_get_real_time(info: Psnd_seq_queue_status_t): Psnd_seq_real_time_t; cdecl; external libasound;
function snd_seq_queue_status_get_status(info: Psnd_seq_queue_status_t): cuint; cdecl; external libasound;

function snd_seq_get_queue_status(handle: Psnd_seq_t; q: cint; status: Psnd_seq_queue_status_t): cint; cdecl; external libasound;

(*
 *)
function snd_seq_queue_tempo_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_queue_tempo_t container on stack *)
//#define snd_seq_queue_tempo_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_queue_tempo)
function snd_seq_queue_tempo_malloc(ptr: PPsnd_seq_queue_tempo_t): cint; cdecl; external libasound;
procedure snd_seq_queue_tempo_free(ptr: Psnd_seq_queue_tempo_t); cdecl; external libasound;
procedure snd_seq_queue_tempo_copy(dst: Psnd_seq_queue_tempo_t; src: Psnd_seq_queue_tempo_t); cdecl; external libasound;

function snd_seq_queue_tempo_get_queue(info: Psnd_seq_queue_tempo_t): cint; cdecl; external libasound;
function snd_seq_queue_tempo_get_tempo(info: Psnd_seq_queue_tempo_t): cuint; cdecl; external libasound;
function snd_seq_queue_tempo_get_ppq(info: Psnd_seq_queue_tempo_t): cint; cdecl; external libasound;
function snd_seq_queue_tempo_get_skew(info: Psnd_seq_queue_tempo_t): cuint; cdecl; external libasound;
function snd_seq_queue_tempo_get_skew_base(info: Psnd_seq_queue_tempo_t): cuint; cdecl; external libasound;
procedure snd_seq_queue_tempo_set_tempo(info: Psnd_seq_queue_tempo_t; tempo: cuint); cdecl; external libasound;
procedure snd_seq_queue_tempo_set_ppq(info: Psnd_seq_queue_tempo_t; ppq: cint); cdecl; external libasound;
procedure snd_seq_queue_tempo_set_skew(info: Psnd_seq_queue_tempo_t; skew: cuint); cdecl; external libasound;
procedure snd_seq_queue_tempo_set_skew_base(info: Psnd_seq_queue_tempo_t; base: cuint); cdecl; external libasound;

function snd_seq_get_queue_tempo(handle: Psnd_seq_t; q: cint; tempo: Psnd_seq_queue_tempo_t): cint; cdecl; external libasound;
function snd_seq_set_queue_tempo(handle: Psnd_seq_t; q: cint; tempo: Psnd_seq_queue_tempo_t): cint; cdecl; external libasound;

(*
 *)

type
(** sequencer timer sources *)
  PPsnd_seq_queue_timer_type_t = ^Psnd_seq_queue_timer_type_t;
  Psnd_seq_queue_timer_type_t = ^snd_seq_queue_timer_type_t;
  snd_seq_queue_timer_type_t = (
	SND_SEQ_TIMER_ALSA = 0,		(* ALSA timer *)
	SND_SEQ_TIMER_MIDI_CLOCK = 1,	(* Midi Clock (CLOCK event) *)
	SND_SEQ_TIMER_MIDI_TICK = 2	(* Midi Timer Tick (TICK event *)
  );

function snd_seq_queue_timer_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_queue_timer_t container on stack *)
//#define snd_seq_queue_timer_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_queue_timer)
function snd_seq_queue_timer_malloc(ptr: PPsnd_seq_queue_timer_t): cint; cdecl; external libasound;
procedure snd_seq_queue_timer_free(ptr: Psnd_seq_queue_timer_t); cdecl; external libasound;
procedure snd_seq_queue_timer_copy(dst: Psnd_seq_queue_timer_t; src: Psnd_seq_queue_timer_t); cdecl; external libasound;

function snd_seq_queue_timer_get_queue(info: Psnd_seq_queue_timer_t): cint; cdecl; external libasound;
function snd_seq_queue_timer_get_type(info: Psnd_seq_queue_timer_t): snd_seq_queue_timer_type_t; cdecl; external libasound;
function snd_seq_queue_timer_get_id(info: Psnd_seq_queue_timer_t): Psnd_timer_id_t; cdecl; external libasound;
function snd_seq_queue_timer_get_resolution(info: Psnd_seq_queue_timer_t): cuint; cdecl; external libasound;

procedure snd_seq_queue_timer_set_type(info: Psnd_seq_queue_timer_t; type_: snd_seq_queue_timer_type_t); cdecl; external libasound;
procedure snd_seq_queue_timer_set_id(info: Psnd_seq_queue_timer_t; id: Psnd_timer_id_t); cdecl; external libasound;
procedure snd_seq_queue_timer_set_resolution(info: Psnd_seq_queue_timer_t; resolution: cuint); cdecl; external libasound;

function snd_seq_get_queue_timer(handle: Psnd_seq_t; q: cint; timer: Psnd_seq_queue_timer_t): cint; cdecl; external libasound;
function snd_seq_set_queue_timer(handle: Psnd_seq_t; q: cint; timer: Psnd_seq_queue_timer_t): cint; cdecl; external libasound;

(** \} *)

(**
 *  \defgroup SeqEvent Sequencer Event API
 *  Sequencer Event API
 *  \ingroup Sequencer
 *  \{
 *)

function snd_seq_free_event(ev: Psnd_seq_event_t): cint; cdecl; external libasound;
function snd_seq_event_length(ev: Psnd_seq_event_t): ssize_t; cdecl; external libasound;
function snd_seq_event_output(handle: Psnd_seq_t; ev: Psnd_seq_event_t): cint; cdecl; external libasound;
function snd_seq_event_output_buffer(handle: Psnd_seq_t; ev: Psnd_seq_event_t): cint; cdecl; external libasound;
function snd_seq_event_output_direct(handle: Psnd_seq_t; ev: Psnd_seq_event_t): cint; cdecl; external libasound;
function snd_seq_event_input(handle: Psnd_seq_t; ev: PPsnd_seq_event_t): cint; cdecl; external libasound;
function snd_seq_event_input_pending(seq: Psnd_seq_t; fetch_sequencer: cint): cint; cdecl; external libasound;
function snd_seq_drain_output(handle: Psnd_seq_t): cint; cdecl; external libasound;
function snd_seq_event_output_pending(seq: Psnd_seq_t): cint; cdecl; external libasound;
function snd_seq_extract_output(handle: Psnd_seq_t; ev: PPsnd_seq_event_t): cint; cdecl; external libasound;
function snd_seq_drop_output(handle: Psnd_seq_t): cint; cdecl; external libasound;
function snd_seq_drop_output_buffer(handle: Psnd_seq_t): cint; cdecl; external libasound;
function snd_seq_drop_input(handle: Psnd_seq_t): cint; cdecl; external libasound;
function snd_seq_drop_input_buffer(handle: Psnd_seq_t): cint; cdecl; external libasound;

type
(** event removal conditionals *)
  PPsnd_seq_remove_events_t = ^Psnd_seq_remove_events_t;
  Psnd_seq_remove_events_t = ^snd_seq_remove_events_t;
  snd_seq_remove_events_t = record
  end;

const
(** Remove conditional flags *)
  SND_SEQ_REMOVE_INPUT        = 1 shl 0;  (**< Flush input queues *)
  SND_SEQ_REMOVE_OUTPUT       = 1 shl 1;  (**< Flush output queues *)
  SND_SEQ_REMOVE_DEST         = 1 shl 2;  (**< Restrict by destination q:client:port *)
  SND_SEQ_REMOVE_DEST_CHANNEL = 1 shl 3;  (**< Restrict by channel *)
  SND_SEQ_REMOVE_TIME_BEFORE  = 1 shl 4;  (**< Restrict to before time *)
  SND_SEQ_REMOVE_TIME_AFTER   = 1 shl 5;  (**< Restrict to time or after *)
  SND_SEQ_REMOVE_TIME_TICK    = 1 shl 6;  (**< Time is in ticks *)
  SND_SEQ_REMOVE_EVENT_TYPE   = 1 shl 7;  (**< Restrict to event type *)
  SND_SEQ_REMOVE_IGNORE_OFF   = 1 shl 8;  (**< Do not flush off events *)
  SND_SEQ_REMOVE_TAG_MATCH    = 1 shl 9;  (**< Restrict to events with given tag *)

function snd_seq_remove_events_sizeof: size_t; cdecl; external libasound;
(** allocate a #snd_seq_remove_events_t container on stack *)
//#define snd_seq_remove_events_alloca(ptr) \
//__snd_alloca(ptr, snd_seq_remove_events)
function snd_seq_remove_events_malloc(ptr: PPsnd_seq_remove_events_t): cint; cdecl; external libasound;
procedure snd_seq_remove_events_free(ptr: Psnd_seq_remove_events_t); cdecl; external libasound;
procedure snd_seq_remove_events_copy(dst: Psnd_seq_remove_events_t; src: Psnd_seq_remove_events_t); cdecl; external libasound;

function snd_seq_remove_events_get_condition(info: Psnd_seq_remove_events_t): cuint; cdecl; external libasound;
function snd_seq_remove_events_get_queue(info: Psnd_seq_remove_events_t): cint; cdecl; external libasound;
function snd_seq_remove_events_get_time(info: Psnd_seq_remove_events_t): Psnd_seq_timestamp_t; cdecl; external libasound;
function snd_seq_remove_events_get_dest(info: Psnd_seq_remove_events_t): Psnd_seq_addr_t; cdecl; external libasound;
function snd_seq_remove_events_get_channel(info: Psnd_seq_remove_events_t): cint; cdecl; external libasound;
function snd_seq_remove_events_get_event_type(info: Psnd_seq_remove_events_t): cint; cdecl; external libasound;
function snd_seq_remove_events_get_tag(info: Psnd_seq_remove_events_t): cint; cdecl; external libasound;

procedure snd_seq_remove_events_set_condition(info: Psnd_seq_remove_events_t; flags: cuint); cdecl; external libasound;
procedure snd_seq_remove_events_set_queue(info: Psnd_seq_remove_events_t; queue: cint); cdecl; external libasound;
procedure snd_seq_remove_events_set_time(info: Psnd_seq_remove_events_t; time: Psnd_seq_timestamp_t); cdecl; external libasound;
procedure snd_seq_remove_events_set_dest(info: Psnd_seq_remove_events_t; addr: Psnd_seq_addr_t); cdecl; external libasound;
procedure snd_seq_remove_events_set_channel(info: Psnd_seq_remove_events_t; channel: cint); cdecl; external libasound;
procedure snd_seq_remove_events_set_event_type(info: Psnd_seq_remove_events_t; type_: cint); cdecl; external libasound;
procedure snd_seq_remove_events_set_tag(info: Psnd_seq_remove_events_t; tag: cint); cdecl; external libasound;

function snd_seq_remove_events(handle: Psnd_seq_t; info: Psnd_seq_remove_events_t): cint; cdecl; external libasound;

(** \} *)

(**
 *  \defgroup SeqMisc Sequencer Miscellaneous
 *  Sequencer Miscellaneous
 *  \ingroup Sequencer
 *  \{
 *)

procedure snd_seq_set_bit(nr: cint; array_: Pointer); cdecl; external libasound;
procedure snd_seq_unset_bit(nr: cint; array_: Pointer); cdecl; external libasound;
function snd_seq_change_bit(nr: cint; array_: Pointer): cint; cdecl; external libasound;
function snd_seq_get_bit(nr: cint; array_: Pointer): cint; cdecl; external libasound;

(** \} *)


(**
 *  \defgroup SeqEvType Sequencer Event type Checks
 *  Sequencer Event type Checks
 *  \ingroup Sequencer
 *  \{
 *)

(* event type macros *)
type
  TSND_SEQ_EVFLG = (
        SND_SEQ_EVFLG_RESULT,
	SND_SEQ_EVFLG_NOTE,
	SND_SEQ_EVFLG_CONTROL,
	SND_SEQ_EVFLG_QUEUE,
	SND_SEQ_EVFLG_SYSTEM,
	SND_SEQ_EVFLG_MESSAGE,
	SND_SEQ_EVFLG_CONNECTION,
	SND_SEQ_EVFLG_SAMPLE,
	SND_SEQ_EVFLG_USERS,
	SND_SEQ_EVFLG_INSTR,
	SND_SEQ_EVFLG_QUOTE,
	SND_SEQ_EVFLG_NONE,
	SND_SEQ_EVFLG_RAW,
	SND_SEQ_EVFLG_FIXED,
	SND_SEQ_EVFLG_VARIABLE,
	SND_SEQ_EVFLG_VARUSR
  );

  TSND_SEQ_EVFLG_NOTE = (
	SND_SEQ_EVFLG_NOTE_ONEARG,
	SND_SEQ_EVFLG_NOTE_TWOARG
  );

  TSND_SEQ_EVFLG_QUEUE = (
	SND_SEQ_EVFLG_QUEUE_NOARG,
	SND_SEQ_EVFLG_QUEUE_TICK,
	SND_SEQ_EVFLG_QUEUE_TIME,
	SND_SEQ_EVFLG_QUEUE_VALUE
  );

{**
 * Exported event type table
 *
 * This table is referred by snd_seq_ev_is_xxx.
 *}
//extern const unsigned int snd_seq_event_types[];
var
  snd_seq_event_types: Pcuint; external libasound;

function _SND_SEQ_TYPE(x: TSND_SEQ_EVFLG): cuint; inline;

//#define _SND_SEQ_TYPE_OPT(x)	((x)<<24)	/**< optional type - 8bit */

(** check the event type *)
function snd_seq_type_check(ev: Psnd_seq_event_t; x: TSND_SEQ_EVFLG): Boolean; inline;
(** event type check: result events *)
function snd_seq_ev_is_result_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: note events *)
function snd_seq_ev_is_note_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: control events *)
function snd_seq_ev_is_control_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: channel specific events *)
function snd_seq_ev_is_channel_type(ev: Psnd_seq_event_t): Boolean; inline;



(** event type check: queue control events *)
function snd_seq_ev_is_queue_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: system status messages *)
function snd_seq_ev_is_message_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: system status messages *)
function snd_seq_ev_is_subscribe_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: sample messages *)
function snd_seq_ev_is_sample_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: user-defined messages *)
function snd_seq_ev_is_user_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: instrument layer events *)
function snd_seq_ev_is_instr_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: fixed length events *)
function snd_seq_ev_is_fixed_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: variable length events *)
function snd_seq_ev_is_variable_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: user pointer events *)
function snd_seq_ev_is_varusr_type(ev: Psnd_seq_event_t): Boolean; inline;
(** event type check: reserved for kernel *)
function snd_seq_ev_is_reserved(ev: Psnd_seq_event_t): Boolean; inline;



(**
 * macros to check event flags
 *)
(** prior events *)
function snd_seq_ev_is_prior(ev: Psnd_seq_event_t): Boolean; inline;



(** get the data length type *)
function snd_seq_ev_length_type(ev: Psnd_seq_event_t): cuchar; inline;
(** fixed length events *)
function snd_seq_ev_is_fixed(ev: Psnd_seq_event_t): Boolean; inline;
(** variable length events *)
function snd_seq_ev_is_variable(ev: Psnd_seq_event_t): Boolean; inline;
(** variable length on user-space *)
function snd_seq_ev_is_varusr(ev: Psnd_seq_event_t): Boolean; inline;



(** time-stamp type *)
function snd_seq_ev_timestamp_type(ev: Psnd_seq_event_t): cuchar; inline;
(** event is in tick time *)
function snd_seq_ev_is_tick(ev: Psnd_seq_event_t): Boolean; inline;
(** event is in real-time *)
function snd_seq_ev_is_real(ev: Psnd_seq_event_t): Boolean; inline;



(** time-mode type *)
function snd_seq_ev_timemode_type(ev: Psnd_seq_event_t): cuchar; inline;
(** scheduled in absolute time *)
function snd_seq_ev_is_abstime(ev: Psnd_seq_event_t): Boolean; inline;
(** scheduled in relative time *)
function snd_seq_ev_is_reltime(ev: Psnd_seq_event_t): Boolean; inline;



(** direct dispatched events *)
function snd_seq_ev_is_direct(ev: Psnd_seq_event_t): Boolean; inline;

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_SEQ_H */*)
