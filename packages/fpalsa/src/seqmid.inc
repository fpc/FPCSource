{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

(**
 * \file include/seqmid.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Application interface library for the ALSA driver
 *)
(*
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

//#ifndef __ALSA_SEQMID_H
//#define __ALSA_SEQMID_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup SeqMiddle Sequencer Middle Level Interface
 *  Sequencer Middle Level Interface
 *  \ingroup Sequencer
 *  \{
 *)

(**
 * \brief initialize event record
 * \param ev event record pointer
 *
 * This macro clears the given event record pointer to the default status.
 *)
procedure snd_seq_ev_clear(ev: Psnd_seq_event_t); inline;


(**
 * \brief set the tag for given event
 * \param ev event record
 * \param t event tag
 *
 * This macro sets the tag to the given event record.
 *)
procedure snd_seq_ev_set_tag(ev: Psnd_seq_event_t; t: cuchar); inline;

(**
 * \brief set the explicit destination
 * \param ev event record
 * \param c destination client id
 * \param p destination port id
 *
 * This macro sets the client and port id numbers to the given event record.
 *
 * \sa snd_seq_ev_set_subs()
 *)
procedure snd_seq_ev_set_dest(ev: Psnd_seq_event_t; c, p: cuchar); inline;

(**
 * \brief set broadcasting to subscribers
 * \param ev event record
 *
 * This macro sets the destination as the subscribers.
 *
 * \sa snd_seq_ev_set_dest()
 *)
procedure snd_seq_ev_set_subs(ev: Psnd_seq_event_t); inline;

(**
 * \brief set broadcasting to all clients/ports
 * \param ev event record
 *
 * This macro sets the destination as the broadcasting.
 *
 * \sa snd_seq_ev_set_dest()
 *)
procedure snd_seq_ev_set_broadcast(ev: Psnd_seq_event_t); inline;

(**
 * \brief set the source port
 * \param ev event record
 * \param p source port id
 *
 * This macro sets the source port id number.
 *)
procedure snd_seq_ev_set_source(ev: Psnd_seq_event_t; p: cuchar); inline;

(**
 * \brief set direct passing mode (without queued)
 * \param ev event instance
 *
 * This macro sets the event to the direct passing mode
 * to be delivered immediately without queueing.
 *
 * \sa snd_seq_ev_schedule_tick(), snd_seq_ev_schedule_real()
 *)
procedure snd_seq_ev_set_direct(ev: Psnd_seq_event_t); inline;

(**
 * \brief set tick-scheduling mode on queue
 * \param ev event instance
 * \param q queue id to schedule
 * \param relative relative time-stamp if non-zero
 * \param ttick tick time-stamp to be delivered
 *
 * This macro sets the scheduling of the event in the
 * MIDI tick mode.
 *
 * \sa snd_seq_ev_schedule_real(), snd_seq_ev_set_direct()
 *)
procedure snd_seq_ev_schedule_tick(ev: Psnd_seq_event_t; q: cuchar; relative: Boolean; ttick: snd_seq_tick_time_t); inline;

(**
 * \brief set real-time-scheduling mode on queue
 * \param ev event instance
 * \param q queue id to schedule
 * \param relative relative time-stamp if non-zero
 * \param rtime time-stamp to be delivered
 *
 * This macro sets the scheduling of the event in the
 * realtime mode.
 *
 * \sa snd_seq_ev_schedule_tick(), snd_seq_ev_set_direct()
 *)
procedure snd_seq_ev_schedule_real(ev: Psnd_seq_event_t; q: cuchar; relative: Boolean; rtime: Psnd_seq_real_time_t); inline;

(**
 * \brief set event priority
 * \param ev event instance
 * \param high_prior 1 for high priority mode
 *)
procedure snd_seq_ev_set_priority(ev: Psnd_seq_event_t; high_prior: Boolean); inline;

(**
 * \brief set fixed data
 * \param ev event instance
 *
 * Sets the event length mode as fixed size.
 *
 * \sa snd_seq_ev_set_variable(), snd_seq_ev_set_varusr()
 *)
procedure snd_seq_ev_set_fixed(ev: Psnd_seq_event_t); inline;

(**
 * \brief set variable data
 * \param ev event instance
 * \param datalen length of the external data
 * \param dataptr pointer of the external data
 *
 * Sets the event length mode as variable length and stores the data.
 *
 * \sa snd_seq_ev_set_fixed(), snd_seq_ev_set_varusr()
 *)
procedure snd_seq_ev_set_variable(ev: Psnd_seq_event_t; datalen: cuint; dataptr: Pointer); inline;

(**
 * \brief set varusr data
 * \param ev event instance
 * \param datalen length of the external data
 * \param dataptr pointer of the external data
 *
 * Sets the event length mode as variable user-space data and stores the data.
 *
 * \sa snd_seq_ev_set_fixed(), snd_seq_ev_set_variable()
 *)
procedure snd_seq_ev_set_varusr(ev: Psnd_seq_event_t; datalen: cuint; dataptr: Pointer); inline;

(**
 * \brief set queue controls
 * \param ev event record
 * \param typ event type
 * \param q queue id
 * \param val control value
 *)
procedure snd_seq_ev_set_queue_control(ev: Psnd_seq_event_t; typ: snd_seq_event_type; q: cuchar; val: cint); inline;

(**
 * \brief set the start queue event
 * \param ev event record
 * \param q queue id to start
 *
 * \sa snd_seq_ev_set_queue_stop(), snd_seq_ev_set_queue_continue()
 *)
procedure snd_seq_ev_set_queue_start(ev: Psnd_seq_event_t; q: cuchar); inline;

(**
 * \brief set the stop queue event
 * \param ev event record
 * \param q queue id to stop
 *
 * \sa snd_seq_ev_set_queue_start(), snd_seq_ev_set_queue_continue()
 *)
procedure snd_seq_ev_set_queue_stop(ev: Psnd_seq_event_t; q: cuchar); inline;

(**
 * \brief set the stop queue event
 * \param ev event record
 * \param q queue id to continue
 *
 * \sa snd_seq_ev_set_queue_start(), snd_seq_ev_set_queue_stop()
 *)
procedure snd_seq_ev_set_queue_continue(ev: Psnd_seq_event_t; q: cuchar); inline;

(**
 * \brief set the stop queue event
 * \param ev event record
 * \param q queue id to change tempo
 * \param val the new tempo value
 *)
procedure snd_seq_ev_set_queue_tempo(ev: Psnd_seq_event_t; q: cuchar; val: cint); inline;

(**
 * \brief set the real-time position of a queue
 * \param ev event record
 * \param q queue id to change tempo
 * \param rtime the new real-time pointer
 *)
procedure snd_seq_ev_set_queue_pos_real(ev: Psnd_seq_event_t; q: cuchar; rtime: Psnd_seq_real_time_t); inline;

(**
 * \brief set the tick-time position of a queue
 * \param ev event record
 * \param q queue id to change tempo
 * \param ttime the new tick-time
 *)
procedure snd_seq_ev_set_queue_pos_tick(ev: Psnd_seq_event_t; q: cuchar; ttime: snd_seq_tick_time_t); inline;


(* set and send a queue control event *)
function snd_seq_control_queue(seq: Psnd_seq_t; q, type_, value: cint; ev: Psnd_seq_event_t): cint; cdecl; external libasound;

(**
 * \brief start the specified queue
 * \param seq sequencer handle
 * \param q queue id to start
 * \param ev optional event record (see #snd_seq_control_queue)
 *)
function snd_seq_start_queue(seq: Psnd_seq_t; q: cint; ev: Psnd_seq_event_t): cint; inline;

(**
 * \brief stop the specified queue
 * \param seq sequencer handle
 * \param q queue id to stop
 * \param ev optional event record (see #snd_seq_control_queue)
 *)
function snd_seq_stop_queue(seq: Psnd_seq_t; q: cint; ev: Psnd_seq_event_t): cint; inline;

(**
 * \brief continue the specified queue
 * \param seq sequencer handle
 * \param q queue id to continue
 * \param ev optional event record (see #snd_seq_control_queue)
 *)
function snd_seq_continue_queue(seq: Psnd_seq_t; q: cint; ev: Psnd_seq_event_t): cint; inline;

(**
 * \brief change the tempo of the specified queue
 * \param seq sequencer handle
 * \param q queue id
 * \param tempo the new tempo value
 * \param ev optional event record (see #snd_seq_control_queue)
 *)
function snd_seq_change_queue_tempo(seq: Psnd_seq_t; q, tempo: cint; ev: Psnd_seq_event_t): cint; inline;

(* create a port - simple version - return the port number *)
function snd_seq_create_simple_port(seq: Psnd_seq_t; name: PChar;
                                    caps, type_: cuint): cint; cdecl; external libasound;
(* delete the port *)
function snd_seq_delete_simple_port(seq: Psnd_seq_t; port: cint): cint; cdecl; external libasound;

(* simple subscription between this port and another port
   (w/o exclusive & time conversion)
   *)
function snd_seq_connect_from(seq: Psnd_seq_t; my_port, src_client, src_port: cint): cint; cdecl; external libasound;
function snd_seq_connect_to(seq: Psnd_seq_t; my_port, dest_client, dest_port: cint): cint; cdecl; external libasound;
function snd_seq_disconnect_from(seq: Psnd_seq_t; my_port, src_client, src_port: cint): cint; cdecl; external libasound;
function snd_seq_disconnect_to(seq: Psnd_seq_t; my_port, dest_client, dest_port: cint): cint; cdecl; external libasound;

(*
 * set client information
 *)
function snd_seq_set_client_name(seq: Psnd_seq_t; name: PChar): cint; cdecl; external libasound;
function snd_seq_set_client_event_filter(seq: Psnd_seq_t; event_type: cint): cint; cdecl; external libasound;
function snd_seq_set_client_pool_output(seq: Psnd_seq_t; size: size_t): cint; cdecl; external libasound;
function snd_seq_set_client_pool_output_room(seq: Psnd_seq_t; size: size_t): cint; cdecl; external libasound;
function snd_seq_set_client_pool_input(seq: Psnd_seq_t; size: size_t): cint; cdecl; external libasound;
(* sync output queue *)
function snd_seq_sync_output_queue(seq: Psnd_seq_t): cint; cdecl; external libasound;

(*
 * parse the given string and get the sequencer address
 *)
function snd_seq_parse_address(seq: Psnd_seq_t; addr: Psnd_seq_addr_t; str: PChar): cint; cdecl; external libasound;

(*
 * reset client input/output pool
 *)
function snd_seq_reset_pool_output(seq: Psnd_seq_t): cint; cdecl; external libasound;
function snd_seq_reset_pool_input(seq: Psnd_seq_t): cint; cdecl; external libasound;

(**
 * \brief set note event
 * \param ev event record
 * \param ch channel number
 * \param key note key
 * \param vel velocity
 * \param dur duration (in tick or msec)
 *)
procedure snd_seq_ev_set_note(ev: Psnd_seq_event_t; ch, key, vel: cuchar; dur: cuint); inline;

(**
 * \brief set note-on event
 * \param ev event record
 * \param ch channel number
 * \param key note key
 * \param vel velocity
 *)
procedure snd_seq_ev_set_noteon(ev: Psnd_seq_event_t; ch, key, vel: cuchar); inline;

(**
 * \brief set note-off event
 * \param ev event record
 * \param ch channel number
 * \param key note key
 * \param vel velocity
 *)
procedure snd_seq_ev_set_noteoff(ev: Psnd_seq_event_t; ch, key, vel: cuchar); inline;

(**
 * \brief set key-pressure event
 * \param ev event record
 * \param ch channel number
 * \param key note key
 * \param vel velocity
 *)
procedure snd_seq_ev_set_keypress(ev: Psnd_seq_event_t; ch, key, vel: cuchar); inline;

(**
 * \brief set MIDI controller event
 * \param ev event record
 * \param ch channel number
 * \param cc controller number
 * \param val control value
 *)
procedure snd_seq_ev_set_controller(ev: Psnd_seq_event_t; ch: cuchar; cc: cuint; val: cint); inline;

(**
 * \brief set program change event
 * \param ev event record
 * \param ch channel number
 * \param val program number
 *)
procedure snd_seq_ev_set_pgmchange(ev: Psnd_seq_event_t; ch: cuchar; val: cint); inline;

(**
 * \brief set pitch-bend event
 * \param ev event record
 * \param ch channel number
 * \param val pitch bend; zero centered from -8192 to 8191
 *)
procedure snd_seq_ev_set_pitchbend(ev: Psnd_seq_event_t; ch: cuchar; val: cint); inline;

(**
 * \brief set channel pressure event
 * \param ev event record
 * \param ch channel number
 * \param val channel pressure value
 *)
procedure snd_seq_ev_set_chanpress(ev: Psnd_seq_event_t; ch: cuchar; val: cint); inline;

(**
 * \brief set sysex event
 * \param ev event record
 * \param datalen length of sysex data
 * \param dataptr sysex data pointer
 *
 * the sysex data must contain the start byte 0xf0 and the end byte 0xf7.
 *)
procedure snd_seq_ev_set_sysex(ev: Psnd_seq_event_t; datalen: cuint; dataptr: Pointer); inline;

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_SEQMID_H */
