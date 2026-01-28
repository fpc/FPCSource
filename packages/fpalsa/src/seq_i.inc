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

//#define _SND_SEQ_TYPE(x)	(1<<(x))	/**< master type - 24bit */
function _SND_SEQ_TYPE(x: TSND_SEQ_EVFLG): cuint; inline;
begin
  Result := 1 shl cint(x);
end;

//#define _SND_SEQ_TYPE_OPT(x)	((x)<<24)	/**< optional type - 8bit */

(** check the event type *)
function snd_seq_type_check(ev: Psnd_seq_event_t; x: TSND_SEQ_EVFLG): Boolean; inline;
begin
  Result := (snd_seq_event_types[ev^.type_] and _SND_SEQ_TYPE(x)) <> 0;
end;

(** event type check: result events *)
function snd_seq_ev_is_result_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_RESULT);
end;

(** event type check: note events *)
function snd_seq_ev_is_note_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_NOTE);
end;

(** event type check: control events *)
function snd_seq_ev_is_control_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_CONTROL);
end;

(** event type check: channel specific events *)
function snd_seq_ev_is_channel_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := (snd_seq_event_types[ev^.type_] and (_SND_SEQ_TYPE(SND_SEQ_EVFLG_NOTE) or _SND_SEQ_TYPE(SND_SEQ_EVFLG_CONTROL))) <> 0;
end;



(** event type check: queue control events *)
function snd_seq_ev_is_queue_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_QUEUE);
end;

(** event type check: system status messages *)
function snd_seq_ev_is_message_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_MESSAGE);
end;

(** event type check: system status messages *)
function snd_seq_ev_is_subscribe_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_CONNECTION);
end;

(** event type check: sample messages *)
function snd_seq_ev_is_sample_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_SAMPLE);
end;

(** event type check: user-defined messages *)
function snd_seq_ev_is_user_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_USERS);
end;

(** event type check: instrument layer events *)
function snd_seq_ev_is_instr_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_INSTR);
end;

(** event type check: fixed length events *)
function snd_seq_ev_is_fixed_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_FIXED);
end;

(** event type check: variable length events *)
function snd_seq_ev_is_variable_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_VARIABLE);
end;

(** event type check: user pointer events *)
function snd_seq_ev_is_varusr_type(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_type_check(ev, SND_SEQ_EVFLG_VARUSR);
end;

(** event type check: reserved for kernel *)
function snd_seq_ev_is_reserved(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_event_types[ev^.type_] = 0;
end;



(**
 * macros to check event flags
 *)
(** prior events *)
function snd_seq_ev_is_prior(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := (ev^.flags and SND_SEQ_PRIORITY_MASK) = SND_SEQ_PRIORITY_HIGH;
end;



(** get the data length type *)
function snd_seq_ev_length_type(ev: Psnd_seq_event_t): cuchar; inline;
begin
  Result := ev^.flags and SND_SEQ_EVENT_LENGTH_MASK;
end;

(** fixed length events *)
function snd_seq_ev_is_fixed(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_ev_length_type(ev) = SND_SEQ_EVENT_LENGTH_FIXED;
end;

(** variable length events *)
function snd_seq_ev_is_variable(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_ev_length_type(ev) = SND_SEQ_EVENT_LENGTH_VARIABLE;
end;

(** variable length on user-space *)
function snd_seq_ev_is_varusr(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_ev_length_type(ev) = SND_SEQ_EVENT_LENGTH_VARUSR;
end;



(** time-stamp type *)
function snd_seq_ev_timestamp_type(ev: Psnd_seq_event_t): cuchar; inline;
begin
  Result := ev^.flags and SND_SEQ_TIME_STAMP_MASK;
end;

(** event is in tick time *)
function snd_seq_ev_is_tick(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_ev_timestamp_type(ev) = SND_SEQ_TIME_STAMP_TICK;
end;

(** event is in real-time *)
function snd_seq_ev_is_real(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_ev_timestamp_type(ev) = SND_SEQ_TIME_STAMP_REAL;
end;



(** time-mode type *)
function snd_seq_ev_timemode_type(ev: Psnd_seq_event_t): cuchar; inline;
begin
  Result := ev^.flags and SND_SEQ_TIME_MODE_MASK;
end;

(** scheduled in absolute time *)
function snd_seq_ev_is_abstime(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_ev_timemode_type(ev) = SND_SEQ_TIME_MODE_ABS;
end;

(** scheduled in relative time *)
function snd_seq_ev_is_reltime(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := snd_seq_ev_timemode_type(ev) = SND_SEQ_TIME_MODE_REL;
end;



(** direct dispatched events *)
function snd_seq_ev_is_direct(ev: Psnd_seq_event_t): Boolean; inline;
begin
  Result := ev^.queue = SND_SEQ_QUEUE_DIRECT;
end;
