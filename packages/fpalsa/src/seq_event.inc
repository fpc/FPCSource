{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

(**
 * \file include/seq_event.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Application interface library for the ALSA driver
 */
/*
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

//#ifndef __ALSA_SEQ_EVENT_H
//#define __ALSA_SEQ_EVENT_H

(**
 *  \defgroup SeqEvents Sequencer Event Definitions
 *  Sequencer Event Definitions
 *  \ingroup Sequencer
 *  \{
 *)

(**
 * Sequencer event data type
 *)
type
  Psnd_seq_event_type_t = ^snd_seq_event_type_t;
  snd_seq_event_type_t = cuchar;

(** Sequencer event type *)
  Psnd_seq_event_type = ^snd_seq_event_type;
  snd_seq_event_type = (
	(** system status; event data type = #snd_seq_result_t *)
	SND_SEQ_EVENT_SYSTEM = 0,
	(** returned result status; event data type = #snd_seq_result_t *)
	SND_SEQ_EVENT_RESULT,

	(** note on and off with duration; event data type = #snd_seq_ev_note_t *)
	SND_SEQ_EVENT_NOTE = 5,
	(** note on; event data type = #snd_seq_ev_note_t *)
	SND_SEQ_EVENT_NOTEON,
	(** note off; event data type = #snd_seq_ev_note_t *)
	SND_SEQ_EVENT_NOTEOFF,
	(** key pressure change (aftertouch); event data type = #snd_seq_ev_note_t *)
	SND_SEQ_EVENT_KEYPRESS,
	
	(** controller; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_CONTROLLER = 10,
	(** program change; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_PGMCHANGE,
	(** channel pressure; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_CHANPRESS,
	(** pitchwheel; event data type = #snd_seq_ev_ctrl_t; data is from -8192 to 8191) *)
	SND_SEQ_EVENT_PITCHBEND,
	(** 14 bit controller value; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_CONTROL14,
	(** 14 bit NRPN;  event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_NONREGPARAM,
	(** 14 bit RPN; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_REGPARAM,

	(** SPP with LSB and MSB values; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_SONGPOS = 20,
	(** Song Select with song ID number; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_SONGSEL,
	(** midi time code quarter frame; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_QFRAME,
	(** SMF Time Signature event; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_TIMESIGN,
	(** SMF Key Signature event; event data type = #snd_seq_ev_ctrl_t *)
	SND_SEQ_EVENT_KEYSIGN,
	
	(** MIDI Real Time Start message; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_START = 30,
	(** MIDI Real Time Continue message; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_CONTINUE,
	(** MIDI Real Time Stop message; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_STOP,
	(** Set tick queue position; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_SETPOS_TICK,
	(** Set real-time queue position; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_SETPOS_TIME,
	(** (SMF) Tempo event; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_TEMPO,
	(** MIDI Real Time Clock message; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_CLOCK,
	(** MIDI Real Time Tick message; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_TICK,
	(** Queue timer skew; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_QUEUE_SKEW,
	(** Sync position changed; event data type = #snd_seq_ev_queue_control_t *)
	SND_SEQ_EVENT_SYNC_POS,

	(** Tune request; event data type = none *)
	SND_SEQ_EVENT_TUNE_REQUEST = 40,
	(** Reset to power-on state; event data type = none *)
	SND_SEQ_EVENT_RESET,
	(** Active sensing event; event data type = none *)
	SND_SEQ_EVENT_SENSING,

	(** Echo-back event; event data type = any type *)
	SND_SEQ_EVENT_ECHO = 50,
	(** OSS emulation raw event; event data type = any type *)
	SND_SEQ_EVENT_OSS,

	(** New client has connected; event data type = #snd_seq_addr_t *)
	SND_SEQ_EVENT_CLIENT_START = 60,
	(** Client has left the system; event data type = #snd_seq_addr_t *)
	SND_SEQ_EVENT_CLIENT_EXIT,
	(** Client status/info has changed; event data type = #snd_seq_addr_t *)
	SND_SEQ_EVENT_CLIENT_CHANGE,
	(** New port was created; event data type = #snd_seq_addr_t *)
	SND_SEQ_EVENT_PORT_START,
	(** Port was deleted from system; event data type = #snd_seq_addr_t *)
	SND_SEQ_EVENT_PORT_EXIT,
	(** Port status/info has changed; event data type = #snd_seq_addr_t *)
	SND_SEQ_EVENT_PORT_CHANGE,

	(** Ports connected; event data type = #snd_seq_connect_t *)
	SND_SEQ_EVENT_PORT_SUBSCRIBED,
	(** Ports disconnected; event data type = #snd_seq_connect_t *)
	SND_SEQ_EVENT_PORT_UNSUBSCRIBED,

	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR0 = 90,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR1,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR2,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR3,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR4,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR5,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR6,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR7,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR8,
	(** user-defined event; event data type = any (fixed size) *)
	SND_SEQ_EVENT_USR9,

	(** system exclusive data (variable length);  event data type = #snd_seq_ev_ext_t *)
	SND_SEQ_EVENT_SYSEX = 130,
	(** error event;  event data type = #snd_seq_ev_ext_t *)
	SND_SEQ_EVENT_BOUNCE,
	(** reserved for user apps;  event data type = #snd_seq_ev_ext_t *)
	SND_SEQ_EVENT_USR_VAR0 = 135,
	(** reserved for user apps; event data type = #snd_seq_ev_ext_t *)
	SND_SEQ_EVENT_USR_VAR1,
	(** reserved for user apps; event data type = #snd_seq_ev_ext_t *)
	SND_SEQ_EVENT_USR_VAR2,
	(** reserved for user apps; event data type = #snd_seq_ev_ext_t *)
	SND_SEQ_EVENT_USR_VAR3,
	(** reserved for user apps; event data type = #snd_seq_ev_ext_t *)
	SND_SEQ_EVENT_USR_VAR4,

	(** NOP; ignored in any case *)
	SND_SEQ_EVENT_NONE = 255);


(** Sequencer event address *)
  Psnd_seq_addr_t = ^snd_seq_addr_t;
  snd_seq_addr_t = record
	client: cuchar;	(**< Client id *)
	port: cuchar;		(**< Port id *)
  end;

(** Connection (subscription) between ports *)
  Psnd_seq_connect_t = ^snd_seq_connect_t;
  snd_seq_connect_t = record
	sender: snd_seq_addr_t;	(**< sender address *)
	dest: snd_seq_addr_t;		(**< destination address *)
  end;


(** Real-time data record *)
  Psnd_seq_real_time_t = ^snd_seq_real_time_t;
  snd_seq_real_time_t = record
	tv_sec: cuint;		(**< seconds *)
	tv_nsec: cuint;	(**< nanoseconds *)
  end;

(** (MIDI) Tick-time data record *)
  Psnd_seq_tick_time_t = ^snd_seq_tick_time_t;
  snd_seq_tick_time_t = cuint;

(** unioned time stamp *)
  Psnd_seq_timestamp_t = ^snd_seq_timestamp_t;
  snd_seq_timestamp_t = record
    case Integer of
      0: (
          tick: snd_seq_tick_time_t;	(**< tick-time *)
        );
      1: (
          time: snd_seq_real_time_t;	(**< real-time *)
        );
  end;


(**
 * Event mode flags
 *
 * NOTE: only 8 bits available!
 *)
const
  SND_SEQ_TIME_STAMP_TICK       = 0 shl 0;  (**< timestamp in clock ticks *)
  SND_SEQ_TIME_STAMP_REAL       = 1 shl 0;  (**< timestamp in real time *)
  SND_SEQ_TIME_STAMP_MASK       = 1 shl 0;  (**< mask for timestamp bits *)

  SND_SEQ_TIME_MODE_ABS         = 0 shl 1;  (**< absolute timestamp *)
  SND_SEQ_TIME_MODE_REL         = 1 shl 1;  (**< relative to current time *)
  SND_SEQ_TIME_MODE_MASK        = 1 shl 1;  (**< mask for time mode bits *)

  SND_SEQ_EVENT_LENGTH_FIXED    = 0 shl 2;  (**< fixed event size *)
  SND_SEQ_EVENT_LENGTH_VARIABLE = 1 shl 2;  (**< variable event size *)
  SND_SEQ_EVENT_LENGTH_VARUSR   = 2 shl 2;  (**< variable event size - user memory space *)
  SND_SEQ_EVENT_LENGTH_MASK     = 3 shl 2;  (**< mask for event length bits *)

  SND_SEQ_PRIORITY_NORMAL       = 0 shl 4;  (**< normal priority *)
  SND_SEQ_PRIORITY_HIGH         = 1 shl 4;  (**< event should be processed before others *)
  SND_SEQ_PRIORITY_MASK         = 1 shl 4;  (**< mask for priority bits *)


type
(** Note event *)
  Psnd_seq_ev_note_t = ^snd_seq_ev_note_t;
  snd_seq_ev_note_t = record
    channel: cuchar;		(**< channel number *)
    note: cuchar;		(**< note *)
    velocity: cuchar;		(**< velocity *)
    off_velocity: cuchar;	(**< note-off velocity; only for #SND_SEQ_EVENT_NOTE *)
    duration: cuint;		(**< duration until note-off; only for #SND_SEQ_EVENT_NOTE *)
  end;

(** Controller event *)
  Psnd_seq_ev_ctrl_t = ^snd_seq_ev_ctrl_t;
  snd_seq_ev_ctrl_t = record
    channel: cuchar;                (**< channel number *)
    unused: array [0..2] of cuchar;  (**< reserved *)
    param: cuint;                   (**< control parameter *)
    value: cint;                    (**< control value *)
  end;

(** generic set of bytes (12x8 bit) *)
  Psnd_seq_ev_raw8_t = ^snd_seq_ev_raw8_t;
  snd_seq_ev_raw8_t = record
    d: array [0..11] of cuchar;  (**< 8 bit value *)
  end;

(** generic set of integers (3x32 bit) *)
  Psnd_seq_ev_raw32_t = ^snd_seq_ev_raw32_t;
  snd_seq_ev_raw32_t = record
    d: array [0..2] of cuint;  (**< 32 bit value *)
  end;

(** external stored data *)
  Psnd_seq_ev_ext_t = ^snd_seq_ev_ext_t;
  snd_seq_ev_ext_t = {__attribute__((packed))} packed record
    len: cuint;    (**< length of data *)
    ptr: Pointer;  (**< pointer to data (note: can be 64-bit) *)
  end;

(** Result events *)
  Psnd_seq_result_t = ^snd_seq_result_t;
  snd_seq_result_t = record
    event: cint;   (**< processed event type *)
    result: cint;  (**< status *)
  end;

(** Queue skew values *)
  Psnd_seq_queue_skew_t = ^snd_seq_queue_skew_t;
  snd_seq_queue_skew_t = record
    value: cuint;  (**< skew value *)
    base: cuint;   (**< skew base *)
  end;

(** queue timer control *)
  Psnd_seq_ev_queue_control_t = ^snd_seq_ev_queue_control_t;
  snd_seq_ev_queue_control_t = record
    queue: cuchar;			(**< affected queue *)
    unused: array [0..2] of cuchar;		(**< reserved *)
    param: record  (**< data value union *)
      case Integer of
        0: (
          value: cint;                 (**< affected value (e.g. tempo) *)
        );
        1: (
          time: snd_seq_timestamp_t;   (**< time *)
        );
        2: (
          position: cuint;             (**< sync position *)
        );
        3: (
          skew: snd_seq_queue_skew_t;  (**< queue skew *)
        );
        4: (
          d32: array [0..1] of cuint;   (**< any data *)
        );
        5: (
          d8: array [0..7] of cuchar;   (**< any data *)
        );
    end;
  end;


(** Sequencer event *)
  PPsnd_seq_event_t = ^Psnd_seq_event_t;
  Psnd_seq_event_t = ^snd_seq_event_t;
  snd_seq_event_t = record
    type_: snd_seq_event_type_t;  (**< event type *)
    flags: cuchar;                (**< event flags *)
    tag: cuchar;                  (**< tag *)

    queue: cuchar;                (**< schedule queue *)
    time: snd_seq_timestamp_t;    (**< schedule time *)

    source: snd_seq_addr_t;       (**< source address *)
    dest: snd_seq_addr_t;         (**< destination address *)

    data: record  (**< event data... *)
      case Integer of
        0: (
          note: snd_seq_ev_note_t;           (**< note information *)
        );
        1: (
          control: snd_seq_ev_ctrl_t;        (**< MIDI control information *)
        );
        2: (
          raw8: snd_seq_ev_raw8_t;           (**< raw8 data *)
        );
        3: (
          raw32: snd_seq_ev_raw32_t;         (**< raw32 data *)
        );
        4: (
          ext: snd_seq_ev_ext_t;             (**< external data *)
        );
        5: (
          queue: snd_seq_ev_queue_control_t; (**< queue control *)
        );
        6: (
          time: snd_seq_timestamp_t;         (**< timestamp *)
        );
        7: (
          addr: snd_seq_addr_t;              (**< address *)
        );
        8: (
          connect: snd_seq_connect_t;        (**< connect information *)
        );
        9: (
          result: snd_seq_result_t;          (**< operation result code *)
        );
    end;
  end;


(** \} *)

//#endif /* __ALSA_SEQ_EVENT_H */*)

