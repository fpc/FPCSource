{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

(**
 * \file include/seq_midi_event.h
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

//#ifndef __ALSA_SEQ_MIDI_EVENT_H
//#define __ALSA_SEQ_MIDI_EVENT_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup MIDI_Event Sequencer event <-> MIDI byte stream coder
 *  \ingroup Sequencer
 *  Sequencer event <-> MIDI byte stream coder
 *  \{
 *)

(** container for sequencer midi event parsers *)
type
  PPsnd_midi_event_t = ^Psnd_midi_event_t;
  Psnd_midi_event_t = ^snd_midi_event_t;
  snd_midi_event_t = record
  end;

function snd_midi_event_new(bufsize: size_t; rdev: PPsnd_midi_event_t): cint; cdecl; external libasound;
function snd_midi_event_resize_buffer(dev: Psnd_midi_event_t; bufsize: size_t): cint; cdecl; external libasound;
procedure snd_midi_event_free(dev: Psnd_midi_event_t); cdecl; external libasound;
procedure snd_midi_event_init(dev: Psnd_midi_event_t); cdecl; external libasound;
procedure snd_midi_event_reset_encode(dev: Psnd_midi_event_t); cdecl; external libasound;
procedure snd_midi_event_reset_decode(dev: Psnd_midi_event_t); cdecl; external libasound;
procedure snd_midi_event_no_status(dev: Psnd_midi_event_t; on: cint); cdecl; external libasound;
{ encode from byte stream - return number of written bytes if success }
function snd_midi_event_encode(dev: Psnd_midi_event_t; buf: Pointer; count: clong; ev: Psnd_seq_event_t): clong; cdecl; external libasound;
function snd_midi_event_encode_byte(dev: Psnd_midi_event_t; c: cint; ev: Psnd_seq_event_t): cint; cdecl; external libasound;
{ decode from event to bytes - return number of written bytes if success }
function snd_midi_event_decode(dev: Psnd_midi_event_t; buf: Pointer; count: clong; ev: Psnd_seq_event_t): clong; cdecl; external libasound;

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_SEQ_MIDI_EVENT_H */

