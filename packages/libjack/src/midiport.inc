(*
    Copyright (C) 2004 Ian Esten

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*)


{$ifndef __JACK_MIDIPORT_H}
{$define __JACK_MIDIPORT_H}

//#ifdef __cplusplus
//extern "C" {
//#endif

//#include <jack/weakmacros.h>
{$I weakmacros.inc}
//#include <jack/types.h>
//#include <stdlib.h>


(** Type for raw event data contained in @ref jack_midi_event_t. *)
type
  PPjack_midi_data_t = ^Pjack_midi_data_t;
  Pjack_midi_data_t = ^jack_midi_data_t;
  jack_midi_data_t = cuchar;


(** A Jack MIDI event. *)
  PPjack_midi_event_t = ^Pjack_midi_event_t;
  Pjack_midi_event_t = ^jack_midi_event_t;
  jack_midi_event_t = record
    time: jack_nframes_t;   (**< Sample index at which event is valid *)
    size: csize_t;   (**< Number of bytes of data in \a buffer *)
    buffer: Pjack_midi_data_t; (**< Raw MIDI data *)
  end;


(**
 * @defgroup MIDIAPI Reading and writing MIDI data
 * @{
 *)

(** Get number of events in a port buffer.
 *
 * @param port_buffer Port buffer from which to retrieve event.
 * @return number of events inside @a port_buffer
 *)
function
jack_midi_get_event_count(port_buffer: Pointer): uint32_t; cdecl; JACK_OPTIONAL_WEAK_EXPORT;


(** Get a MIDI event from an event port buffer.
 *
 * Jack MIDI is normalised, the MIDI event returned by this function is
 * guaranteed to be a complete MIDI event (the status byte will always be
 * present, and no realtime events will interspered with the event).
 *
 * This rule does not apply to System Exclusive MIDI messages
 * since they can be of arbitrary length.
 * To maintain smooth realtime operation such events CAN be deliverd
 * as multiple, non-normalised events.
 * The maximum size of one event "chunk" depends on the MIDI backend in use.
 * For example the midiseq driver will create chunks of 256 bytes.
 * The first SysEx "chunked" event starts with 0xF0 and the last
 * delivered chunk ends with 0xF7.
 * To receive the full SysEx message, a caller of jack_midi_event_get()
 * must concatenate chunks until a chunk ends with 0xF7.
 *
 * @param event Event structure to store retrieved event in.
 * @param port_buffer Port buffer from which to retrieve event.
 * @param event_index Index of event to retrieve.
 * @return 0 on success, ENODATA if buffer is empty.
 *)
function
jack_midi_event_get(event: Pjack_midi_event_t;
                    port_buffer: Pointer;
                    event_index: uint32_t): cint; cdecl; JACK_OPTIONAL_WEAK_EXPORT;


(** Clear an event buffer.
 *
 * This should be called at the beginning of each process cycle before calling
 * @ref jack_midi_event_reserve or @ref jack_midi_event_write. This
 * function may not be called on an input port's buffer.
 *
 * @param port_buffer Port buffer to clear (must be an output port buffer).
 *)
procedure
jack_midi_clear_buffer(port_buffer: Pointer); cdecl; JACK_OPTIONAL_WEAK_EXPORT;

(** Reset an event buffer (from data allocated outside of JACK).
 *
 * This should be called at the beginning of each process cycle before calling
 * @ref jack_midi_event_reserve or @ref jack_midi_event_write. This
 * function may not be called on an input port's buffer.
 *
 * @deprecated Please use jack_midi_clear_buffer().
 *
 * @param port_buffer Port buffer to reset.
 *)
procedure
jack_midi_reset_buffer(port_buffer: Pointer); cdecl; JACK_OPTIONAL_WEAK_DEPRECATED_EXPORT;


(** Get the size of the largest event that can be stored by the port.
 *
 * This function returns the current space available, taking into account
 * events already stored in the port.
 *
 * @param port_buffer Port buffer to check size of.
 *)
function
jack_midi_max_event_size(port_buffer: Pointer): csize_t; cdecl; JACK_OPTIONAL_WEAK_EXPORT;


(** Allocate space for an event to be written to an event port buffer.
 *
 * Clients are to write the actual event data to be written starting at the
 * pointer returned by this function. Clients must not write more than
 * @a data_size bytes into this buffer.  Clients must write normalised
 * MIDI data to the port - no running status and no (1-byte) realtime
 * messages interspersed with other messages (realtime messages are fine
 * when they occur on their own, like other messages).
 *
 * Events must be written in order, sorted by their sample offsets.
 * JACK will not sort the events for you, and will refuse to store
 * out-of-order events.
 *
 * @param port_buffer Buffer to write event to.
 * @param time Sample offset of event.
 * @param data_size Length of event's raw data in bytes.
 * @return Pointer to the beginning of the reserved event's data buffer, or
 * NULL on error (ie not enough space).
 *)
function
jack_midi_event_reserve(port_buffer: Pointer;
                        time: jack_nframes_t;
                        data_size: csize_t): Pjack_midi_data_t; cdecl; JACK_OPTIONAL_WEAK_EXPORT;


(** Write an event into an event port buffer.
 *
 * This function is simply a wrapper for @ref jack_midi_event_reserve
 * which writes the event data into the space reserved in the buffer.
 *
 * Clients must not write more than
 * @a data_size bytes into this buffer.  Clients must write normalised
 * MIDI data to the port - no running status and no (1-byte) realtime
 * messages interspersed with other messages (realtime messages are fine
 * when they occur on their own, like other messages).
 *
 * Events must be written in order, sorted by their sample offsets.
 * JACK will not sort the events for you, and will refuse to store
 * out-of-order events.
 *
 * @param port_buffer Buffer to write event to.
 * @param time Sample offset of event.
 * @param data Message data to be written.
 * @param data_size Length of @a data in bytes.
 * @return 0 on success, ENOBUFS if there's not enough space in buffer for event.
 *)
function
jack_midi_event_write(port_buffer: Pointer;
                      time: jack_nframes_t;
                      const data: Pjack_midi_data_t;
                      data_size: csize_t): cint; cdecl; JACK_OPTIONAL_WEAK_EXPORT;


(** Get the number of events that could not be written to @a port_buffer.
 *
 * This function returning a non-zero value implies @a port_buffer is full.
 * Currently the only way this can happen is if events are lost on port mixdown.
 *
 * @param port_buffer Port to receive count for.
 * @returns Number of events that could not be written to @a port_buffer.
 *)
function
jack_midi_get_lost_event_count(port_buffer: Pointer): uint32_t; cdecl; JACK_OPTIONAL_WEAK_EXPORT;

///**@}*/

//#ifdef __cplusplus
//}
//#endif


{$endif __JACK_MIDIPORT_H}


