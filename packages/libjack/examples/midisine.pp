{
    Copyright (C) 2004 Ian Esten

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

program midisine;

{$MODE objfpc}{$H+}

uses
  CTypes, Math, SysUtils, Jack, JackMidiPort;

var
  input_port: Pjack_port_t;
  output_port: Pjack_port_t;
  ramp: jack_default_audio_sample_t = 0.0;
  note_on: jack_default_audio_sample_t;
  note: cuchar = 0;
  note_frqs: array [0..127] of jack_default_audio_sample_t;

procedure calc_note_frqs(srate: jack_default_audio_sample_t);
var
  i: cint;
begin
  for i := 0 to 127 do
    note_frqs[i] := (2.0 * 440.0 / 32.0) * Power(2, ((jack_default_audio_sample_t(i) - 9.0) / 12.0)) / srate;
end;

function process(nframes: jack_nframes_t; arg: Pointer): cint; cdecl;
var
  i: cint;
  port_buf: Pointer;
  _out: Pjack_default_audio_sample_t;
  in_event: jack_midi_event_t;
  event_index: jack_nframes_t;
  event_count: jack_nframes_t;
begin
  port_buf := jack_port_get_buffer(input_port, nframes);
  _out := jack_port_get_buffer (output_port, nframes);
  event_index := 0;
  event_count := jack_midi_get_event_count(port_buf);
  if event_count > 1 then
  begin
    Writeln(' midisine: have ', event_count, ' events');
    for i := 0 to event_count - 1 do
    begin
      jack_midi_event_get(@in_event, port_buf, i);
      Writeln('    event ', i, ' time is ', in_event.time, '. 1st byte is $', HexStr(in_event.buffer^, 2));
    end;
    {printf("1st byte of 1st event addr is %p\n", in_events[0].buffer);}
  end;
  jack_midi_event_get(@in_event, port_buf, 0);
  for i := 0 to nframes - 1 do
  begin
    if (in_event.time = i) and (event_index < event_count) then
    begin
      if (in_event.buffer^ and $f0) = $90 then
      begin
        { note on }
        note := (in_event.buffer + 1)^;
        note_on := 1.0;
      end
      else if (in_event.buffer^ and $f0) = $80 then
      begin
        { note off }
        note := (in_event.buffer + 1)^;
        note_on := 0.0;
      end;
      Inc(event_index);
      if event_index < event_count then
        jack_midi_event_get(@in_event, port_buf, event_index);
    end;
    ramp += note_frqs[note];
    if ramp > 1.0 then
      ramp -= 2.0;
    _out[i] := note_on*sin(2*PI*ramp);
  end;
  Result := 0;
end;

function srate(nframes: jack_nframes_t; arg: Pointer): cint; cdecl;
begin
  Writeln('the sample rate is now ', nframes, '/sec');
  calc_note_frqs(jack_default_audio_sample_t(nframes));
  Result := 0;
end;

procedure jack_shutdown(arg: Pointer); cdecl;
begin
  Halt(1);
end;

var
  client: Pjack_client_t;
begin
  client := jack_client_open ('midisine', JackNullOption, nil);
  if client = nil then
  begin
    Writeln(StdErr, 'jack server not running?');
    Halt(1);
  end;

  calc_note_frqs(jack_get_sample_rate (client));

  jack_set_process_callback (client, @process, nil);

  jack_set_sample_rate_callback (client, @srate, nil);

  jack_on_shutdown (client, @jack_shutdown, nil);

  input_port := jack_port_register (client, 'midi_in', JACK_DEFAULT_MIDI_TYPE, Ord(JackPortIsInput), 0);
  output_port := jack_port_register (client, 'audio_out', JACK_DEFAULT_AUDIO_TYPE, Ord(JackPortIsOutput), 0);

  if jack_activate (client) <> 0 then
  begin
    Writeln(StdErr, 'cannot activate client');
    Halt(1);
  end;

  { run until interrupted }
  while True do
    Sleep(1000);
  jack_client_close(client);
  Halt (0);
end.

