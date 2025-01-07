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

program midiseq;

{$MODE objfpc}{$H+}

uses
  CTypes, SysUtils, BaseUnix, Jack, JackMidiPort;

var
  client: Pjack_client_t;
  output_port: Pjack_port_t;

  note_frqs: Pcuchar;
  note_starts: Pjack_nframes_t;
  note_lengths: Pjack_nframes_t;
  num_notes: jack_nframes_t;
  loop_nsamp: jack_nframes_t;
  loop_index: jack_nframes_t;

procedure signal_handler(sig: cint); cdecl;
begin
  jack_client_close(client);
  Writeln(StdErr, 'signal received, exiting ...');
  Halt(0);
end;

procedure usage;
begin
  WriteLn(StdErr, 'usage: jack_midiseq name nsamp [startindex note nsamp] ...... [startindex note nsamp]');
  WriteLn(StdErr, 'eg: jack_midiseq Sequencer 24000 0 60 8000 12000 63 8000');
  WriteLn(StdErr, 'will play a 1/2 sec loop (if srate is 48khz) with a c4 note at the start of the loop');
  WriteLn(StdErr, 'that lasts for 12000 samples, then a d4# that starts at 1/4 sec that lasts for 800 samples');
end;

function process(nframes: jack_nframes_t; arg: Pointer): cint; cdecl;
var
  i, j: cint;
  port_buf: Pointer;
  buffer: Pcuchar;
begin
  port_buf := jack_port_get_buffer(output_port, nframes);
  jack_midi_clear_buffer(port_buf);
  {FillChar(buffer^, nframes*SizeOf(jack_default_audio_sample_t), 0);}

  for i := 0 to nframes - 1 do
  begin
    for j := 0 to num_notes - 1 do
    begin
      if note_starts[j] = loop_index then
      begin
	buffer := jack_midi_event_reserve(port_buf, i, 3);
	{printf("wrote a note on, port buffer = 0x%x, event buffer = 0x%x\n", port_buf, buffer);}
	buffer[2] := 64;        { velocity }
	buffer[1] := note_frqs[j];
	buffer[0] := $90;       { note on }
      end
      else if (note_starts[j] + note_lengths[j]) = loop_index then
      begin
	buffer := jack_midi_event_reserve(port_buf, i, 3);
	{printf("wrote a note off, port buffer = 0x%x, event buffer = 0x%x\n", port_buf, buffer);}
	buffer[2] := 64;        { velocity }
	buffer[1] := note_frqs[j];
	buffer[0] := $80;       { note off }
      end;
    end;
    if loop_index + 1 >= loop_nsamp then
      loop_index := 0
    else
      loop_index := loop_index + 1;
  end;
  Result := 0;
end;

var
  i: cint;
  nframes: jack_nframes_t;
  client_name: string;
begin
  if (ParamCount<5) or ((ParamCount-2) mod 3 <> 0) then
  begin
    usage;
    Halt(1);
  end;
  client_name := ParamStr(1);
  client := jack_client_open (PChar(client_name), JackNullOption, nil);
  if client = nil then
  begin
    Writeln (StdErr, 'JACK server not running?');
    Halt(1);
  end;
  jack_set_process_callback (client, @process, nil);
  output_port := jack_port_register (client, 'out', JACK_DEFAULT_MIDI_TYPE, Ord(JackPortIsOutput), 0);
  nframes := jack_get_buffer_size(client);
  loop_index := 0;
  num_notes := (ParamCount - 2) div 3;
  note_frqs := GetMem(num_notes*SizeOf(cuchar));
  note_starts := GetMem(num_notes*SizeOf(jack_nframes_t));
  note_lengths := GetMem(num_notes*SizeOf(jack_nframes_t));
  loop_nsamp := StrToInt(ParamStr(2));
  for i := 0 to num_notes - 1 do
  begin
    note_starts[i] := StrToInt(ParamStr(3 + 3*i));
    note_frqs[i] := StrToInt(ParamStr(4 + 3*i));
    note_lengths[i] := StrToInt(ParamStr(5 + 3*i));
  end;

  if jack_activate(client) <> 0 then
  begin
    Writeln (StdErr, 'cannot activate client');
    Halt(1);
  end;

  { install a signal handler to properly quits jack client }
{$ifndef WIN32}
  fpsignal(SIGQUIT, @signal_handler);
  fpsignal(SIGHUP, @signal_handler);
{$endif}
  fpsignal(SIGTERM, @signal_handler);
  fpsignal(SIGINT, @signal_handler);

  { run until interrupted }
  repeat
    Sleep(1000);
  until False;

  jack_client_close(client);
  Halt (0);
end.
