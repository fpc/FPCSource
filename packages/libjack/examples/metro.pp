{
    Copyright (C) 2002 Anthony Van Groningen
    
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

program metro;

uses
  CTypes, SysUtils, Jack;

type
  Psample_t = ^sample_t;
  sample_t = jack_default_audio_sample_t;

var
  client: Pjack_client_t;
  output_port: Pjack_port_t;
  sr: culong;
  freq: cint = 880;
  bpm: cint;
  tone_length, wave_length: jack_nframes_t;
  wave: Psample_t;
  offset: clong = 0;
  transport_aware: Boolean = False;
  transport_state: jack_transport_state_t;

procedure usage;
begin
  Writeln(StdErr);
  Writeln(StdErr, 'usage: jack_metro ');
  Writeln(StdErr, '              [ --frequency OR -f frequency (in Hz) ]');
  Writeln(StdErr, '              [ --amplitude OR -A maximum amplitude (between 0 and 1) ]');
  Writeln(StdErr, '              [ --duration OR -D duration (in ms) ]');
  Writeln(StdErr, '              [ --attack OR -a attack (in percent of duration) ]');
  Writeln(StdErr, '              [ --decay OR -d decay (in percent of duration) ]');
  Writeln(StdErr, '              [ --name OR -n jack name for metronome client ]');
  Writeln(StdErr, '              [ --transport OR -t transport aware ]');
  Writeln(StdErr, '              --bpm OR -b beats per minute');
end;

procedure process_silence (nframes: jack_nframes_t); cdecl;
var
  buffer: Psample_t;
begin
  buffer := jack_port_get_buffer (output_port, nframes);
  FillChar (buffer^, SizeOf (jack_default_audio_sample_t) * nframes, 0);
end;

procedure process_audio (nframes: jack_nframes_t); cdecl;
var
  buffer: Psample_t;
  frames_left: jack_nframes_t;
begin
  buffer := jack_port_get_buffer (output_port, nframes);
  frames_left := nframes;

  while (wave_length - offset) < frames_left do
  begin
    Move((wave + offset)^, (buffer + (nframes - frames_left))^, SizeOf (sample_t) * (wave_length - offset));
    frames_left -= wave_length - offset;
    offset := 0;
  end;
  if frames_left > 0 then
  begin
    Move((wave + offset)^, (buffer + (nframes - frames_left))^, SizeOf (sample_t) * frames_left);
    offset += frames_left;
  end;
end;

function process (nframes: jack_nframes_t; arg: Pointer): cint; cdecl;
var
  pos: jack_position_t;
begin
  if transport_aware then
  begin
    if (jack_transport_query (client, @pos) <> JackTransportRolling) then
    begin
      process_silence (nframes);
      Exit(0);
    end;
    offset := pos.frame mod wave_length;
  end;
  process_audio (nframes);
  Result := 0;
end;

function sample_rate_change: cint; cdecl;
begin
  Writeln('Sample rate has changed! Exiting...');
  Halt(-1);
end;

var
  scale: sample_t;
  i, attack_length, decay_length: cint;
  amp: Pcdouble;
  max_amp: cdouble = 0.5;
  option_index: cint = 0;
  got_bpm: Boolean = false;
  bpm_string: string = 'bpm';
  attack_percent: cint = 1;
  decay_percent: cint = 10;
  dur_arg: cint = 100;
  client_name: string = '';
  verbose: Boolean = False;
  status: jack_status_t;
  code: Integer;
begin
  while option_index < ParamCount do
  begin
    Inc(option_index);
    case ParamStr(option_index) of
      '-f', '--frequency':
        begin
          Inc(option_index);
          Val(ParamStr(option_index), freq, code);
          if code <> 0 then
          begin
            Writeln (StdErr, 'invalid frequency');
            Halt(-1);
          end;
        end;
      '-A', '--amplitude':
        begin
          Inc(option_index);
          Val(ParamStr(option_index), max_amp, code);
          if (code <> 0) or (max_amp <= 0) or (max_amp > 1) then
          begin
            Writeln (StdErr, 'invalid amplitude');
            Halt(-1);
          end;
        end;
      '-D', '--duration':
        begin
          Inc(option_index);
          dur_arg := StrToInt (ParamStr(option_index));
          Writeln (StdErr, 'durarg = ', dur_arg);
        end;
      '-a', '--attack':
        begin
          Inc(option_index);
          Val(ParamStr(option_index), attack_percent, code);
          if (code <> 0) or (attack_percent < 0) or (attack_percent > 100) then
          begin
            Writeln (StdErr, 'invalid attack percent');
            Halt(-1);
          end;
        end;
      '-d', '--decay':
        begin
          Inc(option_index);
          Val(ParamStr(option_index), decay_percent, code);
          if (code <> 0) or (decay_percent < 0) or (decay_percent > 100) then
          begin
            Writeln (StdErr, 'invalid decay percent');
            Halt(-1);
          end;
        end;
      '-b', '--bpm':
        begin
          got_bpm := True;
          Inc(option_index);
          Val(ParamStr(option_index), bpm, code);
          if code <> 0 then
          begin
            Writeln (StdErr, 'invalid bpm');
            Halt(-1);
          end;
          bpm_string := IntToStr(bpm) + '_bpm';
        end;
      '-n', '--name':
        begin
          Inc(option_index);
          client_name := ParamStr(option_index);
        end;
      '-v', '--verbose':
        verbose := True;
      '-t', '--transport':
        transport_aware := True;
      '-h', '--help':
        begin
    	  usage;
    	  Halt(-1);
        end;
      else
        begin
          Writeln (StdErr, 'unknown option ', ParamStr(option_index));
    	  usage;
    	  Halt(-1);
        end;
    end;
  end;
  if not got_bpm then
  begin
    Writeln (StdErr, 'bpm not specified');
    usage;
    Halt(-1);
  end;

  { Initial Jack setup, get sample rate }
  if client_name = '' then
    client_name := 'metro';
  client := jack_client_open (PChar(client_name), JackNoStartServer, @status);
  if client = nil then
  begin
    Writeln (StdErr, 'jack server not running?');
    Halt(1);
  end;
  jack_set_process_callback (client, @process, nil);
  output_port := jack_port_register (client, PChar(bpm_string), JACK_DEFAULT_AUDIO_TYPE, Ord(JackPortIsOutput), 0);

  sr := jack_get_sample_rate (client);

  { setup wave table parameters }
  wave_length := 60 * sr div bpm;
  tone_length := sr * dur_arg div 1000;
  attack_length := tone_length * attack_percent div 100;
  decay_length := tone_length * decay_percent div 100;
  scale := 2 * PI * freq / sr;

  if tone_length >= wave_length then
  begin
    Writeln (StdErr, 'invalid duration (tone length = ', tone_length,
             ', wave length = ', wave_length);
    Halt(-1);
  end;
  if (attack_length + decay_length) > cint(tone_length) then
  begin
    Writeln (StdErr, 'invalid attack/decay');
    Halt(-1);
  end;

  { Build the wave table }
  wave := GetMem (wave_length * SizeOf(sample_t));
  amp := GetMem (tone_length * SizeOf(cdouble));

  for i := 0 to attack_length - 1 do
    amp[i] := max_amp * i / (cdouble(attack_length));
  for i := attack_length to cint(tone_length) - decay_length - 1 do
    amp[i] := max_amp;
  for i := cint(tone_length) - decay_length to cint(tone_length) - 1 do
    amp[i] := - max_amp * (i - cdouble(tone_length)) / cdouble(decay_length);
  for i := 0 to cint(tone_length) - 1 do
    wave[i] := amp[i] * sin (scale * i);
  for i := tone_length to cint(wave_length) - 1 do
    wave[i] := 0;

  if jack_activate (client) <> 0 then
  begin
    Writeln (StdErr, 'cannot activate client');
    Halt(1);
  end;

  while True do
    sleep(1000);
end.

