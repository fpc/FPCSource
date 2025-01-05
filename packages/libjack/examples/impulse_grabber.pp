{
     Copyright (C) 2001 Steve Harris

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

program impulse_grabber;

{$MODE objfpc}{$H+}

uses
  Jack, CTypes, SysUtils;

var
  input_port: Pjack_port_t;
  output_port: Pjack_port_t;

  impulse_sent: Boolean = False;
  response: Pcfloat;
  response_duration: culong;
  response_pos: culong;
  grab_finished: Boolean = False;

function process (nframes: jack_nframes_t; arg: Pointer): cint; cdecl;
var
  _in, _out: Pjack_default_audio_sample_t;
  i: cuint;
begin
  _out := Pjack_default_audio_sample_t(jack_port_get_buffer (output_port, nframes));
  _in := Pjack_default_audio_sample_t(jack_port_get_buffer (input_port, nframes));

  if grab_finished then
    Result := 0
  else if impulse_sent then
  begin
    i := 0;
    while (i < nframes) and (response_pos < response_duration) do
    begin
      response[response_pos] := _in[i];
      Inc(response_pos);
      Inc(i);
    end;
    if response_pos >= response_duration then
      grab_finished := True;
    for i := 0 to nframes - 1 do
      _out[i] := 0;
  end
  else
  begin
    _out[0] := 1;
    for i := 1 to nframes - 1 do
      _out[i] := 0;
    impulse_sent := True;
  end;

  Result := 0;
end;

procedure jack_shutdown (arg: Pointer); cdecl;
begin
  Halt (1);
end;

var
  client: Pjack_client_t;
  ports: PPChar;
  fs: cfloat;  // The sample rate
  peak: cfloat;
  peak_sample: culong;
  i: cuint;
  duration: cfloat = 0.0;
  c_format: Boolean = False;
  longopt_index: Integer = 0;
  show_usage: Boolean = False;
  tmpS: string;
begin
  while longopt_index < ParamCount do
  begin
    Inc(longopt_index);
    case ParamStr(longopt_index) of
      '-h', '--help':
        begin
          show_usage := True;
          break;
        end;
      '-d', '--duration':
        begin
          Inc(longopt_index);
          if longopt_index > ParamCount then
          begin
            show_usage := True;
            break;
          end;
          duration := StrToFloat(ParamStr(longopt_index));
        end;
      '-f', '--format':
        begin
          Inc(longopt_index);
          if longopt_index > ParamCount then
          begin
            show_usage := True;
            break;
          end;
          if UpCase(ParamStr(longopt_index)) = 'C' then
            c_format := True;
        end;
      else
        begin
          show_usage := True;
          break;
        end;
    end;
  end;
  if show_usage or (duration <= 0) then
  begin
    Writeln(StdErr, 'usage: jack_impulse_grab -d duration [-f (C|gnuplot)]');
    Halt(1);
  end;

  { try to become a client of the JACK server }

  client := jack_client_open('impulse_grabber', JackNullOption, nil);
  if client = nil then
  begin
    Writeln (StdErr, 'jack server not running?');
    Halt(1);
  end;

  { tell the JACK server to call `process()' whenever
    there is work to be done.
  }

  jack_set_process_callback (client, @process, nil);

  { tell the JACK server to call `jack_shutdown()' if
    it ever shuts down, either entirely, or if it
    just decides to stop calling us.
  }

  jack_on_shutdown (client, @jack_shutdown, nil);

  { display the current sample rate. once the client is activated
    (see below), you should rely on your own sample rate
    callback (see above) for this value.
  }

  fs := jack_get_sample_rate(client);
  response_duration := Trunc(fs * duration);
  response := GetMem(response_duration * SizeOf(cfloat));
  Writeln(StdErr,
          'Grabbing ', duration:0:12, ' seconds (', response_duration, ' samples) of impulse response');

  { create two ports }

  input_port := jack_port_register (client, 'input', JACK_DEFAULT_AUDIO_TYPE, Ord(JackPortIsInput), 0);
  output_port := jack_port_register (client, 'output', JACK_DEFAULT_AUDIO_TYPE, Ord(JackPortIsOutput), 0);

  { tell the JACK server that we are ready to roll }

  if jack_activate (client) <> 0 then
  begin
    Writeln (StdErr, 'cannot activate client');
    Halt(1);
  end;

  { connect the ports. Note: you can't do this before
    the client is activated (this may change in the future).
  }

  ports := jack_get_ports (client, nil, nil, Ord(JackPortIsPhysical) or Ord(JackPortIsOutput));
  if ports = nil then
  begin
    Writeln(StdErr, 'Cannot find any physical capture ports');
    Halt(1);
  end;

  if jack_connect (client, ports[0], jack_port_name (input_port)) <> 0 then
  begin
    Writeln (StdErr, 'cannot connect input ports');
  end;

  jack_free (ports);

  ports := jack_get_ports (client, nil, nil, Ord(JackPortIsPhysical) or Ord(JackPortIsInput));
  if ports = nil then
  begin
    Writeln(StdErr, 'Cannot find any physical playback ports');
    Halt(1);
  end;

  if jack_connect (client, jack_port_name (output_port), ports[0]) <> 0 then
  begin
    Writeln (StdErr, 'cannot connect output ports');
  end;

  jack_free (ports);

  { Wait for grab to finish }
  while not grab_finished do
  begin
    sleep (1000);
  end;
  jack_client_close (client);

  peak := response[0];
  peak_sample := 0;
  if c_format then
  begin
    Write('impulse[', response_duration, '] = {');
    for i := 0 to response_duration - 1 do
    begin
      if (i mod 4) <> 0 then
        Write(' ')
      else
      begin
        Writeln;
        Write(#9);
      end;
      WriteStr(tmpS, response[i]:0:10);
      if Pos('-', tmpS) = 0 then
        tmpS := '+' + tmpS;
      Write('"', tmpS, '"');
      if i < (response_duration - 1) then
        write(',');
      if Abs(response[i]) > peak then
      begin
        peak := Abs(response[i]);
        peak_sample := i;
      end;
    end;
    Writeln;
    Writeln('};');
  end
  else
  begin
    for i :=0 to response_duration - 1 do
    begin
      Writeln(response[i]:0:12);
      if Abs(response[i]) > peak then
      begin
        peak := Abs(response[i]);
        peak_sample := i;
      end;
    end;
  end;
  Writeln(StdErr, 'Peak value was ', peak:0:12, ' at sample ', peak_sample);

  Halt (0);
end.

