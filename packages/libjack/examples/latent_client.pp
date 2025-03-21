{
  This simple client demonstrates the most basic features of JACK
  as they would be used by many applications.
}

program latent_client;

{$MODE objfpc}{$H+}

uses
  Jack, CTypes, SysUtils;

var
  input_port: Pjack_port_t;
  output_port: Pjack_port_t;
  client: Pjack_client_t;

  delay_line: Pjack_default_audio_sample_t;
  delay_index: jack_nframes_t;
  latency: jack_nframes_t = 1024;

{
  The process callback for this JACK application is called in a
  special realtime thread once for each audio cycle.

  This client does nothing more than copy data from its input
  port to its output port. It will exit when stopped by
  the user (e.g. using Ctrl-C on a unix-ish operating system)
}
function process (nframes: jack_nframes_t; arg: Pointer): cint; cdecl;
var
  _in: Pjack_default_audio_sample_t;
  _out: Pjack_default_audio_sample_t;
  k: cint;
begin
  _in := jack_port_get_buffer (input_port, nframes);
  _out := jack_port_get_buffer (output_port, nframes);

  for k := 0 to nframes - 1 do
  begin
    _out[k] := delay_line[delay_index];
    delay_line[delay_index] := _in[k];
    delay_index := (delay_index + 1) mod latency;
  end;

  Result := 0;
end;

procedure latency_cb (mode: jack_latency_callback_mode_t; arg: Pointer); cdecl;
var
  range: jack_latency_range_t;
begin
  if mode = JackCaptureLatency then
  begin
    jack_port_get_latency_range (input_port, mode, @range);
    Inc(range.min, latency);
    Inc(range.max, latency);
    jack_port_set_latency_range (output_port, mode, @range);
  end
  else
  begin
    jack_port_get_latency_range (output_port, mode, @range);
    Inc(range.min, latency);
    Inc(range.max, latency);
    jack_port_set_latency_range (input_port, mode, @range);
  end;
end;

{
  JACK calls this shutdown_callback if the server ever shuts down or
  decides to disconnect the client.
}
procedure jack_shutdown (arg: Pointer); cdecl;
begin
  Writeln(StdErr, 'JACK shut down, exiting ...');
  Halt (1);
end;

var
  ports: PPChar;
  client_name: PChar = 'latent';
  server_name: PChar = nil;
  options: jack_options_t = JackNullOption;
  status: jack_status_t;
begin
  if ParamCount = 1 then
    latency := StrToInt(ParamStr(1));

  delay_line := GetMem( latency * SizeOf(jack_default_audio_sample_t));
  if delay_line = nil then
  begin
    Writeln (StdErr, 'no memory');
    Halt(1);
  end;

  FillChar (delay_line^, 0, latency * SizeOf(jack_default_audio_sample_t));

  { open a client connection to the JACK server }

  client := jack_client_open (client_name, options, @status, server_name);
  if client = nil then
  begin
    Writeln(StdErr, 'jack_client_open() failed, ',
            'status = $', HexStr(Ord(status), 4));
    if (Ord(status) and Ord(JackServerFailed)) <> 0  then
    begin
      Writeln(StdErr, 'Unable to connect to JACK server');
    end;
    Halt (1);
  end;
  if (Ord(status) and Ord(JackServerStarted)) <> 0 then
  begin
    Writeln (StdErr, 'JACK server started');
  end;
  if (Ord(status) and Ord(JackNameNotUnique)) <> 0 then
  begin
    client_name := jack_get_client_name(client);
    Writeln (StdErr, 'unique name `', client_name, ''' assigned');
  end;

  { tell the JACK server to call `process()' whenever
    there is work to be done.
  }

  jack_set_process_callback (client, @process, nil);

  { tell the JACK server to call `latency()' whenever
    the latency needs to be recalculated.
  }
  if Assigned(@jack_set_latency_callback) then
    jack_set_latency_callback (client, @latency_cb, nil);

  { tell the JACK server to call `jack_shutdown()' if
    it ever shuts down, either entirely, or if it
    just decides to stop calling us.
  }

  jack_on_shutdown (client, @jack_shutdown, nil);

  { display the current sample rate.
  }

  Writeln ('engine sample rate: ',
           jack_get_sample_rate (client));

  { create two ports }

  input_port := jack_port_register (client, 'input',
                                    JACK_DEFAULT_AUDIO_TYPE,
                                    Ord(JackPortIsInput), 0);
  output_port := jack_port_register (client, 'output',
                                     JACK_DEFAULT_AUDIO_TYPE,
                                     Ord(JackPortIsOutput), 0);

  if (input_port = nil) or (output_port = nil) then
  begin
    Writeln(StdErr, 'no more JACK ports available');
    Halt (1);
  end;

  { Tell the JACK server that we are ready to roll.  Our
    process() callback will start running now. }

  if jack_activate (client) <> 0 then
  begin
    Writeln (StdErr, 'cannot activate client');
    Halt (1);
  end;

  { Connect the ports.  You can't do this before the client is
    activated, because we can't make connections to clients
    that aren't running.  Note the confusing (but necessary)
    orientation of the driver backend ports: playback ports are
    "input" to the backend, and capture ports are "output" from
    it.
  }

  ports := jack_get_ports (client, nil, nil,
                           Ord(JackPortIsPhysical) or Ord(JackPortIsOutput));
  if ports = nil then
  begin
    Writeln(StdErr, 'no physical capture ports');
    Halt (1);
  end;

  if jack_connect (client, ports[0], jack_port_name (input_port)) <> 0 then
  begin
    Writeln (StdErr, 'cannot connect input ports');
  end;

  jack_free (ports);

  ports := jack_get_ports (client, nil, nil,
                           Ord(JackPortIsPhysical) or Ord(JackPortIsInput));
  if ports = nil then
  begin
    Writeln(StdErr, 'no physical playback ports');
    Halt (1);
  end;

  if jack_connect (client, jack_port_name (output_port), ports[0]) <> 0 then
  begin
    Writeln (StdErr, 'cannot connect output ports');
  end;

  jack_free (ports);

  { keep running until stopped by the user }

  repeat
    sleep (High(Cardinal));
  until False;

  { this is never reached but if the program
    had some other way to exit besides being killed,
    they would be important to call.
  }

  jack_client_close (client);
  Halt (0);
end.
