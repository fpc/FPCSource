program showtime;

{$MODE objfpc}{$H+}

uses
  Jack, CTypes, SysUtils, BaseUnix;

var
  client: Pjack_client_t;

procedure showtime;
var
  current: jack_position_t;
  transport_state: jack_transport_state_t;
  frame_time: jack_nframes_t;
begin
  transport_state := jack_transport_query (client, @current);
  frame_time := jack_frame_time (client);

  Write('frame: ', current.frame:7, ' @ ', frame_time, #9);

  case transport_state of
    JackTransportStopped:
      Write ('state: Stopped');
    JackTransportRolling:
      Write ('state: Rolling');
    JackTransportStarting:
      Write ('state: Starting');
    else
      Write ('state: [unknown]');
  end;

  if (Ord(current.valid) and Ord(JackPositionBBT)) <> 0 then
    Write (#9'BBT: ', current.bar:3, '|', current.beat, '|', current.tick:4);

  if (Ord(current.valid) and Ord(JackPositionTimecode)) <> 0 then
    Write (#9'TC: (', current.frame_time:0:6, ', ', current.next_time:0:6, ')');

  if (Ord(current.valid) and Ord(JackBBTFrameOffset)) <> 0 then
    Write (#9'BBT offset: (', current.bbt_offset, ')');

  if (Ord(current.valid) and Ord(JackAudioVideoRatio)) <> 0 then
    Write (#9'audio/video: (', current.audio_frames_per_video_frame:0:12, ')');

  if (Ord(current.valid) and Ord(JackVideoFrameOffset)) <> 0 then
  begin
    if current.video_offset <> 0 then
      Write (#9' video@: (', current.video_offset, ')')
    else
      Write(#9' no video');
  end;
	
  Writeln;
end;

procedure jack_shutdown (arg: Pointer); cdecl;
begin
  Halt (1);
end;

procedure signal_handler (sig: cint); cdecl;
begin
  jack_client_close (client);
  Writeln (StdErr, 'signal received, exiting ...');
  Halt (0);
end;

begin
  { try to become a client of the JACK server }

  client := jack_client_open ('showtime', JackNullOption, nil);
  if client = nil then
  begin
    Writeln (StdErr, 'jack server not running?');
    Halt(1);
  end;

  fpsignal (SIGQUIT, @signal_handler);
  fpsignal (SIGTERM, @signal_handler);
  fpsignal (SIGHUP, @signal_handler);
  fpsignal (SIGINT, @signal_handler);

  { tell the JACK server to call `jack_shutdown()' if
    it ever shuts down, either entirely, or if it
    just decides to stop calling us.
  }

  jack_on_shutdown (client, @jack_shutdown, nil);

  { tell the JACK server that we are ready to roll }

  if jack_activate (client) <> 0 then
  begin
    Writeln (StdErr, 'cannot activate client');
    Halt(1);
  end;

  repeat
    sleep (1);
    showtime;
  until False;

  jack_client_close (client);
  Halt (0);
end.

