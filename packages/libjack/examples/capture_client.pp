{
    Copyright (C) 2001 Paul Davis
    Copyright (C) 2003 Jack O'Quin

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

    * 2002/08/23 - modify for libsndfile 1.0.0 <andy@alsaplayer.org>
    * 2003/05/26 - use ringbuffers - joq

}

program capture_client;

{$MODE ObjFpc}{$H+}

uses
{$ifdef UNIX}
  CThreads,
{$endif}
  PThreads, CTypes, SysUtils, SndFile, Jack, JackRingBuffer;

const
  EIO = 5;
  EPIPE = 32;

type
  Pjack_thread_info_t = ^jack_thread_info_t;
  jack_thread_info_t = record
    thread_id: TThreadId;
    sf: PSNDFILE;
    duration: jack_nframes_t;
    rb_size: jack_nframes_t;
    client: Pjack_client_t;
    channels: cuint;
    bitdepth: cint;
    path: string;
    can_capture: Boolean; {volatile;}
    can_process: Boolean; {volatile;}
    status: cint; {volatile;}
  end;

var
  { JACK data }
  nports: cuint;
  ports: PPjack_port_t;
  _in: PPjack_default_audio_sample_t;
  nframes: jack_nframes_t;

const
  sample_size = SizeOf(jack_default_audio_sample_t);

{ Synchronization between process thread and disk thread. }
  DEFAULT_RB_SIZE = 16384;              { ringbuffer size in frames }

var
  rb: Pjack_ringbuffer_t;
  disk_thread_lock: TPthreadMutex;
  data_ready: TCondVar;
  overruns: clong = 0;
  total_captured: jack_nframes_t = 0;

function disk_thread (arg: Pointer): PtrInt;
label
  done;
var
  info: Pjack_thread_info_t;
  samples_per_frame: jack_nframes_t;
  bytes_per_frame: csize_t;
  framebuf: Pointer;
  errstr: array [0..255] of Char;
begin
  info := arg;
  samples_per_frame := info^.channels;
  bytes_per_frame := samples_per_frame * sample_size;
  framebuf := GetMem (bytes_per_frame);

  pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, nil);
  pthread_mutex_lock (@disk_thread_lock);


  info^.status := 0;

  while True do
  begin
    { Write the data one frame at a time.  This is
      inefficient, but makes things simpler. }
    while info^.can_capture and
         (jack_ringbuffer_read_space (rb) >= bytes_per_frame) do
    begin
      jack_ringbuffer_read (rb, framebuf, bytes_per_frame);

      if sf_writef_float (info^.sf, framebuf, 1) <> 1 then
      begin
	      sf_error_str (nil, @errstr, sizeof (errstr) - 1);
	      Writeln (StdErr,
		       'cannot write sndfile (', errstr, ')');
	      info^.status := EIO; { write failed }
	      goto done;
      end;

      Inc(total_captured);
      if total_captured >= info^.duration then
      begin
	Writeln ('disk thread finished');
	goto done;
      end;
    end;

    { wait until process() signals more data }
    pthread_cond_wait (@data_ready, @disk_thread_lock);
  end;

done:
  pthread_mutex_unlock (@disk_thread_lock);
  FreeMem (framebuf);
  Result := 0;
end;

function process (nframes: jack_nframes_t; arg: Pointer): cint; cdecl;
var
  chn: cint;
  i: csize_t;
  info: Pjack_thread_info_t;
begin
  info := arg;

  { Do nothing until we're ready to begin. }
  if (not info^.can_process) or (not info^.can_capture) then
    Exit(0);

  for chn := 0 to nports - 1 do
    _in[chn] := jack_port_get_buffer (ports[chn], nframes);

  { Sndfile requires interleaved data.  It is simpler here to
    just queue interleaved samples to a single ringbuffer. }
  for i := 0 to nframes - 1 do
    for chn := 0 to nports - 1 do
      if jack_ringbuffer_write (rb, Pointer(_in[chn]+i), sample_size) < sample_size then
        Inc(overruns);

  { Tell the disk thread there is work to do.  If it is already
    running, the lock will not be available.  We can't wait
    here in the process() thread, but we don't need to signal
    in that case, because the disk thread will read all the
    data queued before waiting again. }
  if pthread_mutex_trylock (@disk_thread_lock) = 0 then
  begin
    pthread_cond_signal (@data_ready);
    pthread_mutex_unlock (@disk_thread_lock);
  end;

  Result := 0;
end;

procedure jack_shutdown (arg: Pointer); cdecl;
begin
  Writeln (StdErr, 'JACK shutdown');
  // Halt (0);
  //abort();
end;

procedure setup_disk_thread (info: Pjack_thread_info_t);
var
  sf_info: TSF_INFO;
  short_mask: cint;
  errstr: array [0..255] of Char;
begin
  sf_info.samplerate := jack_get_sample_rate (info^.client);
  sf_info.channels := info^.channels;

  case info^.bitdepth of
    8:
      short_mask := SF_FORMAT_PCM_U8;
    16:
      short_mask := SF_FORMAT_PCM_16;
    24:
      short_mask := SF_FORMAT_PCM_24;
    32:
      short_mask := SF_FORMAT_PCM_32;
    else
      short_mask := SF_FORMAT_PCM_16;
  end;
  sf_info.format := SF_FORMAT_WAV or short_mask;

  info^.sf := sf_open (PChar(info^.path), SFM_WRITE, @sf_info);
  if info^.sf = nil then
  begin
    sf_error_str (nil, @errstr, sizeof (errstr) - 1);
    Writeln (StdErr, 'cannot open sndfile ', info^.path, ' for output (', errstr, ')');
    jack_client_close (info^.client);
    Halt (1);
  end;

  if info^.duration = 0 then
    info^.duration := JACK_MAX_FRAMES
  else
    info^.duration *= sf_info.samplerate;

  info^.can_capture := False;

  info^.thread_id := BeginThread(@disk_thread, info);
end;

procedure run_disk_thread (info: Pjack_thread_info_t);
begin
  info^.can_capture := True;
  WaitForThreadTerminate(info^.thread_id, -1);
  sf_close (info^.sf);
  if overruns > 0 then
  begin
    Writeln (StdErr,
	     'jackrec failed with ', overruns, ' overruns.');
    Writeln (StdErr, ' try a bigger buffer than -B ', info^.rb_size, '.');
    info^.status := EPIPE;
  end;
end;

procedure setup_ports (sources: cint; first_source_param: Integer; info: Pjack_thread_info_t);
var
  i: cuint;
  in_size: csize_t;
  name, source_name: string;
begin
  { Allocate data structures that depend on the number of ports. }
  nports := sources;
  ports := GetMem (SizeOf (Pjack_port_t) * nports);
  in_size := nports * SizeOf (Pjack_default_audio_sample_t);
  _in := GetMem (in_size);
  rb := jack_ringbuffer_create (nports * sample_size * info^.rb_size);

  { When JACK is running realtime, jack_activate() will have
    called mlockall() to lock our pages into memory.  But, we
    still need to touch any newly allocated pages before
    process() starts using them.  Otherwise, a page fault could
    create a delay that would force JACK to shut us down. }
  FillChar(_in^, in_size, 0);
  FillChar(rb^.buf^, rb^.size, 0);

  for i := 0 to nports - 1 do
  begin
    WriteStr (name, 'input', i+1);

    ports[i] := jack_port_register (info^.client, PChar(name), JACK_DEFAULT_AUDIO_TYPE, Ord(JackPortIsInput), 0);
    if ports[i] = nil then
    begin
      Writeln (StdErr, 'cannot register input port "', name, '"!');
      jack_client_close (info^.client);
      Halt (1);
    end;
  end;

  for i := 0 to nports - 1 do
  begin
    source_name := ParamStr(first_source_param + i);
    if jack_connect (info^.client, PChar(source_name), jack_port_name (ports[i])) <> 0 then
    begin
      Writeln (StdErr, 'cannot connect input port ', jack_port_name (ports[i]), ' to ', source_name);
      jack_client_close (info^.client);
      Halt (1);
    end;
  end;

  info^.can_process := True;            { process() can start, now }
end;

var
  client: Pjack_client_t;
  thread_info: jack_thread_info_t;
  longopt_index: cint = 0;
  show_usage: Boolean = False;
begin
  pthread_mutex_init(@disk_thread_lock, nil);
  pthread_cond_init(@data_ready, nil);
  FillChar (thread_info, SizeOf (thread_info), 0);
  thread_info.rb_size := DEFAULT_RB_SIZE;

  while longopt_index < ParamCount do
  begin
    Inc(longopt_index);
    case ParamStr(longopt_index) of
      '-h', '--help':
        show_usage := True;
      '-d', '--duration':
        begin
          Inc(longopt_index);
          if longopt_index > ParamCount then
          begin
  	    Writeln (StdErr, 'error');
  	    show_usage := True;
            break;
          end;
	  thread_info.duration := StrToInt (ParamStr(longopt_index));
        end;
      '-f', '--file':
        begin
          Inc(longopt_index);
          if longopt_index > ParamCount then
          begin
  	    Writeln (StdErr, 'error');
  	    show_usage := True;
            break;
          end;
	  thread_info.path := ParamStr(longopt_index);
        end;
      '-b', '--bitdepth':
        begin
          Inc(longopt_index);
          if longopt_index > ParamCount then
          begin
  	    Writeln (StdErr, 'error');
  	    show_usage := True;
            break;
          end;
	  thread_info.bitdepth := StrToInt (ParamStr(longopt_index));
        end;
      '-B', '--bufsize':
        begin
          Inc(longopt_index);
          if longopt_index > ParamCount then
          begin
  	    Writeln (StdErr, 'error');
  	    show_usage := True;
            break;
          end;
	  thread_info.rb_size := StrToInt (ParamStr(longopt_index));
        end;
      else
        begin
          if (Length(ParamStr(longopt_index)) >= 1) and (ParamStr(longopt_index)[1] = '-') then
          begin
	    Writeln (StdErr, 'error');
	    show_usage := True;
            break;
          end
          else
          begin
            Dec(longopt_index);
            break;
          end;
        end;
    end;
  end;

  if show_usage or (thread_info.path = '') or (longopt_index >= ParamCount) then
  begin
    Writeln (StdErr, 'usage: jackrec -f filename [ -d second ] [ -b bitdepth ] [ -B bufsize ] port1 [ port2 ... ]');
    Halt (1);
  end;

  client := jack_client_open ('jackrec', JackNullOption, nil);
  if client = nil then
  begin
    Writeln (StdErr, 'jack server not running?');
    Halt (1);
  end;

  thread_info.client := client;
  thread_info.channels := ParamCount - longopt_index;
  thread_info.can_process := False;

  setup_disk_thread (@thread_info);

  jack_set_process_callback (client, @process, @thread_info);
  jack_on_shutdown (client, @jack_shutdown, @thread_info);

  if jack_activate (client) <> 0 then
  begin
    Writeln (StdErr, 'cannot activate client');
  end;

  setup_ports (ParamCount - longopt_index, longopt_index + 1, @thread_info);

  run_disk_thread (@thread_info);

  jack_client_close (client);

  jack_ringbuffer_free (rb);

  pthread_cond_destroy(@data_ready);
  pthread_mutex_destroy(@disk_thread_lock);
end.
