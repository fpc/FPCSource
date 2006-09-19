program test;

{$mode objfpc}

uses
  classes, sysutils, openal, mad;

const
  BLOCK_SIZE            = 4*1152;
  MAD_INPUT_BUFFER_SIZE = 5*8192;


// Note: if you lower the bufcnt or bufsize, then you have to modify the polltime also!
  bufcnt                = 4;
  bufsize               = 4*BLOCK_SIZE;
  polltime              = 100;

var
// openal
  device  : PALCdevice;
  context : PALCcontext;
  source  : ALuint;
  buffers : array[0..bufcnt-1] of ALuint;

// mad
  stream  : mad_stream;
  frame   : mad_frame;
  synth   : mad_synth;
  inbuf   : array[0..MAD_INPUT_BUFFER_SIZE-1] of Byte;

// others
  mp3data : TStream;

procedure mad_reset;
begin
  mad_stream_finish(stream);
  mad_stream_init(stream);
  mp3data.Position := 0;
end;

function mad_read(const Buffer: Pointer; const Count: Longword): Longword;
var
  X         : Integer;
  Num       : Longword;
  Ofs       : Longword;
  Remaining : Integer;
  ReadStart : Pointer;
  ReadSize  : Integer;
  Output    : PSmallint;
begin
  if Count mod BLOCK_SIZE <> 0 then
    Exit(0);

  Ofs := 0;
  Num := Count;
  while Num > 0 do
  begin
    if (stream.buffer = nil) or (stream.error = MAD_ERROR_BUFLEN) then
    begin
      if Assigned(stream.next_frame) then
      begin
        Remaining := PtrInt(stream.bufend) - PtrInt(stream.next_frame);
        Move(stream.next_frame^, inbuf, Remaining);
        ReadStart := Pointer(PtrInt(@inbuf) + Remaining);
        ReadSize  := MAD_INPUT_BUFFER_SIZE - Remaining;
      end else begin
        ReadSize  := MAD_INPUT_BUFFER_SIZE;
        ReadStart := @inbuf;
        Remaining := 0;
      end;

      ReadSize := mp3data.Read(ReadStart^, ReadSize);
      if ReadSize = 0 then
        Break;

      mad_stream_buffer(stream, @inbuf, ReadSize+Remaining);
      stream.error := MAD_ERROR_NONE;
    end;

    if mad_frame_decode(frame, stream) <> 0 then
    begin
      if MAD_RECOVERABLE(stream.error) or (stream.error = MAD_ERROR_BUFLEN) then
        Continue;

      Exit(0);
    end;

    mad_synth_frame(synth, frame);

    Output := Pointer(PtrUInt(Buffer) + Ofs);
    with synth do
    if pcm.channels = 2 then
    begin
      for X := 0 to pcm.length -1 do
      begin
         if pcm.samples[0][X] >= MAD_F_ONE then
           pcm.samples[0][X] := MAD_F_ONE - 1;
         if pcm.samples[0][X] < -MAD_F_ONE then
           pcm.samples[0][X] := -MAD_F_ONE;
         pcm.samples[0][X] := pcm.samples[0][X] shr (MAD_F_FRACBITS + 1 - 16);
         Output[X shl 1] := pcm.samples[0][X] div 2;

         if pcm.samples[1][X] >= MAD_F_ONE then
           pcm.samples[1][X] := MAD_F_ONE - 1;
         if pcm.samples[1][X] < -MAD_F_ONE then
           pcm.samples[1][X] := -MAD_F_ONE;
         pcm.samples[1][X] := pcm.samples[1][X] shr (MAD_F_FRACBITS + 1 - 16);
         Output[(X shl 1)+1] := pcm.samples[1][X] div 2;
      end;
    end else begin
      for X := 0 to pcm.length -1 do
      begin
         if pcm.samples[0][X] >= MAD_F_ONE then
           pcm.samples[0][X] := MAD_F_ONE - 1;
         if pcm.samples[0][X] < -MAD_F_ONE then
           pcm.samples[0][X] := -MAD_F_ONE;
         pcm.samples[0][X] := pcm.samples[0][X] shr (MAD_F_FRACBITS + 1 - 16);

         Output[X shl 1] := pcm.samples[0][X] div 2;
         Output[(X shl 1)+1] := pcm.samples[0][X] div 2;
      end;
    end;

    Ofs := Ofs + PtrUInt(4*synth.pcm.length);
    Num := Num - PtrUInt(4*synth.pcm.length);
  end;

  Result := Ofs;
end;

procedure alPlay;
var
  i: Integer;
  t: array[0..bufsize-1] of Byte;
begin
  WriteLn('Play');

  alSourceStop(source);
  alSourceRewind(source);
  alSourcei(source, AL_BUFFER, 0);

  for i := 0 to bufcnt - 1 do
  begin
    if mad_read(@t, bufsize) = 0 then
      Break;

    alBufferData(buffers[i], AL_FORMAT_STEREO16, @t, bufsize, 44100);
    alSourceQueueBuffers(source, 1, @buffers[i]);
  end;

  // Under windows, AL_LOOPING = AL_TRUE breaks queueing, no idea why
  alSourcei(source, AL_LOOPING, AL_FALSE);
  alSourcePlay(source);
end;

procedure alStop;
begin
  alSourceStop(source);
  alSourceRewind(source);
  alSourcei(source, AL_BUFFER, 0);
  WriteLn('Stop');
end;

function alProcess: Boolean;
var
  processed : ALint;
  buffer    : ALuint;
  t         : array[0..bufsize-1] of Byte;
begin
  alGetSourcei(source, AL_BUFFERS_PROCESSED, processed);
  while (processed > 0) and (processed <= bufcnt) do
  begin
    alSourceUnqueueBuffers(source, 1, @buffer);

    if mad_read(@t, bufsize) = 0 then
    begin
      alStop;
      Exit(False);
    end;
    
    alBufferData(buffer, AL_FORMAT_STEREO16, @t, bufsize, 44100);
    alSourceQueueBuffers(source, 1, @buffer);

    Dec(processed);
  end;

  Result := True;
end;

var
  Filename: String;
begin
// init openal
  device := alcOpenDevice(nil);
  context := alcCreateContext(device, nil);
  alcMakeContextCurrent(context);

  alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED);
  alGenSources(1, @source);
  alGenBuffers(bufcnt, @buffers);

// init mad
  mad_stream_init(stream);
  mad_frame_init(frame);
  mad_synth_init(synth);

// load file
  Write('Path to mp3 file: '); ReadLn(Filename);
  mp3data := TFileStream.Create(Filename, fmOpenRead);

// play
  alPlay;
  while alProcess do
    Sleep(polltime);

// close file
  mp3data.Free;

// finalize mad
  mad_synth_finish(synth);
  mad_frame_finish(frame);
  mad_stream_finish(stream);

// finalize openal
  alDeleteSources(1, @source);
  alDeleteBuffers(bufcnt, @buffers);
  alcDestroyContext(context);
  alcCloseDevice(device);
end.