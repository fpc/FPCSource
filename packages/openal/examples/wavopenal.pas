(* WavOpenAL - OpenAL wave playing example

 Copyright (c) 2010 Dmitry Boyarintsev

 This software is provided 'as-is', without any express or implied
 warranty. In no event will the authors be held liable for any damages
 arising from the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.

  WaveOpenAL is based on MadOpenAL playing sample.
  Yhe wavopenal program accepts a single .wav file name as a parameter and
  plays it using openal until the end.

*)
program wavopenal;

{$mode objfpc}

uses
  classes, sysutils, openal;

// WAVE UTILS   
  
type
  TRiffHeader = packed record
    ID      : array [0..3] of char;
    Size    : LongWord;
    Format  : array [0..3] of char;
  end;

  TWaveFormat = packed record
    ID            : array [0..3] of char;
    Size          : LongWord;
    Format        : Word;
    Channels      : Word;
    SampleRate    : LongWord;
    ByteRate      : LongWord;
    BlockAlign    : Word;
    BitsPerSample : Word;
  end;

  TDataChunk = packed record
    Id      : array [0..3] of char;
    Size    : LongWord;
  end;

type

  { TWaveReader }

  TWaveReader = class(TObject)
  private
    loaded    : Boolean;
    chunkdata : TDataChunk;
    chunkpos  : Int64;
    pos       : Int64;
    eof       : Boolean;
    fStream   : TStream;

  public
    fmt   : TWaveFormat;
    function LoadFromStream(AStream: TStream): Boolean;
    function ReadBuf(var Buffer; BufferSize: Integer): Integer;
  end;

const
  ID_RIFF = 'RIFF';
  ID_WAVE = 'WAVE';
  ID_fmt  = 'fmt ';
  ID_data = 'data';

{ TWaveReader }

function TWaveReader.LoadFromStream(AStream:TStream):Boolean;
var
  riff  : TRiffHeader;
begin
  fStream:=AStream;
  loaded:=True;
  try
    Result:=fStream.Read(riff, sizeof(riff))=sizeof(riff);
    riff.Size:=LEtoN(riff.Size);
    Result:=Result and (riff.ID=ID_RIFF) and (riff.Format=ID_WAVE);
    if not Result then Exit;

    Result:=fStream.Read(fmt, sizeof(fmt))=sizeof(fmt);
    fmt.Size:=LEtoN(fmt.Size);
    fmt.Format:=LEtoN(fmt.Format);
    fmt.Channels:=LEtoN(fmt.Channels);
    fmt.SampleRate:=LEtoN(fmt.SampleRate);
    fmt.ByteRate:=LEtoN(fmt.ByteRate);
    fmt.BlockAlign:=LEtoN(fmt.BlockAlign);
    fmt.BitsPerSample:=LEtoN(fmt.BitsPerSample);
    
    Result:=fmt.ID=ID_fmt;
    pos:=-1;
  except
    Result:=False;
    Exit;
  end;
end;

function Min(a,b: Integer): Integer;
begin
  if a<b then Result:=a
  else Result:=b;
end;

function TWaveReader.ReadBuf(var Buffer;BufferSize:Integer):Integer;
var
  sz  : Integer;
  p   : PByteArray;
  i   : Integer;
begin
  FillChar(Buffer, BufferSize, 0);
  Result:=0;
  // all data read
  if eof then Exit; 

  p:=@Buffer;
  i:=0;
  while (not eof) and (i<bufferSize) do begin
    if chunkpos>=chunkdata.Size then begin
      if pos<0 then
        fstream.Position:=sizeof(TRiffHeader)+Int64(fmt.Size)+sizeof(TDataChunk)
     else
        fstream.Position:=pos+chunkdata.size+SizeOf(chunkdata);

      eof:=pos>=fStream.Size;
      if not eof then begin
        pos:=fStream.Position;
        sz:=fstream.Read(chunkdata, sizeof(chunkdata));
        chunkdata.Size:=LEtoN(chunkdata.Size);
        if (sz<>sizeof(chunkdata)) or (chunkdata.Id<>ID_data) then
          chunkpos:=chunkdata.Size
        else
          chunkpos:=0;
      end;
    end else begin
      sz:=Min(BufferSize, chunkdata.Size-chunkpos);
      fStream.Position:=pos+sizeof(chunkdata)+chunkpos;
      sz:=fStream.Read(p[i], sz);
      if sz<0 then Exit;
      inc(chunkpos, sz);
      inc(i, sz);
    end;
  end;
  Result:=i;
end;
  
// ------------------------ OPEN AL ----------------------

var
  source     : TStream;
  codec_bs   : Longword;

// openal
const
  // Note: if you lower the al_bufcount, then you have to modify the al_polltime also!
  al_bufcount           = 4;
  al_polltime           = 100;

var
  al_device   : PALCdevice;
  al_context  : PALCcontext;
  al_source   : ALuint;
  al_format   : Integer;
  al_buffers  : array[0..al_bufcount-1] of ALuint;
  al_bufsize  : Longword;
  al_readbuf  : Pointer;
  al_rate     : Longword;
  
  wave       : TWaveReader;

procedure alPlay;
var
  i: Integer;
begin
  alSourceStop(al_source);
  alSourceRewind(al_source);
  alSourcei(al_source, AL_BUFFER, 0);

  for i := 0 to al_bufcount - 1 do
  begin
    if wave.ReadBuf(al_readbuf^, al_bufsize) = 0 then
      Break;

    alBufferData(al_buffers[i], al_format, al_readbuf, al_bufsize, al_rate);
    alSourceQueueBuffers(al_source, 1, @al_buffers[i]);
  end;

  // Under windows, AL_LOOPING = AL_TRUE breaks queueing, no idea why
  alSourcei(al_source, AL_LOOPING, AL_FALSE);
  alSourcePlay(al_source);
end;

procedure alStop;
begin
  alSourceStop(al_source);
  alSourceRewind(al_source);
  alSourcei(al_source, AL_BUFFER, 0);
end;

function alProcess: Boolean;
var
  processed : ALint;
  buffer    : ALuint;
  sz        : Integer;
begin
  alGetSourcei(al_source, AL_BUFFERS_PROCESSED, processed);
  while (processed > 0) and (processed <= al_bufcount) do
  begin
    Write('.');

    alSourceUnqueueBuffers(al_source, 1, @buffer);

    sz:=wave.ReadBuf(al_readbuf^, al_bufsize);
    if sz <= 0 then
    begin
      Exit(False);
    end;

    alBufferData(buffer, al_format, al_readbuf, sz, al_rate);
    alSourceQueueBuffers(al_source, 1, @buffer);

    Dec(processed);
  end;

  Result := True;
end;


var
  Filename: String;
  queued  : Integer;
  done    : Boolean;
begin
  // define codec
  if (ParamCount<=0) or not FileExists(ParamStr(1)) then begin
    writeln('please specify .wav file name');
    Exit;
  end;
  FileName:=ParamStr(1);

  source := TFileStream.Create(Filename, fmOpenRead);

  // inittialize codec
  wave:=TWaveReader.Create;
  if not wave.LoadFromStream(source) then begin
    writeln('unable to read WAVE format');
    Exit;
  end;
  if wave.fmt.Format<>1 then begin
    writeln('WAVE file is using compression. Cannot play sorry. Please provide uncompressed .wav');
    Exit;
  end;
  if wave.fmt.Channels=2 then begin
    if wave.fmt.BitsPerSample=8 then al_format:=AL_FORMAT_STEREO8
    else al_format:=AL_FORMAT_STEREO16
  end else begin
    if wave.fmt.BitsPerSample=8 then al_format:=AL_FORMAT_MONO8
    else al_format:=AL_FORMAT_MONO16
  end;
  
  codec_bs:=2*wave.fmt.Channels;

  //al_bufsize := 20000 - (20000 mod codec_bs);
  al_bufsize := 20000 - (20000 mod codec_bs);
  al_rate:=wave.fmt.SampleRate;
  WriteLn('Blocksize    : ', codec_bs);
  WriteLn('Rate         : ', wave.fmt.SampleRate);
  WriteLn('Channels     : ', wave.fmt.Channels);
  WriteLn('OpenAL Buffers     : ', al_bufcount);
  WriteLn('OpenAL Buffer Size : ', al_bufsize);

  // init openal
  al_device := alcOpenDevice(nil);
  al_context := alcCreateContext(al_device, nil);
  alcMakeContextCurrent(al_context);

  alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED);
  alGenSources(1, @al_source);
  alGenBuffers(al_bufcount, @al_buffers);

  GetMem(al_readbuf, al_bufsize);


  // play loop
  alPlay;

  done:=False;
  queued:=0;
  repeat
    if alProcess then begin
      alGetSourcei(al_source, AL_BUFFERS_QUEUED, queued);
      done:=queued=0;
    end;
    Sleep(al_polltime);
  until done;

  alStop;

  // finalize openal
  alDeleteSources(1, @al_source);
  alDeleteBuffers(al_bufcount, @al_buffers);
  alcDestroyContext(al_context);
  alcCloseDevice(al_device);
  FreeMem(al_readbuf);


  // finalize codec
  wave.Free;
  
  // close file
  source.Free;
end.
