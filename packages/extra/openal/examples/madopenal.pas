program test;

{$mode objfpc}

uses
  classes, sysutils, ctypes, openal, mad, ogg, vorbis, a52, dts, modplug, matroska;

var
  source     : TStream;
  codec      : Integer;
  codec_bs   : Longword;
  codec_read : function(const Buffer: Pointer; const Count: Longword): Longword = nil;
  codec_rate : Longword;
  codec_chan : Longword;

function source_read_func(datasource: pointer; ptr: pointer; size: cuint): cuint; cdecl;
begin
  Result := TStream(datasource).Read(ptr^, size);
end;

function source_read_func_ogg(ptr: pointer; size, nmemb: csize_t; datasource: pointer): csize_t; cdecl;
begin
  Result := TStream(datasource).Read(ptr^, size*nmemb);
end;

function source_seek_func(datasource: pointer; offset: clong; whence: cint): clong; cdecl;
begin
  case whence of
    {SEEK_SET} 0: Result := TStream(datasource).Seek(offset, soFromBeginning);
    {SEEK_CUR} 1: Result := TStream(datasource).Seek(offset, soFromCurrent);
    {SEEK_END} 2: Result := TStream(datasource).Seek(offset, soFromEnd);
    else          Result := 0;
  end;
end;

function source_seek_func_ogg(datasource: pointer; offset: ogg_int64_t; whence: cint): cint; cdecl;
begin
  case whence of
    {SEEK_SET} 0: TStream(datasource).Seek(offset, soFromBeginning);
    {SEEK_CUR} 1: TStream(datasource).Seek(offset, soFromCurrent);
    {SEEK_END} 2: TStream(datasource).Seek(offset, soFromEnd);
  end;
  Result := 0;
end;

function source_close_func(datasource: pointer): cint; cdecl;
begin
  TStream(datasource).Position := 0;
  Result := 0;
end;

function source_tell_func(datasource: pointer): clong; cdecl;
begin
  Result := TStream(datasource).Position;
end;


// mad
var
  mad_decoder: pmad_decoder;

{procedure mad_reset;
begin
  mad_stream_finish(m_stream);
  mad_stream_init(m_stream);
  source.Position := 0;
end;}

function mad_read(const Buffer: Pointer; const Count: Longword): Longword;
var
  Res: cint;
begin
  Res := mad_decoder_read(mad_decoder, Buffer, Count);
  if Res < 0 then
    Result := 0 else
    Result := Res;
end;


// oggvorbis
var
  ogg_file      : OggVorbis_File;
  ogg_callbacks : ov_callbacks;

{procedure ogg_reset;
begin
  ov_pcm_seek(ogg_file, 0);
end;}

function ogg_read(const Buffer: Pointer; const Count: Longword): Longword;
var
  Res: clong;
begin
  Res := ov_read_ext(ogg_file, Buffer, Count, false, 2, true);
  if Res < 0 then
    Result := 0 else
    Result := Res;
end;


// a52
var
  a52_decoder : pa52_decoder;

function a52_read(const Buffer: Pointer; const Count: Longword): Longword;
var
  Res: cint;
begin
  Res := a52_decoder_read(a52_decoder, Buffer, Count);
  if Res < 0 then
    Result := 0 else
    Result := Res;
end;


// dts
var
  dts_decoder : pdts_decoder;

function dts_read(const Buffer: Pointer; const Count: Longword): Longword;
var
  Res: cint;
begin
  //WriteLn('enter dts_decoder_read');
  Res := dts_decoder_read(dts_decoder, Buffer, Count);
  //WriteLn('leave dts_decoder_read ', Res);
  if Res < 0 then
    Result := 0 else
    Result := Res;
end;


// modplug
var
  mod_file: PModPlugFile;

function mod_read(const Buffer: Pointer; const Count: Longword): Longword;
var
  Res: cint;
begin
  Res := ModPlug_Read(mod_file, Buffer, Count);
  if Res < 0 then
    Result := 0 else
    Result := Res;
end;


// openal
const
  al_format  : array[1..2] of ALenum = (AL_FORMAT_MONO16, AL_FORMAT_STEREO16);

// Note: if you lower the al_bufcount, then you have to modify the al_polltime also!
  al_bufcount           = 4;
  al_polltime           = 100;

var
  al_device  : PALCdevice;
  al_context : PALCcontext;
  al_source  : ALuint;
  al_buffers : array[0..al_bufcount-1] of ALuint;
  al_bufsize : Longword;
  al_readbuf : Pointer;

procedure alPlay;
var
  i: Integer;
begin
  alSourceStop(al_source);
  alSourceRewind(al_source);
  alSourcei(al_source, AL_BUFFER, 0);

  for i := 0 to al_bufcount - 1 do
  begin
    if codec_read(al_readbuf, al_bufsize) = 0 then
      Break;

    //alBufferData(al_buffers[i], al_format[codec_chan], al_readbuf, al_bufsize, codec_rate);
    alBufferWriteData_LOKI(al_buffers[i], al_format[codec_chan], al_readbuf, al_bufsize, codec_rate, al_format[codec_chan]);
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
begin
  alGetSourcei(al_source, AL_BUFFERS_PROCESSED, processed);
  while (processed > 0) and (processed <= al_bufcount) do
  begin
    Write('.');

    alSourceUnqueueBuffers(al_source, 1, @buffer);

    if codec_read(al_readbuf, al_bufsize) = 0 then
    begin
      alStop;
      Exit(False);
    end;

    //alBufferData(buffer, al_format[codec_chan], al_readbuf, al_bufsize, codec_rate);
    alBufferWriteData_LOKI(buffer, al_format[codec_chan], al_readbuf, al_bufsize, codec_rate, al_format[codec_chan]);
    alSourceQueueBuffers(al_source, 1, @buffer);

    Dec(processed);
  end;

  Result := True;
end;

var
  Filename: String;
  ov: pvorbis_info;
  tmp: pointer;
begin
// define codec
  WriteLn('Define codec');
  Writeln('  (1) mp3');
  Writeln('  (2) ogg');
  Writeln('  (3) ac3');
  Writeln('  (4) dts');
  Writeln('  (5) xm,mod,it,s3m');
  Writeln('  (6) mka');
  Write('Enter: '); ReadLn(codec);
  Write('File: '); ReadLn(Filename);

  {codec := 4;
  Filename := 'test.dts';}


// load file
  source := TFileStream.Create(Filename, fmOpenRead);


// inittialize codec
  case codec of
    1: // mad
      begin
        mad_decoder := mad_decoder_init(source, @source_read_func, @source_seek_func, @source_close_func, @source_tell_func);
        codec_read := @mad_read;
        codec_rate := 44100;
        codec_chan := 2;
        codec_bs   := 2*codec_chan;
      end;

    2: // oggvorbis
      begin
        ogg_callbacks.read  := @source_read_func_ogg;
        ogg_callbacks.seek  := @source_seek_func_ogg;
        ogg_callbacks.close := @source_close_func;
        ogg_callbacks.tell  := @source_tell_func;

        if ov_open_callbacks(source, ogg_file, nil, 0, ogg_callbacks) >= 0 then
        begin
          ov := ov_info(ogg_file, -1);
          codec_read := @ogg_read;
          codec_rate := ov^.rate;
          codec_chan := ov^.channels;
          codec_bs   := 2*codec_chan;
        end;
      end;

    3: // a52
      begin
        a52_decoder := a52_decoder_init(0, source, @source_read_func, @source_seek_func, @source_close_func, @source_tell_func);
        codec_read := @a52_read;
        codec_rate := 44100;//48000;
        codec_chan := 2;
        codec_bs   := 2*codec_chan;
      end;

    4: // a52
      begin
        dts_decoder := dts_decoder_init(0, source, @source_read_func, @source_seek_func, @source_close_func, @source_tell_func);
        codec_read := @dts_read;
        codec_rate := 44100;//48000;
        codec_chan := 2;
        codec_bs   := 2*codec_chan;
      end;

    5: // modplug
      begin
        GetMem(tmp, source.Size);
        source.Read(tmp^, source.Size);
        mod_file := ModPlug_Load(tmp, source.Size);
        FreeMem(tmp);

        codec_read := @mod_read;
        codec_rate := 44100;//48000;
        codec_chan := 2;
        codec_bs   := 2*codec_chan;
      end;
  end;

  if not Assigned(codec_read) then
    Exit;

  //al_bufsize := 20000 - (20000 mod codec_bs);
  al_bufsize := 20000 - (20000 mod codec_bs);
  WriteLn('Codec Blocksize    : ', codec_bs);
  WriteLn('Codec Rate         : ', codec_rate);
  WriteLn('Codec Channels     : ', codec_chan);
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


// play
  alPlay;
  while alProcess do
    Sleep(al_polltime);

// finalize openal
  alDeleteSources(1, @al_source);
  alDeleteBuffers(al_bufcount, @al_buffers);
  alcDestroyContext(al_context);
  alcCloseDevice(al_device);
  FreeMem(al_readbuf);


// finalize codec
  case codec of
    1: // mad
      begin
        mad_decoder_free(mad_decoder);
      end;

    2: // oggvorbis
      begin
        ov_clear(ogg_file);
      end;

    3: // a52
      begin
        a52_decoder_free(a52_decoder);
      end;

    4: // dts
      begin
        dts_decoder_free(dts_decoder);
      end;

    5: // modplug
      begin
        ModPlug_Unload(mod_file);
      end;
  end;


// close file
  source.Free;
end.