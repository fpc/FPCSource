program sfplay;

{$mode objfpc}
{$h+}

uses sndfile, baseunix;

Const
  BUFFERLEN = 1024;

Const
  { Values obtained from a C program - These are complex (!) C macros }
  SNDCTL_DSP_STEREO = -1073459197;
  SNDCTL_DSP_RESET = 20480;
  SNDCTL_DSP_SYNC = 20481;
  SOUND_PCM_WRITE_BITS = -1073459195;
  SOUND_PCM_WRITE_CHANNELS = -1073459194;
  SOUND_PCM_WRITE_RATE = -1073459198;

ResourceString
  SPlaying = 'Playing : ';
  SErrChannels = 'Error : Number of channels not supported: ';
  SErrOpeningDevice = 'Could not open sound device';
  SErrSettingStereo = 'Could not set stereo';
  SErrResettingDevice = 'Could not reset DSP device';
  SErrSetWriteBits = 'Could not set write bits to 16';
  SErrSetChannels = 'Could not set channels';
  SErrSetSampleRate = 'Could not set sync mode';
  SErrSetSyncMode = 'Could not set sync mode';

Procedure PlayError(Msg : String);
begin
  Writeln(stderr,Msg);
  Halt(1);
end;

Function OpenDSPDevice(Channels,Samplerate : LongInt) : LongInt; forward;

procedure PlayFile(FileName : String);
var
  Buffer: Array[0..BUFFERLEN-1] of word;
  SoundFile: PSndFile;
  Info: TSF_INFO;
  k, m, AudioDevice, readcount: Longint;
  ScaleData: Boolean;
begin
  Writeln(SPlaying,FileName);
  SoundFile:=sf_open(pChar(FileName), SFM_READ, @Info);
  If (SoundFile=Nil) then
    begin
      sf_perror(Nil);
      exit;
    end;
  If not (Info.Channels in [1,2]) then
    PlayError(SerrChannels);
  AudioDevice:=OpenDSPDevice(Info.channels, Info.samplerate);
  ScaleData:=(Info.samplerate < 16);
  readcount:=sf_read_short(SoundFile,@Buffer,BUFFERLEN);
  While ReadCount<>0 do
    begin
      If ScaleData then
        For m:=0 to BufferLen-1 do
          Buffer[m]:=buffer[m] * 256;
      FpWrite(AudioDevice, buffer, readcount * sizeof (word)) ;
      readcount:=sf_read_short(SoundFile,@Buffer,BUFFERLEN);
    end;
  sf_close (Soundfile) ;
  FpClose(AudioDevice) ;
end;

Function OpenDSPDevice (channels,SampleRate : LongInt) : Longint;
var
 fd, stereo, temp, error : longint ;
begin
  fd:=fpOpen('/dev/dsp',O_WRONLY,0);
  if fd<0 then
    PlayError(SErrOpeningDevice);
  Stereo:=0;
  if Not (FpIOCtl(fd, SNDCTL_DSP_STEREO  , @stereo) <> -1) then
    PlayError(SErrSettingStereo);
  if Not (FpIOCtl(fd, SNDCTL_DSP_RESET, Nil) <> -1) then
    PlayError(SErrResettingDevice);
  temp := 16 ;
  If not (FpIOCtl(fd, SOUND_PCM_WRITE_BITS, @temp) <> -1) then
    PlayError(SErrSetWriteBits);
  If not (FpIOCtl(fd, SOUND_PCM_WRITE_CHANNELS, @channels) <> -1) then
    PlayError(SErrSetChannels);
  If Not (FpIOCtl(fd, SOUND_PCM_WRITE_RATE, @SampleRate) <> -1) then
    PlayError(SErrSetSampleRate);
  If not (FpIOCtl(fd, SNDCTL_DSP_SYNC, Nil) <> -1) then
    PlayError(SErrSetSyncMode);
  OpenDSPDevice:=Fd;
end;


Var
  I : Integer;
begin
  For I:=1 to ParamCount do
    PlayFile(Paramstr(i));
end.
