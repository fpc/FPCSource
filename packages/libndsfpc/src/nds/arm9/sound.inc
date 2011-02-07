{$ifdef NDS_INTERFACE}

type
  MicCallback = procedure(completedBuffer: pointer; length: cint);

  SoundFormat = integer;
const
  SoundFormat_8Bit  : SoundFormat = 0;
  SoundFormat_16Bit : SoundFormat = 1;
  SoundFormat_ADPCM : SoundFormat = 2;
  SoundFormat_PSG   : SoundFormat = 3;

type
  MicFormat = integer;
const
  MicFormat_12Bit : MicFormat = 0;
  MicFormat_8Bit  : MicFormat = 1;
  
type
  DutyCycle = integer;
const
  DutyCycle_12  : DutyCycle = 0;
  DutyCycle_25  : DutyCycle = 1;
  DutyCycle_37  : DutyCycle = 2;
  DutyCycle_50  : DutyCycle = 3;
  DutyCycle_62  : DutyCycle = 4;
  DutyCycle_75  : DutyCycle = 5;
  DutyCycle_87  : DutyCycle = 6;
  DutyCycle_0   : DutyCycle = 7;
  
procedure soundEnable(); cdecl; external;

procedure soundDisable(); cdecl; external;

function soundPlaySample(const data: pointer; format: SoundFormat; dataSize: cuint32; freq: cuint16; volume, pan: cuint8; loop: cbool; loopPoint: cuint16): cint; cdecl; external; 

function soundPlayPSG(cycle: DutyCycle; freq: cuint16; volume, pan: cuint8): cint; cdecl; external;

function soundPlayNoise(freq: cuint16; volume, pan: cuint8): cint; cdecl; external;

procedure soundPause(soundId: cint); cdecl; external;

procedure soundSetWaveDuty(soundId: cint; cycle: DutyCycle); cdecl; external;

procedure soundKill(soundId: cint); cdecl; external;

procedure soundResume(soundId: cint); cdecl; external;

procedure soundSetVolume(soundId: cint; volume: cuint8); cdecl; external;

procedure soundSetPan(soundId: cint; pan: cuint8); cdecl; external;

procedure soundSetFreq(soundId: cint; freq: cuint16); cdecl; external;

function soundMicRecord(buffer: pointer; bufferLength: cuint32; format: MicFormat; freq: cint; callback: MicCallback): cint; cdecl; external;

procedure soundMicOff(); cdecl; external;

{$endif NDS_INTERFACE}
