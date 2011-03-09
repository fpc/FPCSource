{$ifdef OGC_INTERFACE}

const
  AI_STREAM_STOP = $00000000;  (*!< Stop streaming audio playback  *)
  AI_STREAM_START = $00000001;  (*!< Start streaming audio playback  *)
  AI_SAMPLERATE_32KHZ = $00000000;  (*!< AI sampling rate at 32kHz  *)
  AI_SAMPLERATE_48KHZ = $00000001;  (*!< AI sampling rate at 48kHz  *)

type
  AIDCallback = procedure; cdecl;
  AISCallback = procedure(smp_cnt: cuint32); cdecl;



function AUDIO_RegisterStreamCallback(callback: AISCallback): AISCallback; cdecl; external;



procedure AUDIO_Init(stack: pcuint8); cdecl; external;


procedure AUDIO_SetStreamVolLeft(vol: cuint8); cdecl; external;


function AUDIO_GetStreamVolLeft: cuint8; cdecl; external;


procedure AUDIO_SetStreamVolRight(vol: cuint8); cdecl; external;


function AUDIO_GetStreamVolRight: cuint8; cdecl; external;


procedure AUDIO_SetStreamSampleRate(rate: cuint32); cdecl; external;

function AUDIO_GetStreamSampleRate: cuint32; cdecl; external;

function AUDIO_RegisterDMACallback(callback: AIDCallback): AIDCallback; cdecl; external;

procedure AUDIO_InitDMA(startaddr, len: cuint32); cdecl; external;

function AUDIO_GetDMAEnableFlag: cuint16; cdecl; external;

procedure AUDIO_StartDMA; cdecl; external;

procedure AUDIO_StopDMA; cdecl; external;

function AUDIO_GetDMABytesLeft: cuint32; cdecl; external;

function AUDIO_GetDMALength: cuint32; cdecl; external;

function AUDIO_GetDMAStartAddr: cuint32; cdecl; external;

procedure AUDIO_SetStreamTrigger(cnt: cuint32); cdecl; external;

procedure AUDIO_ResetStreamSampleCnt; cdecl; external;

procedure AUDIO_SetDSPSampleRate(rate: cuint8); cdecl; external;

function AUDIO_GetDSPSampleRate: cuint32; cdecl; external;

procedure AUDIO_SetStreamPlayState(state: cuint32); cdecl; external;

function AUDIO_GetStreamPlayState: cuint32; cdecl; external;

{$endif OGC_INTERFACE}

