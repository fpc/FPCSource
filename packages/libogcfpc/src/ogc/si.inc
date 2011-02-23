
{$IFDEF OGC_INTERFACE}
const
  SI_CHAN0 = 0;
  SI_CHAN1 = 1;
  SI_CHAN2 = 2;
  SI_CHAN3 = 3;
  SI_MAX_CHAN = 4;
  SI_CHAN0_BIT = $80000000;
  SI_CHAN1_BIT = $40000000;
  SI_CHAN2_BIT = $20000000;
  SI_CHAN3_BIT = $10000000;

function SI_CHAN_BIT(chn: cint): cint; inline;

const
  SI_ERROR_UNDER_RUN = $0001;
  SI_ERROR_OVER_RUN = $0002;
  SI_ERROR_COLLISION = $0004;
  SI_ERROR_NO_RESPONSE = $0008;
  SI_ERROR_WRST = $0010;
  SI_ERROR_RDST = $0020;
  SI_ERR_UNKNOWN = $0040;
  SI_ERR_BUSY = $0080;

  SI_TYPE_MASK = $18000000;
  SI_TYPE_N64 = $00000000;
  SI_TYPE_DOLPHIN = $08000000;
  SI_TYPE_GC = SI_TYPE_DOLPHIN;

// GameCube specific
const
  SI_GC_WIRELESS = $80000000;
  SI_GC_NOMOTOR = $20000000;  // no rumble motor
  SI_GC_STANDARD = $01000000;

// WaveBird specific
const
  SI_WIRELESS_RECEIVED = $40000000;  // 0: no wireless unit
  SI_WIRELESS_IR = $04000000; // 0: IR  1: RF
  SI_WIRELESS_STATE = $02000000; // 0: variable  1: fixed
  SI_WIRELESS_ORIGIN = $00200000; // 0: invalid  1: valid
  SI_WIRELESS_FIX_ID = $00100000; // 0: not fixed  1: fixed
  SI_WIRELESS_TYPE = $000f0000;
  SI_WIRELESS_LITE_MASK = $000c0000;  // 0: normal 1: lite controller
  SI_WIRELESS_LITE = $00040000; // 0: normal 1: lite controller
  SI_WIRELESS_CONT_MASK = $00080000; // 0: non-controller 1: non-controller
  SI_WIRELESS_CONT  =$00000000;
  SI_WIRELESS_ID = $00c0ff00;
  SI_WIRELESS_TYPE_ID = ( SI_WIRELESS_TYPE or SI_WIRELESS_ID );

  SI_N64_CONTROLLER = ( SI_TYPE_N64 or $05000000 );
  SI_N64_MIC = ( SI_TYPE_N64 or $00010000 );
  SI_N64_KEYBOARD = ( SI_TYPE_N64 or $00020000 );
  SI_N64_MOUSE = ( SI_TYPE_N64 or $02000000 );
  SI_GBA = ( SI_TYPE_N64 or $00040000 );
  SI_GC_CONTROLLER = ( SI_TYPE_GC or SI_GC_STANDARD );
  SI_GC_RECEIVER = ( SI_TYPE_GC or SI_GC_WIRELESS );
  SI_GC_WAVEBIRD = ( SI_TYPE_GC or SI_GC_WIRELESS or SI_GC_STANDARD or SI_WIRELESS_STATE or SI_WIRELESS_FIX_ID );
  SI_GC_KEYBOARD = ( SI_TYPE_GC or $00200000 );
  SI_GC_STEERING = ( SI_TYPE_GC or $00000000 );

type
  SICallback = procedure(par0: cint32; par1: cuint32); cdecl;
  RDSTHandler = procedure(par0: cuint32; par1: pointer); cdecl;


function SI_Sync: cuint32; cdecl; external;

function SI_Busy: cuint32; cdecl; external;

function SI_IsChanBusy(chan: cint32): cuint32; cdecl; external;

procedure SI_EnablePolling(poll: cuint32); cdecl; external;

procedure SI_DisablePolling(poll: cuint32); cdecl; external;

procedure SI_SetCommand(chan: cint32; cmd: cuint32); cdecl; external;

function SI_GetStatus(chan: cint32): cuint32; cdecl; external;

function SI_GetResponse(chan: cint32; buf: pointer): cuint32; cdecl; external;

function SI_GetResponseRaw(chan: cint32): cuint32; cdecl; external;

procedure SI_RefreshSamplingRate; cdecl; external;

function SI_Transfer(chan: cint32; out_: pointer; out_len: cuint32;
  in_: pointer; in_len: cuint32; cb: SICallback; us_delay: cuint32): cuint32; cdecl; external;

function SI_GetTypeAsync(chan: cint32; cb: SICallback): cuint32; cdecl; external;

function SI_GetType(chan: cint32): cuint32; cdecl; external;

function SI_GetCommand(chan: cint32): cuint32; cdecl; external;

procedure SI_TransferCommands; cdecl; external;

function SI_RegisterPollingHandler(handler: RDSTHandler): cuint32; cdecl; external;

function SI_UnregisterPollingHandler(handler: RDSTHandler): cuint32; cdecl; external;

function SI_EnablePollingInterrupt(enable: cint32): cuint32; cdecl; external;
{$ENDIF OGC_INTERFACE}


{$IFDEF OGC_IMPLEMENTATION}
function SI_CHAN_BIT(chn: cint): cint; inline;
begin
  SI_CHAN_BIT := SI_CHAN0_BIT shr chn;
end;

{$ENDIF OGC_IMPLEMENTATION}
