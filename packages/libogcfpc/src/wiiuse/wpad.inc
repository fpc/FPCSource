{$IFDEF OGC_INTERFACE}
const
  WPAD_MAX_IR_DOTS = 4;

  WPAD_CHAN_ALL = - 1;
  WPAD_CHAN_0 = WPAD_CHAN_ALL + 1;
  WPAD_CHAN_1 = WPAD_CHAN_0 + 1;
  WPAD_CHAN_2 = WPAD_CHAN_1 + 1;
  WPAD_CHAN_3 = WPAD_CHAN_2 + 1;
  WPAD_BALANCE_BOARD = WPAD_CHAN_3 + 1;
  WPAD_MAX_WIIMOTES = WPAD_BALANCE_BOARD + 1;

  WPAD_BUTTON_2 = $0001;
  WPAD_BUTTON_1 = $0002;
  WPAD_BUTTON_B = $0004;
  WPAD_BUTTON_A = $0008;
  WPAD_BUTTON_MINUS = $0010;
  WPAD_BUTTON_HOME = $0080;
  WPAD_BUTTON_LEFT = $0100;
  WPAD_BUTTON_RIGHT = $0200;
  WPAD_BUTTON_DOWN = $0400;
  WPAD_BUTTON_UP = $0800;
  WPAD_BUTTON_PLUS = $1000;

  WPAD_NUNCHUK_BUTTON_Z = ( $0001 shl 16 );
  WPAD_NUNCHUK_BUTTON_C = ( $0002 shl 16 );

  WPAD_CLASSIC_BUTTON_UP = ( $0001 shl 16 );
  WPAD_CLASSIC_BUTTON_LEFT = ( $0002 shl 16 );
  WPAD_CLASSIC_BUTTON_ZR = ( $0004 shl 16 );
  WPAD_CLASSIC_BUTTON_X = ( $0008 shl 16 );
  WPAD_CLASSIC_BUTTON_A = ( $0010 shl 16 );
  WPAD_CLASSIC_BUTTON_Y = ( $0020 shl 16 );
  WPAD_CLASSIC_BUTTON_B = ( $0040 shl 16 );
  WPAD_CLASSIC_BUTTON_ZL = ( $0080 shl 16 );
  WPAD_CLASSIC_BUTTON_FULL_R = ( $0200 shl 16 );
  WPAD_CLASSIC_BUTTON_PLUS = ( $0400 shl 16 );
  WPAD_CLASSIC_BUTTON_HOME = ( $0800 shl 16 );
  WPAD_CLASSIC_BUTTON_MINUS = ( $1000 shl 16 );
  WPAD_CLASSIC_BUTTON_FULL_L = ( $2000 shl 16 );
  WPAD_CLASSIC_BUTTON_DOWN = ( $4000 shl 16 );
  WPAD_CLASSIC_BUTTON_RIGHT = ( $8000 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_STRUM_UP = ( $0001 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_YELLOW = ( $0008 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_GREEN = ( $0010 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_BLUE = ( $0020 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_RED = ( $0040 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_ORANGE = ( $0080 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_PLUS = ( $0400 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_MINUS = ( $1000 shl 16 );
  WPAD_GUITAR_HERO_3_BUTTON_STRUM_DOWN = ( $4000 shl 16 );

  WPAD_EXP_NONE = 0;
  WPAD_EXP_NUNCHUK = 1;
  WPAD_EXP_CLASSIC = 2;
  WPAD_EXP_GUITARHERO3 = 3;
  WPAD_EXP_WIIBOARD = 4;
  WPAD_EXP_UNKNOWN = 255;

  WPAD_FMT_BTNS = 0;
  WPAD_FMT_BTNS_ACC = 1;
  WPAD_FMT_BTNS_ACC_IR = 2;

  WPAD_STATE_DISABLED = 0;
  WPAD_STATE_ENABLING = 1;
  WPAD_STATE_ENABLED = 2;

  WPAD_ERR_NONE = 0;
  WPAD_ERR_NO_CONTROLLER = - 1;
  WPAD_ERR_NOT_READY = - 2;
  WPAD_ERR_TRANSFER = - 3;
  WPAD_ERR_NONEREGISTERED = - 4;
  WPAD_ERR_UNKNOWN = - 5;
  WPAD_ERR_BAD_CHANNEL = - 6;
  WPAD_ERR_QUEUE_EMPTY = - 7;
  WPAD_ERR_BADVALUE = - 8;
  WPAD_ERR_BADCONF = - 9;
  WPAD_DATA_BUTTONS = $01;
  WPAD_DATA_ACCEL = $02;
  WPAD_DATA_EXPANSION = $04;
  WPAD_DATA_IR = $08;
  WPAD_ENC_FIRST = $00;
  WPAD_ENC_CONT = $01;
  WPAD_THRESH_IGNORE = - 1;
  WPAD_THRESH_ANY = 0;
  WPAD_THRESH_DEFAULT_BUTTONS = 0;
  WPAD_THRESH_DEFAULT_IR = WPAD_THRESH_IGNORE;
  WPAD_THRESH_DEFAULT_ACCEL = 20;
  WPAD_THRESH_DEFAULT_JOYSTICK = 2;
  WPAD_THRESH_DEFAULT_BALANCEBOARD = 60;
  WPAD_THRESH_DEFAULT_MOTION_PLUS = 100;

type
  _wpad_data = packed record
    err : cint16;
    data_present : cuint32;
    battery_level : cuint8;
    btns_h : cuint32;
    btns_l : cuint32;
    btns_d : cuint32;
    btns_u : cuint32;
    ir : ir_t;
    accel : vec3w_t;
    orient : orient_t;
    gforce : gforce_t;
    exp : expansion_t;
  end;
  WPADData = _wpad_data;
  PWPADData = ^WPADData;

  _wpad_encstatus = packed record
    data : array [0..31] of cuint8;
  end;
  WPADEncStatus = _wpad_encstatus;
  PWPADEncStatus = ^_wpad_encstatus;

  WPADDataCallback = procedure(chan: cint32; data: PWPADData); cdecl;
  WPADShutdownCallback = procedure(chan: cint32); cdecl;


function WPAD_Init: cint32; cdecl; external;

function WPAD_ControlSpeaker(chan, enable: cint32): cint32; cdecl; external;

function WPAD_ReadEvent(chan: cint32; data: PWPADData): cint32; cdecl; external;

function WPAD_DroppedEvents(chan: cint32): cint32; cdecl; external;

function WPAD_Flush(chan: cint32): cint32; cdecl; external;

function WPAD_ReadPending(chan: cint32; datacb: WPADDataCallback): cint32; cdecl; external;

function WPAD_SetDataFormat(chan, fmt: cint32): cint32; cdecl; external;

function WPAD_SetMotionPlus(chan: cint32; enable: cuint8): cint32; cdecl; external;

function WPAD_SetVRes(chan: cint32; xres, yres: cuint32): cint32; cdecl; external;

function WPAD_GetStatus: cint32; cdecl; external;

function WPAD_Probe(chan: cint32; type_: pcuint32): cint32; cdecl; external;

function WPAD_SetEventBufs(chan: cint32; bufs: PWPADData; cnt: cuint32): cint32; cdecl; external;

function WPAD_Disconnect(chan: cint32): cint32; cdecl; external;

function WPAD_IsSpeakerEnabled(chan: cint32): cint32; cdecl; external;

function WPAD_SendStreamData(chan: cint32; buf: pointer; len: cuint32): cint32; cdecl; external;

procedure WPAD_Shutdown; cdecl; external;

procedure WPAD_SetIdleTimeout(seconds: cuint32); cdecl; external;

procedure WPAD_SetPowerButtonCallback(cb: WPADShutdownCallback); cdecl; external;

procedure WPAD_SetBatteryDeadCallback(cb: WPADShutdownCallback); cdecl; external;

function WPAD_ScanPads: cint32; cdecl; external;

function WPAD_Rumble(chan: cint32; status: cint): cint32; cdecl; external;

function WPAD_SetIdleThresholds(chan, btns, ir, accel, js, wb, mp: cint32)
  : cint32; cdecl; external;

procedure WPAD_EncodeData(info: PWPADEncStatus; flag: cuint32;
  pcmSamples: pcint16; numSamples: cint32; encData: pcuint8); cdecl; external;

function WPAD_Data(chan: cint): PWPADData; cdecl; external;

function WPAD_BatteryLevel(chan: cint): cuint8; cdecl; external;

function WPAD_ButtonsUp(chan: cint): cuint32; cdecl; external;

function WPAD_ButtonsDown(chan: cint): cuint32; cdecl; external;

function WPAD_ButtonsHeld(chan: cint): cuint32; cdecl; external;

procedure WPAD_IR(chan: cint; ir: Pir_t); cdecl; external;

procedure WPAD_Orientation(chan: cint; orient: Porient_t); cdecl; external;

procedure WPAD_GForce(chan: cint; gforce: Pgforce_t); cdecl; external;

procedure WPAD_Accel(chan: cint; accel: Pvec3w_t); cdecl; external;

procedure WPAD_Expansion(chan: cint; exp: Pexpansion_t); cdecl; external;
{$ENDIF}

