
{$IFDEF OGC_INTERFACE}
const
  PAD_CHAN0 = 0;
  PAD_CHAN1 = 1;
  PAD_CHAN2 = 2;
  PAD_CHAN3 = 3;
  PAD_CHANMAX = 4;
  PAD_MOTOR_STOP = 0;
  PAD_MOTOR_RUMBLE = 1;
  PAD_MOTOR_STOP_HARD = 2;
  PAD_ERR_NONE = 0;
  PAD_ERR_NO_CONTROLLER = - 1;
  PAD_ERR_NOT_READY = - 2;
  PAD_ERR_TRANSFER = - 3;
  PAD_BUTTON_LEFT = $0001;
  PAD_BUTTON_RIGHT = $0002;
  PAD_BUTTON_DOWN = $0004;
  PAD_BUTTON_UP = $0008;
  PAD_TRIGGER_Z = $0010;
  PAD_TRIGGER_R = $0020;
  PAD_TRIGGER_L = $0040;
  PAD_BUTTON_A = $0100;
  PAD_BUTTON_B = $0200;
  PAD_BUTTON_X = $0400;
  PAD_BUTTON_Y = $0800;
  PAD_BUTTON_MENU = $1000;
  PAD_BUTTON_START = $1000;
  PAD_CHAN0_BIT = $80000000;
  PAD_CHAN1_BIT = $40000000;
  PAD_CHAN2_BIT = $20000000;
  PAD_CHAN3_BIT = $10000000;

type
  _padstatus = record
    button : cuint16;
    stickX : cint8;
    stickY : cint8;
    substickX : cint8;
    substickY : cint8;
    triggerL : cuint8;
    triggerR : cuint8;
    analogA : cuint8;
    analogB : cuint8;
    err : cint8;
  end;
  PADStatus = _padstatus;
  PPADStatus = ^PADStatus;


  sampling_callback = procedure; cdecl;


function PAD_Init: cuint32; cdecl; external;

function PAD_Sync: cuint32; cdecl; external;

function PAD_Read(status: PPADStatus): cuint32; cdecl; external;

function PAD_Reset(mask: cuint32): cuint32; cdecl; external;

function PAD_Recalibrate(mask: cuint32): cuint32; cdecl; external;

procedure PAD_Clamp(status: PPADStatus); cdecl; external;

procedure PAD_ControlMotor(chan: cint32; cmd: cuint32); cdecl; external;

procedure PAD_SetSpec(spec: cuint32); cdecl; external;


function PAD_ScanPads: cuint32; cdecl; external;


function PAD_ButtonsUp(pad: cint): cuint16; cdecl; external;

function PAD_ButtonsDown(pad: cint): cuint16; cdecl; external;

function PAD_ButtonsHeld(pad: cint): cuint16; cdecl; external;


function PAD_SubStickX(pad: cint): cint8; cdecl; external;

function PAD_SubStickY(pad: cint): cint8; cdecl; external;


function PAD_StickX(pad: cint): cint8; cdecl; external;

function PAD_StickY(pad: cint): cint8; cdecl; external;


function PAD_TriggerL(pad: cint): cuint8; cdecl; external;

function PAD_TriggerR(pad: cint): cuint8; cdecl; external;


function PAD_SetSamplingCallback(cb: sampling_callback): sampling_callback; cdecl; external;
{$ENDIF OGC_INTERFACE}

