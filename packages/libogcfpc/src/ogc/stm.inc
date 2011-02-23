
{$IFDEF OGC_INTERFACE}
{$ifdef HW_RVL}

const
  STM_EVENT_RESET = $00020000;
  STM_EVENT_POWER = $00000800;
  STM_EINVAL = - $2004;
  STM_ENOTINIT = - $2100;
  STM_ENOHANDLER = - $2101;
type
  stmcallback = procedure(event: cuint32); cdecl;


function __STM_Init: cint32; cdecl; external;

function __STM_Close: cint32; cdecl; external;

function STM_ShutdownToStandby: cint32; cdecl; external;

function STM_ShutdownToIdle: cint32; cdecl; external;

function STM_SetLedMode(mode: cuint32): cint32; cdecl; external;

function STM_RebootSystem: cint32; cdecl; external;

function STM_RegisterEventHandler(newhandler: stmcallback): stmcallback; cdecl; external;


{$endif HW_RVL}
{$ENDIF OGC_INTERFACE}


