
{$IFDEF OGC_INTERFACE}
{$ifdef HW_RVL}


type
  mouse_event = record
    button : cuint8;
    rx : cint;
    ry : cint;
    rz : cint;
  end;
  Pmouse_event = ^mouse_event;


function MOUSE_Init: cint32; cdecl; external;

function MOUSE_Deinit: cint32; cdecl; external;


function MOUSE_GetEvent(event: Pmouse_event): cint32; cdecl; external;

function MOUSE_FlushEvents: cint32; cdecl; external;

function MOUSE_IsConnected: cbool; cdecl; external;


{$endif HW_RVL}
{$ENDIF}


