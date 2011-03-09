
{$IFDEF OGC_INTERFACE}
//#include "wsksymdef.h"

const
  MOD_SHIFT_L = ( 1 shl 0 );
  MOD_SHIFT_R = ( 1 shl 1 );
  MOD_SHIFTLOCK = ( 1 shl 2 );
  MOD_CAPSLOCK = ( 1 shl 3 );
  MOD_CONTROL_L = ( 1 shl 4 );
  MOD_CONTROL_R = ( 1 shl 5 );
  MOD_META_L = ( 1 shl 6 );
  MOD_META_R = ( 1 shl 7 );
  MOD_MODESHIFT = ( 1 shl 8 );
  MOD_NUMLOCK = ( 1 shl 9 );
  MOD_COMPOSE = ( 1 shl 10 );
  MOD_HOLDSCREEN = ( 1 shl 11 );
  MOD_COMMAND = ( 1 shl 12 );
  MOD_COMMAND1 = ( 1 shl 13 );
  MOD_COMMAND2 = ( 1 shl 14 );
  MOD_MODELOCK = ( 1 shl 15 );
  MOD_ANYSHIFT = ( MOD_SHIFT_L or MOD_SHIFT_R or MOD_SHIFTLOCK );
  MOD_ANYCONTROL = ( MOD_CONTROL_L or MOD_CONTROL_R );
  MOD_ANYMETA = ( MOD_META_L or MOD_META_R );

function MOD_ONESET(val, mask: cint): boolean; inline;

function MOD_ALLSET(val, mask: cint): boolean; inline;


type
  keyboard_event_type = (
  	KEYBOARD_CONNECTED,
  	KEYBOARD_DISCONNECTED,
  	KEYBOARD_PRESSED,
  	KEYBOARD_RELEASED);

type
  keyboard_event = record
    type_ : keyboard_event_type;
    modifiers : cuint16;
    keycode : cuint8;
    symbol : cuint16;
  end;
  pkeyboard_event = ^keyboard_event;


type
  keyPressCallback = procedure(symbol: cchar); cdecl;


function KEYBOARD_Init(keypress_cb: keyPressCallback): cint32; cdecl; external;

function KEYBOARD_Deinit: cint32; cdecl; external;


function KEYBOARD_GetEvent(event: Pkeyboard_event): cint32; cdecl; external;

function KEYBOARD_FlushEvents: cint32; cdecl; external;
{$ENDIF}


{$IFDEF OGC_IMPLEMENTATION}
function MOD_ONESET(val, mask: cint): boolean; inline;
begin
  MOD_ONESET := (val and mask) <> 0;
end;

function MOD_ALLSET(val, mask: cint): boolean; inline;
begin
  MOD_ALLSET := (val and mask) = mask;
end;

{$ENDIF}
