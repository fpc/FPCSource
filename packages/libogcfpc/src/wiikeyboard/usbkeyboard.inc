{$IFDEF OGC_INTERFACE}
const
  USBKEYBOARD_PRESSED = 0;
  USBKEYBOARD_RELEASED = 1;
  USBKEYBOARD_DISCONNECTED = 2;
type
  USBKeyboard_eventType = Integer;


const
  USBKEYBOARD_LEDNUM = 0;
  USBKEYBOARD_LEDCAPS = 1;
  USBKEYBOARD_LEDSCROLL = 2;
type
  USBKeyboard_led = Integer;


  USBKeyboard_event = packed record
    type_ : USBKeyboard_eventType;
    keyCode : cuint8;
  end;


  eventcallback = procedure(event: USBKeyboard_event); cdecl;


function USBKeyboard_Initialize: cint32; cdecl; external;

function USBKeyboard_Deinitialize: cint32; cdecl; external;


function USBKeyboard_Open(cb: eventcallback): cint32; cdecl; external;

procedure USBKeyboard_Close; cdecl; external;


function USBKeyboard_IsConnected: cbool; cdecl; external;

function USBKeyboard_Scan: cint32; cdecl; external;


function USBKeyboard_SetLed(led: USBKeyboard_led; on_: cbool): cint32; cdecl; external;

function USBKeyboard_ToggleLed(led: USBKeyboard_led): cint32; cdecl; external;
{$ENDIF}

