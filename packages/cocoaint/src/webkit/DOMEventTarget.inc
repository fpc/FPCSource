{ Parsed from Webkit.framework DOMEventTarget.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:41 CET 2011 }


{$ifdef TYPES}
{$ifndef DOMEVENTTARGET_PAS_T}
{$define DOMEVENTTARGET_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMEVENTTARGET_PAS_R}
{$define DOMEVENTTARGET_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMEVENTTARGET_PAS_F}
{$define DOMEVENTTARGET_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMEVENTTARGET_PAS_S}
{$define DOMEVENTTARGET_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMEventTargetProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef DOMEVENTTARGET_PAS_P}
{$define DOMEVENTTARGET_PAS_P}
  
{ DOMEventTarget Protocol }
  DOMEventTargetProtocol = objcprotocol external name 'DOMEventTarget'
    procedure addEventListener_listener_useCapture(type_: NSString; listener: id; useCapture: Boolean); message 'addEventListener:listener:useCapture:';
    procedure removeEventListener_listener_useCapture(type_: NSString; listener: id; useCapture: Boolean); message 'removeEventListener:listener:useCapture:';
    function dispatchEvent(event: DOMEvent): Boolean; message 'dispatchEvent:';
    procedure addEventListener(type_: NSString; listener: id; useCapture: Boolean); message 'addEventListener:type:listener:';
    procedure removeEventListener(type_: NSString; listener: id; useCapture: Boolean); message 'removeEventListener:type:listener:';
  end;
{$endif}
{$endif}
