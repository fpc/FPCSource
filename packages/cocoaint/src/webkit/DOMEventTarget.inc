{ Parsed from Webkit.framework DOMEventTarget.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMEventTargetProtocol = objcprotocol
    procedure addEventListener_listener_useCapture(type_: NSString; listener: id; useCapture: Boolean); message 'addEventListener:listener:useCapture:';
    procedure removeEventListener_listener_useCapture(type_: NSString; listener: id; useCapture: Boolean); message 'removeEventListener:listener:useCapture:';
    function dispatchEvent(event: DOMEvent): Boolean; message 'dispatchEvent:';
    procedure addEventListener(type_: NSString; listener: id; useCapture: Boolean); message 'addEventListener:type:listener:';
    procedure removeEventListener(type_: NSString; listener: id; useCapture: Boolean); message 'removeEventListener:type:listener:';
  end; external name 'DOMEventTarget';
{$endif}
{$endif}
