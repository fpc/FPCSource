{ Parsed from WebKit.framework DOMEventListener.h }


{$ifdef TYPES}
type
  DOMEventListenerProtocolPtr = ^DOMEventListenerProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  DOMEventListenerProtocol = objcprotocol external name 'DOMEventListener' (NSObjectProtocol)
    procedure handleEvent (evt: DOMEvent); message 'handleEvent:';
  end;
{$endif}

