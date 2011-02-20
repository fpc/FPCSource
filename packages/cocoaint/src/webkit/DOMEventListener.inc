{ Parsed from Webkit.framework DOMEventListener.h }

{$ifdef TYPES}
{$ifndef DOMEVENTLISTENER_PAS_T}
{$define DOMEVENTLISTENER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMEVENTLISTENER_PAS_R}
{$define DOMEVENTLISTENER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMEVENTLISTENER_PAS_F}
{$define DOMEVENTLISTENER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMEVENTLISTENER_PAS_S}
{$define DOMEVENTLISTENER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMEventListenerProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef DOMEVENTLISTENER_PAS_P}
{$define DOMEVENTLISTENER_PAS_P}
  
{ DOMEventListener Protocol }
  DOMEventListenerProtocol = objcprotocol external name 'DOMEventListener'
    procedure handleEvent(evt: DOMEvent); message 'handleEvent:';
  end;
{$endif}
{$endif}
