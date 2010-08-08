{ Parsed from Webkit.framework DOMEventListener.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
  DOMEventListenerProtocol = objcprotocol
    procedure handleEvent(evt: DOMEvent); message 'handleEvent:';
  end; external name 'DOMEventListener';
{$endif}
{$endif}
