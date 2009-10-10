{ Parsed from Webkit.framework DOMEvent.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMEVENT_PAS_T}
{$define DOMEVENT_PAS_T}

{ Constants }

const
  DOM_CAPTURING_PHASE = 1;
  DOM_AT_TARGET = 2;
  DOM_BUBBLING_PHASE = 3;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMEVENT_PAS_R}
{$define DOMEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMEVENT_PAS_F}
{$define DOMEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMEVENT_PAS_S}
{$define DOMEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMEvent = objcclass;
  DOMEventPointer = ^DOMEvent;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMEVENT_PAS_C}
{$define DOMEVENT_PAS_C}

{ DOMEvent }
  DOMEvent = objcclass(DOMObject)
    
  public
    class function alloc: DOMEvent; message 'alloc';

    function type_: NSString; message 'type_';
    function target: id; message 'target';
    function currentTarget: id; message 'currentTarget';
    function eventPhase: cushort; message 'eventPhase';
    function bubbles: Boolean; message 'bubbles';
    function cancelable: Boolean; message 'cancelable';
    function timeStamp: DOMTimeStamp; message 'timeStamp';
    procedure stopPropagation; message 'stopPropagation';
    procedure preventDefault; message 'preventDefault';
    procedure initEvent_canBubbleArg_cancelableArg(eventTypeArg: NSString; canBubbleArg: Boolean; cancelableArg: Boolean); message 'initEvent:canBubbleArg:cancelableArg:';

    { Category: DOMEventDeprecated }
    procedure initEvent(eventTypeArg: NSString; canBubbleArg: Boolean; cancelableArg: Boolean); message 'initEvent:eventTypeArg:canBubbleArg:';
  end; external;

{$endif}
{$endif}
