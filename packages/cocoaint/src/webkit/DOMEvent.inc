{ Parsed from Webkit.framework DOMEvent.h }

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
  DOMEventPtr = DOMEventPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMEVENT_PAS_C}
{$define DOMEVENT_PAS_C}

{ DOMEvent }
  DOMEvent = objcclass external (DOMObject)
    
  public
    function type_: NSString; message 'type_';
    function target: id; message 'target';
    function currentTarget: id; message 'currentTarget';
    function eventPhase: cuint; message 'eventPhase';
    function bubbles: ObjCBOOL; message 'bubbles';
    function cancelable: ObjCBOOL; message 'cancelable';
    function timeStamp: DOMTimeStamp; message 'timeStamp';
    function srcElement: id; message 'srcElement';
    procedure setReturnValue (newValue: ObjCBOOL); message 'setReturnValue:';
    function returnValue: ObjCBOOL; message 'returnValue';
    procedure setCancelBubble (newValue: ObjCBOOL); message 'setCancelBubble:';
    function cancelBubble: ObjCBOOL; message 'cancelBubble';
    procedure stopPropagation; message 'stopPropagation';
    procedure preventDefault; message 'preventDefault';
    procedure initEvent_canBubbleArg_cancelableArg(eventTypeArg: NSString; canBubbleArg: ObjCBOOL; cancelableArg: ObjCBOOL); message 'initEvent:canBubbleArg:cancelableArg:';
  end;

{ DOMEventDeprecatedCategory }
  DOMEventDeprecatedCategory = objccategory external (DOMEvent)
    procedure initEvent(eventTypeArg: NSString; canBubbleArg: ObjCBOOL; cancelableArg: ObjCBOOL); message 'initEvent:eventTypeArg:canBubbleArg:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
