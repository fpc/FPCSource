{ Parsed from WebKit.framework DOMEvent.h }


{$ifdef TYPES}
type
  DOMEventPtr = ^DOMEvent;
{$endif}

{$ifdef TYPES}
const
  DOM_NONE = 0;
  DOM_CAPTURING_PHASE = 1;
  DOM_AT_TARGET = 2;
  DOM_BUBBLING_PHASE = 3;
{$endif}

{$ifdef CLASSES}

type
  DOMEvent = objcclass external (DOMObject)
  public
    function type_: NSString; message 'type';
    function target: DOMEventTargetProtocol; message 'target';
    function currentTarget: DOMEventTargetProtocol; message 'currentTarget';
    function eventPhase: cushort; message 'eventPhase';
    function bubbles: ObjCBOOL; message 'bubbles';
    function cancelable: ObjCBOOL; message 'cancelable';
    function timeStamp: DOMTimeStamp; message 'timeStamp';
    function srcElement: DOMEventTargetProtocol; message 'srcElement';
    procedure setReturnValue(newValue: ObjCBOOL); message 'setReturnValue:';
    function returnValue: ObjCBOOL; message 'returnValue';
    procedure setCancelBubble(newValue: ObjCBOOL); message 'setCancelBubble:';
    function cancelBubble: ObjCBOOL; message 'cancelBubble';
    procedure stopPropagation; message 'stopPropagation';
    procedure preventDefault; message 'preventDefault';
    procedure initEvent_canBubbleArg_cancelableArg (eventTypeArg: NSString; canBubbleArg: ObjCBOOL; cancelableArg: ObjCBOOL); message 'initEvent:canBubbleArg:cancelableArg:'; { available in 10_5 }
  end;


type
  DOMEventDeprecated = objccategory external (DOMEvent)
    procedure initEvent_canBubbleArg (eventTypeArg: NSString; cancelableArg: ObjCBOOL); message 'initEvent:canBubbleArg:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

