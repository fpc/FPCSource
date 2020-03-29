{ Parsed from WebKit.framework DOMMouseEvent.h }


{$ifdef TYPES}
type
  DOMMouseEventPtr = ^DOMMouseEvent;
{$endif}

{$ifdef CLASSES}

type
  DOMMouseEvent = objcclass external (DOMUIEvent)
  public
    function screenX: cint; message 'screenX';
    function screenY: cint; message 'screenY';
    function clientX: cint; message 'clientX';
    function clientY: cint; message 'clientY';
    function ctrlKey: ObjCBOOL; message 'ctrlKey';
    function shiftKey: ObjCBOOL; message 'shiftKey';
    function altKey: ObjCBOOL; message 'altKey';
    function metaKey: ObjCBOOL; message 'metaKey';
    function button: cushort; message 'button';
    function relatedTarget: DOMEventTargetProtocol; message 'relatedTarget';
    function offsetX: cint; message 'offsetX';
    function offsetY: cint; message 'offsetY';
    function x: cint; message 'x';
    function y: cint; message 'y';
    function fromElement: DOMNode; message 'fromElement';
    function toElement: DOMNode; message 'toElement';
    procedure initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTarget (type_: NSString; canBubble: ObjCBOOL; cancelable_: ObjCBOOL; view_: DOMAbstractView; detail_: cint; screenX_: cint; screenY_: cint; clientX_: cint; clientY_: cint; ctrlKey_: ObjCBOOL; altKey_: ObjCBOOL; shiftKey_: ObjCBOOL; metaKey_: ObjCBOOL; button_: cushort; relatedTarget_: DOMEventTargetProtocol); message 'initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:'; { available in 10_5 }
  end;


type
  DOMMouseEventDeprecated = objccategory external (DOMMouseEvent)
    procedure initMouseEvent_canBubble_view_screenX_clientX_ctrlKey_shiftKey_button (type_: NSString; cancelable_: ObjCBOOL; detail_: cint; screenY_: cint; clientY_: cint; altKey_: ObjCBOOL; metaKey_: ObjCBOOL; relatedTarget_: DOMEventTargetProtocol); message 'initMouseEvent:canBubble:view:screenX:clientX:ctrlKey:shiftKey:button:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

