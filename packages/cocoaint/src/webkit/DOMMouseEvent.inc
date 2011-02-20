{ Parsed from Webkit.framework DOMMouseEvent.h }

{$ifdef TYPES}
{$ifndef DOMMOUSEEVENT_PAS_T}
{$define DOMMOUSEEVENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMMOUSEEVENT_PAS_R}
{$define DOMMOUSEEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMMOUSEEVENT_PAS_F}
{$define DOMMOUSEEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMMOUSEEVENT_PAS_S}
{$define DOMMOUSEEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMMouseEvent = objcclass;
  DOMMouseEventPointer = ^DOMMouseEvent;
  DOMMouseEventPtr = DOMMouseEventPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMMOUSEEVENT_PAS_C}
{$define DOMMOUSEEVENT_PAS_C}

{ DOMMouseEvent }
  DOMMouseEvent = objcclass external (DOMUIEvent)
    
  public
    function screenX: cint; message 'screenX';
    function screenY: cint; message 'screenY';
    function clientX: cint; message 'clientX';
    function clientY: cint; message 'clientY';
    function ctrlKey: Boolean; message 'ctrlKey';
    function shiftKey: Boolean; message 'shiftKey';
    function altKey: Boolean; message 'altKey';
    function metaKey: Boolean; message 'metaKey';
    function button: cuint; message 'button';
    function relatedTarget: id; message 'relatedTarget';
    function offsetX: cint; message 'offsetX';
    function offsetY: cint; message 'offsetY';
    function x: cint; message 'x';
    function y: cint; message 'y';
    function fromElement: DOMNode; message 'fromElement';
    function toElement: DOMNode; message 'toElement';
    procedure initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTarget(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; detail_: cint; screenX_: cint; screenY_: cint; clientX_: cint; clientY_: cint; ctrlKey_: Boolean; altKey_: Boolean; shiftKey_: Boolean; metaKey_: Boolean; button_: cushort; relatedTarget_: DOMEventTargetProtocol); message 'initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:';
  end;

{ DOMMouseEventDeprecatedCategory }
  DOMMouseEventDeprecatedCategory = objccategory external (DOMMouseEvent)
    procedure initMouseEvent(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; detail_: cint; screenX_: cint; screenY_: cint; clientX_: cint; clientY_: cint; ctrlKey_: Boolean; altKey_: Boolean; shiftKey_: Boolean; metaKey_: Boolean; button_: cushort; relatedTarget_: DOMEventTargetProtocol); message 'initMouseEvent:type:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
