{ Parsed from Webkit.framework DOMWheelEvent.h }

{$ifdef TYPES}
{$ifndef DOMWHEELEVENT_PAS_T}
{$define DOMWHEELEVENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMWHEELEVENT_PAS_R}
{$define DOMWHEELEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMWHEELEVENT_PAS_F}
{$define DOMWHEELEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMWHEELEVENT_PAS_S}
{$define DOMWHEELEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMWheelEvent = objcclass;
  DOMWheelEventPointer = ^DOMWheelEvent;
  DOMWheelEventPtr = DOMWheelEventPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMWHEELEVENT_PAS_C}
{$define DOMWHEELEVENT_PAS_C}

{ DOMWheelEvent }
  DOMWheelEvent = objcclass external (DOMUIEvent)
    
  public
    function screenX: cint; message 'screenX';
    function screenY: cint; message 'screenY';
    function clientX: cint; message 'clientX';
    function clientY: cint; message 'clientY';
    function ctrlKey: Boolean; message 'ctrlKey';
    function shiftKey: Boolean; message 'shiftKey';
    function altKey: Boolean; message 'altKey';
    function metaKey: Boolean; message 'metaKey';
    function wheelDelta: cint; message 'wheelDelta';
    function wheelDeltaX: cint; message 'wheelDeltaX';
    function wheelDeltaY: cint; message 'wheelDeltaY';
    function offsetX: cint; message 'offsetX';
    function offsetY: cint; message 'offsetY';
    function x: cint; message 'x';
    function y: cint; message 'y';
    function isHorizontal: Boolean; message 'isHorizontal';
    procedure initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey(wheelDeltaX_: cint; wheelDeltaY_: cint; view_: DOMAbstractView; screenX_: cint; screenY_: cint; clientX_: cint; clientY_: cint; ctrlKey_: Boolean; altKey_: Boolean; shiftKey_: Boolean; metaKey_: Boolean); message 'initWheelEvent:wheelDeltaY:view:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:';
  end;

{$endif}
{$endif}
