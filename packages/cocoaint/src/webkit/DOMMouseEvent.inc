{ Parsed from Webkit.framework DOMMouseEvent.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMMOUSEEVENT_PAS_C}
{$define DOMMOUSEEVENT_PAS_C}

{ DOMMouseEvent }
  DOMMouseEvent = objcclass(DOMUIEvent)
    
  public
    class function alloc: DOMMouseEvent; message 'alloc';

    function screenX: cint; message 'screenX';
    function screenY: cint; message 'screenY';
    function clientX: cint; message 'clientX';
    function clientY: cint; message 'clientY';
    function ctrlKey: Boolean; message 'ctrlKey';
    function shiftKey: Boolean; message 'shiftKey';
    function altKey: Boolean; message 'altKey';
    function metaKey: Boolean; message 'metaKey';
    function button: cushort; message 'button';
    function relatedTarget: id; message 'relatedTarget';
    procedure initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTarget(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; detail_: cint; screenX_: cint; screenY_: cint; clientX_: cint; clientY_: cint; ctrlKey_: Boolean; altKey_: Boolean; shiftKey_: Boolean; metaKey_: Boolean; button_: cushort; relatedTarget_: id); message 'initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:';

    { Category: DOMMouseEventDeprecated }
    procedure initMouseEvent(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; detail_: cint; screenX_: cint; screenY_: cint; clientX_: cint; clientY_: cint; ctrlKey_: Boolean; altKey_: Boolean; shiftKey_: Boolean; metaKey_: Boolean; button_: cushort; relatedTarget_: id); message 'initMouseEvent:type:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:';
  end; external;

{$endif}
{$endif}
