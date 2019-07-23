{ Parsed from Webkit.framework DOMKeyboardEvent.h }

{$ifdef TYPES}
{$ifndef DOMKEYBOARDEVENT_PAS_T}
{$define DOMKEYBOARDEVENT_PAS_T}

{ Constants }

const
  DOM_KEY_LOCATION_STANDARD = $00;
  DOM_KEY_LOCATION_LEFT = $01;
  DOM_KEY_LOCATION_RIGHT = $02;
  DOM_KEY_LOCATION_NUMPAD = $03;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMKEYBOARDEVENT_PAS_R}
{$define DOMKEYBOARDEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMKEYBOARDEVENT_PAS_F}
{$define DOMKEYBOARDEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMKEYBOARDEVENT_PAS_S}
{$define DOMKEYBOARDEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMKeyboardEvent = objcclass;
  DOMKeyboardEventPointer = ^DOMKeyboardEvent;
  DOMKeyboardEventPtr = DOMKeyboardEventPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMKEYBOARDEVENT_PAS_C}
{$define DOMKEYBOARDEVENT_PAS_C}

{ DOMKeyboardEvent }
  DOMKeyboardEvent = objcclass external (DOMUIEvent)
    
  public
    function keyIdentifier: NSString; message 'keyIdentifier';
    function keyLocation: cuint; message 'keyLocation';
    function ctrlKey: ObjCBOOL; message 'ctrlKey';
    function shiftKey: ObjCBOOL; message 'shiftKey';
    function altKey: ObjCBOOL; message 'altKey';
    function metaKey: ObjCBOOL; message 'metaKey';
    function altGraphKey: ObjCBOOL; message 'altGraphKey';
    function keyCode: cint; message 'keyCode';
    function charCode: cint; message 'charCode';
    function getModifierState(keyIdentifierArg: NSString): ObjCBOOL; message 'getModifierState:';
    procedure initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKey(type__: NSString; canBubble: ObjCBOOL; cancelable_: ObjCBOOL; view_: DOMAbstractView; keyIdentifier_: NSString; keyLocation_: cuint; ctrlKey_: ObjCBOOL; altKey_: ObjCBOOL; shiftKey_: ObjCBOOL; metaKey_: ObjCBOOL; altGraphKey_: ObjCBOOL); message 'initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:';
    procedure initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey(type__: NSString; canBubble: ObjCBOOL; cancelable_: ObjCBOOL; view_: DOMAbstractView; keyIdentifier_: NSString; keyLocation_: cuint; ctrlKey_: ObjCBOOL; altKey_: ObjCBOOL; shiftKey_: ObjCBOOL; metaKey_: ObjCBOOL); message 'initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:';
  end;

{$endif}
{$endif}
