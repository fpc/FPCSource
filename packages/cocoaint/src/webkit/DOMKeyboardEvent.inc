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
    function ctrlKey: Boolean; message 'ctrlKey';
    function shiftKey: Boolean; message 'shiftKey';
    function altKey: Boolean; message 'altKey';
    function metaKey: Boolean; message 'metaKey';
    function altGraphKey: Boolean; message 'altGraphKey';
    function keyCode: cint; message 'keyCode';
    function charCode: cint; message 'charCode';
    function getModifierState(keyIdentifierArg: NSString): Boolean; message 'getModifierState:';
    procedure initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKey(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; keyIdentifier_: NSString; keyLocation_: cuint; ctrlKey_: Boolean; altKey_: Boolean; shiftKey_: Boolean; metaKey_: Boolean; altGraphKey_: Boolean); message 'initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:';
    procedure initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; keyIdentifier_: NSString; keyLocation_: cuint; ctrlKey_: Boolean; altKey_: Boolean; shiftKey_: Boolean; metaKey_: Boolean); message 'initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:';
  end;

{$endif}
{$endif}
