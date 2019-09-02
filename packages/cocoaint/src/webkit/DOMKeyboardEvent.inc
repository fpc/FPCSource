{ Parsed from WebKit.framework DOMKeyboardEvent.h }


{$ifdef TYPES}
type
  DOMKeyboardEventPtr = ^DOMKeyboardEvent;
{$endif}

{$ifdef TYPES}
const
  DOM_KEY_LOCATION_STANDARD = $00;
  DOM_KEY_LOCATION_LEFT = $01;
  DOM_KEY_LOCATION_RIGHT = $02;
  DOM_KEY_LOCATION_NUMPAD = $03;
{$endif}

{$ifdef CLASSES}

type
  DOMKeyboardEvent = objcclass external (DOMUIEvent)
  public
    function keyIdentifier: NSString; message 'keyIdentifier';
    function location: cuint; message 'location';
    function keyLocation: cuint; message 'keyLocation';
    function ctrlKey: ObjCBOOL; message 'ctrlKey';
    function shiftKey: ObjCBOOL; message 'shiftKey';
    function altKey: ObjCBOOL; message 'altKey';
    function metaKey: ObjCBOOL; message 'metaKey';
    function altGraphKey: ObjCBOOL; message 'altGraphKey';
    function keyCode: cint; message 'keyCode';
    function charCode: cint; message 'charCode';
    function getModifierState (keyIdentifierArg: NSString): ObjCBOOL; message 'getModifierState:';
    procedure initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKey (type_: NSString; canBubble: ObjCBOOL; cancelable_: ObjCBOOL; view_: DOMAbstractView; keyIdentifier_: NSString; location_: cuint; ctrlKey_: ObjCBOOL; altKey_: ObjCBOOL; shiftKey_: ObjCBOOL; metaKey_: ObjCBOOL; altGraphKey_: ObjCBOOL); message 'initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:'; { available in 10_8 }
    procedure initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey (type_: NSString; canBubble: ObjCBOOL; cancelable_: ObjCBOOL; view_: DOMAbstractView; keyIdentifier_: NSString; location_: cuint; ctrlKey_: ObjCBOOL; altKey_: ObjCBOOL; shiftKey_: ObjCBOOL; metaKey_: ObjCBOOL); message 'initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:'; { available in 10_8 }
    procedure initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKey (type_: NSString; canBubble: ObjCBOOL; cancelable_: ObjCBOOL; view_: DOMAbstractView; keyIdentifier_: NSString; keyLocation_: cuint; ctrlKey_: ObjCBOOL; altKey_: ObjCBOOL; shiftKey_: ObjCBOOL; metaKey_: ObjCBOOL; altGraphKey_: ObjCBOOL); message 'initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:'; deprecated 'in 10_5, 10_5';
    procedure initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey (type_: NSString; canBubble: ObjCBOOL; cancelable_: ObjCBOOL; view_: DOMAbstractView; keyIdentifier_: NSString; keyLocation_: cuint; ctrlKey_: ObjCBOOL; altKey_: ObjCBOOL; shiftKey_: ObjCBOOL; metaKey_: ObjCBOOL); message 'initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:'; deprecated 'in 10_5, 10_5';
  end;
{$endif}

