{ Parsed from Webkit.framework DOMKeyboardEvent.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMKEYBOARDEVENT_PAS_C}
{$define DOMKEYBOARDEVENT_PAS_C}

{ DOMKeyboardEvent }
  DOMKeyboardEvent = objcclass(DOMUIEvent)
    
  public
    class function alloc: DOMKeyboardEvent; message 'alloc';

    function keyIdentifier: NSString; message 'keyIdentifier';
    function keyLocation: cuint; message 'keyLocation';
    function ctrlKey: Boolean; message 'ctrlKey';
    function shiftKey: Boolean; message 'shiftKey';
    function altKey: Boolean; message 'altKey';
    function metaKey: Boolean; message 'metaKey';
    function keyCode: cint; message 'keyCode';
    function charCode: cint; message 'charCode';
    function getModifierState(keyIdentifierArg: NSStringPointer): Boolean; message 'getModifierState:';
  end; external;

{$endif}
{$endif}
