{ Parsed from Appkit.framework NSButton.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSBUTTON_PAS_H}
{$define NSBUTTON_PAS_H}
type
  NSButtonPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSBUTTON_PAS_T}
{$define NSBUTTON_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBUTTON_PAS_R}
{$define NSBUTTON_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSBUTTON_PAS_F}
{$define NSBUTTON_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSBUTTON_PAS_T}
{$define NSBUTTON_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSButton = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBUTTON_PAS_C}
{$define NSBUTTON_PAS_C}

{ NSButton }
  NSButton = objcclass(NSControl, NSUserInterfaceValidationsProtocol)
    
  public
    class function alloc: NSButton; message 'alloc';

    function title: NSString; message 'title';
    procedure setTitle(aString: NSString); message 'setTitle:';
    function alternateTitle: NSString; message 'alternateTitle';
    procedure setAlternateTitle(aString: NSString); message 'setAlternateTitle:';
    function image: NSImage; message 'image';
    procedure setImage(image_: NSImage); message 'setImage:';
    function alternateImage: NSImage; message 'alternateImage';
    procedure setAlternateImage(image_: NSImage); message 'setAlternateImage:';
    function imagePosition: NSCellImagePosition; message 'imagePosition';
    procedure setImagePosition(aPosition: NSCellImagePosition); message 'setImagePosition:';
    procedure setButtonType(aType: NSButtonType); message 'setButtonType:';
    function state: clong; message 'state';
    procedure setState(value: clong); message 'setState:';
    function isBordered: Boolean; message 'isBordered';
    procedure setBordered(flag: Boolean); message 'setBordered:';
    function isTransparent: Boolean; message 'isTransparent';
    procedure setTransparent(flag: Boolean); message 'setTransparent:';
    procedure setPeriodicDelay_interval(delay: single; interval: single); message 'setPeriodicDelay:interval:';
    procedure getPeriodicDelay_interval(var delay: single; var interval: single); message 'getPeriodicDelay:interval:';
    function keyEquivalent: NSString; message 'keyEquivalent';
    procedure setKeyEquivalent(charCode: NSString); message 'setKeyEquivalent:';
    function keyEquivalentModifierMask: culong; message 'keyEquivalentModifierMask';
    procedure setKeyEquivalentModifierMask(mask: culong); message 'setKeyEquivalentModifierMask:';
    procedure highlight(flag: Boolean); message 'highlight:';
    function performKeyEquivalent(key: NSEvent): Boolean; message 'performKeyEquivalent:';

    { Category: NSKeyboardUI }
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';

    { Category: NSButtonAttributedStringMethods }
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setAttributedTitle(aString: NSAttributedString); message 'setAttributedTitle:';
    function attributedAlternateTitle: NSAttributedString; message 'attributedAlternateTitle';
    procedure setAttributedAlternateTitle(obj: NSAttributedString); message 'setAttributedAlternateTitle:';

    { Category: NSButtonBezelStyles }
    procedure setBezelStyle(bezelStyle_: NSBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSBezelStyle; message 'bezelStyle';

    { Category: NSButtonMixedState }
    procedure setAllowsMixedState(flag: Boolean); message 'setAllowsMixedState:';
    function allowsMixedState: Boolean; message 'allowsMixedState';
    procedure setNextState; message 'setNextState';

    { Category: NSButtonBorder }
    procedure setShowsBorderOnlyWhileMouseInside(show: Boolean); message 'setShowsBorderOnlyWhileMouseInside:';
    function showsBorderOnlyWhileMouseInside: Boolean; message 'showsBorderOnlyWhileMouseInside';

    { Category: NSButtonSoundExtensions }
    procedure setSound(aSound: NSSound); message 'setSound:';
    function sound: NSSound; message 'sound';
  end; external;

{$endif}
{$endif}
