{ Parsed from Appkit.framework NSButton.h }

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
{$ifndef NSBUTTON_PAS_S}
{$define NSBUTTON_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSButton = objcclass;
  NSButtonPointer = ^NSButton;
  NSButtonPtr = NSButtonPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBUTTON_PAS_C}
{$define NSBUTTON_PAS_C}

{ NSButton }
  NSButton = objcclass external (NSControl, NSUserInterfaceValidationsProtocol)
    
  public
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
    function state: NSInteger; message 'state';
    procedure setState(value: NSInteger); message 'setState:';
    function isBordered: Boolean; message 'isBordered';
    procedure setBordered(flag: Boolean); message 'setBordered:';
    function isTransparent: Boolean; message 'isTransparent';
    procedure setTransparent(flag: Boolean); message 'setTransparent:';
    procedure setPeriodicDelay_interval(delay: single; interval: single); message 'setPeriodicDelay:interval:';
    procedure getPeriodicDelay_interval(delay: psingle; interval: psingle); message 'getPeriodicDelay:interval:';
    function keyEquivalent: NSString; message 'keyEquivalent';
    procedure setKeyEquivalent(charCode: NSString); message 'setKeyEquivalent:';
    function keyEquivalentModifierMask: NSUInteger; message 'keyEquivalentModifierMask';
    procedure setKeyEquivalentModifierMask(mask: NSUInteger); message 'setKeyEquivalentModifierMask:';
    procedure highlight(flag: Boolean); message 'highlight:';
    function performKeyEquivalent(key: NSEvent): Boolean; message 'performKeyEquivalent:';

    { Adopted Protocols }
    function validateUserInterfaceItem(anItem: NSValidatedUserInterfaceItemProtocol): Boolean;
  end;

{ NSKeyboardUI_NSButtonCategory }
  NSKeyboardUI_NSButtonCategory = objccategory external name 'NSKeyboardUI' (NSButton)
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';
  end;

{ NSButtonAttributedStringMethodsCategory }
  NSButtonAttributedStringMethodsCategory = objccategory external (NSButton)
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setAttributedTitle(aString: NSAttributedString); message 'setAttributedTitle:';
    function attributedAlternateTitle: NSAttributedString; message 'attributedAlternateTitle';
    procedure setAttributedAlternateTitle(obj: NSAttributedString); message 'setAttributedAlternateTitle:';
  end;

{ NSButtonBezelStylesCategory }
  NSButtonBezelStylesCategory = objccategory external (NSButton)
    procedure setBezelStyle(bezelStyle_: NSBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSBezelStyle; message 'bezelStyle';
  end;

{ NSButtonMixedStateCategory }
  NSButtonMixedStateCategory = objccategory external (NSButton)
    procedure setAllowsMixedState(flag: Boolean); message 'setAllowsMixedState:';
    function allowsMixedState: Boolean; message 'allowsMixedState';
    procedure setNextState; message 'setNextState';
  end;

{ NSButtonBorderCategory }
  NSButtonBorderCategory = objccategory external (NSButton)
    procedure setShowsBorderOnlyWhileMouseInside(show: Boolean); message 'setShowsBorderOnlyWhileMouseInside:';
    function showsBorderOnlyWhileMouseInside: Boolean; message 'showsBorderOnlyWhileMouseInside';
  end;

{ NSButtonSoundExtensionsCategory }
  NSButtonSoundExtensionsCategory = objccategory external (NSButton)
    procedure setSound(aSound: NSSound); message 'setSound:';
    function sound: NSSound; message 'sound';
  end;

{$endif}
{$endif}
