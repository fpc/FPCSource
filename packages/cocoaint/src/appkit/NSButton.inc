{ Parsed from AppKit.framework NSButton.h }


{$ifdef TYPES}
type
  NSButtonPtr = ^NSButton;
{$endif}

{$ifdef CLASSES}

type
  NSButton = objcclass external (NSControl, NSUserInterfaceValidationsProtocol, NSAccessibilityButtonProtocol)
  public
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setAlternateTitle(newValue: NSString); message 'setAlternateTitle:';
    function alternateTitle: NSString; message 'alternateTitle';
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setAlternateImage(newValue: NSImage); message 'setAlternateImage:';
    function alternateImage: NSImage; message 'alternateImage';
    procedure setImagePosition(newValue: NSCellImagePosition); message 'setImagePosition:';
    function imagePosition: NSCellImagePosition; message 'imagePosition';
    procedure setButtonType (aType: NSButtonType); message 'setButtonType:';
    procedure setState(newValue: NSInteger); message 'setState:';
    function state: NSInteger; message 'state';
    procedure setBordered(newValue: ObjCBOOL); message 'setBordered:';
    function isBordered: ObjCBOOL; message 'isBordered';
    procedure setTransparent(newValue: ObjCBOOL); message 'setTransparent:';
    function isTransparent: ObjCBOOL; message 'isTransparent';
    procedure setPeriodicDelay_interval (delay: single; interval: single); message 'setPeriodicDelay:interval:';
    procedure getPeriodicDelay_interval (delay: psingle; interval: psingle); message 'getPeriodicDelay:interval:';
    procedure setKeyEquivalent(newValue: NSString); message 'setKeyEquivalent:';
    function keyEquivalent: NSString; message 'keyEquivalent';
    procedure setKeyEquivalentModifierMask(newValue: NSUInteger); message 'setKeyEquivalentModifierMask:';
    function keyEquivalentModifierMask: NSUInteger; message 'keyEquivalentModifierMask';
    procedure highlight (flag: ObjCBOOL); message 'highlight:';
    function performKeyEquivalent (key: NSEvent): ObjCBOOL; message 'performKeyEquivalent:';

    { Adopted protocols }
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
  end;


type
  NSButtonAttributedStringMethods = objccategory external (NSButton)
    procedure setAttributedTitle(newValue: NSAttributedString); message 'setAttributedTitle:';
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setAttributedAlternateTitle(newValue: NSAttributedString); message 'setAttributedAlternateTitle:';
    function attributedAlternateTitle: NSAttributedString; message 'attributedAlternateTitle';
  end;

type
  NSButtonBezelStyles = objccategory external (NSButton)
    procedure setBezelStyle(newValue: NSBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSBezelStyle; message 'bezelStyle';
  end;

type
  NSButtonMixedState = objccategory external (NSButton)
    procedure setAllowsMixedState(newValue: ObjCBOOL); message 'setAllowsMixedState:';
    function allowsMixedState: ObjCBOOL; message 'allowsMixedState';
    procedure setNextState; message 'setNextState';
  end;

type
  NSButtonBorder = objccategory external (NSButton)
    procedure setShowsBorderOnlyWhileMouseInside(newValue: ObjCBOOL); message 'setShowsBorderOnlyWhileMouseInside:';
    function showsBorderOnlyWhileMouseInside: ObjCBOOL; message 'showsBorderOnlyWhileMouseInside';
  end;

type
  NSButtonSoundExtensions = objccategory external (NSButton)
    procedure setSound(newValue: NSSound); message 'setSound:';
    function sound: NSSound; message 'sound';
  end;

type
  NSButton_NSKeyboardUI = objccategory external name 'NSKeyboardUI' (NSButton)
    procedure setTitleWithMnemonic (stringWithAmpersand: NSString); message 'setTitleWithMnemonic:'; deprecated 'in 10_0, 10_8';
  end;
{$endif}

