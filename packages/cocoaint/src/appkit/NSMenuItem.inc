{ Parsed from AppKit.framework NSMenuItem.h }


{$ifdef TYPES}
type
  NSMenuItemPtr = ^NSMenuItem;
{$endif}

{$ifdef CLASSES}

type
  NSMenuItem = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol, NSValidatedUserInterfaceItemProtocol)
  private
    _menu: NSMenu;
    _title: NSString;
    _keyEquivalent: NSString;
    {$if defined(__LP64__)}
    _uiid: NSString;
    _repObject: id;
    _submenu: NSMenu;
    _extraData: id;
    _target: id;
    _action: SEL;
    _tag: NSInteger;
    _keyEquivalentModifierMask: char;
    {$else}
    _keyEquivalentModifierMask_: NSUInteger;
    _userKEGenerationCount: NSInteger;
    _state: NSInteger;
    _image: NSImage;
    _submenu_: NSMenu;
    _uiid_: NSString;
    _repObject_: id;
    _target_: id;
    _action_: SEL;
    _tag_: NSInteger;
    _extraData_: id;
    {$endif}
    __miFlags: bitpacked record
      case byte of
        0: (_anonBitField___miFlags0: cuint);
        1: (
          keGenerationCount: 0..((1 shl 8)-1);
          disabled: 0..1;
          isSeparator: 0..1;
          hidden: 0..1;
          alternate: 0..1;
          moreAlternate: 0..1;
          singleAlternate: 0..1;
          indent: 0..((1 shl 4)-1);
          keShareMode: 0..((1 shl 3)-1);
          state: 0..((1 shl 2)-1);
          RESERVED1: 0..((1 shl 2)-1);
          limitedView: 0..1;
          nextItemIsAlternate: 0..1;
          blockKE: 0..1;
          ignoredForAccessibility: 0..1;
          hiddenActiveKE: 0..1;
          noRepeatKEs: 0..1;
          targetWeak: 0..1;
        );
      end;
  public
    class procedure setUsesUserKeyEquivalents (flag: ObjCBOOL); message 'setUsesUserKeyEquivalents:';
    class function usesUserKeyEquivalents: ObjCBOOL; message 'usesUserKeyEquivalents';
    class function separatorItem: NSMenuItem; message 'separatorItem';
    function initWithTitle_action_keyEquivalent (aString: NSString; aSelector: SEL; charCode: NSString): instancetype; message 'initWithTitle:action:keyEquivalent:';
    procedure setMenu(newValue: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    function hasSubmenu: ObjCBOOL; message 'hasSubmenu';
    procedure setSubmenu(newValue: NSMenu); message 'setSubmenu:';
    function submenu: NSMenu; message 'submenu';
    function parentItem: NSMenuItem; message 'parentItem';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setAttributedTitle(newValue: NSAttributedString); message 'setAttributedTitle:';
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    function isSeparatorItem: ObjCBOOL; message 'isSeparatorItem';
    procedure setKeyEquivalent(newValue: NSString); message 'setKeyEquivalent:';
    function keyEquivalent: NSString; message 'keyEquivalent';
    procedure setKeyEquivalentModifierMask(newValue: NSUInteger); message 'setKeyEquivalentModifierMask:';
    function keyEquivalentModifierMask: NSUInteger; message 'keyEquivalentModifierMask';
    function userKeyEquivalent: NSString; message 'userKeyEquivalent';
    procedure setTitleWithMnemonic (stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setState(newValue: NSInteger); message 'setState:';
    function state: NSInteger; message 'state';
    procedure setOnStateImage(newValue: NSImage); message 'setOnStateImage:';
    function onStateImage: NSImage; message 'onStateImage';
    procedure setOffStateImage(newValue: NSImage); message 'setOffStateImage:';
    function offStateImage: NSImage; message 'offStateImage';
    procedure setMixedStateImage(newValue: NSImage); message 'setMixedStateImage:';
    function mixedStateImage: NSImage; message 'mixedStateImage';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure setAlternate(newValue: ObjCBOOL); message 'setAlternate:';
    function isAlternate: ObjCBOOL; message 'isAlternate';
    procedure setIndentationLevel(newValue: NSInteger); message 'setIndentationLevel:';
    function indentationLevel: NSInteger; message 'indentationLevel';
    procedure setTarget(newValue: id); message 'setTarget:';
    function target: id; message 'target';
    procedure setAction(newValue: SEL); message 'setAction:';
    function action: SEL; message 'action';
    procedure setTag(newValue: NSInteger); message 'setTag:';
    function tag: NSInteger; message 'tag';
    procedure setRepresentedObject(newValue: id); message 'setRepresentedObject:';
    function representedObject: id; message 'representedObject';
    procedure setView(newValue: NSView); message 'setView:';
    function view: NSView; message 'view';
    function isHighlighted: ObjCBOOL; message 'isHighlighted';
    procedure setHidden(newValue: ObjCBOOL); message 'setHidden:';
    function isHidden: ObjCBOOL; message 'isHidden';
    function isHiddenOrHasHiddenAncestor: ObjCBOOL; message 'isHiddenOrHasHiddenAncestor';
    procedure setToolTip(newValue: NSString); message 'setToolTip:';
    function toolTip: NSString; message 'toolTip';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSViewEnclosingMenuItem = objccategory external (NSView)
    function enclosingMenuItem: NSMenuItem; message 'enclosingMenuItem';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSMenuItem_NSDeprecated = objccategory external name 'NSDeprecated' (NSMenuItem)
    procedure setMnemonicLocation (location: NSUInteger); message 'setMnemonicLocation:'; deprecated 'in 10_0, 10_6';
    function mnemonicLocation: NSUInteger; message 'mnemonicLocation'; deprecated 'in 10_0, 10_6';
    function mnemonic: NSString; message 'mnemonic'; deprecated 'in 10_0, 10_6';
  end;
{$endif}

