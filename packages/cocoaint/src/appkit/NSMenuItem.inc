{ Parsed from Appkit.framework NSMenuItem.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSMENUITEM_PAS_H}
{$define NSMENUITEM_PAS_H}
type
  NSMenuItemPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSMENUITEM_PAS_T}
{$define NSMENUITEM_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMENUITEM_PAS_R}
{$define NSMENUITEM_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMENUITEM_PAS_F}
{$define NSMENUITEM_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMENUITEM_PAS_T}
{$define NSMENUITEM_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMenuItem = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMENUITEM_PAS_C}
{$define NSMENUITEM_PAS_C}

{ NSMenuItem }
  NSMenuItem = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol, NSValidatedUserInterfaceItemProtocol)
  private
    __menu: NSMenu;
    __title: NSString;
    __keyEquivalent: NSString;
    __keyEquivalentModifierMask: culong;
    __mnemonicLocation: clong;
    __state: clong;
    __image: NSImage;
    __onStateImage: NSImage;
    __offStateImage: NSImage;
    __mixedStateImage: NSImage;
    __target: id;
    __action: SEL;
    __tag: clong;
    __extraData: id;
    __miFlags: bitpacked record
        disabled: 0..1;
        isSeparator: 0..1;
        hidden: 0..1;
        alternate: 0..1;
        indent: 0..((1 shl 4)-1);
        changed: 0..((1 shl 16)-1);
        highlighted: 0..1;
        limitedView: 0..1;
        RESERVED: 0..((1 shl 6)-1);
      end;
    
  public
    class function alloc: NSMenuItem; message 'alloc';

    class procedure setUsesUserKeyEquivalents(flag: Boolean); message 'setUsesUserKeyEquivalents:';
    class function usesUserKeyEquivalents: Boolean; message 'usesUserKeyEquivalents';
    class function separatorItem: NSMenuItem; message 'separatorItem';
    function initWithTitle_action_keyEquivalent(aString: NSString; aSelector: SEL; charCode: NSString): id; message 'initWithTitle:action:keyEquivalent:';
    procedure setMenu(menu_: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    function hasSubmenu: Boolean; message 'hasSubmenu';
    procedure setSubmenu(submenu_: NSMenu); message 'setSubmenu:';
    function submenu: NSMenu; message 'submenu';
    procedure setTitle(aString: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setAttributedTitle(string_: NSAttributedString); message 'setAttributedTitle:';
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    function isSeparatorItem: Boolean; message 'isSeparatorItem';
    procedure setKeyEquivalent(aKeyEquivalent: NSString); message 'setKeyEquivalent:';
    function keyEquivalent: NSString; message 'keyEquivalent';
    procedure setKeyEquivalentModifierMask(mask: culong); message 'setKeyEquivalentModifierMask:';
    function keyEquivalentModifierMask: culong; message 'keyEquivalentModifierMask';
    function userKeyEquivalent: NSString; message 'userKeyEquivalent';
    procedure setMnemonicLocation(location: culong); message 'setMnemonicLocation:';
    function mnemonicLocation: culong; message 'mnemonicLocation';
    function mnemonic: NSString; message 'mnemonic';
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';
    procedure setImage(menuImage: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setState(state_: clong); message 'setState:';
    function state: clong; message 'state';
    procedure setOnStateImage(image_: NSImage); message 'setOnStateImage:';
    function onStateImage: NSImage; message 'onStateImage';
    procedure setOffStateImage(image_: NSImage); message 'setOffStateImage:';
    function offStateImage: NSImage; message 'offStateImage';
    procedure setMixedStateImage(image_: NSImage); message 'setMixedStateImage:';
    function mixedStateImage: NSImage; message 'mixedStateImage';
    procedure setEnabled(flag: Boolean); message 'setEnabled:';
    function isEnabled: Boolean; message 'isEnabled';
    procedure setAlternate(isAlternate_: Boolean); message 'setAlternate:';
    function isAlternate: Boolean; message 'isAlternate';
    procedure setIndentationLevel(indentationLevel_: clong); message 'setIndentationLevel:';
    function indentationLevel: clong; message 'indentationLevel';
    procedure setTarget(anObject: id); message 'setTarget:';
    function target: id; message 'target';
    procedure setAction(aSelector: SEL); message 'setAction:';
    function action: SEL; message 'action';
    procedure setTag(anInt: clong); message 'setTag:';
    function tag: clong; message 'tag';
    procedure setRepresentedObject(anObject: id); message 'setRepresentedObject:';
    function representedObject: id; message 'representedObject';
    procedure setView(view_: NSView); message 'setView:';
    function view: NSView; message 'view';
    function isHighlighted: Boolean; message 'isHighlighted';
    procedure setHidden(hidden: Boolean); message 'setHidden:';
    function isHidden: Boolean; message 'isHidden';
    function isHiddenOrHasHiddenAncestor: Boolean; message 'isHiddenOrHasHiddenAncestor';
    procedure setToolTip(toolTip_: NSString); message 'setToolTip:';
    function toolTip: NSString; message 'toolTip';
  end; external;

{$endif}
{$endif}
