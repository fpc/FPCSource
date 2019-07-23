{ Parsed from Appkit.framework NSTextField.h }

{$ifdef TYPES}
{$ifndef NSTEXTFIELD_PAS_T}
{$define NSTEXTFIELD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTFIELD_PAS_R}
{$define NSTEXTFIELD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTFIELD_PAS_F}
{$define NSTEXTFIELD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTFIELD_PAS_S}
{$define NSTEXTFIELD_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextFieldDelegateProtocol = objcprotocol;
  NSTextField = objcclass;
  NSTextFieldPointer = ^NSTextField;
  NSTextFieldPtr = NSTextFieldPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTFIELD_PAS_C}
{$define NSTEXTFIELD_PAS_C}

{ NSTextField }
  NSTextField = objcclass external (NSControl, NSUserInterfaceValidationsProtocol)
  private
    _delegate: id;
    _errorAction: SEL;
    
  public
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground(flag: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setTextColor(color: NSColor); message 'setTextColor:';
    function textColor: NSColor; message 'textColor';
    function isBordered: ObjCBOOL; message 'isBordered';
    procedure setBordered(flag: ObjCBOOL); message 'setBordered:';
    function isBezeled: ObjCBOOL; message 'isBezeled';
    procedure setBezeled(flag: ObjCBOOL); message 'setBezeled:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setEditable(flag: ObjCBOOL); message 'setEditable:';
    function isSelectable: ObjCBOOL; message 'isSelectable';
    procedure setSelectable(flag: ObjCBOOL); message 'setSelectable:';
    procedure selectText(sender: id); message 'selectText:';
    function delegate: NSTextFieldDelegateProtocol; message 'delegate';
    procedure setDelegate(anObject: NSTextFieldDelegateProtocol); message 'setDelegate:';
    function textShouldBeginEditing(textObject: NSText): ObjCBOOL; message 'textShouldBeginEditing:';
    function textShouldEndEditing(textObject: NSText): ObjCBOOL; message 'textShouldEndEditing:';
    procedure textDidBeginEditing(notification: NSNotification); message 'textDidBeginEditing:';
    procedure textDidEndEditing(notification: NSNotification); message 'textDidEndEditing:';
    procedure textDidChange(notification: NSNotification); message 'textDidChange:';
    function acceptsFirstResponder: ObjCBOOL; message 'acceptsFirstResponder';
    procedure setBezelStyle(style: NSTextFieldBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSTextFieldBezelStyle; message 'bezelStyle';

    { Adopted Protocols }
    function validateUserInterfaceItem(anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL;
  end;

{ NSKeyboardUI_NSTextFieldCategory }
  NSKeyboardUI_NSTextFieldCategory = objccategory external name 'NSKeyboardUI' (NSTextField)
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';
  end;

{ NSTextFieldAttributedStringMethodsCategory }
  NSTextFieldAttributedStringMethodsCategory = objccategory external (NSTextField)
    function allowsEditingTextAttributes: ObjCBOOL; message 'allowsEditingTextAttributes';
    procedure setAllowsEditingTextAttributes(flag: ObjCBOOL); message 'setAllowsEditingTextAttributes:';
    function importsGraphics: ObjCBOOL; message 'importsGraphics';
    procedure setImportsGraphics(flag: ObjCBOOL); message 'setImportsGraphics:';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSTEXTFIELD_PAS_P}
{$define NSTEXTFIELD_PAS_P}
  
{ NSTextFieldDelegate Protocol }
  NSTextFieldDelegateProtocol = objcprotocol external name 'NSTextFieldDelegate'
  end;
{$endif}
{$endif}
