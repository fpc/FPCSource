{ Parsed from Appkit.framework NSTextField.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTEXTFIELD_PAS_H}
{$define NSTEXTFIELD_PAS_H}
type
  NSTextFieldPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSTEXTFIELD_PAS_T}
{$define NSTEXTFIELD_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextField = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTFIELD_PAS_C}
{$define NSTEXTFIELD_PAS_C}

{ NSTextField }
  NSTextField = objcclass(NSControl, NSUserInterfaceValidationsProtocol)
  private
    __delegate: id;
    __errorAction: SEL;
    
  public
    class function alloc: NSTextField; message 'alloc';

    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground(flag: Boolean); message 'setDrawsBackground:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setTextColor(color: NSColor); message 'setTextColor:';
    function textColor: NSColor; message 'textColor';
    function isBordered: Boolean; message 'isBordered';
    procedure setBordered(flag: Boolean); message 'setBordered:';
    function isBezeled: Boolean; message 'isBezeled';
    procedure setBezeled(flag: Boolean); message 'setBezeled:';
    function isEditable: Boolean; message 'isEditable';
    procedure setEditable(flag: Boolean); message 'setEditable:';
    function isSelectable: Boolean; message 'isSelectable';
    procedure setSelectable(flag: Boolean); message 'setSelectable:';
    procedure selectText(sender: id); message 'selectText:';
    function delegate: id; message 'delegate';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function textShouldBeginEditing(textObject: NSText): Boolean; message 'textShouldBeginEditing:';
    function textShouldEndEditing(textObject: NSText): Boolean; message 'textShouldEndEditing:';
    procedure textDidBeginEditing(notification: NSNotification); message 'textDidBeginEditing:';
    procedure textDidEndEditing(notification: NSNotification); message 'textDidEndEditing:';
    procedure textDidChange(notification: NSNotification); message 'textDidChange:';
    function acceptsFirstResponder: Boolean; message 'acceptsFirstResponder';
    procedure setBezelStyle(style: NSTextFieldBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSTextFieldBezelStyle; message 'bezelStyle';

    { Category: NSKeyboardUI }
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';

    { Category: NSTextFieldAttributedStringMethods }
    function allowsEditingTextAttributes: Boolean; message 'allowsEditingTextAttributes';
    procedure setAllowsEditingTextAttributes(flag: Boolean); message 'setAllowsEditingTextAttributes:';
    function importsGraphics: Boolean; message 'importsGraphics';
    procedure setImportsGraphics(flag: Boolean); message 'setImportsGraphics:';
  end; external;

{$endif}
{$endif}
