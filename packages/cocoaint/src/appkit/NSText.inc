{ Parsed from AppKit.framework NSText.h }


{$ifdef TYPES}
type
  NSTextPtr = ^NSText;
  NSTextDelegateProtocolPtr = ^NSTextDelegateProtocol;
{$endif}

{$ifdef TYPES}
const
  NSEnterCharacter = $0003;
  NSBackspaceCharacter = $0008;
  NSTabCharacter = $0009;
  NSNewlineCharacter = $000a;
  NSFormFeedCharacter = $000c;
  NSCarriageReturnCharacter = $000d;
  NSBackTabCharacter = $0019;
  NSDeleteCharacter = $007f;
  NSLineSeparatorCharacter = $2028;
  NSParagraphSeparatorCharacter = $2029;

type
  NSTextAlignment = NSUInteger;
  NSTextAlignmentPtr = ^NSTextAlignment;

const
  NSLeftTextAlignment = 0;
  NSRightTextAlignment = 1;
  NSCenterTextAlignment = 2;
  NSJustifiedTextAlignment = 3;
  NSNaturalTextAlignment = 4;

type
  NSWritingDirection = NSInteger;
  NSWritingDirectionPtr = ^NSWritingDirection;

const
  NSWritingDirectionNatural = -1;
  NSWritingDirectionLeftToRight = 0;
  NSWritingDirectionRightToLeft = 1;
const
  NSTextWritingDirectionEmbedding = 0 shl 1;
  NSTextWritingDirectionOverride = 1 shl 1;
const
  NSIllegalTextMovement = 0;
  NSReturnTextMovement = $10;
  NSTabTextMovement = $11;
  NSBacktabTextMovement = $12;
  NSLeftTextMovement = $13;
  NSRightTextMovement = $14;
  NSUpTextMovement = $15;
  NSDownTextMovement = $16;
  NSCancelTextMovement = $17;
  NSOtherTextMovement = 0;
{$endif}

{$ifdef CLASSES}

type
  NSText = objcclass external (NSView, NSChangeSpellingProtocol, NSIgnoreMisspelledWordsProtocol)
  private
    _ivars: id;
  public
    function initWithFrame (frameRect: NSRect): instancetype; message 'initWithFrame:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    procedure setString(newValue: NSString); message 'setString:';
    function string_: NSString; message 'string';
    procedure replaceCharactersInRange_withString (range: NSRange; aString: NSString); message 'replaceCharactersInRange:withString:';
    procedure replaceCharactersInRange_withRTF (range: NSRange; rtfData: NSData); message 'replaceCharactersInRange:withRTF:';
    procedure replaceCharactersInRange_withRTFD (range: NSRange; rtfdData: NSData); message 'replaceCharactersInRange:withRTFD:';
    function RTFFromRange (range: NSRange): NSData; message 'RTFFromRange:';
    function RTFDFromRange (range: NSRange): NSData; message 'RTFDFromRange:';
    function writeRTFDToFile_atomically (path: NSString; flag: ObjCBOOL): ObjCBOOL; message 'writeRTFDToFile:atomically:';
    function readRTFDFromFile (path: NSString): ObjCBOOL; message 'readRTFDFromFile:';
    procedure setDelegate(newValue: NSTextDelegateProtocol); message 'setDelegate:';
    function delegate: NSTextDelegateProtocol; message 'delegate';
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setSelectable(newValue: ObjCBOOL); message 'setSelectable:';
    function isSelectable: ObjCBOOL; message 'isSelectable';
    procedure setRichText(newValue: ObjCBOOL); message 'setRichText:';
    function isRichText: ObjCBOOL; message 'isRichText';
    procedure setImportsGraphics(newValue: ObjCBOOL); message 'setImportsGraphics:';
    function importsGraphics: ObjCBOOL; message 'importsGraphics';
    procedure setFieldEditor(newValue: ObjCBOOL); message 'setFieldEditor:';
    function isFieldEditor: ObjCBOOL; message 'isFieldEditor';
    procedure setUsesFontPanel(newValue: ObjCBOOL); message 'setUsesFontPanel:';
    function usesFontPanel: ObjCBOOL; message 'usesFontPanel';
    procedure setDrawsBackground(newValue: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    function isRulerVisible: ObjCBOOL; message 'isRulerVisible';
    procedure setSelectedRange(newValue: NSRange); message 'setSelectedRange:';
    function selectedRange: NSRange; message 'selectedRange';
    procedure scrollRangeToVisible (range: NSRange); message 'scrollRangeToVisible:';
    procedure setFont(newValue: NSFont); message 'setFont:';
    function font: NSFont; message 'font';
    procedure setTextColor(newValue: NSColor); message 'setTextColor:';
    function textColor: NSColor; message 'textColor';
    procedure setAlignment(newValue: NSTextAlignment); message 'setAlignment:';
    function alignment: NSTextAlignment; message 'alignment';
    procedure setBaseWritingDirection(newValue: NSWritingDirection); message 'setBaseWritingDirection:';
    function baseWritingDirection: NSWritingDirection; message 'baseWritingDirection';
    procedure setTextColor_range (color: NSColor; range: NSRange); message 'setTextColor:range:';
    procedure setFont_range (font_: NSFont; range: NSRange); message 'setFont:range:';
    procedure setMaxSize(newValue: NSSize); message 'setMaxSize:';
    function maxSize: NSSize; message 'maxSize';
    procedure setMinSize(newValue: NSSize); message 'setMinSize:';
    function minSize: NSSize; message 'minSize';
    procedure setHorizontallyResizable(newValue: ObjCBOOL); message 'setHorizontallyResizable:';
    function isHorizontallyResizable: ObjCBOOL; message 'isHorizontallyResizable';
    procedure setVerticallyResizable(newValue: ObjCBOOL); message 'setVerticallyResizable:';
    function isVerticallyResizable: ObjCBOOL; message 'isVerticallyResizable';
    procedure sizeToFit; message 'sizeToFit';
    procedure copy_ (sender: id); message 'copy:';
    procedure copyFont (sender: id); message 'copyFont:';
    procedure copyRuler (sender: id); message 'copyRuler:';
    procedure cut (sender: id); message 'cut:';
    procedure delete (sender: id); message 'delete:';
    procedure paste (sender: id); message 'paste:';
    procedure pasteFont (sender: id); message 'pasteFont:';
    procedure pasteRuler (sender: id); message 'pasteRuler:';
    procedure selectAll (sender: id); message 'selectAll:';
    procedure changeFont (sender: id); message 'changeFont:';
    procedure alignLeft (sender: id); message 'alignLeft:';
    procedure alignRight (sender: id); message 'alignRight:';
    procedure alignCenter (sender: id); message 'alignCenter:';
    procedure subscript (sender: id); message 'subscript:';
    procedure superscript (sender: id); message 'superscript:';
    procedure underline (sender: id); message 'underline:';
    procedure unscript (sender: id); message 'unscript:';
    procedure showGuessPanel (sender: id); message 'showGuessPanel:';
    procedure checkSpelling (sender: id); message 'checkSpelling:';
    procedure toggleRuler (sender: id); message 'toggleRuler:';

    { Adopted protocols }
    procedure changeSpelling (sender: id); message 'changeSpelling:';
    procedure ignoreSpelling (sender: id); message 'ignoreSpelling:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTextDelegateProtocol = objcprotocol external name 'NSTextDelegate' (NSObjectProtocol)
  optional
    function textShouldBeginEditing (textObject: NSText): ObjCBOOL; message 'textShouldBeginEditing:';
    function textShouldEndEditing (textObject: NSText): ObjCBOOL; message 'textShouldEndEditing:';
    procedure textDidBeginEditing (notification: NSNotification); message 'textDidBeginEditing:';
    procedure textDidEndEditing (notification: NSNotification); message 'textDidEndEditing:';
    procedure textDidChange (notification: NSNotification); message 'textDidChange:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTextDidBeginEditingNotification: NSString; cvar; external;
  NSTextDidEndEditingNotification: NSString; cvar; external;
  NSTextDidChangeNotification: NSString; cvar; external;
{$endif}

