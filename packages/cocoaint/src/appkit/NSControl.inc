{ Parsed from Appkit.framework NSControl.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSCONTROL_PAS_T}
{$define NSCONTROL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCONTROL_PAS_R}
{$define NSCONTROL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCONTROL_PAS_F}
{$define NSCONTROL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCONTROL_PAS_S}
{$define NSCONTROL_PAS_S}

{ External string constants }
var
  NSControlTextDidBeginEditingNotification: NSString; external name '_NSControlTextDidBeginEditingNotification';
  NSControlTextDidEndEditingNotification: NSString; external name '_NSControlTextDidEndEditingNotification';
  NSControlTextDidChangeNotification: NSString; external name '_NSControlTextDidChangeNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSControl = objcclass;
  NSControlPointer = ^NSControl;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCONTROL_PAS_C}
{$define NSCONTROL_PAS_C}

{ NSControl }
  NSControl = objcclass(NSView)
  private
    __tag: NSInteger;
    __cell: id;
    __conFlags: bitpacked record
        enabled: 0..1;
        ignoreMultiClick: 0..1;
        calcSize: 0..1;
        drawingAncestor: 0..1;
        ibReserved: 0..1;
        updateCellFocus: 0..1;
        reserved: 0..((1 shl 26)-1);
      end;
    
  public
    class function alloc: NSControl; message 'alloc';

    class procedure setCellClass(factoryId: Pobjc_class); message 'setCellClass:';
    class function cellClass: Pobjc_class; message 'cellClass';
    function initWithFrame(frameRect: NSRect): id; message 'initWithFrame:';
    procedure sizeToFit; message 'sizeToFit';
    procedure calcSize; message 'calcSize';
    function cell: id; message 'cell';
    procedure setCell(aCell: NSCell); message 'setCell:';
    function selectedCell: id; message 'selectedCell';
    function target: id; message 'target';
    procedure setTarget(anObject: id); message 'setTarget:';
    function action: SEL; message 'action';
    procedure setAction(aSelector: SEL); message 'setAction:';
    function tag: NSInteger; message 'tag';
    procedure setTag(anInt: NSInteger); message 'setTag:';
    function selectedTag: NSInteger; message 'selectedTag';
    procedure setIgnoresMultiClick(flag: Boolean); message 'setIgnoresMultiClick:';
    function ignoresMultiClick: Boolean; message 'ignoresMultiClick';
    function sendActionOn(mask: NSInteger): NSInteger; message 'sendActionOn:';
    function isContinuous: Boolean; message 'isContinuous';
    procedure setContinuous(flag: Boolean); message 'setContinuous:';
    function isEnabled: Boolean; message 'isEnabled';
    procedure setEnabled(flag: Boolean); message 'setEnabled:';
    procedure setFloatingPointFormat_left_right(autoRange: Boolean; leftDigits: NSUInteger; rightDigits: NSUInteger); message 'setFloatingPointFormat:left:right:';
    function alignment: NSTextAlignment; message 'alignment';
    procedure setAlignment(mode: NSTextAlignment); message 'setAlignment:';
    function font: NSFont; message 'font';
    procedure setFont(fontObj: NSFont); message 'setFont:';
    procedure setFormatter(newFormatter: NSFormatter); message 'setFormatter:';
    function formatter: id; message 'formatter';
    procedure setObjectValue(obj: id); message 'setObjectValue:';
    procedure setStringValue(aString: NSString); message 'setStringValue:';
    procedure setIntValue(anInt: cint); message 'setIntValue:';
    procedure setFloatValue(aFloat: single); message 'setFloatValue:';
    procedure setDoubleValue(aDouble: double); message 'setDoubleValue:';
    function objectValue: id; message 'objectValue';
    function stringValue: NSString; message 'stringValue';
    function intValue: cint; message 'intValue';
    function floatValue: single; message 'floatValue';
    function doubleValue: double; message 'doubleValue';
    procedure setNeedsDisplay; message 'setNeedsDisplay';
    procedure updateCell(aCell: NSCell); message 'updateCell:';
    procedure updateCellInside(aCell: NSCell); message 'updateCellInside:';
    procedure drawCellInside(aCell: NSCell); message 'drawCellInside:';
    procedure drawCell(aCell: NSCell); message 'drawCell:';
    procedure selectCell(aCell: NSCell); message 'selectCell:';
    function sendAction_to(theAction: SEL; theTarget: id): Boolean; message 'sendAction:to:';
    procedure takeIntValueFrom(sender: id); message 'takeIntValueFrom:';
    procedure takeFloatValueFrom(sender: id); message 'takeFloatValueFrom:';
    procedure takeDoubleValueFrom(sender: id); message 'takeDoubleValueFrom:';
    procedure takeStringValueFrom(sender: id); message 'takeStringValueFrom:';
    procedure takeObjectValueFrom(sender: id); message 'takeObjectValueFrom:';
    function currentEditor: NSText; message 'currentEditor';
    function abortEditing: Boolean; message 'abortEditing';
    procedure validateEditing; message 'validateEditing';
    procedure mouseDown(theEvent: NSEvent); message 'mouseDown:';
    function baseWritingDirection: NSWritingDirection; message 'baseWritingDirection';
    procedure setBaseWritingDirection(writingDirection: NSWritingDirection); message 'setBaseWritingDirection:';
    function integerValue: NSInteger; message 'integerValue';
    procedure setIntegerValue(anInteger: NSInteger); message 'setIntegerValue:';
    procedure takeIntegerValueFrom(sender: id); message 'takeIntegerValueFrom:';

    { Category: NSKeyboardUI }
    procedure performClick(sender: id); message 'performClick:';
    procedure setRefusesFirstResponder(flag: Boolean); message 'setRefusesFirstResponder:';
    function refusesFirstResponder: Boolean; message 'refusesFirstResponder';

    { Category: NSControlAttributedStringMethods }
    function attributedStringValue: NSAttributedString; message 'attributedStringValue';
    procedure setAttributedStringValue(obj: NSAttributedString); message 'setAttributedStringValue:';
  end; external;

{$endif}
{$endif}
