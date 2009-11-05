{ Parsed from Appkit.framework NSActionCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSACTIONCELL_PAS_T}
{$define NSACTIONCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSACTIONCELL_PAS_R}
{$define NSACTIONCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSACTIONCELL_PAS_F}
{$define NSACTIONCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSACTIONCELL_PAS_S}
{$define NSACTIONCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSActionCell = objcclass;
  NSActionCellPointer = ^NSActionCell;

{$endif}

{$ifdef CLASSES}
{$ifndef NSACTIONCELL_PAS_C}
{$define NSACTIONCELL_PAS_C}

{ NSActionCell }
  NSActionCell = objcclass(NSCell)
  private
    __tag: NSInteger;
    __target: id;
    __action: SEL;
    __controlView: id;
    
  public
    class function alloc: NSActionCell; message 'alloc';

    function controlView: NSView; message 'controlView';
    procedure setControlView(view: NSView); message 'setControlView:';
    procedure setFont(fontObj: NSFont); message 'setFont:';
    procedure setAlignment(mode: NSTextAlignment); message 'setAlignment:';
    procedure setBordered(flag: Boolean); message 'setBordered:';
    procedure setBezeled(flag: Boolean); message 'setBezeled:';
    procedure setEnabled(flag: Boolean); message 'setEnabled:';
    procedure setFloatingPointFormat_left_right(autoRange: Boolean; leftDigits: NSUInteger; rightDigits: NSUInteger); message 'setFloatingPointFormat:left:right:';
    procedure setImage(image_: NSImage); message 'setImage:';
    function target: id; message 'target';
    procedure setTarget(anObject: id); message 'setTarget:';
    function action: SEL; message 'action';
    procedure setAction(aSelector: SEL); message 'setAction:';
    function tag: NSInteger; message 'tag';
    procedure setTag(anInt: NSInteger); message 'setTag:';
    function stringValue: NSString; message 'stringValue';
    function intValue: cint; message 'intValue';
    function floatValue: single; message 'floatValue';
    function doubleValue: double; message 'doubleValue';
    procedure setObjectValue(obj: id); message 'setObjectValue:';
    function integerValue: NSInteger; message 'integerValue';
  end; external;

{$endif}
{$endif}
