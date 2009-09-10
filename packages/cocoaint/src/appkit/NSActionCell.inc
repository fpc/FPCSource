{ Parsed from Appkit.framework NSActionCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSACTIONCELL_PAS_H}
{$define NSACTIONCELL_PAS_H}
type
  NSActionCellPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSACTIONCELL_PAS_T}
{$define NSACTIONCELL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSActionCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSACTIONCELL_PAS_C}
{$define NSACTIONCELL_PAS_C}

{ NSActionCell }
  NSActionCell = objcclass(NSCell)
  private
    __tag: clong;
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
    procedure setFloatingPointFormat_left_right(autoRange: Boolean; leftDigits: culong; rightDigits: culong); message 'setFloatingPointFormat:left:right:';
    procedure setImage(image_: NSImage); message 'setImage:';
    function target: id; message 'target';
    procedure setTarget(anObject: id); message 'setTarget:';
    function action: SEL; message 'action';
    procedure setAction(aSelector: SEL); message 'setAction:';
    function tag: clong; message 'tag';
    procedure setTag(anInt: clong); message 'setTag:';
    function stringValue: NSString; message 'stringValue';
    function intValue: cint; message 'intValue';
    function floatValue: single; message 'floatValue';
    function doubleValue: double; message 'doubleValue';
    procedure setObjectValue(obj: id); message 'setObjectValue:';
    function integerValue: clong; message 'integerValue';
  end; external;

{$endif}
{$endif}
