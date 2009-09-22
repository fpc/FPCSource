{ Parsed from Appkit.framework NSBox.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSBOX_PAS_H}
{$define NSBOX_PAS_H}
type
  NSBoxPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSBOX_PAS_T}
{$define NSBOX_PAS_T}

{ Constants }

const
  NSNoTitle = 0;
  NSAboveTop = 1;
  NSAtTop = 2;
  NSBelowTop = 3;
  NSAboveBottom = 4;
  NSAtBottom = 5;
  NSBelowBottom = 6;

{ Types }
type
  NSTitlePosition = NSUInteger;
  NSBoxType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBOX_PAS_R}
{$define NSBOX_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSBOX_PAS_F}
{$define NSBOX_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSBOX_PAS_T}
{$define NSBOX_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSBox = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBOX_PAS_C}
{$define NSBOX_PAS_C}

{ NSBox }
  NSBox = objcclass(NSView)
  private
    __titleCell: id;
    __contentView: id;
    __offsets: NSSize;
    __borderRect: NSRect;
    __titleRect: NSRect;
    __bFlags: bitpacked record
        borderType: 0..((1 shl 2)-1);
        titlePosition: 0..((1 shl 3)-1);
        backgroundTransparent: 0..1;
        reserved: 0..((1 shl 2)-1);
        needsTile: 0..1;
        transparent: 0..1;
        colorAltInterpretation: 0..1;
        boxType: 0..((1 shl 3)-1);
        _RESERVED: 0..((1 shl 18)-1);
      end;
    __unused: id;
    
  public
    class function alloc: NSBox; message 'alloc';

    function borderType: NSBorderType; message 'borderType';
    function titlePosition: NSTitlePosition; message 'titlePosition';
    procedure setBorderType(aType: NSBorderType); message 'setBorderType:';
    procedure setBoxType(boxType_: NSBoxType); message 'setBoxType:';
    function boxType: NSBoxType; message 'boxType';
    procedure setTitlePosition(aPosition: NSTitlePosition); message 'setTitlePosition:';
    function title: NSString; message 'title';
    procedure setTitle(aString: NSString); message 'setTitle:';
    function titleFont: NSFont; message 'titleFont';
    procedure setTitleFont(fontObj: NSFont); message 'setTitleFont:';
    function borderRect: NSRect; message 'borderRect';
    function titleRect: NSRect; message 'titleRect';
    function titleCell: id; message 'titleCell';
    procedure sizeToFit; message 'sizeToFit';
    function contentViewMargins: NSSize; message 'contentViewMargins';
    procedure setContentViewMargins(offsetSize: NSSize); message 'setContentViewMargins:';
    procedure setFrameFromContentFrame(contentFrame: NSRect); message 'setFrameFromContentFrame:';
    function contentView: id; message 'contentView';
    procedure setContentView(aView: NSView); message 'setContentView:';
    function isTransparent: Boolean; message 'isTransparent';
    procedure setTransparent(flag: Boolean); message 'setTransparent:';

    { Category: NSKeyboardUI }
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';

    { Category: NSCustomBoxTypeProperties }
    function borderWidth: CGFloat; message 'borderWidth';
    procedure setBorderWidth(borderWidth_: CGFloat); message 'setBorderWidth:';
    function cornerRadius: CGFloat; message 'cornerRadius';
    procedure setCornerRadius(cornerRadius_: CGFloat); message 'setCornerRadius:';
    function borderColor: NSColor; message 'borderColor';
    procedure setBorderColor(borderColor_: NSColor); message 'setBorderColor:';
    function fillColor: NSColor; message 'fillColor';
    procedure setFillColor(fillColor_: NSColor); message 'setFillColor:';
  end; external;

{$endif}
{$endif}
