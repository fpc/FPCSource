{ Parsed from Appkit.framework NSTrackingArea.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSTRACKINGAREA_PAS_T}
{$define NSTRACKINGAREA_PAS_T}

{ Constants }

const
  NSTrackingMouseEnteredAndExited = $01;
  NSTrackingMouseMoved = $02;
  NSTrackingCursorUpdate = $04;

const
  NSTrackingActiveWhenFirstResponder = $10;
  NSTrackingActiveInKeyWindow = $20;
  NSTrackingActiveInActiveApp = $40;
  NSTrackingActiveAlways = $80;

const
  NSTrackingAssumeInside = $100;
  NSTrackingInVisibleRect = $200;
  NSTrackingEnabledDuringMouseDrag = $400;

{ Types }
type
  NSTrackingAreaOptions = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTRACKINGAREA_PAS_R}
{$define NSTRACKINGAREA_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTRACKINGAREA_PAS_F}
{$define NSTRACKINGAREA_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTRACKINGAREA_PAS_S}
{$define NSTRACKINGAREA_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTrackingArea = objcclass;
  NSTrackingAreaPointer = ^NSTrackingArea;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTRACKINGAREA_PAS_C}
{$define NSTRACKINGAREA_PAS_C}

{ NSTrackingArea }
  NSTrackingArea = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __rect: NSRect;
    __owner: id; {garbage collector: __weak }
    __userInfo: NSDictionary;
    __options: NSTrackingAreaOptions;
    __privateFlags: NSInteger;
    __reserved: Pointer;
    
  public
    class function alloc: NSTrackingArea; message 'alloc';

    function initWithRect_options_owner_userInfo(rect_: NSRect; options_: NSTrackingAreaOptions; owner_: id; userInfo_: NSDictionary): NSTrackingArea; message 'initWithRect:options:owner:userInfo:';
    function rect: NSRect; message 'rect';
    function options: NSTrackingAreaOptions; message 'options';
    function owner: id; message 'owner';
    function userInfo: NSDictionary; message 'userInfo';
  end; external;

{$endif}
{$endif}
