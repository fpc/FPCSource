{ Parsed from Appkit.framework NSTrackingArea.h }

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
  NSTrackingAreaOptionsPtr = ^NSTrackingAreaOptions;

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
  NSTrackingAreaPtr = NSTrackingAreaPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTRACKINGAREA_PAS_C}
{$define NSTRACKINGAREA_PAS_C}

{ NSTrackingArea }
  NSTrackingArea = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _rect: NSRect;
    _owner: id; {garbage collector: __weak }
    _userInfo: NSDictionary;
    _options: NSTrackingAreaOptions;
    _privateFlags: NSInteger;
    _reserved: Pointer;
    
  public
    function initWithRect_options_owner_userInfo(rect_: NSRect; options_: NSTrackingAreaOptions; owner_: id; userInfo_: NSDictionary): id; message 'initWithRect:options:owner:userInfo:';
    function rect: NSRect; message 'rect';
    function options: NSTrackingAreaOptions; message 'options';
    function owner: id; message 'owner';
    function userInfo: NSDictionary; message 'userInfo';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
