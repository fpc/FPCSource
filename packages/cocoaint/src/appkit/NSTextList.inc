{ Parsed from Appkit.framework NSTextList.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSTEXTLIST_PAS_T}
{$define NSTEXTLIST_PAS_T}

{ Constants }

const
  NSTextListPrependEnclosingMarker = 1 shl 0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTLIST_PAS_R}
{$define NSTEXTLIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTLIST_PAS_F}
{$define NSTEXTLIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTLIST_PAS_S}
{$define NSTEXTLIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextList = objcclass;
  NSTextListPointer = ^NSTextList;
  NSTextListPtr = NSTextListPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTLIST_PAS_C}
{$define NSTEXTLIST_PAS_C}

{ NSTextList }
  NSTextList = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _markerFormat: NSString;
    _listFlags: NSUInteger;
    _startIndex: NSInteger;
    _listSecondary: Pointer;
    
  public
    class function alloc: NSTextList; message 'alloc';

    function initWithMarkerFormat_options(format: NSString; mask: NSUInteger): id; message 'initWithMarkerFormat:options:';
    function markerFormat: NSString; message 'markerFormat';
    function listOptions: NSUInteger; message 'listOptions';
    function markerForItemNumber(itemNum: NSInteger): NSString; message 'markerForItemNumber:';
    procedure setStartingItemNumber(itemNum: NSInteger); message 'setStartingItemNumber:';
    function startingItemNumber: NSInteger; message 'startingItemNumber';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end; external;

{$endif}
{$endif}
