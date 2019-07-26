{ Parsed from AppKit.framework NSTextList.h }


{$ifdef TYPES}
type
  NSTextListPtr = ^NSTextList;
{$endif}

{$ifdef TYPES}
type
  NSTextListOptions = NSUInteger;
  NSTextListOptionsPtr = ^NSTextListOptions;

const
  NSTextListPrependEnclosingMarker = 1 shl 0;
{$endif}

{$ifdef CLASSES}

type
  NSTextList = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _markerFormat: NSString;
    _listFlags: NSUInteger;
    _startIndex: NSInteger;
    _listSecondary: pointer;
  public
    function initWithMarkerFormat_options (format: NSString; mask: NSUInteger): instancetype; message 'initWithMarkerFormat:options:';
    function markerFormat: NSString; message 'markerFormat';
    function listOptions: NSTextListOptions; message 'listOptions';
    function markerForItemNumber (itemNum: NSInteger): NSString; message 'markerForItemNumber:';
    procedure setStartingItemNumber(newValue: NSInteger); message 'setStartingItemNumber:';
    function startingItemNumber: NSInteger; message 'startingItemNumber';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

