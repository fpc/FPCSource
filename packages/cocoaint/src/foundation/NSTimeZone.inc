{ Parsed from Foundation.framework NSTimeZone.h }

{$ifdef TYPES}
{$ifndef NSTIMEZONE_PAS_T}
{$define NSTIMEZONE_PAS_T}

{ Constants }

const
  NSTimeZoneNameStyleStandard = 0;
  NSTimeZoneNameStyleShortStandard = 1;
  NSTimeZoneNameStyleDaylightSaving = 2;
  NSTimeZoneNameStyleShortDaylightSaving = 3;
  NSTimeZoneNameStyleGeneric = 4;
  NSTimeZoneNameStyleShortGeneric = 5;

{ Types }
type
  NSTimeZoneNameStyle = NSInteger;
  NSTimeZoneNameStylePtr = ^NSTimeZoneNameStyle;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTIMEZONE_PAS_R}
{$define NSTIMEZONE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTIMEZONE_PAS_F}
{$define NSTIMEZONE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTIMEZONE_PAS_S}
{$define NSTIMEZONE_PAS_S}

{ External string constants }
var
  NSSystemTimeZoneDidChangeNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTimeZone = objcclass;
  NSTimeZonePointer = ^NSTimeZone;
  NSTimeZonePtr = NSTimeZonePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTIMEZONE_PAS_C}
{$define NSTIMEZONE_PAS_C}

{ NSTimeZone }
  NSTimeZone = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    function name: NSString; message 'name';
    function data: NSData; message 'data';
    function secondsFromGMTForDate(aDate: NSDate): NSInteger; message 'secondsFromGMTForDate:';
    function abbreviationForDate(aDate: NSDate): NSString; message 'abbreviationForDate:';
    function isDaylightSavingTimeForDate(aDate: NSDate): Boolean; message 'isDaylightSavingTimeForDate:';
    function daylightSavingTimeOffsetForDate(aDate: NSDate): NSTimeInterval; message 'daylightSavingTimeOffsetForDate:';
    function nextDaylightSavingTimeTransitionAfterDate(aDate: NSDate): NSDate; message 'nextDaylightSavingTimeTransitionAfterDate:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSExtendedTimeZoneCategory }
  NSExtendedTimeZoneCategory = objccategory external (NSTimeZone)
    class function systemTimeZone: NSTimeZone; message 'systemTimeZone';
    class procedure resetSystemTimeZone; message 'resetSystemTimeZone';
    class function defaultTimeZone: NSTimeZone; message 'defaultTimeZone';
    class procedure setDefaultTimeZone(aTimeZone: NSTimeZone); message 'setDefaultTimeZone:';
    class function localTimeZone: NSTimeZone; message 'localTimeZone';
    class function knownTimeZoneNames: NSArray; message 'knownTimeZoneNames';
    class function abbreviationDictionary: NSDictionary; message 'abbreviationDictionary';
    class procedure setAbbreviationDictionary(dict: NSDictionary); message 'setAbbreviationDictionary:';
    class function timeZoneDataVersion: NSString; message 'timeZoneDataVersion';
    function secondsFromGMT: NSInteger; message 'secondsFromGMT';
    function abbreviation: NSString; message 'abbreviation';
    function isDaylightSavingTime: Boolean; message 'isDaylightSavingTime';
    function daylightSavingTimeOffset: NSTimeInterval; message 'daylightSavingTimeOffset';
    function nextDaylightSavingTimeTransition: NSDate; message 'nextDaylightSavingTimeTransition';
    function description: NSString; message 'description';
    function isEqualToTimeZone(aTimeZone: NSTimeZone): Boolean; message 'isEqualToTimeZone:';
    function localizedName_locale(style: NSTimeZoneNameStyle; locale: NSLocale): NSString; message 'localizedName:locale:';
  end;

{ NSTimeZoneCreationCategory }
  NSTimeZoneCreationCategory = objccategory external (NSTimeZone)
    class function timeZoneWithName(tzName: NSString): id; message 'timeZoneWithName:';
    class function timeZoneWithName_data(tzName: NSString; aData: NSData): id; message 'timeZoneWithName:data:';
    function initWithName(tzName: NSString): id; message 'initWithName:';
    function initWithName_data(tzName: NSString; aData: NSData): id; message 'initWithName:data:';
    class function timeZoneForSecondsFromGMT(seconds: NSInteger): id; message 'timeZoneForSecondsFromGMT:';
    class function timeZoneWithAbbreviation(abbreviation_: NSString): id; message 'timeZoneWithAbbreviation:';
  end;

{$endif}
{$endif}
