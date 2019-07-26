{ Parsed from Foundation.framework NSTimeZone.h }


{ Types from NSExtendedTimeZone }
{$ifdef TYPES}

type
  NSTimeZoneNameStyle = NSInteger;
type
  NSTimeZoneNameStylePtr = ^NSTimeZoneNameStyle;

const
  NSTimeZoneNameStyleStandard = 0;
  NSTimeZoneNameStyleShortStandard = 1;
  NSTimeZoneNameStyleDaylightSaving = 2;
  NSTimeZoneNameStyleShortDaylightSaving = 3;
  NSTimeZoneNameStyleGeneric = 4;
  NSTimeZoneNameStyleShortGeneric = 5;
{$endif}


{$ifdef TYPES}
type
  NSTimeZonePtr = ^NSTimeZone;
{$endif}

{$ifdef CLASSES}

type
  NSTimeZone = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    function name: NSString; message 'name';
    function data: NSData; message 'data';
    function secondsFromGMTForDate (aDate: NSDate): NSInteger; message 'secondsFromGMTForDate:';
    function abbreviationForDate (aDate: NSDate): NSString; message 'abbreviationForDate:';
    function isDaylightSavingTimeForDate (aDate: NSDate): ObjCBOOL; message 'isDaylightSavingTimeForDate:';
    function daylightSavingTimeOffsetForDate (aDate: NSDate): NSTimeInterval; message 'daylightSavingTimeOffsetForDate:'; { available in 10_5, 2_0 }
    function nextDaylightSavingTimeTransitionAfterDate (aDate: NSDate): NSDate; message 'nextDaylightSavingTimeTransitionAfterDate:'; { available in 10_5, 2_0 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSExtendedTimeZone = objccategory external (NSTimeZone)
    class function systemTimeZone: NSTimeZone; message 'systemTimeZone';
    class procedure resetSystemTimeZone; message 'resetSystemTimeZone';
    class function defaultTimeZone: NSTimeZone; message 'defaultTimeZone';
    class procedure setDefaultTimeZone (aTimeZone: NSTimeZone); message 'setDefaultTimeZone:';
    class function localTimeZone: NSTimeZone; message 'localTimeZone';
    class function knownTimeZoneNames: NSArray; message 'knownTimeZoneNames';
    class function abbreviationDictionary: NSDictionary; message 'abbreviationDictionary';
    class procedure setAbbreviationDictionary (dict: NSDictionary); message 'setAbbreviationDictionary:'; { available in 10_6, 4_0 }
    class function timeZoneDataVersion: NSString; message 'timeZoneDataVersion'; { available in 10_6, 4_0 }
    function secondsFromGMT: NSInteger; message 'secondsFromGMT';
    function abbreviation: NSString; message 'abbreviation';
    function isDaylightSavingTime: ObjCBOOL; message 'isDaylightSavingTime';
    function daylightSavingTimeOffset: NSTimeInterval; message 'daylightSavingTimeOffset';
    function nextDaylightSavingTimeTransition: NSDate; message 'nextDaylightSavingTimeTransition';
    function description: NSString; message 'description';
    function isEqualToTimeZone (aTimeZone: NSTimeZone): ObjCBOOL; message 'isEqualToTimeZone:';
    function localizedName_locale (style: NSTimeZoneNameStyle; locale: NSLocale): NSString; message 'localizedName:locale:'; { available in 10_5, 2_0 }
  end;

type
  NSTimeZoneCreation = objccategory external (NSTimeZone)
    class function timeZoneWithName (tzName: NSString): instancetype; message 'timeZoneWithName:';
    class function timeZoneWithName_data (tzName: NSString; aData: NSData): instancetype; message 'timeZoneWithName:data:';
    function initWithName (tzName: NSString): instancetype; message 'initWithName:';
    function initWithName_data (tzName: NSString; aData: NSData): instancetype; message 'initWithName:data:';
    class function timeZoneForSecondsFromGMT (seconds: NSInteger): instancetype; message 'timeZoneForSecondsFromGMT:';
    class function timeZoneWithAbbreviation (abbreviation: NSString): instancetype; message 'timeZoneWithAbbreviation:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSystemTimeZoneDidChangeNotification: NSString { available in 10_5, 2_0 }; cvar; external;
{$endif}

