{ Parsed from Foundation.framework NSTimeZone.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSTIMEZONE_PAS_T}
{$define NSTIMEZONE_PAS_T}

{ Constants }

const
  NSTimeZoneNameStyleStandard = 0;
  NSTimeZoneNameStyleShortStandard = 1;
  NSTimeZoneNameStyleDaylightSaving = 2;
  NSTimeZoneNameStyleShortDaylightSaving = 3;

{ Types }
type
  NSTimeZoneNameStyle = NSInteger;

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
  NSSystemTimeZoneDidChangeNotification: NSString; external name '_NSSystemTimeZoneDidChangeNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTimeZone = objcclass;
  NSTimeZonePointer = ^NSTimeZone;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTIMEZONE_PAS_C}
{$define NSTIMEZONE_PAS_C}

{ NSTimeZone }
  NSTimeZone = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSTimeZone; message 'alloc';

    function name: NSString; message 'name';
    function data: NSData; message 'data';
    function secondsFromGMTForDate(aDate: NSDate): NSInteger; message 'secondsFromGMTForDate:';
    function abbreviationForDate(aDate: NSDate): NSString; message 'abbreviationForDate:';
    function isDaylightSavingTimeForDate(aDate: NSDate): Boolean; message 'isDaylightSavingTimeForDate:';
    function daylightSavingTimeOffsetForDate(aDate: NSDate): NSTimeInterval; message 'daylightSavingTimeOffsetForDate:';
    function nextDaylightSavingTimeTransitionAfterDate(aDate: NSDate): NSDate; message 'nextDaylightSavingTimeTransitionAfterDate:';

    { Category: NSExtendedTimeZone }
    class function systemTimeZone: NSTimeZone; message 'systemTimeZone';
    class procedure resetSystemTimeZone; message 'resetSystemTimeZone';
    class function defaultTimeZone: NSTimeZone; message 'defaultTimeZone';
    class procedure setDefaultTimeZone(aTimeZone: NSTimeZone); message 'setDefaultTimeZone:';
    class function localTimeZone: NSTimeZone; message 'localTimeZone';
    class function knownTimeZoneNames: NSArray; message 'knownTimeZoneNames';
    class function abbreviationDictionary: NSDictionary; message 'abbreviationDictionary';
    function secondsFromGMT: NSInteger; message 'secondsFromGMT';
    function abbreviation: NSString; message 'abbreviation';
    function isDaylightSavingTime: Boolean; message 'isDaylightSavingTime';
    function description: NSString; message 'description';
    function isEqualToTimeZone(aTimeZone: NSTimeZone): Boolean; message 'isEqualToTimeZone:';
    function localizedName_locale(style: NSTimeZoneNameStyle; locale: NSLocale): NSString; message 'localizedName:locale:';

    { Category: NSTimeZoneCreation }
    class function timeZoneWithName(tzName: NSString): id; message 'timeZoneWithName:';
    class function timeZoneWithName_data(tzName: NSString; aData: NSData): id; message 'timeZoneWithName:data:';
    function initWithName(tzName: NSString): id; message 'initWithName:';
    function initWithName_data(tzName: NSString; aData: NSData): id; message 'initWithName:data:';
    class function timeZoneForSecondsFromGMT(seconds: NSInteger): id; message 'timeZoneForSecondsFromGMT:';
    class function timeZoneWithAbbreviation(abbreviation_: NSString): id; message 'timeZoneWithAbbreviation:';
  end; external;

{$endif}
{$endif}
