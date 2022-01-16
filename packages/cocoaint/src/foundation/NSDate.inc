{ Parsed from Foundation.framework NSDate.h }


{$ifdef TYPES}
type
  NSDatePtr = ^NSDate;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSystemClockDidChangeNotification: NSString { available in 10_6, 4_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSTimeInterval = double;
  NSTimeIntervalPtr = ^NSTimeInterval;

const
  NSTimeIntervalSince1970 = 978307200.0;
{$endif}

{$ifdef CLASSES}

type
  NSDate = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    function timeIntervalSinceReferenceDate: NSTimeInterval; message 'timeIntervalSinceReferenceDate';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithTimeIntervalSinceReferenceDate (ti: NSTimeInterval): instancetype; message 'initWithTimeIntervalSinceReferenceDate:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSExtendedDate = objccategory external (NSDate)
    function timeIntervalSinceDate (anotherDate: NSDate): NSTimeInterval; message 'timeIntervalSinceDate:';
    function timeIntervalSinceNow: NSTimeInterval; message 'timeIntervalSinceNow';
    function timeIntervalSince1970: NSTimeInterval; message 'timeIntervalSince1970';
    function addTimeInterval (seconds: NSTimeInterval): id; message 'addTimeInterval:'; deprecated 'in 10_0, 10_6, 2_0, 4_0';
    function dateByAddingTimeInterval (ti: NSTimeInterval): instancetype; message 'dateByAddingTimeInterval:'; { available in 10_6, 2_0 }
    function earlierDate (anotherDate: NSDate): NSDate; message 'earlierDate:';
    function laterDate (anotherDate: NSDate): NSDate; message 'laterDate:';
    function compare (other: NSDate): NSComparisonResult; message 'compare:';
    function isEqualToDate (otherDate: NSDate): ObjCBOOL; message 'isEqualToDate:';
    function description: NSString; message 'description';
    function descriptionWithLocale (locale: id): NSString; message 'descriptionWithLocale:';
    class function timeIntervalSinceReferenceDate_: NSTimeInterval; message 'timeIntervalSinceReferenceDate';
  end;

type
  NSDateCreation = objccategory external (NSDate)
    class function date: instancetype; message 'date';
    class function dateWithTimeIntervalSinceNow (secs: NSTimeInterval): instancetype; message 'dateWithTimeIntervalSinceNow:';
    class function dateWithTimeIntervalSinceReferenceDate (ti: NSTimeInterval): instancetype; message 'dateWithTimeIntervalSinceReferenceDate:';
    class function dateWithTimeIntervalSince1970 (secs: NSTimeInterval): instancetype; message 'dateWithTimeIntervalSince1970:';
    class function dateWithTimeInterval_sinceDate (secsToBeAdded: NSTimeInterval; date_: NSDate): instancetype; message 'dateWithTimeInterval:sinceDate:';
    class function distantFuture: id; message 'distantFuture';
    class function distantPast: id; message 'distantPast';
    function initWithTimeIntervalSinceNow (secs: NSTimeInterval): instancetype; message 'initWithTimeIntervalSinceNow:';
    function initWithTimeIntervalSince1970 (secs: NSTimeInterval): instancetype; message 'initWithTimeIntervalSince1970:';
    function initWithTimeInterval_sinceDate (secsToBeAdded: NSTimeInterval; date_: NSDate): instancetype; message 'initWithTimeInterval:sinceDate:';
  end;
{$endif}

