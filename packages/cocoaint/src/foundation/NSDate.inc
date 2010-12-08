{ Parsed from Foundation.framework NSDate.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSDATE_PAS_T}
{$define NSDATE_PAS_T}

{ Types }
type
  NSTimeInterval = double;
  NSTimeIntervalPtr = ^NSTimeInterval;

{ Defines }
const
  NSTimeIntervalSince1970 = 978307200.0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDATE_PAS_R}
{$define NSDATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDATE_PAS_F}
{$define NSDATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDATE_PAS_S}
{$define NSDATE_PAS_S}

{ External string constants }
var
  NSSystemClockDidChangeNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDate = objcclass;
  NSDatePointer = ^NSDate;
  NSDatePtr = NSDatePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDATE_PAS_C}
{$define NSDATE_PAS_C}

{ NSDate }
  NSDate = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSDate; message 'alloc';

    function timeIntervalSinceReferenceDate: NSTimeInterval; message 'timeIntervalSinceReferenceDate';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{ NSExtendedDateCategory }
  NSExtendedDateCategory = objccategory(NSDate)
    function timeIntervalSinceDate(anotherDate: NSDate): NSTimeInterval; message 'timeIntervalSinceDate:';
    function timeIntervalSinceNow: NSTimeInterval; message 'timeIntervalSinceNow';
    function timeIntervalSince1970: NSTimeInterval; message 'timeIntervalSince1970';
    function addTimeInterval(seconds: NSTimeInterval): id; message 'addTimeInterval:'; deprecated 'in Mac OS X 10.6 and later';
    function dateByAddingTimeInterval(ti: NSTimeInterval): id; message 'dateByAddingTimeInterval:';
    function earlierDate(anotherDate: NSDate): NSDate; message 'earlierDate:';
    function laterDate(anotherDate: NSDate): NSDate; message 'laterDate:';
    function compare(other: NSDate): NSComparisonResult; message 'compare:';
    function isEqualToDate(otherDate: NSDate): Boolean; message 'isEqualToDate:';
    function description: NSString; message 'description';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
  end; external;

{ NSDateCreationCategory }
  NSDateCreationCategory = objccategory(NSDate)
    class function date: id; message 'date';
    class function dateWithTimeIntervalSinceNow(secs: NSTimeInterval): id; message 'dateWithTimeIntervalSinceNow:';
    class function dateWithTimeIntervalSinceReferenceDate(secs: NSTimeInterval): id; message 'dateWithTimeIntervalSinceReferenceDate:';
    class function dateWithTimeIntervalSince1970(secs: NSTimeInterval): id; message 'dateWithTimeIntervalSince1970:';
    class function dateWithTimeInterval_sinceDate(ti: NSTimeInterval; date_: NSDate): id; message 'dateWithTimeInterval:sinceDate:';
    class function distantFuture: id; message 'distantFuture';
    class function distantPast: id; message 'distantPast';
    function init: id; message 'init';
    function initWithTimeIntervalSinceNow(secs: NSTimeInterval): id; message 'initWithTimeIntervalSinceNow:';
    function initWithTimeIntervalSinceReferenceDate(secsToBeAdded: NSTimeInterval): id; message 'initWithTimeIntervalSinceReferenceDate:';
    function initWithTimeIntervalSince1970(ti: NSTimeInterval): id; message 'initWithTimeIntervalSince1970:';
    function initWithTimeInterval_sinceDate(secsToBeAdded: NSTimeInterval; anotherDate: NSDate): id; message 'initWithTimeInterval:sinceDate:';
  end; external;

{$endif}
{$endif}
