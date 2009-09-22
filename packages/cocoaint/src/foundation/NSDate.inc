{ Parsed from Foundation.framework NSDate.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:45 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDATE_PAS_H}
{$define NSDATE_PAS_H}
type
  NSDatePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSDATE_PAS_T}
{$define NSDATE_PAS_T}

{ Types }
type
  NSTimeInterval = double;

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
{$ifndef NSDATE_PAS_T}
{$define NSDATE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDate = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDATE_PAS_C}
{$define NSDATE_PAS_C}

{ NSDate }
  NSDate = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSDate; message 'alloc';

    function timeIntervalSinceReferenceDate: NSTimeInterval; message 'timeIntervalSinceReferenceDate';

    { Category: NSExtendedDate }
    function timeIntervalSinceDate(anotherDate: NSDate): NSTimeInterval; message 'timeIntervalSinceDate:';
    function timeIntervalSinceNow: NSTimeInterval; message 'timeIntervalSinceNow';
    function timeIntervalSince1970: NSTimeInterval; message 'timeIntervalSince1970';
    function addTimeInterval(seconds: NSTimeInterval): id; message 'addTimeInterval:';
    function earlierDate(anotherDate: NSDate): NSDate; message 'earlierDate:';
    function laterDate(anotherDate: NSDate): NSDate; message 'laterDate:';
    function compare(other: NSDate): NSComparisonResult; message 'compare:';
    function description: NSString; message 'description';
    function isEqualToDate(otherDate: NSDate): Boolean; message 'isEqualToDate:';

    { Category: NSDateCreation }
    class function date: id; message 'date';
    class function dateWithTimeIntervalSinceNow(secs: NSTimeInterval): id; message 'dateWithTimeIntervalSinceNow:';
    class function dateWithTimeIntervalSinceReferenceDate(secs: NSTimeInterval): id; message 'dateWithTimeIntervalSinceReferenceDate:';
    class function dateWithTimeIntervalSince1970(secs: NSTimeInterval): id; message 'dateWithTimeIntervalSince1970:';
    class function distantFuture: id; message 'distantFuture';
    class function distantPast: id; message 'distantPast';
    function init: id; message 'init';
    function initWithTimeIntervalSinceReferenceDate(secsToBeAdded: NSTimeInterval): id; message 'initWithTimeIntervalSinceReferenceDate:';
    function initWithTimeInterval_sinceDate(secsToBeAdded: NSTimeInterval; anotherDate: NSDate): id; message 'initWithTimeInterval:sinceDate:';
    function initWithTimeIntervalSinceNow(secsToBeAddedToNow: NSTimeInterval): id; message 'initWithTimeIntervalSinceNow:';

    { Category: NSCalendarDateExtras }
    class function dateWithString(aString: NSString): id; message 'dateWithString:';
    function initWithString(description_: NSString): id; message 'initWithString:';
    function dateWithCalendarFormat_timeZone(format: NSString; aTimeZone: NSTimeZone): NSCalendarDate; message 'dateWithCalendarFormat:timeZone:';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithCalendarFormat_timeZone_locale(format: NSString; aTimeZone: NSTimeZone; locale: id): NSString; message 'descriptionWithCalendarFormat:timeZone:locale:';

    { Category: NSNaturalLangage }
  end; external;

{$endif}
{$endif}
