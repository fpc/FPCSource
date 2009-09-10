{ Parsed from Foundation.framework NSDistributedNotificationCenter.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_H}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_H}
type
  NSDistributedNotificationCenterPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_T}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_T}

{ CFString constants }
var
  NSLocalNotificationCenterType: CFStringRef; external name '_NSLocalNotificationCenterType';

{ Constants }

const
  NSNotificationSuspensionBehaviorDrop = 1;
  NSNotificationSuspensionBehaviorCoalesce = 2;
  NSNotificationSuspensionBehaviorHold = 3;
  NSNotificationSuspensionBehaviorDeliverImmediately = 4;

const
  NSNotificationDeliverImmediately = 1  shl  0;
  NSNotificationPostToAllSessions = 1  shl  1;

{ Types }
type
  NSNotificationSuspensionBehavior = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_R}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_F}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_T}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDistributedNotificationCenter = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_C}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_C}

{ NSDistributedNotificationCenter }
  NSDistributedNotificationCenter = objcclass(NSNotificationCenter)
    
  public
    class function alloc: NSDistributedNotificationCenter; message 'alloc';

    class function notificationCenterForType(notificationCenterType: NSString): NSDistributedNotificationCenter; message 'notificationCenterForType:';
    class function defaultCenter: id; message 'defaultCenter';
    procedure addObserver_selector_name_object_suspensionBehavior(observer: id; selector: SEL; name: NSString; object_: NSString; suspensionBehavior: NSNotificationSuspensionBehavior); message 'addObserver:selector:name:object:suspensionBehavior:';
    procedure postNotificationName_object_userInfo_deliverImmediately(name: NSString; object_: NSString; userInfo: NSDictionary; deliverImmediately: Boolean); message 'postNotificationName:object:userInfo:deliverImmediately:';
  end; external;

{$endif}
{$endif}
