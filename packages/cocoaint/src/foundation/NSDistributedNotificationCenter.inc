{ Parsed from Foundation.framework NSDistributedNotificationCenter.h }

{$ifdef TYPES}
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_T}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_T}

{ Constants }

const
  NSNotificationSuspensionBehaviorDrop = 1;
  NSNotificationSuspensionBehaviorCoalesce = 2;
  NSNotificationSuspensionBehaviorHold = 3;
  NSNotificationSuspensionBehaviorDeliverImmediately = 4;

const
  NSNotificationDeliverImmediately = 1 shl 0;
  NSNotificationPostToAllSessions = 1 shl 1;

{ Types }
type
  NSNotificationSuspensionBehavior = NSUInteger;
  NSNotificationSuspensionBehaviorPtr = ^NSNotificationSuspensionBehavior;

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
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_S}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_S}

{ External string constants }
var
  NSLocalNotificationCenterType: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDistributedNotificationCenter = objcclass;
  NSDistributedNotificationCenterPointer = ^NSDistributedNotificationCenter;
  NSDistributedNotificationCenterPtr = NSDistributedNotificationCenterPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_C}
{$define NSDISTRIBUTEDNOTIFICATIONCENTER_PAS_C}

{ NSDistributedNotificationCenter }
  NSDistributedNotificationCenter = objcclass external (NSNotificationCenter)
    
  public
    class function notificationCenterForType(notificationCenterType: NSString): NSDistributedNotificationCenter; message 'notificationCenterForType:';
    class function defaultCenter: id; message 'defaultCenter';
    procedure addObserver_selector_name_object_suspensionBehavior(observer: id; selector: SEL; name: NSString; object_: NSString; suspensionBehavior: NSNotificationSuspensionBehavior); message 'addObserver:selector:name:object:suspensionBehavior:';
    procedure postNotificationName_object_userInfo_deliverImmediately(name: NSString; object_: NSString; userInfo: NSDictionary; deliverImmediately: Boolean); message 'postNotificationName:object:userInfo:deliverImmediately:';
    procedure postNotificationName_object_userInfo_options(name: NSString; object_: NSString; userInfo: NSDictionary; options: NSUInteger); message 'postNotificationName:object:userInfo:options:';
    procedure setSuspended(suspended_: Boolean); message 'setSuspended:';
    function suspended: Boolean; message 'suspended';
    procedure addObserver_selector_name_object(observer: id; aSelector: SEL; aName: NSString; anObject: NSString); message 'addObserver:selector:name:object:';
    procedure postNotificationName_object(aName: NSString; anObject: NSString); message 'postNotificationName:object:';
    procedure postNotificationName_object_userInfo(aName: NSString; anObject: NSString; aUserInfo: NSDictionary); message 'postNotificationName:object:userInfo:';
    procedure removeObserver_name_object(observer: id; aName: NSString; anObject: NSString); message 'removeObserver:name:object:';
  end;

{$endif}
{$endif}
