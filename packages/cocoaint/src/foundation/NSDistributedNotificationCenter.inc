{ Parsed from Foundation.framework NSDistributedNotificationCenter.h }


{$ifdef TYPES}
type
  NSDistributedNotificationCenterPtr = ^NSDistributedNotificationCenter;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSLocalNotificationCenterType: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSNotificationSuspensionBehavior = NSUInteger;
  NSNotificationSuspensionBehaviorPtr = ^NSNotificationSuspensionBehavior;

const
  NSNotificationSuspensionBehaviorDrop = 1;
  NSNotificationSuspensionBehaviorCoalesce = 2;
  NSNotificationSuspensionBehaviorHold = 3;
  NSNotificationSuspensionBehaviorDeliverImmediately = 4;

const
	NSNotificationDeliverImmediately = 1 shl 0;
	NSNotificationPostToAllSessions = 1 shl 1;
	
{$endif}

{$ifdef CLASSES}

type
  NSDistributedNotificationCenter = objcclass external (NSNotificationCenter)
  private
  public
		class function notificationCenterForType (notificationCenterType: NSString): NSDistributedNotificationCenter; message 'notificationCenterForType:';
		class function defaultCenter: NSDistributedNotificationCenter; message 'defaultCenter';
		procedure addObserver_selector_name_object_suspensionBehavior (observer: id; aSelector: SEL; aName: NSString; anObject: NSString; suspensionBehavior: NSNotificationSuspensionBehavior); message 'addObserver:selector:name:object:suspensionBehavior:';
    procedure postNotificationName_object_userInfo_deliverImmediately (aName: NSString; anObject: NSString; aUserInfo: NSDictionary; deliverImmediately: ObjCBOOL); message 'postNotificationName:object:userInfo:deliverImmediately:';
    procedure postNotificationName_object_userInfo_options (name: NSString; object_: NSString; userInfo: NSDictionary; options: NSUInteger); message 'postNotificationName:object:userInfo:options:';
    procedure setSuspended(newValue: ObjCBOOL); message 'setSuspended:';
    function suspended: ObjCBOOL; message 'suspended';
    procedure addObserver_selector_name_object (observer: id; aSelector: SEL; aName: NSString; anObject: NSString); message 'addObserver:selector:name:object:';
    procedure postNotificationName_object (aName: NSString; anObject: NSString); message 'postNotificationName:object:';
    procedure postNotificationName_object_userInfo (aName: NSString; anObject: NSString; aUserInfo: NSDictionary); message 'postNotificationName:object:userInfo:';
    procedure removeObserver_name_object (observer: id; aName: NSString; anObject: NSString); message 'removeObserver:name:object:';
  end;
{$endif}

