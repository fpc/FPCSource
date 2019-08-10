{ Parsed from Foundation.framework NSNotification.h }


{$ifdef TYPES}
type
  NSNotificationPtr = ^NSNotification;
  NSNotificationCenterPtr = ^NSNotificationCenter;
{$endif}

{$ifdef CLASSES}

type
  NSNotification = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  public
    function name: NSString; message 'name';
    function object_: id; message 'object';
    function userInfo: NSDictionary; message 'userInfo';
    function initWithName_object_userInfo (name_: NSString; object_: id; userInfo_: NSDictionary): instancetype; message 'initWithName:object:userInfo:'; { available in 10_6, 4_0 }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;


type
  NSNotificationCreation = objccategory external (NSNotification)
    class function notificationWithName_object (aName: NSString; anObject: id): instancetype; message 'notificationWithName:object:';
    class function notificationWithName_object_userInfo (aName: NSString; anObject: id; aUserInfo: NSDictionary): instancetype; message 'notificationWithName:object:userInfo:';
    function init: instancetype; message 'init';
  end;


type
  NSNotificationCenter = objcclass external (NSObject)
  private
    _impl: pointer;
    _callback: pointer;
    _pad:array[0..10] of pointer;
  public
    class function defaultCenter: NSNotificationCenter; message 'defaultCenter';
    procedure addObserver_selector_name_object (observer: id; aSelector: SEL; aName: NSString; anObject: id); message 'addObserver:selector:name:object:';
    procedure postNotification (notification: NSNotification); message 'postNotification:';
    procedure postNotificationName_object (aName: NSString; anObject: id); message 'postNotificationName:object:';
    procedure postNotificationName_object_userInfo (aName: NSString; anObject: id; aUserInfo: NSDictionary); message 'postNotificationName:object:userInfo:';
    procedure removeObserver (observer: id); message 'removeObserver:';
    procedure removeObserver_name_object (observer: id; aName: NSString; anObject: id); message 'removeObserver:name:object:';
    function addObserverForName_object_queue_usingBlock (name: NSString; obj: id; queue: NSOperationQueue; block: OpaqueCBlock): NSObjectProtocol; message 'addObserverForName:object:queue:usingBlock:'; { available in 10_6, 4_0 }
  end;
{$endif}

