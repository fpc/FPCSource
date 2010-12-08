{ Parsed from Foundation.framework NSNotification.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSNOTIFICATION_PAS_T}
{$define NSNOTIFICATION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSNOTIFICATION_PAS_R}
{$define NSNOTIFICATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSNOTIFICATION_PAS_F}
{$define NSNOTIFICATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSNOTIFICATION_PAS_S}
{$define NSNOTIFICATION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNotification = objcclass;
  NSNotificationPointer = ^NSNotification;
  NSNotificationPtr = NSNotificationPointer;
  NSNotificationCenter = objcclass;
  NSNotificationCenterPointer = ^NSNotificationCenter;
  NSNotificationCenterPtr = NSNotificationCenterPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSNOTIFICATION_PAS_C}
{$define NSNOTIFICATION_PAS_C}

{ NSNotification }
  NSNotification = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSNotification; message 'alloc';

    function name: NSString; message 'name';
    function object_: id; message 'object';
    function userInfo: NSDictionary; message 'userInfo';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{ NSNotificationCenter }
  NSNotificationCenter = objcclass(NSObject)
  private
    _impl: Pointer; {garbage collector: __strong }
        _callback_block: array[0..3] of Pointer;
        _pad: array[0..7] of Pointer;
    
  public
    class function alloc: NSNotificationCenter; message 'alloc';

    class function defaultCenter: id; message 'defaultCenter';
    procedure addObserver_selector_name_object(observer: id; aSelector: SEL; aName: NSString; anObject: id); message 'addObserver:selector:name:object:';
    procedure postNotification(notification: NSNotification); message 'postNotification:';
    procedure postNotificationName_object(aName: NSString; anObject: id); message 'postNotificationName:object:';
    procedure postNotificationName_object_userInfo(aName: NSString; anObject: id; aUserInfo: NSDictionary); message 'postNotificationName:object:userInfo:';
    procedure removeObserver(observer: id); message 'removeObserver:';
    procedure removeObserver_name_object(observer: id; aName: NSString; anObject: id); message 'removeObserver:name:object:';
  end; external;

{ NSNotificationCreationCategory }
  NSNotificationCreationCategory = objccategory(NSNotification)
    class function notificationWithName_object(aName: NSString; anObject: id): id; message 'notificationWithName:object:';
    class function notificationWithName_object_userInfo(aName: NSString; anObject: id; aUserInfo: NSDictionary): id; message 'notificationWithName:object:userInfo:';
  end; external;

{$endif}
{$endif}
