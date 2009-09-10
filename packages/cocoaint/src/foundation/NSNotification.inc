{ Parsed from Foundation.framework NSNotification.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSNOTIFICATION_PAS_H}
{$define NSNOTIFICATION_PAS_H}
type
  NSNotificationPointer = Pointer;
  NSNotificationCenterPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSNOTIFICATION_PAS_T}
{$define NSNOTIFICATION_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNotification = objcclass;
  NSNotificationCenter = objcclass;

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

    { Category: NSNotificationCreation }
    class function notificationWithName_object(aName: NSString; anObject: id): id; message 'notificationWithName:object:';
    class function notificationWithName_object_userInfo(aName: NSString; anObject: id; aUserInfo: NSDictionary): id; message 'notificationWithName:object:userInfo:';
  end; external;

{ NSNotificationCenter }
  NSNotificationCenter = objcclass(NSObject)
  private
    __impl: Pointer; {garbage collector: __strong }
    __callback_block: Pointer;
    __pad: Pointer;
    
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

{$endif}
{$endif}
