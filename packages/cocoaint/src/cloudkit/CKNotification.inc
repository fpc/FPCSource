{ Parsed from CloudKit.framework CKNotification.h }


{$ifdef TYPES}
type
  CKNotificationIDPtr = ^CKNotificationID;
  CKNotificationPtr = ^CKNotification;
  CKQueryNotificationPtr = ^CKQueryNotification;
  CKRecordZoneNotificationPtr = ^CKRecordZoneNotification;
{$endif}

{$ifdef CLASSES}

type
  CKNotificationID = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef TYPES}
type
  CKNotificationType = NSInteger;
  CKNotificationTypePtr = ^CKNotificationType;

const
  CKNotificationTypeQuery = 1;
  CKNotificationTypeRecordZone = 2;
  CKNotificationTypeReadNotification = 3;
{$endif}

{$ifdef CLASSES}

type
  CKNotification = objcclass external (NSObject)
  public
    function init: instancetype; message 'init';
    class function notificationFromRemoteNotificationDictionary (notificationDictionary: NSDictionary): instancetype; message 'notificationFromRemoteNotificationDictionary:';
    function notificationType: CKNotificationType; message 'notificationType';
    function notificationID: CKNotificationID; message 'notificationID';
    function containerIdentifier: NSString; message 'containerIdentifier';
    function isPruned: ObjCBOOL; message 'isPruned';
    function alertBody: NSString; message 'alertBody';
    function alertLocalizationKey: NSString; message 'alertLocalizationKey';
    function alertLocalizationArgs: NSArray; message 'alertLocalizationArgs';
    function alertActionLocalizationKey: NSString; message 'alertActionLocalizationKey';
    function alertLaunchImage: NSString; message 'alertLaunchImage';
    function badge: NSNumber; message 'badge';
    function soundName: NSString; message 'soundName';
  end;
{$endif}

{$ifdef TYPES}
type
  CKQueryNotificationReason = NSInteger;
  CKQueryNotificationReasonPtr = ^CKQueryNotificationReason;

const
  CKQueryNotificationReasonRecordCreated = 1;
  CKQueryNotificationReasonRecordUpdated = 2;
  CKQueryNotificationReasonRecordDeleted = 3;
{$endif}

{$ifdef CLASSES}

type
  CKQueryNotification = objcclass external (CKNotification)
  public
    function queryNotificationReason: CKQueryNotificationReason; message 'queryNotificationReason';
    function recordFields: NSDictionary; message 'recordFields';
    function recordID: CKRecordID; message 'recordID';
    function isPublicDatabase: ObjCBOOL; message 'isPublicDatabase';
  end;

type
  CKRecordZoneNotification = objcclass external (CKNotification)
  public
    function recordZoneID: CKRecordZoneID; message 'recordZoneID';
  end;
{$endif}

