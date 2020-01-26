{ Parsed from CloudKit.framework CKSubscription.h }


{$ifdef TYPES}
type
  CKSubscriptionPtr = ^CKSubscription;
  CKNotificationInfoPtr = ^CKNotificationInfo;
{$endif}

{$ifdef TYPES}
type
  CKSubscriptionType = NSInteger;
  CKSubscriptionTypePtr = ^CKSubscriptionType;

const
  CKSubscriptionTypeQuery = 1;
  CKSubscriptionTypeRecordZone = 2;

type
  CKSubscriptionOptions = NSUInteger;
  CKSubscriptionOptionsPtr = ^CKSubscriptionOptions;

const
  CKSubscriptionOptionsFiresOnRecordCreation = 1 shl 0;
  CKSubscriptionOptionsFiresOnRecordUpdate = 1 shl 1;
  CKSubscriptionOptionsFiresOnRecordDeletion = 1 shl 2;
  CKSubscriptionOptionsFiresOnce = 1 shl 3;
{$endif}

{$ifdef CLASSES}

type
  CKSubscription = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  public
    function init: instancetype; message 'init';
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function initWithRecordType_predicate_options (recordType: NSString; predicate: NSPredicate; subscriptionOptions: CKSubscriptionOptions): instancetype; message 'initWithRecordType:predicate:options:';
    function initWithRecordType_predicate_subscriptionID_options (recordType: NSString; predicate: NSPredicate; subscriptionID: NSString; subscriptionOptions: CKSubscriptionOptions): instancetype; message 'initWithRecordType:predicate:subscriptionID:options:'; { NS_DESIGNATED_INITIALIZER }
    function initWithZoneID_options (zoneID: CKRecordZoneID; subscriptionOptions: CKSubscriptionOptions): instancetype; message 'initWithZoneID:options:';
    function initWithZoneID_subscriptionID_options (zoneID: CKRecordZoneID; subscriptionID: NSString; subscriptionOptions: CKSubscriptionOptions): instancetype; message 'initWithZoneID:subscriptionID:options:'; { NS_DESIGNATED_INITIALIZER }
    function subscriptionID: NSString; message 'subscriptionID';
    function subscriptionType: CKSubscriptionType; message 'subscriptionType';
    function recordType: NSString; message 'recordType';
    function predicate: NSPredicate; message 'predicate';
    function subscriptionOptions: CKSubscriptionOptions; message 'subscriptionOptions';
    procedure setNotificationInfo(newValue: CKNotificationInfo); message 'setNotificationInfo:';
    function notificationInfo: CKNotificationInfo; message 'notificationInfo';
    procedure setZoneID(newValue: CKRecordZoneID); message 'setZoneID:';
    function zoneID: CKRecordZoneID; message 'zoneID';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  CKNotificationInfo = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  public
    procedure setAlertBody(newValue: NSString); message 'setAlertBody:';
    function alertBody: NSString; message 'alertBody';
    procedure setAlertLocalizationKey(newValue: NSString); message 'setAlertLocalizationKey:';
    function alertLocalizationKey: NSString; message 'alertLocalizationKey';
    procedure setAlertLocalizationArgs(newValue: NSArray); message 'setAlertLocalizationArgs:';
    function alertLocalizationArgs: NSArray; message 'alertLocalizationArgs';
    procedure setAlertActionLocalizationKey(newValue: NSString); message 'setAlertActionLocalizationKey:';
    function alertActionLocalizationKey: NSString; message 'alertActionLocalizationKey';
    procedure setAlertLaunchImage(newValue: NSString); message 'setAlertLaunchImage:';
    function alertLaunchImage: NSString; message 'alertLaunchImage';
    procedure setSoundName(newValue: NSString); message 'setSoundName:';
    function soundName: NSString; message 'soundName';
    procedure setDesiredKeys(newValue: NSArray); message 'setDesiredKeys:';
    function desiredKeys: NSArray; message 'desiredKeys';
    procedure setShouldBadge(newValue: ObjCBOOL); message 'setShouldBadge:';
    function shouldBadge: ObjCBOOL; message 'shouldBadge';
    procedure setShouldSendContentAvailable(newValue: ObjCBOOL); message 'setShouldSendContentAvailable:';
    function shouldSendContentAvailable: ObjCBOOL; message 'shouldSendContentAvailable';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

