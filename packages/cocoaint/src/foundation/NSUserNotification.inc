{ Parsed from Foundation.framework NSUserNotification.h }


{$ifdef TYPES}
type
  NSUserNotificationPtr = ^NSUserNotification;
  NSUserNotificationActionPtr = ^NSUserNotificationAction;
  NSUserNotificationCenterPtr = ^NSUserNotificationCenter;
  NSUserNotificationCenterDelegateProtocolPtr = ^NSUserNotificationCenterDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSUserNotificationActivationType = NSInteger;
  NSUserNotificationActivationTypePtr = ^NSUserNotificationActivationType;

const
  NSUserNotificationActivationTypeNone = 0;
  NSUserNotificationActivationTypeContentsClicked = 1;
  NSUserNotificationActivationTypeActionButtonClicked = 2;
  NSUserNotificationActivationTypeReplied = 3 { available in 10_9, NA };
  NSUserNotificationActivationTypeAdditionalActionClicked = 4 { available in 10_10, NA };
{$endif}

{$ifdef CLASSES}

type
  NSUserNotification = objcclass external (NSObject, NSCopyingProtocol)
  private
    _internal: id;
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setSubtitle(newValue: NSString); message 'setSubtitle:';
    function subtitle: NSString; message 'subtitle';
    procedure setInformativeText(newValue: NSString); message 'setInformativeText:';
    function informativeText: NSString; message 'informativeText';
    procedure setActionButtonTitle(newValue: NSString); message 'setActionButtonTitle:';
    function actionButtonTitle: NSString; message 'actionButtonTitle';
    procedure setUserInfo(newValue: NSDictionary); message 'setUserInfo:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setDeliveryDate(newValue: NSDate); message 'setDeliveryDate:';
    function deliveryDate: NSDate; message 'deliveryDate';
    procedure setDeliveryTimeZone(newValue: NSTimeZone); message 'setDeliveryTimeZone:';
    function deliveryTimeZone: NSTimeZone; message 'deliveryTimeZone';
    procedure setDeliveryRepeatInterval(newValue: NSDateComponents); message 'setDeliveryRepeatInterval:';
    function deliveryRepeatInterval: NSDateComponents; message 'deliveryRepeatInterval';
    function actualDeliveryDate: NSDate; message 'actualDeliveryDate';
    function isPresented: ObjCBOOL; message 'isPresented';
    function isRemote: ObjCBOOL; message 'isRemote';
    procedure setSoundName(newValue: NSString); message 'setSoundName:';
    function soundName: NSString; message 'soundName';
    procedure setHasActionButton(newValue: ObjCBOOL); message 'setHasActionButton:';
    function hasActionButton: ObjCBOOL; message 'hasActionButton';
    function activationType: NSUserNotificationActivationType; message 'activationType';
    procedure setOtherButtonTitle(newValue: NSString); message 'setOtherButtonTitle:';
    function otherButtonTitle: NSString; message 'otherButtonTitle';
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
    procedure setContentImage(newValue: NSImage); message 'setContentImage:';
    function contentImage: NSImage; message 'contentImage';
    procedure setHasReplyButton(newValue: ObjCBOOL); message 'setHasReplyButton:';
    function hasReplyButton: ObjCBOOL; message 'hasReplyButton';
    procedure setResponsePlaceholder(newValue: NSString); message 'setResponsePlaceholder:';
    function responsePlaceholder: NSString; message 'responsePlaceholder';
    function response: NSAttributedString; message 'response';
    procedure setAdditionalActions(newValue: NSArray); message 'setAdditionalActions:';
    function additionalActions: NSArray; message 'additionalActions';
    function additionalActivationAction: NSUserNotificationAction; message 'additionalActivationAction';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;

type
  NSUserNotificationAction = objcclass external (NSObject, NSCopyingProtocol)
  public
    class function actionWithIdentifier_title (identifier: NSString; title: NSString): instancetype; message 'actionWithIdentifier:title:';
    function identifier: NSString; message 'identifier';
    function title: NSString; message 'title';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSUserNotificationDefaultSoundName: NSString { available in 10_8, NA }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSUserNotificationCenter = objcclass external (NSObject)
  private
    _internal: id;
  public
    class function defaultUserNotificationCenter: NSUserNotificationCenter; message 'defaultUserNotificationCenter';
    procedure setDelegate(newValue: NSUserNotificationCenterDelegateProtocol); message 'setDelegate:';
    function delegate: NSUserNotificationCenterDelegateProtocol; message 'delegate';
    procedure setScheduledNotifications(newValue: NSArray); message 'setScheduledNotifications:';
    function scheduledNotifications: NSArray; message 'scheduledNotifications';
    procedure scheduleNotification (notification: NSUserNotification); message 'scheduleNotification:';
    procedure removeScheduledNotification (notification: NSUserNotification); message 'removeScheduledNotification:';
    function deliveredNotifications: NSArray; message 'deliveredNotifications';
    procedure deliverNotification (notification: NSUserNotification); message 'deliverNotification:';
    procedure removeDeliveredNotification (notification: NSUserNotification); message 'removeDeliveredNotification:';
    procedure removeAllDeliveredNotifications; message 'removeAllDeliveredNotifications';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSUserNotificationCenterDelegateProtocol = objcprotocol external name 'NSUserNotificationCenterDelegate' (NSObjectProtocol)
  optional
    procedure userNotificationCenter_didDeliverNotification (center: NSUserNotificationCenter; notification: NSUserNotification); message 'userNotificationCenter:didDeliverNotification:';
    procedure userNotificationCenter_didActivateNotification (center: NSUserNotificationCenter; notification: NSUserNotification); message 'userNotificationCenter:didActivateNotification:';
    function userNotificationCenter_shouldPresentNotification (center: NSUserNotificationCenter; notification: NSUserNotification): ObjCBOOL; message 'userNotificationCenter:shouldPresentNotification:';
  end;
{$endif}

