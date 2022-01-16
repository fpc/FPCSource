{ Parsed from InstantMessage.framework IMService.h }


{ Types from IMService }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  IMServicePtr = ^IMService;
{$endif}

{$ifndef _IMSERVICE_H_}
{$ifdef EXTERNAL_SYMBOLS}
var
  IMServiceStatusChangedNotification: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMMyStatusChangedNotification: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonStatusChangedNotification: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonInfoChangedNotification: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMStatusImagesChangedAppearanceNotification: NSString deprecated 'in 10_5, 10_9'; cvar; external;
{$endif}

{$ifdef TYPES}
const
  IMServiceStatusLoggedOut = 0;
  IMServiceStatusDisconnected = 1;
  IMServiceStatusLoggingOut = 2;
  IMServiceStatusLoggingIn = 3;
  IMServiceStatusLoggedIn = 4;

type
  IMServiceStatus = NSUInteger deprecated 'in 10_5, 10_9';
  IMServiceStatusPtr = ^IMServiceStatus deprecated 'in 10_5, 10_9';

const
  IMPersonStatusUnknown = 0;
  IMPersonStatusOffline = 1;
  IMPersonStatusIdle = 2;
  IMPersonStatusAway = 3;
  IMPersonStatusAvailable = 4;
  {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
  IMPersonStatusNoStatus = 5;
  {$endif}

type
  IMPersonStatus = NSUInteger deprecated 'in 10_5, 10_9';
  IMPersonStatusPtr = ^IMPersonStatus deprecated 'in 10_5, 10_9';
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  IMPersonServiceNameKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonScreenNameKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonStatusKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonStatusMessageKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonIdleSinceKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonFirstNameKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonLastNameKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonEmailKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonPictureDataKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonAVBusyKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMPersonCapabilitiesKey: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMCapabilityText: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMCapabilityDirectIM: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMCapabilityFileTransfer: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMCapabilityFileSharing: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMCapabilityAudioConference: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMCapabilityVideoConference: NSString deprecated 'in 10_5, 10_9'; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  IMService = objcclass external (NSObject)
  private
  public
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    class function imageNameForStatus (status: IMPersonStatus): NSString; message 'imageNameForStatus:'; deprecated 'in 10_5, 10_9';
    {$endif}
    class function allServices: NSArray; message 'allServices'; deprecated 'in 10_5, 10_9';
    class function serviceWithName (name: NSString): IMService; message 'serviceWithName:'; deprecated 'in 10_5, 10_9';
    class function notificationCenter: NSNotificationCenter; message 'notificationCenter'; deprecated 'in 10_5, 10_9';
    class function myStatus: IMPersonStatus; message 'myStatus'; deprecated 'in 10_5, 10_9';
    class function myIdleTime: NSDate; message 'myIdleTime'; deprecated 'in 10_5, 10_9';
    function localizedName: NSString; message 'localizedName'; deprecated 'in 10_5, 10_9';
    function localizedShortName: NSString; message 'localizedShortName'; deprecated 'in 10_5, 10_9';
    function name: NSString; message 'name'; deprecated 'in 10_5, 10_9';
    function status: IMServiceStatus; message 'status'; deprecated 'in 10_5, 10_9';
    function infoForScreenName (screenName: NSString): NSDictionary; message 'infoForScreenName:'; deprecated 'in 10_5, 10_9';
    function infoForAllScreenNames: NSArray; message 'infoForAllScreenNames'; deprecated 'in 10_5, 10_9';
    function infoForPreferredScreenNames: NSArray; message 'infoForPreferredScreenNames'; deprecated 'in 10_5, 10_9';
    function peopleWithScreenName (screenName: NSString): NSArray; message 'peopleWithScreenName:'; deprecated 'in 10_5, 10_9';
    function screenNamesForPerson (person: ABPerson): NSArray; message 'screenNamesForPerson:'; deprecated 'in 10_5, 10_9';
  end;


type
  IMService_NSDeprecatedMethods = objccategory external name 'NSDeprecatedMethods' (IMService)
    class function imageURLForStatus (status_: IMPersonStatus): NSURL; message 'imageURLForStatus:'; deprecated 'available in 10.4 and later but deprecated in 10.5';
  end;
{$endif}

{$endif}
