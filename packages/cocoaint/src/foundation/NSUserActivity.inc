{ Parsed from Foundation.framework NSUserActivity.h }


{$ifdef TYPES}
type
  NSUserActivityPtr = ^NSUserActivity;
  NSUserActivityDelegateProtocolPtr = ^NSUserActivityDelegateProtocol;
{$endif}

{$if defined(__OBJC2__)}
{$ifdef CLASSES}

type
  NSUserActivity = objcclass external (NSObject)
  public
    function initWithActivityType (activityType: NSString): instancetype; message 'initWithActivityType:';
    function init: instancetype; message 'init';
    function activityType: NSString; message 'activityType';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setUserInfo(newValue: NSDictionary); message 'setUserInfo:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure addUserInfoEntriesFromDictionary (otherDictionary: NSDictionary); message 'addUserInfoEntriesFromDictionary:';
    procedure setNeedsSave(newValue: ObjCBOOL); message 'setNeedsSave:';
    function needsSave: ObjCBOOL; message 'needsSave';
    procedure setWebpageURL(newValue: NSURL); message 'setWebpageURL:';
    function webpageURL: NSURL; message 'webpageURL';
    procedure setSupportsContinuationStreams(newValue: ObjCBOOL); message 'setSupportsContinuationStreams:';
    function supportsContinuationStreams: ObjCBOOL; message 'supportsContinuationStreams';
    procedure setDelegate(newValue: NSUserActivityDelegateProtocol); message 'setDelegate:';
    function delegate: NSUserActivityDelegateProtocol; message 'delegate';
    procedure becomeCurrent; message 'becomeCurrent';
    procedure invalidate; message 'invalidate';
    procedure getContinuationStreamsWithCompletionHandler (completionHandler: OpaqueCBlock); message 'getContinuationStreamsWithCompletionHandler:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSUserActivityTypeBrowsingWeb: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  NSUserActivityDelegateProtocol = objcprotocol external name 'NSUserActivityDelegate' (NSObjectProtocol)
  optional
    procedure userActivityWillSave (userActivity: NSUserActivity); message 'userActivityWillSave:';
    procedure userActivityWasContinued (userActivity: NSUserActivity); message 'userActivityWasContinued:';
    procedure userActivity_didReceiveInputStream_outputStream (userActivity: NSUserActivity; inputStream: NSInputStream; outputStream: NSOutputStream); message 'userActivity:didReceiveInputStream:outputStream:';
  end;
{$endif}

{$endif}
