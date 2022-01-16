{ Parsed from ScriptingBridge.framework SBApplication.h }


{$ifdef TYPES}
type
  SBApplicationPtr = ^SBApplication;
  SBApplicationDelegateProtocolPtr = ^SBApplicationDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  SBApplication = objcclass external (SBObject, NSCodingProtocol)
  public
    function initWithBundleIdentifier (ident: NSString): id; message 'initWithBundleIdentifier:';
    function initWithURL (url: NSURL): id; message 'initWithURL:';
    function initWithProcessIdentifier (pid: pid_t): id; message 'initWithProcessIdentifier:';
    class function applicationWithBundleIdentifier (ident: NSString): id; message 'applicationWithBundleIdentifier:';
    class function applicationWithURL (url: NSURL): id; message 'applicationWithURL:';
    class function applicationWithProcessIdentifier (pid: pid_t): id; message 'applicationWithProcessIdentifier:';
    function classForScriptingClass (className: NSString): pobjc_class; message 'classForScriptingClass:';
    function isRunning: ObjCBOOL; message 'isRunning';
    procedure activate; message 'activate';
    procedure setDelegate(newValue: SBApplicationDelegateProtocol); message 'setDelegate:';
    function delegate: SBApplicationDelegateProtocol; message 'delegate';
    procedure setLaunchFlags(newValue: LSLaunchFlags); message 'setLaunchFlags:';
    function launchFlags: LSLaunchFlags; message 'launchFlags';
    procedure setSendMode(newValue: AESendMode); message 'setSendMode:';
    function sendMode: AESendMode; message 'sendMode';
    procedure setTimeout(newValue: clong); message 'setTimeout:';
    function timeout: clong; message 'timeout';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  SBApplicationDelegateProtocol = objcprotocol external name 'SBApplicationDelegate'
    function eventDidFail_withError (event: AppleEventPtr; error: NSError): id; message 'eventDidFail:withError:';
  end;
{$endif}

