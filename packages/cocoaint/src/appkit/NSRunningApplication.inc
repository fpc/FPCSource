{ Parsed from AppKit.framework NSRunningApplication.h }


{$ifdef TYPES}
type
  NSRunningApplicationPtr = ^NSRunningApplication;
{$endif}

{$ifdef TYPES}
type
  NSApplicationActivationOptions = NSUInteger;
  NSApplicationActivationOptionsPtr = ^NSApplicationActivationOptions;

const
  NSApplicationActivateAllWindows = 1 shl 0;
  NSApplicationActivateIgnoringOtherApps = 1 shl 1;

type
  NSApplicationActivationPolicy = NSInteger;
  NSApplicationActivationPolicyPtr = ^NSApplicationActivationPolicy;

const
  NSApplicationActivationPolicyRegular = 0;
  NSApplicationActivationPolicyAccessory = 1;
  NSApplicationActivationPolicyProhibited = 2;
{$endif}

{$ifdef CLASSES}

type
  NSRunningApplication = objcclass external (NSObject)
  private
    _superReserved: id;
    _asn: pointer;
    _helpers: pointer;
    _obsInfo: id;
    _lock: NSLock;
    _bundleID: NSString;
    _localizedName: NSString;
    _bundleURL: NSURL;
    _executableURL: NSURL;
    _launchDate: NSDate;
    _icon: NSImage;
    _pid: cint;
    _lastSeed: cuint;
    _activeSeed: cuint;
    _staleSeed: cuint;
    _obsMask: culonglong;
    _aflags: bitpacked record
      case byte of
        0: (_anonBitField__aflags0: cuint);
        1: (
          fetchedDynamic: 0..1;
          deadToUs: 0..1;
          terminated: 0..1;
          finishedLaunching: 0..1;
          hidden: 0..1;
          active: 0..1;
          ownsMenuBar: 0..1;
          arch: 0..((1 shl 3)-1);
          activationPolicy: 0..((1 shl 3)-1);
          reserved1: 0..((1 shl 19)-1);
        );
      end;
    _appReserved: id;
  public
    function isTerminated: ObjCBOOL; message 'isTerminated';
    function isFinishedLaunching: ObjCBOOL; message 'isFinishedLaunching';
    function isHidden: ObjCBOOL; message 'isHidden';
    function isActive: ObjCBOOL; message 'isActive';
    function ownsMenuBar: ObjCBOOL; message 'ownsMenuBar';
    function activationPolicy: NSApplicationActivationPolicy; message 'activationPolicy';
    function localizedName: NSString; message 'localizedName';
    function bundleIdentifier: NSString; message 'bundleIdentifier';
    function bundleURL: NSURL; message 'bundleURL';
    function executableURL: NSURL; message 'executableURL';
    function processIdentifier: pid_t; message 'processIdentifier';
    function launchDate: NSDate; message 'launchDate';
    function icon: NSImage; message 'icon';
    function executableArchitecture: NSInteger; message 'executableArchitecture';
    function hide: ObjCBOOL; message 'hide';
    function unhide: ObjCBOOL; message 'unhide';
    function activateWithOptions (options: NSApplicationActivationOptions): ObjCBOOL; message 'activateWithOptions:';
    function terminate: ObjCBOOL; message 'terminate';
    function forceTerminate: ObjCBOOL; message 'forceTerminate';
    class function runningApplicationsWithBundleIdentifier (bundleIdentifier_: NSString): NSArray; message 'runningApplicationsWithBundleIdentifier:';
    class function runningApplicationWithProcessIdentifier (pid: pid_t): instancetype; message 'runningApplicationWithProcessIdentifier:';
    class function currentApplication: instancetype; message 'currentApplication';
    class procedure terminateAutomaticallyTerminableApplications; message 'terminateAutomaticallyTerminableApplications';
  end;


type
  NSWorkspaceRunningApplications = objccategory external (NSWorkspace)
    function runningApplications: NSArray; message 'runningApplications';
  end;
{$endif}

