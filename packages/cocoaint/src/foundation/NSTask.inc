{ Parsed from Foundation.framework NSTask.h }


{$ifdef TYPES}
type
  NSTaskPtr = ^NSTask;
{$endif}

{$ifdef TYPES}
type
  NSTaskTerminationReason = NSInteger;
  NSTaskTerminationReasonPtr = ^NSTaskTerminationReason;

const
  NSTaskTerminationReasonExit = 1;
  NSTaskTerminationReasonUncaughtSignal = 2;
{$endif}

{$ifdef CLASSES}

type
  NSTask = objcclass external (NSObject)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    procedure setLaunchPath(newValue: NSString); message 'setLaunchPath:';
    function launchPath: NSString; message 'launchPath';
    procedure setArguments(newValue: NSArray); message 'setArguments:';
    function arguments: NSArray; message 'arguments';
    procedure setEnvironment(newValue: NSDictionary); message 'setEnvironment:';
    function environment: NSDictionary; message 'environment';
    procedure setCurrentDirectoryPath(newValue: NSString); message 'setCurrentDirectoryPath:';
    function currentDirectoryPath: NSString; message 'currentDirectoryPath';
    procedure setStandardInput(newValue: id); message 'setStandardInput:';
    function standardInput: id; message 'standardInput';
    procedure setStandardOutput(newValue: id); message 'setStandardOutput:';
    function standardOutput: id; message 'standardOutput';
    procedure setStandardError(newValue: id); message 'setStandardError:';
    function standardError: id; message 'standardError';
    procedure launch; message 'launch';
    procedure interrupt; message 'interrupt';
    procedure terminate; message 'terminate';
    function suspend: ObjCBOOL; message 'suspend';
    function resume: ObjCBOOL; message 'resume';
    function processIdentifier: cint; message 'processIdentifier';
    function isRunning: ObjCBOOL; message 'isRunning';
    function terminationStatus: cint; message 'terminationStatus';
    function terminationReason: NSTaskTerminationReason; message 'terminationReason';
    procedure setTerminationHandler(newValue: OpaqueCBlock); message 'setTerminationHandler:';
    function terminationHandler: OpaqueCBlock; message 'terminationHandler';
    procedure setQualityOfService(newValue: NSQualityOfService); message 'setQualityOfService:';
    function qualityOfService: NSQualityOfService; message 'qualityOfService';
  end;


type
  NSTaskConveniences = objccategory external (NSTask)
    class function launchedTaskWithLaunchPath_arguments (path: NSString; arguments_: NSArray): NSTask; message 'launchedTaskWithLaunchPath:arguments:';
    procedure waitUntilExit; message 'waitUntilExit';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTaskDidTerminateNotification: NSString; cvar; external;
{$endif}

