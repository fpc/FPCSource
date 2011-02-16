{ Parsed from Foundation.framework NSTask.h }

{$ifdef TYPES}
{$ifndef NSTASK_PAS_T}
{$define NSTASK_PAS_T}

{ Constants }

const
  NSTaskTerminationReasonExit = 1;
  NSTaskTerminationReasonUncaughtSignal = 2;

{ Types }
type
  NSTaskTerminationReason = NSInteger;
  NSTaskTerminationReasonPtr = ^NSTaskTerminationReason;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTASK_PAS_R}
{$define NSTASK_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTASK_PAS_F}
{$define NSTASK_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTASK_PAS_S}
{$define NSTASK_PAS_S}

{ External string constants }
var
  NSTaskDidTerminateNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTask = objcclass;
  NSTaskPointer = ^NSTask;
  NSTaskPtr = NSTaskPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTASK_PAS_C}
{$define NSTASK_PAS_C}

{ NSTask }
  NSTask = objcclass external (NSObject)
    
  public
    function init: id; message 'init';
    procedure setLaunchPath(path: NSString); message 'setLaunchPath:';
    procedure setArguments(arguments_: NSArray); message 'setArguments:';
    procedure setEnvironment(dict: NSDictionary); message 'setEnvironment:';
    procedure setCurrentDirectoryPath(path: NSString); message 'setCurrentDirectoryPath:';
    procedure setStandardInput(input: id); message 'setStandardInput:';
    procedure setStandardOutput(output: id); message 'setStandardOutput:';
    procedure setStandardError(error: id); message 'setStandardError:';
    function launchPath: NSString; message 'launchPath';
    function arguments: NSArray; message 'arguments';
    function environment: NSDictionary; message 'environment';
    function currentDirectoryPath: NSString; message 'currentDirectoryPath';
    function standardInput: id; message 'standardInput';
    function standardOutput: id; message 'standardOutput';
    function standardError: id; message 'standardError';
    procedure launch; message 'launch';
    procedure interrupt; message 'interrupt';
    procedure terminate; message 'terminate';
    function suspend: Boolean; message 'suspend';
    function resume: Boolean; message 'resume';
    function processIdentifier: cint; message 'processIdentifier';
    function isRunning: Boolean; message 'isRunning';
    function terminationStatus: cint; message 'terminationStatus';
    function terminationReason: NSTaskTerminationReason; message 'terminationReason';
  end;

{ NSTaskConveniencesCategory }
  NSTaskConveniencesCategory = objccategory external (NSTask)
    class function launchedTaskWithLaunchPath_arguments(path: NSString; arguments_: NSArray): NSTask; message 'launchedTaskWithLaunchPath:arguments:';
    procedure waitUntilExit; message 'waitUntilExit';
  end;

{$endif}
{$endif}
