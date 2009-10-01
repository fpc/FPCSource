{ Parsed from Foundation.framework NSTask.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSTASK_PAS_T}
{$define NSTASK_PAS_T}

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
  NSTaskDidTerminateNotification: NSString; external name '_NSTaskDidTerminateNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTask = objcclass;
  NSTaskPointer = ^NSTask;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTASK_PAS_C}
{$define NSTASK_PAS_C}

{ NSTask }
  NSTask = objcclass(NSObject)
    
  public
    class function alloc: NSTask; message 'alloc';

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

    { Category: NSTaskConveniences }
    class function launchedTaskWithLaunchPath_arguments(path: NSString; arguments_: NSArray): NSTask; message 'launchedTaskWithLaunchPath:arguments:';
    procedure waitUntilExit; message 'waitUntilExit';
  end; external;

{$endif}
{$endif}
