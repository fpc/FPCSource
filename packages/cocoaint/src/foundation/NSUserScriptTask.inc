{ Parsed from Foundation.framework NSUserScriptTask.h }


{ Types from NSUserScriptTask }
{$ifdef TYPES}

type
  NSUserScriptTaskCompletionHandler = OpaqueCBlock;
{$endif}


{ Types from NSUserUnixTask }
{$ifdef TYPES}

type
  NSUserUnixTaskCompletionHandler = OpaqueCBlock;
{$endif}


{ Types from NSUserAppleScriptTask }
{$ifdef TYPES}

type
  NSUserAppleScriptTaskCompletionHandler = OpaqueCBlock;
{$endif}


{ Types from NSUserAutomatorTask }
{$ifdef TYPES}

type
  NSUserAutomatorTaskCompletionHandler = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  NSUserScriptTaskPtr = ^NSUserScriptTask;
  NSUserUnixTaskPtr = ^NSUserUnixTask;
  NSUserAppleScriptTaskPtr = ^NSUserAppleScriptTask;
  NSUserAutomatorTaskPtr = ^NSUserAutomatorTask;
{$endif}

{$ifdef CLASSES}

type
  NSUserScriptTask = objcclass external (NSObject)
  private
    _scriptURL: NSURL;
    _connection: NSXPCConnection;
    _hasExeced: ObjCBOOL;
    _hasTerminated: ObjCBOOL;
    _stdin: NSFileHandle;
    _stdout: NSFileHandle;
    _stderr: NSFileHandle;
  public
    function initWithURL_error (url: NSURL; error: NSErrorPtr): instancetype; message 'initWithURL:error:'; { NS_DESIGNATED_INITIALIZER }
    function scriptURL: NSURL; message 'scriptURL';
    procedure executeWithCompletionHandler (handler: NSUserScriptTaskCompletionHandler); message 'executeWithCompletionHandler:';
  end;

type
  NSUserUnixTask = objcclass external (NSUserScriptTask)
  public
    procedure setStandardInput(newValue: NSFileHandle); message 'setStandardInput:';
    function standardInput: NSFileHandle; message 'standardInput';
    procedure setStandardOutput(newValue: NSFileHandle); message 'setStandardOutput:';
    function standardOutput: NSFileHandle; message 'standardOutput';
    procedure setStandardError(newValue: NSFileHandle); message 'setStandardError:';
    function standardError: NSFileHandle; message 'standardError';
    procedure executeWithArguments_completionHandler (arguments: NSArray; handler: NSUserUnixTaskCompletionHandler); message 'executeWithArguments:completionHandler:';
  end;

type
  NSUserAppleScriptTask = objcclass external (NSUserScriptTask)
  private
    _isParentDefaultTarget: ObjCBOOL;
  public
    procedure executeWithAppleEvent_completionHandler (event: NSAppleEventDescriptor; handler: NSUserAppleScriptTaskCompletionHandler); message 'executeWithAppleEvent:completionHandler:';
  end;

type
  NSUserAutomatorTask = objcclass external (NSUserScriptTask)
  private
    _variables: NSDictionary;
  public
    procedure setVariables(newValue: NSDictionary); message 'setVariables:';
    function variables: NSDictionary; message 'variables';
    procedure executeWithInput_completionHandler (input: NSSecureCodingProtocol; handler: NSUserAutomatorTaskCompletionHandler); message 'executeWithInput:completionHandler:';
  end;
{$endif}

