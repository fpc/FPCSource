{ Parsed from Automator.framework AMAction.h }


{ Types from AMAction }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  AMActionPtr = ^AMAction;
{$endif}

{$ifdef TYPES}
type
  AMLogLevel = NSUInteger;
  AMLogLevelPtr = ^AMLogLevel;

const
  AMLogLevelDebug = 0;
  AMLogLevelInfo = 1;
  AMLogLevelWarn = 2;
  AMLogLevelError = 3;
{$endif}

{$ifdef CLASSES}

type
  AMAction = objcclass external (NSObject)
  private
    _definition: NSMutableDictionary;
    _argumentsWithOutlets: NSDictionary;
    _currentInput: NSString;
    _error: NSString;
    _output: NSString;
    _currentRunner: id;
    _loopParent: AMAction;
    _actionDescription: NSString;
    _icon: NSImage;
    _relevance: NSUInteger;
    _showsRelevance: ObjCBOOL;
    _stopped: ObjCBOOL;
    _future: id;
    _future2: id;
    _future3: id;
    _future4: id;
  public
    function initWithDefinition_fromArchive (dict: NSDictionary; archived: ObjCBOOL): instancetype; message 'initWithDefinition:fromArchive:';
    function initWithContentsOfURL_error (fileURL: NSURL; outError: NSErrorPtr): instancetype; message 'initWithContentsOfURL:error:';
    {$ifndef __LP64__}
    function definition: NSMutableDictionary; message 'definition'; deprecated 'available in 10.4 and later but deprecated';
    {$endif}
    function name: NSString; message 'name';
    function ignoresInput: ObjCBOOL; message 'ignoresInput';
    procedure setSelectedInputType(newValue: NSString); message 'setSelectedInputType:';
    function selectedInputType: NSString; message 'selectedInputType';
    procedure setSelectedOutputType(newValue: NSString); message 'setSelectedOutputType:';
    function selectedOutputType: NSString; message 'selectedOutputType';
    procedure setProgressValue(newValue: CGFloat); message 'setProgressValue:';
    function progressValue: CGFloat; message 'progressValue';
    function runWithInput_fromAction_error (input: id; anAction: AMAction; errorInfo: NSDictionaryPtr): id; message 'runWithInput:fromAction:error:'; deprecated 'available in 10.4 and later but deprecated';
    function runWithInput_error (input: id; error: NSErrorPtr): id; message 'runWithInput:error:';
    procedure runAsynchronouslyWithInput (input: id); message 'runAsynchronouslyWithInput:';
    procedure willFinishRunning; message 'willFinishRunning';
    procedure didFinishRunningWithError (errorInfo: NSDictionary); message 'didFinishRunningWithError:'; deprecated 'available in 10.5 and later but deprecated';
    procedure finishRunningWithError (error: NSError); message 'finishRunningWithError:';
    procedure setOutput(newValue: id); message 'setOutput:';
    function output: id; message 'output';
    procedure stop; message 'stop';
    procedure reset; message 'reset';
    procedure writeToDictionary (dictionary: NSMutableDictionary); message 'writeToDictionary:';
    procedure opened; message 'opened';
    procedure activated; message 'activated';
    procedure closed; message 'closed';
    procedure updateParameters; message 'updateParameters';
    procedure parametersUpdated; message 'parametersUpdated';
    procedure logMessageWithLevel_format (level: AMLogLevel; format: NSString); varargs; message 'logMessageWithLevel:format:';
    function isStopped: ObjCBOOL; message 'isStopped';
  end;
{$endif}

