{ Parsed from Foundation.framework NSOperation.h }


{ Types from NSOperation }
{$ifdef TYPES}

type
  NSOperationQueuePriority = NSInteger;
type
  NSOperationQueuePriorityPtr = ^NSOperationQueuePriority;

const
  NSOperationQueuePriorityVeryLow = -8;
  NSOperationQueuePriorityLow = -4;
  NSOperationQueuePriorityNormal = 0;
  NSOperationQueuePriorityHigh = 4;
  NSOperationQueuePriorityVeryHigh = 8;
{$endif}


{ Types from NSOperationQueue }
{$ifdef TYPES}

const
  NSOperationQueueDefaultMaxConcurrentOperationCount = -1;
{$endif}


{$ifdef TYPES}
type
  NSOperationPtr = ^NSOperation;
  NSBlockOperationPtr = ^NSBlockOperation;
  NSInvocationOperationPtr = ^NSInvocationOperation;
  NSOperationQueuePtr = ^NSOperationQueue;
{$endif}

{$ifdef TYPES}
const
  NSOperationQualityOfServiceUserInteractive = NSQualityOfServiceUserInteractive;
  NSOperationQualityOfServiceUserInitiated = NSQualityOfServiceUserInitiated;
  NSOperationQualityOfServiceUtility = NSQualityOfServiceUtility;
{$endif}

{$ifdef CLASSES}

type
  NSOperation = objcclass external (NSObject)
  private
    _private: id;
    _private1: cint32;
    {$if defined(__LP64__)}
    _private1b: cint32;
    {$endif}
  public
    procedure start; message 'start';
    procedure main; message 'main';
    function isCancelled: ObjCBOOL; message 'isCancelled';
    procedure cancel; message 'cancel';
    function isExecuting: ObjCBOOL; message 'isExecuting';
    function isFinished: ObjCBOOL; message 'isFinished';
    function isConcurrent: ObjCBOOL; message 'isConcurrent';
    function isAsynchronous: ObjCBOOL; message 'isAsynchronous';
    function isReady: ObjCBOOL; message 'isReady';
    procedure addDependency (op: NSOperation); message 'addDependency:';
    procedure removeDependency (op: NSOperation); message 'removeDependency:';
    function dependencies: NSArray; message 'dependencies';
    procedure setQueuePriority(newValue: NSOperationQueuePriority); message 'setQueuePriority:';
    function queuePriority: NSOperationQueuePriority; message 'queuePriority';
    procedure setCompletionBlock(newValue: OpaqueCBlock); message 'setCompletionBlock:';
    function completionBlock: OpaqueCBlock; message 'completionBlock';
    procedure waitUntilFinished; message 'waitUntilFinished'; { available in 10_6, 4_0 }
    procedure setThreadPriority(newValue: double); message 'setThreadPriority:';
    function threadPriority: double; message 'threadPriority';
    procedure setQualityOfService(newValue: NSQualityOfService); message 'setQualityOfService:';
    function qualityOfService: NSQualityOfService; message 'qualityOfService';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
  end;

type
  NSBlockOperation = objcclass external (NSOperation)
  private
    _private2: id;
    _reserved2: pointer;
  public
    class function blockOperationWithBlock (block: OpaqueCBlock): instancetype; message 'blockOperationWithBlock:';
    procedure addExecutionBlock (block: OpaqueCBlock); message 'addExecutionBlock:';
    function executionBlocks: NSArray; message 'executionBlocks';
  end;

type
  NSInvocationOperation = objcclass external (NSOperation)
  private
    _inv: id;
    _exception: id;
    _reserved2: pointer;
  public
    function initWithTarget_selector_object (target: id; sel_: SEL; arg: id): instancetype; message 'initWithTarget:selector:object:';
    function initWithInvocation (inv: NSInvocation): instancetype; message 'initWithInvocation:'; { NS_DESIGNATED_INITIALIZER }
    function invocation: NSInvocation; message 'invocation';
    function result_: id; message 'result';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSInvocationOperationVoidResultException: NSString { available in 10_5, 2_0 }; cvar; external;
  NSInvocationOperationCancelledException: NSString { available in 10_5, 2_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSOperationQueue = objcclass external (NSObject)
  private
    _private: id;
    _reserved: pointer;
  public
    procedure addOperation (op: NSOperation); message 'addOperation:';
    procedure addOperations_waitUntilFinished (ops: NSArray; wait: ObjCBOOL); message 'addOperations:waitUntilFinished:'; { available in 10_6, 4_0 }
    procedure addOperationWithBlock (block: OpaqueCBlock); message 'addOperationWithBlock:'; { available in 10_6, 4_0 }
    function operations: NSArray; message 'operations';
    function operationCount: NSUInteger; message 'operationCount';
    procedure setMaxConcurrentOperationCount(newValue: NSInteger); message 'setMaxConcurrentOperationCount:';
    function maxConcurrentOperationCount: NSInteger; message 'maxConcurrentOperationCount';
    procedure setSuspended(newValue: ObjCBOOL); message 'setSuspended:';
    function isSuspended: ObjCBOOL; message 'isSuspended';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setQualityOfService(newValue: NSQualityOfService); message 'setQualityOfService:';
    function qualityOfService: NSQualityOfService; message 'qualityOfService';
    procedure setUnderlyingQueue(newValue: dispatch_queue_t); message 'setUnderlyingQueue:';
    function underlyingQueue: dispatch_queue_t; message 'underlyingQueue';
    procedure cancelAllOperations; message 'cancelAllOperations';
    procedure waitUntilAllOperationsAreFinished; message 'waitUntilAllOperationsAreFinished';
    class function currentQueue: NSOperationQueue; message 'currentQueue'; { available in 10_6, 4_0 }
    class function mainQueue: NSOperationQueue; message 'mainQueue'; { available in 10_6, 4_0 }
  end;
{$endif}

