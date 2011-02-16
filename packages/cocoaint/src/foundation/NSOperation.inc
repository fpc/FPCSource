{ Parsed from Foundation.framework NSOperation.h }

{$ifdef TYPES}
{$ifndef NSOPERATION_PAS_T}
{$define NSOPERATION_PAS_T}

{ Constants }

const
  NSOperationQueuePriorityVeryLow = -8;
  NSOperationQueuePriorityLow = -4;
  NSOperationQueuePriorityNormal = 0;
  NSOperationQueuePriorityHigh = 4;
  NSOperationQueuePriorityVeryHigh = 8;

const
  NSOperationQueueDefaultMaxConcurrentOperationCount = -1;

{ Types }
type
  NSOperationQueuePriority = NSInteger;
  NSOperationQueuePriorityPtr = ^NSOperationQueuePriority;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOPERATION_PAS_R}
{$define NSOPERATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOPERATION_PAS_F}
{$define NSOPERATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOPERATION_PAS_S}
{$define NSOPERATION_PAS_S}

{ External string constants }
var
  NSInvocationOperationVoidResultException: NSString; cvar; external;
  NSInvocationOperationCancelledException: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOperation = objcclass;
  NSOperationPointer = ^NSOperation;
  NSOperationPtr = NSOperationPointer;
  NSInvocationOperation = objcclass;
  NSInvocationOperationPointer = ^NSInvocationOperation;
  NSInvocationOperationPtr = NSInvocationOperationPointer;
  NSOperationQueue = objcclass;
  NSOperationQueuePointer = ^NSOperationQueue;
  NSOperationQueuePtr = NSOperationQueuePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOPERATION_PAS_C}
{$define NSOPERATION_PAS_C}

{ NSOperation }
  NSOperation = objcclass external (NSObject)
  private
    _private: id;
    _reserved: Pointer;
    
  public
    function init: id; message 'init';
    procedure start; message 'start';
    procedure main; message 'main';
    function isCancelled: Boolean; message 'isCancelled';
    procedure cancel; message 'cancel';
    function isExecuting: Boolean; message 'isExecuting';
    function isFinished: Boolean; message 'isFinished';
    function isConcurrent: Boolean; message 'isConcurrent';
    function isReady: Boolean; message 'isReady';
    procedure addDependency(op: NSOperation); message 'addDependency:';
    procedure removeDependency(op: NSOperation); message 'removeDependency:';
    function dependencies: NSArray; message 'dependencies';
    function queuePriority: NSOperationQueuePriority; message 'queuePriority';
    procedure setQueuePriority(p: NSOperationQueuePriority); message 'setQueuePriority:';
    procedure waitUntilFinished; message 'waitUntilFinished';
    function threadPriority: double; message 'threadPriority';
    procedure setThreadPriority(p: double); message 'setThreadPriority:';
  end;

{ NSInvocationOperation }
  NSInvocationOperation = objcclass external (NSOperation)
  private
    _inv: id;
    _exception: id;
    _reserved2: Pointer;
    
  public
    function initWithTarget_selector_object(target: id; sel_: SEL; arg: id): id; message 'initWithTarget:selector:object:';
    function initWithInvocation(inv: NSInvocation): id; message 'initWithInvocation:';
    function invocation: NSInvocation; message 'invocation';
    function result_: id; message 'result';
  end;

{ NSOperationQueue }
  NSOperationQueue = objcclass external (NSObject)
  private
    _private: id;
    _reserved: Pointer;
    
  public
    procedure addOperation(op: NSOperation); message 'addOperation:';
    procedure addOperations_waitUntilFinished(ops: NSArray; wait: Boolean); message 'addOperations:waitUntilFinished:';
    function operations: NSArray; message 'operations';
    function operationCount: NSUInteger; message 'operationCount';
    function maxConcurrentOperationCount: NSInteger; message 'maxConcurrentOperationCount';
    procedure setMaxConcurrentOperationCount(cnt: NSInteger); message 'setMaxConcurrentOperationCount:';
    procedure setSuspended(b: Boolean); message 'setSuspended:';
    function isSuspended: Boolean; message 'isSuspended';
    procedure setName(n: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure cancelAllOperations; message 'cancelAllOperations';
    procedure waitUntilAllOperationsAreFinished; message 'waitUntilAllOperationsAreFinished';
    class function currentQueue: id; message 'currentQueue';
    class function mainQueue: id; message 'mainQueue';
  end;

{$endif}
{$endif}
