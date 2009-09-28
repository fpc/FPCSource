{ Parsed from Foundation.framework NSOperation.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSOPERATION_PAS_H}
{$define NSOPERATION_PAS_H}
type
  NSOperationPointer = Pointer;
  NSInvocationOperationPointer = Pointer;
  NSOperationQueuePointer = Pointer;

{$endif}
{$endif}

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
  NSInvocationOperationVoidResultException: NSString; external name '_NSInvocationOperationVoidResultException';
  NSInvocationOperationCancelledException: NSString; external name '_NSInvocationOperationCancelledException';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOperation = objcclass;
  NSInvocationOperation = objcclass;
  NSOperationQueue = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOPERATION_PAS_C}
{$define NSOPERATION_PAS_C}

{ NSOperation }
  NSOperation = objcclass(NSObject)
  private
    __private: id;
    __reserved: Pointer;
    
  public
    class function alloc: NSOperation; message 'alloc';

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
  end; external;

{ NSInvocationOperation }
  NSInvocationOperation = objcclass(NSOperation)
  private
    __inv: id;
    __exception: id;
    __reserved2: Pointer;
    
  public
    class function alloc: NSInvocationOperation; message 'alloc';

    function initWithTarget_selector_object(target: id; sel: SEL; arg: id): id; message 'initWithTarget:selector:object:';
    function initWithInvocation(inv: NSInvocation): id; message 'initWithInvocation:';
    function invocation: NSInvocation; message 'invocation';
    function result_: id; message 'result';
  end; external;

{ NSOperationQueue }
  NSOperationQueue = objcclass(NSObject)
  private
    __private: id;
    __reserved: Pointer;
    
  public
    class function alloc: NSOperationQueue; message 'alloc';

    procedure addOperation(op: NSOperation); message 'addOperation:';
    function operations: NSArray; message 'operations';
    function maxConcurrentOperationCount: NSInteger; message 'maxConcurrentOperationCount';
    procedure setMaxConcurrentOperationCount(cnt: NSInteger); message 'setMaxConcurrentOperationCount:';
    procedure setSuspended(b: Boolean); message 'setSuspended:';
    function isSuspended: Boolean; message 'isSuspended';
    procedure cancelAllOperations; message 'cancelAllOperations';
    procedure waitUntilAllOperationsAreFinished; message 'waitUntilAllOperationsAreFinished';
  end; external;

{$endif}
{$endif}
