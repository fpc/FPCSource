{ Parsed from Foundation.framework NSBackgroundActivityScheduler.h }


{$ifdef TYPES}
type
  NSBackgroundActivitySchedulerPtr = ^NSBackgroundActivityScheduler;
{$endif}

{$ifdef TYPES}
type
  NSBackgroundActivityResult = NSInteger;
  NSBackgroundActivityResultPtr = ^NSBackgroundActivityResult;

const
  NSBackgroundActivityResultFinished = 1;
  NSBackgroundActivityResultDeferred = 2;

type
  NSBackgroundActivityCompletionHandler = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  NSBackgroundActivityScheduler = objcclass external (NSObject)
  private
    _private1: id;
    _private2: id;
    _private3: id;
    _flags: cint64;
  public
    function initWithIdentifier (identifier: NSString): instancetype; message 'initWithIdentifier:'; { NS_DESIGNATED_INITIALIZER }
    function identifier: NSString; message 'identifier';
    procedure setQualityOfService(newValue: NSQualityOfService); message 'setQualityOfService:';
    function qualityOfService: NSQualityOfService; message 'qualityOfService';
    procedure setRepeats(newValue: ObjCBOOL); message 'setRepeats:';
    function repeats: ObjCBOOL; message 'repeats';
    procedure setInterval(newValue: NSTimeInterval); message 'setInterval:';
    function interval: NSTimeInterval; message 'interval';
    procedure setTolerance(newValue: NSTimeInterval); message 'setTolerance:';
    function tolerance: NSTimeInterval; message 'tolerance';
    procedure scheduleWithBlock (block: OpaqueCBlock); message 'scheduleWithBlock:';
    procedure invalidate; message 'invalidate';
    function shouldDefer: ObjCBOOL; message 'shouldDefer';
  end;
{$endif}

