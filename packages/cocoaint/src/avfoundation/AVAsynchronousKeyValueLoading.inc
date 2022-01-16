{ Parsed from AVFoundation.framework AVAsynchronousKeyValueLoading.h }


{$ifdef TYPES}
type
  AVAsynchronousKeyValueLoadingProtocolPtr = ^AVAsynchronousKeyValueLoadingProtocol;
{$endif}

{$ifdef TYPES}
type
  AVKeyValueStatus = NSInteger;
  AVKeyValueStatusPtr = ^AVKeyValueStatus;

const
  AVKeyValueStatusUnknown = 0;
  AVKeyValueStatusLoading = 1;
  AVKeyValueStatusLoaded = 2;
  AVKeyValueStatusFailed = 3;
  AVKeyValueStatusCancelled = 4;
{$endif}

{$ifdef PROTOCOLS}

type
  AVAsynchronousKeyValueLoadingProtocol = objcprotocol external name 'AVAsynchronousKeyValueLoading'
  required
    function statusOfValueForKey_error (key: NSString; outError: NSErrorPtr): AVKeyValueStatus; message 'statusOfValueForKey:error:';
    procedure loadValuesAsynchronouslyForKeys_completionHandler (keys: NSArray; handler: OpaqueCBlock); message 'loadValuesAsynchronouslyForKeys:completionHandler:';
  end;
{$endif}

