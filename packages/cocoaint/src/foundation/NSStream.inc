{ Parsed from Foundation.framework NSStream.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSTREAM_PAS_H}
{$define NSSTREAM_PAS_H}
type
  NSStreamPointer = Pointer;
  NSInputStreamPointer = Pointer;
  NSOutputStreamPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSTREAM_PAS_T}
{$define NSSTREAM_PAS_T}

{ Constants }

const
  NSStreamStatusNotOpen = 0;
  NSStreamStatusOpening = 1;
  NSStreamStatusOpen = 2;
  NSStreamStatusReading = 3;
  NSStreamStatusWriting = 4;
  NSStreamStatusAtEnd = 5;
  NSStreamStatusClosed = 6;
  NSStreamStatusError = 7;

const
  NSStreamEventNone = 0;
  NSStreamEventOpenCompleted = 1  shl  0;
  NSStreamEventHasBytesAvailable = 1  shl  1;
  NSStreamEventHasSpaceAvailable = 1  shl  2;
  NSStreamEventErrorOccurred = 1  shl  3;
  NSStreamEventEndEncountered = 1  shl  4;

{ Types }
type
  NSStreamStatus = NSUInteger;
  NSStreamEvent = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSTREAM_PAS_R}
{$define NSSTREAM_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSTREAM_PAS_F}
{$define NSSTREAM_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSTREAM_PAS_S}
{$define NSSTREAM_PAS_S}

{ External string constants }
var
  NSStreamSocketSecurityLevelKey: NSString; external name '_NSStreamSocketSecurityLevelKey';
  NSStreamSocketSecurityLevelNone: NSString; external name '_NSStreamSocketSecurityLevelNone';
  NSStreamSocketSecurityLevelNegotiatedSSL: NSString; external name '_NSStreamSocketSecurityLevelNegotiatedSSL';
  NSStreamSOCKSProxyConfigurationKey: NSString; external name '_NSStreamSOCKSProxyConfigurationKey';
  NSStreamSOCKSProxyHostKey: NSString; external name '_NSStreamSOCKSProxyHostKey';
  NSStreamSOCKSProxyPortKey: NSString; external name '_NSStreamSOCKSProxyPortKey';
  NSStreamSOCKSProxyVersionKey: NSString; external name '_NSStreamSOCKSProxyVersionKey';
  NSStreamSOCKSProxyUserKey: NSString; external name '_NSStreamSOCKSProxyUserKey';
  NSStreamSOCKSProxyPasswordKey: NSString; external name '_NSStreamSOCKSProxyPasswordKey';
  NSStreamDataWrittenToMemoryStreamKey: NSString; external name '_NSStreamDataWrittenToMemoryStreamKey';
  NSStreamFileCurrentOffsetKey: NSString; external name '_NSStreamFileCurrentOffsetKey';
  NSStreamSocketSSLErrorDomain: NSString; external name '_NSStreamSocketSSLErrorDomain';
  NSStreamSOCKSErrorDomain: NSString; external name '_NSStreamSOCKSErrorDomain';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStream = objcclass;
  NSInputStream = objcclass;
  NSOutputStream = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTREAM_PAS_C}
{$define NSSTREAM_PAS_C}

{ NSStream }
  NSStream = objcclass(NSObject)
    
  public
    class function alloc: NSStream; message 'alloc';

    procedure open; message 'open';
    procedure close; message 'close';
    function delegate: id; message 'delegate';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function propertyForKey(key: NSString): id; message 'propertyForKey:';
    function setProperty_forKey(property_: id; key: NSString): Boolean; message 'setProperty:forKey:';
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    function streamStatus: NSStreamStatus; message 'streamStatus';
    function streamError: NSError; message 'streamError';

    { Category: NSSocketStreamCreationExtensions }
    class procedure getStreamsToHost_port_inputStream_outputStream(host: NSHost; port: NSInteger; inputStream: NSInputStreamPointer; outputStream: NSOutputStreamPointer); message 'getStreamsToHost:port:inputStream:outputStream:';
  end; external;

{ NSInputStream }
  NSInputStream = objcclass(NSStream)
    
  public
    class function alloc: NSInputStream; message 'alloc';

    function read_maxLength(buffer: pbyte; len: NSUInteger): NSInteger; message 'read:maxLength:';
    function getBuffer_length(buffer: pbyte; len: NSUIntegerPointer): Boolean; message 'getBuffer:length:';
    function hasBytesAvailable: Boolean; message 'hasBytesAvailable';

    { Category: NSInputStreamExtensions }
    function initWithData(data: NSData): id; message 'initWithData:';
    function initWithFileAtPath(path: NSString): id; message 'initWithFileAtPath:';
    class function inputStreamWithData(data: NSData): id; message 'inputStreamWithData:';
    class function inputStreamWithFileAtPath(path: NSString): id; message 'inputStreamWithFileAtPath:';
  end; external;

{ NSOutputStream }
  NSOutputStream = objcclass(NSStream)
    
  public
    class function alloc: NSOutputStream; message 'alloc';

    function write_maxLength(buffer: pbyte; len: NSUInteger): NSInteger; message 'write:maxLength:';
    function hasSpaceAvailable: Boolean; message 'hasSpaceAvailable';

    { Category: NSOutputStreamExtensions }
    function initToMemory: id; message 'initToMemory';
    function initToBuffer_capacity(buffer: pbyte; capacity: NSUInteger): id; message 'initToBuffer:capacity:';
    function initToFileAtPath_append(path: NSString; shouldAppend: Boolean): id; message 'initToFileAtPath:append:';
    class function outputStreamToMemory: id; message 'outputStreamToMemory';
    class function outputStreamToBuffer_capacity(buffer: pbyte; capacity: NSUInteger): id; message 'outputStreamToBuffer:capacity:';
    class function outputStreamToFileAtPath_append(path: NSString; shouldAppend: Boolean): id; message 'outputStreamToFileAtPath:append:';
  end; external;

{$endif}
{$endif}
