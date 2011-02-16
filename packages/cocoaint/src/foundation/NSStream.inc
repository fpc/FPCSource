{ Parsed from Foundation.framework NSStream.h }

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
  NSStreamEventOpenCompleted = 1 shl 0;
  NSStreamEventHasBytesAvailable = 1 shl 1;
  NSStreamEventHasSpaceAvailable = 1 shl 2;
  NSStreamEventErrorOccurred = 1 shl 3;
  NSStreamEventEndEncountered = 1 shl 4;

{ Types }
type
  NSStreamStatus = NSUInteger;
  NSStreamStatusPtr = ^NSStreamStatus;
  NSStreamEvent = NSUInteger;
  NSStreamEventPtr = ^NSStreamEvent;

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
  NSStreamSocketSecurityLevelKey: NSString; cvar; external;
  NSStreamSocketSecurityLevelNone: NSString; cvar; external;
  NSStreamSocketSecurityLevelSSLv2: NSString; cvar; external;
  NSStreamSocketSecurityLevelSSLv3: NSString; cvar; external;
  NSStreamSocketSecurityLevelTLSv1: NSString; cvar; external;
  NSStreamSocketSecurityLevelNegotiatedSSL: NSString; cvar; external;
  NSStreamSOCKSProxyConfigurationKey: NSString; cvar; external;
  NSStreamSOCKSProxyHostKey: NSString; cvar; external;
  NSStreamSOCKSProxyPortKey: NSString; cvar; external;
  NSStreamSOCKSProxyVersionKey: NSString; cvar; external;
  NSStreamSOCKSProxyUserKey: NSString; cvar; external;
  NSStreamSOCKSProxyPasswordKey: NSString; cvar; external;
  NSStreamSOCKSProxyVersion4: NSString; cvar; external;
  NSStreamSOCKSProxyVersion5: NSString; cvar; external;
  NSStreamDataWrittenToMemoryStreamKey: NSString; cvar; external;
  NSStreamFileCurrentOffsetKey: NSString; cvar; external;
  NSStreamSocketSSLErrorDomain: NSString; cvar; external;
  NSStreamSOCKSErrorDomain: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStreamDelegateProtocol = objcprotocol;
  NSStream = objcclass;
  NSStreamPointer = ^NSStream;
  NSStreamPtr = NSStreamPointer;
  NSInputStream = objcclass;
  NSInputStreamPointer = ^NSInputStream;
  NSInputStreamPtr = NSInputStreamPointer;
  NSOutputStream = objcclass;
  NSOutputStreamPointer = ^NSOutputStream;
  NSOutputStreamPtr = NSOutputStreamPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTREAM_PAS_C}
{$define NSSTREAM_PAS_C}

{ NSStream }
  NSStream = objcclass external (NSObject)
    
  public
    procedure open; message 'open';
    procedure close; message 'close';
    function delegate: NSStreamDelegateProtocol; message 'delegate';
    procedure setDelegate(delegate_: NSStreamDelegateProtocol); message 'setDelegate:';
    function propertyForKey(key: NSString): id; message 'propertyForKey:';
    function setProperty_forKey(property_: id; key: NSString): Boolean; message 'setProperty:forKey:';
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    function streamStatus: NSStreamStatus; message 'streamStatus';
    function streamError: NSError; message 'streamError';
  end;

{ NSInputStream }
  NSInputStream = objcclass external (NSStream)
    
  public
    function read_maxLength(buffer: pbyte; len: NSUInteger): NSInteger; message 'read:maxLength:';
    function getBuffer_length(buffer: pbyte; len: NSUIntegerPtr): Boolean; message 'getBuffer:length:';
    function hasBytesAvailable: Boolean; message 'hasBytesAvailable';
  end;

{ NSOutputStream }
  NSOutputStream = objcclass external (NSStream)
    
  public
    function write_maxLength(buffer: pbyte; len: NSUInteger): NSInteger; message 'write:maxLength:';
    function hasSpaceAvailable: Boolean; message 'hasSpaceAvailable';
  end;

{ NSSocketStreamCreationExtensionsCategory }
  NSSocketStreamCreationExtensionsCategory = objccategory external (NSStream)
    class procedure getStreamsToHost_port_inputStream_outputStream(host: NSHost; port: NSInteger; inputStream: NSInputStreamPointer; outputStream: NSOutputStreamPointer); message 'getStreamsToHost:port:inputStream:outputStream:';
  end;

{ NSInputStreamExtensionsCategory }
  NSInputStreamExtensionsCategory = objccategory external (NSInputStream)
    function initWithData(data: NSData): id; message 'initWithData:';
    function initWithFileAtPath(path: NSString): id; message 'initWithFileAtPath:';
    function initWithURL(url: NSURL): id; message 'initWithURL:';
    class function inputStreamWithData(data: NSData): id; message 'inputStreamWithData:';
    class function inputStreamWithFileAtPath(path: NSString): id; message 'inputStreamWithFileAtPath:';
    class function inputStreamWithURL(url: NSURL): id; message 'inputStreamWithURL:';
  end;

{ NSOutputStreamExtensionsCategory }
  NSOutputStreamExtensionsCategory = objccategory external (NSOutputStream)
    function initToMemory: id; message 'initToMemory';
    function initToBuffer_capacity(buffer: pbyte; capacity: NSUInteger): id; message 'initToBuffer:capacity:';
    function initToFileAtPath_append(path: NSString; shouldAppend: Boolean): id; message 'initToFileAtPath:append:';
    function initWithURL_append(url: NSURL; shouldAppend: Boolean): id; message 'initWithURL:append:';
    class function outputStreamToMemory: id; message 'outputStreamToMemory';
    class function outputStreamToBuffer_capacity(buffer: pbyte; capacity: NSUInteger): id; message 'outputStreamToBuffer:capacity:';
    class function outputStreamToFileAtPath_append(path: NSString; shouldAppend: Boolean): id; message 'outputStreamToFileAtPath:append:';
    class function outputStreamWithURL_append(url: NSURL; shouldAppend: Boolean): id; message 'outputStreamWithURL:append:';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSSTREAM_PAS_P}
{$define NSSTREAM_PAS_P}
  
{ NSStreamDelegate Protocol }
  NSStreamDelegateProtocol = objcprotocol external name 'NSStreamDelegate'
  optional
    procedure stream_handleEvent(aStream: NSStream; eventCode: NSStreamEvent); message 'stream:handleEvent:';
  end;
{$endif}
{$endif}
