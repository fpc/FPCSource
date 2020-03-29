{ Parsed from Foundation.framework NSStream.h }


{ Types from NSSocketStreamCreationExtensions }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSStreamPtr = ^NSStream;
  NSInputStreamPtr = ^NSInputStream;
  NSOutputStreamPtr = ^NSOutputStream;
  NSStreamDelegateProtocolPtr = ^NSStreamDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSStreamStatus = NSUInteger;
  NSStreamStatusPtr = ^NSStreamStatus;

const
  NSStreamStatusNotOpen = 0;
  NSStreamStatusOpening = 1;
  NSStreamStatusOpen = 2;
  NSStreamStatusReading = 3;
  NSStreamStatusWriting = 4;
  NSStreamStatusAtEnd = 5;
  NSStreamStatusClosed = 6;
  NSStreamStatusError = 7;

type
  NSStreamEvent = NSUInteger;
  NSStreamEventPtr = ^NSStreamEvent;

const
  NSStreamEventNone = 0;
  NSStreamEventOpenCompleted = 1 shl 0;
  NSStreamEventHasBytesAvailable = 1 shl 1;
  NSStreamEventHasSpaceAvailable = 1 shl 2;
  NSStreamEventErrorOccurred = 1 shl 3;
  NSStreamEventEndEncountered = 1 shl 4;
{$endif}

{$ifdef CLASSES}

type
  NSStream = objcclass external (NSObject)
  public
    procedure open; message 'open';
    procedure close; message 'close';
    procedure setDelegate(newValue: NSStreamDelegateProtocol); message 'setDelegate:';
    function delegate: NSStreamDelegateProtocol; message 'delegate';
    function propertyForKey (key: NSString): id; message 'propertyForKey:';
    function setProperty_forKey (property_: id; key: NSString): ObjCBOOL; message 'setProperty:forKey:';
    procedure scheduleInRunLoop_forMode (aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode (aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    function streamStatus: NSStreamStatus; message 'streamStatus';
    function streamError: NSError; message 'streamError';
  end;

type
  NSInputStream = objcclass external (NSStream)
  public
    function read_maxLength (buffer: pbyte; len: NSUInteger): NSInteger; message 'read:maxLength:';
    function getBuffer_length (buffer: pbyte; len: NSUIntegerPtr): ObjCBOOL; message 'getBuffer:length:';
    function hasBytesAvailable: ObjCBOOL; message 'hasBytesAvailable';
    function initWithData (data: NSData): instancetype; message 'initWithData:'; { NS_DESIGNATED_INITIALIZER }
    function initWithURL (url: NSURL): instancetype; message 'initWithURL:'; { available in 10_6, 4_0 }
  end;

type
  NSOutputStream = objcclass external (NSStream)
  public
    function write_maxLength (buffer: pbyte; len: NSUInteger): NSInteger; message 'write:maxLength:';
    function hasSpaceAvailable: ObjCBOOL; message 'hasSpaceAvailable';
    function initToMemory: instancetype; message 'initToMemory'; { NS_DESIGNATED_INITIALIZER }
    function initToBuffer_capacity (buffer: pbyte; capacity: NSUInteger): instancetype; message 'initToBuffer:capacity:'; { NS_DESIGNATED_INITIALIZER }
    function initWithURL_append (url: NSURL; shouldAppend: ObjCBOOL): instancetype; message 'initWithURL:append:'; { available in 10_6, 4_0 }
  end;


type
  NSSocketStreamCreationExtensions = objccategory external (NSStream)
    class procedure getStreamsToHostWithName_port_inputStream_outputStream (hostname: NSString; port: NSInteger; inputStream: NSInputStreamPtr; outputStream: NSOutputStreamPtr); message 'getStreamsToHostWithName:port:inputStream:outputStream:'; { available in 10_10, 8_0 }
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
    class procedure getStreamsToHost_port_inputStream_outputStream (host: NSHost; port: NSInteger; inputStream: NSInputStreamPtr; outputStream: NSOutputStreamPtr); message 'getStreamsToHost:port:inputStream:outputStream:'; deprecated 'in 10_3, 10_10, "Please use getStreamsToHostWithName:port:inputStream:outputStream: instead"';
    {$endif}
  end;

type
  NSStreamBoundPairCreationExtensions = objccategory external (NSStream)
    class procedure getBoundStreamsWithBufferSize_inputStream_outputStream (bufferSize: NSUInteger; inputStream: NSInputStreamPtr; outputStream: NSOutputStreamPtr); message 'getBoundStreamsWithBufferSize:inputStream:outputStream:'; { available in 10_10, 8_0 }
  end;

type
  NSInputStreamExtensions = objccategory external (NSInputStream)
    function initWithFileAtPath (path: NSString): instancetype; message 'initWithFileAtPath:';
    class function inputStreamWithData (data: NSData): instancetype; message 'inputStreamWithData:';
    class function inputStreamWithFileAtPath (path: NSString): instancetype; message 'inputStreamWithFileAtPath:';
    class function inputStreamWithURL (url: NSURL): instancetype; message 'inputStreamWithURL:'; { available in 10_6, 4_0 }
  end;

type
  NSOutputStreamExtensions = objccategory external (NSOutputStream)
    function initToFileAtPath_append (path: NSString; shouldAppend: ObjCBOOL): instancetype; message 'initToFileAtPath:append:';
    class function outputStreamToMemory: instancetype; message 'outputStreamToMemory';
    class function outputStreamToBuffer_capacity (buffer: pbyte; capacity: NSUInteger): instancetype; message 'outputStreamToBuffer:capacity:';
    class function outputStreamToFileAtPath_append (path: NSString; shouldAppend: ObjCBOOL): instancetype; message 'outputStreamToFileAtPath:append:';
    class function outputStreamWithURL_append (url: NSURL; shouldAppend: ObjCBOOL): instancetype; message 'outputStreamWithURL:append:'; { available in 10_6, 4_0 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSStreamDelegateProtocol = objcprotocol external name 'NSStreamDelegate' (NSObjectProtocol)
  optional
    procedure stream_handleEvent (aStream: NSStream; eventCode: NSStreamEvent); message 'stream:handleEvent:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSStreamSocketSecurityLevelKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSocketSecurityLevelNone: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSocketSecurityLevelSSLv2: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSocketSecurityLevelSSLv3: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSocketSecurityLevelTLSv1: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSocketSecurityLevelNegotiatedSSL: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSProxyConfigurationKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSProxyHostKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSProxyPortKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSProxyVersionKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSProxyUserKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSProxyPasswordKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSProxyVersion4: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSProxyVersion5: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamDataWrittenToMemoryStreamKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamFileCurrentOffsetKey: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSocketSSLErrorDomain: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamSOCKSErrorDomain: NSString { available in 10_3, 2_0 }; cvar; external;
  NSStreamNetworkServiceType: NSString { available in 10_7, 4_0 }; cvar; external;
  NSStreamNetworkServiceTypeVoIP: NSString { available in 10_7, 4_0 }; cvar; external;
  NSStreamNetworkServiceTypeVideo: NSString { available in 10_7, 5_0 }; cvar; external;
  NSStreamNetworkServiceTypeBackground: NSString { available in 10_7, 5_0 }; cvar; external;
  NSStreamNetworkServiceTypeVoice: NSString { available in 10_7, 5_0 }; cvar; external;
{$endif}

