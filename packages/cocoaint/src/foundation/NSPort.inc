{ Parsed from Foundation.framework NSPort.h }


{ Types from NSPort }
{$ifdef TYPES}

{$endif}


{ Types from NSMachPort }
{$ifdef TYPES}

const
  NSMachPortDeallocateNone = 0;
  NSMachPortDeallocateSendRight = 1 shl 0;
  NSMachPortDeallocateReceiveRight = 1 shl 1;
{$endif}


{$ifdef TYPES}
type
  NSPortPtr = ^NSPort;
  NSMachPortPtr = ^NSMachPort;
  NSMessagePortPtr = ^NSMessagePort;
  NSSocketPortPtr = ^NSSocketPort;
  NSPortDelegateProtocolPtr = ^NSPortDelegateProtocol;
  NSMachPortDelegateProtocolPtr = ^NSMachPortDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSSocketNativeHandle = cint;
  NSSocketNativeHandlePtr = ^NSSocketNativeHandle;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPortDidBecomeInvalidNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSPort = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  public
    class function port: NSPort; message 'port';
    procedure invalidate; message 'invalidate';
    function isValid: ObjCBOOL; message 'isValid';
    procedure setDelegate (anObject: NSPortDelegateProtocol); message 'setDelegate:';
    function delegate: NSPortDelegateProtocol; message 'delegate';
    procedure scheduleInRunLoop_forMode (runLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode (runLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    function reservedSpaceLength: NSUInteger; message 'reservedSpaceLength';
    function sendBeforeDate_components_from_reserved (limitDate: NSDate; components: NSMutableArray; receivePort: NSPort; headerSpaceReserved: NSUInteger): ObjCBOOL; message 'sendBeforeDate:components:from:reserved:';
    function sendBeforeDate_msgid_components_from_reserved (limitDate: NSDate; msgID: NSUInteger; components: NSMutableArray; receivePort: NSPort; headerSpaceReserved: NSUInteger): ObjCBOOL; message 'sendBeforeDate:msgid:components:from:reserved:';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or (defined(TARGET_OS_WIN32))}
    procedure addConnection_toRunLoop_forMode (conn: NSConnection; runLoop: NSRunLoop; mode: NSString); message 'addConnection:toRunLoop:forMode:';
    procedure removeConnection_fromRunLoop_forMode (conn: NSConnection; runLoop: NSRunLoop; mode: NSString); message 'removeConnection:fromRunLoop:forMode:';
    {$endif}

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPortDelegateProtocol = objcprotocol external name 'NSPortDelegate' (NSObjectProtocol)
  optional
    procedure handlePortMessage (message_: NSPortMessage); message 'handlePortMessage:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))}
{$ifdef CLASSES}

type
  NSMachPort = objcclass external (NSPort)
  private
    _delegate: id;
    _flags: NSUInteger;
    _machPort: cuint32;
    _reserved: NSUInteger;
  public
    class function portWithMachPort (machPort: cuint32): NSPort; message 'portWithMachPort:';
    function initWithMachPort (machPort: cuint32): instancetype; message 'initWithMachPort:';
    procedure setDelegate (anObject: NSMachPortDelegateProtocol); message 'setDelegate:';
    function delegate: NSMachPortDelegateProtocol; message 'delegate';
    class function portWithMachPort_options (machPort: cuint32; f: NSUInteger): NSPort; message 'portWithMachPort:options:'; { available in 10_5, 2_0 }
    function initWithMachPort_options (machPort: cuint32; f: NSUInteger): instancetype; message 'initWithMachPort:options:'; { available in 10_5, 2_0 }
    function machPort: cuint32; message 'machPort';
    procedure scheduleInRunLoop_forMode (runLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode (runLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSMachPortDelegateProtocol = objcprotocol external name 'NSMachPortDelegate' (NSPortDelegateProtocol)
  optional
    procedure handleMachMessage (msg: pointer); message 'handleMachMessage:';
  end;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  NSMessagePort = objcclass external (NSPort)
  private
    _port: pointer;
    _delegate: id;
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32) or defined(TARGET_IPHONE_SIMULATOR)}
{$ifdef CLASSES}

type
  NSSocketPort = objcclass external (NSPort)
  private
    _receiver: pointer;
    _connectors: id;
    _loops: pointer;
    _data: pointer;
    _signature: id;
    _delegate: id;
    _lock: id;
    _maxSize: NSUInteger;
    _useCount: NSUInteger;
    _reserved: NSUInteger;
  public
    function init: instancetype; message 'init';
    function initWithTCPPort (port_: cushort): instancetype; message 'initWithTCPPort:';
    function initWithProtocolFamily_socketType_protocol_address (family: cint; type_: cint; protocol: cint; address: NSData): instancetype; message 'initWithProtocolFamily:socketType:protocol:address:'; { NS_DESIGNATED_INITIALIZER }
    function initWithProtocolFamily_socketType_protocol_socket (family: cint; type_: cint; protocol: cint; sock: NSSocketNativeHandle): instancetype; message 'initWithProtocolFamily:socketType:protocol:socket:'; { NS_DESIGNATED_INITIALIZER }
    function initRemoteWithTCPPort_host (port_: cushort; hostName: NSString): instancetype; message 'initRemoteWithTCPPort:host:';
    function initRemoteWithProtocolFamily_socketType_protocol_address (family: cint; type_: cint; protocol: cint; address: NSData): instancetype; message 'initRemoteWithProtocolFamily:socketType:protocol:address:'; { NS_DESIGNATED_INITIALIZER }
    function protocolFamily: cint; message 'protocolFamily';
    function socketType: cint; message 'socketType';
    function protocol: cint; message 'protocol';
    function address: NSData; message 'address';
    function socket: NSSocketNativeHandle; message 'socket';
  end;
{$endif}

{$endif}
