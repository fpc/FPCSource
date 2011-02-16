{ Parsed from Foundation.framework NSPort.h }

{$ifdef TYPES}
{$ifndef NSPORT_PAS_T}
{$define NSPORT_PAS_T}

{ Types }
type
  NSSocketNativeHandle = cint;
  NSSocketNativeHandlePtr = ^NSSocketNativeHandle;

{ Constants }

const
  NSMachPortDeallocateNone = 0;
  NSMachPortDeallocateSendRight = 1 shl 0;
  NSMachPortDeallocateReceiveRight = 1 shl 1;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPORT_PAS_R}
{$define NSPORT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPORT_PAS_F}
{$define NSPORT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPORT_PAS_S}
{$define NSPORT_PAS_S}

{ External string constants }
var
  NSPortDidBecomeInvalidNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPortDelegateProtocol = objcprotocol;
  NSMachPortDelegateProtocol = objcprotocol;
  NSPort = objcclass;
  NSPortPointer = ^NSPort;
  NSPortPtr = NSPortPointer;
  NSMachPort = objcclass;
  NSMachPortPointer = ^NSMachPort;
  NSMachPortPtr = NSMachPortPointer;
  NSMessagePort = objcclass;
  NSMessagePortPointer = ^NSMessagePort;
  NSMessagePortPtr = NSMessagePortPointer;
  NSSocketPort = objcclass;
  NSSocketPortPointer = ^NSSocketPort;
  NSSocketPortPtr = NSSocketPortPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPORT_PAS_C}
{$define NSPORT_PAS_C}

{ NSPort }
  NSPort = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function allocWithZone(zone_: NSZonePtr): id; message 'allocWithZone:';
    class function port: NSPort; message 'port';
    procedure invalidate; message 'invalidate';
    function isValid: Boolean; message 'isValid';
    procedure setDelegate(anObject: NSPortDelegateProtocol); message 'setDelegate:';
    function delegate: NSPortDelegateProtocol; message 'delegate';
    procedure scheduleInRunLoop_forMode(runLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(runLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    function reservedSpaceLength: NSUInteger; message 'reservedSpaceLength';
    function sendBeforeDate_components_from_reserved(limitDate: NSDate; components: NSMutableArray; receivePort: NSPort; headerSpaceReserved: NSUInteger): Boolean; message 'sendBeforeDate:components:from:reserved:';
    function sendBeforeDate_msgid_components_from_reserved(limitDate: NSDate; msgID: NSUInteger; components: NSMutableArray; receivePort: NSPort; headerSpaceReserved: NSUInteger): Boolean; message 'sendBeforeDate:msgid:components:from:reserved:';
    procedure addConnection_toRunLoop_forMode(conn: NSConnection; runLoop: NSRunLoop; mode: NSString); message 'addConnection:toRunLoop:forMode:';
    procedure removeConnection_fromRunLoop_forMode(conn: NSConnection; runLoop: NSRunLoop; mode: NSString); message 'removeConnection:fromRunLoop:forMode:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSMachPort }
  NSMachPort = objcclass external (NSPort)
  private
    _delegate: id;
    _flags: NSUInteger;
    _machPort: cuint32;
    _reserved: NSUInteger;
    
  public
    class function portWithMachPort(machPort_: cuint32): NSPort; message 'portWithMachPort:';
    function initWithMachPort(machPort_: cuint32): id; message 'initWithMachPort:';
    procedure setDelegate(anObject: NSMachPortDelegateProtocol); message 'setDelegate:';
    function delegate: NSMachPortDelegateProtocol; message 'delegate';
    class function portWithMachPort_options(machPort_: cuint32; f: NSUInteger): NSPort; message 'portWithMachPort:options:';
    function initWithMachPort_options(machPort_: cuint32; f: NSUInteger): id; message 'initWithMachPort:options:';
    function machPort: cuint32; message 'machPort';
    procedure scheduleInRunLoop_forMode(runLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(runLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
  end;

{ NSMessagePort }
  NSMessagePort = objcclass external (NSPort)
  private
    _port: Pointer; {garbage collector: __strong }
    _delegate: id;
    
  public
  end;

{ NSSocketPort }
  NSSocketPort = objcclass external (NSPort)
  private
    _receiver: Pointer; {garbage collector: __strong }
    _connectors: Pointer; {garbage collector: __strong }
    _loops: Pointer; {garbage collector: __strong }
    _data: Pointer; {garbage collector: __strong }
    _signature: id;
    _delegate: id;
    _lock: id;
    _maxSize: NSUInteger;
    _useCount: NSUInteger;
    _reserved: NSUInteger;
    
  public
    function init: id; message 'init';
    function initWithTCPPort(port_: cushort): id; message 'initWithTCPPort:';
    function initWithProtocolFamily_socketType_protocol_address(family: cint; type_: cint; protocol_: cint; address_: NSData): id; message 'initWithProtocolFamily:socketType:protocol:address:';
    function initWithProtocolFamily_socketType_protocol_socket(family: cint; type_: cint; protocol_: cint; sock: NSSocketNativeHandle): id; message 'initWithProtocolFamily:socketType:protocol:socket:';
    function initRemoteWithTCPPort_host(port_: cushort; hostName: NSString): id; message 'initRemoteWithTCPPort:host:';
    function initRemoteWithProtocolFamily_socketType_protocol_address(family: cint; type_: cint; protocol_: cint; address_: NSData): id; message 'initRemoteWithProtocolFamily:socketType:protocol:address:';
    function protocolFamily: cint; message 'protocolFamily';
    function socketType: cint; message 'socketType';
    function protocol: cint; message 'protocol';
    function address: NSData; message 'address';
    function socket: NSSocketNativeHandle; message 'socket';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSPORT_PAS_P}
{$define NSPORT_PAS_P}
  
{ NSPortDelegate Protocol }
  NSPortDelegateProtocol = objcprotocol external name 'NSPortDelegate'
  optional
    procedure handlePortMessage(message: NSPortMessage); message 'handlePortMessage:';
  end;
  
{ NSMachPortDelegate Protocol }
  NSMachPortDelegateProtocol = objcprotocol external name 'NSMachPortDelegate'
  optional
    procedure handleMachMessage(msg: Pointer); message 'handleMachMessage:';
  end;
{$endif}
{$endif}
