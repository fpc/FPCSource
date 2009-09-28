{ Parsed from Foundation.framework NSPort.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPORT_PAS_H}
{$define NSPORT_PAS_H}
type
  NSPortPointer = Pointer;
  NSMachPortPointer = Pointer;
  NSMessagePortPointer = Pointer;
  NSSocketPortPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSPORT_PAS_T}
{$define NSPORT_PAS_T}

{ Types }
type
  NSSocketNativeHandle = cint;

{ Constants }

const
  NSMachPortDeallocateNone = 0;
  NSMachPortDeallocateSendRight = 1  shl  0;
  NSMachPortDeallocateReceiveRight = 1  shl  1;

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
  NSPortDidBecomeInvalidNotification: NSString; external name '_NSPortDidBecomeInvalidNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPort = objcclass;
  NSMachPort = objcclass;
  NSMessagePort = objcclass;
  NSSocketPort = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPORT_PAS_C}
{$define NSPORT_PAS_C}

{ NSPort }
  NSPort = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSPort; message 'alloc';

    class function allocWithZone(zone_: NSZonePointer): id; message 'allocWithZone:';
    class function port: NSPort; message 'port';
    procedure invalidate; message 'invalidate';
    function isValid: Boolean; message 'isValid';
    procedure setDelegate(anId: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure scheduleInRunLoop_forMode(runLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(runLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    function reservedSpaceLength: NSUInteger; message 'reservedSpaceLength';
    function sendBeforeDate_components_from_reserved(limitDate: NSDate; components: NSMutableArray; receivePort: NSPort; headerSpaceReserved: NSUInteger): Boolean; message 'sendBeforeDate:components:from:reserved:';
    function sendBeforeDate_msgid_components_from_reserved(limitDate: NSDate; msgID: NSUInteger; components: NSMutableArray; receivePort: NSPort; headerSpaceReserved: NSUInteger): Boolean; message 'sendBeforeDate:msgid:components:from:reserved:';
    procedure addConnection_toRunLoop_forMode(conn: NSConnection; runLoop: NSRunLoop; mode: NSString); message 'addConnection:toRunLoop:forMode:';
    procedure removeConnection_fromRunLoop_forMode(conn: NSConnection; runLoop: NSRunLoop; mode: NSString); message 'removeConnection:fromRunLoop:forMode:';
  end; external;

{ NSMachPort }
  NSMachPort = objcclass(NSPort)
  private
    __delegate: id;
    __flags: NSUInteger;
    __machPort: cardinal;
    __reserved: NSUInteger;
    
  public
    class function alloc: NSMachPort; message 'alloc';

    class function portWithMachPort(machPort_: cardinal): NSPort; message 'portWithMachPort:';
    function initWithMachPort(machPort_: cardinal): id; message 'initWithMachPort:';
    class function portWithMachPort_options(machPort_: cardinal; f: NSUInteger): NSPort; message 'portWithMachPort:options:';
    function initWithMachPort_options(machPort_: cardinal; f: NSUInteger): id; message 'initWithMachPort:options:';
    function machPort: cardinal; message 'machPort';
    procedure scheduleInRunLoop_forMode(runLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(runLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
  end; external;

{ NSMessagePort }
  NSMessagePort = objcclass(NSPort)
  private
    __port: Pointer; {garbage collector: __strong }
    __delegate: id;
    
  public
    class function alloc: NSMessagePort; message 'alloc';
  end; external;

{ NSSocketPort }
  NSSocketPort = objcclass(NSPort)
  private
    __receiver: Pointer;
    __connectors: Pointer;
    __loops: Pointer;
    __data: Pointer;
    __signature: id;
    __delegate: id;
    __lock: id;
    __maxSize: NSUInteger;
    __maxSockets: NSUInteger;
    __reserved: NSUInteger;
    
  public
    class function alloc: NSSocketPort; message 'alloc';

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
  end; external;

{$endif}
{$endif}
