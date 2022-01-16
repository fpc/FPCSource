{ Parsed from Foundation.framework NSXPCConnection.h }


{$ifdef TYPES}
type
  NSXPCConnectionPtr = ^NSXPCConnection;
  NSXPCListenerPtr = ^NSXPCListener;
  NSXPCInterfacePtr = ^NSXPCInterface;
  NSXPCListenerEndpointPtr = ^NSXPCListenerEndpoint;
  NSXPCProxyCreatingProtocolPtr = ^NSXPCProxyCreatingProtocol;
  NSXPCListenerDelegateProtocolPtr = ^NSXPCListenerDelegateProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSXPCProxyCreatingProtocol = objcprotocol external name 'NSXPCProxyCreating'
    function remoteObjectProxy: id; message 'remoteObjectProxy';
    function remoteObjectProxyWithErrorHandler (handler: OpaqueCBlock): id; message 'remoteObjectProxyWithErrorHandler:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSXPCConnectionOptions = NSUInteger;
  NSXPCConnectionOptionsPtr = ^NSXPCConnectionOptions;

const
  NSXPCConnectionPrivileged = 1 shl 12;
{$endif}

{$ifdef CLASSES}

type
  NSXPCConnection = objcclass external (NSObject, NSXPCProxyCreatingProtocol)
  private
    _xconnection: pointer;
    _incomingReplyInfo: id;
    _userQueue: dispatch_queue_t;
    _state: cuint32;
    _state2: cuint32;
    _interruptionHandler: OpaqueCBlock;
    _invalidationHandler: OpaqueCBlock;
    _exportInfo: id;
    _replyTable: id;
    _importInfo: id;
    _otherInfo: NSObjectProtocol;
    _reserved1: id;
    _lock: id;
    _remoteObjectInterface: NSXPCInterface;
    _serviceName: NSString;
    _endpoint: NSXPCListenerEndpoint;
    _eCache: id;
    _dCache: id;
  public
    function initWithServiceName (serviceName: NSString): instancetype; message 'initWithServiceName:';
    function serviceName: NSString; message 'serviceName';
    function initWithMachServiceName_options (name: NSString; options: NSXPCConnectionOptions): instancetype; message 'initWithMachServiceName:options:';
    function initWithListenerEndpoint (endpoint: NSXPCListenerEndpoint): instancetype; message 'initWithListenerEndpoint:';
    function endpoint: NSXPCListenerEndpoint; message 'endpoint';
    procedure setExportedInterface(newValue: NSXPCInterface); message 'setExportedInterface:';
    function exportedInterface: NSXPCInterface; message 'exportedInterface';
    procedure setExportedObject(newValue: id); message 'setExportedObject:';
    function exportedObject: id; message 'exportedObject';
    procedure setRemoteObjectInterface(newValue: NSXPCInterface); message 'setRemoteObjectInterface:';
    function remoteObjectInterface: NSXPCInterface; message 'remoteObjectInterface';
    function remoteObjectProxy: id; message 'remoteObjectProxy';
    function remoteObjectProxyWithErrorHandler (handler: OpaqueCBlock): id; message 'remoteObjectProxyWithErrorHandler:';
    procedure setInterruptionHandler(newValue: OpaqueCBlock); message 'setInterruptionHandler:';
    function interruptionHandler: OpaqueCBlock; message 'interruptionHandler';
    procedure setInvalidationHandler(newValue: OpaqueCBlock); message 'setInvalidationHandler:';
    function invalidationHandler: OpaqueCBlock; message 'invalidationHandler';
    procedure resume; message 'resume';
    procedure suspend; message 'suspend';
    procedure invalidate; message 'invalidate';
    function auditSessionIdentifier: au_asid_t; message 'auditSessionIdentifier';
    function processIdentifier: pid_t; message 'processIdentifier';
    function effectiveUserIdentifier: uid_t; message 'effectiveUserIdentifier';
    function effectiveGroupIdentifier: gid_t; message 'effectiveGroupIdentifier';
  end;

type
  NSXPCListener = objcclass external (NSObject)
  private
    _xconnection: pointer;
    _userQueue: dispatch_queue_t;
    reserved0: pointer;
    _delegate: NSXPCListenerDelegateProtocol;
    _serviceName: NSString;
    _state: cuint64;
    _reserved1: id;
    _reserved2: id;
  public
    class function serviceListener: NSXPCListener; message 'serviceListener';
    class function anonymousListener: NSXPCListener; message 'anonymousListener';
    function initWithMachServiceName (name: NSString): instancetype; message 'initWithMachServiceName:'; { NS_DESIGNATED_INITIALIZER }
    procedure setDelegate(newValue: NSXPCListenerDelegateProtocol); message 'setDelegate:';
    function delegate: NSXPCListenerDelegateProtocol; message 'delegate';
    function endpoint: NSXPCListenerEndpoint; message 'endpoint';
    procedure resume; message 'resume';
    procedure suspend; message 'suspend';
    procedure invalidate; message 'invalidate';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSXPCListenerDelegateProtocol = objcprotocol external name 'NSXPCListenerDelegate' (NSObjectProtocol)
  optional
    function listener_shouldAcceptNewConnection (listener: NSXPCListener; newConnection: NSXPCConnection): ObjCBOOL; message 'listener:shouldAcceptNewConnection:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSXPCInterface = objcclass external (NSObject)
  private
    _protocol: Protocol;
    _methods2: CFMutableDictionaryRef;
    _reserved1: id;
  public
    class function interfaceWithProtocol (protocol_: Protocol): NSXPCInterface; message 'interfaceWithProtocol:';
    procedure setProtocol(newValue: Protocol); message 'setProtocol:';
    function protocol: Protocol; message 'protocol';
    procedure setClasses_forSelector_argumentIndex_ofReply (classes: NSSet; sel_: SEL; arg: NSUInteger; ofReply: ObjCBOOL); message 'setClasses:forSelector:argumentIndex:ofReply:';
    function classesForSelector_argumentIndex_ofReply (sel_: SEL; arg: NSUInteger; ofReply: ObjCBOOL): NSSet; message 'classesForSelector:argumentIndex:ofReply:';
    procedure setInterface_forSelector_argumentIndex_ofReply (ifc: NSXPCInterface; sel_: SEL; arg: NSUInteger; ofReply: ObjCBOOL); message 'setInterface:forSelector:argumentIndex:ofReply:';
    function interfaceForSelector_argumentIndex_ofReply (sel_: SEL; arg: NSUInteger; ofReply: ObjCBOOL): NSXPCInterface; message 'interfaceForSelector:argumentIndex:ofReply:';
  end;

type
  NSXPCListenerEndpoint = objcclass external (NSObject, NSSecureCodingProtocol)
  private
    _internal: pointer;

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

