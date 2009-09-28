{ Parsed from Foundation.framework NSPortNameServer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPORTNAMESERVER_PAS_H}
{$define NSPORTNAMESERVER_PAS_H}
type
  NSPortNameServerPointer = Pointer;
  NSMachBootstrapServerPointer = Pointer;
  NSMessagePortNameServerPointer = Pointer;
  NSSocketPortNameServerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSPORTNAMESERVER_PAS_T}
{$define NSPORTNAMESERVER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPORTNAMESERVER_PAS_R}
{$define NSPORTNAMESERVER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPORTNAMESERVER_PAS_F}
{$define NSPORTNAMESERVER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPORTNAMESERVER_PAS_S}
{$define NSPORTNAMESERVER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPortNameServer = objcclass;
  NSMachBootstrapServer = objcclass;
  NSMessagePortNameServer = objcclass;
  NSSocketPortNameServer = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPORTNAMESERVER_PAS_C}
{$define NSPORTNAMESERVER_PAS_C}

{ NSPortNameServer }
  NSPortNameServer = objcclass(NSObject)
    
  public
    class function alloc: NSPortNameServer; message 'alloc';

    class function systemDefaultPortNameServer: NSPortNameServer; message 'systemDefaultPortNameServer';
    function portForName(name: NSString): NSPort; message 'portForName:';
    function portForName_host(name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name(port: NSPort; name: NSString): Boolean; message 'registerPort:name:';
    function removePortForName(name: NSString): Boolean; message 'removePortForName:';
  end; external;

{ NSMachBootstrapServer }
  NSMachBootstrapServer = objcclass(NSPortNameServer)
    
  public
    class function alloc: NSMachBootstrapServer; message 'alloc';

    class function sharedInstance: id; message 'sharedInstance';
    function portForName(name: NSString): NSPort; message 'portForName:';
    function portForName_host(name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name(port: NSPort; name: NSString): Boolean; message 'registerPort:name:';
    function servicePortWithName(name: NSString): NSPort; message 'servicePortWithName:';
  end; external;

{ NSMessagePortNameServer }
  NSMessagePortNameServer = objcclass(NSPortNameServer)
    
  public
    class function alloc: NSMessagePortNameServer; message 'alloc';

    class function sharedInstance: id; message 'sharedInstance';
    function portForName(name: NSString): NSPort; message 'portForName:';
    function portForName_host(name: NSString; host: NSString): NSPort; message 'portForName:host:';
  end; external;

{ NSSocketPortNameServer }
  NSSocketPortNameServer = objcclass(NSPortNameServer)
    
  public
    class function alloc: NSSocketPortNameServer; message 'alloc';

    class function sharedInstance: id; message 'sharedInstance';
    function portForName(name: NSString): NSPort; message 'portForName:';
    function portForName_host(name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name(port: NSPort; name: NSString): Boolean; message 'registerPort:name:';
    function removePortForName(name: NSString): Boolean; message 'removePortForName:';
    function portForName_host_nameServerPortNumber(name: NSString; host: NSString; portNumber: word): NSPort; message 'portForName:host:nameServerPortNumber:';
    function registerPort_name_nameServerPortNumber(port: NSPort; name: NSString; portNumber: word): Boolean; message 'registerPort:name:nameServerPortNumber:';
    procedure setDefaultNameServerPortNumber(portNumber: word); message 'setDefaultNameServerPortNumber:';
    function defaultNameServerPortNumber: word; message 'defaultNameServerPortNumber';
  end; external;

{$endif}
{$endif}
