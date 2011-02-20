{ Parsed from Foundation.framework NSPortNameServer.h }

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
  NSPortNameServerPointer = ^NSPortNameServer;
  NSPortNameServerPtr = NSPortNameServerPointer;
  NSMachBootstrapServer = objcclass;
  NSMachBootstrapServerPointer = ^NSMachBootstrapServer;
  NSMachBootstrapServerPtr = NSMachBootstrapServerPointer;
  NSMessagePortNameServer = objcclass;
  NSMessagePortNameServerPointer = ^NSMessagePortNameServer;
  NSMessagePortNameServerPtr = NSMessagePortNameServerPointer;
  NSSocketPortNameServer = objcclass;
  NSSocketPortNameServerPointer = ^NSSocketPortNameServer;
  NSSocketPortNameServerPtr = NSSocketPortNameServerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPORTNAMESERVER_PAS_C}
{$define NSPORTNAMESERVER_PAS_C}

{ NSPortNameServer }
  NSPortNameServer = objcclass external (NSObject)
    
  public
    class function systemDefaultPortNameServer: NSPortNameServer; message 'systemDefaultPortNameServer';
    function portForName(name: NSString): NSPort; message 'portForName:';
    function portForName_host(name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name(port: NSPort; name: NSString): Boolean; message 'registerPort:name:';
    function removePortForName(name: NSString): Boolean; message 'removePortForName:';
  end;

{ NSMachBootstrapServer }
  NSMachBootstrapServer = objcclass external (NSPortNameServer)
    
  public
    class function sharedInstance: id; message 'sharedInstance';
    function portForName(name: NSString): NSPort; message 'portForName:';
    function portForName_host(name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name(port: NSPort; name: NSString): Boolean; message 'registerPort:name:';
    function servicePortWithName(name: NSString): NSPort; message 'servicePortWithName:';
  end;

{ NSMessagePortNameServer }
  NSMessagePortNameServer = objcclass external (NSPortNameServer)
    
  public
    class function sharedInstance: id; message 'sharedInstance';
    function portForName(name: NSString): NSPort; message 'portForName:';
    function portForName_host(name: NSString; host: NSString): NSPort; message 'portForName:host:';
  end;

{ NSSocketPortNameServer }
  NSSocketPortNameServer = objcclass external (NSPortNameServer)
    
  public
    class function sharedInstance: id; message 'sharedInstance';
    function portForName(name: NSString): NSPort; message 'portForName:';
    function portForName_host(name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name(port: NSPort; name: NSString): Boolean; message 'registerPort:name:';
    function removePortForName(name: NSString): Boolean; message 'removePortForName:';
    function portForName_host_nameServerPortNumber(name: NSString; host: NSString; portNumber: cuint16): NSPort; message 'portForName:host:nameServerPortNumber:';
    function registerPort_name_nameServerPortNumber(port: NSPort; name: NSString; portNumber: cuint16): Boolean; message 'registerPort:name:nameServerPortNumber:';
    procedure setDefaultNameServerPortNumber(portNumber: cuint16); message 'setDefaultNameServerPortNumber:';
    function defaultNameServerPortNumber: cuint16; message 'defaultNameServerPortNumber';
  end;

{$endif}
{$endif}
