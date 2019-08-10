{ Parsed from Foundation.framework NSPortNameServer.h }


{$ifdef TYPES}
type
  NSPortNameServerPtr = ^NSPortNameServer;
  NSMachBootstrapServerPtr = ^NSMachBootstrapServer;
  NSMessagePortNameServerPtr = ^NSMessagePortNameServer;
  NSSocketPortNameServerPtr = ^NSSocketPortNameServer;
{$endif}

{$ifdef CLASSES}

type
  NSPortNameServer = objcclass external (NSObject)
  public
    class function systemDefaultPortNameServer: NSPortNameServer; message 'systemDefaultPortNameServer';
    function portForName (name: NSString): NSPort; message 'portForName:';
    function portForName_host (name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name (port: NSPort; name: NSString): ObjCBOOL; message 'registerPort:name:';
    function removePortForName (name: NSString): ObjCBOOL; message 'removePortForName:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))}
{$ifdef CLASSES}

type
  NSMachBootstrapServer = objcclass external (NSPortNameServer)
  public
    class function sharedInstance: id; message 'sharedInstance';
    function portForName (name: NSString): NSPort; message 'portForName:';
    function portForName_host (name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name (port: NSPort; name: NSString): ObjCBOOL; message 'registerPort:name:';
    function servicePortWithName (name: NSString): NSPort; message 'servicePortWithName:'; { available in 10_5, 2_0 }
  end;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  NSMessagePortNameServer = objcclass external (NSPortNameServer)
  public
    class function sharedInstance: id; message 'sharedInstance';
    function portForName (name: NSString): NSPort; message 'portForName:';
    function portForName_host (name: NSString; host: NSString): NSPort; message 'portForName:host:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or (defined(TARGET_OS_WIN32))}
{$ifdef CLASSES}

type
  NSSocketPortNameServer = objcclass external (NSPortNameServer)
  public
    class function sharedInstance: id; message 'sharedInstance';
    function portForName (name: NSString): NSPort; message 'portForName:';
    function portForName_host (name: NSString; host: NSString): NSPort; message 'portForName:host:';
    function registerPort_name (port: NSPort; name: NSString): ObjCBOOL; message 'registerPort:name:';
    function removePortForName (name: NSString): ObjCBOOL; message 'removePortForName:';
    function portForName_host_nameServerPortNumber (name: NSString; host: NSString; portNumber: cuint16): NSPort; message 'portForName:host:nameServerPortNumber:';
    function registerPort_name_nameServerPortNumber (port: NSPort; name: NSString; portNumber: cuint16): ObjCBOOL; message 'registerPort:name:nameServerPortNumber:';
    procedure setDefaultNameServerPortNumber(newValue: cuint16); message 'setDefaultNameServerPortNumber:';
    function defaultNameServerPortNumber: cuint16; message 'defaultNameServerPortNumber';
  end;
{$endif}

{$endif}
