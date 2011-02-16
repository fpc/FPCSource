{ Parsed from Foundation.framework NSDistantObject.h }

{$ifdef TYPES}
{$ifndef NSDISTANTOBJECT_PAS_T}
{$define NSDISTANTOBJECT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDISTANTOBJECT_PAS_R}
{$define NSDISTANTOBJECT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDISTANTOBJECT_PAS_F}
{$define NSDISTANTOBJECT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDISTANTOBJECT_PAS_S}
{$define NSDISTANTOBJECT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDistantObject = objcclass;
  NSDistantObjectPointer = ^NSDistantObject;
  NSDistantObjectPtr = NSDistantObjectPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDISTANTOBJECT_PAS_C}
{$define NSDISTANTOBJECT_PAS_C}

{ NSDistantObject }
  NSDistantObject = objcclass external (NSProxy, NSCodingProtocol)
  private
    _knownSelectors: id;
    _wireCount: NSUInteger;
    _refCount: NSUInteger;
    _proto: id;
    ___2: cuint16;
    ___1: cuint8;
    _wireType: cuint8;
    _remoteClass: id;
    
  public
    class function proxyWithTarget_connection(target: id; connection: NSConnection): NSDistantObject; message 'proxyWithTarget:connection:';
    function initWithTarget_connection(target: id; connection: NSConnection): id; message 'initWithTarget:connection:';
    class function proxyWithLocal_connection(target: id; connection: NSConnection): NSDistantObject; message 'proxyWithLocal:connection:';
    function initWithLocal_connection(target: id; connection: NSConnection): id; message 'initWithLocal:connection:';
    procedure setProtocolForProxy(proto: objc_protocol); message 'setProtocolForProxy:';
    function connectionForProxy: NSConnection; message 'connectionForProxy';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
