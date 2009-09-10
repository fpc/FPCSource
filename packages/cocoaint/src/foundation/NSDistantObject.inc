{ Parsed from Foundation.framework NSDistantObject.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDISTANTOBJECT_PAS_H}
{$define NSDISTANTOBJECT_PAS_H}
type
  NSDistantObjectPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSDISTANTOBJECT_PAS_T}
{$define NSDISTANTOBJECT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDistantObject = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDISTANTOBJECT_PAS_C}
{$define NSDISTANTOBJECT_PAS_C}

{ NSDistantObject }
  NSDistantObject = objcclass(NSProxy, NSCodingProtocol)
  private
    __knownSelectors: id;
    __wireCount: culong;
    __refCount: culong;
    __proto: id;
    ____2: word;
    ____1: byte;
    __wireType: byte;
    __remoteClass: id;
    
  public
    class function alloc: NSDistantObject; message 'alloc';

    class function proxyWithTarget_connection(target: id; connection: NSConnection): NSDistantObject; message 'proxyWithTarget:connection:';
    function initWithTarget_connection(target: id; connection: NSConnection): id; message 'initWithTarget:connection:';
    class function proxyWithLocal_connection(target: id; connection: NSConnection): NSDistantObject; message 'proxyWithLocal:connection:';
    function initWithLocal_connection(target: id; connection: NSConnection): id; message 'initWithLocal:connection:';
    procedure setProtocolForProxy(proto: objc_protocol); message 'setProtocolForProxy:';
    function connectionForProxy: NSConnection; message 'connectionForProxy';
  end; external;

{$endif}
{$endif}
