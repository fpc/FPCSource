{ Parsed from Foundation.framework NSURLProtectionSpace.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSURLPROTECTIONSPACE_PAS_H}
{$define NSURLPROTECTIONSPACE_PAS_H}
type
  NSURLProtectionSpacePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSURLPROTECTIONSPACE_PAS_T}
{$define NSURLPROTECTIONSPACE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLPROTECTIONSPACE_PAS_R}
{$define NSURLPROTECTIONSPACE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLPROTECTIONSPACE_PAS_F}
{$define NSURLPROTECTIONSPACE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLPROTECTIONSPACE_PAS_T}
{$define NSURLPROTECTIONSPACE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLProtectionSpace = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLPROTECTIONSPACE_PAS_C}
{$define NSURLPROTECTIONSPACE_PAS_C}

{ NSURLProtectionSpace }
  NSURLProtectionSpace = objcclass(NSObject, NSCopyingProtocol)
  private
    __internal: NSURLProtectionSpaceInternal;
    
  public
    class function alloc: NSURLProtectionSpace; message 'alloc';

    function initWithHost_port_protocol_realm_authenticationMethod(host_: NSString; port_: NSInteger; protocol_: NSString; realm_: NSString; authenticationMethod_: NSString): id; message 'initWithHost:port:protocol:realm:authenticationMethod:';
    function initWithProxyHost_port_type_realm_authenticationMethod(host_: NSString; port_: NSInteger; type_: NSString; realm_: NSString; authenticationMethod_: NSString): id; message 'initWithProxyHost:port:type:realm:authenticationMethod:';
    function realm: NSString; message 'realm';
    function receivesCredentialSecurely: Boolean; message 'receivesCredentialSecurely';
    function isProxy: Boolean; message 'isProxy';
    function host: NSString; message 'host';
    function port: NSInteger; message 'port';
    function proxyType: NSString; message 'proxyType';
    function protocol: NSString; message 'protocol';
    function authenticationMethod: NSString; message 'authenticationMethod';
  end; external;

{$endif}
{$endif}
