{ Parsed from Foundation.framework NSURLProtectionSpace.h }

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
{$ifndef NSURLPROTECTIONSPACE_PAS_S}
{$define NSURLPROTECTIONSPACE_PAS_S}

{ External string constants }
var
  NSURLProtectionSpaceHTTP: NSString; cvar; external;
  NSURLProtectionSpaceHTTPS: NSString; cvar; external;
  NSURLProtectionSpaceFTP: NSString; cvar; external;
  NSURLProtectionSpaceHTTPProxy: NSString; cvar; external;
  NSURLProtectionSpaceHTTPSProxy: NSString; cvar; external;
  NSURLProtectionSpaceFTPProxy: NSString; cvar; external;
  NSURLProtectionSpaceSOCKSProxy: NSString; cvar; external;
  NSURLAuthenticationMethodDefault: NSString; cvar; external;
  NSURLAuthenticationMethodHTTPBasic: NSString; cvar; external;
  NSURLAuthenticationMethodHTTPDigest: NSString; cvar; external;
  NSURLAuthenticationMethodHTMLForm: NSString; cvar; external;
  NSURLAuthenticationMethodNTLM: NSString; cvar; external;
  NSURLAuthenticationMethodNegotiate: NSString; cvar; external;
  NSURLAuthenticationMethodClientCertificate: NSString; cvar; external;
  NSURLAuthenticationMethodServerTrust: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLProtectionSpace = objcclass;
  NSURLProtectionSpacePointer = ^NSURLProtectionSpace;
  NSURLProtectionSpacePtr = NSURLProtectionSpacePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLPROTECTIONSPACE_PAS_C}
{$define NSURLPROTECTIONSPACE_PAS_C}

{ NSURLProtectionSpace }
  NSURLProtectionSpace = objcclass external (NSObject, NSCopyingProtocol)
  private
    _internal: NSURLProtectionSpaceInternal;
    
  public
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

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{ NSClientCertificateSpaceCategory }
  NSClientCertificateSpaceCategory = objccategory external (NSURLProtectionSpace)
    function distinguishedNames: NSArray; message 'distinguishedNames';
  end;

{ NSServerTrustValidationSpaceCategory }
  NSServerTrustValidationSpaceCategory = objccategory external (NSURLProtectionSpace)
    function serverTrust: SecTrustRef; message 'serverTrust';
  end;

{$endif}
{$endif}
