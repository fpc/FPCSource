{ Parsed from Foundation.framework NSURLProtectionSpace.h }


{$ifdef TYPES}
type
  NSURLProtectionSpacePtr = ^NSURLProtectionSpace;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSURLProtectionSpaceHTTP: NSString { available in 10_5, 2_0 }; cvar; external;
  NSURLProtectionSpaceHTTPS: NSString { available in 10_5, 2_0 }; cvar; external;
  NSURLProtectionSpaceFTP: NSString { available in 10_5, 2_0 }; cvar; external;
  NSURLProtectionSpaceHTTPProxy: NSString; cvar; external;
  NSURLProtectionSpaceHTTPSProxy: NSString; cvar; external;
  NSURLProtectionSpaceFTPProxy: NSString; cvar; external;
  NSURLProtectionSpaceSOCKSProxy: NSString; cvar; external;
  NSURLAuthenticationMethodDefault: NSString; cvar; external;
  NSURLAuthenticationMethodHTTPBasic: NSString; cvar; external;
  NSURLAuthenticationMethodHTTPDigest: NSString; cvar; external;
  NSURLAuthenticationMethodHTMLForm: NSString; cvar; external;
  NSURLAuthenticationMethodNTLM: NSString { available in 10_5, 2_0 }; cvar; external;
  NSURLAuthenticationMethodNegotiate: NSString { available in 10_5, 2_0 }; cvar; external;
  NSURLAuthenticationMethodClientCertificate: NSString { available in 10_6, 3_0 }; cvar; external;
  NSURLAuthenticationMethodServerTrust: NSString { available in 10_6, 3_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSURLProtectionSpace = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _internal: NSURLProtectionSpaceInternal;
  public
    function initWithHost_port_protocol_realm_authenticationMethod (host: NSString; port: NSInteger; protocol: NSString; realm: NSString; authenticationMethod: NSString): instancetype; message 'initWithHost:port:protocol:realm:authenticationMethod:';
    function initWithProxyHost_port_type_realm_authenticationMethod (host: NSString; port: NSInteger; type_: NSString; realm: NSString; authenticationMethod: NSString): instancetype; message 'initWithProxyHost:port:type:realm:authenticationMethod:';
    function realm: NSString; message 'realm';
    function receivesCredentialSecurely: ObjCBOOL; message 'receivesCredentialSecurely';
    function isProxy: ObjCBOOL; message 'isProxy';
    function host: NSString; message 'host';
    function port: NSInteger; message 'port';
    function proxyType: NSString; message 'proxyType';
    function protocol: NSString; message 'protocol';
    function authenticationMethod: NSString; message 'authenticationMethod';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSClientCertificateSpace = objccategory external (NSURLProtectionSpace)
    function distinguishedNames: NSArray; message 'distinguishedNames';
  end;

type
  NSServerTrustValidationSpace = objccategory external (NSURLProtectionSpace)
    function serverTrust: SecTrustRef; message 'serverTrust';
  end;
{$endif}

