{ Parsed from Foundation.framework NSURLCredential.h }


{$ifdef TYPES}
type
  NSURLCredentialPtr = ^NSURLCredential;
{$endif}

{$ifdef TYPES}
type
  NSURLCredentialPersistence = NSUInteger;
  NSURLCredentialPersistencePtr = ^NSURLCredentialPersistence;

const
  NSURLCredentialPersistenceNone = 0;
  NSURLCredentialPersistenceForSession = 1;
  NSURLCredentialPersistencePermanent = 2;
  NSURLCredentialPersistenceSynchronizable = 3 { available in 10_8, 6_0 };
{$endif}

{$ifdef CLASSES}

type
  NSURLCredential = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _internal: NSURLCredentialInternal;
  public
    function persistence: NSURLCredentialPersistence; message 'persistence';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSInternetPassword = objccategory external (NSURLCredential)
    function initWithUser_password_persistence (user: NSString; password: NSString; persistence_: NSURLCredentialPersistence): instancetype; message 'initWithUser:password:persistence:';
    class function credentialWithUser_password_persistence (user: NSString; password: NSString; persistence_: NSURLCredentialPersistence): NSURLCredential; message 'credentialWithUser:password:persistence:';
    function user: NSString; message 'user';
    function password: NSString; message 'password';
    function hasPassword: ObjCBOOL; message 'hasPassword';
  end;

type
  NSClientCertificate = objccategory external (NSURLCredential)
    function initWithIdentity_certificates_persistence (identity: SecIdentityRef; certArray: NSArray; persistence_: NSURLCredentialPersistence): instancetype; message 'initWithIdentity:certificates:persistence:'; { available in 10_6, 3_0 }
    class function credentialWithIdentity_certificates_persistence (identity: SecIdentityRef; certArray: NSArray; persistence_: NSURLCredentialPersistence): NSURLCredential; message 'credentialWithIdentity:certificates:persistence:'; { available in 10_6, 3_0 }
    function identity: SecIdentityRef; message 'identity';
    function certificates: NSArray; message 'certificates';
  end;

type
  NSServerTrust = objccategory external (NSURLCredential)
    function initWithTrust (trust: SecTrustRef): instancetype; message 'initWithTrust:'; { available in 10_6, 3_0 }
    class function credentialForTrust (trust: SecTrustRef): NSURLCredential; message 'credentialForTrust:'; { available in 10_6, 3_0 }
  end;
{$endif}

