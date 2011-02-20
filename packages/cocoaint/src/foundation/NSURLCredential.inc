{ Parsed from Foundation.framework NSURLCredential.h }

{$ifdef TYPES}
{$ifndef NSURLCREDENTIAL_PAS_T}
{$define NSURLCREDENTIAL_PAS_T}

{ Constants }

const
  NSURLCredentialPersistenceNone = 0;
  NSURLCredentialPersistenceForSession = 1;
  NSURLCredentialPersistencePermanent = 2;

{ Types }
type
  NSURLCredentialPersistence = NSUInteger;
  NSURLCredentialPersistencePtr = ^NSURLCredentialPersistence;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLCREDENTIAL_PAS_R}
{$define NSURLCREDENTIAL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLCREDENTIAL_PAS_F}
{$define NSURLCREDENTIAL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLCREDENTIAL_PAS_S}
{$define NSURLCREDENTIAL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLCredential = objcclass;
  NSURLCredentialPointer = ^NSURLCredential;
  NSURLCredentialPtr = NSURLCredentialPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLCREDENTIAL_PAS_C}
{$define NSURLCREDENTIAL_PAS_C}

{ NSURLCredential }
  NSURLCredential = objcclass external (NSObject, NSCopyingProtocol)
  private
    _internal: NSURLCredentialInternal; {garbage collector: __strong }
    
  public
    function persistence: NSURLCredentialPersistence; message 'persistence';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{ NSInternetPasswordCategory }
  NSInternetPasswordCategory = objccategory external (NSURLCredential)
    function initWithUser_password_persistence(user_: NSString; password_: NSString; persistence_: NSURLCredentialPersistence): id; message 'initWithUser:password:persistence:';
    class function credentialWithUser_password_persistence(user_: NSString; password_: NSString; persistence_: NSURLCredentialPersistence): NSURLCredential; message 'credentialWithUser:password:persistence:';
    function user: NSString; message 'user';
    function password: NSString; message 'password';
    function hasPassword: Boolean; message 'hasPassword';
  end;

{ NSClientCertificateCategory }
  NSClientCertificateCategory = objccategory external (NSURLCredential)
    function initWithIdentity_certificates_persistence(identity_: SecIdentityRef; certArray: NSArray; persistence_: NSURLCredentialPersistence): id; message 'initWithIdentity:certificates:persistence:';
    class function credentialWithIdentity_certificates_persistence(identity_: SecIdentityRef; certArray: NSArray; persistence_: NSURLCredentialPersistence): NSURLCredential; message 'credentialWithIdentity:certificates:persistence:';
    function identity: SecIdentityRef; message 'identity';
    function certificates: NSArray; message 'certificates';
  end;

{ NSServerTrustCategory }
  NSServerTrustCategory = objccategory external (NSURLCredential)
    function initWithTrust(trust: SecTrustRef): id; message 'initWithTrust:';
    class function credentialForTrust(trust: SecTrustRef): NSURLCredential; message 'credentialForTrust:';
  end;

{$endif}
{$endif}
