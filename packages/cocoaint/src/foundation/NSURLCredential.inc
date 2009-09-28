{ Parsed from Foundation.framework NSURLCredential.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSURLCREDENTIAL_PAS_H}
{$define NSURLCREDENTIAL_PAS_H}
type
  NSURLCredentialPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLCREDENTIAL_PAS_C}
{$define NSURLCREDENTIAL_PAS_C}

{ NSURLCredential }
  NSURLCredential = objcclass(NSObject, NSCopyingProtocol)
  private
    __internal: NSURLCredentialInternal;
    
  public
    class function alloc: NSURLCredential; message 'alloc';

    function initWithUser_password_persistence(user_: NSString; password_: NSString; persistence_: NSURLCredentialPersistence): id; message 'initWithUser:password:persistence:';
    class function credentialWithUser_password_persistence(user_: NSString; password_: NSString; persistence_: NSURLCredentialPersistence): NSURLCredential; message 'credentialWithUser:password:persistence:';
    function user: NSString; message 'user';
    function password: NSString; message 'password';
    function hasPassword: Boolean; message 'hasPassword';
    function persistence: NSURLCredentialPersistence; message 'persistence';
  end; external;

{$endif}
{$endif}
