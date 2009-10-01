{ Parsed from Foundation.framework NSURLCredentialStorage.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSURLCREDENTIALSTORAGE_PAS_T}
{$define NSURLCREDENTIALSTORAGE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLCREDENTIALSTORAGE_PAS_R}
{$define NSURLCREDENTIALSTORAGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLCREDENTIALSTORAGE_PAS_F}
{$define NSURLCREDENTIALSTORAGE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLCREDENTIALSTORAGE_PAS_S}
{$define NSURLCREDENTIALSTORAGE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLCredentialStorage = objcclass;
  NSURLCredentialStoragePointer = ^NSURLCredentialStorage;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLCREDENTIALSTORAGE_PAS_C}
{$define NSURLCREDENTIALSTORAGE_PAS_C}

{ NSURLCredentialStorage }
  NSURLCredentialStorage = objcclass(NSObject)
  private
    __internal: NSURLCredentialStorageInternal;
    
  public
    class function alloc: NSURLCredentialStorage; message 'alloc';

    class function sharedCredentialStorage: NSURLCredentialStorage; message 'sharedCredentialStorage';
    function credentialsForProtectionSpace(space: NSURLProtectionSpace): NSDictionary; message 'credentialsForProtectionSpace:';
    function allCredentials: NSDictionary; message 'allCredentials';
    procedure setCredential_forProtectionSpace(credential: NSURLCredential; space: NSURLProtectionSpace); message 'setCredential:forProtectionSpace:';
    procedure removeCredential_forProtectionSpace(credential: NSURLCredential; space: NSURLProtectionSpace); message 'removeCredential:forProtectionSpace:';
    function defaultCredentialForProtectionSpace(space: NSURLProtectionSpace): NSURLCredential; message 'defaultCredentialForProtectionSpace:';
    procedure setDefaultCredential_forProtectionSpace(credential: NSURLCredential; space: NSURLProtectionSpace); message 'setDefaultCredential:forProtectionSpace:';
  end; external;

{$endif}
{$endif}
