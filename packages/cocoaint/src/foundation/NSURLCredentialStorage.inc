{ Parsed from Foundation.framework NSURLCredentialStorage.h }

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

{ External string constants }
var
  NSURLCredentialStorageChangedNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLCredentialStorage = objcclass;
  NSURLCredentialStoragePointer = ^NSURLCredentialStorage;
  NSURLCredentialStoragePtr = NSURLCredentialStoragePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLCREDENTIALSTORAGE_PAS_C}
{$define NSURLCREDENTIALSTORAGE_PAS_C}

{ NSURLCredentialStorage }
  NSURLCredentialStorage = objcclass external (NSObject)
  private
    _internal: NSURLCredentialStorageInternal;
    
  public
    class function sharedCredentialStorage: NSURLCredentialStorage; message 'sharedCredentialStorage';
    function credentialsForProtectionSpace(space: NSURLProtectionSpace): NSDictionary; message 'credentialsForProtectionSpace:';
    function allCredentials: NSDictionary; message 'allCredentials';
    procedure setCredential_forProtectionSpace(credential: NSURLCredential; space: NSURLProtectionSpace); message 'setCredential:forProtectionSpace:';
    procedure removeCredential_forProtectionSpace(credential: NSURLCredential; space: NSURLProtectionSpace); message 'removeCredential:forProtectionSpace:';
    function defaultCredentialForProtectionSpace(space: NSURLProtectionSpace): NSURLCredential; message 'defaultCredentialForProtectionSpace:';
    procedure setDefaultCredential_forProtectionSpace(credential: NSURLCredential; space: NSURLProtectionSpace); message 'setDefaultCredential:forProtectionSpace:';
  end;

{$endif}
{$endif}
