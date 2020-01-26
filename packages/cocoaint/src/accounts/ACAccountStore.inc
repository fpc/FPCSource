{ Parsed from Accounts.framework ACAccountStore.h }


{$ifdef TYPES}
type
  ACAccountStorePtr = ^ACAccountStore;
{$endif}

{$ifdef TYPES}
type
  ACAccountCredentialRenewResult = NSInteger;
  ACAccountCredentialRenewResultPtr = ^ACAccountCredentialRenewResult;

const
  ACAccountCredentialRenewResultRenewed = 0;
  ACAccountCredentialRenewResultRejected = 1;
  ACAccountCredentialRenewResultFailed = 2;

type
  ACAccountStoreSaveCompletionHandler = OpaqueCBlock;
  ACAccountStoreRemoveCompletionHandler = OpaqueCBlock;
  ACAccountStoreRequestAccessCompletionHandler = OpaqueCBlock;
  ACAccountStoreCredentialRenewalHandler = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  ACAccountStore = objcclass external (NSObject)
  public
    function accounts: NSArray; message 'accounts';
    function accountWithIdentifier (identifier: NSString): ACAccount; message 'accountWithIdentifier:';
    function accountTypeWithAccountTypeIdentifier (typeIdentifier: NSString): ACAccountType; message 'accountTypeWithAccountTypeIdentifier:';
    function accountsWithAccountType (accountType: ACAccountType): NSArray; message 'accountsWithAccountType:';
    procedure saveAccount_withCompletionHandler (account: ACAccount; completionHandler: ACAccountStoreSaveCompletionHandler); message 'saveAccount:withCompletionHandler:';
    procedure requestAccessToAccountsWithType_withCompletionHandler (accountType: ACAccountType; handler: ACAccountStoreRequestAccessCompletionHandler); message 'requestAccessToAccountsWithType:withCompletionHandler:'; deprecated 'in NA, NA, 5_0, NA';
    procedure requestAccessToAccountsWithType_options_completion (accountType: ACAccountType; options: NSDictionary; completion: ACAccountStoreRequestAccessCompletionHandler); message 'requestAccessToAccountsWithType:options:completion:';
    procedure renewCredentialsForAccount_completion (account: ACAccount; completionHandler: ACAccountStoreCredentialRenewalHandler); message 'renewCredentialsForAccount:completion:';
    procedure removeAccount_withCompletionHandler (account: ACAccount; completionHandler: ACAccountStoreRemoveCompletionHandler); message 'removeAccount:withCompletionHandler:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ACAccountStoreDidChangeNotification: NSString { available in 10_8, 5_0 }; cvar; external;
{$endif}

