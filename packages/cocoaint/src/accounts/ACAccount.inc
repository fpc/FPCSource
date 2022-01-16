{ Parsed from Accounts.framework ACAccount.h }


{$ifdef TYPES}
type
  ACAccountPtr = ^ACAccount;
{$endif}

{$ifdef CLASSES}

type
  ACAccount = objcclass external (NSObject)
  public
    function initWithAccountType (type_: ACAccountType): instancetype; message 'initWithAccountType:'; { NS_DESIGNATED_INITIALIZER }
    function identifier: NSString; message 'identifier';
    procedure setAccountType(newValue: ACAccountType); message 'setAccountType:';
    function accountType: ACAccountType; message 'accountType';
    procedure setAccountDescription(newValue: NSString); message 'setAccountDescription:';
    function accountDescription: NSString; message 'accountDescription';
    procedure setUsername(newValue: NSString); message 'setUsername:';
    function username: NSString; message 'username';
    function userFullName: NSString; message 'userFullName';
    procedure setCredential(newValue: ACAccountCredential); message 'setCredential:';
    function credential: ACAccountCredential; message 'credential';
  end;
{$endif}

