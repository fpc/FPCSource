{ Parsed from Accounts.framework ACAccountCredential.h }


{$ifdef TYPES}
type
  ACAccountCredentialPtr = ^ACAccountCredential;
{$endif}

{$ifdef CLASSES}

type
  ACAccountCredential = objcclass external (NSObject)
  public
    function initWithOAuthToken_tokenSecret (token: NSString; secret: NSString): instancetype; message 'initWithOAuthToken:tokenSecret:';
    function initWithOAuth2Token_refreshToken_expiryDate (token: NSString; refreshToken: NSString; expiryDate: NSDate): instancetype; message 'initWithOAuth2Token:refreshToken:expiryDate:';
    procedure setOauthToken(newValue: NSString); message 'setOauthToken:';
    function oauthToken: NSString; message 'oauthToken';
  end;
{$endif}

