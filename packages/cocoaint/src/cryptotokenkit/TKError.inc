{ Parsed from CryptoTokenKit.framework TKError.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  TKErrorDomain: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  TKErrorCode = NSInteger;
  TKErrorCodePtr = ^TKErrorCode;

const
  TKErrorCodeNotImplemented = -1;
  TKErrorCodeCommunicationError = -2;
  TKErrorCodeCorruptedData = -3;
  TKErrorCodeCanceledByUser = -4;
  TKErrorAuthenticationFailed = -5;
  TKErrorObjectNotFound = -6;
  TKErrorTokenNotFound = -7;
{$endif}

