{ Parsed from MultipeerConnectivity.framework MCError.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  MCErrorDomain: NSStringPtr; cvar; external;
{$endif}

{$ifdef TYPES}
type
  MCErrorCode = NSInteger;
  MCErrorCodePtr = ^MCErrorCode;

const
  MCErrorUnknown = 0;
  MCErrorNotConnected = 1;
  MCErrorInvalidParameter = 2;
  MCErrorUnsupported = 3;
  MCErrorTimedOut = 4;
  MCErrorCancelled = 5;
  MCErrorUnavailable = 6;
{$endif}

