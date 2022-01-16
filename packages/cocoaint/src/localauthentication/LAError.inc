{ Parsed from LocalAuthentication.framework LAError.h }

{$ifdef TYPES}
type
  LAError = NSInteger;
  LAErrorPtr = ^LAError;

const
  LAErrorAuthenticationFailed = kLAErrorAuthenticationFailed;
  LAErrorUserCancel = kLAErrorUserCancel;
  LAErrorUserFallback = kLAErrorUserFallback;
  LAErrorSystemCancel = kLAErrorSystemCancel;
  LAErrorPasscodeNotSet = kLAErrorPasscodeNotSet;
  LAErrorTouchIDNotAvailable = kLAErrorTouchIDNotAvailable;
  LAErrorTouchIDNotEnrolled = kLAErrorTouchIDNotEnrolled;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  LAErrorDomain: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

