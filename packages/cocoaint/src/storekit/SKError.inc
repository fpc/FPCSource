{ Parsed from StoreKit.framework SKError.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  SKErrorDomain: NSString { available in 10_7, NA }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  SKErrorUnknown = 0;
  SKErrorClientInvalid = 1;
  SKErrorPaymentCancelled = 2;
  SKErrorPaymentInvalid = 3;
  SKErrorPaymentNotAllowed = 4;
{$endif}

