{ Parsed from CoreBluetooth.framework CBError.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  CBErrorDomain: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  CBError = NSInteger;
  CBErrorPtr = ^CBError;

const
  CBErrorUnknown = 0;
  CBErrorInvalidParameters = 1 { available in 10_9, 6_0 };
  CBErrorInvalidHandle = 2 { available in 10_9, 6_0 };
  CBErrorNotConnected = 3 { available in 10_9, 6_0 };
  CBErrorOutOfSpace = 4 { available in 10_9, 6_0 };
  CBErrorOperationCancelled = 5 { available in 10_9, 6_0 };
  CBErrorConnectionTimeout = 6 { available in 10_9, 6_0 };
  CBErrorPeripheralDisconnected = 7 { available in 10_9, 6_0 };
  CBErrorUUIDNotAllowed = 8 { available in 10_9, 6_0 };
  CBErrorAlreadyAdvertising = 9 { available in 10_9, 6_0 };
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CBATTErrorDomain: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  CBATTError = NSInteger;
  CBATTErrorPtr = ^CBATTError;

const
  CBATTErrorSuccess = $00 { available in 10_9, 6_0 };
  CBATTErrorInvalidHandle = $01;
  CBATTErrorReadNotPermitted = $02;
  CBATTErrorWriteNotPermitted = $03;
  CBATTErrorInvalidPdu = $04;
  CBATTErrorInsufficientAuthentication = $05;
  CBATTErrorRequestNotSupported = $06;
  CBATTErrorInvalidOffset = $07;
  CBATTErrorInsufficientAuthorization = $08;
  CBATTErrorPrepareQueueFull = $09;
  CBATTErrorAttributeNotFound = $0A;
  CBATTErrorAttributeNotLong = $0B;
  CBATTErrorInsufficientEncryptionKeySize = $0C;
  CBATTErrorInvalidAttributeValueLength = $0D;
  CBATTErrorUnlikelyError = $0E;
  CBATTErrorInsufficientEncryption = $0F;
  CBATTErrorUnsupportedGroupType = $10;
  CBATTErrorInsufficientResources = $11;
{$endif}

