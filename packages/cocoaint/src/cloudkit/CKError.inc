{ Parsed from CloudKit.framework CKError.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  CKErrorDomain: NSString { available in 10_10, 8_0 }; cvar; external;
  CKPartialErrorsByItemIDKey: NSString { available in 10_10, 8_0 }; cvar; external;
  CKRecordChangedErrorAncestorRecordKey: NSString { available in 10_10, 8_0 }; cvar; external;
  CKRecordChangedErrorServerRecordKey: NSString { available in 10_10, 8_0 }; cvar; external;
  CKRecordChangedErrorClientRecordKey: NSString { available in 10_10, 8_0 }; cvar; external;
  CKErrorRetryAfterKey: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  CKErrorCode = NSInteger;
  CKErrorCodePtr = ^CKErrorCode;

const
  CKErrorInternalError = 1;
  CKErrorPartialFailure = 2;
  CKErrorNetworkUnavailable = 3;
  CKErrorNetworkFailure = 4;
  CKErrorBadContainer = 5;
  CKErrorServiceUnavailable = 6;
  CKErrorRequestRateLimited = 7;
  CKErrorMissingEntitlement = 8;
  CKErrorNotAuthenticated = 9;
  CKErrorPermissionFailure = 10;
  CKErrorUnknownItem = 11;
  CKErrorInvalidArguments = 12;
  CKErrorResultsTruncated = 13;
  CKErrorServerRecordChanged = 14;
  CKErrorServerRejectedRequest = 15;
  CKErrorAssetFileNotFound = 16;
  CKErrorAssetFileModified = 17;
  CKErrorIncompatibleVersion = 18;
  CKErrorConstraintViolation = 19;
  CKErrorOperationCancelled = 20;
  CKErrorChangeTokenExpired = 21;
  CKErrorBatchRequestFailed = 22;
  CKErrorZoneBusy = 23;
  CKErrorBadDatabase = 24;
  CKErrorQuotaExceeded = 25;
  CKErrorZoneNotFound = 26;
  CKErrorLimitExceeded = 27;
  CKErrorUserDeletedZone = 28;
{$endif}

