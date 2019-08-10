{ Parsed from Foundation.framework NSURLError.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  NSURLErrorDomain: NSString; cvar; external;
  NSURLErrorFailingURLErrorKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSURLErrorFailingURLStringErrorKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSErrorFailingURLStringKey: NSString deprecated 'in 10_0, 10_6, 2_0, 4_0'; cvar; external;
  NSURLErrorFailingURLPeerTrustErrorKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSURLErrorBackgroundTaskCancelledReasonKey: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  NSURLErrorCancelledReasonUserForceQuitApplication = 0;
  NSURLErrorCancelledReasonBackgroundUpdatesDisabled = 1;
  NSURLErrorCancelledReasonInsufficientSystemResources = 2 { available in 10_10, 8_0 };
const
  NSURLErrorUnknown = -1;
  NSURLErrorCancelled = kCFURLErrorCancelled;
  NSURLErrorBadURL = kCFURLErrorBadURL;
  NSURLErrorTimedOut = kCFURLErrorTimedOut;
  NSURLErrorUnsupportedURL = kCFURLErrorUnsupportedURL;
  NSURLErrorCannotFindHost = kCFURLErrorCannotFindHost;
  NSURLErrorCannotConnectToHost = kCFURLErrorCannotConnectToHost;
  NSURLErrorNetworkConnectionLost = kCFURLErrorNetworkConnectionLost;
  NSURLErrorDNSLookupFailed = kCFURLErrorDNSLookupFailed;
  NSURLErrorHTTPTooManyRedirects = kCFURLErrorHTTPTooManyRedirects;
  NSURLErrorResourceUnavailable = kCFURLErrorResourceUnavailable;
  NSURLErrorNotConnectedToInternet = kCFURLErrorNotConnectedToInternet;
  NSURLErrorRedirectToNonExistentLocation = kCFURLErrorRedirectToNonExistentLocation;
  NSURLErrorBadServerResponse = kCFURLErrorBadServerResponse;
  NSURLErrorUserCancelledAuthentication = kCFURLErrorUserCancelledAuthentication;
  NSURLErrorUserAuthenticationRequired = kCFURLErrorUserAuthenticationRequired;
  NSURLErrorZeroByteResource = kCFURLErrorZeroByteResource;
  NSURLErrorCannotDecodeRawData = kCFURLErrorCannotDecodeRawData;
  NSURLErrorCannotDecodeContentData = kCFURLErrorCannotDecodeContentData;
  NSURLErrorCannotParseResponse = kCFURLErrorCannotParseResponse;
  NSURLErrorFileDoesNotExist = kCFURLErrorFileDoesNotExist;
  NSURLErrorFileIsDirectory = kCFURLErrorFileIsDirectory;
  NSURLErrorNoPermissionsToReadFile = kCFURLErrorNoPermissionsToReadFile;
  NSURLErrorDataLengthExceedsMaximum = kCFURLErrorDataLengthExceedsMaximum { available in 10_5, 2_0 };
  NSURLErrorSecureConnectionFailed = kCFURLErrorSecureConnectionFailed;
  NSURLErrorServerCertificateHasBadDate = kCFURLErrorServerCertificateHasBadDate;
  NSURLErrorServerCertificateUntrusted = kCFURLErrorServerCertificateUntrusted;
  NSURLErrorServerCertificateHasUnknownRoot = kCFURLErrorServerCertificateHasUnknownRoot;
  NSURLErrorServerCertificateNotYetValid = kCFURLErrorServerCertificateNotYetValid;
  NSURLErrorClientCertificateRejected = kCFURLErrorClientCertificateRejected;
  NSURLErrorClientCertificateRequired = kCFURLErrorClientCertificateRequired;
  NSURLErrorCannotLoadFromNetwork = kCFURLErrorCannotLoadFromNetwork;
  NSURLErrorCannotCreateFile = kCFURLErrorCannotCreateFile;
  NSURLErrorCannotOpenFile = kCFURLErrorCannotOpenFile;
  NSURLErrorCannotCloseFile = kCFURLErrorCannotCloseFile;
  NSURLErrorCannotWriteToFile = kCFURLErrorCannotWriteToFile;
  NSURLErrorCannotRemoveFile = kCFURLErrorCannotRemoveFile;
  NSURLErrorCannotMoveFile = kCFURLErrorCannotMoveFile;
  NSURLErrorDownloadDecodingFailedMidStream = kCFURLErrorDownloadDecodingFailedMidStream;
  NSURLErrorDownloadDecodingFailedToComplete = kCFURLErrorDownloadDecodingFailedToComplete;
  NSURLErrorInternationalRoamingOff = kCFURLErrorInternationalRoamingOff { available in 10_7, 3_0 };
  NSURLErrorCallIsActive = kCFURLErrorCallIsActive { available in 10_7, 3_0 };
  NSURLErrorDataNotAllowed = kCFURLErrorDataNotAllowed { available in 10_7, 3_0 };
  NSURLErrorRequestBodyStreamExhausted = kCFURLErrorRequestBodyStreamExhausted { available in 10_7, 3_0 };
  NSURLErrorBackgroundSessionRequiresSharedContainer = -995 { available in 10_10, 8_0 };
  NSURLErrorBackgroundSessionInUseByAnotherProcess = -996 { available in 10_10, 8_0 };
  NSURLErrorBackgroundSessionWasDisconnected = -997 { available in 10_10, 8_0 };
{$endif}

