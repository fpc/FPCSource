{ Parsed from Foundation.framework NSURLError.h }

{$ifdef TYPES}
{$ifndef NSURLERROR_PAS_T}
{$define NSURLERROR_PAS_T}

{ Constants }

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
  NSURLErrorDataLengthExceedsMaximum = kCFURLErrorDataLengthExceedsMaximum;
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

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLERROR_PAS_R}
{$define NSURLERROR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLERROR_PAS_F}
{$define NSURLERROR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLERROR_PAS_S}
{$define NSURLERROR_PAS_S}

{ External string constants }
var
  NSURLErrorDomain: NSString; cvar; external;
  NSURLErrorFailingURLErrorKey: NSString; cvar; external;
  NSURLErrorFailingURLStringErrorKey: NSString; cvar; external;
  NSErrorFailingURLStringKey: NSString deprecated 'in Mac OS X 10.6 and later'; cvar; external;
  NSURLErrorFailingURLPeerTrustErrorKey: NSString; cvar; external;

{$endif}
{$endif}
