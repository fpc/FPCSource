{ Parsed from AVFoundation.framework AVError.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  AVFoundationErrorDomain: NSString { available in 10_7, 4_0 }; cvar; external;
  AVErrorDeviceKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVErrorTimeKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVErrorFileSizeKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVErrorPIDKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVErrorRecordingSuccessfullyFinishedKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVErrorMediaTypeKey: NSString { available in 10_7, 4_3 }; cvar; external;
  AVErrorMediaSubTypeKey: NSString { available in 10_7, 4_3 }; cvar; external;
  AVErrorPresentationTimeStampKey: NSString { available in 10_10, 8_0 }; cvar; external;
  AVErrorPersistentTrackIDKey: NSString { available in 10_10, 8_0 }; cvar; external;
  AVErrorFileTypeKey: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVErrorDiscontinuityFlagsKey: NSString { available in 10_7, NA }; cvar; external;
{$endif}

{$endif}
{$ifdef TYPES}
type
  AVError = NSInteger;
  AVErrorPtr = ^AVError;

const
  AVErrorUnknown = -11800;
  AVErrorOutOfMemory = -11801;
  AVErrorSessionNotRunning = -11803;
  AVErrorDeviceAlreadyUsedByAnotherSession = -11804;
  AVErrorNoDataCaptured = -11805;
  AVErrorSessionConfigurationChanged = -11806;
  AVErrorDiskFull = -11807;
  AVErrorDeviceWasDisconnected = -11808;
  AVErrorMediaChanged = -11809;
  AVErrorMaximumDurationReached = -11810;
  AVErrorMaximumFileSizeReached = -11811;
  AVErrorMediaDiscontinuity = -11812;
  AVErrorMaximumNumberOfSamplesForFileFormatReached = -11813;
  AVErrorDeviceNotConnected = -11814;
  AVErrorDeviceInUseByAnotherApplication = -11815;
  AVErrorDeviceLockedForConfigurationByAnotherProcess = -11817;
  {$if defined(TARGET_OS_IPHONE)}
  AVErrorSessionWasInterrupted = -11818;
  AVErrorMediaServicesWereReset = -11819;
  {$endif}
  AVErrorExportFailed = -11820;
  AVErrorDecodeFailed = -11821;
  AVErrorInvalidSourceMedia = -11822;
  AVErrorFileAlreadyExists = -11823;
  AVErrorCompositionTrackSegmentsNotContiguous = -11824;
  AVErrorInvalidCompositionTrackSegmentDuration = -11825;
  AVErrorInvalidCompositionTrackSegmentSourceStartTime = -11826;
  AVErrorInvalidCompositionTrackSegmentSourceDuration = -11827;
  AVErrorFileFormatNotRecognized = -11828;
  AVErrorFileFailedToParse = -11829;
  AVErrorMaximumStillImageCaptureRequestsExceeded = -11830;
  AVErrorContentIsProtected = -11831;
  AVErrorNoImageAtTime = -11832;
  AVErrorDecoderNotFound = -11833;
  AVErrorEncoderNotFound = -11834;
  AVErrorContentIsNotAuthorized = -11835;
  AVErrorApplicationIsNotAuthorized = -11836;
  {$if defined(TARGET_OS_IPHONE)}
  AVErrorDeviceIsNotAvailableInBackground = -11837;
  {$endif}
  AVErrorOperationNotSupportedForAsset = -11838;
  AVErrorDecoderTemporarilyUnavailable = -11839;
  AVErrorEncoderTemporarilyUnavailable = -11840;
  AVErrorInvalidVideoComposition = -11841;
  AVErrorReferenceForbiddenByReferencePolicy = -11842;
  AVErrorInvalidOutputURLPathExtension = -11843;
  AVErrorScreenCaptureFailed = -11844;
  AVErrorDisplayWasDisabled = -11845;
  AVErrorTorchLevelUnavailable = -11846;
  {$if defined(TARGET_OS_IPHONE)}
  AVErrorOperationInterrupted = -11847;
  {$endif}
  AVErrorIncompatibleAsset = -11848;
  AVErrorFailedToLoadMediaData = -11849;
  AVErrorServerIncorrectlyConfigured = -11850;
  AVErrorApplicationIsNotAuthorizedToUseDevice = -11852;
  AVErrorFailedToParse = -11853 { available in 10_10, 8_0 };
  AVErrorFileTypeDoesNotSupportSampleReferences = -11854 { available in 10_10, 8_0 };
  AVErrorUndecodableMediaData = -11855 { available in 10_10, 8_0 };
{$endif}

