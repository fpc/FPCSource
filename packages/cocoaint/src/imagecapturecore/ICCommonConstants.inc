{ Parsed from ImageCaptureCore.framework ICCommonConstants.h }

{$ifdef TYPES}
const
  ICEXIFOrientation1 = 1;
  ICEXIFOrientation2 = 2;
  ICEXIFOrientation3 = 3;
  ICEXIFOrientation4 = 4;
  ICEXIFOrientation5 = 5;
  ICEXIFOrientation6 = 6;
  ICEXIFOrientation7 = 7;
  ICEXIFOrientation8 = 8;

type
  ICEXIFOrientationType = NSUInteger;
  ICEXIFOrientationTypePtr = ^ICEXIFOrientationType;

const
  ICReturnSuccess = 0;
  ICReturnInvalidParam = -9922;
  ICReturnCommunicationTimedOut = -9923;
  ICReturnScanOperationCanceled = -9924;
  ICReturnScannerInUseByLocalUser = -9925;
  ICReturnScannerInUseByRemoteUser = -9926;
  ICReturnDeviceFailedToOpenSession = -9927;
  ICReturnDeviceFailedToCloseSession = -9928;
  ICReturnScannerFailedToSelectFunctionalUnit = -9929;
  ICReturnScannerFailedToCompleteOverviewScan = -9930;
  ICReturnScannerFailedToCompleteScan = -9931;
  ICReturnReceivedUnsolicitedScannerStatusInfo = -9932;
  ICReturnReceivedUnsolicitedScannerErrorInfo = -9933;
  ICReturnDownloadFailed = -9934;
  ICReturnUploadFailed = -9935;
  ICReturnFailedToCompletePassThroughCommand = -9936;
  ICReturnDownloadCanceled = -9937;
  ICReturnFailedToEnabeTethering = -9938;
  ICReturnFailedToDisabeTethering = -9939;
  ICReturnFailedToCompleteSendMessageRequest = -9940;
  ICReturnDeleteFilesFailed = -9941;
  ICReturnDeleteFilesCanceled = -9942;
  ICReturnDeviceIsPasscodeLocked = -9943;
  ICReturnDeviceFailedToTakePicture = -9944;
  ICReturnDeviceSoftwareNotInstalled = -9945;
  ICReturnDeviceSoftwareIsBeingInstalled = -9946;
  ICReturnDeviceSoftwareInstallationCompleted = -9947;
  ICReturnDeviceSoftwareInstallationCanceled = -9948;
  ICReturnDeviceSoftwareInstallationFailed = -9949;
  ICReturnDeviceSoftwareNotAvailable = -9950;
type
  ICReturnCode = clong;
{$endif}

