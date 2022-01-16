{ Parsed from QTKit.framework QTError.h }

{$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
{$ifdef EXTERNAL_SYMBOLS}
var
  QTErrorRecordingSuccesfullyFinishedKey: NSString deprecated 'available in 7.2 and later but deprecated in 7.7'; cvar; external;
{$endif}

{$ifdef TYPES}
const
  QTErrorUnknown = -1;
  QTErrorIncompatibleInput = 1002;
  QTErrorIncompatibleOutput = 1003;
  QTErrorInvalidInputsOrOutputs = 1100;
  QTErrorDeviceAlreadyUsedbyAnotherSession = 1101;
  QTErrorNoDataCaptured = 1200;
  QTErrorSessionConfigurationChanged = 1201;
  QTErrorDiskFull = 1202;
  QTErrorDeviceWasDisconnected = 1203;
  QTErrorMediaChanged = 1204;
  QTErrorMaximumDurationReached = 1205;
  QTErrorMaximumFileSizeReached = 1206;
  QTErrorMediaDiscontinuity = 1207;
  {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_6_3)}
  QTErrorMaximumNumberOfSamplesForFileFormatReached = 1208;
  {$endif}
  QTErrorDeviceNotConnected = 1300;
  QTErrorDeviceInUseByAnotherApplication = 1301;
  QTErrorDeviceExcludedByAnotherDevice = 1302;
  QTErrorInvalidDestinationFileTypeForExport = 1501;
  QTErrorInvalidSourceFileTypeForExport = 1502;
  QTErrorExportExecutionFailed = 1503;
  QTErrorExportInsufficientSpaceOnDevice = 1504;
  QTErrorExportNoSuchDirectoryOrFile = 1505;
  QTErrorExportIOError = 1506;
{$endif}

{$endif}
