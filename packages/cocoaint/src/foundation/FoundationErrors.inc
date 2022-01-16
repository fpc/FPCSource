{ Parsed from Foundation.framework FoundationErrors.h }

{$ifdef TYPES}
const
  NSFileNoSuchFileError = 4;
  NSFileLockingError = 255;
  NSFileReadUnknownError = 256;
  NSFileReadNoPermissionError = 257;
  NSFileReadInvalidFileNameError = 258;
  NSFileReadCorruptFileError = 259;
  NSFileReadNoSuchFileError = 260;
  NSFileReadInapplicableStringEncodingError = 261;
  NSFileReadUnsupportedSchemeError = 262;
  NSFileReadTooLargeError = 263 { available in 10_5, 2_0 };
  NSFileReadUnknownStringEncodingError = 264 { available in 10_5, 2_0 };
  NSFileWriteUnknownError = 512;
  NSFileWriteNoPermissionError = 513;
  NSFileWriteInvalidFileNameError = 514;
  NSFileWriteFileExistsError = 516 { available in 10_7, 5_0 };
  NSFileWriteInapplicableStringEncodingError = 517;
  NSFileWriteUnsupportedSchemeError = 518;
  NSFileWriteOutOfSpaceError = 640;
  NSFileWriteVolumeReadOnlyError = 642 { available in 10_6, 4_0 };
  NSKeyValueValidationError = 1024;
  NSFormattingError = 2048;
  NSUserCancelledError = 3072;
  NSFeatureUnsupportedError = 3328 { available in 10_8, 6_0 };
  NSExecutableNotLoadableError = 3584 { available in 10_5, 2_0 };
  NSExecutableArchitectureMismatchError = 3585 { available in 10_5, 2_0 };
  NSExecutableRuntimeMismatchError = 3586 { available in 10_5, 2_0 };
  NSExecutableLoadError = 3587 { available in 10_5, 2_0 };
  NSExecutableLinkError = 3588 { available in 10_5, 2_0 };
  NSFileErrorMinimum = 0;
  NSFileErrorMaximum = 1023;
  NSValidationErrorMinimum = 1024;
  NSValidationErrorMaximum = 2047;
  NSExecutableErrorMinimum = 3584 { available in 10_5, 2_0 };
  NSExecutableErrorMaximum = 3839 { available in 10_5, 2_0 };
  NSFormattingErrorMinimum = 2048;
  NSFormattingErrorMaximum = 2559;
  NSPropertyListReadCorruptError = 3840 { available in 10_6, 4_0 };
  NSPropertyListReadUnknownVersionError = 3841 { available in 10_6, 4_0 };
  NSPropertyListReadStreamError = 3842 { available in 10_6, 4_0 };
  NSPropertyListWriteStreamError = 3851 { available in 10_6, 4_0 };
  NSPropertyListWriteInvalidError = 3852 { available in 10_10, 8_0 };
  NSPropertyListErrorMinimum = 3840 { available in 10_6, 4_0 };
  NSPropertyListErrorMaximum = 4095 { available in 10_6, 4_0 };
  NSXPCConnectionInterrupted = 4097 { available in 10_8, 6_0 };
  NSXPCConnectionInvalid = 4099 { available in 10_8, 6_0 };
  NSXPCConnectionReplyInvalid = 4101 { available in 10_8, 6_0 };
  NSXPCConnectionErrorMinimum = 4096 { available in 10_8, 6_0 };
  NSXPCConnectionErrorMaximum = 4224 { available in 10_8, 6_0 };
  NSUbiquitousFileUnavailableError = 4353 { available in 10_9, 7_0 };
  NSUbiquitousFileNotUploadedDueToQuotaError = 4354 { available in 10_9, 7_0 };
  NSUbiquitousFileUbiquityServerNotAvailable = 4355 { available in 10_9, 7_0 };
  NSUbiquitousFileErrorMinimum = 4352 { available in 10_9, 7_0 };
  NSUbiquitousFileErrorMaximum = 4607 { available in 10_9, 7_0 };
  NSUserActivityHandoffFailedError = 4608 { available in 10_10, 8_0 };
  NSUserActivityConnectionUnavailableError = 4609 { available in 10_10, 8_0 };
  NSUserActivityRemoteApplicationTimedOutError = 4610 { available in 10_10, 8_0 };
  NSUserActivityHandoffUserInfoTooLargeError = 4611 { available in 10_10, 8_0 };
  NSUserActivityErrorMinimum = 4608 { available in 10_10, 8_0 };
  NSUserActivityErrorMaximum = 4863 { available in 10_10, 8_0 };
{$endif}

