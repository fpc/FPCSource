{ Parsed from AVFoundation.framework AVVideoSettings.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  AVVideoCodecKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoCodecH264: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoCodecJPEG: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoCodecAppleProRes4444: NSString { available in 10_7, NA }; cvar; external;
  AVVideoCodecAppleProRes422: NSString { available in 10_7, NA }; cvar; external;
  AVVideoWidthKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoHeightKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoPixelAspectRatioKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoPixelAspectRatioHorizontalSpacingKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoPixelAspectRatioVerticalSpacingKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoCleanApertureKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoCleanApertureWidthKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoCleanApertureHeightKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoCleanApertureHorizontalOffsetKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoCleanApertureVerticalOffsetKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoScalingModeKey: NSString { available in 10_7, 5_0 }; cvar; external;
  AVVideoScalingModeFit: NSString { available in 10_7, 5_0 }; cvar; external;
  AVVideoScalingModeResize: NSString { available in 10_7, 5_0 }; cvar; external;
  AVVideoScalingModeResizeAspect: NSString { available in 10_7, 5_0 }; cvar; external;
  AVVideoScalingModeResizeAspectFill: NSString { available in 10_7, 5_0 }; cvar; external;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVVideoColorPropertiesKey: NSString { available in 10_7, NA }; cvar; external;
  AVVideoColorPrimariesKey: NSString { available in 10_7, NA }; cvar; external;
  AVVideoColorPrimaries_ITU_R_709_2: NSString { available in 10_7, NA }; cvar; external;
  AVVideoColorPrimaries_EBU_3213: NSString { available in 10_7, NA }; cvar; external;
  AVVideoColorPrimaries_SMPTE_C: NSString { available in 10_7, NA }; cvar; external;
  AVVideoTransferFunctionKey: NSString { available in 10_7, NA }; cvar; external;
  AVVideoTransferFunction_ITU_R_709_2: NSString { available in 10_7, NA }; cvar; external;
  AVVideoTransferFunction_SMPTE_240M_1995: NSString { available in 10_7, NA }; cvar; external;
  AVVideoYCbCrMatrixKey: NSString { available in 10_7, NA }; cvar; external;
  AVVideoYCbCrMatrix_ITU_R_709_2: NSString { available in 10_7, NA }; cvar; external;
  AVVideoYCbCrMatrix_ITU_R_601_4: NSString { available in 10_7, NA }; cvar; external;
  AVVideoYCbCrMatrix_SMPTE_240M_1995: NSString { available in 10_7, NA }; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  AVVideoCompressionPropertiesKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoAverageBitRateKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoQualityKey: NSString { available in 10_7, 5_0 }; cvar; external;
  AVVideoMaxKeyFrameIntervalKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVVideoMaxKeyFrameIntervalDurationKey: NSString { available in 10_9, 7_0 }; cvar; external;
  AVVideoAllowFrameReorderingKey: NSString { available in 10_10, 7_0 }; cvar; external;
  AVVideoProfileLevelKey: NSString { available in 10_8, 4_0 }; cvar; external;
  AVVideoProfileLevelH264Baseline30: NSString { available in 10_8, 4_0 }; cvar; external;
  AVVideoProfileLevelH264Baseline31: NSString { available in 10_8, 4_0 }; cvar; external;
  AVVideoProfileLevelH264Baseline41: NSString { available in 10_8, 5_0 }; cvar; external;
  AVVideoProfileLevelH264BaselineAutoLevel: NSString { available in 10_9, 7_0 }; cvar; external;
  AVVideoProfileLevelH264Main30: NSString { available in 10_8, 4_0 }; cvar; external;
  AVVideoProfileLevelH264Main31: NSString { available in 10_8, 4_0 }; cvar; external;
  AVVideoProfileLevelH264Main32: NSString { available in 10_8, 5_0 }; cvar; external;
  AVVideoProfileLevelH264Main41: NSString { available in 10_8, 5_0 }; cvar; external;
  AVVideoProfileLevelH264MainAutoLevel: NSString { available in 10_9, 7_0 }; cvar; external;
  AVVideoProfileLevelH264High40: NSString { available in 10_9, 6_0 }; cvar; external;
  AVVideoProfileLevelH264High41: NSString { available in 10_9, 6_0 }; cvar; external;
  AVVideoProfileLevelH264HighAutoLevel: NSString { available in 10_9, 7_0 }; cvar; external;
  AVVideoH264EntropyModeKey: NSString { available in 10_10, 7_0 }; cvar; external;
  AVVideoH264EntropyModeCAVLC: NSString { available in 10_10, 7_0 }; cvar; external;
  AVVideoH264EntropyModeCABAC: NSString { available in 10_10, 7_0 }; cvar; external;
  AVVideoExpectedSourceFrameRateKey: NSString { available in 10_10, 7_0 }; cvar; external;
  AVVideoAverageNonDroppableFrameRateKey: NSString { available in 10_10, 7_0 }; cvar; external;
  AVVideoEncoderSpecificationKey: NSString { available in 10_10, NA }; cvar; external;
{$endif}

