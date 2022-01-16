{ Parsed from AVFoundation.framework AVAudioSettings.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  AVFormatIDKey: NSString; cvar; external;
  AVSampleRateKey: NSString; cvar; external;
  AVNumberOfChannelsKey: NSString; cvar; external;
  AVLinearPCMBitDepthKey: NSString; cvar; external;
  AVLinearPCMIsBigEndianKey: NSString; cvar; external;
  AVLinearPCMIsFloatKey: NSString; cvar; external;
  AVLinearPCMIsNonInterleaved: NSString { available in 10_7, 4_0 }; cvar; external;
  AVEncoderAudioQualityKey: NSString; cvar; external;
  AVEncoderAudioQualityForVBRKey: NSString { available in 10_9, 7_0 }; cvar; external;
  AVEncoderBitRateKey: NSString; cvar; external;
  AVEncoderBitRatePerChannelKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVEncoderBitRateStrategyKey: NSString { available in 10_9, 7_0 }; cvar; external;
  AVEncoderBitDepthHintKey: NSString; cvar; external;
  AVSampleRateConverterAlgorithmKey: NSString { available in 10_9, 7_0 }; cvar; external;
  AVSampleRateConverterAudioQualityKey: NSString; cvar; external;
  AVChannelLayoutKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVAudioBitRateStrategy_Constant: NSString { available in 10_9, 7_0 }; cvar; external;
  AVAudioBitRateStrategy_LongTermAverage: NSString { available in 10_9, 7_0 }; cvar; external;
  AVAudioBitRateStrategy_VariableConstrained: NSString { available in 10_9, 7_0 }; cvar; external;
  AVAudioBitRateStrategy_Variable: NSString { available in 10_9, 7_0 }; cvar; external;
  AVSampleRateConverterAlgorithm_Normal: NSString { available in 10_9, 7_0 }; cvar; external;
  AVSampleRateConverterAlgorithm_Mastering: NSString { available in 10_9, 7_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  AVAudioQuality = NSInteger;
  AVAudioQualityPtr = ^AVAudioQuality;

const
  AVAudioQualityMin = 0;
  AVAudioQualityLow = $20;
  AVAudioQualityMedium = $40;
  AVAudioQualityHigh = $60;
  AVAudioQualityMax = $7F;
{$endif}

