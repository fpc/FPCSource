{ Parsed from CoreAudio.framework CoreAudioTypes.h }

{$if  not (defined(__CoreAudioTypes_h__))}
{$ifdef TYPES}
const
  COREAUDIOTYPES_VERSION = 1051;
{$endif}

{$if  not (defined(CA_PREFER_FIXED_POINT))}

{$if defined(TARGET_OS_IPHONE)}

{$if (defined(TARGET_CPU_X86) or defined(TARGET_CPU_X86_64) or defined(TARGET_CPU_PPC) or defined(TARGET_CPU_PPC64)) and not defined(TARGET_IPHONE_SIMULATOR)}
{$ifdef TYPES}
const
  CA_PREFER_FIXED_POINT = 0;
{$endif}

{$else}
{$ifdef TYPES}
const
  CA_PREFER_FIXED_POINT = 1;
{$endif}

{$endif}

{$else}
{$ifdef TYPES}
const
  CA_PREFER_FIXED_POINT = 0;
{$endif}

{$endif}
{$endif}

{$ifdef TYPES}
const
  kAudio_UnimplementedError = -4;
  kAudio_FileNotFoundError = -43;
  kAudio_FilePermissionError = -54;
  kAudio_TooManyFilesOpenError = -42;
  kAudio_BadFilePathError = '!pth';
  kAudio_ParamError = -50;
  kAudio_MemFullError = -108;

type
  AudioValueRange = record
    mMinimum: Float64;
    mMaximum: Float64;
  end;
type
  AudioValueRangePtr = ^AudioValueRange;
type
  AudioValueTranslation = record
    mInputData: pointer;
    mInputDataSize: UInt32;
    mOutputData: pointer;
    mOutputDataSize: UInt32;
  end;
type
  AudioValueTranslationPtr = ^AudioValueTranslation;
type
  AudioBuffer = record
    mNumberChannels: UInt32;
    mDataByteSize: UInt32;
    mData: pointer;
  end;
type
  AudioBufferPtr = ^AudioBuffer;
type
  AudioBufferList = record
    mNumberBuffers: UInt32;
    mBuffers:array[0..0] of AudioBuffer;
    {$if (defined(__cplusplus)) and defined(CA_STRICT)}
    AudioBufferList&): AudioBufferList(;
    AudioBufferList&)_: AudioBufferList& operator=(;
    {$endif}
  end;
type
  AudioBufferListPtr = ^AudioBufferList;
{$endif}

{$endif}

{$if  not defined(CA_PREFER_FIXED_POINT)}
{$ifdef TYPES}
type
  AudioSampleType = Float32;
  AudioSampleTypePtr = ^AudioSampleType;
  AudioUnitSampleType = Float32;
  AudioUnitSampleTypePtr = ^AudioUnitSampleType;
{$endif}

{$else}
{$ifdef TYPES}
type
  AudioSampleType = SInt16;
  AudioSampleTypePtr = ^AudioSampleType;
  AudioUnitSampleType = SInt32;
  AudioUnitSampleTypePtr = ^AudioUnitSampleType;

const
  kAudioUnitSampleFractionBits = 24;
{$endif}

{$endif}
{$ifdef TYPES}
type
  AudioFormatID = UInt32;
  AudioFormatIDPtr = ^AudioFormatID;
  AudioFormatFlags = UInt32;
  AudioFormatFlagsPtr = ^AudioFormatFlags;

type
  AudioStreamBasicDescription = record
    mSampleRate: Float64;
    mFormatID: AudioFormatID;
    mFormatFlags: AudioFormatFlags;
    mBytesPerPacket: UInt32;
    mFramesPerPacket: UInt32;
    mBytesPerFrame: UInt32;
    mChannelsPerFrame: UInt32;
    mBitsPerChannel: UInt32;
    mReserved: UInt32;
  end;
type
  AudioStreamBasicDescriptionPtr = ^AudioStreamBasicDescription;

const
  kAudioStreamAnyRate = 0;
const
  kAudioFormatLinearPCM = 'lpcm';
  kAudioFormatAC3 = 'ac-3';
  kAudioFormat60958AC3 = 'cac3';
  kAudioFormatAppleIMA4 = 'ima4';
  kAudioFormatMPEG4AAC = 'aac ';
  kAudioFormatMPEG4CELP = 'celp';
  kAudioFormatMPEG4HVXC = 'hvxc';
  kAudioFormatMPEG4TwinVQ = 'twvq';
  kAudioFormatMACE3 = 'MAC3';
  kAudioFormatMACE6 = 'MAC6';
  kAudioFormatULaw = 'ulaw';
  kAudioFormatALaw = 'alaw';
  kAudioFormatQDesign = 'QDMC';
  kAudioFormatQDesign2 = 'QDM2';
  kAudioFormatQUALCOMM = 'Qclp';
  kAudioFormatMPEGLayer1 = '.mp1';
  kAudioFormatMPEGLayer2 = '.mp2';
  kAudioFormatMPEGLayer3 = '.mp3';
  kAudioFormatTimeCode = 'time';
  kAudioFormatMIDIStream = 'midi';
  kAudioFormatParameterValueStream = 'apvs';
  kAudioFormatAppleLossless = 'alac';
  kAudioFormatMPEG4AAC_HE = 'aach';
  kAudioFormatMPEG4AAC_LD = 'aacl';
  kAudioFormatMPEG4AAC_ELD = 'aace';
  kAudioFormatMPEG4AAC_ELD_SBR = 'aacf';
  kAudioFormatMPEG4AAC_ELD_V2 = 'aacg';
  kAudioFormatMPEG4AAC_HE_V2 = 'aacp';
  kAudioFormatMPEG4AAC_Spatial = 'aacs';
  kAudioFormatAMR = 'samr';
  kAudioFormatAMR_WB = 'sawb';
  kAudioFormatAudible = 'AUDB';
  kAudioFormatiLBC = 'ilbc';
  kAudioFormatDVIIntelIMA = $6D730011;
  kAudioFormatMicrosoftGSM = $6D730031;
  kAudioFormatAES3 = 'aes3';
const
  kAudioFormatFlagIsFloat = 1 shl 0;
  kAudioFormatFlagIsBigEndian = 1 shl 1;
  kAudioFormatFlagIsSignedInteger = 1 shl 2;
  kAudioFormatFlagIsPacked = 1 shl 3;
  kAudioFormatFlagIsAlignedHigh = 1 shl 4;
  kAudioFormatFlagIsNonInterleaved = 1 shl 5;
  kAudioFormatFlagIsNonMixable = 1 shl 6;
  kAudioFormatFlagsAreAllClear = 1 shl 31;
  kLinearPCMFormatFlagIsFloat = kAudioFormatFlagIsFloat;
  kLinearPCMFormatFlagIsBigEndian = kAudioFormatFlagIsBigEndian;
  kLinearPCMFormatFlagIsSignedInteger = kAudioFormatFlagIsSignedInteger;
  kLinearPCMFormatFlagIsPacked = kAudioFormatFlagIsPacked;
  kLinearPCMFormatFlagIsAlignedHigh = kAudioFormatFlagIsAlignedHigh;
  kLinearPCMFormatFlagIsNonInterleaved = kAudioFormatFlagIsNonInterleaved;
  kLinearPCMFormatFlagIsNonMixable = kAudioFormatFlagIsNonMixable;
  kLinearPCMFormatFlagsSampleFractionShift = 7;
  kLinearPCMFormatFlagsSampleFractionMask = $3F shl kLinearPCMFormatFlagsSampleFractionShift;
  kLinearPCMFormatFlagsAreAllClear = kAudioFormatFlagsAreAllClear;
  kAppleLosslessFormatFlag_16BitSourceData = 1;
  kAppleLosslessFormatFlag_20BitSourceData = 2;
  kAppleLosslessFormatFlag_24BitSourceData = 3;
  kAppleLosslessFormatFlag_32BitSourceData = 4;
const
  {$if defined(TARGET_RT_BIG_ENDIAN)}
  kAudioFormatFlagsNativeEndian = kAudioFormatFlagIsBigEndian;
  {$else}
  kAudioFormatFlagsNativeEndian = 0;
  {$endif}
  {$if  not defined(CA_PREFER_FIXED_POINT)}
  kAudioFormatFlagsCanonical = 1;
  kAudioFormatFlagsAudioUnitCanonical = 2;
  {$else}
  kAudioFormatFlagsCanonical = 3;
  kAudioFormatFlagsAudioUnitCanonical = 4;
  {$endif}
  kAudioFormatFlagsNativeFloatPacked = kAudioFormatFlagIsFloat + kAudioFormatFlagsNativeEndian + kAudioFormatFlagIsPacked;
{$endif}

{$ifdef TYPES}
type
  AudioStreamPacketDescription = record
    mStartOffset: SInt64;
    mVariableFramesInPacket: UInt32;
    mDataByteSize: UInt32;
  end;
type
  AudioStreamPacketDescriptionPtr = ^AudioStreamPacketDescription;
{$endif}

{$if  not (defined(__SMPTETime__))}
{$ifdef TYPES}
type
  SMPTETime = record
    mSubframes: SInt16;
    mSubframeDivisor: SInt16;
    mCounter: UInt32;
    mType: UInt32;
    mFlags: UInt32;
    mHours: SInt16;
    mMinutes: SInt16;
    mSeconds: SInt16;
    mFrames: SInt16;
  end;
type
  SMPTETimePtr = ^SMPTETime;

const
  kSMPTETimeType24 = 0;
  kSMPTETimeType25 = 1;
  kSMPTETimeType30Drop = 2;
  kSMPTETimeType30 = 3;
  kSMPTETimeType2997 = 4;
  kSMPTETimeType2997Drop = 5;
  kSMPTETimeType60 = 6;
  kSMPTETimeType5994 = 7;
  kSMPTETimeType60Drop = 8;
  kSMPTETimeType5994Drop = 9;
  kSMPTETimeType50 = 10;
  kSMPTETimeType2398 = 11;
const
  kSMPTETimeValid = 1 shl 0;
  kSMPTETimeRunning = 1 shl 1;
{$endif}

{$endif}
{$ifdef TYPES}
type
  AudioTimeStamp = record
    mSampleTime: Float64;
    mHostTime: UInt64;
    mRateScalar: Float64;
    mWordClockTime: UInt64;
    mSMPTETime: SMPTETime;
    mFlags: UInt32;
    mReserved: UInt32;
  end;
type
  AudioTimeStampPtr = ^AudioTimeStamp;

const
  kAudioTimeStampSampleTimeValid = 1 shl 0;
  kAudioTimeStampHostTimeValid = 1 shl 1;
  kAudioTimeStampRateScalarValid = 1 shl 2;
  kAudioTimeStampWordClockTimeValid = 1 shl 3;
  kAudioTimeStampSMPTETimeValid = 1 shl 4;
const
  kAudioTimeStampSampleHostTimeValid = kAudioTimeStampSampleTimeValid + kAudioTimeStampHostTimeValid;
{$endif}

{$ifdef TYPES}
type
  AudioClassDescription = record
    mType: OSType;
    mSubType: OSType;
    mManufacturer: OSType;
  end;
type
  AudioClassDescriptionPtr = ^AudioClassDescription;

type
  AudioChannelLabel = UInt32;
  AudioChannelLabelPtr = ^AudioChannelLabel;
  AudioChannelLayoutTag = UInt32;
  AudioChannelLayoutTagPtr = ^AudioChannelLayoutTag;

type
  AudioChannelDescription = record
    mChannelLabel: AudioChannelLabel;
    mChannelFlags: UInt32;
    mCoordinates:array[0..2] of Float32;
  end;
type
  AudioChannelDescriptionPtr = ^AudioChannelDescription;
type
  AudioChannelLayout = record
    mChannelLayoutTag: AudioChannelLayoutTag;
    mChannelBitmap: UInt32;
    mNumberChannelDescriptions: UInt32;
    mChannelDescriptions:array[0..0] of AudioChannelDescription;
    {$if (defined(__cplusplus)) and defined(CA_STRICT)}
    AudioChannelLayout&): AudioChannelLayout(;
    AudioChannelLayout&)_: AudioChannelLayout& operator=(;
    {$endif}
  end;
type
  AudioChannelLayoutPtr = ^AudioChannelLayout;

const
  kAudioChannelLabel_Unknown = $FFFFFFFF;
  kAudioChannelLabel_Unused = 0;
  kAudioChannelLabel_UseCoordinates = 100;
  kAudioChannelLabel_Left = 1;
  kAudioChannelLabel_Right = 2;
  kAudioChannelLabel_Center = 3;
  kAudioChannelLabel_LFEScreen = 4;
  kAudioChannelLabel_LeftSurround = 5;
  kAudioChannelLabel_RightSurround = 6;
  kAudioChannelLabel_LeftCenter = 7;
  kAudioChannelLabel_RightCenter = 8;
  kAudioChannelLabel_CenterSurround = 9;
  kAudioChannelLabel_LeftSurroundDirect = 10;
  kAudioChannelLabel_RightSurroundDirect = 11;
  kAudioChannelLabel_TopCenterSurround = 12;
  kAudioChannelLabel_VerticalHeightLeft = 13;
  kAudioChannelLabel_VerticalHeightCenter = 14;
  kAudioChannelLabel_VerticalHeightRight = 15;
  kAudioChannelLabel_TopBackLeft = 16;
  kAudioChannelLabel_TopBackCenter = 17;
  kAudioChannelLabel_TopBackRight = 18;
  kAudioChannelLabel_RearSurroundLeft = 33;
  kAudioChannelLabel_RearSurroundRight = 34;
  kAudioChannelLabel_LeftWide = 35;
  kAudioChannelLabel_RightWide = 36;
  kAudioChannelLabel_LFE2 = 37;
  kAudioChannelLabel_LeftTotal = 38;
  kAudioChannelLabel_RightTotal = 39;
  kAudioChannelLabel_HearingImpaired = 40;
  kAudioChannelLabel_Narration = 41;
  kAudioChannelLabel_Mono = 42;
  kAudioChannelLabel_DialogCentricMix = 43;
  kAudioChannelLabel_CenterSurroundDirect = 44;
  kAudioChannelLabel_Haptic = 45;
  kAudioChannelLabel_Ambisonic_W = 200;
  kAudioChannelLabel_Ambisonic_X = 201;
  kAudioChannelLabel_Ambisonic_Y = 202;
  kAudioChannelLabel_Ambisonic_Z = 203;
  kAudioChannelLabel_MS_Mid = 204;
  kAudioChannelLabel_MS_Side = 205;
  kAudioChannelLabel_XY_X = 206;
  kAudioChannelLabel_XY_Y = 207;
  kAudioChannelLabel_HeadphonesLeft = 301;
  kAudioChannelLabel_HeadphonesRight = 302;
  kAudioChannelLabel_ClickTrack = 304;
  kAudioChannelLabel_ForeignLanguage = 305;
  kAudioChannelLabel_Discrete = 400;
  kAudioChannelLabel_Discrete_0 = (1 shl 16) + 0;
  kAudioChannelLabel_Discrete_1 = (1 shl 16) + 1;
  kAudioChannelLabel_Discrete_2 = (1 shl 16) + 2;
  kAudioChannelLabel_Discrete_3 = (1 shl 16) + 3;
  kAudioChannelLabel_Discrete_4 = (1 shl 16) + 4;
  kAudioChannelLabel_Discrete_5 = (1 shl 16) + 5;
  kAudioChannelLabel_Discrete_6 = (1 shl 16) + 6;
  kAudioChannelLabel_Discrete_7 = (1 shl 16) + 7;
  kAudioChannelLabel_Discrete_8 = (1 shl 16) + 8;
  kAudioChannelLabel_Discrete_9 = (1 shl 16) + 9;
  kAudioChannelLabel_Discrete_10 = (1 shl 16) + 10;
  kAudioChannelLabel_Discrete_11 = (1 shl 16) + 11;
  kAudioChannelLabel_Discrete_12 = (1 shl 16) + 12;
  kAudioChannelLabel_Discrete_13 = (1 shl 16) + 13;
  kAudioChannelLabel_Discrete_14 = (1 shl 16) + 14;
  kAudioChannelLabel_Discrete_15 = (1 shl 16) + 15;
  kAudioChannelLabel_Discrete_65535 = (1 shl 16) + 65535;
const
  kAudioChannelBit_Left = 1 shl 0;
  kAudioChannelBit_Right = 1 shl 1;
  kAudioChannelBit_Center = 1 shl 2;
  kAudioChannelBit_LFEScreen = 1 shl 3;
  kAudioChannelBit_LeftSurround = 1 shl 4;
  kAudioChannelBit_RightSurround = 1 shl 5;
  kAudioChannelBit_LeftCenter = 1 shl 6;
  kAudioChannelBit_RightCenter = 1 shl 7;
  kAudioChannelBit_CenterSurround = 1 shl 8;
  kAudioChannelBit_LeftSurroundDirect = 1 shl 9;
  kAudioChannelBit_RightSurroundDirect = 1 shl 10;
  kAudioChannelBit_TopCenterSurround = 1 shl 11;
  kAudioChannelBit_VerticalHeightLeft = 1 shl 12;
  kAudioChannelBit_VerticalHeightCenter = 1 shl 13;
  kAudioChannelBit_VerticalHeightRight = 1 shl 14;
  kAudioChannelBit_TopBackLeft = 1 shl 15;
  kAudioChannelBit_TopBackCenter = 1 shl 16;
  kAudioChannelBit_TopBackRight = 1 shl 17;
const
  kAudioChannelFlags_AllOff = 0;
  kAudioChannelFlags_RectangularCoordinates = 1 shl 0;
  kAudioChannelFlags_SphericalCoordinates = 1 shl 1;
  kAudioChannelFlags_Meters = 1 shl 2;
const
  kAudioChannelCoordinates_LeftRight = 0;
  kAudioChannelCoordinates_BackFront = 1;
  kAudioChannelCoordinates_DownUp = 2;
  kAudioChannelCoordinates_Azimuth = 0;
  kAudioChannelCoordinates_Elevation = 1;
  kAudioChannelCoordinates_Distance = 2;
const
  kAudioChannelLayoutTag_UseChannelDescriptions = (0 shl 16) + 0;
  kAudioChannelLayoutTag_UseChannelBitmap = (1 shl 16) + 0;
  kAudioChannelLayoutTag_Mono = (100 shl 16) + 1;
  kAudioChannelLayoutTag_Stereo = (101 shl 16) + 2;
  kAudioChannelLayoutTag_StereoHeadphones = (102 shl 16) + 2;
  kAudioChannelLayoutTag_MatrixStereo = (103 shl 16) + 2;
  kAudioChannelLayoutTag_MidSide = (104 shl 16) + 2;
  kAudioChannelLayoutTag_XY = (105 shl 16) + 2;
  kAudioChannelLayoutTag_Binaural = (106 shl 16) + 2;
  kAudioChannelLayoutTag_Ambisonic_B_Format = (107 shl 16) + 4;
  kAudioChannelLayoutTag_Quadraphonic = (108 shl 16) + 4;
  kAudioChannelLayoutTag_Pentagonal = (109 shl 16) + 5;
  kAudioChannelLayoutTag_Hexagonal = (110 shl 16) + 6;
  kAudioChannelLayoutTag_Octagonal = (111 shl 16) + 8;
  kAudioChannelLayoutTag_Cube = (112 shl 16) + 8;
  kAudioChannelLayoutTag_MPEG_1_0 = kAudioChannelLayoutTag_Mono;
  kAudioChannelLayoutTag_MPEG_2_0 = kAudioChannelLayoutTag_Stereo;
  kAudioChannelLayoutTag_MPEG_3_0_A = (113 shl 16) + 3;
  kAudioChannelLayoutTag_MPEG_3_0_B = (114 shl 16) + 3;
  kAudioChannelLayoutTag_MPEG_4_0_A = (115 shl 16) + 4;
  kAudioChannelLayoutTag_MPEG_4_0_B = (116 shl 16) + 4;
  kAudioChannelLayoutTag_MPEG_5_0_A = (117 shl 16) + 5;
  kAudioChannelLayoutTag_MPEG_5_0_B = (118 shl 16) + 5;
  kAudioChannelLayoutTag_MPEG_5_0_C = (119 shl 16) + 5;
  kAudioChannelLayoutTag_MPEG_5_0_D = (120 shl 16) + 5;
  kAudioChannelLayoutTag_MPEG_5_1_A = (121 shl 16) + 6;
  kAudioChannelLayoutTag_MPEG_5_1_B = (122 shl 16) + 6;
  kAudioChannelLayoutTag_MPEG_5_1_C = (123 shl 16) + 6;
  kAudioChannelLayoutTag_MPEG_5_1_D = (124 shl 16) + 6;
  kAudioChannelLayoutTag_MPEG_6_1_A = (125 shl 16) + 7;
  kAudioChannelLayoutTag_MPEG_7_1_A = (126 shl 16) + 8;
  kAudioChannelLayoutTag_MPEG_7_1_B = (127 shl 16) + 8;
  kAudioChannelLayoutTag_MPEG_7_1_C = (128 shl 16) + 8;
  kAudioChannelLayoutTag_Emagic_Default_7_1 = (129 shl 16) + 8;
  kAudioChannelLayoutTag_SMPTE_DTV = (130 shl 16) + 8;
  kAudioChannelLayoutTag_ITU_1_0 = kAudioChannelLayoutTag_Mono;
  kAudioChannelLayoutTag_ITU_2_0 = kAudioChannelLayoutTag_Stereo;
  kAudioChannelLayoutTag_ITU_2_1 = (131 shl 16) + 3;
  kAudioChannelLayoutTag_ITU_2_2 = (132 shl 16) + 4;
  kAudioChannelLayoutTag_ITU_3_0 = kAudioChannelLayoutTag_MPEG_3_0_A;
  kAudioChannelLayoutTag_ITU_3_1 = kAudioChannelLayoutTag_MPEG_4_0_A;
  kAudioChannelLayoutTag_ITU_3_2 = kAudioChannelLayoutTag_MPEG_5_0_A;
  kAudioChannelLayoutTag_ITU_3_2_1 = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_ITU_3_4_1 = kAudioChannelLayoutTag_MPEG_7_1_C;
  kAudioChannelLayoutTag_DVD_0 = kAudioChannelLayoutTag_Mono;
  kAudioChannelLayoutTag_DVD_1 = kAudioChannelLayoutTag_Stereo;
  kAudioChannelLayoutTag_DVD_2 = kAudioChannelLayoutTag_ITU_2_1;
  kAudioChannelLayoutTag_DVD_3 = kAudioChannelLayoutTag_ITU_2_2;
  kAudioChannelLayoutTag_DVD_4 = (133 shl 16) + 3;
  kAudioChannelLayoutTag_DVD_5 = (134 shl 16) + 4;
  kAudioChannelLayoutTag_DVD_6 = (135 shl 16) + 5;
  kAudioChannelLayoutTag_DVD_7 = kAudioChannelLayoutTag_MPEG_3_0_A;
  kAudioChannelLayoutTag_DVD_8 = kAudioChannelLayoutTag_MPEG_4_0_A;
  kAudioChannelLayoutTag_DVD_9 = kAudioChannelLayoutTag_MPEG_5_0_A;
  kAudioChannelLayoutTag_DVD_10 = (136 shl 16) + 4;
  kAudioChannelLayoutTag_DVD_11 = (137 shl 16) + 5;
  kAudioChannelLayoutTag_DVD_12 = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_DVD_13 = kAudioChannelLayoutTag_DVD_8;
  kAudioChannelLayoutTag_DVD_14 = kAudioChannelLayoutTag_DVD_9;
  kAudioChannelLayoutTag_DVD_15 = kAudioChannelLayoutTag_DVD_10;
  kAudioChannelLayoutTag_DVD_16 = kAudioChannelLayoutTag_DVD_11;
  kAudioChannelLayoutTag_DVD_17 = kAudioChannelLayoutTag_DVD_12;
  kAudioChannelLayoutTag_DVD_18 = (138 shl 16) + 5;
  kAudioChannelLayoutTag_DVD_19 = kAudioChannelLayoutTag_MPEG_5_0_B;
  kAudioChannelLayoutTag_DVD_20 = kAudioChannelLayoutTag_MPEG_5_1_B;
  kAudioChannelLayoutTag_AudioUnit_4 = kAudioChannelLayoutTag_Quadraphonic;
  kAudioChannelLayoutTag_AudioUnit_5 = kAudioChannelLayoutTag_Pentagonal;
  kAudioChannelLayoutTag_AudioUnit_6 = kAudioChannelLayoutTag_Hexagonal;
  kAudioChannelLayoutTag_AudioUnit_8 = kAudioChannelLayoutTag_Octagonal;
  kAudioChannelLayoutTag_AudioUnit_5_0 = kAudioChannelLayoutTag_MPEG_5_0_B;
  kAudioChannelLayoutTag_AudioUnit_6_0 = (139 shl 16) + 6;
  kAudioChannelLayoutTag_AudioUnit_7_0 = (140 shl 16) + 7;
  kAudioChannelLayoutTag_AudioUnit_7_0_Front = (148 shl 16) + 7;
  kAudioChannelLayoutTag_AudioUnit_5_1 = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_AudioUnit_6_1 = kAudioChannelLayoutTag_MPEG_6_1_A;
  kAudioChannelLayoutTag_AudioUnit_7_1 = kAudioChannelLayoutTag_MPEG_7_1_C;
  kAudioChannelLayoutTag_AudioUnit_7_1_Front = kAudioChannelLayoutTag_MPEG_7_1_A;
  kAudioChannelLayoutTag_AAC_3_0 = kAudioChannelLayoutTag_MPEG_3_0_B;
  kAudioChannelLayoutTag_AAC_Quadraphonic = kAudioChannelLayoutTag_Quadraphonic;
  kAudioChannelLayoutTag_AAC_4_0 = kAudioChannelLayoutTag_MPEG_4_0_B;
  kAudioChannelLayoutTag_AAC_5_0 = kAudioChannelLayoutTag_MPEG_5_0_D;
  kAudioChannelLayoutTag_AAC_5_1 = kAudioChannelLayoutTag_MPEG_5_1_D;
  kAudioChannelLayoutTag_AAC_6_0 = (141 shl 16) + 6;
  kAudioChannelLayoutTag_AAC_6_1 = (142 shl 16) + 7;
  kAudioChannelLayoutTag_AAC_7_0 = (143 shl 16) + 7;
  kAudioChannelLayoutTag_AAC_7_1 = kAudioChannelLayoutTag_MPEG_7_1_B;
  kAudioChannelLayoutTag_AAC_7_1_B = (183 shl 16) + 8;
  kAudioChannelLayoutTag_AAC_7_1_C = (184 shl 16) + 8;
  kAudioChannelLayoutTag_AAC_Octagonal = (144 shl 16) + 8;
  kAudioChannelLayoutTag_TMH_10_2_std = (145 shl 16) + 16;
  kAudioChannelLayoutTag_TMH_10_2_full = (146 shl 16) + 21;
  kAudioChannelLayoutTag_AC3_1_0_1 = (149 shl 16) + 2;
  kAudioChannelLayoutTag_AC3_3_0 = (150 shl 16) + 3;
  kAudioChannelLayoutTag_AC3_3_1 = (151 shl 16) + 4;
  kAudioChannelLayoutTag_AC3_3_0_1 = (152 shl 16) + 4;
  kAudioChannelLayoutTag_AC3_2_1_1 = (153 shl 16) + 4;
  kAudioChannelLayoutTag_AC3_3_1_1 = (154 shl 16) + 5;
  kAudioChannelLayoutTag_EAC_6_0_A = (155 shl 16) + 6;
  kAudioChannelLayoutTag_EAC_7_0_A = (156 shl 16) + 7;
  kAudioChannelLayoutTag_EAC3_6_1_A = (157 shl 16) + 7;
  kAudioChannelLayoutTag_EAC3_6_1_B = (158 shl 16) + 7;
  kAudioChannelLayoutTag_EAC3_6_1_C = (159 shl 16) + 7;
  kAudioChannelLayoutTag_EAC3_7_1_A = (160 shl 16) + 8;
  kAudioChannelLayoutTag_EAC3_7_1_B = (161 shl 16) + 8;
  kAudioChannelLayoutTag_EAC3_7_1_C = (162 shl 16) + 8;
  kAudioChannelLayoutTag_EAC3_7_1_D = (163 shl 16) + 8;
  kAudioChannelLayoutTag_EAC3_7_1_E = (164 shl 16) + 8;
  kAudioChannelLayoutTag_EAC3_7_1_F = (165 shl 16) + 8;
  kAudioChannelLayoutTag_EAC3_7_1_G = (166 shl 16) + 8;
  kAudioChannelLayoutTag_EAC3_7_1_H = (167 shl 16) + 8;
  kAudioChannelLayoutTag_DTS_3_1 = (168 shl 16) + 4;
  kAudioChannelLayoutTag_DTS_4_1 = (169 shl 16) + 5;
  kAudioChannelLayoutTag_DTS_6_0_A = (170 shl 16) + 6;
  kAudioChannelLayoutTag_DTS_6_0_B = (171 shl 16) + 6;
  kAudioChannelLayoutTag_DTS_6_0_C = (172 shl 16) + 6;
  kAudioChannelLayoutTag_DTS_6_1_A = (173 shl 16) + 7;
  kAudioChannelLayoutTag_DTS_6_1_B = (174 shl 16) + 7;
  kAudioChannelLayoutTag_DTS_6_1_C = (175 shl 16) + 7;
  kAudioChannelLayoutTag_DTS_7_0 = (176 shl 16) + 7;
  kAudioChannelLayoutTag_DTS_7_1 = (177 shl 16) + 8;
  kAudioChannelLayoutTag_DTS_8_0_A = (178 shl 16) + 8;
  kAudioChannelLayoutTag_DTS_8_0_B = (179 shl 16) + 8;
  kAudioChannelLayoutTag_DTS_8_1_A = (180 shl 16) + 9;
  kAudioChannelLayoutTag_DTS_8_1_B = (181 shl 16) + 9;
  kAudioChannelLayoutTag_DTS_6_1_D = (182 shl 16) + 7;
  kAudioChannelLayoutTag_DiscreteInOrder = (147 shl 16) + 0;
  kAudioChannelLayoutTag_Unknown = $FFFF0000;
const
  kMPEG4Object_AAC_Main = 1;
  kMPEG4Object_AAC_LC = 2;
  kMPEG4Object_AAC_SSR = 3;
  kMPEG4Object_AAC_LTP = 4;
  kMPEG4Object_AAC_SBR = 5;
  kMPEG4Object_AAC_Scalable = 6;
  kMPEG4Object_TwinVQ = 7;
  kMPEG4Object_CELP = 8;
  kMPEG4Object_HVXC = 9;
{$endif}

