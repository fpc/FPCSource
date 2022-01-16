{ Parsed from CoreMediaIO.framework CMIOHardwareDevice.h }

{$if  not (defined(__CMIOHardwareDevice_h__))}

{$ifdef TYPES}
type
  CMIODeviceID = CMIOObjectID;
  CMIODeviceIDPtr = ^CMIODeviceID;
  CMIODevicePropertyID = CMIOObjectPropertySelector;
  CMIODevicePropertyIDPtr = ^CMIODevicePropertyID;

type
  CMIODeviceStreamConfiguration = record
    mNumberStreams: UInt32;
    mNumberChannels: UInt32;
  end;
type
  CMIODeviceStreamConfigurationPtr = ^CMIODeviceStreamConfiguration;
type
  CMIODeviceAVCCommand = record
    mCommand: UInt8Ptr;
    mCommandLength: UInt32;
    mResponse: UInt8Ptr;
    mResponseLength: UInt32;
    mResponseUsed: UInt32;
  end;
type
  CMIODeviceAVCCommandPtr = ^CMIODeviceAVCCommand;
type
  CMIODeviceRS422Command = record
    mCommand: UInt8Ptr;
    mCommandLength: UInt32;
    mResponse: UInt8Ptr;
    mResponseLength: UInt32;
    mResponseUsed: UInt32;
  end;
type
  CMIODeviceRS422CommandPtr = ^CMIODeviceRS422Command;

type
  CMIODeviceGetSMPTETimeProc = function (refCon: pointer; frameNumber: UInt64Ptr; isDropFrame: pObjCBOOL; tolerance: UInt32Ptr): OSStatus; cdecl;

type
  CMIODeviceSMPTETimeCallback = record
    mGetSMPTETimeProc: CMIODeviceGetSMPTETimeProc;
    mRefCon: pointer;
  end;
type
  CMIODeviceSMPTETimeCallbackPtr = ^CMIODeviceSMPTETimeCallback;

const
  kCMIODevicePropertyScopeInput = 'inpt';
  kCMIODevicePropertyScopeOutput = 'outp';
  kCMIODevicePropertyScopePlayThrough = 'ptru';
  kCMIODeviceClassID = 'adev';
  kCMIODeviceUnknown = kCMIOObjectUnknown;
const
  kCMIOAVCDeviceType_Unknown = 'unkn';
  kCMIOAVCDeviceType_DV_NTSC = 'dvc ';
  kCMIOAVCDeviceType_DV_PAL = 'dvcp';
  kCMIOAVCDeviceType_DVCPro_NTSC = 'dvpn';
  kCMIOAVCDeviceType_DVCPro_PAL = 'dvpp';
  kCMIOAVCDeviceType_DVCPro50_NTSC = 'dv5n';
  kCMIOAVCDeviceType_DVCPro50_PAL = 'dv5p';
  kCMIOAVCDeviceType_DVCPro100_NTSC = 'dv1n';
  kCMIOAVCDeviceType_DVCPro100_PAL = 'dv1p';
  kCMIOAVCDeviceType_DVCPro100_720p = 'dvhp';
  kCMIOAVCDeviceType_DVCProHD_1080i50 = 'dvh5';
  kCMIOAVCDeviceType_DVCProHD_1080i60 = 'dvh6';
  kCMIOAVCDeviceType_MPEG2 = 'mpg2';
const
  kCMIODeviceAVCSignalModeSD525_60 = $00;
  kCMIODeviceAVCSignalModeSDL525_60 = $04;
  kCMIODeviceAVCSignalModeHD1125_60 = $08;
  kCMIODeviceAVCSignalModeSD625_50 = $80;
  kCMIODeviceAVCSignalModeSDL625_50 = $84;
  kCMIODeviceAVCSignalModeHD1250_50 = $88;
  kCMIODeviceAVCSignalModeMPEG25Mbps_60 = $10;
  kCMIODeviceAVCSignalModeHDV1_60 = $10;
  kCMIODeviceAVCSignalModeMPEG12Mbps_60 = $14;
  kCMIODeviceAVCSignalModeMPEG6Mbps_60 = $18;
  kCMIODeviceAVCSignalModeMPEG25Mbps_50 = $90;
  kCMIODeviceAVCSignalModeHDV1_50 = $90;
  kCMIODeviceAVCSignalModeMPEG12Mbps_50 = $94;
  kCMIODeviceAVCSignalModeMPEG6Mbps_50 = $98;
  kCMIODeviceAVCSignalModeDVHS = $01;
  kCMIODeviceAVCSignalModeVHSNTSC = $05;
  kCMIODeviceAVCSignalModeVHSMPAL = $25;
  kCMIODeviceAVCSignalModeVHSPAL = $a5;
  kCMIODeviceAVCSignalModeVHSNPAL = $b5;
  kCMIODeviceAVCSignalModeVHSSECAM = $c5;
  kCMIODeviceAVCSignalModeVHSMESECAM = $d5;
  kCMIODeviceAVCSignalModeSVHS525_60 = $0d;
  kCMIODeviceAVCSignalModeSVHS625_50 = $ed;
  kCMIODeviceAVCSignalMode8mmNTSC = $06;
  kCMIODeviceAVCSignalMode8mmPAL = $86;
  kCMIODeviceAVCSignalModeHi8NTSC = $0e;
  kCMIODeviceAVCSignalModeHi8PAL = $8e;
  kCMIODeviceAVCSignalModeMicroMV12Mbps_60 = $24;
  kCMIODeviceAVCSignalModeMicroMV6Mbps_60 = $28;
  kCMIODeviceAVCSignalModeMicroMV12Mbps_50 = $A4;
  kCMIODeviceAVCSignalModeMicroMV6Mbps_50 = $A8;
  kCMIODeviceAVCSignalModeAudio = $20;
  kCMIODeviceAVCSignalModeHDV2_60 = $1A;
  kCMIODeviceAVCSignalModeHDV2_50 = $9A;
  kCMIODeviceAVCSignalModeDVCPro25_625_50 = $F8;
  kCMIODeviceAVCSignalModeDVCPro25_525_60 = $78;
  kCMIODeviceAVCSignalModeDVCPro50_625_50 = $F4;
  kCMIODeviceAVCSignalModeDVCPro50_525_60 = $74;
  kCMIODeviceAVCSignalModeDVCPro100_50 = $F0;
  kCMIODeviceAVCSignalModeDVCPro100_60 = $70;
const
  kCMIODevicePropertyPlugIn = 'plug';
  kCMIODevicePropertyDeviceUID = 'uid ';
  kCMIODevicePropertyModelUID = 'muid';
  kCMIODevicePropertyTransportType = 'tran';
  kCMIODevicePropertyDeviceIsAlive = 'livn';
  kCMIODevicePropertyDeviceHasChanged = 'diff';
  kCMIODevicePropertyDeviceIsRunning = 'goin';
  kCMIODevicePropertyDeviceIsRunningSomewhere = 'gone';
  kCMIODevicePropertyDeviceCanBeDefaultDevice = 'dflt';
  kCMIODevicePropertyHogMode = 'oink';
  kCMIODevicePropertyLatency = 'ltnc';
  kCMIODevicePropertyStreams = 'stm#';
  kCMIODevicePropertyStreamConfiguration = 'slay';
  kCMIODevicePropertyDeviceMaster = 'pmnh';
  kCMIODevicePropertyExcludeNonDALAccess = 'ixna';
  kCMIODevicePropertyClientSyncDiscontinuity = 'pmcs';
  kCMIODevicePropertySMPTETimeCallback = 'pmsc';
  kCMIODevicePropertyCanProcessAVCCommand = 'pmac';
  kCMIODevicePropertyAVCDeviceType = 'pmat';
  kCMIODevicePropertyAVCDeviceSignalMode = 'pmsm';
  kCMIODevicePropertyCanProcessRS422Command = 'r422';
  kCMIODevicePropertyLinkedCoreAudioDeviceUID = 'plud';
  kCMIODevicePropertyVideoDigitizerComponents = 'vdig';
  kCMIODevicePropertySuspendedByUser = 'sbyu';
  kCMIODevicePropertyLinkedAndSyncedCoreAudioDeviceUID = 'plsd';
  kCMIODevicePropertyIIDCInitialUnitSpace = 'iuns';
  kCMIODevicePropertyIIDCCSRData = 'csrd';
  kCMIODevicePropertyCanSwitchFrameRatesWithoutFrameDrops = 'frnd';
{$endif}

{$ifdef FUNCTIONS}
function CMIODeviceStartStream(deviceID: CMIODeviceID; streamID: CMIOStreamID): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIODeviceStopStream(deviceID: CMIODeviceID; streamID: CMIOStreamID): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIODeviceProcessAVCCommand(deviceID: CMIODeviceID; ioAVCCommand: CMIODeviceAVCCommandPtr): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIODeviceProcessRS422Command(deviceID: CMIODeviceID; ioRS422Command: CMIODeviceRS422CommandPtr): OSStatus; cdecl; external; { available in 10.7 and later }
{$endif}

{$endif}
