{ Parsed from CoreAudio.framework AudioHardware.h }

{$if  not (defined(__AudioHardware_h__))}

{$ifdef TYPES}
const
  kAudioObjectSystemObject = 1;

type
  AudioObjectPropertyListenerProc = function (inObjectID: AudioObjectID; inNumberAddresses: UInt32; inAddresses: AudioObjectPropertyAddressPtr { variable size array of AudioObjectPropertyAddress }; inClientData: pointer): OSStatus; cdecl;
{$endif}

{$if (defined(__BLOCKS__))}
{$ifdef TYPES}
type
  AudioObjectPropertyListenerBlock = OpaqueCBlock;
{$endif}

{$endif}
{$ifdef TYPES}
const
  kAudioObjectPropertyCreator = 'oplg';
  kAudioObjectPropertyListenerAdded = 'lisa';
  kAudioObjectPropertyListenerRemoved = 'lisr';
{$endif}

{$ifdef FUNCTIONS}
procedure AudioObjectShow(inObjectID: AudioObjectID); cdecl; external; { available starting in __MAC_10_4, __IPHONE_2_0 }
function AudioObjectHasProperty(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr): Boolean; cdecl; external; { available starting in __MAC_10_4, __IPHONE_2_0 }
function AudioObjectIsPropertySettable(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr; var outIsSettable: pObjCBOOL): OSStatus; cdecl; external; { available starting in __MAC_10_4, __IPHONE_2_0 }
function AudioObjectGetPropertyDataSize(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr; inQualifierDataSize: UInt32; inQualifierData: pointer; var outDataSize: UInt32): OSStatus; cdecl; external; { available starting in __MAC_10_4, __IPHONE_2_0 }
function AudioObjectGetPropertyData(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr; inQualifierDataSize: UInt32; inQualifierData: pointer; var ioDataSize: UInt32; outData: pointer): OSStatus; cdecl; external; { available starting in __MAC_10_4, __IPHONE_2_0 }
function AudioObjectSetPropertyData(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr; inQualifierDataSize: UInt32; inQualifierData: pointer; inDataSize: UInt32; inData: pointer): OSStatus; cdecl; external; { available starting in __MAC_10_4, __IPHONE_2_0 }
function AudioObjectAddPropertyListener(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr; inListener: AudioObjectPropertyListenerProc; inClientData: pointer): OSStatus; cdecl; external; { available starting in __MAC_10_4, __IPHONE_2_0 }
function AudioObjectRemovePropertyListener(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr; inListener: AudioObjectPropertyListenerProc; inClientData: pointer): OSStatus; cdecl; external; { available starting in __MAC_10_4, __IPHONE_2_0 }
{$endif}

{$if (defined(__BLOCKS__))}
{$ifdef FUNCTIONS}
function AudioObjectAddPropertyListenerBlock(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr; inDispatchQueue: dispatch_queue_t; inListener: AudioObjectPropertyListenerBlock): OSStatus; cdecl; external; { available starting in __MAC_10_7, __IPHONE_7_0 }
function AudioObjectRemovePropertyListenerBlock(inObjectID: AudioObjectID; inAddress: AudioObjectPropertyAddressPtr; inDispatchQueue: dispatch_queue_t; inListener: AudioObjectPropertyListenerBlock): OSStatus; cdecl; external; { available starting in __MAC_10_7, __IPHONE_7_0 }
{$endif}

{$endif}
{$ifdef TYPES}
const
  kAudioSystemObjectClassID = 'asys';
const
  kAudioHardwarePowerHintNone = 0;
  kAudioHardwarePowerHintFavorSavingPower = 1;
const
  kAudioHardwarePropertyDevices = 'dev#';
  kAudioHardwarePropertyDefaultInputDevice = 'dIn ';
  kAudioHardwarePropertyDefaultOutputDevice = 'dOut';
  kAudioHardwarePropertyDefaultSystemOutputDevice = 'sOut';
  kAudioHardwarePropertyTranslateUIDToDevice = 'uidd';
  kAudioHardwarePropertyMixStereoToMono = 'stmo';
  kAudioHardwarePropertyPlugInList = 'plg#';
  kAudioHardwarePropertyTranslateBundleIDToPlugIn = 'bidp';
  kAudioHardwarePropertyTransportManagerList = 'tmg#';
  kAudioHardwarePropertyTranslateBundleIDToTransportManager = 'tmbi';
  kAudioHardwarePropertyBoxList = 'box#';
  kAudioHardwarePropertyTranslateUIDToBox = 'uidb';
  kAudioHardwarePropertyProcessIsMaster = 'mast';
  kAudioHardwarePropertyIsInitingOrExiting = 'inot';
  kAudioHardwarePropertyUserIDChanged = 'euid';
  kAudioHardwarePropertyProcessIsAudible = 'pmut';
  kAudioHardwarePropertySleepingIsAllowed = 'slep';
  kAudioHardwarePropertyUnloadingIsAllowed = 'unld';
  kAudioHardwarePropertyHogModeIsAllowed = 'hogr';
  kAudioHardwarePropertyUserSessionIsActiveOrHeadless = 'user';
  kAudioHardwarePropertyServiceRestarted = 'srst';
  kAudioHardwarePropertyPowerHint = 'powh';
{$endif}

{$ifdef FUNCTIONS}
function AudioHardwareCreateAggregateDevice(inDescription: CFDictionaryRef; outDeviceID: AudioObjectIDPtr): OSStatus; cdecl; external; { available starting in __MAC_10_9, __IPHONE_7_0 }
function AudioHardwareDestroyAggregateDevice(inDeviceID: AudioObjectID): OSStatus; cdecl; external; { available starting in __MAC_10_9, __IPHONE_7_0 }
{$endif}

{$ifdef TYPES}
const
  kAudioPlugInCreateAggregateDevice = 'cagg';
  kAudioPlugInDestroyAggregateDevice = 'dagg';
const
  kAudioTransportManagerCreateEndPointDevice = 'cdev';
  kAudioTransportManagerDestroyEndPointDevice = 'ddev';

type
  AudioDeviceIOProc = function (inDevice: AudioObjectID; inNow: AudioTimeStampPtr; inInputData: AudioBufferListPtr; inInputTime: AudioTimeStampPtr; var outOutputData: AudioBufferList; inOutputTime: AudioTimeStampPtr; inClientData: pointer): OSStatus; cdecl;
{$endif}

{$if (defined(__BLOCKS__))}
{$ifdef TYPES}
type
  AudioDeviceIOBlock = OpaqueCBlock;
{$endif}

{$endif}
{$ifdef TYPES}
type
  AudioDeviceIOProcID = AudioDeviceIOProc;
  AudioDeviceIOProcIDPtr = ^AudioDeviceIOProcID;

type
  AudioHardwareIOProcStreamUsage = record
    mIOProc: pointer;
    mNumberStreams: UInt32;
    mStreamIsOn:array[0..0] of UInt32;
  end;
type
  AudioHardwareIOProcStreamUsagePtr = ^AudioHardwareIOProcStreamUsage;

const
  kAudioDeviceStartTimeIsInputFlag = 1 shl 0;
  kAudioDeviceStartTimeDontConsultDeviceFlag = 1 shl 1;
  kAudioDeviceStartTimeDontConsultHALFlag = 1 shl 2;
const
  kAudioDevicePropertyPlugIn = 'plug';
  kAudioDevicePropertyDeviceHasChanged = 'diff';
  kAudioDevicePropertyDeviceIsRunningSomewhere = 'gone';
  kAudioDeviceProcessorOverload = 'over';
  kAudioDevicePropertyIOStoppedAbnormally = 'stpd';
  kAudioDevicePropertyHogMode = 'oink';
  kAudioDevicePropertyBufferFrameSize = 'fsiz';
  kAudioDevicePropertyBufferFrameSizeRange = 'fsz#';
  kAudioDevicePropertyUsesVariableBufferFrameSizes = 'vfsz';
  kAudioDevicePropertyIOCycleUsage = 'ncyc';
  kAudioDevicePropertyStreamConfiguration = 'slay';
  kAudioDevicePropertyIOProcStreamUsage = 'suse';
  kAudioDevicePropertyActualSampleRate = 'asrt';
const
  kAudioDevicePropertyJackIsConnected = 'jack';
  kAudioDevicePropertyVolumeScalar = 'volm';
  kAudioDevicePropertyVolumeDecibels = 'vold';
  kAudioDevicePropertyVolumeRangeDecibels = 'vdb#';
  kAudioDevicePropertyVolumeScalarToDecibels = 'v2db';
  kAudioDevicePropertyVolumeDecibelsToScalar = 'db2v';
  kAudioDevicePropertyStereoPan = 'span';
  kAudioDevicePropertyStereoPanChannels = 'spn#';
  kAudioDevicePropertyMute = 'mute';
  kAudioDevicePropertySolo = 'solo';
  kAudioDevicePropertyPhantomPower = 'phan';
  kAudioDevicePropertyPhaseInvert = 'phsi';
  kAudioDevicePropertyClipLight = 'clip';
  kAudioDevicePropertyTalkback = 'talb';
  kAudioDevicePropertyListenback = 'lsnb';
  kAudioDevicePropertyDataSource = 'ssrc';
  kAudioDevicePropertyDataSources = 'ssc#';
  kAudioDevicePropertyDataSourceNameForIDCFString = 'lscn';
  kAudioDevicePropertyDataSourceKindForID = 'ssck';
  kAudioDevicePropertyClockSource = 'csrc';
  kAudioDevicePropertyClockSources = 'csc#';
  kAudioDevicePropertyClockSourceNameForIDCFString = 'lcsn';
  kAudioDevicePropertyClockSourceKindForID = 'csck';
  kAudioDevicePropertyPlayThru = 'thru';
  kAudioDevicePropertyPlayThruSolo = 'thrs';
  kAudioDevicePropertyPlayThruVolumeScalar = 'mvsc';
  kAudioDevicePropertyPlayThruVolumeDecibels = 'mvdb';
  kAudioDevicePropertyPlayThruVolumeRangeDecibels = 'mvd#';
  kAudioDevicePropertyPlayThruVolumeScalarToDecibels = 'mv2d';
  kAudioDevicePropertyPlayThruVolumeDecibelsToScalar = 'mv2s';
  kAudioDevicePropertyPlayThruStereoPan = 'mspn';
  kAudioDevicePropertyPlayThruStereoPanChannels = 'msp#';
  kAudioDevicePropertyPlayThruDestination = 'mdds';
  kAudioDevicePropertyPlayThruDestinations = 'mdd#';
  kAudioDevicePropertyPlayThruDestinationNameForIDCFString = 'mddc';
  kAudioDevicePropertyChannelNominalLineLevel = 'nlvl';
  kAudioDevicePropertyChannelNominalLineLevels = 'nlv#';
  kAudioDevicePropertyChannelNominalLineLevelNameForIDCFString = 'lcnl';
  kAudioDevicePropertyHighPassFilterSetting = 'hipf';
  kAudioDevicePropertyHighPassFilterSettings = 'hip#';
  kAudioDevicePropertyHighPassFilterSettingNameForIDCFString = 'hipl';
  kAudioDevicePropertySubVolumeScalar = 'svlm';
  kAudioDevicePropertySubVolumeDecibels = 'svld';
  kAudioDevicePropertySubVolumeRangeDecibels = 'svd#';
  kAudioDevicePropertySubVolumeScalarToDecibels = 'sv2d';
  kAudioDevicePropertySubVolumeDecibelsToScalar = 'sd2v';
  kAudioDevicePropertySubMute = 'smut';
{$endif}

{$ifdef FUNCTIONS}
function AudioDeviceCreateIOProcID(inDevice: AudioObjectID; inProc: AudioDeviceIOProc; inClientData: pointer; var outIOProcID: AudioDeviceIOProcID): OSStatus; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
{$endif}

{$if (defined(__BLOCKS__))}
{$ifdef FUNCTIONS}
function AudioDeviceCreateIOProcIDWithBlock(outIOProcID: AudioDeviceIOProcIDPtr; inDevice: AudioObjectID; inDispatchQueue: dispatch_queue_t; inIOBlock: AudioDeviceIOBlock): OSStatus; cdecl; external; { available starting in __MAC_10_7, __IPHONE_7_0 }
{$endif}

{$endif}
{$ifdef FUNCTIONS}
function AudioDeviceDestroyIOProcID(inDevice: AudioObjectID; inIOProcID: AudioDeviceIOProcID): OSStatus; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function AudioDeviceStart(inDevice: AudioObjectID; inProcID: AudioDeviceIOProcID): OSStatus; cdecl; external; { available starting in __MAC_10_0, __IPHONE_2_0 }
function AudioDeviceStartAtTime(inDevice: AudioObjectID; inProcID: AudioDeviceIOProcID; ioRequestedStartTime: AudioTimeStampPtr; inFlags: UInt32): OSStatus; cdecl; external; { available starting in __MAC_10_3, __IPHONE_2_0 }
function AudioDeviceStop(inDevice: AudioObjectID; inProcID: AudioDeviceIOProcID): OSStatus; cdecl; external; { available starting in __MAC_10_0, __IPHONE_2_0 }
function AudioDeviceGetCurrentTime(inDevice: AudioObjectID; var outTime: AudioTimeStamp): OSStatus; cdecl; external; { available starting in __MAC_10_0, __IPHONE_2_0 }
function AudioDeviceTranslateTime(inDevice: AudioObjectID; inTime: AudioTimeStampPtr; var outTime: AudioTimeStamp): OSStatus; cdecl; external; { available starting in __MAC_10_0, __IPHONE_2_0 }
function AudioDeviceGetNearestStartTime(inDevice: AudioObjectID; var ioRequestedStartTime: AudioTimeStamp; inFlags: UInt32): OSStatus; cdecl; external; { available starting in __MAC_10_3, __IPHONE_2_0 }
{$endif}

{$ifdef TYPES}
const
  kAudioAggregateDeviceClassID = 'aagg';

const
  kAudioAggregateDeviceUIDKey = 'uid';
  kAudioAggregateDeviceNameKey = 'name';
  kAudioAggregateDeviceSubDeviceListKey = 'subdevices';
  kAudioAggregateDeviceMasterSubDeviceKey = 'master';
  kAudioAggregateDeviceIsPrivateKey = 'private';
  kAudioAggregateDeviceIsStackedKey = 'stacked';

const
  kAudioAggregateDevicePropertyFullSubDeviceList = 'grup';
  kAudioAggregateDevicePropertyActiveSubDeviceList = 'agrp';
  kAudioAggregateDevicePropertyComposition = 'acom';
  kAudioAggregateDevicePropertyMasterSubDevice = 'amst';
const
  kAudioSubDeviceClassID = 'asub';
const
  kAudioSubDeviceDriftCompensationMinQuality = 0;
  kAudioSubDeviceDriftCompensationLowQuality = $20;
  kAudioSubDeviceDriftCompensationMediumQuality = $40;
  kAudioSubDeviceDriftCompensationHighQuality = $60;
  kAudioSubDeviceDriftCompensationMaxQuality = $7F;

const
  kAudioSubDeviceUIDKey = 'uid';
  kAudioSubDeviceNameKey = 'name';
  kAudioSubDeviceInputChannelsKey = 'channels-in';
  kAudioSubDeviceOutputChannelsKey = 'channels-out';
  kAudioSubDeviceExtraInputLatencyKey = 'latency-in';
  kAudioSubDeviceExtraOutputLatencyKey = 'latency-out';
  kAudioSubDeviceDriftCompensationKey = 'drift';
  kAudioSubDeviceDriftCompensationQualityKey = 'drift quality';

const
  kAudioSubDevicePropertyExtraLatency = 'xltc';
  kAudioSubDevicePropertyDriftCompensation = 'drft';
  kAudioSubDevicePropertyDriftCompensationQuality = 'drfq';
{$endif}

{$endif}
