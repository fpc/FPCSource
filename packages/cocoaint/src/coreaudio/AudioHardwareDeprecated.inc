{ Parsed from CoreAudio.framework AudioHardwareDeprecated.h }

{$if  not (defined(__AudioHardwareDeprecated_h__))}

{$ifdef TYPES}
const
  kAudioDevicePropertyScopeInput = kAudioObjectPropertyScopeInput;
  kAudioDevicePropertyScopeOutput = kAudioObjectPropertyScopeOutput;
  kAudioDevicePropertyScopePlayThrough = kAudioObjectPropertyScopePlayThrough;
const
  kAudioPropertyWildcardPropertyID = kAudioObjectPropertySelectorWildcard;
  kAudioPropertyWildcardSection = $FF;
  kAudioPropertyWildcardChannel = kAudioObjectPropertyElementWildcard;
const
  kAudioISubOwnerControlClassID = 'atch';
const
  kAudioLevelControlPropertyDecibelsToScalarTransferFunction = 'lctf';
const
  kAudioLevelControlTranferFunctionLinear = 0;
  kAudioLevelControlTranferFunction1Over3 = 1;
  kAudioLevelControlTranferFunction1Over2 = 2;
  kAudioLevelControlTranferFunction3Over4 = 3;
  kAudioLevelControlTranferFunction3Over2 = 4;
  kAudioLevelControlTranferFunction2Over1 = 5;
  kAudioLevelControlTranferFunction3Over1 = 6;
  kAudioLevelControlTranferFunction4Over1 = 7;
  kAudioLevelControlTranferFunction5Over1 = 8;
  kAudioLevelControlTranferFunction6Over1 = 9;
  kAudioLevelControlTranferFunction7Over1 = 10;
  kAudioLevelControlTranferFunction8Over1 = 11;
  kAudioLevelControlTranferFunction9Over1 = 12;
  kAudioLevelControlTranferFunction10Over1 = 13;
  kAudioLevelControlTranferFunction11Over1 = 14;
  kAudioLevelControlTranferFunction12Over1 = 15;

type
  AudioHardwarePropertyID = AudioObjectPropertySelector;
  AudioHardwarePropertyIDPtr = ^AudioHardwarePropertyID;
  AudioHardwarePropertyListenerProc = function (inPropertyID: AudioHardwarePropertyID; inClientData: pointer): OSStatus; cdecl;

const
  kAudioHardwareRunLoopMode = 'com.apple.audio.CoreAudio';

const
  kAudioHardwarePropertyRunLoop = 'rnlp';
  kAudioHardwarePropertyDeviceForUID = 'duid';
  kAudioHardwarePropertyPlugInForBundleID = 'pibi';
const
  kAudioHardwarePropertyBootChimeVolumeScalar = 'bbvs';
  kAudioHardwarePropertyBootChimeVolumeDecibels = 'bbvd';
  kAudioHardwarePropertyBootChimeVolumeRangeDecibels = 'bbd#';
  kAudioHardwarePropertyBootChimeVolumeScalarToDecibels = 'bv2d';
  kAudioHardwarePropertyBootChimeVolumeDecibelsToScalar = 'bd2v';
  kAudioHardwarePropertyBootChimeVolumeDecibelsToScalarTransferFunction = 'bvtf';
{$endif}

{$ifdef FUNCTIONS}
function AudioHardwareAddRunLoopSource(inRunLoopSource: CFRunLoopSourceRef): OSStatus; cdecl; external; deprecated 'in __MAC_10_3, __MAC_10_7, __IPHONE_2_0, __IPHONE_4_1';
function AudioHardwareRemoveRunLoopSource(inRunLoopSource: CFRunLoopSourceRef): OSStatus; cdecl; external; deprecated 'in __MAC_10_3, __MAC_10_7, __IPHONE_2_0, __IPHONE_4_1';
function AudioHardwareGetPropertyInfo(inPropertyID: AudioHardwarePropertyID; outSize: UInt32Ptr; outWritable: pObjCBOOL): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioHardwareGetProperty(inPropertyID: AudioHardwarePropertyID; var ioPropertyDataSize: UInt32; outPropertyData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioHardwareSetProperty(inPropertyID: AudioHardwarePropertyID; inPropertyDataSize: UInt32; inPropertyData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioHardwareAddPropertyListener(inPropertyID: AudioHardwarePropertyID; inProc: AudioHardwarePropertyListenerProc; inClientData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioHardwareRemovePropertyListener(inPropertyID: AudioHardwarePropertyID; inProc: AudioHardwarePropertyListenerProc): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
{$endif}

{$ifdef TYPES}
type
  AudioDeviceID = AudioObjectID;
  AudioDeviceIDPtr = ^AudioDeviceID;
  AudioDevicePropertyID = AudioObjectPropertySelector;
  AudioDevicePropertyIDPtr = ^AudioDevicePropertyID;
  AudioDevicePropertyListenerProc = function (inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inClientData: pointer): OSStatus; cdecl;

const
  kAudioDeviceUnknown = kAudioObjectUnknown;
const
  kAudioDeviceTransportTypeAutoAggregate = 'fgrp';
const
  kAudioDevicePropertyVolumeDecibelsToScalarTransferFunction = 'vctf';
  kAudioDevicePropertyPlayThruVolumeDecibelsToScalarTransferFunction = 'mvtf';
  kAudioDevicePropertyDriverShouldOwniSub = 'isub';
  kAudioDevicePropertySubVolumeDecibelsToScalarTransferFunction = 'svtf';
const
  kAudioDevicePropertyDeviceName = 'name';
  kAudioDevicePropertyDeviceNameCFString = kAudioObjectPropertyName;
  kAudioDevicePropertyDeviceManufacturer = 'makr';
  kAudioDevicePropertyDeviceManufacturerCFString = kAudioObjectPropertyManufacturer;
  kAudioDevicePropertyRegisterBufferList = 'rbuf';
  kAudioDevicePropertyBufferSize = 'bsiz';
  kAudioDevicePropertyBufferSizeRange = 'bsz#';
  kAudioDevicePropertyChannelName = 'chnm';
  kAudioDevicePropertyChannelNameCFString = kAudioObjectPropertyElementName;
  kAudioDevicePropertyChannelCategoryName = 'ccnm';
  kAudioDevicePropertyChannelCategoryNameCFString = kAudioObjectPropertyElementCategoryName;
  kAudioDevicePropertyChannelNumberName = 'cnnm';
  kAudioDevicePropertyChannelNumberNameCFString = kAudioObjectPropertyElementNumberName;
  kAudioDevicePropertySupportsMixing = 'mix?';
  kAudioDevicePropertyStreamFormat = 'sfmt';
  kAudioDevicePropertyStreamFormats = 'sfm#';
  kAudioDevicePropertyStreamFormatSupported = 'sfm?';
  kAudioDevicePropertyStreamFormatMatch = 'sfmm';
  kAudioDevicePropertyDataSourceNameForID = 'sscn';
  kAudioDevicePropertyClockSourceNameForID = 'cscn';
  kAudioDevicePropertyPlayThruDestinationNameForID = 'mddn';
  kAudioDevicePropertyChannelNominalLineLevelNameForID = 'cnlv';
  kAudioDevicePropertyHighPassFilterSettingNameForID = 'chip';
{$endif}

{$ifdef FUNCTIONS}
function AudioDeviceAddIOProc(inDevice: AudioDeviceID; inProc: AudioDeviceIOProc; inClientData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_5, __IPHONE_2_0, __IPHONE_2_0';
function AudioDeviceRemoveIOProc(inDevice: AudioDeviceID; inProc: AudioDeviceIOProc): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_5, __IPHONE_2_0, __IPHONE_2_0';
function AudioDeviceRead(inDevice: AudioDeviceID; inStartTime: AudioTimeStampPtr; var outData: AudioBufferList): OSStatus; cdecl; external; deprecated 'in __MAC_10_1, __MAC_10_5, __IPHONE_2_0, __IPHONE_2_0';
function AudioDeviceGetPropertyInfo(inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; outSize: UInt32Ptr; outWritable: pObjCBOOL): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioDeviceGetProperty(inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; var ioPropertyDataSize: UInt32; outPropertyData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioDeviceSetProperty(inDevice: AudioDeviceID; inWhen: AudioTimeStampPtr; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inPropertyDataSize: UInt32; inPropertyData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioDeviceAddPropertyListener(inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inProc: AudioDevicePropertyListenerProc; inClientData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioDeviceRemovePropertyListener(inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inProc: AudioDevicePropertyListenerProc): OSStatus; cdecl; external; deprecated 'in __MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
{$endif}

{$ifdef TYPES}
type
  AudioStreamID = AudioObjectID;
  AudioStreamIDPtr = ^AudioStreamID;
  AudioStreamPropertyListenerProc = function (inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inClientData: pointer): OSStatus; cdecl;

const
  kAudioStreamUnknown = kAudioObjectUnknown;
const
  kAudioStreamPropertyOwningDevice = kAudioObjectPropertyOwner;
  kAudioStreamPropertyPhysicalFormats = 'pft#';
  kAudioStreamPropertyPhysicalFormatSupported = 'pft?';
  kAudioStreamPropertyPhysicalFormatMatch = 'pftm';
{$endif}

{$ifdef FUNCTIONS}
function AudioStreamGetPropertyInfo(inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; var outSize: UInt32; var outWritable: pObjCBOOL): OSStatus; cdecl; external; deprecated 'in __MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioStreamGetProperty(inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; var ioPropertyDataSize: UInt32; outPropertyData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioStreamSetProperty(inStream: AudioStreamID; inWhen: AudioTimeStampPtr; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inPropertyDataSize: UInt32; inPropertyData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioStreamAddPropertyListener(inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inProc: AudioStreamPropertyListenerProc; inClientData: pointer): OSStatus; cdecl; external; deprecated 'in __MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
function AudioStreamRemovePropertyListener(inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inProc: AudioStreamPropertyListenerProc): OSStatus; cdecl; external; deprecated 'in __MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0';
{$endif}

{$ifdef TYPES}
const
  kAudioBootChimeVolumeControlClassID = 'pram';
const
  kAudioControlPropertyVariant = 'cvar';
const
  kAudioClockSourceControlPropertyItemKind = kAudioSelectorControlPropertyItemKind;
{$endif}

{$endif}
