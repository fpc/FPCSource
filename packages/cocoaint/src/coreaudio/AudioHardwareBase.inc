{ Parsed from CoreAudio.framework AudioHardwareBase.h }

{$if  not (defined(__AudioHardwareBase_h__))}

{$ifdef TYPES}
type
  AudioObjectID = UInt32;
  AudioObjectIDPtr = ^AudioObjectID;
  AudioClassID = UInt32;
  AudioClassIDPtr = ^AudioClassID;
  AudioObjectPropertySelector = UInt32;
  AudioObjectPropertySelectorPtr = ^AudioObjectPropertySelector;
  AudioObjectPropertyScope = UInt32;
  AudioObjectPropertyScopePtr = ^AudioObjectPropertyScope;
  AudioObjectPropertyElement = UInt32;
  AudioObjectPropertyElementPtr = ^AudioObjectPropertyElement;

type
  AudioObjectPropertyAddress = record
    mSelector: AudioObjectPropertySelector;
    mScope: AudioObjectPropertyScope;
    mElement: AudioObjectPropertyElement;
  end;
type
  AudioObjectPropertyAddressPtr = ^AudioObjectPropertyAddress;

const
  kAudioHardwareNoError = 0;
  kAudioHardwareNotRunningError = 'stop';
  kAudioHardwareUnspecifiedError = 'what';
  kAudioHardwareUnknownPropertyError = 'who?';
  kAudioHardwareBadPropertySizeError = '!siz';
  kAudioHardwareIllegalOperationError = 'nope';
  kAudioHardwareBadObjectError = '!obj';
  kAudioHardwareBadDeviceError = '!dev';
  kAudioHardwareBadStreamError = '!str';
  kAudioHardwareUnsupportedOperationError = 'unop';
  kAudioDeviceUnsupportedFormatError = '!dat';
  kAudioDevicePermissionsError = '!hog';
const
  kAudioObjectUnknown = 0;
const
  kAudioObjectPropertyScopeGlobal = 'glob';
  kAudioObjectPropertyScopeInput = 'inpt';
  kAudioObjectPropertyScopeOutput = 'outp';
  kAudioObjectPropertyScopePlayThrough = 'ptru';
  kAudioObjectPropertyElementMaster = 0;
const
  kAudioObjectPropertySelectorWildcard = '****';
  kAudioObjectPropertyScopeWildcard = '****';
  kAudioObjectPropertyElementWildcard = $FFFFFFFF;
  kAudioObjectClassIDWildcard = '****';
const
  kAudioObjectClassID = 'aobj';
const
  kAudioObjectPropertyBaseClass = 'bcls';
  kAudioObjectPropertyClass = 'clas';
  kAudioObjectPropertyOwner = 'stdv';
  kAudioObjectPropertyName = 'lnam';
  kAudioObjectPropertyModelName = 'lmod';
  kAudioObjectPropertyManufacturer = 'lmak';
  kAudioObjectPropertyElementName = 'lchn';
  kAudioObjectPropertyElementCategoryName = 'lccn';
  kAudioObjectPropertyElementNumberName = 'lcnn';
  kAudioObjectPropertyOwnedObjects = 'ownd';
  kAudioObjectPropertyIdentify = 'iden';
  kAudioObjectPropertySerialNumber = 'snum';
  kAudioObjectPropertyFirmwareVersion = 'fwvn';
const
  kAudioPlugInClassID = 'aplg';
const
  kAudioPlugInPropertyBundleID = 'piid';
  kAudioPlugInPropertyDeviceList = 'dev#';
  kAudioPlugInPropertyTranslateUIDToDevice = 'uidd';
  kAudioPlugInPropertyBoxList = 'box#';
  kAudioPlugInPropertyTranslateUIDToBox = 'uidb';
const
  kAudioTransportManagerClassID = 'trpm';
const
  kAudioTransportManagerPropertyEndPointList = 'end#';
  kAudioTransportManagerPropertyTranslateUIDToEndPoint = 'uide';
  kAudioTransportManagerPropertyTransportType = 'tran';
const
  kAudioBoxClassID = 'abox';
const
  kAudioBoxPropertyBoxUID = 'buid';
  kAudioBoxPropertyTransportType = 'tran';
  kAudioBoxPropertyHasAudio = 'bhau';
  kAudioBoxPropertyHasVideo = 'bhvi';
  kAudioBoxPropertyHasMIDI = 'bhmi';
  kAudioBoxPropertyIsProtected = 'bpro';
  kAudioBoxPropertyAcquired = 'bxon';
  kAudioBoxPropertyAcquisitionFailed = 'bxof';
  kAudioBoxPropertyDeviceList = 'bdv#';
const
  kAudioDeviceClassID = 'adev';
const
  kAudioDeviceTransportTypeUnknown = 0;
  kAudioDeviceTransportTypeBuiltIn = 'bltn';
  kAudioDeviceTransportTypeAggregate = 'grup';
  kAudioDeviceTransportTypeVirtual = 'virt';
  kAudioDeviceTransportTypePCI = 'pci ';
  kAudioDeviceTransportTypeUSB = 'usb ';
  kAudioDeviceTransportTypeFireWire = '1394';
  kAudioDeviceTransportTypeBluetooth = 'blue';
  kAudioDeviceTransportTypeBluetoothLE = 'blea';
  kAudioDeviceTransportTypeHDMI = 'hdmi';
  kAudioDeviceTransportTypeDisplayPort = 'dprt';
  kAudioDeviceTransportTypeAirPlay = 'airp';
  kAudioDeviceTransportTypeAVB = 'eavb';
  kAudioDeviceTransportTypeThunderbolt = 'thun';
const
  kAudioDevicePropertyConfigurationApplication = 'capp';
  kAudioDevicePropertyDeviceUID = 'uid ';
  kAudioDevicePropertyModelUID = 'muid';
  kAudioDevicePropertyTransportType = 'tran';
  kAudioDevicePropertyRelatedDevices = 'akin';
  kAudioDevicePropertyClockDomain = 'clkd';
  kAudioDevicePropertyDeviceIsAlive = 'livn';
  kAudioDevicePropertyDeviceIsRunning = 'goin';
  kAudioDevicePropertyDeviceCanBeDefaultDevice = 'dflt';
  kAudioDevicePropertyDeviceCanBeDefaultSystemDevice = 'sflt';
  kAudioDevicePropertyLatency = 'ltnc';
  kAudioDevicePropertyStreams = 'stm#';
  kAudioObjectPropertyControlList = 'ctrl';
  kAudioDevicePropertySafetyOffset = 'saft';
  kAudioDevicePropertyNominalSampleRate = 'nsrt';
  kAudioDevicePropertyAvailableNominalSampleRates = 'nsr#';
  kAudioDevicePropertyIcon = 'icon';
  kAudioDevicePropertyIsHidden = 'hidn';
  kAudioDevicePropertyPreferredChannelsForStereo = 'dch2';
  kAudioDevicePropertyPreferredChannelLayout = 'srnd';
const
  kAudioEndPointDeviceClassID = 'edev';

const
  kAudioEndPointDeviceUIDKey = 'uid';
  kAudioEndPointDeviceNameKey = 'name';
  kAudioEndPointDeviceEndPointListKey = 'endpoints';
  kAudioEndPointDeviceMasterEndPointKey = 'master';
  kAudioEndPointDeviceIsPrivateKey = 'private';

const
  kAudioEndPointDevicePropertyComposition = 'acom';
  kAudioEndPointDevicePropertyEndPointList = 'agrp';
  kAudioEndPointDevicePropertyIsPrivate = 'priv';
const
  kAudioEndPointClassID = 'endp';

const
  kAudioEndPointUIDKey = 'uid';
  kAudioEndPointNameKey = 'name';
  kAudioEndPointInputChannelsKey = 'channels-in';
  kAudioEndPointOutputChannelsKey = 'channels-out';

type
  AudioStreamRangedDescription = record
    mFormat: AudioStreamBasicDescription;
    mSampleRateRange: AudioValueRange;
  end;
type
  AudioStreamRangedDescriptionPtr = ^AudioStreamRangedDescription;

const
  kAudioStreamClassID = 'astr';
const
  kAudioStreamTerminalTypeUnknown = 0;
  kAudioStreamTerminalTypeLine = 'line';
  kAudioStreamTerminalTypeDigitalAudioInterface = 'spdf';
  kAudioStreamTerminalTypeSpeaker = 'spkr';
  kAudioStreamTerminalTypeHeadphones = 'hdph';
  kAudioStreamTerminalTypeLFESpeaker = 'lfes';
  kAudioStreamTerminalTypeReceiverSpeaker = 'rspk';
  kAudioStreamTerminalTypeMicrophone = 'micr';
  kAudioStreamTerminalTypeHeadsetMicrophone = 'hmic';
  kAudioStreamTerminalTypeReceiverMicrophone = 'rmic';
  kAudioStreamTerminalTypeTTY = 'tty_';
  kAudioStreamTerminalTypeHDMI = 'hdmi';
  kAudioStreamTerminalTypeDisplayPort = 'dprt';
const
  kAudioStreamPropertyIsActive = 'sact';
  kAudioStreamPropertyDirection = 'sdir';
  kAudioStreamPropertyTerminalType = 'term';
  kAudioStreamPropertyStartingChannel = 'schn';
  kAudioStreamPropertyLatency = kAudioDevicePropertyLatency;
  kAudioStreamPropertyVirtualFormat = 'sfmt';
  kAudioStreamPropertyAvailableVirtualFormats = 'sfma';
  kAudioStreamPropertyPhysicalFormat = 'pft ';
  kAudioStreamPropertyAvailablePhysicalFormats = 'pfta';
const
  kAudioControlClassID = 'actl';
const
  kAudioControlPropertyScope = 'cscp';
  kAudioControlPropertyElement = 'celm';
const
  kAudioSliderControlClassID = 'sldr';
const
  kAudioSliderControlPropertyValue = 'sdrv';
  kAudioSliderControlPropertyRange = 'sdrr';
const
  kAudioLevelControlClassID = 'levl';
  kAudioVolumeControlClassID = 'vlme';
  kAudioLFEVolumeControlClassID = 'subv';
const
  kAudioLevelControlPropertyScalarValue = 'lcsv';
  kAudioLevelControlPropertyDecibelValue = 'lcdv';
  kAudioLevelControlPropertyDecibelRange = 'lcdr';
  kAudioLevelControlPropertyConvertScalarToDecibels = 'lcsd';
  kAudioLevelControlPropertyConvertDecibelsToScalar = 'lcds';
const
  kAudioBooleanControlClassID = 'togl';
  kAudioMuteControlClassID = 'mute';
  kAudioSoloControlClassID = 'solo';
  kAudioJackControlClassID = 'jack';
  kAudioLFEMuteControlClassID = 'subm';
  kAudioPhantomPowerControlClassID = 'phan';
  kAudioPhaseInvertControlClassID = 'phsi';
  kAudioClipLightControlClassID = 'clip';
  kAudioTalkbackControlClassID = 'talb';
  kAudioListenbackControlClassID = 'lsnb';
const
  kAudioBooleanControlPropertyValue = 'bcvl';
const
  kAudioSelectorControlClassID = 'slct';
  kAudioDataSourceControlClassID = 'dsrc';
  kAudioDataDestinationControlClassID = 'dest';
  kAudioClockSourceControlClassID = 'clck';
  kAudioLineLevelControlClassID = 'nlvl';
  kAudioHighPassFilterControlClassID = 'hipf';
const
  kAudioSelectorControlPropertyCurrentItem = 'scci';
  kAudioSelectorControlPropertyAvailableItems = 'scai';
  kAudioSelectorControlPropertyItemName = 'scin';
  kAudioSelectorControlPropertyItemKind = 'clkk';
const
  kAudioSelectorControlItemKindSpacer = 'spcr';
const
  kAudioClockSourceItemKindInternal = 'int ';
const
  kAudioStereoPanControlClassID = 'span';
const
  kAudioStereoPanControlPropertyValue = 'spcv';
  kAudioStereoPanControlPropertyPanningChannels = 'spcc';
{$endif}

{$endif}
