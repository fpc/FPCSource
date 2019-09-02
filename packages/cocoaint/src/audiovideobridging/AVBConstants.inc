{ Parsed from AudioVideoBridging.framework AVBConstants.h }

{$ifndef AudioVideoBridging_AVBConstants_h}
{$ifdef TYPES}
const
  AVBMACAddressSize = 6;

type
  AVB17221ADPEntityCapabilities = cuint32;
  AVB17221ADPEntityCapabilitiesPtr = ^AVB17221ADPEntityCapabilities;

const
  AVB17221ADPEntityCapabilitiesDFUMode = $00000001 deprecated 'in 10_8, 10_9, NA, NA';
  AVB17221ADPEntityCapabilitiesEFUMode = $00000001 { available in 10_9, NA };
  AVB17221ADPEntityCapabilitiesAddressAccessSupported = $00000002;
  AVB17221ADPEntityCapabilitiesGatewayEntity = $00000004;
  AVB17221ADPEntityCapabilitiesAEMSupported = $00000008;
  AVB17221ADPEntityCapabilitiesLegacyAVC = $00000010;
  AVB17221ADPEntityCapabilitiesAssociationIDSupported = $00000020;
  AVB17221ADPEntityCapabilitiesAssociationIDValid = $00000040;
  AVB17221ADPEntityCapabilitiesVendorUniqueSupported = $00000080;
  AVB17221ADPEntityCapabilitiesClassASupported = $00000100;
  AVB17221ADPEntityCapabilitiesClassBSupported = $00000200;
  AVB17221ADPEntityCapabilitiesASSupported = $00000400 deprecated 'in 10_8, 10_9, NA, NA';
  AVB17221ADPEntityCapabilitiesGPTPSupported = $00000400 { available in 10_9, NA };
  AVB17221ADPEntityCapabilitiesAEMAuthenticationSupported = $00000800 { available in 10_9, NA };
  AVB17221ADPEntityCapabilitiesAEMAuthenticationRequired = $00001000 { available in 10_9, NA };
  AVB17221ADPEntityCapabilitiesAEMPersistentAcquireSupported = $00002000 { available in 10_9, NA };
  AVB17221ADPEntityCapabilitiesAEMIdenitifyControlIndexValid = $00004000 { available in 10_9, NA };
  AVB17221ADPEntityCapabilitiesAEMInterfaceIndexValid = $00008000 { available in 10_9, NA };
  AVB17221ADPEntityCapabilitiesGeneralControllerIgnore = $00010000 { available in 10_9, NA };
  AVB17221ADPEntityCapabilitiesEntityNotReady = $00020000 { available in 10_9, NA };

type
  AVB17221ADPTalkerCapabilities = cuint16;
  AVB17221ADPTalkerCapabilitiesPtr = ^AVB17221ADPTalkerCapabilities;

const
  AVB17221ADPTalkerCapabilitiesImplemented = $0001;
  AVB17221ADPTalkerCapabilitiesHasOtherSource = $0200;
  AVB17221ADPTalkerCapabilitiesHasControlSource = $0400;
  AVB17221ADPTalkerCapabilitiesHasMediaClockSource = $0800;
  AVB17221ADPTalkerCapabilitiesHasSMPTESource = $1000;
  AVB17221ADPTalkerCapabilitiesHasMIDISource = $2000;
  AVB17221ADPTalkerCapabilitiesHasAudioSource = $4000;
  AVB17221ADPTalkerCapabilitiesHasVideoSource = $8000;

type
  AVB17221ADPListenerCapabilities = cuint16;
  AVB17221ADPListenerCapabilitiesPtr = ^AVB17221ADPListenerCapabilities;

const
  AVB17221ADPListenerCapabilitiesImplemented = $0001;
  AVB17221ADPListenerCapabilitiesHasOtherSink = $0200;
  AVB17221ADPListenerCapabilitiesHasControlSink = $0400;
  AVB17221ADPListenerCapabilitiesHasMediaClockSink = $0800;
  AVB17221ADPListenerCapabilitiesHasSMPTESink = $1000;
  AVB17221ADPListenerCapabilitiesHasMIDISink = $2000;
  AVB17221ADPListenerCapabilitiesHasAudioSink = $4000;
  AVB17221ADPListenerCapabilitiesHasVideoSink = $8000;

type
  AVB17221ADPControllerCapabilities = cuint32;
  AVB17221ADPControllerCapabilitiesPtr = ^AVB17221ADPControllerCapabilities;

const
  AVB17221ADPControllerCapabilitiesImplemented = $00000001;
  AVB17221ADPControllerCapabilitiesHasLayer3Proxy = $00000002;

type
  AVB17221AECPMessageType = cuint8;
  AVB17221AECPMessageTypePtr = ^AVB17221AECPMessageType;

const
  AVB17221AECPMessageTypeAEMCommand = $0;
  AVB17221AECPMessageTypeAEMResponse = $1;
  AVB17221AECPMessageTypeAddressAccessCommand = $2;
  AVB17221AECPMessageTypeAddressAccessResponse = $3;
  AVB17221AECPMessageTypeLegacyAVCCommand = $4;
  AVB17221AECPMessageTypeLegacyAVCResponse = $5;
  AVB17221AECPMessageTypeVendorUniqueCommand = $6;
  AVB17221AECPMessageTypeVendorUniqueResponse = $7;

type
  AVB17221AECPStatusCode = cuint8;
  AVB17221AECPStatusCodePtr = ^AVB17221AECPStatusCode;

const
  AVB17221AECPStatusSuccess = $00;
  AVB17221AECPStatusNotImplemented = $01;
  AVB17221AECPStatusNoSuchDescriptor = $02;
  AVB17221AECPStatusEntityLocked = $03;
  AVB17221AECPStatusEntityAcquired = $04;
  AVB17221AECPStatusNotAuthorized = $05;
  AVB17221AECPStatusInsufficientPrivileges = $06;
  AVB17221AECPStatusBadArguments = $07;
  AVB17221AECPStatusNoResources = $08;
  AVB17221AECPStatusInProgress = $09;
  AVB17221AECPStatusEntityMisbehaving = $0a;
  AVB17221AECPStatusNotSupported = $0b;
  AVB17221AECPStatusAddressAccessAddressTooLow = $02;
  AVB17221AECPStatusAddressAccessAddressTooHigh = $03;
  AVB17221AECPStatusAddressAccessAddressInvalid = $04;
  AVB17221AECPStatusAddressAccessTLVInvalid = $05;
  AVB17221AECPStatusAddressAccessDataInvalid = $06;
  AVB17221AECPStatusAddressAccessUnsupported = $07;
  AVB17221AECPStatusAVCFailure = $02;

type
  AVB17221ACMPMessageType = cuint8;
  AVB17221ACMPMessageTypePtr = ^AVB17221ACMPMessageType;

const
  AVB17221ACMPMessageTypeConnectTXCommand = $0;
  AVB17221ACMPMessageTypeConnectTXResponse = $1;
  AVB17221ACMPMessageTypeDisconnectTXCommand = $2;
  AVB17221ACMPMessageTypeDisconnectTXResponse = $3;
  AVB17221ACMPMessageTypeGetTXStateCommand = $4;
  AVB17221ACMPMessageTypeGetTXStateResponse = $5;
  AVB17221ACMPMessageTypeConnectRXCommand = $6;
  AVB17221ACMPMessageTypeConnectRXResponse = $7;
  AVB17221ACMPMessageTypeDisconnectRXCommand = $8;
  AVB17221ACMPMessageTypeDisconnectRXResponse = $9;
  AVB17221ACMPMessageTypeGetRXStateCommand = $A;
  AVB17221ACMPMessageTypeGetRXStateResponse = $B;
  AVB17221ACMPMessageTypeGetTXConnectionCommand = $C;
  AVB17221ACMPMessageTypeGetTXConnectionResponse = $D;

type
  AVB17221ACMPStatusCode = cuint8;
  AVB17221ACMPStatusCodePtr = ^AVB17221ACMPStatusCode;

const
  AVB17221ACMPStatusSuccess = $00;
  AVB17221ACMPStatusListenerUnknownID = $01;
  AVB17221ACMPStatusTalkerUnknownID = $02;
  AVB17221ACMPStatusTalkerDestMACFail = $03;
  AVB17221ACMPStatusTalkerNoStreamIndex = $04;
  AVB17221ACMPStatusTalkerNoBandwidth = $05;
  AVB17221ACMPStatusTalkerExclusive = $06;
  AVB17221ACMPStatusListenerTalkerTimeout = $07;
  AVB17221ACMPStatusListenerExclusive = $08;
  AVB17221ACMPStatusStateUnavailable = $09;
  AVB17221ACMPStatusNotConnected = $0a;
  AVB17221ACMPStatusNoSuchConnection = $0b;
  AVB17221ACMPStatusUnableToSendMessage = $0c;
  AVB17221ACMPStatusTalkerMisbehaving = $0d;
  AVB17221ACMPStatusListenerMisbehaving = $0e;
  AVB17221ACMPStatusSRPFace = $0f;
  AVB17221ACMPStatusControllerNotAuthorized = $10;
  AVB17221ACMPStatusIncompatibleRequest = $11;
  AVB17221ACMPStatusNotSupported = $1f;

type
  AVB17221ACMPFlags = cuint16;
  AVB17221ACMPFlagsPtr = ^AVB17221ACMPFlags;

const
  AVB17221ACMPFlagsNone = $0000;
  AVB17221ACMPFlagsClassB = $0001;
  AVB17221ACMPFlagsFastConnect = $0002;
  AVB17221ACMPFlagsSavedState = $0004;
  AVB17221ACMPFlagsStreamingWait = $0008;
  AVB17221ACMPFlagsSupportsEncrypted = $0010;
  AVB17221ACMPFlagsEncryptedPDU = $0020;
  AVB17221ACMPFlagsStreamingTalkerFailed = $0040;

type
  AVB17221AEMCommandType = cuint16;
  AVB17221AEMCommandTypePtr = ^AVB17221AEMCommandType;

const
  AVB17221AEMCommandTypeAcquireEntity = $0000;
  AVB17221AEMCommandTypeLockEntity = $0001;
  AVB17221AEMCommandTypeEntityAvailable = $0002;
  AVB17221AEMCommandTypeControllerAvailable = $0003;
  AVB17221AEMCommandTypeReadDescriptor = $0004;
  AVB17221AEMCommandTypeWriteDescriptor = $0005;
  AVB17221AEMCommandTypeSetConfiguration = $0006;
  AVB17221AEMCommandTypeGetConfiguration = $0007;
  AVB17221AEMCommandTypeSetStreamFormat = $0008;
  AVB17221AEMCommandTypeGetStreamFormat = $0009;
  AVB17221AEMCommandTypeSetVideoFormat = $000a;
  AVB17221AEMCommandTypeGetVideoFormat = $000b;
  AVB17221AEMCommandTypeSetSensorFormat = $000c;
  AVB17221AEMCommandTypeGetSensorFormat = $000d;
  AVB17221AEMCommandTypeSetStreamInfo = $000e;
  AVB17221AEMCommandTypeGetStreamInfo = $000f;
  AVB17221AEMCommandTypeSetName = $0010;
  AVB17221AEMCommandTypeGetName = $0011;
  AVB17221AEMCommandTypeSetAssociationID = $0012;
  AVB17221AEMCommandTypeGetAssociationID = $0013;
  AVB17221AEMCommandTypeSetSamplingRate = $0014;
  AVB17221AEMCommandTypeGetSamplingRate = $0015;
  AVB17221AEMCommandTypeSetClockSource = $0016;
  AVB17221AEMCommandTypeGetClockSource = $0017;
  AVB17221AEMCommandTypeSetControl = $0018;
  AVB17221AEMCommandTypeGetControl = $0019;
  AVB17221AEMCommandTypeIncrementControl = $001a;
  AVB17221AEMCommandTypeDecrementControl = $001b;
  AVB17221AEMCommandTypeSetSignalSelector = $001c;
  AVB17221AEMCommandTypeGetSignalSelector = $001d;
  AVB17221AEMCommandTypeSetMixer = $001e;
  AVB17221AEMCommandTypeGetMixer = $001f;
  AVB17221AEMCommandTypeSetMatrix = $0020;
  AVB17221AEMCommandTypeGetMatrix = $0021;
  AVB17221AEMCommandTypeStartStreaming = $0022;
  AVB17221AEMCommandTypeStopStreaming = $0023;
  AVB17221AEMCommandTypeRegisterUnsolicitedNotification = $0024;
  AVB17221AEMCommandTypeDeregisterUnsolicitedNotification = $0025;
  AVB17221AEMCommandTypeIdentifyNotification = $0026;
  AVB17221AEMCommandTypeGetAVBInfo = $0027;
  AVB17221AEMCommandTypeGetASPath = $0028;
  AVB17221AEMCommandTypeGetCounters = $0029;
  AVB17221AEMCommandTypeReboot = $002a;
  AVB17221AEMCommandTypeGetAudioMap = $002b;
  AVB17221AEMCommandTypeAddAudioMapping = $002c;
  AVB17221AEMCommandTypeRemoveAudioMapping = $002d;
  AVB17221AEMCommandTypeGetVideoMap = $002e;
  AVB17221AEMCommandTypeAddVideoMapping = $002f;
  AVB17221AEMCommandTypeRemoveVideoMapping = $0030;
  AVB17221AEMCommandTypeGetSensorMap = $0031;
  AVB17221AEMCommandTypeAddSensorMapping = $0032;
  AVB17221AEMCommandTypeRemoveSensorMapping = $0033;
  AVB17221AEMCommandTypeStartOperation = $0034;
  AVB17221AEMCommandTypeAbortOperation = $0035;
  AVB17221AEMCommandTypeOperationStatus = $0036;
  AVB17221AEMCommandTypeAuthenticateAddKey = $0037;
  AVB17221AEMCommandTypeAuthenticateDeleteKey = $0038;
  AVB17221AEMCommandTypeAuthenticateGetKeyList = $0039;
  AVB17221AEMCommandTypeAuthenticateGetKey = $003a;
  AVB17221AEMCommandTypeAuthenticateAddKeyToChain = $003b;
  AVB17221AEMCommandTypeAuthenticateDeleteKeyFromChain = $003c;
  AVB17221AEMCommandTypeAuthenticateGetKeychainList = $003d;
  AVB17221AEMCommandTypeAuthenticateGetIdentity = $003e;
  AVB17221AEMCommandTypeAuthenticateAddToken = $003f;
  AVB17221AEMCommandTypeAuthenticateDeleteToken = $0040;
  AVB17221AEMCommandTypeAuthenticate = $0041;
  AVB17221AEMCommandTypeDeauthenticate = $0042;
  AVB17221AEMCommandTypeEnableTransportSecurity = $0043;
  AVB17221AEMCommandTypeDisableTransportSecurity = $0044;
  AVB17221AEMCommandTypeEnableStreamEncryption = $0045;
  AVB17221AEMCommandTypeDisableStreamEncryption = $0046;
  AVB17221AEMCommandTypeSetMemoryObjectLength = $0047;
  AVB17221AEMCommandTypeGetMemoryObjectLength = $0048;
  AVB17221AEMCommandTypeSetStreamBackup = $0049;
  AVB17221AEMCommandTypeGetStreamBackup = $004a;

type
  AVB17221AECPAddressAccessTLVMode = cuint8;
  AVB17221AECPAddressAccessTLVModePtr = ^AVB17221AECPAddressAccessTLVMode;

const
  AVB17221AECPAddressAccessTLVModeRead = $00;
  AVB17221AECPAddressAccessTLVModeWrite = $01;
  AVB17221AECPAddressAccessTLVModeExecute = $02;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVBErrorDomain: NSString; cvar; external;
{$endif}

{$endif}
