{ Parsed from IOBluetooth.framework Bluetooth.h }

{$ifdef TYPES}
type
  BluetoothConnectionHandle = cuint16;
  BluetoothConnectionHandlePtr = ^BluetoothConnectionHandle;
  BluetoothLMPHandle = cuint8;
  BluetoothLMPHandlePtr = ^BluetoothLMPHandle;

const
  kBluetoothConnectionHandleNone = $ffff;

type
  BluetoothReasonCode = cuint8;
  BluetoothReasonCodePtr = ^BluetoothReasonCode;
  BluetoothEncryptionEnable = cuint8;
  BluetoothEncryptionEnablePtr = ^BluetoothEncryptionEnable;

const
  kBluetoothEncryptionEnableOff = $00;
  kBluetoothEncryptionEnableOn = $01;
  kBluetoothEncryptionEnableBREDRE0 = $01;
  kBluetoothEncryptionEnableLEAESCCM = $01;
  kBluetoothEncryptionEnableBREDRAESCCM = $02;

type
  BluetoothKeyFlag = cuint8;
  BluetoothKeyFlagPtr = ^BluetoothKeyFlag;

const
  kBluetoothKeyFlagSemiPermanent = $00;
  kBluetoothKeyFlagTemporary = $01;

type
  BluetoothKeyType = cuint8;
  BluetoothKeyTypePtr = ^BluetoothKeyType;

const
  kBluetoothKeyTypeCombination = $00;
  kBluetoothKeyTypeLocalUnit = $01;
  kBluetoothKeyTypeRemoteUnit = $02;
  kBluetoothKeyTypeDebugCombination = $03;
  kBluetoothKeyTypeUnauthenticatedCombination = $04;
  kBluetoothKeyTypeAuthenticatedCombination = $05;
  kBluetoothKeyTypeChangedCombination = $06;

type
  BluetoothPacketType = cuint16;
  BluetoothPacketTypePtr = ^BluetoothPacketType;

const
  kBluetoothPacketTypeReserved1 = $0001;
  kBluetoothPacketType2DH1Omit = $0002;
  kBluetoothPacketType3DH1Omit = $0004;
  kBluetoothPacketTypeDM1 = $0008;
  kBluetoothPacketTypeDH1 = $0010;
  kBluetoothPacketTypeHV1 = $0020;
  kBluetoothPacketTypeHV2 = $0040;
  kBluetoothPacketTypeHV3 = $0080;
  kBluetoothPacketTypeDV = $0100;
  kBluetoothPacketType2DH3Omit = $0100;
  kBluetoothPacketType3DH3Omit = $0200;
  kBluetoothPacketTypeAUX = $0200;
  kBluetoothPacketTypeDM3 = $0400;
  kBluetoothPacketTypeDH3 = $0800;
  kBluetoothPacketType2DH5Omit = $1000;
  kBluetoothPacketType3DM5Omit = $2000;
  kBluetoothPacketTypeDM5 = $4000;
  kBluetoothPacketTypeDH5 = $8000;
  kBluetoothPacketTypeEnd = 0;
const
  kBluetoothSynchronousConnectionPacketTypeNone = $0000;
  kBluetoothSynchronousConnectionPacketTypeHV1 = $0001;
  kBluetoothSynchronousConnectionPacketTypeHV2 = $0002;
  kBluetoothSynchronousConnectionPacketTypeHV3 = $0004;
  kBluetoothSynchronousConnectionPacketTypeEV3 = $0008;
  kBluetoothSynchronousConnectionPacketTypeEV4 = $0010;
  kBluetoothSynchronousConnectionPacketTypeEV5 = $0020;
  kBluetoothSynchronousConnectionPacketType2EV3Omit = $0040;
  kBluetoothSynchronousConnectionPacketType3EV3Omit = $0080;
  kBluetoothSynchronousConnectionPacketType2EV5Omit = $0100;
  kBluetoothSynchronousConnectionPacketType3EV5Omit = $0200;
  kBluetoothSynchronousConnectionPacketTypeFutureUse = $FC00;
  kBluetoothSynchronousConnectionPacketTypeAll = $FFFF;
  kBluetoothSynchronousConnectionPacketTypeEnd = 0;

type
  BluetoothLAP = cuint32;
  BluetoothLAPPtr = ^BluetoothLAP;

const
  kBluetoothGeneralInquiryAccessCodeIndex = 0;
  kBluetoothGeneralInquiryAccessCodeLAPValue = $9E8B33;
  kBluetoothLimitedInquiryAccessCodeIndex = 1;
  kBluetoothLimitedInquiryAccessCodeLAPValue = $9E8B00;
  kBluetoothLimitedInquiryAccessCodeEnd = 2;

type
  BluetoothPageScanRepetitionMode = cuint8;
  BluetoothPageScanRepetitionModePtr = ^BluetoothPageScanRepetitionMode;

const
  kBluetoothPageScanRepetitionModeR0 = $00;
  kBluetoothPageScanRepetitionModeR1 = $01;
  kBluetoothPageScanRepetitionModeR2 = $02;

type
  BluetoothPageScanPeriodMode = cuint8;
  BluetoothPageScanPeriodModePtr = ^BluetoothPageScanPeriodMode;

const
  kBluetoothPageScanPeriodModeP0 = $00;
  kBluetoothPageScanPeriodModeP1 = $01;
  kBluetoothPageScanPeriodModeP2 = $02;

type
  BluetoothPageScanMode = cuint8;
  BluetoothPageScanModePtr = ^BluetoothPageScanMode;

const
  kBluetoothPageScanModeMandatory = $00;
  kBluetoothPageScanModeOptional1 = $01;
  kBluetoothPageScanModeOptional2 = $02;
  kBluetoothPageScanModeOptional3 = $03;

type
  BluetoothHCIPageScanType = cuint8;
  BluetoothHCIPageScanTypePtr = ^BluetoothHCIPageScanType;

const
  kBluetoothHCIPageScanTypeStandard = $00;
  kBluetoothHCIPageScanTypeInterlaced = $01;
  kBluetoothHCIPageScanTypeReservedStart = $02;
  kBluetoothHCIPageScanTypeReservedEnd = $FF;

type
  BluetoothHCIErroneousDataReporting = cuint8;
  BluetoothHCIErroneousDataReportingPtr = ^BluetoothHCIErroneousDataReporting;

const
  kBluetoothHCIErroneousDataReportingDisabled = $00;
  kBluetoothHCIErroneousDataReportingEnabled = $01;
  kBluetoothHCIErroneousDataReportingReservedStart = $02;
  kBluetoothHCIErroneousDataReportingReservedEnd = $FF;
{$endif}

{$ifdef TYPES}
type
  BluetoothDeviceAddress = record
    data:array[0..5] of cuint8;
  end;
type
  BluetoothDeviceAddressPtr = ^BluetoothDeviceAddress;
type
  BluetoothKey = record
    data:array[0..15] of cuint8;
  end;
type
  BluetoothKeyPtr = ^BluetoothKey;
type
  BluetoothIRK = record
    data:array[0..15] of cuint8;
  end;
type
  BluetoothIRKPtr = ^BluetoothIRK;
type
  BluetoothPINCode = record
    data:array[0..15] of cuint8;
  end;
type
  BluetoothPINCodePtr = ^BluetoothPINCode;

type
  BluetoothClassOfDevice = cuint32;
  BluetoothClassOfDevicePtr = ^BluetoothClassOfDevice;
  BluetoothServiceClassMajor = cuint32;
  BluetoothServiceClassMajorPtr = ^BluetoothServiceClassMajor;
  BluetoothDeviceClassMajor = cuint32;
  BluetoothDeviceClassMajorPtr = ^BluetoothDeviceClassMajor;
  BluetoothDeviceClassMinor = cuint32;
  BluetoothDeviceClassMinorPtr = ^BluetoothDeviceClassMinor;

const
  kBluetoothDeviceNameMaxLength = 248;

type
  BluetoothDeviceName = array[0..255] of cuint8;
  BluetoothDeviceNamePtr = ^BluetoothDeviceName;
  BluetoothClockOffset = cuint16;
  BluetoothClockOffsetPtr = ^BluetoothClockOffset;
  BluetoothRole = cuint8;
  BluetoothRolePtr = ^BluetoothRole;
  BluetoothAllowRoleSwitch = cuint8;
  BluetoothAllowRoleSwitchPtr = ^BluetoothAllowRoleSwitch;

const
  kBluetoothDontAllowRoleSwitch = $00;
  kBluetoothAllowRoleSwitch = $01;
const
  kBluetoothRoleBecomeMaster = $00;
  kBluetoothRoleRemainSlave = $01;

type
  BluetoothSetEventMask = record
    data:array[0..7] of cuint8;
  end;
type
  BluetoothSetEventMaskPtr = ^BluetoothSetEventMask;

type
  BluetoothPINType = cuint8;
  BluetoothPINTypePtr = ^BluetoothPINType;
{$endif}

{$ifdef TYPES}
const
  kBluetoothL2CAPMaxPacketSize = 65535;
  kBluetoothACLLogicalChannelReserved = 0;
  kBluetoothACLLogicalChannelL2CAPContinue = 1;
  kBluetoothACLLogicalChannelL2CAPStart = 2;
  kBluetoothACLLogicalChannelLMP = 3;

type
  BluetoothL2CAPChannelID = cuint16;
  BluetoothL2CAPChannelIDPtr = ^BluetoothL2CAPChannelID;

const
  kBluetoothL2CAPChannelNull = $0000;
  kBluetoothL2CAPChannelSignalling = $0001;
  kBluetoothL2CAPChannelConnectionLessData = $0002;
  kBluetoothL2CAPChannelAMPManagerProtocol = $0003;
  kBluetoothL2CAPChannelAttributeProtocol = $0004;
  kBluetoothL2CAPChannelLESignalling = $0005;
  kBluetoothL2CAPChannelSecurityManager = $0006;
  kBluetoothL2CAPChannelReservedStart = $0007;
  kBluetoothL2CAPChannelLEAP = $002A;
  kBluetoothL2CAPChannelLEAS = $002B;
  kBluetoothL2CAPChannelMagnet = $003A;
  kBluetoothL2CAPChannelReservedEnd = $003F;
  kBluetoothL2CAPChannelDynamicStart = $0040;
  kBluetoothL2CAPChannelDynamicEnd = $ffff;
  kBluetoothL2CAPChannelEnd = $ffff;

type
  BluetoothL2CAPGroupID = BluetoothL2CAPChannelID;
  BluetoothL2CAPGroupIDPtr = ^BluetoothL2CAPGroupID;
  BluetoothL2CAPPSM = cuint16;
  BluetoothL2CAPPSMPtr = ^BluetoothL2CAPPSM;

const
  kBluetoothL2CAPCommandCodeReserved = $00;
  kBluetoothL2CAPCommandCodeCommandReject = $01;
  kBluetoothL2CAPCommandCodeConnectionRequest = $02;
  kBluetoothL2CAPCommandCodeConnectionResponse = $03;
  kBluetoothL2CAPCommandCodeConfigureRequest = $04;
  kBluetoothL2CAPCommandCodeConfigureResponse = $05;
  kBluetoothL2CAPCommandCodeDisconnectionRequest = $06;
  kBluetoothL2CAPCommandCodeDisconnectionResponse = $07;
  kBluetoothL2CAPCommandCodeEchoRequest = $08;
  kBluetoothL2CAPCommandCodeEchoResponse = $09;
  kBluetoothL2CAPCommandCodeInformationRequest = $0A;
  kBluetoothL2CAPCommandCodeInformationResponse = $0B;
  kBluetoothL2CAPCommandCodeCreateChannelRequest = $0C;
  kBluetoothL2CAPCommandCodeCreateChannelResponse = $0D;
  kBluetoothL2CAPCommandCodeMoveChannelRequest = $0E;
  kBluetoothL2CAPCommandCodeMoveChannelResponse = $0F;
  kBluetoothL2CAPCommandCodeMoveChannelConfirmation = $10;
  kBluetoothL2CAPCommandCodeMoveChannelConfirmationResponse = $11;
  kBluetoothL2CAPCommandCodeConnectionParameterUpdateRequest = $12;
  kBluetoothL2CAPCommandCodeConnectionParameterUpdateResponse = $13;
  kBluetoothL2CAPCommandCodeLECreditBasedConnectionRequest = $14;
  kBluetoothL2CAPCommandCodeLECreditBasedConnectionResponse = $15;
  kBluetoothL2CAPCommandCodeLEFlowControlCredit = $16;
type
  BluetoothL2CAPCommandCode = clong;
const
  kBluetoothL2CAPCommandRejectReasonCommandNotUnderstood = $0000;
  kBluetoothL2CAPCommandRejectReasonSignallingMTUExceeded = $0001;
  kBluetoothL2CAPCommandRejectReasonInvalidCIDInRequest = $0002;
type
  BluetoothL2CAPCommandRejectReason = clong;

type
  BluetoothL2CAPMTU = cuint16;
  BluetoothL2CAPMTUPtr = ^BluetoothL2CAPMTU;
  BluetoothL2CAPLinkTimeout = cuint16;
  BluetoothL2CAPLinkTimeoutPtr = ^BluetoothL2CAPLinkTimeout;
  BluetoothL2CAPFlushTimeout = cuint16;
  BluetoothL2CAPFlushTimeoutPtr = ^BluetoothL2CAPFlushTimeout;

const
  kBluetoothL2CAPFlushTimeoutUseExisting = $0000;
  kBluetoothL2CAPFlushTimeoutImmediate = $0001;
  kBluetoothL2CAPFlushTimeoutForever = $FFFF;
  kBluetoothL2CAPFlushTimeoutEnd = 0;

type
  BluetoothL2CAPQualityOfServiceOptions = record
    flags: cuint8;
    serviceType: cuint8;
    tokenRate: cuint32;
    tokenBucketSize: cuint32;
    peakBandwidth: cuint32;
    latency: cuint32;
    delayVariation: cuint32;
  end;
type
  BluetoothL2CAPQualityOfServiceOptionsPtr = ^BluetoothL2CAPQualityOfServiceOptions;
type
  BluetoothL2CAPRetransmissionAndFlowControlOptions = record
    flags: cuint8;
    txWindowSize: cuint8;
    maxTransmit: cuint8;
    retransmissionTimeout: cuint16;
    monitorTimeout: cuint16;
    maxPDUPayloadSize: cuint16;
  end;
type
  BluetoothL2CAPRetransmissionAndFlowControlOptionsPtr = ^BluetoothL2CAPRetransmissionAndFlowControlOptions;

const
  kBluetoothL2CAPInfoTypeMaxConnectionlessMTUSize = $0001;
const
  kBluetoothL2CAPPacketHeaderSize = 4;

type
  BluetoothL2CAPByteCount = cuint16;
  BluetoothL2CAPByteCountPtr = ^BluetoothL2CAPByteCount;
  BluetoothL2CAPCommandID = cuint8;
  BluetoothL2CAPCommandIDPtr = ^BluetoothL2CAPCommandID;
  BluetoothL2CAPCommandByteCount = cuint16;
  BluetoothL2CAPCommandByteCountPtr = ^BluetoothL2CAPCommandByteCount;

const
  kBluetoothL2CAPConnectionResultSuccessful = $0000;
  kBluetoothL2CAPConnectionResultPending = $0001;
  kBluetoothL2CAPConnectionResultRefusedPSMNotSupported = $0002;
  kBluetoothL2CAPConnectionResultRefusedSecurityBlock = $0003;
  kBluetoothL2CAPConnectionResultRefusedNoResources = $0004;
type
  BluetoothL2CAPConnectionResult = clong;
const
  kBluetoothL2CAPConnectionStatusNoInfoAvailable = $0000;
  kBluetoothL2CAPConnectionStatusAuthenticationPending = $0001;
  kBluetoothL2CAPConnectionStatusAuthorizationPending = $0002;
type
  BluetoothL2CAPConnectionStatus = clong;
const
  kBluetoothL2CAPConfigurationResultSuccess = $0000;
  kBluetoothL2CAPConfigurationResultUnacceptableParams = $0001;
  kBluetoothL2CAPConfigurationResultRejected = $0002;
  kBluetoothL2CAPConfigurationResultUnknownOptions = $0003;
type
  BluetoothL2CAPConfigurationResult = clong;
const
  kBluetoothL2CAPConfigurationOptionMTU = $01;
  kBluetoothL2CAPConfigurationOptionFlushTimeout = $02;
  kBluetoothL2CAPConfigurationOptionQoS = $03;
  kBluetoothL2CAPConfigurationOptionRetransmissionAndFlowControl = $04;
  kBluetoothL2CAPConfigurationOptionFrameCheckSequence = $05;
  kBluetoothL2CAPConfigurationOptionExtendedFlowSpecification = $06;
  kBluetoothL2CAPConfigurationOptionExtendedWindowSize = $07;
type
  BluetoothL2CAPConfigurationOption = clong;
const
  kBluetoothL2CAPConfigurationOptionMTULength = 2;
  kBluetoothL2CAPConfigurationOptionFlushTimeoutLength = 2;
  kBluetoothL2CAPConfigurationOptionQoSLength = 22;
  kBluetoothL2CAPConfigurationOptionRetransmissionAndFlowControlLength = 9;
const
  kBluetoothL2CAPConfigurationBasicL2CAPModeFlag = $00;
  kBluetoothL2CAPConfigurationRetransmissionModeFlag = $01;
  kBluetoothL2CAPConfigurationFlowControlModeFlag = $02;
  kBluetoothL2CAPConfigurationEnhancedRetransmissionMode = $03;
  kBluetoothL2CAPConfigurationStreamingMode = $04;
type
  BluetoothL2CAPConfigurationRetransmissionAndFlowControlFlags = clong;
const
  kBluetoothL2CAPInformationTypeConnectionlessMTU = $0001;
  kBluetoothL2CAPInformationTypeExtendedFeatures = $0002;
  kBluetoothL2CAPInformationTypeFixedChannelsSupported = $0003;
type
  BluetoothL2CAPInformationType = clong;
const
  kBluetoothL2CAPInformationResultSuccess = $0000;
  kBluetoothL2CAPInformationResultNotSupported = $0001;
type
  BluetoothL2CAPInformationResult = clong;
const
  kBluetoothL2CAPInformationNoExtendedFeatures = $00000000;
  kBluetoothL2CAPInformationFlowControlMode = $00000001;
  kBluetoothL2CAPInformationRetransmissionMode = $00000002;
  kBluetoothL2CAPInformationBidirectionalQoS = $00000004;
  kBluetoothL2CAPInformationEnhancedRetransmissionMode = $00000008;
  kBluetoothL2CAPInformationStreamingMode = $00000010;
  kBluetoothL2CAPInformationFCSOption = $00000020;
  kBluetoothL2CAPInformationExtendedFlowSpecification = $00000040;
  kBluetoothL2CAPInformationFixedChannels = $00000080;
  kBluetoothL2CAPInformationExtendedWindowSize = $00000100;
  kBluetoothL2CAPUnicastConnectionlessDataReception = $00000200;
type
  BluetoothL2CAPInformationExtendedFeaturesMask = clong;
const
  kBluetoothL2CAPQoSTypeNoTraffic = $00;
  kBluetoothL2CAPQoSTypeBestEffort = $01;
  kBluetoothL2CAPQoSTypeGuaranteed = $02;
type
  BluetoothL2CAPQoSType = clong;
const
  kBluetoothL2CAPMTULowEnergyDefault = $001b;
  kBluetoothL2CAPMTUMinimum = $0030;
  kBluetoothL2CAPMTUDefault = $03F9;
  kBluetoothL2CAPMTUMaximum = $ffff;
  kBluetoothL2CAPMTUStart = $7fff;
  kBluetoothL2CAPMTUSIG = $0030;
  kBluetoothL2CAPFlushTimeoutDefault = kBluetoothL2CAPFlushTimeoutForever;
  kBluetoothL2CAPQoSFlagsDefault = 0;
  kBluetoothL2CAPQoSTypeDefault = kBluetoothL2CAPQoSTypeBestEffort;
  kBluetoothL2CAPQoSTokenRateDefault = $00000000;
  kBluetoothL2CAPQoSTokenBucketSizeDefault = $00000000;
  kBluetoothL2CAPQoSPeakBandwidthDefault = $00000000;
  kBluetoothL2CAPQoSLatencyDefault = $ffffffff;
  kBluetoothL2CAPQoSDelayVariationDefault = $ffffffff;

const
  kBluetoothLESMPTimeout = 30;
  kBluetoothLESMPMinEncryptionKeySize = 7;
  kBluetoothLESMPMaxEncryptionKeySize = 16;

const
  kBluetoothLESecurityManagerEncryptionKey = 1 shl 0;
  kBluetoothLESecurityManagerIDKey = 1 shl 1;
  kBluetoothLESecurityManagerSignKey = 1 shl 2;
const
  kBluetoothLESecurityManagerCommandCodeReserved = $00;
  kBluetoothLESecurityManagerCommandCodePairingRequest = $01;
  kBluetoothLESecurityManagerCommandCodePairingResponse = $02;
  kBluetoothLESecurityManagerCommandCodePairingConfirm = $03;
  kBluetoothLESecurityManagerCommandCodePairingRandom = $04;
  kBluetoothLESecurityManagerCommandCodePairingFailed = $05;
  kBluetoothLESecurityManagerCommandCodeEncryptionInfo = $06;
  kBluetoothLESecurityManagerCommandCodeMasterIdentification = $07;
  kBluetoothLESecurityManagerCommandCodeIdentityInfo = $08;
  kBluetoothLESecurityManagerCommandCodeIdentityAddressInfo = $09;
  kBluetoothLESecurityManagerCommandCodeSigningInfo = $0A;
  kBluetoothLESecurityManagerCommandCodeSecurityRequest = $0B;
  kBluetoothLESecurityManagerCommandCodePairingPublicKey = $0C;
  kBluetoothLESecurityManagerCommandCodePairingDHKeyCheck = $0D;
  kBluetoothLESecurityManagerCommandCodePairingKeypressNotification = $0E;
  kBluetoothLESecurityManagerCommandCodeReservedStart = $0C;
  kBluetoothLESecurityManagerCommandCodeReservedEnd = $FF;
type
  BluetoothLESecurityManagerCommandCode = clong;
const
  kBluetoothLESecurityManagerUserInputCapabilityNoInput = $01;
  kBluetoothLESecurityManagerUserInputCapabilityYesNo = $02;
  kBluetoothLESecurityManagerUserInputCapabilityKeyboard = $03;
type
  BluetoothLESecurityManagerUserInputCapability = clong;
const
  kBluetoothLESecurityManagerUserOutputCapabilityNoOutput = $01;
  kBluetoothLESecurityManagerUserOutputCapabilityNumericOutput = $02;
type
  BluetoothLESecurityManagerUserOutputCapability = clong;
const
  kBluetoothLESecurityManagerIOCapabilityDisplayOnly = $00;
  kBluetoothLESecurityManagerIOCapabilityDisplayYesNo = $01;
  kBluetoothLESecurityManagerIOCapabilityKeyboardOnly = $02;
  kBluetoothLESecurityManagerIOCapabilityNoInputNoOutput = $03;
  kBluetoothLESecurityManagerIOCapabilityKeyboardDisplay = $04;
  kBluetoothLESecurityManagerIOCapabilityReservedStart = $05;
  kBluetoothLESecurityManagerIOCapabilityReservedEnd = $FF;
type
  BluetoothLESecurityManagerIOCapability = clong;
const
  kBluetoothLESecurityManagerOOBAuthenticationDataNotPresent = $00;
  kBluetoothLESecurityManagerOOBAuthenticationDataPresent = $01;
  kBluetoothLESecurityManagerOOBDataReservedStart = $02;
  kBluetoothLESecurityManagerOOBDataReservedEnd = $FF;
type
  BluetoothLESecurityManagerOOBData = clong;
const
  kBluetoothLESecurityManagerNoBonding = 0;
  kBluetoothLESecurityManagerBonding = 1;
  kBluetoothLESecurityManagerReservedStart = 2;
  kBluetoothLESecurityManagerReservedEnd = 3;
const
  kBluetoothLESecurityManagerReasonCodeReserved = $00;
  kBluetoothLESecurityManagerReasonCodePasskeyEntryFailed = $01;
  kBluetoothLESecurityManagerReasonCodeOOBNotAvailbale = $02;
  kBluetoothLESecurityManagerReasonCodeAuthenticationRequirements = $03;
  kBluetoothLESecurityManagerReasonCodeConfirmValueFailed = $04;
  kBluetoothLESecurityManagerReasonCodePairingNotSupported = $05;
  kBluetoothLESecurityManagerReasonCodeEncryptionKeySize = $06;
  kBluetoothLESecurityManagerReasonCodeCommandNotSupported = $07;
  kBluetoothLESecurityManagerReasonCodeUnspecifiedReason = $08;
  kBluetoothLESecurityManagerReasonCodeRepeatedAttempts = $09;
  kBluetoothLESecurityManagerReasonCodeInvalidParameters = $0A;
  kBluetoothLESecurityManagerReasonCodeDHKeyCheckFailed = $0B;
  kBluetoothLESecurityManagerReasonCodeNumericComparisonFailed = $0C;
  kBluetoothLESecurityManagerReasonCodeReservedStart = $0D;
  kBluetoothLESecurityManagerReasonCodeReservedEnd = $FF;
type
  BluetoothLESecurityManagerPairingFailedReasonCode = clong;
const
  kBluetoothAMPManagerCodeReserved = $00;
  kBluetoothAMPManagerCodeAMPCommandReject = $01;
  kBluetoothAMPManagerCodeAMPDiscoverRequest = $02;
  kBluetoothAMPManagerCodeAMPDiscoverResponse = $03;
  kBluetoothAMPManagerCodeAMPChangeNotify = $04;
  kBluetoothAMPManagerCodeAMPChangeResponse = $05;
  kBluetoothAMPManagerCodeAMPGetInfoRequest = $06;
  kBluetoothAMPManagerCodeAMPGetInfoResponse = $07;
  kBluetoothAMPManagerCodeAMPGetAssocRequest = $08;
  kBluetoothAMPManagerCodeAMPGetAssocResponse = $09;
  kBluetoothAMPManagerCodeAMPCreatePhysicalLinkRequest = $0A;
  kBluetoothAMPManagerCodeAMPCreatePhysicalLinkResponse = $0B;
  kBluetoothAMPManagerCodeAMPDisconnectPhysicalLinkRequest = $0C;
  kBluetoothAMPManagerCodeAMPDisconnectPhysicalLinkResponse = $0D;
type
  BluetoothAMPManagerCode = clong;
const
  kBluetoothAMPManagerCommandRejectReasonCommandNotRecognized = $0000;
type
  BluetoothAMPCommandRejectReason = clong;
const
  kBluetoothAMPManagerDiscoverResponseControllerStatusPoweredDown = $00;
  kBluetoothAMPManagerDiscoverResponseControllerStatusBluetoothOnly = $01;
  kBluetoothAMPManagerDiscoverResponseControllerStatusNoCapacity = $02;
  kBluetoothAMPManagerDiscoverResponseControllerStatusLowCapacity = $03;
  kBluetoothAMPManagerDiscoverResponseControllerStatusMediumCapacity = $04;
  kBluetoothAMPManagerDiscoverResponseControllerStatusHighCapacity = $05;
  kBluetoothAMPManagerDiscoverResponseControllerStatusFullCapacity = $06;
type
  BluetoothAMPDiscoverResponseControllerStatus = clong;
const
  kBluetoothAMPManagerGetInfoResponseSuccess = $00;
  kBluetoothAMPManagerGetInfoResponseInvalidControllerID = $01;
type
  BluetoothAMPGetInfoResponseStatus = clong;
const
  kBluetoothAMPManagerGetAssocResponseSuccess = $00;
  kBluetoothAMPManagerGetAssocResponseInvalidControllerID = $01;
type
  BluetoothAMPGetAssocResponseStatus = clong;
const
  kBluetoothAMPManagerCreatePhysicalLinkResponseSuccess = $00;
  kBluetoothAMPManagerCreatePhysicalLinkResponseInvalidControllerID = $01;
  kBluetoothAMPManagerCreatePhysicalLinkResponseUnableToStartLinkCreation = $02;
  kBluetoothAMPManagerCreatePhysicalLinkResponseCollisionOccurred = $03;
  kBluetoothAMPManagerCreatePhysicalLinkResponseAMPDisconnectedPhysicalLinkRequestReceived = $04;
  kBluetoothAMPManagerCreatePhysicalLinkResponsePhysicalLinkAlreadyExists = $05;
  kBluetoothAMPManagerCreatePhysicalLinkResponseSecurityViolation = $06;
type
  BluetoothAMPCreatePhysicalLinkResponseStatus = clong;
const
  kBluetoothAMPManagerDisconnectPhysicalLinkResponseSuccess = $00;
  kBluetoothAMPManagerDisconnectPhysicalLinkResponseInvalidControllerID = $01;
  kBluetoothAMPManagerDisconnectPhysicalLinkResponseNoPhysicalLink = $02;
type
  BluetoothAMPDisconnectPhysicalLinkResponseStatus = clong;
{$endif}

{$ifdef TYPES}
type
  BluetoothHCICommandOpCodeGroup = cuint8;
  BluetoothHCICommandOpCodeGroupPtr = ^BluetoothHCICommandOpCodeGroup;
  BluetoothHCICommandOpCodeCommand = cuint16;
  BluetoothHCICommandOpCodeCommandPtr = ^BluetoothHCICommandOpCodeCommand;
  BluetoothHCICommandOpCode = cuint16;
  BluetoothHCICommandOpCodePtr = ^BluetoothHCICommandOpCode;
  BluetoothHCIVendorCommandSelector = cuint32;
  BluetoothHCIVendorCommandSelectorPtr = ^BluetoothHCIVendorCommandSelector;

const
  kBluetoothHCIOpCodeNoOp = 0;
  kBluetoothHCICommandGroupNoOp = $00;
  kBluetoothHCICommandNoOp = $0000;
  kBluetoothHCICommandGroupLinkControl = $01;
  kBluetoothHCICommandInquiry = $0001;
  kBluetoothHCICommandInquiryCancel = $0002;
  kBluetoothHCICommandPeriodicInquiryMode = $0003;
  kBluetoothHCICommandExitPeriodicInquiryMode = $0004;
  kBluetoothHCICommandCreateConnection = $0005;
  kBluetoothHCICommandDisconnect = $0006;
  kBluetoothHCICommandAddSCOConnection = $0007;
  kBluetoothHCICommandCreateConnectionCancel = $0008;
  kBluetoothHCICommandAcceptConnectionRequest = $0009;
  kBluetoothHCICommandRejectConnectionRequest = $000A;
  kBluetoothHCICommandLinkKeyRequestReply = $000B;
  kBluetoothHCICommandLinkKeyRequestNegativeReply = $000C;
  kBluetoothHCICommandPINCodeRequestReply = $000D;
  kBluetoothHCICommandPINCodeRequestNegativeReply = $000E;
  kBluetoothHCICommandChangeConnectionPacketType = $000F;
  kBluetoothHCICommandAuthenticationRequested = $0011;
  kBluetoothHCICommandSetConnectionEncryption = $0013;
  kBluetoothHCICommandChangeConnectionLinkKey = $0015;
  kBluetoothHCICommandMasterLinkKey = $0017;
  kBluetoothHCICommandRemoteNameRequest = $0019;
  kBluetoothHCICommandReadRemoteSupportedFeatures = $001B;
  kBluetoothHCICommandReadRemoteExtendedFeatures = $001C;
  kBluetoothHCICommandReadRemoteVersionInformation = $001D;
  kBluetoothHCICommandReadClockOffset = $001F;
  kBluetoothHCICommandRemoteNameRequestCancel = $001A;
  kBluetoothHCICommandReadLMPHandle = $0020;
  kBluetoothHCICommandSetupSynchronousConnection = $0028;
  kBluetoothHCICommandAcceptSynchronousConnectionRequest = $0029;
  kBluetoothHCICommandRejectSynchronousConnectionRequest = $002A;
  kBluetoothHCICommandIOCapabilityRequestReply = $002B;
  kBluetoothHCICommandUserConfirmationRequestReply = $002C;
  kBluetoothHCICommandUserConfirmationRequestNegativeReply = $002D;
  kBluetoothHCICommandUserPasskeyRequestReply = $002E;
  kBluetoothHCICommandUserPasskeyRequestNegativeReply = $002F;
  kBluetoothHCICommandRemoteOOBDataRequestReply = $0030;
  kBluetoothHCICommandRemoteOOBDataRequestNegativeReply = $0033;
  kBluetoothHCICommandIOCapabilityRequestNegativeReply = $0034;
  kBluetoothHCICommandGroupLinkPolicy = $02;
  kBluetoothHCICommandHoldMode = $0001;
  kBluetoothHCICommandSniffMode = $0003;
  kBluetoothHCICommandExitSniffMode = $0004;
  kBluetoothHCICommandParkMode = $0005;
  kBluetoothHCICommandExitParkMode = $0006;
  kBluetoothHCICommandQoSSetup = $0007;
  kBluetoothHCICommandRoleDiscovery = $0009;
  kBluetoothHCICommandSwitchRole = $000B;
  kBluetoothHCICommandReadLinkPolicySettings = $000C;
  kBluetoothHCICommandWriteLinkPolicySettings = $000D;
  kBluetoothHCICommandReadDefaultLinkPolicySettings = $000E;
  kBluetoothHCICommandWriteDefaultLinkPolicySettings = $000F;
  kBluetoothHCICommandFlowSpecification = $0010;
  kBluetoothHCICommandSniffSubrating = $0011;
  kBluetoothHCICommandAcceptSniffRequest = $0031;
  kBluetoothHCICommandRejectSniffRequest = $0032;
  kBluetoothHCICommandGroupHostController = $03;
  kBluetoothHCICommandSetEventMask = $0001;
  kBluetoothHCICommandReset = $0003;
  kBluetoothHCICommandSetEventFilter = $0005;
  kBluetoothHCICommandFlush = $0008;
  kBluetoothHCICommandReadPINType = $0009;
  kBluetoothHCICommandWritePINType = $000A;
  kBluetoothHCICommandCreateNewUnitKey = $000B;
  kBluetoothHCICommandReadStoredLinkKey = $000D;
  kBluetoothHCICommandWriteStoredLinkKey = $0011;
  kBluetoothHCICommandDeleteStoredLinkKey = $0012;
  kBluetoothHCICommandChangeLocalName = $0013;
  kBluetoothHCICommandReadLocalName = $0014;
  kBluetoothHCICommandReadConnectionAcceptTimeout = $0015;
  kBluetoothHCICommandWriteConnectionAcceptTimeout = $0016;
  kBluetoothHCICommandReadPageTimeout = $0017;
  kBluetoothHCICommandWritePageTimeout = $0018;
  kBluetoothHCICommandReadScanEnable = $0019;
  kBluetoothHCICommandWriteScanEnable = $001A;
  kBluetoothHCICommandReadPageScanActivity = $001B;
  kBluetoothHCICommandWritePageScanActivity = $001C;
  kBluetoothHCICommandReadInquiryScanActivity = $001D;
  kBluetoothHCICommandWriteInquiryScanActivity = $001E;
  kBluetoothHCICommandReadAuthenticationEnable = $001F;
  kBluetoothHCICommandWriteAuthenticationEnable = $0020;
  kBluetoothHCICommandReadEncryptionMode = $0021;
  kBluetoothHCICommandWriteEncryptionMode = $0022;
  kBluetoothHCICommandReadClassOfDevice = $0023;
  kBluetoothHCICommandWriteClassOfDevice = $0024;
  kBluetoothHCICommandReadVoiceSetting = $0025;
  kBluetoothHCICommandWriteVoiceSetting = $0026;
  kBluetoothHCICommandReadAutomaticFlushTimeout = $0027;
  kBluetoothHCICommandWriteAutomaticFlushTimeout = $0028;
  kBluetoothHCICommandReadNumberOfBroadcastRetransmissions = $0029;
  kBluetoothHCICommandWriteNumberOfBroadcastRetransmissions = $002A;
  kBluetoothHCICommandReadHoldModeActivity = $002B;
  kBluetoothHCICommandWriteHoldModeActivity = $002C;
  kBluetoothHCICommandReadTransmitPowerLevel = $002D;
  kBluetoothHCICommandReadSCOFlowControlEnable = $002E;
  kBluetoothHCICommandWriteSCOFlowControlEnable = $002F;
  kBluetoothHCICommandSetHostControllerToHostFlowControl = $0031;
  kBluetoothHCICommandHostBufferSize = $0033;
  kBluetoothHCICommandHostNumberOfCompletedPackets = $0035;
  kBluetoothHCICommandReadLinkSupervisionTimeout = $0036;
  kBluetoothHCICommandWriteLinkSupervisionTimeout = $0037;
  kBluetoothHCICommandReadNumberOfSupportedIAC = $0038;
  kBluetoothHCICommandReadCurrentIACLAP = $0039;
  kBluetoothHCICommandWriteCurrentIACLAP = $003A;
  kBluetoothHCICommandReadPageScanPeriodMode = $003B;
  kBluetoothHCICommandWritePageScanPeriodMode = $003C;
  kBluetoothHCICommandReadPageScanMode = $003D;
  kBluetoothHCICommandWritePageScanMode = $003E;
  kBluetoothHCICommandSetAFHClassification = $003F;
  kBluetoothHCICommandReadInquiryScanType = $0042;
  kBluetoothHCICommandWriteInquiryScanType = $0043;
  kBluetoothHCICommandReadInquiryMode = $0044;
  kBluetoothHCICommandWriteInquiryMode = $0045;
  kBluetoothHCICommandReadPageScanType = $0046;
  kBluetoothHCICommandWritePageScanType = $0047;
  kBluetoothHCICommandReadAFHChannelAssessmentMode = $0048;
  kBluetoothHCICommandWriteAFHChannelAssessmentMode = $0049;
  kBluetoothHCICommandReadExtendedInquiryResponse = $0051;
  kBluetoothHCICommandWriteExtendedInquiryResponse = $0052;
  kBluetoothHCICommandRefreshEncryptionKey = $0053;
  kBluetoothHCICommandReadSimplePairingMode = $0055;
  kBluetoothHCICommandWriteSimplePairingMode = $0056;
  kBluetoothHCICommandReadLocalOOBData = $0057;
  kBluetoothHCICommandReadInquiryResponseTransmitPower = $0058;
  kBluetoothHCICommandWriteInquiryResponseTransmitPower = $0059;
  kBluetoothHCICommandSendKeypressNotification = $0060;
  kBluetoothHCICommandReadDefaultErroneousDataReporting = $005A;
  kBluetoothHCICommandWriteDefaultErroneousDataReporting = $005B;
  kBluetoothHCICommandEnhancedFlush = $005F;
  kBluetoothHCICommandReadLogicalLinkAcceptTimeout = $0061;
  kBluetoothHCICommandWriteLogicalLinkAcceptTimeout = $0062;
  kBluetoothHCICommandSetEventMaskPageTwo = $0063;
  kBluetoothHCICommandReadLocationData = $0064;
  kBluetoothHCICommandWriteLocationData = $0065;
  kBluetoothHCICommandReadFlowControlMode = $0066;
  kBluetoothHCICommandWriteFlowControlMode = $0067;
  kBluetoothHCICommandReadEnhancedTransmitPowerLevel = $0068;
  kBluetoothHCICommandReadBestEffortFlushTimeout = $0069;
  kBluetoothHCICommandWriteBestEffortFlushTimeout = $006A;
  kBluetoothHCICommandShortRangeMode = $006B;
  kBluetoothHCICommandReadLEHostSupported = $006C;
  kBluetoothHCICommandWriteLEHostSupported = $006D;
  kBluetoothHCICommandGroupInformational = $04;
  kBluetoothHCICommandReadLocalVersionInformation = $0001;
  kBluetoothHCICommandReadLocalSupportedCommands = $0002;
  kBluetoothHCICommandReadLocalSupportedFeatures = $0003;
  kBluetoothHCICommandReadLocalExtendedFeatures = $0004;
  kBluetoothHCICommandReadBufferSize = $0005;
  kBluetoothHCICommandReadCountryCode = $0007;
  kBluetoothHCICommandReadDeviceAddress = $0009;
  kBluetoothHCICommandGroupStatus = $05;
  kBluetoothHCICommandReadFailedContactCounter = $0001;
  kBluetoothHCICommandResetFailedContactCounter = $0002;
  kBluetoothHCICommandGetLinkQuality = $0003;
  kBluetoothHCICommandReadRSSI = $0005;
  kBluetoothHCICommandReadAFHMappings = $0006;
  kBluetoothHCICommandReadClock = $0007;
  kBluetoothHCICommandReadEncryptionKeySize = $0008;
  kBluetoothHCICommandReadLocalAMPInfo = $0009;
  kBluetoothHCICommandReadLocalAMPASSOC = $000A;
  kBluetoothHCICommandWriteRemoteAMPASSOC = $000B;
  kBluetoothHCICommandGroupTesting = $06;
  kBluetoothHCICommandReadLoopbackMode = $0001;
  kBluetoothHCICommandWriteLoopbackMode = $0002;
  kBluetoothHCICommandEnableDeviceUnderTestMode = $0003;
  kBluetoothHCICommandWriteSimplePairingDebugMode = $0004;
  kBluetoothHCICommandEnableAMPReceiverReports = $0007;
  kBluetoothHCICommandAMPTestEnd = $0008;
  kBluetoothHCICommandAMPTest = $0009;
  kBluetoothHCICommandGroupLowEnergy = $08;
  kBluetoothHCICommandLESetEventMask = $0001;
  kBluetoothHCICommandLEReadBufferSize = $0002;
  kBluetoothHCICommandLEReadLocalSupportedFeatures = $0003;
  kBluetoothHCICommandLESetRandomAddress = $0005;
  kBluetoothHCICommandLESetAdvertisingParameters = $0006;
  kBluetoothHCICommandLEReadAdvertisingChannelTxPower = $0007;
  kBluetoothHCICommandLESetAdvertisingData = $0008;
  kBluetoothHCICommandLESetScanResponseData = $0009;
  kBluetoothHCICommandLESetAdvertiseEnable = $000A;
  kBluetoothHCICommandLESetScanParameters = $000B;
  kBluetoothHCICommandLESetScanEnable = $000C;
  kBluetoothHCICommandLECreateConnection = $000D;
  kBluetoothHCICommandLECreateConnectionCancel = $000E;
  kBluetoothHCICommandLEReadWhiteListSize = $000F;
  kBluetoothHCICommandLEClearWhiteList = $0010;
  kBluetoothHCICommandLEAddDeviceToWhiteList = $0011;
  kBluetoothHCICommandLERemoveDeviceFromWhiteList = $0012;
  kBluetoothHCICommandLEConnectionUpdate = $0013;
  kBluetoothHCICommandLESetHostChannelClassification = $0014;
  kBluetoothHCICommandLEReadChannelMap = $0015;
  kBluetoothHCICommandLEReadRemoteUsedFeatures = $0016;
  kBluetoothHCICommandLEEncrypt = $0017;
  kBluetoothHCICommandLERand = $0018;
  kBluetoothHCICommandLEStartEncryption = $0019;
  kBluetoothHCICommandLELongTermKeyRequestReply = $001A;
  kBluetoothHCICommandLELongTermKeyRequestNegativeReply = $001B;
  kBluetoothHCICommandLEReadSupportedStates = $001C;
  kBluetoothHCICommandLEReceiverTest = $001D;
  kBluetoothHCICommandLETransmitterTest = $001E;
  kBluetoothHCICommandLETestEnd = $001F;
  kBluetoothHCICommandGroupLogoTesting = $3E;
  kBluetoothHCICommandGroupVendorSpecific = $3F;
  kBluetoothHCICommandGroupMax = $40;
  kBluetoothHCICommandMax = $03FF;

type
  BluetoothHCIQoSFlags = cuint8;
  BluetoothHCIQoSFlagsPtr = ^BluetoothHCIQoSFlags;
  BluetoothHCIParamByteCount = cuint8;
  BluetoothHCIParamByteCountPtr = ^BluetoothHCIParamByteCount;
  BluetoothHCIACLDataByteCount = cuint16;
  BluetoothHCIACLDataByteCountPtr = ^BluetoothHCIACLDataByteCount;
  BluetoothHCISCODataByteCount = cuint8;
  BluetoothHCISCODataByteCountPtr = ^BluetoothHCISCODataByteCount;
  BluetoothHCIInquiryLength = cuint8;
  BluetoothHCIInquiryLengthPtr = ^BluetoothHCIInquiryLength;
  BluetoothHCIResponseCount = cuint8;
  BluetoothHCIResponseCountPtr = ^BluetoothHCIResponseCount;
  BluetoothHCICountryCode = cuint8;
  BluetoothHCICountryCodePtr = ^BluetoothHCICountryCode;
  BluetoothHCIModeInterval = cuint16;
  BluetoothHCIModeIntervalPtr = ^BluetoothHCIModeInterval;
  BluetoothHCISniffAttemptCount = cuint16;
  BluetoothHCISniffAttemptCountPtr = ^BluetoothHCISniffAttemptCount;
  BluetoothHCISniffTimeout = cuint16;
  BluetoothHCISniffTimeoutPtr = ^BluetoothHCISniffTimeout;
  BluetoothHCIParkModeBeaconInterval = cuint16;
  BluetoothHCIParkModeBeaconIntervalPtr = ^BluetoothHCIParkModeBeaconInterval;
  BluetoothMaxSlots = cuint8;
  BluetoothMaxSlotsPtr = ^BluetoothMaxSlots;
  BluetoothManufacturerName = cuint16;
  BluetoothManufacturerNamePtr = ^BluetoothManufacturerName;
  BluetoothLMPVersion = cuint8;
  BluetoothLMPVersionPtr = ^BluetoothLMPVersion;
  BluetoothLMPSubversion = cuint16;
  BluetoothLMPSubversionPtr = ^BluetoothLMPSubversion;
  BluetoothHCIConnectionMode = cuint8;
  BluetoothHCIConnectionModePtr = ^BluetoothHCIConnectionMode;

const
  kConnectionActiveMode = 0;
  kConnectionHoldMode = 1;
  kConnectionSniffMode = 2;
  kConnectionParkMode = 3;
  kConnectionModeReservedForFutureUse = 4;

type
  BluetoothHCISupportedCommands = record
    data:array[0..63] of cuint8;
  end;
type
  BluetoothHCISupportedCommandsPtr = ^BluetoothHCISupportedCommands;
type
  BluetoothHCISupportedFeatures = record
    data:array[0..7] of cuint8;
  end;
type
  BluetoothHCISupportedFeaturesPtr = ^BluetoothHCISupportedFeatures;

type
  BluetoothHCIPageNumber = cuint8;
  BluetoothHCIPageNumberPtr = ^BluetoothHCIPageNumber;

type
  BluetoothHCIExtendedFeaturesInfo = record
    page: BluetoothHCIPageNumber;
    maxPage: BluetoothHCIPageNumber;
    data:array[0..7] of cuint8;
  end;
type
  BluetoothHCIExtendedFeaturesInfoPtr = ^BluetoothHCIExtendedFeaturesInfo;

const
  kBluetoothFeatureThreeSlotPackets = 1 shl 0;
  kBluetoothFeatureFiveSlotPackets = 1 shl 1;
  kBluetoothFeatureEncryption = 1 shl 2;
  kBluetoothFeatureSlotOffset = 1 shl 3;
  kBluetoothFeatureTimingAccuracy = 1 shl 4;
  kBluetoothFeatureSwitchRoles = 1 shl 5;
  kBluetoothFeatureHoldMode = 1 shl 6;
  kBluetoothFeatureSniffMode = 1 shl 7;
  kBluetoothFeatureParkMode = 1 shl 0;
  kBluetoothFeatureRSSI = 1 shl 1;
  kBluetoothFeaturePowerControlRequests = 1 shl 1;
  kBluetoothFeatureChannelQuality = 1 shl 2;
  kBluetoothFeatureSCOLink = 1 shl 3;
  kBluetoothFeatureHV2Packets = 1 shl 4;
  kBluetoothFeatureHV3Packets = 1 shl 5;
  kBluetoothFeatureULawLog = 1 shl 6;
  kBluetoothFeatureALawLog = 1 shl 7;
  kBluetoothFeatureCVSD = 1 shl 0;
  kBluetoothFeaturePagingScheme = 1 shl 1;
  kBluetoothFeaturePowerControl = 1 shl 2;
  kBluetoothFeatureTransparentSCOData = 1 shl 3;
  kBluetoothFeatureFlowControlLagBit0 = 1 shl 4;
  kBluetoothFeatureFlowControlLagBit1 = 1 shl 5;
  kBluetoothFeatureFlowControlLagBit2 = 1 shl 6;
  kBluetoothFeatureBroadcastEncryption = 1 shl 7;
  kBluetoothFeatureScatterMode = 1 shl 0;
  kBluetoothFeatureEnhancedDataRateACL2MbpsMode = 1 shl 1;
  kBluetoothFeatureEnhancedDataRateACL3MbpsMode = 1 shl 2;
  kBluetoothFeatureEnhancedInquiryScan = 1 shl 3;
  kBluetoothFeatureInterlacedInquiryScan = 1 shl 4;
  kBluetoothFeatureInterlacedPageScan = 1 shl 5;
  kBluetoothFeatureRSSIWithInquiryResult = 1 shl 6;
  kBluetoothFeatureExtendedSCOLink = 1 shl 7;
  kBluetoothFeatureEV4Packets = 1 shl 0;
  kBluetoothFeatureEV5Packets = 1 shl 1;
  kBluetoothFeatureAbsenceMasks = 1 shl 2;
  kBluetoothFeatureAFHCapableSlave = 1 shl 3;
  kBluetoothFeatureAFHClassificationSlave = 1 shl 4;
  kBluetoothFeatureAliasAuhentication = 1 shl 5;
  kBluetoothFeatureLESupportedController = 1 shl 6;
  kBluetoothFeature3SlotEnhancedDataRateACLPackets = 1 shl 7;
  kBluetoothFeature5SlotEnhancedDataRateACLPackets = 1 shl 0;
  kBluetoothFeatureSniffSubrating = 1 shl 1;
  kBluetoothFeaturePauseEncryption = 1 shl 2;
  kBluetoothFeatureAFHCapableMaster = 1 shl 3;
  kBluetoothFeatureAFHClassificationMaster = 1 shl 4;
  kBluetoothFeatureEnhancedDataRateeSCO2MbpsMode = 1 shl 5;
  kBluetoothFeatureEnhancedDataRateeSCO3MbpsMode = 1 shl 6;
  kBluetoothFeature3SlotEnhancedDataRateeSCOPackets = 1 shl 7;
  kBluetoothFeatureExtendedInquiryResponse = 1 shl 0;
  kBluetoothFeatureSecureSimplePairing = 1 shl 3;
  kBluetoothFeatureEncapsulatedPDU = 1 shl 4;
  kBluetoothFeatureErroneousDataReporting = 1 shl 5;
  kBluetoothFeatureNonFlushablePacketBoundaryFlag = 1 shl 6;
  kBluetoothFeatureLinkSupervisionTimeoutChangedEvent = 1 shl 0;
  kBluetoothFeatureInquiryTransmissionPowerLevel = 1 shl 1;
  kBluetoothFeatureExtendedFeatures = 1 shl 7;
  kBluetoothFeatureSimpleSecurePairingHostMode = 1 shl 0;

type
  BluetoothEventFilterCondition = record
    data:array[0..6] of cuint8;
  end;
type
  BluetoothEventFilterConditionPtr = ^BluetoothEventFilterCondition;

type
  BluetoothHCIFailedContactCount = cuint16;
  BluetoothHCIFailedContactCountPtr = ^BluetoothHCIFailedContactCount;

type
  BluetoothHCIFailedContactInfo = record
    count: BluetoothHCIFailedContactCount;
    handle: BluetoothConnectionHandle;
  end;
type
  BluetoothHCIFailedContactInfoPtr = ^BluetoothHCIFailedContactInfo;

type
  BluetoothHCIRSSIValue = SInt8;
  BluetoothHCIRSSIValuePtr = ^BluetoothHCIRSSIValue;

type
  BluetoothHCIRSSIInfo = record
    handle: BluetoothConnectionHandle;
    RSSIValue: BluetoothHCIRSSIValue;
  end;
type
  BluetoothHCIRSSIInfoPtr = ^BluetoothHCIRSSIInfo;

type
  BluetoothHCILinkQuality = cuint8;
  BluetoothHCILinkQualityPtr = ^BluetoothHCILinkQuality;

type
  BluetoothHCILinkQualityInfo = record
    handle: BluetoothConnectionHandle;
    qualityValue: BluetoothHCILinkQuality;
  end;
type
  BluetoothHCILinkQualityInfoPtr = ^BluetoothHCILinkQualityInfo;

type
  BluetoothHCIRole = cuint8;
  BluetoothHCIRolePtr = ^BluetoothHCIRole;

type
  BluetoothHCIRoleInfo = record
    role: cuint8;
    handle: BluetoothConnectionHandle;
  end;
type
  BluetoothHCIRoleInfoPtr = ^BluetoothHCIRoleInfo;

const
  kBluetoothHCIMasterRole = $00;
  kBluetoothHCISlaveRole = $01;

type
  BluetoothHCILinkPolicySettings = cuint16;
  BluetoothHCILinkPolicySettingsPtr = ^BluetoothHCILinkPolicySettings;

const
  kDisableAllLMModes = $0000;
  kEnableMasterSlaveSwitch = $0001;
  kEnableHoldMode = $0002;
  kEnableSniffMode = $0004;
  kEnableParkMode = $0008;
  kReservedForFutureUse = $0010;

type
  BluetoothHCILinkPolicySettingsInfo = record
    settings: BluetoothHCILinkPolicySettings;
    handle: BluetoothConnectionHandle;
  end;
type
  BluetoothHCILinkPolicySettingsInfoPtr = ^BluetoothHCILinkPolicySettingsInfo;
type
  BluetoothHCIQualityOfServiceSetupParams = record
    flags: cuint8;
    serviceType: cuint8;
    tokenRate: cuint32;
    peakBandwidth: cuint32;
    latency: cuint32;
    delayVariation: cuint32;
  end;
type
  BluetoothHCIQualityOfServiceSetupParamsPtr = ^BluetoothHCIQualityOfServiceSetupParams;
type
  BluetoothHCISetupSynchronousConnectionParams = record
    transmitBandwidth: cuint32;
    receiveBandwidth: cuint32;
    maxLatency: cuint16;
    voiceSetting: cuint16;
    retransmissionEffort: cuint8;
    packetType: cuint16;
  end;
type
  BluetoothHCISetupSynchronousConnectionParamsPtr = ^BluetoothHCISetupSynchronousConnectionParams;
type
  BluetoothHCIAcceptSynchronousConnectionRequestParams = record
    transmitBandwidth: cuint32;
    receiveBandwidth: cuint32;
    maxLatency: cuint16;
    contentFormat: cuint16;
    retransmissionEffort: cuint8;
    packetType: cuint16;
  end;
type
  BluetoothHCIAcceptSynchronousConnectionRequestParamsPtr = ^BluetoothHCIAcceptSynchronousConnectionRequestParams;

type
  BluetoothHCILoopbackMode = cuint8;
  BluetoothHCILoopbackModePtr = ^BluetoothHCILoopbackMode;

const
  kBluetoothHCILoopbackModeOff = $00;
  kBluetoothHCILoopbackModeLocal = $01;
  kBluetoothHCILoopbackModeRemote = $02;

type
  BluetoothReadClockInfo = record
    handle: BluetoothConnectionHandle;
    clock: cuint32;
    accuracy: cuint16;
  end;
type
  BluetoothReadClockInfoPtr = ^BluetoothReadClockInfo;
type
  BluetoothHCIEventFlowSpecificationData = record
    connectionHandle: BluetoothConnectionHandle;
    flags: cuint8;
    flowDirection: cuint8;
    serviceType: cuint8;
    tokenRate: cuint32;
    tokenBucketSize: cuint32;
    peakBandwidth: cuint32;
    accessLatency: cuint32;
  end;
type
  BluetoothHCIEventFlowSpecificationDataPtr = ^BluetoothHCIEventFlowSpecificationData;

type
  BluetoothHCIOperationID = cuint32;
  BluetoothHCIOperationIDPtr = ^BluetoothHCIOperationID;
  BluetoothHCIEventID = cuint32;
  BluetoothHCIEventIDPtr = ^BluetoothHCIEventID;
  BluetoothHCIDataID = cuint32;
  BluetoothHCIDataIDPtr = ^BluetoothHCIDataID;
  BluetoothHCISignalID = cuint32;
  BluetoothHCISignalIDPtr = ^BluetoothHCISignalID;
  BluetoothHCITransportID = cuint32;
  BluetoothHCITransportIDPtr = ^BluetoothHCITransportID;
  BluetoothHCITransportCommandID = cuint32;
  BluetoothHCITransportCommandIDPtr = ^BluetoothHCITransportCommandID;
  BluetoothHCIRequestID = cuint32;
  BluetoothHCIRequestIDPtr = ^BluetoothHCIRequestID;

type
  BluetoothHCIVersionInfo = record
    manufacturerName: BluetoothManufacturerName;
    lmpVersion: BluetoothLMPVersion;
    lmpSubVersion: BluetoothLMPSubversion;
    hciVersion: cuint8;
    hciRevision: cuint16;
  end;
type
  BluetoothHCIVersionInfoPtr = ^BluetoothHCIVersionInfo;
type
  BluetoothHCIBufferSize = record
    ACLDataPacketLength: cuint16;
    SCODataPacketLength: cuint8;
    totalNumACLDataPackets: cuint16;
    totalNumSCODataPackets: cuint16;
  end;
type
  BluetoothHCIBufferSizePtr = ^BluetoothHCIBufferSize;
type
  BluetoothHCILEBufferSize = record
    ACLDataPacketLength: cuint16;
    totalNumACLDataPackets: cuint8;
  end;
type
  BluetoothHCILEBufferSizePtr = ^BluetoothHCILEBufferSize;

type
  BluetoothHCIConnectionAcceptTimeout = cuint16;
  BluetoothHCIConnectionAcceptTimeoutPtr = ^BluetoothHCIConnectionAcceptTimeout;
  BluetoothHCIPageTimeout = cuint16;
  BluetoothHCIPageTimeoutPtr = ^BluetoothHCIPageTimeout;

const
  kDefaultPageTimeout = $2710;

type
  BluetoothHCINumLinkKeysDeleted = cuint16;
  BluetoothHCINumLinkKeysDeletedPtr = ^BluetoothHCINumLinkKeysDeleted;
  BluetoothHCINumLinkKeysToWrite = cuint8;
  BluetoothHCINumLinkKeysToWritePtr = ^BluetoothHCINumLinkKeysToWrite;
  BluetoothHCIDeleteStoredLinkKeyFlag = cuint8;
  BluetoothHCIDeleteStoredLinkKeyFlagPtr = ^BluetoothHCIDeleteStoredLinkKeyFlag;

const
  kDeleteKeyForSpecifiedDeviceOnly = $00;
  kDeleteAllStoredLinkKeys = $01;

type
  BluetoothHCIReadStoredLinkKeysFlag = cuint8;
  BluetoothHCIReadStoredLinkKeysFlagPtr = ^BluetoothHCIReadStoredLinkKeysFlag;

const
  kReturnLinkKeyForSpecifiedDeviceOnly = $00;
  kReadAllStoredLinkKeys = $01;

type
  BluetoothHCIStoredLinkKeysInfo = record
    numLinkKeysRead: cuint16;
    maxNumLinkKeysAllowedInDevice: cuint16;
  end;
type
  BluetoothHCIStoredLinkKeysInfoPtr = ^BluetoothHCIStoredLinkKeysInfo;

type
  BluetoothHCIPageScanMode = cuint8;
  BluetoothHCIPageScanModePtr = ^BluetoothHCIPageScanMode;

const
  kMandatoryPageScanMode = $00;
  kOptionalPageScanMode1 = $01;
  kOptionalPageScanMode2 = $02;
  kOptionalPageScanMode3 = $03;

type
  BluetoothHCIPageScanPeriodMode = cuint8;
  BluetoothHCIPageScanPeriodModePtr = ^BluetoothHCIPageScanPeriodMode;

const
  kP0Mode = $00;
  kP1Mode = $01;
  kP2Mode = $02;

type
  BluetoothHCIPageScanEnableState = cuint8;
  BluetoothHCIPageScanEnableStatePtr = ^BluetoothHCIPageScanEnableState;

const
  kNoScansEnabled = $00;
  kInquiryScanEnabledPageScanDisabled = $01;
  kInquiryScanDisabledPageScanEnabled = $02;
  kInquiryScanEnabledPageScanEnabled = $03;

type
  BluetoothHCIScanActivity = record
    scanInterval: cuint16;
    scanWindow: cuint16;
  end;
type
  BluetoothHCIScanActivityPtr = ^BluetoothHCIScanActivity;
type
  BluetoothHCIInquiryAccessCode = record
    data:array[0..2] of cuint8;
  end;
type
  BluetoothHCIInquiryAccessCodePtr = ^BluetoothHCIInquiryAccessCode;

type
  BluetoothHCIInquiryAccessCodeCount = cuint8;
  BluetoothHCIInquiryAccessCodeCountPtr = ^BluetoothHCIInquiryAccessCodeCount;

type
  BluetoothHCICurrentInquiryAccessCodes = record
    count: BluetoothHCIInquiryAccessCodeCount;
    codes: BluetoothHCIInquiryAccessCodePtr;
  end;
type
  BluetoothHCICurrentInquiryAccessCodesPtr = ^BluetoothHCICurrentInquiryAccessCodes;
type
  BluetoothHCILinkSupervisionTimeout = record
    handle: BluetoothConnectionHandle;
    timeout: cuint16;
  end;
type
  BluetoothHCILinkSupervisionTimeoutPtr = ^BluetoothHCILinkSupervisionTimeout;

type
  BluetoothHCIFlowControlState = cuint8;
  BluetoothHCIFlowControlStatePtr = ^BluetoothHCIFlowControlState;

const
  kSCOFlowControlDisabled = $00;
  kSCOFlowControlEnabled = $01;
const
  kHostControllerToHostFlowControlOff = $00;
  kHCIACLDataPacketsOnHCISCODataPacketsOff = $01;
  kHCIACLDataPacketsOffHCISCODataPacketsOn = $02;
  kHCIACLDataPacketsOnHCISCODataPacketsOn = $03;

type
  BluetoothHCITransmitPowerLevel = SInt8;
  BluetoothHCITransmitPowerLevelPtr = ^BluetoothHCITransmitPowerLevel;
  BluetoothHCITransmitPowerLevelType = cuint8;
  BluetoothHCITransmitPowerLevelTypePtr = ^BluetoothHCITransmitPowerLevelType;

const
  kReadCurrentTransmitPowerLevel = $00;
  kReadMaximumTransmitPowerLevel = $01;

type
  BluetoothHCIAFHChannelAssessmentMode = cuint8;
  BluetoothHCIAFHChannelAssessmentModePtr = ^BluetoothHCIAFHChannelAssessmentMode;

const
  kAFHChannelAssessmentModeDisabled = $00;
  kAFHChannelAssessmentModeEnabled = $01;

type
  BluetoothHCITransmitPowerLevelInfo = record
    handle: BluetoothConnectionHandle;
    level: BluetoothHCITransmitPowerLevel;
  end;
type
  BluetoothHCITransmitPowerLevelInfoPtr = ^BluetoothHCITransmitPowerLevelInfo;

type
  BluetoothHCINumBroadcastRetransmissions = cuint8;
  BluetoothHCINumBroadcastRetransmissionsPtr = ^BluetoothHCINumBroadcastRetransmissions;
  BluetoothHCIHoldModeActivity = cuint8;
  BluetoothHCIHoldModeActivityPtr = ^BluetoothHCIHoldModeActivity;

const
  kMaintainCurrentPowerState = $00;
  kSuspendPageScan = $01;
  kSuspendInquiryScan = $02;
  kSuspendPeriodicInquiries = $03;

type
  BluetoothHCIAuthenticationEnable = cuint8;
  BluetoothHCIAuthenticationEnablePtr = ^BluetoothHCIAuthenticationEnable;

const
  kAuthenticationDisabled = $00;
  kAuthenticationEnabled = $01;

type
  BluetoothHCIEncryptionMode = cuint8;
  BluetoothHCIEncryptionModePtr = ^BluetoothHCIEncryptionMode;

const
  kEncryptionDisabled = $00;
  kEncryptionOnlyForPointToPointPackets = $01;
  kEncryptionForBothPointToPointAndBroadcastPackets = $02;

type
  BluetoothHCIAutomaticFlushTimeout = cuint16;
  BluetoothHCIAutomaticFlushTimeoutPtr = ^BluetoothHCIAutomaticFlushTimeout;

type
  BluetoothHCIAutomaticFlushTimeoutInfo = record
    handle: BluetoothConnectionHandle;
    timeout: BluetoothHCIAutomaticFlushTimeout;
  end;
type
  BluetoothHCIAutomaticFlushTimeoutInfoPtr = ^BluetoothHCIAutomaticFlushTimeoutInfo;

const
  kInfoStringMaxLength = 35;

type
  BluetoothTransportInfo = record
    productID: cuint32;
    vendorID: cuint32;
    type_: cuint32;
    productName:array[0..(kInfoStringMaxLength)-1] of char;
    vendorName:array[0..(kInfoStringMaxLength)-1] of char;
    totalDataBytesSent: cuint64;
    totalSCOBytesSent: cuint64;
    totalDataBytesReceived: cuint64;
    totalSCOBytesReceived: cuint64;
  end;
type
  BluetoothTransportInfoPtr = ^BluetoothTransportInfo;

const
  kBluetoothTransportTypeUSB = $01;
  kBluetoothTransportTypePCCard = $02;
  kBluetoothTransportTypePCICard = $03;
  kBluetoothTransportTypeUART = $04;

type
  BluetoothHCIInquiryResult = record
    deviceAddress: BluetoothDeviceAddress;
    pageScanRepetitionMode: BluetoothPageScanRepetitionMode;
    pageScanPeriodMode: BluetoothHCIPageScanPeriodMode;
    pageScanMode: BluetoothHCIPageScanMode;
    classOfDevice: BluetoothClassOfDevice;
    clockOffset: BluetoothClockOffset;
  end;
type
  BluetoothHCIInquiryResultPtr = ^BluetoothHCIInquiryResult;

const
  kBluetoothHCIInquiryResultsMaxResults = 50;

type
  BluetoothHCIInquiryResults = record
    results:array[0..(kBluetoothHCIInquiryResultsMaxResults)-1] of BluetoothHCIInquiryResult;
    count: IOItemCount;
  end;
type
  BluetoothHCIInquiryResultsPtr = ^BluetoothHCIInquiryResults;
type
  BluetoothHCIInquiryWithRSSIResult = record
    deviceAddress: BluetoothDeviceAddress;
    pageScanRepetitionMode: BluetoothPageScanRepetitionMode;
    reserved: cuint8;
    classOfDevice: BluetoothClassOfDevice;
    clockOffset: BluetoothClockOffset;
    RSSIValue: BluetoothHCIRSSIValue;
  end;
type
  BluetoothHCIInquiryWithRSSIResultPtr = ^BluetoothHCIInquiryWithRSSIResult;
type
  BluetoothHCIInquiryWithRSSIResults = record
    results:array[0..49] of BluetoothHCIInquiryWithRSSIResult;
    count: IOItemCount;
  end;
type
  BluetoothHCIInquiryWithRSSIResultsPtr = ^BluetoothHCIInquiryWithRSSIResults;

type
  BluetoothHCIFECRequired = cuint8;
  BluetoothHCIFECRequiredPtr = ^BluetoothHCIFECRequired;

const
  kBluetoothHCIFECRequired = $00;
  kBluetoothHCIFECNotRequired = $01;

type
  BluetoothHCIInquiryMode = cuint8;
  BluetoothHCIInquiryModePtr = ^BluetoothHCIInquiryMode;

const
  kBluetoothHCIInquiryModeResultFormatStandard = $00;
  kBluetoothHCIInquiryModeResultFormatWithRSSI = $01;
  kBluetoothHCIInquiryModeResultFormatWithRSSIOrExtendedInquiryResultFormat = $02;

type
  BluetoothHCIInquiryScanType = cuint8;
  BluetoothHCIInquiryScanTypePtr = ^BluetoothHCIInquiryScanType;

const
  kBluetoothHCIInquiryScanTypeStandard = $00;
  kBluetoothHCIInquiryScanTypeInterlaced = $01;
  kBluetoothHCIInquiryScanTypeReservedStart = $02;
  kBluetoothHCIInquiryScanTypeReservedEnd = $FF;

type
  BluetoothHCIExtendedInquiryResponseDataType = cuint8;
  BluetoothHCIExtendedInquiryResponseDataTypePtr = ^BluetoothHCIExtendedInquiryResponseDataType;

type
  BluetoothHCIExtendedInquiryResponse = record
    data:array[0..239] of cuint8;
  end;
type
  BluetoothHCIExtendedInquiryResponsePtr = ^BluetoothHCIExtendedInquiryResponse;
type
  BluetoothHCIReadExtendedInquiryResponseResults = record
    outFECRequired: BluetoothHCIFECRequired;
    extendedInquiryResponse: BluetoothHCIExtendedInquiryResponse;
  end;
type
  BluetoothHCIReadExtendedInquiryResponseResultsPtr = ^BluetoothHCIReadExtendedInquiryResponseResults;
type
  BluetoothHCIExtendedInquiryResult = record
    numberOfReponses: cuint8;
    deviceAddress: BluetoothDeviceAddress;
    pageScanRepetitionMode: BluetoothPageScanRepetitionMode;
    reserved: cuint8;
    classOfDevice: BluetoothClassOfDevice;
    clockOffset: BluetoothClockOffset;
    RSSIValue: BluetoothHCIRSSIValue;
    extendedInquiryResponse: BluetoothHCIExtendedInquiryResponse;
  end;
type
  BluetoothHCIExtendedInquiryResultPtr = ^BluetoothHCIExtendedInquiryResult;
type
  BluetoothHCIReadLMPHandleResults = record
    handle: BluetoothConnectionHandle;
    lmp_handle: BluetoothLMPHandle;
    reserved: cuint32;
  end;
type
  BluetoothHCIReadLMPHandleResultsPtr = ^BluetoothHCIReadLMPHandleResults;

type
  BluetoothHCISimplePairingMode = cuint8;
  BluetoothHCISimplePairingModePtr = ^BluetoothHCISimplePairingMode;

const
  kBluetoothHCISimplePairingModeNotSet = $00;
  kBluetoothHCISimplePairingModeEnabled = $01;

type
  BluetoothSimplePairingDebugMode = cuint8;
  BluetoothSimplePairingDebugModePtr = ^BluetoothSimplePairingDebugMode;

const
  kBluetoothHCISimplePairingDebugModeDisabled = $00;
  kBluetoothHCISimplePairingDebugModeEnabled = $01;

type
  BluetoothHCISimplePairingOOBData = record
    data:array[0..15] of cuint8;
  end;
type
  BluetoothHCISimplePairingOOBDataPtr = ^BluetoothHCISimplePairingOOBData;
type
  BluetoothHCIReadLocalOOBDataResults = record
    hash: BluetoothHCISimplePairingOOBData;
    randomizer: BluetoothHCISimplePairingOOBData;
  end;
type
  BluetoothHCIReadLocalOOBDataResultsPtr = ^BluetoothHCIReadLocalOOBDataResults;

type
  BluetoothIOCapability = cuint8;
  BluetoothIOCapabilityPtr = ^BluetoothIOCapability;

const
  kBluetoothCapabilityTypeDisplayOnly = $00;
  kBluetoothCapabilityTypeDisplayYesNo = $01;
  kBluetoothCapabilityTypeKeyboardOnly = $02;
  kBluetoothCapabilityTypeNoInputNoOutput = $03;

type
  BluetoothOOBDataPresence = cuint8;
  BluetoothOOBDataPresencePtr = ^BluetoothOOBDataPresence;

const
  kBluetoothOOBAuthenticationDataNotPresent = $00;
  kBluetoothOOBAuthenticationDataFromRemoteDevicePresent = $01;

type
  BluetoothAuthenticationRequirements = cuint8;
  BluetoothAuthenticationRequirementsPtr = ^BluetoothAuthenticationRequirements;

const
  kBluetoothAuthenticationRequirementsMITMProtectionNotRequired = $00;
  kBluetoothAuthenticationRequirementsMITMProtectionRequired = $01;
  kBluetoothAuthenticationRequirementsMITMProtectionNotRequiredNoBonding = $00;
  kBluetoothAuthenticationRequirementsMITMProtectionRequiredNoBonding = $01;
  kBluetoothAuthenticationRequirementsMITMProtectionNotRequiredDedicatedBonding = $02;
  kBluetoothAuthenticationRequirementsMITMProtectionRequiredDedicatedBonding = $03;
  kBluetoothAuthenticationRequirementsMITMProtectionNotRequiredGeneralBonding = $04;
  kBluetoothAuthenticationRequirementsMITMProtectionRequiredGeneralBonding = $05;

type
  BluetoothIOCapabilityResponse = record
    deviceAddress: BluetoothDeviceAddress;
    ioCapability: BluetoothIOCapability;
    OOBDataPresence: BluetoothOOBDataPresence;
    authenticationRequirements: BluetoothAuthenticationRequirements;
  end;
type
  BluetoothIOCapabilityResponsePtr = ^BluetoothIOCapabilityResponse;

type
  BluetoothPasskey = cuint32;
  BluetoothPasskeyPtr = ^BluetoothPasskey;

type
  BluetoothUserPasskeyNotification = record
    deviceAddress: BluetoothDeviceAddress;
    passkey: BluetoothPasskey;
  end;
type
  BluetoothUserPasskeyNotificationPtr = ^BluetoothUserPasskeyNotification;

type
  BluetoothKeypressNotificationType = cuint8;
  BluetoothKeypressNotificationTypePtr = ^BluetoothKeypressNotificationType;

const
  kBluetoothKeypressNotificationTypePasskeyEntryStarted = 0;
  kBluetoothKeypressNotificationTypePasskeyDigitEntered = 1;
  kBluetoothKeypressNotificationTypePasskeyDigitErased = 2;
  kBluetoothKeypressNotificationTypePasskeyCleared = 3;
  kBluetoothKeypressNotificationTypePasskeyEntryCompleted = 4;

type
  BluetoothKeypressNotification = record
    deviceAddress: BluetoothDeviceAddress;
    notificationType: BluetoothKeypressNotificationType;
  end;
type
  BluetoothKeypressNotificationPtr = ^BluetoothKeypressNotification;
type
  BluetoothRemoteHostSupportedFeaturesNotification = record
    deviceAddress: BluetoothDeviceAddress;
    hostSupportedFeatures: BluetoothHCISupportedFeatures;
  end;
type
  BluetoothRemoteHostSupportedFeaturesNotificationPtr = ^BluetoothRemoteHostSupportedFeaturesNotification;

type
  TransmissionPower = SInt8;
  TransmissionPowerPtr = ^TransmissionPower;

type
  BluetoothAFHHostChannelClassification = record
    data:array[0..9] of cuint8;
  end;
type
  BluetoothAFHHostChannelClassificationPtr = ^BluetoothAFHHostChannelClassification;

type
  BluetoothAFHMode = cuint8;
  BluetoothAFHModePtr = ^BluetoothAFHMode;

type
  BluetoothAFHResults = record
    handle: BluetoothConnectionHandle;
    mode: BluetoothAFHMode;
    afhMap:array[0..9] of cuint8;
  end;
type
  BluetoothAFHResultsPtr = ^BluetoothAFHResults;

type
  BluetoothNumericValue = cuint32;
  BluetoothNumericValuePtr = ^BluetoothNumericValue;

type
  BluetoothUserConfirmationRequest = record
    deviceAddress: BluetoothDeviceAddress;
    numericValue: BluetoothNumericValue;
  end;
type
  BluetoothUserConfirmationRequestPtr = ^BluetoothUserConfirmationRequest;
type
  BluetoothHCIEventSimplePairingCompleteResults = record
    deviceAddress: BluetoothDeviceAddress;
  end;
type
  BluetoothHCIEventSimplePairingCompleteResultsPtr = ^BluetoothHCIEventSimplePairingCompleteResults;

const
  kBluetoothHCICommandPacketHeaderSize = 3;
  kBluetoothHCICommandPacketMaxDataSize = 255;
  kBluetoothHCIMaxCommandPacketSize = kBluetoothHCICommandPacketHeaderSize + kBluetoothHCICommandPacketMaxDataSize;
  kBluetoothHCIEventPacketHeaderSize = 2;
  kBluetoothHCIEventPacketMaxDataSize = 255;
  kBluetoothHCIMaxEventPacketSize = kBluetoothHCIEventPacketHeaderSize + kBluetoothHCIEventPacketMaxDataSize;
  kBluetoothHCIDataPacketHeaderSize = 4;
  kBluetoothHCIDataPacketMaxDataSize = 65535;
  kBluetoothHCIMaxDataPacketSize = kBluetoothHCIDataPacketHeaderSize + kBluetoothHCIDataPacketMaxDataSize;

type
  BluetoothHCIEventCode = cuint8;
  BluetoothHCIEventCodePtr = ^BluetoothHCIEventCode;
  BluetoothLinkType = cuint8;
  BluetoothLinkTypePtr = ^BluetoothLinkType;

const
  kBluetoothSCOConnection = 0;
  kBluetoothACLConnection = 1;
  kBluetoothESCOConnection = 2;
  kBluetoothLinkTypeNone = $ff;

type
  BluetoothHCIContentFormat = cuint16;
  BluetoothHCIContentFormatPtr = ^BluetoothHCIContentFormat;
  BluetoothHCIVoiceSetting = cuint16;
  BluetoothHCIVoiceSettingPtr = ^BluetoothHCIVoiceSetting;

const
  kBluetoothVoiceSettingInputCodingMask = $300;
  kBluetoothVoiceSettingInputCodingLinearInputCoding = $000;
  kBluetoothVoiceSettingInputCodingULawInputCoding = $100;
  kBluetoothVoiceSettingInputCodingALawInputCoding = $200;
const
  kBluetoothVoiceSettingInputDataFormatMask = $0C0;
  kBluetoothVoiceSettingInputDataFormat1sComplement = $000;
  kBluetoothVoiceSettingInputDataFormat2sComplement = $040;
  kBluetoothVoiceSettingInputDataFormatSignMagnitude = $080;
  kBluetoothVoiceSettingInputDataFormatUnsigned = $0C0;
const
  kBluetoothVoiceSettingInputSampleSizeMask = $020;
  kBluetoothVoiceSettingInputSampleSize8Bit = $000;
  kBluetoothVoiceSettingInputSampleSize16Bit = $020;
const
  kBluetoothVoiceSettingPCMBitPositionMask = $01C;
const
  kBluetoothVoiceSettingAirCodingFormatMask = $003;
  kBluetoothVoiceSettingAirCodingFormatCVSD = $000;
  kBluetoothVoiceSettingAirCodingFormatULaw = $001;
  kBluetoothVoiceSettingAirCodingFormatALaw = $002;
  kBluetoothVoiceSettingAirCodingFormatTransparentData = $003;

type
  BluetoothHCISupportedIAC = cuint8;
  BluetoothHCISupportedIACPtr = ^BluetoothHCISupportedIAC;
  BluetoothHCITransmitBandwidth = cuint32;
  BluetoothHCITransmitBandwidthPtr = ^BluetoothHCITransmitBandwidth;
  BluetoothHCIReceiveBandwidth = cuint32;
  BluetoothHCIReceiveBandwidthPtr = ^BluetoothHCIReceiveBandwidth;
  BluetoothHCIMaxLatency = cuint16;
  BluetoothHCIMaxLatencyPtr = ^BluetoothHCIMaxLatency;
  BluetoothHCIRetransmissionEffort = cuint8;
  BluetoothHCIRetransmissionEffortPtr = ^BluetoothHCIRetransmissionEffort;

const
  kHCIRetransmissionEffortTypeNone = $00;
  kHCIRetransmissionEffortTypeAtLeastOneAndOptimizeForPower = $01;
  kHCIRetransmissionEffortTypeAtLeastOneAndOptimizeLinkQuality = $02;
  kHCIRetransmissionEffortTypeDontCare = $FF;

type
  BluetoothAirMode = cuint8;
  BluetoothAirModePtr = ^BluetoothAirMode;

const
  kBluetoothAirModeULawLog = $00;
  kBluetoothAirModeALawLog = $01;
  kBluetoothAirModeCVSD = $02;
  kBluetoothAirModeTransparentData = $03;

type
  BluetoothSynchronousConnectionInfo = record
    transmitBandWidth: BluetoothHCITransmitBandwidth;
    receiveBandWidth: BluetoothHCIReceiveBandwidth;
    maxLatency: BluetoothHCIMaxLatency;
    voiceSetting: BluetoothHCIVoiceSetting;
    retransmissionEffort: BluetoothHCIRetransmissionEffort;
    packetType: BluetoothPacketType;
  end;
type
  BluetoothSynchronousConnectionInfoPtr = ^BluetoothSynchronousConnectionInfo;
type
  BluetoothHCIEventSynchronousConnectionCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
    deviceAddress: BluetoothDeviceAddress;
    linkType: BluetoothLinkType;
    transmissionInterval: cuint8;
    retransmissionWindow: cuint8;
    receivePacketLength: cuint16;
    transmitPacketLength: cuint16;
    airMode: BluetoothAirMode;
  end;
type
  BluetoothHCIEventSynchronousConnectionCompleteResultsPtr = ^BluetoothHCIEventSynchronousConnectionCompleteResults;
type
  BluetoothHCIEventSynchronousConnectionChangedResults = record
    connectionHandle: BluetoothConnectionHandle;
    transmissionInterval: cuint8;
    retransmissionWindow: cuint8;
    receivePacketLength: cuint16;
    transmitPacketLength: cuint16;
  end;
type
  BluetoothHCIEventSynchronousConnectionChangedResultsPtr = ^BluetoothHCIEventSynchronousConnectionChangedResults;

type
  BluetoothHCIStatus = cuint8;
  BluetoothHCIStatusPtr = ^BluetoothHCIStatus;
  BluetoothHCIEventStatus = cuint8;
  BluetoothHCIEventStatusPtr = ^BluetoothHCIEventStatus;

const
  kBluetoothHCIEventInquiryComplete = $01;
  kBluetoothHCIEventInquiryResult = $02;
  kBluetoothHCIEventConnectionComplete = $03;
  kBluetoothHCIEventConnectionRequest = $04;
  kBluetoothHCIEventDisconnectionComplete = $05;
  kBluetoothHCIEventAuthenticationComplete = $06;
  kBluetoothHCIEventRemoteNameRequestComplete = $07;
  kBluetoothHCIEventEncryptionChange = $08;
  kBluetoothHCIEventChangeConnectionLinkKeyComplete = $09;
  kBluetoothHCIEventMasterLinkKeyComplete = $0A;
  kBluetoothHCIEventReadRemoteSupportedFeaturesComplete = $0B;
  kBluetoothHCIEventReadRemoteVersionInformationComplete = $0C;
  kBluetoothHCIEventQoSSetupComplete = $0D;
  kBluetoothHCIEventCommandComplete = $0E;
  kBluetoothHCIEventCommandStatus = $0F;
  kBluetoothHCIEventHardwareError = $10;
  kBluetoothHCIEventFlushOccurred = $11;
  kBluetoothHCIEventRoleChange = $12;
  kBluetoothHCIEventNumberOfCompletedPackets = $13;
  kBluetoothHCIEventModeChange = $14;
  kBluetoothHCIEventReturnLinkKeys = $15;
  kBluetoothHCIEventPINCodeRequest = $16;
  kBluetoothHCIEventLinkKeyRequest = $17;
  kBluetoothHCIEventLinkKeyNotification = $18;
  kBluetoothHCIEventLoopbackCommand = $19;
  kBluetoothHCIEventDataBufferOverflow = $1A;
  kBluetoothHCIEventMaxSlotsChange = $1B;
  kBluetoothHCIEventReadClockOffsetComplete = $1C;
  kBluetoothHCIEventConnectionPacketType = $1D;
  kBluetoothHCIEventQoSViolation = $1E;
  kBluetoothHCIEventPageScanModeChange = $1F;
  kBluetoothHCIEventPageScanRepetitionModeChange = $20;
  kBluetoothHCIEventFlowSpecificationComplete = $21;
  kBluetoothHCIEventInquiryResultWithRSSI = $22;
  kBluetoothHCIEventReadRemoteExtendedFeaturesComplete = $23;
  kBluetoothHCIEventSynchronousConnectionComplete = $2C;
  kBluetoothHCIEventSynchronousConnectionChanged = $2D;
  kBluetoothHCIEventSniffSubrating = $2E;
  kBluetoothHCIEventExtendedInquiryResult = $2F;
  kBluetoothHCIEventEncryptionKeyRefreshComplete = $30;
  kBluetoothHCIEventIOCapabilityRequest = $31;
  kBluetoothHCIEventIOCapabilityResponse = $32;
  kBluetoothHCIEventUserConfirmationRequest = $33;
  kBluetoothHCIEventUserPasskeyRequest = $34;
  kBluetoothHCIEventRemoteOOBDataRequest = $35;
  kBluetoothHCIEventSimplePairingComplete = $36;
  kBluetoothHCIEventLinkSupervisionTimeoutChanged = $38;
  kBluetoothHCIEventEnhancedFlushComplete = $39;
  kBluetoothHCIEventUserPasskeyNotification = $3B;
  kBluetoothHCIEventKeypressNotification = $3C;
  kBluetoothHCIEventRemoteHostSupportedFeaturesNotification = $3D;
  kBluetoothHCIEventLEMetaEvent = $3E;
  kBluetoothHCISubEventLEConnectionComplete = $01;
  kBluetoothHCISubEventLEAdvertisingReport = $02;
  kBluetoothHCISubEventLEConnectionUpdateComplete = $03;
  kBluetoothHCISubEventLEReadRemoteUsedFeaturesComplete = $04;
  kBluetoothHCISubEventLELongTermKeyRequest = $05;
  kBluetoothHCIEventPhysicalLinkComplete = $40;
  kBluetoothHCIEventChannelSelected = $41;
  kBluetoothHCIEventDisconnectionPhysicalLinkComplete = $42;
  kBluetoothHCIEventPhysicalLinkLossEarlyWarning = $43;
  kBluetoothHCIEventPhysicalLinkRecovery = $44;
  kBluetoothHCIEventLogicalLinkComplete = $45;
  kBluetoothHCIEventDisconnectionLogicalLinkComplete = $46;
  kBluetoothHCIEventFlowSpecModifyComplete = $47;
  kBluetoothHCIEventNumberOfCompletedDataBlocks = $48;
  kBluetoothHCIEventShortRangeModeChangeComplete = $4C;
  kBluetoothHCIEventAMPStatusChange = $4D;
  kBluetoothHCIEventAMPStartTest = $49;
  kBluetoothHCIEventAMPTestEnd = $4A;
  kBluetoothHCIEventAMPReceiverReport = $4B;
  kBluetoothHCIEventLogoTesting = $FE;
  kBluetoothHCIEventVendorSpecific = $FF;

type
  BluetoothHCIEventMask = cuint64;
  BluetoothHCIEventMaskPtr = ^BluetoothHCIEventMask;

const
  kBluetoothHCIEventMaskLEDefault64Bit = $000000000000001F;
  kBluetoothHCIEventMaskDefault64Bit = $00001FFFFFFFFFFF;
  kBluetoothHCIEventMaskAll64Bit = $FFFFFFFFFFFFFFFF;
  kBluetoothHCIEventMaskFlowSpecificationCompleteEvent = $0000000100000000;
  kBluetoothHCIEventMaskInquiryResultWithRSSIEvent = $0000000200000000;
  kBluetoothHCIEventMaskReadRemoteExtendedFeaturesCompleteEvent = $0000000400000000;
  kBluetoothHCIEventMaskSynchronousConnectionCompleteEvent = $0000080000000000;
  kBluetoothHCIEventMaskSynchronousConnectionChangedEvent = $0000100000000000;
  kBluetoothHCIEventMaskSniffSubratingEvent = $0000200000000000;
  kBluetoothHCIEventMaskExtendedInquiryResultEvent = $0000400000000000;
  kBluetoothHCIEventMaskEncryptionChangeEvent = $0000000000000080;
  kBluetoothHCIEventMaskEncryptionKeyRefreshCompleteEvent = $0000800000000000;
  kBluetoothHCIEventMaskLinkSupervisionTimeoutChangedEvent = $0080000000000000;
  kBluetoothHCIEventMaskEnhancedFlushCompleteEvent = $0100000000000000;
  kBluetoothHCIEventMaskIOCapabilityRequestEvent = $0001000000000000;
  kBluetoothHCIEventMaskIOCapabilityRequestReplyEvent = $0002000000000000;
  kBluetoothHCIEventMaskUserConfirmationRequestEvent = $0004000000000000;
  kBluetoothHCIEventMaskUserPasskeyRequestEvent = $0008000000000000;
  kBluetoothHCIEventMaskRemoteOOBDataRequestEvent = $0010000000000000;
  kBluetoothHCIEventMaskSimplePairingCompleteEvent = $0020000000000000;
  kBluetoothHCIEvnetMaskLinkSupervisionTimeoutChangedEvent = $0080000000000000;
  kBluetoothHCIEvnetMaskEnhancedFlushCompleteEvent = $0100000000000000;
  kBluetoothHCIEventMaskUserPasskeyNotificationEvent = $0400000000000000;
  kBluetoothHCIEventMaskKeypressNotificationEvent = $0800000000000000;
  kBluetoothHCIEventMaskRemoteHostSupportedFeaturesNotificationEvent = $1000000000000000;
  kBluetoothHCIEventMaskLEMetaEvent = $2000000000000000;

const
  kBluetoothHCIEventMaskNone = $00000000;
  kBluetoothHCIEventMaskInquiryComplete = $00000001;
  kBluetoothHCIEventMaskInquiryResult = $00000002;
  kBluetoothHCIEventMaskConnectionComplete = $00000004;
  kBluetoothHCIEventMaskConnectionRequest = $00000008;
  kBluetoothHCIEventMaskDisconnectionComplete = $00000010;
  kBluetoothHCIEventMaskAuthenticationComplete = $00000020;
  kBluetoothHCIEventMaskRemoteNameRequestComplete = $00000040;
  kBluetoothHCIEventMaskEncryptionChange = $00000080;
  kBluetoothHCIEventMaskChangeConnectionLinkKeyComplete = $00000100;
  kBluetoothHCIEventMaskMasterLinkKeyComplete = $00000200;
  kBluetoothHCIEventMaskReadRemoteSupportedFeaturesComplete = $00000400;
  kBluetoothHCIEventMaskReadRemoteVersionInformationComplete = $00000800;
  kBluetoothHCIEventMaskQoSSetupComplete = $00001000;
  kBluetoothHCIEventMaskCommandComplete = $00002000;
  kBluetoothHCIEventMaskCommandStatus = $00004000;
  kBluetoothHCIEventMaskHardwareError = $00008000;
  kBluetoothHCIEventMaskFlushOccurred = $00010000;
  kBluetoothHCIEventMaskRoleChange = $00020000;
  kBluetoothHCIEventMaskNumberOfCompletedPackets = $00040000;
  kBluetoothHCIEventMaskModeChange = $00080000;
  kBluetoothHCIEventMaskReturnLinkKeys = $00100000;
  kBluetoothHCIEventMaskPINCodeRequest = $00200000;
  kBluetoothHCIEventMaskLinkKeyRequest = $00400000;
  kBluetoothHCIEventMaskLinkKeyNotification = $00800000;
  kBluetoothHCIEventMaskLoopbackCommand = $01000000;
  kBluetoothHCIEventMaskDataBufferOverflow = $02000000;
  kBluetoothHCIEventMaskMaxSlotsChange = $04000000;
  kBluetoothHCIEventMaskReadClockOffsetComplete = $08000000;
  kBluetoothHCIEventMaskConnectionPacketTypeChanged = $10000000;
  kBluetoothHCIEventMaskQoSViolation = $20000000;
  kBluetoothHCIEventMaskPageScanModeChange = $40000000;
  kBluetoothHCIEventMaskPageScanRepetitionModeChange = $80000000;
  kBluetoothHCIEventMaskAll = $FFFFFFFF;
  kBluetoothHCIEventMaskDefault = kBluetoothHCIEventMaskAll;

type
  BluetoothHCIEventConnectionCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
    deviceAddress: BluetoothDeviceAddress;
    linkType: BluetoothLinkType;
    encryptionMode: BluetoothHCIEncryptionMode;
  end;
type
  BluetoothHCIEventConnectionCompleteResultsPtr = ^BluetoothHCIEventConnectionCompleteResults;
type
  BluetoothHCIEventLEConnectionCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
    role: cuint8;
    peerAddressType: cuint8;
    peerAddress: BluetoothDeviceAddress;
    connInterval: cuint16;
    connLatency: cuint16;
    supervisionTimeout: cuint16;
    masterClockAccuracy: cuint8;
  end;
type
  BluetoothHCIEventLEConnectionCompleteResultsPtr = ^BluetoothHCIEventLEConnectionCompleteResults;
type
  BluetoothHCIEventLEConnectionUpdateCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
    connInterval: cuint16;
    connLatency: cuint16;
    supervisionTimeout: cuint16;
  end;
type
  BluetoothHCIEventLEConnectionUpdateCompleteResultsPtr = ^BluetoothHCIEventLEConnectionUpdateCompleteResults;
type
  BluetoothHCIEventDisconnectionCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
    reason: BluetoothReasonCode;
  end;
type
  BluetoothHCIEventDisconnectionCompleteResultsPtr = ^BluetoothHCIEventDisconnectionCompleteResults;
type
  BluetoothHCIEventReadSupportedFeaturesResults = record
    connectionHandle: BluetoothConnectionHandle;
    supportedFeatures: BluetoothHCISupportedFeatures;
  end;
type
  BluetoothHCIEventReadSupportedFeaturesResultsPtr = ^BluetoothHCIEventReadSupportedFeaturesResults;
type
  BluetoothHCIEventReadExtendedFeaturesResults = record
    connectionHandle: BluetoothConnectionHandle;
    supportedFeaturesInfo: BluetoothHCIExtendedFeaturesInfo;
  end;
type
  BluetoothHCIEventReadExtendedFeaturesResultsPtr = ^BluetoothHCIEventReadExtendedFeaturesResults;
type
  BluetoothHCIEventReadRemoteVersionInfoResults = record
    connectionHandle: BluetoothConnectionHandle;
    lmpVersion: BluetoothLMPVersion;
    manufacturerName: BluetoothManufacturerName;
    lmpSubversion: BluetoothLMPSubversion;
  end;
type
  BluetoothHCIEventReadRemoteVersionInfoResultsPtr = ^BluetoothHCIEventReadRemoteVersionInfoResults;
type
  BluetoothHCIEventRemoteNameRequestResults = record
    deviceAddress: BluetoothDeviceAddress;
    deviceName: BluetoothDeviceName;
  end;
type
  BluetoothHCIEventRemoteNameRequestResultsPtr = ^BluetoothHCIEventRemoteNameRequestResults;
type
  BluetoothHCIEventReadClockOffsetResults = record
    connectionHandle: BluetoothConnectionHandle;
    clockOffset: BluetoothClockOffset;
  end;
type
  BluetoothHCIEventReadClockOffsetResultsPtr = ^BluetoothHCIEventReadClockOffsetResults;
type
  BluetoothHCIEventConnectionRequestResults = record
    deviceAddress: BluetoothDeviceAddress;
    classOfDevice: BluetoothClassOfDevice;
    linkType: BluetoothLinkType;
  end;
type
  BluetoothHCIEventConnectionRequestResultsPtr = ^BluetoothHCIEventConnectionRequestResults;
type
  BluetoothHCIEventLinkKeyNotificationResults = record
    deviceAddress: BluetoothDeviceAddress;
    linkKey: BluetoothKey;
    keyType: BluetoothKeyType;
  end;
type
  BluetoothHCIEventLinkKeyNotificationResultsPtr = ^BluetoothHCIEventLinkKeyNotificationResults;
type
  BluetoothHCIEventMaxSlotsChangeResults = record
    connectionHandle: BluetoothConnectionHandle;
    maxSlots: BluetoothMaxSlots;
  end;
type
  BluetoothHCIEventMaxSlotsChangeResultsPtr = ^BluetoothHCIEventMaxSlotsChangeResults;
type
  BluetoothHCIEventModeChangeResults = record
    connectionHandle: BluetoothConnectionHandle;
    mode: BluetoothHCIConnectionMode;
    modeInterval: BluetoothHCIModeInterval;
  end;
type
  BluetoothHCIEventModeChangeResultsPtr = ^BluetoothHCIEventModeChangeResults;
type
  BluetoothHCIEventReturnLinkKeysResults = record
    numLinkKeys: cuint8;
    linkKeys: array[0..0] of record
      deviceAddress: BluetoothDeviceAddress;
      linkKey: BluetoothKey;
    end;
  end;
type
  BluetoothHCIEventReturnLinkKeysResultsPtr = ^BluetoothHCIEventReturnLinkKeysResults;
type
  BluetoothHCIEventAuthenticationCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
  end;
type
  BluetoothHCIEventAuthenticationCompleteResultsPtr = ^BluetoothHCIEventAuthenticationCompleteResults;
type
  BluetoothHCIEventEncryptionChangeResults = record
    connectionHandle: BluetoothConnectionHandle;
    enable: BluetoothEncryptionEnable;
  end;
type
  BluetoothHCIEventEncryptionChangeResultsPtr = ^BluetoothHCIEventEncryptionChangeResults;
type
  BluetoothHCIEventChangeConnectionLinkKeyCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
  end;
type
  BluetoothHCIEventChangeConnectionLinkKeyCompleteResultsPtr = ^BluetoothHCIEventChangeConnectionLinkKeyCompleteResults;
type
  BluetoothHCIEventMasterLinkKeyCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
    keyFlag: BluetoothKeyFlag;
  end;
type
  BluetoothHCIEventMasterLinkKeyCompleteResultsPtr = ^BluetoothHCIEventMasterLinkKeyCompleteResults;
type
  BluetoothHCIEventQoSSetupCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
    setupParams: BluetoothHCIQualityOfServiceSetupParams;
  end;
type
  BluetoothHCIEventQoSSetupCompleteResultsPtr = ^BluetoothHCIEventQoSSetupCompleteResults;
type
  BluetoothHCIEventHardwareErrorResults = record
    error: BluetoothHCIStatus;
  end;
type
  BluetoothHCIEventHardwareErrorResultsPtr = ^BluetoothHCIEventHardwareErrorResults;
type
  BluetoothHCIEventFlushOccurredResults = record
    connectionHandle: BluetoothConnectionHandle;
  end;
type
  BluetoothHCIEventFlushOccurredResultsPtr = ^BluetoothHCIEventFlushOccurredResults;
type
  BluetoothHCIEventRoleChangeResults = record
    connectionHandle: BluetoothConnectionHandle;
    deviceAddress: BluetoothDeviceAddress;
    role: BluetoothRole;
  end;
type
  BluetoothHCIEventRoleChangeResultsPtr = ^BluetoothHCIEventRoleChangeResults;
type
  BluetoothHCIEventDataBufferOverflowResults = record
    linkType: BluetoothLinkType;
  end;
type
  BluetoothHCIEventDataBufferOverflowResultsPtr = ^BluetoothHCIEventDataBufferOverflowResults;
type
  BluetoothHCIEventConnectionPacketTypeResults = record
    connectionHandle: BluetoothConnectionHandle;
    packetType: BluetoothPacketType;
  end;
type
  BluetoothHCIEventConnectionPacketTypeResultsPtr = ^BluetoothHCIEventConnectionPacketTypeResults;
type
  BluetoothHCIEventReadRemoteSupportedFeaturesResults = record
    error: BluetoothHCIStatus;
    connectionHandle: BluetoothConnectionHandle;
    lmpFeatures: BluetoothHCISupportedFeatures;
  end;
type
  BluetoothHCIEventReadRemoteSupportedFeaturesResultsPtr = ^BluetoothHCIEventReadRemoteSupportedFeaturesResults;
type
  BluetoothHCIEventReadRemoteExtendedFeaturesResults = record
    error: BluetoothHCIStatus;
    connectionHandle: BluetoothConnectionHandle;
    page: BluetoothHCIPageNumber;
    maxPage: BluetoothHCIPageNumber;
    lmpFeatures: BluetoothHCISupportedFeatures;
  end;
type
  BluetoothHCIEventReadRemoteExtendedFeaturesResultsPtr = ^BluetoothHCIEventReadRemoteExtendedFeaturesResults;
type
  BluetoothHCIEventQoSViolationResults = record
    connectionHandle: BluetoothConnectionHandle;
  end;
type
  BluetoothHCIEventQoSViolationResultsPtr = ^BluetoothHCIEventQoSViolationResults;
type
  BluetoothHCIEventPageScanModeChangeResults = record
    deviceAddress: BluetoothDeviceAddress;
    pageScanMode: BluetoothPageScanMode;
  end;
type
  BluetoothHCIEventPageScanModeChangeResultsPtr = ^BluetoothHCIEventPageScanModeChangeResults;
type
  BluetoothHCIEventPageScanRepetitionModeChangeResults = record
    deviceAddress: BluetoothDeviceAddress;
    pageScanRepetitionMode: BluetoothPageScanRepetitionMode;
  end;
type
  BluetoothHCIEventPageScanRepetitionModeChangeResultsPtr = ^BluetoothHCIEventPageScanRepetitionModeChangeResults;
type
  BluetoothHCIEventVendorSpecificResults = record
    length: cuint8;
    data:array[0..254] of cuint8;
  end;
type
  BluetoothHCIEventVendorSpecificResultsPtr = ^BluetoothHCIEventVendorSpecificResults;
type
  BluetoothHCIEventEncryptionKeyRefreshCompleteResults = record
    connectionHandle: BluetoothConnectionHandle;
  end;
type
  BluetoothHCIEventEncryptionKeyRefreshCompleteResultsPtr = ^BluetoothHCIEventEncryptionKeyRefreshCompleteResults;
type
  BluetoothHCIEventSniffSubratingResults = record
    connectionHandle: BluetoothConnectionHandle;
    maxTransmitLatency: cuint16;
    maxReceiveLatency: cuint16;
    minRemoteTimeout: cuint16;
    minLocalTimeout: cuint16;
  end;
type
  BluetoothHCIEventSniffSubratingResultsPtr = ^BluetoothHCIEventSniffSubratingResults;
type
  BluetoothHCIEventLEMetaResults = record
    length: cuint8;
    data:array[0..254] of cuint8;
  end;
type
  BluetoothHCIEventLEMetaResultsPtr = ^BluetoothHCIEventLEMetaResults;
type
  BluetoothHCIEventLELongTermKeyRequestResults = record
    connectionHandle: BluetoothConnectionHandle;
    randomNumber:array[0..7] of cuint8;
    ediv: cuint16;
  end;
type
  BluetoothHCIEventLELongTermKeyRequestResultsPtr = ^BluetoothHCIEventLELongTermKeyRequestResults;
type
  BluetoothHCIRequestCallbackInfo = record
    userCallback: mach_vm_address_t;
    userRefCon: mach_vm_address_t;
    internalRefCon: mach_vm_address_t;
    asyncIDRefCon: mach_vm_address_t;
    reserved: mach_vm_address_t;
  end;
type
  BluetoothHCIRequestCallbackInfoPtr = ^BluetoothHCIRequestCallbackInfo;

const
  kBluetoothHCIErrorSuccess = $00;
  kBluetoothHCIErrorUnknownHCICommand = $01;
  kBluetoothHCIErrorNoConnection = $02;
  kBluetoothHCIErrorHardwareFailure = $03;
  kBluetoothHCIErrorPageTimeout = $04;
  kBluetoothHCIErrorAuthenticationFailure = $05;
  kBluetoothHCIErrorKeyMissing = $06;
  kBluetoothHCIErrorMemoryFull = $07;
  kBluetoothHCIErrorConnectionTimeout = $08;
  kBluetoothHCIErrorMaxNumberOfConnections = $09;
  kBluetoothHCIErrorMaxNumberOfSCOConnectionsToADevice = $0A;
  kBluetoothHCIErrorACLConnectionAlreadyExists = $0B;
  kBluetoothHCIErrorCommandDisallowed = $0C;
  kBluetoothHCIErrorHostRejectedLimitedResources = $0D;
  kBluetoothHCIErrorHostRejectedSecurityReasons = $0E;
  kBluetoothHCIErrorHostRejectedRemoteDeviceIsPersonal = $0F;
  kBluetoothHCIErrorHostTimeout = $10;
  kBluetoothHCIErrorUnsupportedFeatureOrParameterValue = $11;
  kBluetoothHCIErrorInvalidHCICommandParameters = $12;
  kBluetoothHCIErrorOtherEndTerminatedConnectionUserEnded = $13;
  kBluetoothHCIErrorOtherEndTerminatedConnectionLowResources = $14;
  kBluetoothHCIErrorOtherEndTerminatedConnectionAboutToPowerOff = $15;
  kBluetoothHCIErrorConnectionTerminatedByLocalHost = $16;
  kBluetoothHCIErrorRepeatedAttempts = $17;
  kBluetoothHCIErrorPairingNotAllowed = $18;
  kBluetoothHCIErrorUnknownLMPPDU = $19;
  kBluetoothHCIErrorUnsupportedRemoteFeature = $1A;
  kBluetoothHCIErrorSCOOffsetRejected = $1B;
  kBluetoothHCIErrorSCOIntervalRejected = $1C;
  kBluetoothHCIErrorSCOAirModeRejected = $1D;
  kBluetoothHCIErrorInvalidLMPParameters = $1E;
  kBluetoothHCIErrorUnspecifiedError = $1F;
  kBluetoothHCIErrorUnsupportedLMPParameterValue = $20;
  kBluetoothHCIErrorRoleChangeNotAllowed = $21;
  kBluetoothHCIErrorLMPResponseTimeout = $22;
  kBluetoothHCIErrorLMPErrorTransactionCollision = $23;
  kBluetoothHCIErrorLMPPDUNotAllowed = $24;
  kBluetoothHCIErrorEncryptionModeNotAcceptable = $25;
  kBluetoothHCIErrorUnitKeyUsed = $26;
  kBluetoothHCIErrorQoSNotSupported = $27;
  kBluetoothHCIErrorInstantPassed = $28;
  kBluetoothHCIErrorPairingWithUnitKeyNotSupported = $29;
  kBluetoothHCIErrorHostRejectedUnacceptableDeviceAddress = $0F;
  kBluetoothHCIErrorDifferentTransactionCollision = $2A;
  kBluetoothHCIErrorQoSUnacceptableParameter = $2C;
  kBluetoothHCIErrorQoSRejected = $2D;
  kBluetoothHCIErrorChannelClassificationNotSupported = $2E;
  kBluetoothHCIErrorInsufficientSecurity = $2F;
  kBluetoothHCIErrorParameterOutOfMandatoryRange = $30;
  kBluetoothHCIErrorRoleSwitchPending = $31;
  kBluetoothHCIErrorReservedSlotViolation = $34;
  kBluetoothHCIErrorRoleSwitchFailed = $35;
  kBluetoothHCIErrorExtendedInquiryResponseTooLarge = $36;
  kBluetoothHCIErrorSecureSimplePairingNotSupportedByHost = $37;
  kBluetoothHCIErrorHostBusyPairing = $38;
  kBluetoothHCIErrorConnectionRejectedDueToNoSuitableChannelFound = $39;
  kBluetoothHCIErrorControllerBusy = $3A;
  kBluetoothHCIErrorUnacceptableConnectionInterval = $3B;
  kBluetoothHCIErrorDirectedAdvertisingTimeout = $3C;
  kBluetoothHCIErrorConnectionTerminatedDueToMICFailure = $3D;
  kBluetoothHCIErrorConnectionFailedToBeEstablished = $3E;
  kBluetoothHCIErrorMACConnectionFailed = $3F;
  kBluetoothHCIErrorMax = $3F;
{$endif}

{$ifdef TYPES}
const
  kBluetoothHCIPowerStateON = $01;
  kBluetoothHCIPowerStateOFF = $00;
  kBluetoothHCIPowerStateUnintialized = $FF;
type
  BluetoothHCIPowerState = clong;
const
  kBluetoothHCIErrorPowerIsOFF = kBluetoothHCIErrorMax + 1;
{$endif}

{$ifdef TYPES}
const
  kBluetoothHCITransportUSBClassCode = $E0;
  kBluetoothHCITransportUSBSubClassCode = $01;
  kBluetoothHCITransportUSBProtocolCode = $01;
{$endif}

{$ifdef TYPES}
const
  kBluetoothL2CAPTCIEventIDReserved = $00;
  kBluetoothL2CAPTCIEventIDL2CA_ConnectInd = $01;
  kBluetoothL2CAPTCIEventIDL2CA_ConfigInd = $02;
  kBluetoothL2CAPTCIEventIDL2CA_DisconnectInd = $03;
  kBluetoothL2CAPTCIEventIDL2CA_QoSViolationInd = $04;
  kBluetoothL2CAPTCIEventIDL2CA_TimeOutInd = $05;
const
  kBluetoothL2CAPTCICommandReserved = $0000;
  kBluetoothL2CAPTCICommandL2CA_ConnectReq = $0001;
  kBluetoothL2CAPTCICommandL2CA_DisconnectReq = $0002;
  kBluetoothL2CAPTCICommandL2CA_ConfigReq = $0003;
  kBluetoothL2CAPTCICommandL2CA_DisableCLT = $0004;
  kBluetoothL2CAPTCICommandL2CA_EnableCLT = $0005;
  kBluetoothL2CAPTCICommandL2CA_GroupCreate = $0006;
  kBluetoothL2CAPTCICommandL2CA_GroupClose = $0007;
  kBluetoothL2CAPTCICommandL2CA_GroupAddMember = $0008;
  kBluetoothL2CAPTCICommandL2CA_GroupRemoveMember = $0009;
  kBluetoothL2CAPTCICommandL2CA_GroupMembership = $000A;
  kBluetoothL2CAPTCICommandL2CA_WriteData = $000B;
  kBluetoothL2CAPTCICommandL2CA_ReadData = $000C;
  kBluetoothL2CAPTCICommandL2CA_Ping = $000D;
  kBluetoothL2CAPTCICommandL2CA_GetInfo = $000E;
  kBluetoothL2CAPTCICommandL2CA_Reserved1 = $000F;
  kBluetoothL2CAPTCICommandL2CA_Reserved2 = $0010;
  kBluetoothL2CAPTCICommandL2CA_ConnectResp = $0011;
  kBluetoothL2CAPTCICommandL2CA_DisconnectResp = $0012;
  kBluetoothL2CAPTCICommandL2CA_ConfigResp = $0013;
{$endif}

{$ifdef TYPES}
const
  kMaxChannelIDPerSide = 31;

type
  BluetoothRFCOMMChannelID = cuint8;
  BluetoothRFCOMMChannelIDPtr = ^BluetoothRFCOMMChannelID;
  BluetoothRFCOMMMTU = cuint16;
  BluetoothRFCOMMMTUPtr = ^BluetoothRFCOMMMTU;

const
  kBluetoothRFCOMMParityTypeNoParity = 0;
  kBluetoothRFCOMMParityTypeOddParity = 1;
  kBluetoothRFCOMMParityTypeEvenParity = 2;
  kBluetoothRFCOMMParityTypeMaxParity = 3;
type
  BluetoothRFCOMMParityType = clong;
const
  BluetoothRFCOMMLineStatusNoError = 0;
  BluetoothRFCOMMLineStatusOverrunError = 1;
  BluetoothRFCOMMLineStatusParityError = 2;
  BluetoothRFCOMMLineStatusFramingError = 3;
type
  BluetoothRFCOMMLineStatus = clong;
{$endif}

{$ifdef TYPES}
type
  BluetoothSDPPDUID = cuint8;
  BluetoothSDPPDUIDPtr = ^BluetoothSDPPDUID;

const
  kBluetoothSDPPDUIDReserved = 0;
  kBluetoothSDPPDUIDErrorResponse = 1;
  kBluetoothSDPPDUIDServiceSearchRequest = 2;
  kBluetoothSDPPDUIDServiceSearchResponse = 3;
  kBluetoothSDPPDUIDServiceAttributeRequest = 4;
  kBluetoothSDPPDUIDServiceAttributeResponse = 5;
  kBluetoothSDPPDUIDServiceSearchAttributeRequest = 6;
  kBluetoothSDPPDUIDServiceSearchAttributeResponse = 7;

type
  BluetoothSDPErrorCode = cuint16;
  BluetoothSDPErrorCodePtr = ^BluetoothSDPErrorCode;

const
  kBluetoothSDPErrorCodeSuccess = $0000;
  kBluetoothSDPErrorCodeReserved = $0000;
  kBluetoothSDPErrorCodeInvalidSDPVersion = $0001;
  kBluetoothSDPErrorCodeInvalidServiceRecordHandle = $0002;
  kBluetoothSDPErrorCodeInvalidRequestSyntax = $0003;
  kBluetoothSDPErrorCodeInvalidPDUSize = $0004;
  kBluetoothSDPErrorCodeInvalidContinuationState = $0005;
  kBluetoothSDPErrorCodeInsufficientResources = $0006;
  kBluetoothSDPErrorCodeReservedStart = $0007;
  kBluetoothSDPErrorCodeReservedEnd = $FFFF;

type
  BluetoothSDPTransactionID = cuint16;
  BluetoothSDPTransactionIDPtr = ^BluetoothSDPTransactionID;
  BluetoothSDPServiceRecordHandle = cuint32;
  BluetoothSDPServiceRecordHandlePtr = ^BluetoothSDPServiceRecordHandle;

const
  kBluetoothSDPDataElementTypeNil = 0;
  kBluetoothSDPDataElementTypeUnsignedInt = 1;
  kBluetoothSDPDataElementTypeSignedInt = 2;
  kBluetoothSDPDataElementTypeUUID = 3;
  kBluetoothSDPDataElementTypeString = 4;
  kBluetoothSDPDataElementTypeBoolean = 5;
  kBluetoothSDPDataElementTypeDataElementSequence = 6;
  kBluetoothSDPDataElementTypeDataElementAlternative = 7;
  kBluetoothSDPDataElementTypeURL = 8;
  kBluetoothSDPDataElementTypeReservedStart = 9;
  kBluetoothSDPDataElementTypeReservedEnd = 31;

type
  BluetoothSDPUUID16 = cuint16;
  BluetoothSDPUUID16Ptr = ^BluetoothSDPUUID16;
  BluetoothSDPUUID32 = cuint32;
  BluetoothSDPUUID32Ptr = ^BluetoothSDPUUID32;
  BluetoothSDPDataElementTypeDescriptor = cuint8;
  BluetoothSDPDataElementTypeDescriptorPtr = ^BluetoothSDPDataElementTypeDescriptor;
  BluetoothSDPDataElementSizeDescriptor = cuint8;
  BluetoothSDPDataElementSizeDescriptorPtr = ^BluetoothSDPDataElementSizeDescriptor;
  BluetoothSDPServiceAttributeID = cuint16;
  BluetoothSDPServiceAttributeIDPtr = ^BluetoothSDPServiceAttributeID;
{$endif}

{$ifdef TYPES}
const
  BluetoothLEScanTypePassive = $00;
  BluetoothLEScanTypeActive = $01;
type
  BluetoothLEScanType = clong;
const
  BluetoothLEAddressTypePublic = $00;
  BluetoothLEAddressTypeRandom = $01;
type
  BluetoothLEAddressType = clong;
const
  BluetoothLEScanFilterNone = $00;
  BluetoothLEScanFilterWhitelist = $01;
type
  BluetoothLEScanFilter = clong;
const
  BluetoothLEScanDisable = $00;
  BluetoothLEScanEnable = $01;
type
  BluetoothLEScan = clong;
const
  BluetoothLEScanDuplicateFilterDisable = $00;
  BluetoothLEScanDuplicateFilterEnable = $01;
type
  BluetoothLEScanDuplicateFilter = clong;
const
  BluetoothLEAdvertisingTypeConnectableUndirected = $00;
  BluetoothLEAdvertisingTypeConnectableDirected = $01;
  BluetoothLEAdvertisingTypeDiscoverableUndirected = $02;
  BluetoothLEAdvertisingTypeNonConnectableUndirected = $03;
  BluetoothLEAdvertisingTypeScanResponse = $04;
type
  BluetoothLEAdvertisingType = clong;
{$endif}

