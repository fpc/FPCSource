{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1999-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: TelephonyMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    This is the header file for the Telephony Manager
 *      for Palm OS Wireless Telephony Add-on.
 *    It defines the Telephony Manager public functions.
 *
 * History:
 *       Created by Gilles Fabre
 *    08/05/99    gfa         Initial version.
 *    05/02/00    gfa         Shared lib, 2nd API version.
 *
 *****************************************************************************)

unit telephonymgr;

interface

uses palmos, libtraps, errorbase, systemresources, event_, systemmgr, telephonymgrtypes;

// sysMakeROMVersion(major, minor, fix, stage, buildNum)
const
  kTelMgrVersionMajor = 1;
  kTelMgrVersionMinor = 0;
  kTelMgrVersionFix   = 3;
  kTelMgrVersionBuild = 0;

function kTelMgrVersion(major, minor, fix, stage: UInt8; buildNum: UInt16): UInt32;

const
// TelMgr shared lib internal name
  kTelMgrLibName = 'Telephony Library';

// TelMgr shared lib name and creator
  kTelMgrDatabaseCreator = sysFileCTelMgrLib;
  kTelMgrDatabaseType = sysFileTLibrary;

  kTelTelephonyNotification = Rsc('tmgr'); // telephony notification
  kTelTelephonyEvent = $1200;              // telephony event

// Telephony notification IDs
  kTelSmsLaunchCmdIncomingMessage = 0;  // an incoming SMS
  kTelSpcLaunchCmdIncomingCall = Succ(kTelSmsLaunchCmdIncomingMessage);   // an incoming voice call
  kTelSpcLaunchCmdCallerIDAvailable = Succ(kTelSpcLaunchCmdIncomingCall); // the caller ID is available
  kTelSpcLaunchCmdCallReleased = Succ(kTelSpcLaunchCmdCallerIDAvailable); // the call has been released
  kTelSpcLaunchCmdCallBusy = Succ(kTelSpcLaunchCmdCallReleased);          // the called equipment is busy
  kTelSpcLaunchCmdCallConnect = Succ(kTelSpcLaunchCmdCallBusy);           // the line is opened
  kTelSpcLaunchCmdCallError = Succ(kTelSpcLaunchCmdCallConnect);          // the call has encountered an error
  kTelEmcLaunchCmdCallReleased = Succ(kTelSpcLaunchCmdCallError);         // the call has been released
  kTelEmcLaunchCmdCallBusy = Succ(kTelEmcLaunchCmdCallReleased);          // the called equipment is busy
  kTelEmcLaunchCmdCallConnect = Succ(kTelEmcLaunchCmdCallBusy);           // the line is opened
  kTelEmcLaunchCmdCallError = Succ(kTelEmcLaunchCmdCallConnect);          // the call has encountered an error
  kTelLastLaunchCode = Succ(kTelEmcLaunchCmdCallError);

// notification priorities
  kTelCallNotificationPriority = 0; // higher priority
  kTelSmsNotificationPriority = 1;

// error codes
  telErrMsgAllocation         = telErrorClass or $01; // couldn't allocate message
  telErrUnknown               = telErrorClass or $02; // unknown Tel internal error
  telErrMemAllocation         = telErrorClass or $03; // memory allocation error

  telErrResultTimeOut         = telErrorClass or $04; // time-out was reached
  telErrResultUserCancel      = telErrorClass or $05; // user cancelled action
  telErrResultBusyResource    = telErrorClass or $06; // resource is busy
  telErrInvalidAppId          = telErrorClass or $07; // don't know that application
  telErrTooManyApps           = telErrorClass or $08; // applications table is full
  telErrSecurity              = telErrorClass or $09; // access to ME has not been granted
  telErrBufferSize            = telErrorClass or $0A; // buffer used to retrieve data is too small
  telErrFeatureNotSupported   = telErrorClass or $0B; // the feature is not supported by phone/network

  telErrPhoneComm             = telErrorClass or $0C; // the communication link with the phone is down
  telErrPhoneReply            = telErrorClass or $0D; // the phone reply syntax is incorrect, check the phone driver!
  telErrCommandFailed         = telErrorClass or $0E; // the phone couldn't achieve the associated command, check the phone driver!

  telErrSpcLineIsBusy         = telErrorClass or $0F; // spc call failure events, error field values

  telErrPhoneCodeRequired     = telErrorClass or $10; // phone code required
  telErrNoSIMInserted         = telErrorClass or $11; // no SIM inserted
  telErrPINRequired           = telErrorClass or $12; // PIN is required
  telErrPUKRequired           = telErrorClass or $13; // PUK is required
  telErrSIMFailure            = telErrorClass or $14; // the SIM is not working properly
  telErrSIMBusy               = telErrorClass or $15; // the SIM couldn't reply
  telErrSIMWrong              = telErrorClass or $16; // the SIM is not accepted by the phone
  telErrPassword              = telErrorClass or $17; // incorrect password
  telErrPIN2Required          = telErrorClass or $18; // PIN2 is required
  telErrPUK2Required          = telErrorClass or $19; // PUK2 is required
  telErrPhoneMemAllocation    = telErrorClass or $1A; // phone memory is full
  telErrInvalidIndex          = telErrorClass or $1B; // invalid index when accessing a storage
  telErrEntryNotFound         = telErrorClass or $1C; // entry not found
  telErrPhoneMemFailure       = telErrorClass or $1D; // the phone encountered a memory error
  telErrInvalidString         = telErrorClass or $1E; // bad character in text string
  telErrInvalidDial           = telErrorClass or $1F; // bad character in dial string
  telErrNoNetwork             = telErrorClass or $20; // no network available
  telErrNetworkTimeOut        = telErrorClass or $21; // the network didn't reply within 'normal' time delay
  telErrInvalidParameter      = telErrorClass or $22; // bad parameter passed to an API
  telErrValidityPeriod        = telErrorClass or $23; // the specified short message validity period is invalid
  telErrCodingScheme          = telErrorClass or $24; // the specified short message coding scheme is invalid
  telErrPhoneNumber           = telErrorClass or $25; // the specified short message smsc or destination phone number is invalid
  telErrValueStale            = telErrorClass or $26; // information couldn't be retrieved, a copy of last retrieved value was returned
  telErrTTaskNotRunning       = telErrorClass or $27; // the Telephony Task is not running
  telErrPhoneToSIMPINRequired = telErrorClass or $28; // Phone 2 SIM PIN is required

  telErrSpecificDrvNotFound   = telErrorClass or $29; // the specified driver was not found
  telErrGenericDrvNotFound    = telErrorClass or $2A; // the generic driver was not found
  telErrNoSpecificDrv         = telErrorClass or $2B; // no specific driver was specified

  telErrSpcLineIsReleased     = telErrorClass or $2C; // the call has been released
  telErrSpcCallError          = telErrorClass or $2D; // the call has encountered an error

  telErrNotInstalled          = telErrorClass or $2E; // the shared lib couldn't be installed
  telErrVersion               = telErrorClass or $2F; // the shared lib version doesn't match the application one
  telErrSettings              = telErrorClass or $30; // bad telephony settings: Phone Panel Prefs doesn't exist or Telephony Profile not (correctly) set

  telErrUnavailableValue      = telErrorClass or $31; // the asked value can't be retrieved at that time (i.e.: TelSpcGetCallerNumber and no active line)

  telErrLimitedCompatibility  = telErrorClass or $32; // the current driver is partially compatible with the connected phone

  telErrProfileConflict       = telErrorClass or $33; // the currently used profile conflicts with the requested profile

  telErrLibStillInUse         = telErrorClass or $34; // the shared lib is currently being used by another app, don't unload it!

  telErrTTaskNotFound         = telErrorClass or $35; // couldn't find the specified (by phone driver) telephony task

// constants
  kTelInvalidAppId = TelAppID(-1); // this value can't be returned on TelMgr attachement

  kTelInfiniteDelay = $FFFFFFFF; // infinite time-out delay

  kTelLocationSeparator = ';'; // this symbol is used to separate location string tokens

  kTelNwkAutomaticSearch = 0; // network search mode
  kTelNwkManualSearch    = 1;

  kTelNwkCDMA = 0;  // network type
  kTelNwkGSM  = 1;
  kTelNwkTDMA = 2;
  kTelNwkPDC  = 3;

  kTelPowBatteryPowered    = 0; // battery status
  kTelPowBatteryNotPowered = 1;
  kTelPowNoBattery         = 2;
  kTelPowBatteryFault      = 3;

  kTelSpcCallingLineId = -1;  // ID of a calling line. We can't provide a real ID knowing that an error might occur after
                              //TelSpcCallNumber return... So use this one to 'close' the line
// Messages types
  kTelSmsMessageTypeDelivered = 0;
  kTelSmsMessageTypeReport    = 1;
  kTelSmsMessageTypeSubmitted = 2;
  kTelSmsMessageTypeManualAck = 3;
  kTelSmsMessageAllTypes      = 4;

  kTelSmsMultiPartExtensionTypeId = $00; // Multipart short messages
  kTelSmsNbsExtensionTypeId       = $04; // NBS message, with port number in short
  kTelSmsNbs2ExtensionTypeId      = $05; // NBS message, with port number in long

  kTelSmsDefaultProtocol = 0; // sms message transport protocol
  kTelSmsFaxProtocol     = 1;
  kTelSmsX400Protocol    = 2;
  kTelSmsPagingProtocol  = 3;
  kTelSmsEmailProtocol   = 4;
  kTelSmsErmesProtocol   = 5;
  kTelSmsVoiceProtocol   = 6;

  kTelSmsAPIVersion      = $0001; // SMS api version

  kTelSmsStorageSIM      = 0; // SMS storage IDs
  kTelSmsStoragePhone    = 1;
  kTelSmsStorageAdaptor  = 2;
  kTelSmsStorageFirstOem = 3;

  kTelSmsCMTMessageType = 0; // Cellular Messaging Teleservice message
  kTelSmsCPTMessageType = 1; // Cellular Paging Teleservice message
  kTelSmsVMNMessageType = 2; // Voice Mail Notification message


// Delivery report Type (UInt8) - Only used in CDMA & TDMA advanced parameters
  kTelSmsStatusReportDeliveryType = 0; // Status report or delivery acknowledge
  kTelSmsManualAckDeliveryType    = 1; // Manual acknowledge delivery


// Data coding scheme (UInt8)
  kTelSms8BitsEncoding      = 0;
  kTelSmsBitsASCIIEncoding  = 1; // ANSI X3.4
  kTelSmsIA5Encoding        = 2; // CCITTT T.50
  kTelSmsIS91Encoding       = 3; // TIA/EIA/IS-91 section 3.7.1
  kTelSmsUCS2Encoding       = 4; // Only supported by GSM
  kTelSmsDefaultGSMEncoding = 5; // Only supported by GSM


// Message urgency / priority (UInt8) - Only used in CDMA & TDMA advanced parameters
  kTelSmsUrgencyNormal    = 0;
  kTelSmsUrgencyUrgent    = 1;
  kTelSmsUrgencyEmergency = 2;
//Bulk (CDMA) & Interactive mode (TDMA) are not supported


// Privacy message indicator (UInt8) - Only used in CDMA & TDMA advanced parameters
  kTelSmsPrivacyNotRestricted = 0; // Privacy level 0
  kTelSmsPrivacyRestricted    = 1; // Privacy level 1
  kTelSmsPrivacyConfidential  = 2; // Privacy level 2
  kTelSmsPrivacySecret        = 3; // Privacy level 3


// Delivery status report (UInt8)
  kTelSmsDSRSuccess                = 0;
  kTelSmsDSRMessageReplaced        = 1;
  kTelSmsDSRMessageForwarded       = 2; // unknown delivery result
  kTelSmsDSRTempCongestion         = 3;
  kTelSmsDSRTempSMEBusy            = 4;
  kTelSmsDSRTempServiceRejected    = 5;
  kTelSmsDSRTempServiceUnavailable = 6;
  kTelSmsDSRTempSMEError           = 7;
  kTelSmsDSRTempOther              = 8;
  kTelSmsDSRPermRPError            = 9;
  kTelSmsDSRPermBadDestination     = 10;
  kTelSmsDSRPermUnobtainable       = 11;
  kTelSmsDSRPermServiceUnavailable = 12;
  kTelSmsDSRPermInternetworkError  = 13;
  kTelSmsDSRPermValidityExpired    = 14;
  kTelSmsDSRPermDeletedByOrigSME   = 15;
  kTelSmsDSRPermDeleteByAdm        = 16;
  kTelSmsDSRPermSMNotExist         = 17;
  kTelSmsDSRPermOther              = 18;

  kTelSpeechCallClass = 0; // call classes
  kTelDataCallClass   = 1;
  kTelFaxCallClass    = 2;

  kTelPhbFixedPhonebook       = 0;  // phonebooks
  kTelPhbSimPhonebook         = 1;
  kTelPhbPhonePhonebook       = 2;
  kTelPhbLastDialedPhonebook  = 3;
  kTelPhbSimAndPhonePhonebook = 4;
  kTelPhbAdaptorPhonebook     = 5;
  kTelPhbFirstOemPhonebook    = 6;

  kTelCallIdle          = 0;  // call states
  kTelCallConnecting    = 1;
  kTelCallConnected     = 2;
  kTelCallRedial        = 3;
  kTelCallIncoming      = 4;
  kTelCallIncomingAck   = 5;
  kTelCallDisconnecting = 6;

  kTelCallTypeOutgoing = 0; // call type
  kTelCallTypeIncoming = 1;

  kTelCallServiceVoice = 0; // call service type
  kTelCallServiceData  = 1;

  kTelStyReady            = 0; // no more security code expected
  kTelStyPin1CodeId       = 1; // authentication code IDs
  kTelStyPin2CodeId       = 2;
  kTelStyPuk1CodeId       = 3;
  kTelStyPuk2CodeId       = 4;
  kTelStyPhoneToSimCodeId = 5;
  kTelStyFirstOemCodeId   = 6;

  kTelInfPhoneBrand       = 0; // phone information type
  kTelInfPhoneModel       = 1;
  kTelInfPhoneRevision    = 2;

// TelMgr library call ID's
// first entry points are reserved for internal use only
  telLibTrapReserved1                   = sysLibTrapCustom;
  telLibTrapReserved2                   = sysLibTrapCustom + 1;
  telLibTrapReserved3                   = sysLibTrapCustom + 2;
  telLibTrapReserved4                   = sysLibTrapCustom + 3;
  telLibTrapReserved5                   = sysLibTrapCustom + 4;
  telLibTrapReserved6                   = sysLibTrapCustom + 5;
  telLibTrapReserved7                   = sysLibTrapCustom + 6;
  telLibTrapReserved8                   = sysLibTrapCustom + 7;
  telLibTrapReserved9                   = sysLibTrapCustom + 8;
  telLibTrapReserved10                  = sysLibTrapCustom + 9;

  telLibTrapGetEvent                    = sysLibTrapCustom + 10;
  telLibTrapGetTelephonyEvent           = sysLibTrapCustom + 11;

  telLibTrapOpenPhoneConnection         = sysLibTrapCustom + 12;
  telLibTrapIsPhoneConnected            = sysLibTrapCustom + 13;
  telLibTrapClosePhoneConnection        = sysLibTrapCustom + 14;

  telLibTrapIsServiceAvailable          = sysLibTrapCustom + 15;
  telLibTrapIsFunctionSupported         = sysLibTrapCustom + 16;

  telLibTrapSendCommandString           = sysLibTrapCustom + 17;

  telLibTrapCancel                      = sysLibTrapCustom + 18;

  telLibTrapMatchPhoneDriver            = sysLibTrapCustom + 19;

  telLibTrapGetCallState                = sysLibTrapCustom + 20;

  telLibTrapOemCall                     = sysLibTrapCustom + 21;

  telLibTrapNwkGetNetworks              = sysLibTrapCustom + 22;
  telLibTrapNwkGetNetworkName           = sysLibTrapCustom + 23;
  telLibTrapNwkGetLocation              = sysLibTrapCustom + 24;
  telLibTrapNwkSelectNetwork            = sysLibTrapCustom + 25;
  telLibTrapNwkGetSelectedNetwork       = sysLibTrapCustom + 26;
  telLibTrapNwkGetNetworkType           = sysLibTrapCustom + 27;
  telLibTrapNwkGetSignalLevel           = sysLibTrapCustom + 28;
  telLibTrapNwkGetSearchMode            = sysLibTrapCustom + 29;
  telLibTrapNwkSetSearchMode            = sysLibTrapCustom + 30;

  telLibTrapStyChangeAuthenticationCode = sysLibTrapCustom + 31;
  telLibTrapStyGetAuthenticationState   = sysLibTrapCustom + 32;
  telLibTrapStyEnterAuthenticationCode  = sysLibTrapCustom + 33;

  telLibTrapPowGetPowerLevel            = sysLibTrapCustom + 34;
  telLibTrapPowGetBatteryStatus         = sysLibTrapCustom + 35;
  telLibTrapPowSetPhonePower            = sysLibTrapCustom + 36;

  telLibTrapCfgSetSmsCenter             = sysLibTrapCustom + 37;
  telLibTrapCfgGetSmsCenter             = sysLibTrapCustom + 38;
  telLibTrapCfgGetPhoneNumber           = sysLibTrapCustom + 39;

  telLibTrapSmsGetUniquePartId          = sysLibTrapCustom + 40;
  telLibTrapSmsGetDataMaxSize           = sysLibTrapCustom + 41;
  telLibTrapSmsSendMessage              = sysLibTrapCustom + 42;
  telLibTrapSmsSendManualAcknowledge    = sysLibTrapCustom + 43;
  telLibTrapSmsReadMessage              = sysLibTrapCustom + 44;
  telLibTrapSmsReadMessages             = sysLibTrapCustom + 45;
  telLibTrapSmsReadReport               = sysLibTrapCustom + 46;
  telLibTrapSmsReadReports              = sysLibTrapCustom + 47;
  telLibTrapSmsReadSubmittedMessage     = sysLibTrapCustom + 48;
  telLibTrapSmsReadSubmittedMessages    = sysLibTrapCustom + 49;
  telLibTrapSmsGetMessageCount          = sysLibTrapCustom + 50;
  telLibTrapSmsDeleteMessage            = sysLibTrapCustom + 51;
  telLibTrapSmsGetAvailableStorage      = sysLibTrapCustom + 52;
  telLibTrapSmsGetSelectedStorage       = sysLibTrapCustom + 53;
  telLibTrapSmsSelectStorage            = sysLibTrapCustom + 54;

  telLibTrapEmcCall                     = sysLibTrapCustom + 55;
  telLibTrapEmcCloseLine                = sysLibTrapCustom + 56;
  telLibTrapEmcGetNumberCount           = sysLibTrapCustom + 57;
  telLibTrapEmcGetNumber                = sysLibTrapCustom + 58;
  telLibTrapEmcSetNumber                = sysLibTrapCustom + 59;
  telLibTrapEmcSelectNumber             = sysLibTrapCustom + 60;

  telLibTrapSpcCallNumber               = sysLibTrapCustom + 61;
  telLibTrapSpcCloseLine                = sysLibTrapCustom + 62;
  telLibTrapSpcHoldLine                 = sysLibTrapCustom + 63;
  telLibTrapSpcRetrieveHeldLine         = sysLibTrapCustom + 64;
  telLibTrapSpcConference               = sysLibTrapCustom + 65;
  telLibTrapSpcSelectLine               = sysLibTrapCustom + 66;
  telLibTrapSpcAcceptCall               = sysLibTrapCustom + 67;
  telLibTrapSpcRejectCall               = sysLibTrapCustom + 68;
  telLibTrapSpcGetCallerNumber          = sysLibTrapCustom + 69;
  telLibTrapSpcSendBurstDTMF            = sysLibTrapCustom + 70;
  telLibTrapSpcStartContinuousDTMF      = sysLibTrapCustom + 71;
  telLibTrapSpcStopContinuousDTMF       = sysLibTrapCustom + 72;
  telLibTrapSpcPlayDTMF                 = sysLibTrapCustom + 73;

  telLibTrapPhbGetEntryCount            = sysLibTrapCustom + 74;
  telLibTrapPhbGetEntry                 = sysLibTrapCustom + 75;
  telLibTrapPhbGetEntries               = sysLibTrapCustom + 76;
  telLibTrapPhbAddEntry                 = sysLibTrapCustom + 77;
  telLibTrapPhbDeleteEntry              = sysLibTrapCustom + 78;
  telLibTrapPhbGetAvailablePhonebooks   = sysLibTrapCustom + 79;
  telLibTrapPhbSelectPhonebook          = sysLibTrapCustom + 80;
  telLibTrapPhbGetSelectedPhonebook     = sysLibTrapCustom + 81;
  telLibTrapPhbGetEntryMaxSizes         = sysLibTrapCustom + 82;

  telLibTrapSndPlayKeyTone              = sysLibTrapCustom + 83;
  telLibTrapSndStopKeyTone              = sysLibTrapCustom + 84;
  telLibTrapSndMute                     = sysLibTrapCustom + 85;

  telLibTrapInfGetInformation           = sysLibTrapCustom + 86;

  telLibTrapDtcCallNumber               = sysLibTrapCustom + 87;
  telLibTrapDtcCloseLine                = sysLibTrapCustom + 88;
  telLibTrapDtcSendData                 = sysLibTrapCustom + 89;
  telLibTrapDtcReceiveData              = sysLibTrapCustom + 90;

  telLibTrapUnblockNotifications        = sysLibTrapCustom + 91;

  telLibTrapOpenProfile                 = sysLibTrapCustom + 92;

  telLibTrapLast                        = sysLibTrapCustom + 93;

// function traps
function TelOpen(iRefnum: UInt16; iVersnum: UInt32; var oAppIdP: TelAppID): Err; syscall sysLibTrapOpen;

function TelClose(iRefnum: UInt16; iAppId: TelAppID): Err; syscall sysLibTrapClose;

// events management
procedure TelGetEvent(iRefnum: UInt16; iAppId: TelAppID; oEventP: EventPtr; iTimeOut: Int32); syscall telLibTrapGetEvent;

procedure TelGetTelephonyEvent(iRefnum: UInt16; iAppId: TelAppID; oEventP: EventPtr; iTimeOut: Int32); syscall telLibTrapGetTelephonyEvent;

// phone connection management
function TelOpenPhoneConnection(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapOpenPhoneConnection;

function TelIsPhoneConnected(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapIsPhoneConnected;

function TelClosePhoneConnection(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapClosePhoneConnection;

// service/function availability
function TelIsServiceAvailable(iRefnum: UInt16; iAppId: TelAppID; serviceId: UInt16; var ioTransIdP: UInt16): Err; syscall telLibTrapIsServiceAvailable;

function TelIsFunctionSupported(iRefnum: UInt16; iAppId: TelAppID; functionId: UInt16; var ioTransIdP: UInt16): Err; syscall telLibTrapIsFunctionSupported;

// sending commands to the phone
function TelSendCommandString(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelSendCommandStringType; var ioTransIdP: UInt16): Err; syscall telLibTrapSendCommandString;

// cancelling asynchronous calls
function TelCancel(iRefnum: UInt16; iAppId: TelAppID; iTransId: UInt16; var ioTransIdP: UInt16): Err; syscall telLibTrapCancel;

// checking whether phone and driver match
function TelMatchPhoneDriver(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapMatchPhoneDriver;

// getting phone status
function TelGetCallState(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelGetCallStateType; var ioTransIdP: UInt16): Err; syscall telLibTrapGetCallState;

// OEM support
function TelOemCall(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelOemCallType; var ioTransIdP: UInt16): Err; syscall telLibTrapOemCall;

// network
function TelNwkGetNetworks(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelNwkGetNetworksType; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkGetNetworks;

function TelNwkGetNetworkName(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelNwkGetNetworkNameType; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkGetNetworkName;

function TelNwkGetLocation(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelNwkGetLocationType; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkGetLocation;

function TelNwkSelectNetwork(iRefnum: UInt16; iAppId: TelAppID; iNetworkId: UInt32; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkSelectNetwork;

function TelNwkGetSelectedNetwork(iRefnum: UInt16; iAppId: TelAppID; var oNetworkIdP: UInt32; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkGetSelectedNetwork;

function TelNwkGetNetworkType(iRefnum: UInt16; iAppId: TelAppID; var oTypeP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkGetNetworkType;

function TelNwkGetSignalLevel(iRefnum: UInt16; iAppId: TelAppID; var oSignalP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkGetSignalLevel;

function TelNwkGetSearchMode(iRefnum: UInt16; iAppId: TelAppID; var oModeP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkGetSearchMode;

function TelNwkSetSearchMode(iRefnum: UInt16; iAppId: TelAppID; iMode: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapNwkSetSearchMode;

// security
function TelStyGetAuthenticationState(iRefnum: UInt16; iAppId: TelAppID; var oStateP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapStyGetAuthenticationState;

function TelStyEnterAuthenticationCode(iRefnum: UInt16; iAppId: TelAppID;  const iCodeP: PChar; var ioTransIdP: UInt16): Err; syscall telLibTrapStyEnterAuthenticationCode;

function TelStyChangeAuthenticationCode(iRefnum: UInt16; iAppId: TelAppID; var iParamP: TelStyChangeAuthenticationType; var ioTransIdP: UInt16): Err; syscall telLibTrapStyChangeAuthenticationCode;

// power
function TelPowGetPowerLevel(iRefnum: UInt16; iAppId: TelAppID; var oPowerP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapPowGetPowerLevel;

function TelPowGetBatteryStatus(iRefnum: UInt16; iAppId: TelAppID; var oStatusP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapPowGetBatteryStatus;

function TelPowSetPhonePower(iRefnum: UInt16; iAppId: TelAppID; iPowerOn: Boolean): Err; syscall telLibTrapPowSetPhonePower;

// configuration
function TelCfgSetSmsCenter(iRefnum: UInt16; iAppId: TelAppID; const iDialNumberP: PChar; var ioTransIdP: UInt16): Err; syscall telLibTrapCfgSetSmsCenter;

function TelCfgGetSmsCenter(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelCfgGetSmsCenterType; var ioTransIdP: UInt16): Err; syscall telLibTrapCfgGetSmsCenter;

function TelCfgGetPhoneNumber(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelCfgGetPhoneNumberType; var ioTransIdP: UInt16): Err; syscall telLibTrapCfgGetPhoneNumber;

// sms
function TelSmsGetUniquePartId(iRefnum: UInt16; iAppId: TelAppID; var oUniqueIdP: UInt16; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsGetUniquePartId;

function TelSmsGetDataMaxSize(iRefnum: UInt16; iAppId: TelAppID; var oSizeP: UInt16; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsGetDataMaxSize;

function TelSmsSendMessage(iRefnum: UInt16; iAppId: TelAppID; var ioMessageP: TelSmsSendMessageType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsSendMessage;

function TelSmsSendManualAcknowledge(iRefnum: UInt16; iAppId: TelAppID; var ioAckP: TelSmsManualAckType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsSendManualAcknowledge;

function TelSmsReadMessage(iRefnum: UInt16; iAppId: TelAppID; var ioMessageP: TelSmsDeliveryMessageType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsReadMessage;

function TelSmsReadMessages(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelSmsReadMessagesType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsReadMessages;

function TelSmsReadReport(iRefnum: UInt16; iAppId: TelAppID; var ioReportP: TelSmsReportType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsReadReport;

function TelSmsReadReports(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelSmsReadReportsType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsReadReports;

function TelSmsReadSubmittedMessage(iRefnum: UInt16; iAppId: TelAppID; var ioMessageP: TelSmsSubmittedMessageType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsReadSubmittedMessage;

function TelSmsReadSubmittedMessages(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelSmsReadSubmittedMessagesType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsReadSubmittedMessages;

function TelSmsGetMessageCount(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelSmsGetMessageCountType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsGetMessageCount;

function TelSmsDeleteMessage(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelSmsDeleteMessageType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsDeleteMessage;

function TelSmsGetAvailableStorage(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelSmsGetAvailableStorageType; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsGetAvailableStorage;

function TelSmsGetSelectedStorage(iRefnum: UInt16; iAppId: TelAppID; var oStorageIdP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsGetSelectedStorage;

function TelSmsSelectStorage(iRefnum: UInt16; iAppId: TelAppID; iStorageId: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapSmsSelectStorage;

// emergency calls
function TelEmcCall(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapEmcCall;

function TelEmcCloseLine(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapEmcCloseLine;

function TelEmcGetNumberCount(iRefnum: UInt16; iAppId: TelAppID; var oCountP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapEmcGetNumberCount;

function TelEmcGetNumber(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelEmcGetNumberType; var ioTransIdP: UInt16): Err; syscall telLibTrapEmcGetNumber;

function TelEmcSetNumber(iRefnum: UInt16; iAppId: TelAppID; var iParamP: TelEmcSetNumberType; var ioTransIdP: UInt16): Err; syscall telLibTrapEmcSetNumber;

function TelEmcSelectNumber(iRefnum: UInt16; iAppId: TelAppID; iIndex: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapEmcSelectNumber;

// speech call
function TelSpcCallNumber(iRefnum: UInt16; iAppId: TelAppID; const iDialNumberP: PChar; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcCallNumber;

function TelSpcCloseLine(iRefnum: UInt16; iAppId: TelAppID; iLineId: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcCloseLine;

function TelSpcHoldLine(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcHoldLine;

function TelSpcRetrieveHeldLine(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcRetrieveHeldLine;

function TelSpcConference(iRefnum: UInt16; iAppId: TelAppID; var oLineIdP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcConference;

function TelSpcSelectLine(iRefnum: UInt16; iAppId: TelAppID; iLineId: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcSelectLine;

function TelSpcAcceptCall(iRefnum: UInt16; iAppId: TelAppID; var oLineIdP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcAcceptCall;

function TelSpcRejectCall(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcRejectCall;

function TelSpcGetCallerNumber(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelSpcGetCallerNumberType; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcGetCallerNumber;

function TelSpcSendBurstDTMF(iRefnum: UInt16; iAppId: TelAppID; const iDTMFStringP: PChar; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcSendBurstDTMF;

function TelSpcStartContinuousDTMF(iRefnum: UInt16; iAppId: TelAppID; iKeyCode: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcStartContinuousDTMF;

function TelSpcStopContinuousDTMF(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcStopContinuousDTMF;

function TelSpcPlayDTMF(iRefnum: UInt16; iAppId: TelAppID; var iParamP: TelSpcPlayDTMFType; var ioTransIdP: UInt16): Err; syscall telLibTrapSpcPlayDTMF;

// phonebook
function TelPhbGetEntryCount(iRefnum: UInt16; iAppId: TelAppID; var oParamP: TelPhbGetEntryCountType; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbGetEntryCount;

function TelPhbGetEntry(iRefnum: UInt16; iAppId: TelAppID; var ioEntryP: TelPhbEntryType; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbGetEntry;

function TelPhbGetEntries(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelPhbGetEntriesType; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbGetEntries;

function TelPhbAddEntry(iRefnum: UInt16; iAppId: TelAppID; var iEntryP: TelPhbEntryType; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbAddEntry;

function TelPhbDeleteEntry(iRefnum: UInt16; iAppId: TelAppID; iEntryIndex: UInt16; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbDeleteEntry;

function TelPhbGetAvailablePhonebooks(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelPhbGetAvailablePhonebooksType; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbGetAvailablePhonebooks;

function TelPhbSelectPhonebook(iRefnum: UInt16; iAppId: TelAppID; iPhbId: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbSelectPhonebook;

function TelPhbGetSelectedPhonebook(iRefnum: UInt16; iAppId: TelAppID; var oPhbIdP: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbGetSelectedPhonebook;

function TelPhbGetEntryMaxSizes(iRefnum: UInt16; iAppId: TelAppID; var oParamP: TelPhbGetEntryMaxSizesType; var ioTransIdP: UInt16): Err; syscall telLibTrapPhbGetEntryMaxSizes;

// sound
function TelSndPlayKeyTone(iRefnum: UInt16; iAppId: TelAppID; var iParamP: TelSndPlayKeyToneType; var ioTransIdP: UInt16): Err; syscall telLibTrapSndPlayKeyTone;

function TelSndStopKeyTone(iRefnum: UInt16; iAppId: TelAppID; var ioTransIdP: UInt16): Err; syscall telLibTrapSndStopKeyTone;

function TelSndMute(iRefnum: UInt16; iAppId: TelAppID; iMuteOn: Boolean; var ioTransIdP: UInt16): Err; syscall telLibTrapSndMute;

// information
function TelInfGetInformation(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelInfGetInformationType; var ioTransIdP: UInt16): Err; syscall telLibTrapInfGetInformation;

// data
function TelDtcCallNumber(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelDtcCallNumberType; var ioTransIdP: UInt16): Err; syscall telLibTrapDtcCallNumber;

function TelDtcCloseLine(iRefnum: UInt16; iAppId: TelAppID; iLineId: UInt8; var ioTransIdP: UInt16): Err; syscall telLibTrapDtcCloseLine;

function TelDtcSendData(iRefnum: UInt16; iAppId: TelAppID; var iParamP: TelDtcSendDataType; var ioTransIdP: UInt16): Err; syscall telLibTrapDtcSendData;

function TelDtcReceiveData(iRefnum: UInt16; iAppId: TelAppID; var ioParamP: TelDtcReceiveDataType; var ioTransIdP: UInt16): Err; syscall telLibTrapDtcReceiveData;

function TelUnblockNotifications(iRefnum: UInt16): Err; syscall telLibTrapUnblockNotifications;

// open telephony using a particular connection profile
function TelOpenProfile(iRefnum: UInt16; iVersnum: UInt32; profileId: UInt32; var oAppIdP: TelAppID): Err; syscall telLibTrapOpenProfile;

// MACROS for checking service availability
function TelIsNwkServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsStyServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPowServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsCfgServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsEmcServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsDtcServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsOemServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSndServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsInfServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

// MACROS to check function availability
function TelIsSendCommandStringSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsCancelSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsMatchPhoneDriverSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsGetCallStateSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsOemCallSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkGetNetworksSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkGetNetworkNameSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkGetLocationSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkSelectNetworkSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkGetSelectedNetworkSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkGetNetworkTypeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkGetSignalLevelSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkGetSearchModeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsNwkSetSearchModeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsStyGetAuthenticationStateSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsStyEnterAuthenticationCodeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsStyChangeAuthenticationCodeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPowGetPowerLevelSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPowGetBatteryStatusSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPowSetPhonePowerSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsCfgSetSmsCenterSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsCfgGetSmsCenterSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsCfgGetPhoneNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsGetUniquePartIdSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsGetDataMaxSizeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsSendMessageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsSendManualAcknowledgeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsReadMessageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsReadMessagesSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsReadReportSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsReadReportsSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsReadSubmittedMessageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsReadSubmittedMessagesSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsGetMessageCountSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsDeleteMessageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsGetAvailableStorageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsGetSelectedStorageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSmsSelectStorageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsEmcCallSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsEmcCloseLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsEmcGetNumberCountSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsEmcGetNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsEmcSetNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsEmcSelectNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcCallNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcCloseLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcHoldLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcRetrieveHeldLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcConferenceSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcSelectLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcAcceptCallSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcRejectCallSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcGetCallerNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcSendBurstDTMFSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcStartContinuousDTMFSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcStopContinuousDTMFSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSpcPlayDTMFSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbGetEntryCountSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbGetEntrySupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbGetEntriesSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbAddEntrySupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbDeleteEntrySupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbGetAvailablePhonebooksSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbSelectPhonebookSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbGetSelectedPhonebookSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsPhbGetEntryMaxSizesSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSndPlayKeyToneSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSndStopKeyToneSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsSndMuteSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsInfGetInformationSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsDtcCallNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsDtcCloseLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsDtcSendDataSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

function TelIsDtcReceiveDataSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;

implementation

function kTelMgrVersion(major, minor, fix, stage: UInt8; buildNum: UInt16): UInt32;
begin
   kTelMgrVersion := sysMakeROMVersion(kTelMgrVersionMajor, kTelMgrVersionMinor, kTelMgrVersionFix, sysROMStageBeta, kTelMgrVersionBuild);
end;

// MACROS for checking service availability
function TelIsNwkServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelNwkServiceId, transIdP);
end;

function TelIsStyServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsStyServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelStyServiceId, transIdP);
end;

function TelIsPowServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPowServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelPowServiceId, transIdP);
end;

function TelIsCfgServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsCfgServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelCfgServiceId, transIdP);
end;

function TelIsSmsServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelSmsServiceId, transIdP);
end;

function TelIsEmcServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsEmcServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelEmcServiceId, transIdP);
end;

function TelIsSpcServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelSpcServiceId, transIdP);
end;

function TelIsDtcServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsDtcServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelDtcServiceId, transIdP);
end;

function TelIsPhbServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelPhbServiceId, transIdP);
end;

function TelIsOemServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsOemServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelOemServiceId, transIdP);
end;

function TelIsSndServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSndServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelSndServiceId, transIdP);
end;

function TelIsInfServiceAvailable(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsInfServiceAvailable := TelIsServiceAvailable(refnum, appId, kTelInfServiceId, transIdP);
end;

// MACROS to check function availability
function TelIsSendCommandStringSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSendCommandStringSupported := TelIsFunctionSupported(refnum, appId, kTelSendCommandStringMessage, transIdP);
end;

function TelIsCancelSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsCancelSupported := TelIsFunctionSupported(refnum, appId, kTelUrqCancelMessage, transIdP);
end;

function TelIsMatchPhoneDriverSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsMatchPhoneDriverSupported := TelIsFunctionSupported(refnum, appId, kTelUrqMatchPhoneDriverMessage, transIdP);
end;

function TelIsGetCallStateSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsGetCallStateSupported := TelIsFunctionSupported(refnum, appId, kTelGetCallStateMessage, transIdP);
end;

function TelIsOemCallSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsOemCallSupported := TelIsFunctionSupported(refnum, appId, kTelOemCallMessage, transIdP);
end;

function TelIsNwkGetNetworksSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkGetNetworksSupported := TelIsFunctionSupported(refnum, appId, kTelNwkGetNetworksMessage, transIdP);
end;

function TelIsNwkGetNetworkNameSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkGetNetworkNameSupported := TelIsFunctionSupported(refnum, appId, kTelNwkGetNetworkNameMessage, transIdP);
end;

function TelIsNwkGetLocationSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkGetLocationSupported := TelIsFunctionSupported(refnum, appId, kTelNwkGetLocationMessage, transIdP);
end;

function TelIsNwkSelectNetworkSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkSelectNetworkSupported := TelIsFunctionSupported(refnum, appId, kTelNwkSelectNetworkMessage, transIdP);
end;

function TelIsNwkGetSelectedNetworkSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkGetSelectedNetworkSupported := TelIsFunctionSupported(refnum, appId, kTelNwkGetSelectedNetworkMessage, transIdP);
end;

function TelIsNwkGetNetworkTypeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkGetNetworkTypeSupported := TelIsFunctionSupported(refnum, appId, kTelNwkGetNetworkTypeMessage, transIdP);
end;

function TelIsNwkGetSignalLevelSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkGetSignalLevelSupported := TelIsFunctionSupported(refnum, appId, kTelNwkGetSignalLevelMessage, transIdP);
end;

function TelIsNwkGetSearchModeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkGetSearchModeSupported := TelIsFunctionSupported(refnum, appId, kTelNwkGetSearchModeMessage, transIdP);
end;

function TelIsNwkSetSearchModeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsNwkSetSearchModeSupported := TelIsFunctionSupported(refnum, appId, kTelNwkSetSearchModeMessage, transIdP);
end;

function TelIsStyGetAuthenticationStateSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsStyGetAuthenticationStateSupported := TelIsFunctionSupported(refnum, appId, kTelStyGetAuthenticationStateMessage, transIdP);
end;

function TelIsStyEnterAuthenticationCodeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsStyEnterAuthenticationCodeSupported := TelIsFunctionSupported(refnum, appId, kTelStyEnterAuthenticationCodeMessage, transIdP);
end;

function TelIsStyChangeAuthenticationCodeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsStyChangeAuthenticationCodeSupported := TelIsFunctionSupported(refnum, appId, kTelStyChangeAuthenticationCodeMessage, transIdP);
end;

function TelIsPowGetPowerLevelSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPowGetPowerLevelSupported := TelIsFunctionSupported(refnum, appId, kTelPowGetPowerLevelMessage, transIdP);
end;

function TelIsPowGetBatteryStatusSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPowGetBatteryStatusSupported := TelIsFunctionSupported(refnum, appId, kTelPowGetBatteryStatusMessage, transIdP);
end;

function TelIsPowSetPhonePowerSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPowSetPhonePowerSupported := TelIsFunctionSupported(refnum, appId, kTelPowSetPhonePowerMessage, transIdP);
end;

function TelIsCfgSetSmsCenterSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsCfgSetSmsCenterSupported := TelIsFunctionSupported(refnum, appId, kTelCfgSetSmsCenterMessage, transIdP);
end;

function TelIsCfgGetSmsCenterSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsCfgGetSmsCenterSupported := TelIsFunctionSupported(refnum, appId, kTelCfgGetSmsCenterMessage, transIdP);
end;

function TelIsCfgGetPhoneNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsCfgGetPhoneNumberSupported := TelIsFunctionSupported(refnum, appId, kTelCfgGetPhoneNumberMessage, transIdP);
end;

function TelIsSmsGetUniquePartIdSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsGetUniquePartIdSupported := TelIsFunctionSupported(refnum, appId, kTelUrqSmsGetUniquePartIdMessage, transIdP);
end;

function TelIsSmsGetDataMaxSizeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsGetDataMaxSizeSupported := TelIsFunctionSupported(refnum, appId, kTelSmsGetDataMaxSizeMessage, transIdP);
end;

function TelIsSmsSendMessageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsSendMessageSupported := TelIsFunctionSupported(refnum, appId, kTelSmsSendMessageMessage, transIdP);
end;

function TelIsSmsSendManualAcknowledgeSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsSendManualAcknowledgeSupported := TelIsFunctionSupported(refnum, appId, kTelSmsSendManualAcknowledgeMessage, transIdP);
end;

function TelIsSmsReadMessageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsReadMessageSupported := TelIsFunctionSupported(refnum, appId, kTelSmsReadMessageMessage, transIdP);
end;

function TelIsSmsReadMessagesSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsReadMessagesSupported := TelIsFunctionSupported(refnum, appId, kTelSmsReadMessagesMessage, transIdP);
end;

function TelIsSmsReadReportSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsReadReportSupported := TelIsFunctionSupported(refnum, appId, kTelSmsReadReportMessage, transIdP);
end;

function TelIsSmsReadReportsSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsReadReportsSupported := TelIsFunctionSupported(refnum, appId, kTelSmsReadReportsMessage, transIdP);
end;

function TelIsSmsReadSubmittedMessageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsReadSubmittedMessageSupported := TelIsFunctionSupported(refnum, appId, kTelSmsReadSubmittedMessageMessage, transIdP);
end;

function TelIsSmsReadSubmittedMessagesSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsReadSubmittedMessagesSupported := TelIsFunctionSupported(refnum, appId, kTelSmsReadSubmittedMessagesMessage, transIdP);
end;

function TelIsSmsGetMessageCountSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsGetMessageCountSupported := TelIsFunctionSupported(refnum, appId, kTelSmsGetMessageCountMessage, transIdP);
end;

function TelIsSmsDeleteMessageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsDeleteMessageSupported := TelIsFunctionSupported(refnum, appId, kTelSmsDeleteMessageMessage, transIdP);
end;

function TelIsSmsGetAvailableStorageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsGetAvailableStorageSupported := TelIsFunctionSupported(refnum, appId, kTelSmsGetAvailableStorageMessage, transIdP);
end;

function TelIsSmsGetSelectedStorageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsGetSelectedStorageSupported := TelIsFunctionSupported(refnum, appId, kTelSmsGetSelectedStorageMessage, transIdP);
end;

function TelIsSmsSelectStorageSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSmsSelectStorageSupported := TelIsFunctionSupported(refnum, appId, kTelSmsSelectStorageMessage, transIdP);
end;

function TelIsEmcCallSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsEmcCallSupported := TelIsFunctionSupported(refnum, appId, kTelEmcCallMessage, transIdP);
end;

function TelIsEmcCloseLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsEmcCloseLineSupported := TelIsFunctionSupported(refnum, appId, kTelEmcCloseLineMessage, transIdP);
end;

function TelIsEmcGetNumberCountSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsEmcGetNumberCountSupported := TelIsFunctionSupported(refnum, appId, kTelEmcGetNumberCountMessage, transIdP);
end;

function TelIsEmcGetNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsEmcGetNumberSupported := TelIsFunctionSupported(refnum, appId, kTelEmcGetNumberMessage, transIdP);
end;

function TelIsEmcSetNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsEmcSetNumberSupported := TelIsFunctionSupported(refnum, appId, kTelEmcSetNumberMessage, transIdP);
end;

function TelIsEmcSelectNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsEmcSelectNumberSupported := TelIsFunctionSupported(refnum, appId, kTelEmcSelectNumberMessage, transIdP);
end;

function TelIsSpcCallNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcCallNumberSupported := TelIsFunctionSupported(refnum, appId, kTelSpcCallNumberMessage, transIdP);
end;

function TelIsSpcCloseLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcCloseLineSupported := TelIsFunctionSupported(refnum, appId, kTelSpcCloseLineMessage, transIdP);
end;

function TelIsSpcHoldLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcHoldLineSupported := TelIsFunctionSupported(refnum, appId, kTelSpcHoldLineMessage, transIdP);
end;

function TelIsSpcRetrieveHeldLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcRetrieveHeldLineSupported := TelIsFunctionSupported(refnum, appId, kTelSpcRetrieveHeldLineMessage, transIdP);
end;

function TelIsSpcConferenceSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcConferenceSupported := TelIsFunctionSupported(refnum, appId, kTelSpcConferenceMessage, transIdP);
end;

function TelIsSpcSelectLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcSelectLineSupported := TelIsFunctionSupported(refnum, appId, kTelSpcSelectLineMessage, transIdP);
end;

function TelIsSpcAcceptCallSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcAcceptCallSupported := TelIsFunctionSupported(refnum, appId, kTelSpcAcceptCallMessage, transIdP);
end;

function TelIsSpcRejectCallSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcRejectCallSupported := TelIsFunctionSupported(refnum, appId, kTelSpcRejectCallMessage, transIdP);
end;

function TelIsSpcGetCallerNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcGetCallerNumberSupported := TelIsFunctionSupported(refnum, appId, kTelSpcGetCallerNumberMessage, transIdP);
end;

function TelIsSpcSendBurstDTMFSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcSendBurstDTMFSupported := TelIsFunctionSupported(refnum, appId, kTelSpcSendBurstDTMFMessage, transIdP);
end;

function TelIsSpcStartContinuousDTMFSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcStartContinuousDTMFSupported := TelIsFunctionSupported(refnum, appId, kTelSpcStartContinuousDTMFMessage, transIdP);
end;

function TelIsSpcStopContinuousDTMFSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcStopContinuousDTMFSupported := TelIsFunctionSupported(refnum, appId, kTelSpcStopContinuousDTMFMessage, transIdP);
end;

function TelIsSpcPlayDTMFSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSpcPlayDTMFSupported := TelIsFunctionSupported(refnum, appId, kTelSpcPlayDTMFMessage, transIdP);
end;

function TelIsPhbGetEntryCountSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbGetEntryCountSupported := TelIsFunctionSupported(refnum, appId, kTelPhbGetEntryCountMessage, transIdP);
end;

function TelIsPhbGetEntrySupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbGetEntrySupported := TelIsFunctionSupported(refnum, appId, kTelPhbGetEntryMessage, transIdP);
end;

function TelIsPhbGetEntriesSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbGetEntriesSupported := TelIsFunctionSupported(refnum, appId, kTelPhbGetEntriesMessage, transIdP);
end;

function TelIsPhbAddEntrySupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbAddEntrySupported := TelIsFunctionSupported(refnum, appId, kTelPhbAddEntryMessage, transIdP);
end;

function TelIsPhbDeleteEntrySupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbDeleteEntrySupported := TelIsFunctionSupported(refnum, appId, kTelPhbDeleteEntryMessage, transIdP);
end;

function TelIsPhbGetAvailablePhonebooksSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbGetAvailablePhonebooksSupported := TelIsFunctionSupported(refnum, appId, kTelPhbGetAvailablePhonebooksMessage, transIdP);
end;

function TelIsPhbSelectPhonebookSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbSelectPhonebookSupported := TelIsFunctionSupported(refnum, appId, kTelPhbSelectPhonebookMessage, transIdP);
end;

function TelIsPhbGetSelectedPhonebookSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbGetSelectedPhonebookSupported := TelIsFunctionSupported(refnum, appId, kTelPhbGetSelectedPhonebookMessage, transIdP);
end;

function TelIsPhbGetEntryMaxSizesSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsPhbGetEntryMaxSizesSupported := TelIsFunctionSupported(refnum, appId, kTelPhbGetEntryMaxSizesMessage, transIdP);
end;

function TelIsSndPlayKeyToneSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSndPlayKeyToneSupported := TelIsFunctionSupported(refnum, appId, kTelSndPlayKeyToneMessage, transIdP);
end;

function TelIsSndStopKeyToneSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSndStopKeyToneSupported := TelIsFunctionSupported(refnum, appId, kTelSndStopKeyToneMessage, transIdP);
end;

function TelIsSndMuteSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsSndMuteSupported := TelIsFunctionSupported(refnum, appId, kTelSndMuteMessage, transIdP);
end;

function TelIsInfGetInformationSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsInfGetInformationSupported := TelIsFunctionSupported(refnum, appId, kTelInfGetInformationMessage, transIdP);
end;

function TelIsDtcCallNumberSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsDtcCallNumberSupported := TelIsFunctionSupported(refnum, appId, kTelDtcCallNumberMessage, transIdP);
end;

function TelIsDtcCloseLineSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsDtcCloseLineSupported := TelIsFunctionSupported(refnum, appId, kTelDtcCloseLineMessage, transIdP);
end;

function TelIsDtcSendDataSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsDtcSendDataSupported := TelIsFunctionSupported(refnum, appId, kTelDtcSendDataMessage, transIdP);
end;

function TelIsDtcReceiveDataSupported(refnum: UInt16; appId: TelAppID; var transIdP: UInt16): Err;
begin
   TelIsDtcReceiveDataSupported := TelIsFunctionSupported(refnum, appId, kTelDtcReceiveDataMessage, transIdP);
end;

end.
