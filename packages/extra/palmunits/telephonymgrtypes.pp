(******************************************************************************
 *
 * Copyright (c) 1999-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: TelephonyMgrTypes.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    This is the header file declaring the data types used by the
 *    Telephony Manager for Palm OS Wireless Telephony Add-on.
 *
 * History:
 *       Created by Gilles Fabre
 *    08/02/99    gfa         Initial version.
 *    05/02/00    gfa         Shared lib, 2nd API version.
 *
 *****************************************************************************)

unit telephonymgrtypes;

interface

uses  palmos, errorbase, event_;

const
  telErrorClass = appErrorClass + $100; // DOLATER: remove this constant
  kTelMaxPhoneNumberLen = 30;

// managers id
type
  TelServices = Enum;

const
  kTelNwkServiceId = 0;
  kTelStyServiceId = Succ(kTelNwkServiceId);
  kTelPowServiceId = Succ(kTelStyServiceId);
  kTelCfgServiceId = Succ(kTelPowServiceId);
  kTelSmsServiceId = Succ(kTelCfgServiceId);
  kTelEmcServiceId = Succ(kTelSmsServiceId);
  kTelSpcServiceId = Succ(kTelEmcServiceId);
  kTelDtcServiceId = Succ(kTelSpcServiceId);
  kTelPhbServiceId = Succ(kTelDtcServiceId);
  kTelOemServiceId = Succ(kTelPhbServiceId);
  kTelSndServiceId = Succ(kTelOemServiceId);
  kTelInfServiceId = Succ(kTelSndServiceId);

// messages
type
  TelMessages = Enum;

const
  kTelGetCallStateMessage = 0;

  kTelSendCommandStringMessage           = Succ(kTelGetCallStateMessage);

  kTelNwkGetNetworkNameMessage           = Succ(kTelSendCommandStringMessage);
  kTelNwkGetNetworkTypeMessage           = Succ(kTelNwkGetNetworkNameMessage);
  kTelNwkGetSignalLevelMessage           = Succ(kTelNwkGetNetworkTypeMessage);
  kTelNwkGetSearchModeMessage            = Succ(kTelNwkGetSignalLevelMessage);
  kTelNwkSetSearchModeMessage            = Succ(kTelNwkGetSearchModeMessage);
  kTelNwkGetNetworksMessage              = Succ(kTelNwkSetSearchModeMessage);
  kTelNwkSelectNetworkMessage            = Succ(kTelNwkGetNetworksMessage);
  kTelNwkGetSelectedNetworkMessage       = Succ(kTelNwkSelectNetworkMessage);
  kTelNwkGetLocationMessage              = Succ(kTelNwkGetSelectedNetworkMessage);

  kTelStyGetAuthenticationStateMessage   = Succ(kTelNwkGetLocationMessage       );
  kTelStyEnterAuthenticationCodeMessage  = Succ(kTelStyGetAuthenticationStateMessage);
  kTelStyChangeAuthenticationCodeMessage = Succ(kTelStyEnterAuthenticationCodeMessage);

  kTelPowGetPowerLevelMessage            = Succ(kTelStyChangeAuthenticationCodeMessage);
  kTelPowGetBatteryStatusMessage         = Succ(kTelPowGetPowerLevelMessage);
  kTelPowSetPhonePowerMessage            = Succ(kTelPowGetBatteryStatusMessage);

  kTelCfgSetSmsCenterMessage             = Succ(kTelPowSetPhonePowerMessage);
  kTelCfgGetSmsCenterMessage             = Succ(kTelCfgSetSmsCenterMessage);
  kTelCfgGetPhoneNumberMessage           = Succ(kTelCfgGetSmsCenterMessage);

  kTelSmsSendMessageMessage              = Succ(kTelCfgGetPhoneNumberMessage);
  kTelSmsSendManualAcknowledgeMessage    = Succ(kTelSmsSendMessageMessage);
  kTelSmsGetMessageCountMessage          = Succ(kTelSmsSendManualAcknowledgeMessage);
  kTelSmsReadMessageMessage              = Succ(kTelSmsGetMessageCountMessage);
  kTelSmsReadMessagesMessage             = Succ(kTelSmsReadMessageMessage);
  kTelSmsReadSubmittedMessageMessage     = Succ(kTelSmsReadMessagesMessage);
  kTelSmsReadSubmittedMessagesMessage    = Succ(kTelSmsReadSubmittedMessageMessage);
  kTelSmsReadReportMessage               = Succ(kTelSmsReadSubmittedMessagesMessage);
  kTelSmsReadReportsMessage              = Succ(kTelSmsReadReportMessage);
  kTelSmsDeleteMessageMessage            = Succ(kTelSmsReadReportsMessage);
  kTelSmsGetAvailableStorageMessage      = Succ(kTelSmsDeleteMessageMessage);
  kTelSmsSelectStorageMessage            = Succ(kTelSmsGetAvailableStorageMessage);
  kTelSmsGetSelectedStorageMessage       = Succ(kTelSmsSelectStorageMessage);
  kTelSmsGetDataMaxSizeMessage           = Succ(kTelSmsGetSelectedStorageMessage);

  kTelEmcGetNumberCountMessage           = Succ(kTelSmsGetDataMaxSizeMessage);
  kTelEmcGetNumberMessage                = Succ(kTelEmcGetNumberCountMessage);
  kTelEmcSetNumberMessage                = Succ(kTelEmcGetNumberMessage);
  kTelEmcSelectNumberMessage             = Succ(kTelEmcSetNumberMessage);
  kTelEmcCallMessage                     = Succ(kTelEmcSelectNumberMessage);
  kTelEmcCloseLineMessage                = Succ(kTelEmcCallMessage);

  kTelSpcAcceptCallMessage               = Succ(kTelEmcCloseLineMessage);
  kTelSpcRejectCallMessage               = Succ(kTelSpcAcceptCallMessage);
  kTelSpcCallNumberMessage               = Succ(kTelSpcRejectCallMessage);
  kTelSpcCloseLineMessage                = Succ(kTelSpcCallNumberMessage);
  kTelSpcHoldLineMessage                 = Succ(kTelSpcCloseLineMessage);
  kTelSpcRetrieveHeldLineMessage         = Succ(kTelSpcHoldLineMessage);
  kTelSpcGetCallerNumberMessage          = Succ(kTelSpcRetrieveHeldLineMessage);
  kTelSpcSendBurstDTMFMessage            = Succ(kTelSpcGetCallerNumberMessage);
  kTelSpcStartContinuousDTMFMessage      = Succ(kTelSpcSendBurstDTMFMessage);
  kTelSpcStopContinuousDTMFMessage       = Succ(kTelSpcStartContinuousDTMFMessage);
  kTelSpcConferenceMessage               = Succ(kTelSpcStopContinuousDTMFMessage);
  kTelSpcSelectLineMessage               = Succ(kTelSpcConferenceMessage);
  kTelSpcPlayDTMFMessage                 = Succ(kTelSpcSelectLineMessage);

  kTelDtcCallNumberMessage               = Succ(kTelSpcPlayDTMFMessage);
  kTelDtcCloseLineMessage                = Succ(kTelDtcCallNumberMessage);
  kTelDtcSendDataMessage                 = Succ(kTelDtcCloseLineMessage);
  kTelDtcReceiveDataMessage              = Succ(kTelDtcSendDataMessage);

  kTelPhbGetEntryCountMessage            = Succ(kTelDtcReceiveDataMessage);
  kTelPhbGetEntryMessage                 = Succ(kTelPhbGetEntryCountMessage);
  kTelPhbGetEntriesMessage               = Succ(kTelPhbGetEntryMessage);
  kTelPhbAddEntryMessage                 = Succ(kTelPhbGetEntriesMessage);
  kTelPhbDeleteEntryMessage              = Succ(kTelPhbAddEntryMessage);
  kTelPhbGetAvailablePhonebooksMessage   = Succ(kTelPhbDeleteEntryMessage);
  kTelPhbSelectPhonebookMessage          = Succ(kTelPhbGetAvailablePhonebooksMessage);
  kTelPhbGetSelectedPhonebookMessage     = Succ(kTelPhbSelectPhonebookMessage);
  kTelPhbGetEntryMaxSizesMessage         = Succ(kTelPhbGetSelectedPhonebookMessage);

  kTelSndPlayKeyToneMessage              = Succ(kTelPhbGetEntryMaxSizesMessage);
  kTelSndStopKeyToneMessage              = Succ(kTelSndPlayKeyToneMessage);
  kTelSndMuteMessage                     = Succ(kTelSndStopKeyToneMessage);

  kTelUrqSmsGetUniquePartIdMessage       = Succ(kTelSndMuteMessage);

  kTelUrqClosePhoneConnectionMessage     = Succ(kTelUrqSmsGetUniquePartIdMessage);   // urgent query: ask TT to close connection with phone
  kTelUrqOpenPhoneConnectionMessage      = Succ(kTelUrqClosePhoneConnectionMessage); // urgent query: ask TT to open connection with phone
  kTelUrqIsPhoneConnectedMessage         = Succ(kTelUrqOpenPhoneConnectionMessage);  // urgent query: ask TT the connection state with phone
  kTelUrqMatchPhoneDriverMessage         = Succ(kTelUrqIsPhoneConnectedMessage);     // urgent query: ask TT to check whether the driver matches the phone
  kTelUrqCancelMessage                   = Succ(kTelUrqMatchPhoneDriverMessage);     // urgent query: ask TT to cancel an asynchronous call
  kTelUrqIsServiceAvailableMessage       = Succ(kTelUrqCancelMessage);               // urgent query: ask TT whether a service set is available
  kTelUrqIsFunctionSupportedMessage      = Succ(kTelUrqIsServiceAvailableMessage);   // urgent query: ask TT whether a function is supported

  kTelUrqGetTTStatusMessage              = Succ(kTelUrqIsFunctionSupportedMessage);  // urgent query: ask TT is status
  kTelUrqSleepMessage                    = Succ(kTelUrqGetTTStatusMessage);          // urgent query: warn TT the palm is going asleep
  kTelUrqWakeMessage                     = Succ(kTelUrqSleepMessage);                // urgent query: warn TT the palm is awaking
  kTelUrqDetachMessage                   = Succ(kTelUrqWakeMessage);                 // urgent query: warn TT associated application is about to quit
  kTelUrqStopTTMessage                   = Succ(kTelUrqDetachMessage);               // urgent query: ask TT to stop

  kTelInfGetInformationMessage           = Succ(kTelUrqStopTTMessage);

  kTelOemCallMessage                     = Succ(kTelInfGetInformationMessage);       // oem calls

  kTelLastMessage                        = kTelOemCallMessage;

type
  // basic types
  TelAppID = UInt32;

  // notification structure
  TelNotificationType = record
    notificationData:  UInt32; // associated data if any
    notificationData2: UInt32; // associated data if any
    timeStamp:         UInt32; // time stamp
    notificationId:    UInt16; // what was the associated telephony event
    priority:          UInt8;  // notification priority 0 == max, 255 == min
  end;

  // event structure
  TelEventType = record
    eType:      eventsEnum;
    penDown:    Boolean;
    tapCount:   UInt8;
    screenX:    Int16;
    screenY:    Int16;

    functionId: UInt16;  // ID of the message associated to the asynchronous function call
    transId:    UInt16;  // transId returned on asynchronous function call return
    aramP:      ^MemPtr; // parameter passed at asynchronous function call
    returnCode: Err;     // function return code, errNone if ok, else an error
  end;

  // command string
  TelSendCommandStringType = record
    commandString: PChar;  // command string to be sent
    resultString:  PChar;  // result string
    resultSize:    UInt16; // result string buffer size/max bytes retrieved on result
    timeOut:       UInt32; // milliseconds time out for command processing (before phone starts replying)
  end;

  // call state
  TelGetCallStateType = record
    state: UInt8;           // call state, see kTelCall<State> constants in TelMgr.h
    callType: UInt8;        // incoming or outgoing
    callServiceType: UInt8; // voice or data

    // outgoing or incoming call number
    numberSize: UInt8; // size of number (in), length of number + 1 (out)
    number: PChar;     // called or calling number
  end;

  // network support
  TelNwkGetNetworkNameType = record
    id: UInt32;   // network ID
    value: PChar; // name
    size: UInt16; // size of name (in), name len including '\0' (out)
  end;

  TelNwkGetNetworksType = record
    etworkIdP: ^UInt32; // network IDs array
    size: UInt8;        // size of networkIdP (in), number of network IDs (out)
  end;

  // phone location within network web
  TelNwkGetLocationType = record
     value: PChar; // current location string
     size: UInt16; // size of value (in), location len including '\0' (out)
  end;

  // change security code
  TelStyChangeAuthenticationType = record
    codeId: UInt8;  // code to be changed
    oldCode: PChar; // old code value
    newCode: PChar; // new code value
  end;

  // SMS center
  TelCfgGetSmsCenterType = record
    size: UInt8;  // size of value (in), SMS dial number len including '\0' (out)
    value: PChar; // SMS center dial number
  end;

  // phone number
  TelCfgGetPhoneNumberType = record
    size: UInt8;  // size of value (in), phone dial number len including '\0' (out)
    value: PChar; // phone dial number
  end;

  // SMS
  // SMS time
  TelSmsDateTimeType = record
    absolute: Boolean;
    dateTime: UInt32; // relative time from now, or Palm absolute time
  end;

  // SMS extensions
  TelSmsMultiPartExtensionType = record
    bytesSent: UInt16;
    partCurrent: UInt16;
    partCount: UInt16;
    partId: UInt16;
  end;

  TelSmsNbsExtensionType = record
    destPort: UInt16; // destination NBS port
    srcPort: UInt16;  // source NBS port
  end;

  TelSmsUserExtensionType = record
    extHeader: ^UInt8; // user defined extended header
    extHeaderSize: UInt8;
  end;

  TelSmsExtensionType = record
    extensionTypeId: UInt8; // what does this extension describe?

    case Integer of
      1: (mp: TelSmsMultiPartExtensionType); // multi-part SMS extension
      2: (nbs: TelSmsNbsExtensionType);      // NBS SMS extension
      3: (user: TelSmsUserExtensionType);    // User Defined SMS extension
  end;

  // Advanced parameters for GSM
  TelSmsSubmitAdvancedGSMType = record
    protocolId: UInt16;               // Reserved - not supported (Fax, paging, . . .) GSM only

    rejectDuplicatedRequest: Boolean; // GSM - Network must reject msg if the same exists
    replyPath: Boolean;               // GSM - use reply specified path

    serviceCenterNumber: PChar;       // SMS service center number
    serviceCenterNumberSize: UInt8;   // Used for decoding only
  end;

  // Advanced parameters for CDMA
  TelSmsSubmitAdvancedCDMAType = record
    manualAckRequest: Boolean;
    messageType: UInt8;               // Message Type

    deferredDate: TelSmsDateTimeType; // GSM & CDMA only Absolute or relative

    priority: UInt8;                  // CDMA & TDMA only
    privacy: UInt8;                   // CDMA & TDMA only

    alertOnDeliveryRequest: Boolean;  // CDMA & TDMA(?)

    callbackNumber: PChar;            // CDMA & TDMA only - address to reply
    callbackNumberSize: UInt8;
  end;

  // Advanced parameters for TDMA - currently identical to CDMA
  TelSmsSubmitAdvancedTDMAType = TelSmsSubmitAdvancedCDMAType;

  TelSmsSubmitMessageAdvancedParams = record
  case Integer of
    1: (advancedGSM: TelSmsSubmitAdvancedGSMType);
    2: (advancedCDMA: TelSmsSubmitAdvancedCDMAType);
    3: (advancedTDMA: TelSmsSubmitAdvancedTDMAType);
  end;

  // Submit message structure
  TelSmsSubmitMessageType = record
    version: UInt16;                    // SMS API version

    networkDeliveryRequest: Boolean;    // All - Ask a network delivery report / status report

    destinationAddress: PChar;          // length : GSM 12bytes, CDMA up to 2x64 bytes
    destinationAddressSize: UInt8;      // Used for decoding only

    dataSize: UInt16;                   // Length of data being sent
    data: ^UInt8;                       // All
    dataCodingScheme: UInt8;

    validityPeriod: TelSmsDateTimeType; // All - Absolute or relative

    // Advanced parameters
    standardType: UInt8;                // Indicates the type of advanced parameters

    advancedParams: TelSmsSubmitMessageAdvancedParams;

    extensionsCount: UInt8;             // how many extensions in this message
    extensionsP: ^TelSmsExtensionType;  // SMS extensions array: NBS, Multi-part, etc.
  end;

  // Submit message structure parameter
  TelSmsSendMessageType = record
    messageId: UInt32; // Output parameter, filled by the telephony implementation
    message: TelSmsSubmitMessageType;
  end;

  // Submitted message structure parameter
  TelSmsSubmittedMessageType = record
    index: UInt16; // Message's index on the phone
    message: TelSmsSubmitMessageType;
  end;

  // Manual acknowledge structure
  TelSmsManualAckType = record
    version: UInt16;           // SMS API version
    destinationAddress: PChar; // length : GSM 12bytes, CDMA up to 2x64 bytes
    messagesId: UInt32;        // Message Id of message to be acknowledged

    dataSize: UInt16;          // Length of data being sent
    data: ^UInt8;              // All
    dataCodingScheme: UInt8;

    responseCode: UInt8;       // Value is network dependant
  end;

  TelSmsDeliveryAdvancedGSMType = record
    protocolId: UInt16; // reserved - not supported - GSM only

    replyPath: Boolean; // GSM - must use specified reply path
    serviceCenterNumber: PChar;
    serviceCenterNumberSize: UInt8;
  end;

  TelSmsDeliveryAdvancedCDMAType = record
    messageType: UInt8;                 // Delivery Message Type

    validityPeriod: TelSmsDateTimeType; // CDMA & TDMA only

    priority: UInt8;                    // CDMA & TDMA only
    privacy: UInt8;                     // CDMA & TDMA only

    alertOnDeliveryRequest: Boolean;    // CDMA & TDMA only
    manualAckRequest: Boolean;          // CDMA

    voiceMessageNumber: UInt8;          // CDMA, TDMA, GSM

    callbackNumberSize: UInt8;
    callbackNumberAddress: PChar;       // Store callback address

    languageIndicator: UInt8;           // reserved - not supported - CDMA only
  end;

  TelSmsDeliveryAdvancedTDMAType = record
    messageType: UInt8;                 // Delivery Message Type

    validityPeriod: TelSmsDateTimeType; // CDMA & TDMA only

    priority: UInt8;                    // CDMA & TDMA only
    privacy: UInt8;                     // CDMA & TDMA only

    manualAckRequest: Boolean;          // CDMA

    alertOnDeliveryRequest: Boolean;    // CDMA & TDMA only
    voiceMessageNumber: UInt8;          // CDMA, TDMA, GSM

    callbackNumberSize: UInt8;
    callbackNumberAddress: PChar;       // Store callback address
  end;

  TelSmsDeliveryMessageAdvancedParams = record
    case Integer of
      1: (advancedGSM: TelSmsDeliveryAdvancedGSMType);
      2: (advancedCDMA: TelSmsDeliveryAdvancedCDMAType);
      3: (advancedTDMA: TelSmsDeliveryAdvancedTDMAType);
  end;

  // Delivery message structure
  TelSmsDeliveryMessageType = record

    version: UInt16;    // SMS API version
    index: UInt16;      // SMS index on the phone storage

    messageIdentifier: UInt32;

    timeStamp: TelSmsDateTimeType;

    dataSize: UInt16;                  // Length of data being sent
    data: ^UInt8;
    dataCodingScheme: UInt8;           // enum All

    originatingAddressSize: UInt8;
    originatingAddress: PChar;         // Store originating address (delivery message)

    otherToReceive: Boolean;           // GSM & CDMA & TDMA(?)

    reportDeliveryIndicator: Boolean;  // All

    // Advanced parameters
    standardType: UInt8;
    advancedParams: TelSmsDeliveryMessageAdvancedParams;

    extensionsCount: UInt8;            // how many extensions in this message
    extensionsP: ^TelSmsExtensionType; // SMS extensions array: NBS, Multi-part, etc.
  end;

  // Report message structure
  TelSmsReportType = record
    version: UInt16;
    index: UInt16;             // SMS index on the phone storage
    reportType: UInt8;         // Delivery report type
    messageId: UInt32;

    dataSize: UInt16;          // Length of data being sent
    data: ^UInt8;
    dataCodingScheme: UInt8;

    originatingAddress: PChar; // Store originating address
    originatingAddressSize: UInt8;

    report: UInt8;

    timeStamp: TelSmsDateTimeType; // Time when SC received the corresponding sent message
  end;

  // multiple messages read
  TelSmsReadMessagesType = record
    first: UInt16; // first message physical index, zero based
    count: UInt16; // number of messages to read
    messagesP: ^TelSmsDeliveryMessageType; // messages array
  end;

  // multiple reports read
  TelSmsReadReportsType = record
    first: UInt16; // first report physical index, zero based
    count: UInt16; // number of reports to read
    reportsP: ^TelSmsReportType; // reports array
  end;

  // multiple submitted messages read
  TelSmsReadSubmittedMessagesType = record
    first: UInt16; // first sent message physical index, zero based
    count: UInt16; // number of sent messages to read
    submittedsP: ^TelSmsSubmittedMessageType; // sent messages array
  end;

  // get messages number/slots in selected storage
  TelSmsGetMessageCountType = record
    messageType: UInt8; // report, submitted, etc.
    slots: UInt16;      // number of entries in the selected storage
    count: UInt16;      // number of messages present in the selected storage
  end;

  // delete message selected storage
  TelSmsDeleteMessageType = record
    messageType: UInt8; // report, submitted, etc.
    index: UInt16;      // physical index in storage, zero based
  end;

  // available storage list
  TelSmsGetAvailableStorageType = record
    count: UInt16;     // storage IDs array size (in), retrieved IDs number (out)
    storagesP: ^UInt8; //  storage IDs array
  end;

  // emergency call support
  TelEmcGetNumberType = record
    index: UInt8; // EMC number index, zero based
    size: UInt8;  // EMC dial number len including '\0' (out), value size (in)
    value: PChar; // EMC dial number
  end;

  TelEmcSetNumberType = record
    index: UInt8; // EMC number index, zero based
    value: PChar; // EMC dial number
  end;

  // speech call support
  TelSpcGetCallerNumberType = record
    size: UInt8;  // dial number len including '\0' (out), value size (in)
    value: PChar; // dial number
  end;

  TelSpcPlayDTMFType = record
    keyTone: UInt8;   // keytone to be played
    duration: UInt32; // play duration in 10 ms multiple
  end;

  // phonebook support
  TelPhbEntryType = record
    phoneIndex: UInt16;    // entry's index in the phonebook, zero based
    fullName: PChar;
    fullNameSize: UInt8;   // name len including '\0' (out), name size (in)
    dialNumber: PChar;
    dialNumberSize: UInt8; // dial number len including '\0' (out), dialNumber size (in)
  end;

  TelPhbGetEntryCountType = record
    slots: UInt16; // number of entries in the selected phonebook
    count: UInt16; // number of name/addresse pairs present in the selected phonebook
  end;

  TelPhbGetEntriesType = record
    first: UInt16; // first entry physical index, zero based
    count: UInt16; // number of name/addresse pairs
    entriesP: ^TelPhbEntryType; // name/addresse pairs array
  end;

  TelPhbGetAvailablePhonebooksType = record
    count: UInt16;       // size of array (in), number of IDs (out)
    phonebooksP: ^UInt8; // phonebook IDs array
  end;

  TelPhbGetEntryMaxSizesType = record
    fullNameMaxSize: UInt8;   // name len including '\0'
    dialNumberMaxSize: UInt8; // dial number len including '\0'
  end;

  // sound support
  TelSndPlayKeyToneType = record
    keycode: UInt8; // what key
    type_: UInt8;   // what tone type
  end;

  // information support
  TelInfGetInformationType = record
    infoType: UInt8; // expected information, can be up to 2Kb (!)
    size: UInt16;    // value len including '\0' (out), value size (in)
    value: PChar;    //  returned information string
  end;

  // data call support
  TelDtcCallNumberType = record
    dialNumberP: PChar; // number to dial
    lineId: UInt8;      // resulting line id, sent back in event
  end;

  TelDtcSendDataType = record
    data: ^UInt8; // data to be sent
    size: UInt32; // data size (in), data sent (out)
  end;

  TelDtcReceiveDataType = record
    data: ^UInt8;   // buffer to receive data
    size: UInt32;   // size of buffer (in), data size (out)
    timeOut: Int32; // milliseconds to wait before exiting
  end;

  // OEM support
  TelOemCallType = record
    OemId: UInt32;   // unique ID of OEM function set
    funcId: UInt8;   // function ID
    paramP: Pointer; // parameters block
  end;

implementation

end.
