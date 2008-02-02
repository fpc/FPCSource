(******************************************************************************
 *
 * Copyright (c) 1997-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SmsLib.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for PalmOS SMS Library
 *
 * History:
 *    1/26/00 Created by Hatem Oueslati
 *    3/01/00  Ludovic Ferrandis - Add API
 *    8/28/00  Ludovic Ferrandis - Update structure
 *
 *****************************************************************************)

unit smslib;

interface

uses  palmos, errorbase, exgmgr, telephonymgrui, telephonymgrtypes, telephonymgr;

// The Sms library is used as an Exchange library. ExgLib.h defines all the
// primary entrypoints into the library. The rest of this include file defines the
// specials opCodes used in the ExgControl function and the structure used in the
// socketRef field of the Exchange Manager Socket structure.

(****************************************************************************
 *
 * ExgLibControl opcodes
 *
 ****************************************************************************)

// Those Op codes take SmsPrefType as parameter
const
  exgLibSmsPrefGetOp            = exgLibCtlSpecificOp or 1;
  exgLibSmsPrefGetDefaultOp     = exgLibCtlSpecificOp or 2;
  exgLibSmsPrefSetOp            = exgLibCtlSpecificOp or 3;

// This Op code takes a network type constant as parameter kSmsNetworkXxx
  exgLibSmsPrefDisplayOp        = exgLibCtlSpecificOp or 4;

// This Op code takes an UInt16 as parameter
  exgLibSmsIncompleteGetCountOp = exgLibCtlSpecificOp or 5;

// This OpCode takes a SmsID (UInt16) as parameter
  exgLibSmsIncompleteDeleteOp   = exgLibCtlSpecificOp or 6;

(****************************************************************************
 *
 * Types and Constants
 *
 ****************************************************************************)

const
  kSmsLibName = 'SMS Library'; // name of Sms library

// Feature Creators and numbers, for use with the FtrGet() call. This
// feature can be obtained to get the current version of the Sms Library
const
  kSmsFtrNumVersion       = UInt16(0);

// $MMmfsbbb, where MM is major version, m is minor version
// f is bug fix, s is stage: 3-release, 2-beta, 1-alpha, 0-development,
// bbb is build number for non-releases
// V1.12b3   would be: $01122003
// V2.00a2   would be: $02001002
// V1.01     would be: $01013000

const
  kSmsMessageRegExtensionType = 'sms';
  kSmsReportRegExtensionType  = 'rps';
  kSmsExtensionTypeLength     = 3;

  kSmsScheme                  = '_sms';

  kSmsMaxPhoneSize            = kTelMaxPhoneNumberLen + 1; // Max length for Phone number

  kSmsNBSConverter            = UInt8(0); // NBS header will be add to the data
  kSmsNoConverter             = UInt8(1); // No header will be add to the data.

  kSmsRowDataEncoding         = UInt8(0); // 8 bit encoding scheme
  kSmsTextEncoding            = UInt8(1); // 7 bit compressed encoding scheme.

  kSmsNetworkAuto             = UInt8(-1);
  kSmsNetworkCDMA             = UInt8(kTelNwkCDMA);
  kSmsNetworkGSM              = UInt8(kTelNwkGSM);
  kSmsNetworkTDMA             = UInt8(kTelNwkTDMA);
  kSmsNetworkPDC              = UInt8(kTelNwkPDC);

  kSmsMessageType             = UInt8(0);
  kSmsIncompleteType          = UInt8(1);
  kSmsReportType              = UInt8(2);

(****************************************************************************
 *
 * Sms Error
 *
 ****************************************************************************)

const kSmsErrMaxSizeExceeded = smsErrorClass or $01; // Message exceeds maximum size supported by network

(****************************************************************************
 *
 * Data Structures
 *
 ****************************************************************************)

(****************************************************************************
 * Report parameters
 ****************************************************************************)

(* Report Parameters Type
   ---------------------- *)
type
  SmsReportParamsType = record
    timeStamp: UInt32;         // TimeStamp of the report (when delivered, or last attempt, ...)
    index: UInt16;             // SMS index on the phone storage
    reportType: UInt8;         // Delivery report type
    report: UInt8;             // Status report indicator
    originatingAddress: PChar; // Phone number to which belong the report (was sent)
  end;
  SmsReportParamsTag = SmsReportParamsType;
  SmsReportParamsPtr = ^SmsReportParamsType;

(****************************************************************************
 * Send parameters
 ****************************************************************************)

(* Advanced Parameters Type for TDMA & CDMA network
   ------------------------------------------------ *)
type
  SmsSendCDMAParamsType = record
   messageType: UInt8; // Message Type

   deferredDate: TelSmsDateTimeType;

   priority: UInt8;
   privacy: UInt8;

   bits: UInt8;
{
   alertOnDelivery: UInt8 ; //:1
   manualAckRequest: UInt8; //:1
   reserved: UInt8;         //:6
}
   callbackNumber: PChar; // Address to reply
  end;
  SmsSendCDMAParamsTag = SmsSendCDMAParamsType;
  SmsSendCDMAParamsPtr = ^SmsSendCDMAParamsType;

  SmsSendTDMAParamsType = SmsSendCDMAParamsType;
  SmsSendTDMAParamsPtr  = ^SmsSendCDMAParamsType;

(* Advanced Parameters Type for GSM network
   ---------------------------------------- *)

type
  SmsSendGSMParamsType = record
    protocolId: UInt16;         // Reserved - not supported (Fax, paging, . . .)
    serviceCenterNumber: PChar; // SMS Center number - Optionel

    rejectDuplicated: Boolean;  // Network must reject msg if the same exists
    replyPath: Boolean;         // Use reply specified path
  end;
  SmsSendGSMParamsTag = SmsSendGSMParamsType;
  SmsSendGSMParamsPtr = ^SmsSendGSMParamsType;


(* SMS Send Parameters Type
   ------------------------ *)

type
  SmsSendParamsType = record
    validityPeriod: TelSmsDateTimeType;  // SMS validity Period
    destinationAddress: PChar;           // recipient number -> to send the sms to

    bits: UInt8;
{
    networkDeliveryRequested: UInt8;     // (:1) Sms report wanted
    ignoreDefaultValue: UInt8;           // (:1) If false, the field validityPeriod, ackRequested, reportRequested, smsCenter are ignored.
    reserved: UInt8;                     // (:6)
}
    partCount: UInt16;                   // number of parts of the SMS (output)
    lastPart: UInt16;                    // if error, the last part sent (output)

    converter: UInt8;                    // How the data are going to be typed (0 = NBS, 1 = None)

    case Integer of                      // union 'protocol'
      1: (gsm: SmsSendGSMParamsType);
      2: (cdma: SmsSendCDMAParamsType);
      3: (tdma: SmsSendTDMAParamsType);
  end;
  SmsSendParamsTag = SmsSendParamsType;
  SmsSendParamsPtr = ^SmsSendParamsType;


(****************************************************************************
 * Receive parameters
 ****************************************************************************)

(* Advanced Parameters Type for TDMA & CDMA network
   ----------------------------------------------- *)
type
  SmsReceiveCDMAParamsType = record
    messageType: UInt8; // Delivery Message Type

    validityPeriod: TelSmsDateTimeType;

    priority: UInt8;
    privacy: UInt8;

    alertOnDeliveryRequest: Boolean;
    manualAckRequest: Boolean;

    voiceMessageNumber: UInt8;
    languageIndicator: UInt8;

    callbackNumberAddress: PChar; // Store callback address
  end;
  SmsReceiveCDMAParamsTag = SmsReceiveCDMAParamsType;
  SmsReceiveCDMAParamsPtr = ^SmsReceiveCDMAParamsType;

  SmsReceiveTDMAParamsType = SmsReceiveCDMAParamsType;
  SmsReceiveTDMAParamsPtr  = ^SmsReceiveCDMAParamsType;

(* Advanced Parameters Type for GSM network
   ---------------------------------------- *)
type
  SmsReceiveGSMParamsType = record
    protocolId: UInt16; // reserved - not supported
    serviceCenterNumber: PChar;

    replyPath: Boolean; // Must use specified reply path
  end;
  SmsReceiveGSMParamsTag = SmsReceiveGSMParamsType;
  SmsReceiveGSMParamsPtr = ^SmsReceiveGSMParamsType;

(* SMS Receive Parameters Type
   --------------------------- *)

type
  SmsReceiveParamsType = record
   timeStamp: UInt32;          // Palm
   originatingAddress: PChar;  // originating number -> to send the sms to

   bits: UInt8;
{
   leaveOnPhone: UInt8         // (:1) Received messages won't be deleted on the phone (Input)
   forceSlotMode: UInt8        // (:1) Force parsing methode to Slot Mode (default is Block mode) (Input)
   reserved: UInt8             // (:6)
}
   index: UInt16;              // Index of the storage in the mobile where the message is stored

   otherToReceive: Boolean;
   reportDeliveryIndicator: Boolean;

   case Integer of             // union 'protocol'
     1: (gsm: SmsReceiveGSMParamsType);
     2: (cdma: SmsReceiveCDMAParamsType);
     3: (tdma: SmsReceiveTDMAParamsType);
  end;
  SmsReceiveParamsTag = SmsReceiveParamsType;
  SmsReceiveParamsPtr = ^SmsReceiveParamsType;

(****************************************************************************
 * Sms parameters
 ****************************************************************************)
(* SMS Parameters Type
   ------------------- *)

type
  SmsParamsType = record
    creator: UInt32;         // MUST ALWAYS BE SET TO sysFileCSmsLib
    smsID: UInt16;           // ID of the SMS (output)

    extension: PChar;        // Extension type of the data - Optionel     (Output)
    mimeTypes: PChar;        // Mime type of object - Optionel            (Output)
    appCreator: UInt32;      // Application Creator of the target - Optionel (Output)

    dataCodingScheme: UInt8; // How SMS are going to convert the data (0 = 8 bit, 1 = 7 bit)
    networkType: UInt8;      // Indicates the type of advanced parameters (input - output)
    dataType: UInt8;         // Indicates the kind of message: Sms, incomplete Sms, Report (Output)

    nbsDestPort: UInt16;     // NBS port to use to encode the data (input) - port used in received Sms (output)
    nbsSrcPort: UInt16;      // NBS port to use to encode the data (input) - port used in received Sms (output)

    case Integer of          // union 'data'
      1: (send: SmsSendParamsType);
      2: (receive: SmsReceiveParamsType);
      3: (report: SmsReportParamsType);
  end;
  SmsParamsTag = SmsParamsType;
  SmsParamsPtr = ^SmsParamsType;

(* Preferences Type
   ------------------- *)

type
  SmsPrefType = record
    validity: UInt32;        // Validity period of SMS (relatif) in seconds
    warnOver: UInt16;        // Display an alert if sending more Sms than this value.
    leave: Boolean;          // Leave SMS on Phone
    report: Boolean;         // Ask for a network delivery report
    autoSMSC: Boolean;       // If set, don't use the value stored in smscNumber field
    smscNumber: array [1..kSmsMaxPhoneSize] of Char; // SMS Service Center. Could be null
  end;
  SmsPrefTag = SmsPrefType;
  SmsPrefPtr = ^SmsPrefType;

implementation

end.
