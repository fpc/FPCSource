{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-1999 Palm, Inc. or its subsidiaries.
 * All rights reserved.

 *
 * Portions of this file are:
 *  Copyright Counterpoint Systems Foundry, Inc. 1995, 1996
 *
 * File: IrLib.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   Include file for PalmOS IrDA Library.
 *
 * History:
 *    5/23/97 Created by Gavin Peacock
 *    2/13/98 Merged with counterpoint libraries
 *    6/24/98 Added disconnect timeout opt for IrOpen,
 *          remaining speeds.  Added parentheses to control defines.
 *
 *****************************************************************************)

unit irlib;

interface

uses palmos, libtraps, exgmgr, systemresources;

// The Ir library is used as an Exchange library. ExgLib.h defines all the
// primary entrypoints into the library. The rest of this include file defines the
// direct stack API for apps not using the Exchange interface. The Stack API
// comes after the Exchange library API in the library interface.

// name of Ir library
const
  irLibName = 'IrDA Library';

// Specific scheme for IR exg lib
const
  exgIrObexScheme = '_irobex';

// Feature Creators and numbers, for use with the FtrGet() call. This
//  feature can be obtained to get the current version of the Ir Library
const
  irFtrCreator    = sysFileCIrLib;
  irFtrNumVersion = 0; // get version of Net Library
                       // 0xMMmfsbbb, where MM is major version, m is minor version
                       // f is bug fix, s is stage: 3-release,2-beta,1-alpha,0-development,
                       // bbb is build number for non-releases
                       // V1.12b3   would be: 0x01122003
                       // V2.00a2   would be: 0x02001002
                       // V1.01     would be: 0x01013000

// Options values for IrOpen
// BDDDxxxx xxxxxxxx xxxxxxxx xxxSSSSS
// Where B=background mode, DDD=disconnect timeout, SSSSS=speed

  irOpenOptBackground   = $80000000; // Unsupported background task use
  irOpenOptDisconnect3  = $10000000; // sets amount of time in seconds
  irOpenOptDisconnect8  = $20000000; // after no activity from other
  irOpenOptDisconnect12 = $30000000; // device before disconnect is
  irOpenOptDisconnect16 = $40000000; // initiated.
  irOpenOptDisconnect20 = $50000000; // default is 40 secs
  irOpenOptDisconnect25 = $60000000;
  irOpenOptDisconnect30 = $70000000;
  irOpenOptDisconnect40 = $00000000;
  irOpenOptSpeed115200  = $0000003F; // sets max negotiated baud rate
  irOpenOptSpeed57600   = $0000001F; // default is 57600
  irOpenOptSpeed38400   = $0000000F;
  irOpenOptSpeed19200   = $00000007;
  irOpenOptSpeed9600    = $00000003;

// Option codes for ExgLibControl
// These options are all unique to the Ir transport

  irGetScanningMode  = exgLibCtlSpecificOp or 1; // returns scanning enabled
  irSetScanningMode  = exgLibCtlSpecificOp or 2; // en/disables ir scanning mode
  irGetStatistics    = exgLibCtlSpecificOp or 3; // returns performance stats
  irSetSerialMode    = exgLibCtlSpecificOp or 4; // sets driver to use direct serial
  irSetBaudMask      = exgLibCtlSpecificOp or 5; // set possible baud rates (irOpenOpts)
  irSetSupported     = exgLibCtlSpecificOp or 6; // disables the ir not supported dialog
  irSuppressScanning = exgLibCtlSpecificOp or 7; // temporarily disabled beam receive
  irRestoreScanning  = exgLibCtlSpecificOp or 8; // restore beam receive state

// structure returned by irGetStatistics
type
  IrStatsType = record
    recLineErrors: UInt16; // # of serial errors since library opend
    crcErrors: UInt16;     // # of crc errors ...
  end;

//-----------------------------------------------------------------------------
// Ir library call ID's.
//-----------------------------------------------------------------------------

// These start after the old exchange library interface. If we add any more exchange
// library traps, we'll have to separate the publicly accessible "IR Library" and the
// IrOBEX exchange library.

type
  IrLibTrapNumberEnum = Enum;

const
  irLibTrapBind = sysLibTrapCustom + 10; // these start after the ObxLib interface...
  irLibTrapUnBind = Succ(irLibTrapBind);
  irLibTrapDiscoverReq = Succ(irLibTrapUnBind);
  irLibTrapConnectIrLap = Succ(irLibTrapDiscoverReq);
  irLibTrapDisconnectIrLap = Succ(irLibTrapConnectIrLap);
  irLibTrapConnectReq = Succ(irLibTrapDisconnectIrLap);
  irLibTrapConnectRsp = Succ(irLibTrapConnectReq);
  irLibTrapDataReq = Succ(irLibTrapConnectRsp);
  irLibTrapLocalBusy = Succ(irLibTrapDataReq);
  irLibTrapMaxTxSize = Succ(irLibTrapLocalBusy);
  irLibTrapMaxRxSize = Succ(irLibTrapMaxTxSize);
  irLibTrapSetDeviceInfo = Succ(irLibTrapMaxRxSize);
  irLibTrapIsNoProgress = Succ(irLibTrapSetDeviceInfo);
  irLibTrapIsRemoteBusy = Succ(irLibTrapIsNoProgress);
  irLibTrapIsMediaBusy = Succ(irLibTrapIsRemoteBusy);
  irLibTrapIsIrLapConnected = Succ(irLibTrapIsMediaBusy);
  irLibTrapTestReq = Succ(irLibTrapIsIrLapConnected);
  irLibTrapIAS_Add = Succ(irLibTrapTestReq);
  irLibTrapIAS_Query = Succ(irLibTrapIAS_Add);
  irLibTrapIAS_SetDeviceName = Succ(irLibTrapIAS_Query);
  irLibTrapIAS_Next = Succ(irLibTrapIAS_SetDeviceName);
  irLibTrapIrOpen = Succ(irLibTrapIAS_Next);
  irLibTrapHandleEvent = Succ(irLibTrapIrOpen);
  irLibTrapWaitForEvent = Succ(irLibTrapHandleEvent);

  irLibTrapLast = Succ(irLibTrapWaitForEvent);

(****************************************************************************
 *
 * Types and Constants
 *
 ****************************************************************************)

// Maximum size of packet that can be sent at connect time (ConnectReq or
// ConnectRsp) for IrLMP and Tiny TP connections.

const
  IR_MAX_CON_PACKET     = 60;
  IR_MAX_TTP_CON_PACKET = 52;
  IR_MAX_TEST_PACKET    = 376;
  IR_MAX_DEVICE_INFO    = 23;

// Size of the device list used in discovery process

  IR_DEVICE_LIST_SIZE = 6;

(*---------------------------------------------------------------------------
 *
 * Maximum size of the XID info field used in a discovery frame. The XID
 * info field contains the device hints and nickname.
 *)

  IR_MAX_XID_LEN = 23;

// Maximum allowed LSAP in IrLMP

  IR_MAX_LSAP = $6f;

// The following are used to access the hint bits in the first byte
// of the Device Info field of an XID frame (IrDeviceInfo).

  IR_HINT_PNP      = $01;
  IR_HINT_PDA      = $02;
  IR_HINT_COMPUTER = $04;
  IR_HINT_PRINTER  = $08;
  IR_HINT_MODEM    = $10;
  IR_HINT_FAX      = $20;
  IR_HINT_LAN      = $40;
  IR_HINT_EXT      = $80;

(* The following are used to access the hint bits in the second byte
 * of the Device Info field of an XID frame (IrDeviceInfo). Note
 * that LM_HINT_EXT works for all hint bytes.
 *)

  IR_HINT_TELEPHONY = $01;
  IR_HINT_FILE      = $02;
  IR_HINT_IRCOMM    = $04;
  IR_HINT_MESSAGE   = $08;
  IR_HINT_HTTP      = $10;
  IR_HINT_OBEX      = $20;

(*---------------------------------------------------------------------------
 *
 * Status of a stack operation or of the stack.
 *)

type
  IrStatus = UInt8;

const
  IR_STATUS_SUCCESS        = 0; // Successful and complete
  IR_STATUS_FAILED         = 1; // Operation failed
  IR_STATUS_PENDING        = 2; // Successfully started but pending
  IR_STATUS_DISCONNECT     = 3; // Link disconnected
  IR_STATUS_NO_IRLAP       = 4; // No IrLAP Connection exists
  IR_STATUS_MEDIA_BUSY     = 5; // IR Media is busy
  IR_STATUS_MEDIA_NOT_BUSY = 6; // IR Media is not busy
  IR_STATUS_NO_PROGRESS    = 7; // IrLAP not making progress
  IR_STATUS_LINK_OK        = 8; // No progress condition cleared
  // IR_STATUS_RESERVED1 = 9;  // Reserved for future use
  // IR_STATUS_RESERVED2 = 10; // Reserved for future use

(*---------------------------------------------------------------------------
 *
 * Character set for user strings. These are definitions for the character
 * set in Nicknames and in IAS attributes of type User String.
 *)

type
  IrCharSet = UInt8;

const
  IR_CHAR_ASCII      = 0;
  IR_CHAR_ISO_8859_1 = 1;
  IR_CHAR_ISO_8859_2 = 2;
  IR_CHAR_ISO_8859_3 = 3;
  IR_CHAR_ISO_8859_4 = 4;
  IR_CHAR_ISO_8859_5 = 5;
  IR_CHAR_ISO_8859_6 = 6;
  IR_CHAR_ISO_8859_7 = 7;
  IR_CHAR_ISO_8859_8 = 8;
  IR_CHAR_ISO_8859_9 = 9;
  IR_CHAR_UNICODE    = $ff;

(*---------------------------------------------------------------------------
 *
 * All indication and confirmations are sent to the IrLMP/TTP connections
 * through one callback function. The types of the events passed are
 * defined below. Applications should ignore events listed as "reserved"
 * as well as events not listed at all, since we may add more events in
 * future versions of the IR library.
 *)


type
  IrEvent = UInt8;

const
  LEVENT_LM_CON_IND     = 0;
  LEVENT_LM_DISCON_IND  = 1;
  LEVENT_DATA_IND       = 2;
  LEVENT_PACKET_HANDLED = 3;
  LEVENT_LAP_CON_IND    = 4;
  LEVENT_LAP_DISCON_IND = 5;
  LEVENT_DISCOVERY_CNF  = 6;
  LEVENT_LAP_CON_CNF    = 7;
  LEVENT_LM_CON_CNF     = 8;
  LEVENT_STATUS_IND     = 9;
  LEVENT_TEST_IND       = 10;
  LEVENT_TEST_CNF       = 11;
  // LEVENT_RESERVED1 = 12; // reserved for future use
  LEVENT_LM_SEND_IND    = 13; // added in Palm OS 4.0

// LmConnect flags - used internally

  LCON_FLAGS_TTP = $02;

(****************************************************************************
 *
 * IAS Types and Constants
 *
 ****************************************************************************)

// Maximum size of a query that observes the IrDA Lite rules

  IR_MAX_QUERY_LEN = 61;

// Maximum values for IAS fields. IR_MAX_IAS_NAME is the maximum allowable
// size for IAS Object names and Attribute names.

  IR_MAX_IAS_NAME   = 60;
  IR_MAX_ATTRIBUTES = 255;

(* Maximum size of an IAS attribute that fits within the IrDA Lite rules.
 * Even though attribute values can be larger IrDA Lite highly recommends
 * that the total size of an attribute value fit within one 64 byte packet
 * thus, the allowable size is 56 bytes or less. This size is enforced by the
 * code.
 *)

  IR_MAX_IAS_ATTR_SIZE = 56;

// Type of the IAS entry. This is the value returned for type when parsing
// the results buffer after a successful IAS Query.

  IAS_ATTRIB_MISSING      = 0;
  IAS_ATTRIB_INTEGER      = 1;
  IAS_ATTRIB_OCTET_STRING = 2;
  IAS_ATTRIB_USER_STRING  = 3;
  IAS_ATTRIB_UNDEFINED    = $ff;

// Ias Return Codes. One of these values will be found in the IAS Query
// structure in the retCode field after a successful IAS Query.

  IAS_RET_SUCCESS        = 0;   // Query operation is successful
  IAS_RET_NO_SUCH_CLASS  = 1;   // Query failed no such class exists
  IAS_RET_NO_SUCH_ATTRIB = 2;   // Query failed no such attribute exists
  IAS_RET_UNSUPPORTED    = $ff; // Query failed operation is unsupported

// IAS Get Value By Class opcode number

  IAS_GET_VALUE_BY_CLASS = 4;

(****************************************************************************
 *
 * Data Structures
 *
 ****************************************************************************)

// stack functions use a diferent type for booleans
type
  BOOL = Int16;

(*---------------------------------------------------------------------------
 *
 * ListEntry is used internally by the stack
 *)

  _ListEntry = record
    Flink: ^ListEntry;
    Blink: ^ListEntry;
  end;

  ListEntry = _ListEntry;

// Forward declaration of the IrConnect structure

  IrConnectPtr = ^_hconnect;

(*---------------------------------------------------------------------------
 *
 * Packet Structure for sending IrDA packets.
 *)

  _IrPacket = record
    (* The node field must be the first field in the structure. It is used
     * internally by the stack
     *)
    node: ListEntry;

    (* The buff field is used to point to a buffer of data to send and len
     * field indicates the number of bytes in buff.
     *)
    buff: ^UInt8;
    len: UInt16;

    (*==================  For Internal Use Only =======================
     *
     * The following is used internally by the stack and should not be
     * modified by the upper layer.
     *
     *==================================================================*)

    origin: IrConnectPtr;             // Pointer to connection which owns packet
    headerLen: UInt8;               // Number of bytes in the header
    header: array [0..13] of UInt8; // Storage for the header
    reserved: UInt8;                // Explicitly account for 16-bit alignment padding
  end;

  IrPacket = _IrPacket;

(*---------------------------------------------------------------------------
 *
 * 32-bit Device Address
 *)

  IrDeviceAddr = record
    case Integer of
      0: (u8: array [0..3] of UInt8);
      1: (u16: array [0..1] of UInt16);
      2: (u32: UInt32);
  end;

(*---------------------------------------------------------------------------
 *
 * The information returned for each device discovered during discovery.
 * The maximum size of the xid field is 23. This holds the hints and
 * the nickname.
 *)

  IrDeviceInfo = record
    hDevice: IrDeviceAddr;                     // 32-bit address of device
    len: UInt8;                                // Length of xid
    xid: array [0..IR_MAX_XID_LEN-1] of UInt8; // XID information
  end;

(*---------------------------------------------------------------------------
 *
 * List of Device Discovery info elements.
 *)

  IrDeviceList = record
    nItems: UInt8;                                         // Number items in the list
    reserved: UInt8;                                       // Explicitly account for 16-bit alignment padding
    dev: array [0..IR_DEVICE_LIST_SIZE-1] of IrDeviceInfo; // Fixed size in IrDA Lite
  end;

(*---------------------------------------------------------------------------
 *
 * Callback Parameter Structure is used to pass information from the stack
 * to the upper layer of the stack (application). Not all fields are valid
 * at any given time. The type of event determines which fields are valid.
 *)

  IrCallBackParms = record
    event: IrEvent;            // Event causing callback
    reserved1: UInt8;          // Explicitly account for 16-bit alignment padding
    rxBuff: ^UInt8;            // Receive buffer already advanced to app data
    rxLen: UInt16;             // Length of data in receive buffer
    packet: ^IrPacket;         // Pointer to packet being returned
    deviceList: ^IrDeviceList; // Pointer to discovery device list
    status: IrStatus;          // Status of stack
    reserved2: UInt8;          // Explicitly account for 16-bit alignment padding
  end;
  IrCallBackParmsPtr = ^IrCallBackParms;

(* The definitions for the callback function is given below. How the
 * callback function is used in conjuction with the stack functions is
 * given below in the Callback Reference.
 *)

  IrCallBack = procedure(p1: IrConnectPtr; p2: IrCallBackParmsPtr);

(*---------------------------------------------------------------------------
 *
 * Definition of IrConnect structure. This structure is used to manage an
 * IrLMP or Tiny TP connection.
 *)

  _hconnect = record
    lLsap: UInt8; // Local LSAP this connection will listen on
    rLsap: UInt8; // Remote Lsap

    (*==================  For Internal Use Only =======================
     *
     * The following is used internally by the stack and should not be
     * modified by the user.
     *
     *==================================================================*)

    flags: UInt8;         // Flags containing state, type, etc.
    reserved: UInt8;      // Explicitly account for 16-bit alignment padding
    callBack: IrCallBack; // Pointer to callback function

    (* Tiny TP fields *)
    packet: IrPacket;     // Packet for internal use
    packets: ListEntry;   // List of packets to send
    sendCredit: UInt16;   // Amount of credit from peer
    availCredit: UInt8;   // Amount of credit to give to peer
    dataOff: UInt8;       // Amount of data less than IrLAP size
  end;

  IrConnect = _hconnect;

(****************************************************************************
 *
 * IAS Data Strucutres
 *
 ****************************************************************************)

(*---------------------------------------------------------------------------
 *
 * The LmIasAttribute is a strucutre that holds one attribute of an IAS
 * object.
 *)

  _IrIasAttribute = record
    name: ^UInt8;     // Pointer to name of attribute
    len: UInt8;       // Length of attribute name
    reserved1: UInt8; // Explicitly account for 16-bit alignment padding
    value: ^UInt8;    // Hardcode value (see below)
    valLen: UInt8;    // Length of the value.
    reserved2: UInt8; // Explicitly account for 16-bit alignment padding
  end;

  IrIasAttribute = _IrIasAttribute;

(* The value field of the IrIasAttribute structure is a hard coded string
 * which represents the actual bytes sent over the IR for the attribute
 * value. The value field contains all the bytes which represent an
 * attribute value based on the transmission format described in section
 * 4.3 of the IrLMP specification. An example of a user string is given
 * below.
 *
 * User String:
 *   1 byte type,  1 byte Char set, 1 byte length, length byte string
 *
 * Example of an user string "Hello World" in ASCII
 *
 * U8 helloString[] = {
 *    IAS_ATTRIB_USER_STRING,IR_CHAR_ASCII,11,
 *    'H','e','l','l','o',' ','W','o','r','l','d'
 * };
 *)

(*---------------------------------------------------------------------------
 *
 * The LmIasObject is storage for an IAS object managed by the local
 * IAS server.
 *)

  _IrIasObject = record
    name: ^UInt8;             // Pointer to name of object
    len: UInt8;               // Length of object name

    nAttribs: UInt8;          // Number of attributes
    attribs: ^IrIasAttribute; // A pointer to an array of attributes
  end;

  IrIasObject = _IrIasObject;

(*---------------------------------------------------------------------------
 *
 * Forward declaration of a structure used for performing IAS Queries so
 * that a callback type can be defined for use in the structure.
 *)

type
  IrIasQueryCallBack = procedure(p: IrStatus);

(*---------------------------------------------------------------------------
 *
 * Actual definition of the IrIasQuery structure.
 *)

  _IrIasQuery = record

    (* Query fields. The query buffer contains the class name and class
     * attribute whose value is being queried it is as follows:
     *
     * 1 byte - Length of class name
     * "Length" bytes - class name
     * 1 byte - length of attribute name
     * "Length" bytes - attribute name
     *
     * queryLen - contains the total number of byte in the query
     *)
    queryLen: UInt8;       // Total length of the query
    reserved: UInt8;       // Explicitly account for 16-bit alignment padding
    queryBuf: ^UInt8;      // Points to buffer containing the query

    (* Fields for the query result *)
    resultBufSize: UInt16; // Size of the result buffer
    resultLen: UInt16;     // Actual number of bytes in the result buffer
    listLen: UInt16;       // Number of items in the result list.
    offset: UInt16;        // Offset into results buffer
    retCode: UInt8;        // Return code of operation
    overFlow: UInt8;       // Set TRUE if result exceeded result buffer size
    result: ^UInt8;        // Pointer to buffer containing result;

    (* Pointer to callback function *)
    callBack: IrIasQueryCallBack;
  end;

  IrIasQuery = _IrIasQuery;

(****************************************************************************
 *
 * Function Reference
 *
 ****************************************************************************)

(*---------------------------------------------------------------------------
 *
 * Prototype:     Err   IrOpen(UInt16 refnum, UInt32 options)
 *
 * Description:   Open the Ir library. This allocates the global memory
 *                for the ir stack and reserves and system resources it
 *                requires. This must be done before any other ir libary
 *                calls are made.
 *
 * Parameters:    refNum - ir library refNum
 *
 *                options - open options flags
 *
 *
 * Return Values: zero if no error or exgErrStackInit
 *
 *)

function IrOpen(refnum: UInt16; options: UInt32): Err; syscall irLibTrapIrOpen;

(*---------------------------------------------------------------------------
 *
 * Prototype:     Err   IrClose(UInt16 refnum)
 *
 * Description:   Close the Ir library. This releases the global memory
 *                for the ir stack and any system resources it uses.
 *                This must be called when an application is done with the
 *                ir library.
 *
 * Parameters:    refNum - ir library refNum
 *
 * Return Values: zero if no error
 *
 *)

function IrClose(refnum: UInt16): Err; syscall sysLibTrapClose;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrBind(UInt16 refNum,IrConnect* con,
 *                                 IrCallback callBack)
 *
 * Description:   Obtain a local LSAP selector and register the connection
 *                with the protocol stack. This IrConnect structure will be
 *                initialized. Any values stored in the structure will be
 *                lost. The assigned LSAP will be in the lLsap field of con.
 *                The type of the connection will be set to IrLMP. The
 *                IrConnect must be bound to the stack before it can be used.
 *
 * Parameters:    refNum - ir library refNum
 *
 *                con - pointer to IrConnect structure.
 *
 *                callBack - pointer to a callBack function that handles
 *                the indications and confirmation from the protocol stack.
 *
 * Return Values: IR_STATUS_SUCCESS - operation completed successfully.
 *                The assigned LSAP can be found in con->lLsap.
 *
 *                IR_STATUS_FAILED - the operation failed for one of the
 *                following reasons:
 *                    - con is already bound to the stack
 *                    - no room in the connection table
 *)

function IrBind(refNum: UInt16; var con: IrConnect; callBack: IrCallBack): IrStatus; syscall irLibTrapBind;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrUnbind(UInt16 refNum,IrConnect* con)
 *
 * Description:   Unbind the IrConnect structure from the protocol stack
 *                freeing it's LSAP selector.
 *
 * Parameters:     refNum - ir library refNum
 *
 *                  con - pointer to IrConnect structure to unbind
 *
 * Return Values: IR_STATUS_SUCCESS - operation competed succesfully
 *
 *                IR_STATUS_FAILED - operation failed
 *                either because the IrConnect structure was not bound
 *                or the lLsap field contained an invalid number.
 *)

function IrUnbind(refNum: UInt16; var con: IrConnect): IrStatus; syscall irLibTrapUnBind;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrDiscoverReq(UInt16 refNum,IrConnect* con)
 *
 * Description:   Start an IrLMP discovery process. The result will be
 *                signaled via the callBack function specified in the
 *                IrConnect structure with the event LEVENT_DISCOVERY_CNF.
 *                Only one discovery can be invoked at a time.
 *
 * Parameters:     refNum - ir library refNum
 *
 *                 con - pointer to a bound IrConnect structure.
 *
 * Return Values: IR_STATUS_PENDING - operation is started successfully
 *                result returned via callback.
 *
 *                IR_STATUS_MEDIA_BUSY - operation failed because the media
 *                is busy. Media busy is caused by one of the following
 *                reasons:
 *                    - Other devices are using the IR medium.
 *                    - A discovery process is already in progress
 *                    - An IrLAP connection exists.
 *
 *                IR_STATUS_FAILED - operation failed
 *                because the IrConnect structure is not bound to the stack.
 *)

function IrDiscoverReq(refNum: UInt16; var con: IrConnect): IrStatus; syscall irLibTrapDiscoverReq;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrConnectIrLap(UInt16 refNum,IrDeviceAddr deviceAddr)
 *
 * Description:   Start an IrLAP connection. The result is signaled to all
 *                bound IrConnect structures via the callback function. The
 *                callback event is LEVENT_LAP_CON_CNF if successful or
 *                LEVENT_LAP_DISCON_IND if unsuccessful.
 *
 * Parameters::   refNum - ir library refNum
 *
 *                deviceAddr - 32-bit address of device to which connection
 *                should be made.
 *
 * Return Values: IR_STATUS_PENDING - operation started successfully and
 *                callback will be called with result.
 *
 *                IR_STATUS_MEDIA_BUSY - operation failed to start because
 *                the IR media is busy. Media busy is caused by one of the
 *                following reasons:
 *                    - Other devices are using the IR medium.
 *                    - An IrLAP connection already exists
 *                    - A discovery process is in progress
 *)

function IrConnectIrLap(refNum: UInt16; deviceAddr: IrDeviceAddr): IrStatus; syscall irLibTrapConnectIrLap;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrDisconnectIrLap(UInt16 refNum)
 *
 * Description:   Disconnect the IrLAP connection. When the IrLAP connection
 *                goes down the callback of all bound IrConnect structures
 *                is called with event LEVENT_LAP_DISCON_IND.
 *
 * Parameters:    refNum - ir library refNum
 *
 * Return Values: IR_STATUS_PENDING - operation started successfully and
 *                the all bound IrConnect structures will be called back
 *                when complete.
 *
 *                IR_STATUS_NO_IRLAP - operation failed because no IrLAP
 *                connection exists.
 *)

function IrDisconnectIrLap(refNum: UInt16): IrStatus; syscall irLibTrapDisconnectIrLap;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrConnectReq(UInt16 refNum,
 *                                       IrConnect* con,
 *                                       IrPacket* packet,
 *                                       UInt8 credit);
 *
 * Description:   Request an IrLMP or TinyTP connection. The result is
 *                is signaled via the callback specified in the IrConnect
 *                structure. The callback event is LEVENT_LM_CON_CNF
 *                indicates that the connection is up and LEVENT_LM_DISCON_IND
 *                indicates that the connection failed. Before calling this
 *                function the fields in the con structure must be properly
 *                set.
 *
 * Parameters:    refNum - ir library refNum
 *
 *                con - pointer to IrConnect structure for handing the
 *                the connection. The rLsap field must contain the LSAP
 *                selector for the peer on the other device. Also the type
 *                of the connection must be set. Use IR_SetConTypeLMP() to
 *                set the type to an IrLMP conneciton or IR_SetConTypeTTP()
 *                to set the type to a Tiny TP connection.
 *
 *                packet - pointer to a packet that contains connection data.
 *                Even if no connection data is needed the packet must point
 *                to a valid IrPacket structure. The packet will be returned
 *                via the callback with the LEVENT_PACKET_HANDLED event if no
 *                errors occur. The maximum size of the packet is
 *                IR_MAX_CON_PACKET for an IrLMP connection or
 *                IR_MAX_TTP_CON_PACKET for a Tiny TP connection.
 *
 *                credit - initial amount of credit advanced to the other side.
 *                Must be less than 127. It is ANDed with 0x7f so if it is
 *                greater than 127 unexpected results will occur. This
 *                parameter is ignored if the Connection is an IrLMP connection.
 *
 * Return Values: IR_STATUS_PENDING - operation has been started successfully
 *                and the result will be returned via the callback function with
 *                the event LEVENT_LM_CON_CNF if the connection is made or
 *                LEVENT_LM_DISCON_IND if connection fails. The packet is returned
 *                via the callback with the event LEVENT_PACKET_HANDLED.
 *
 *                IR_STATUS_FAILED - operation failed because of one of the
 *                reasons below. Note that the packet is
 *                available immediately:
 *                   - Connection is busy (already involved in a connection)
 *                   - IrConnect structure is not bound to the stack
 *                   - Packet size exceeds maximum allowed.
 *
 *                IR_STATUS_NO_IRLAP - operation failed because there is no
 *                IrLAP connection (the packet is available immediately).
 *)

function IrConnectReq(refNum: UInt16; var con: IrConnect; var packet: IrPacket; credit: UInt8): IrStatus; syscall irLibTrapConnectReq;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrConnectRsp(UInt16 refNum,
 *                                       IrConnect* con,
 *                                       IrPacket* packet,
 *                                       UInt8 credit);
 *
 * Description:   Accept an incoming connection that has been signaled via
 *                the callback with the event LEVENT_LM_CON_IND. IR_ConnectRsp
 *                can be called during the callback or later to accept
 *                the connection. The type of the connection must already have
 *                been set to IrLMP or Tiny TP before LEVENT_LM_CON_IND event.
 *
 * Parameters:    refNum - ir library refNum
 *
 *                con - pointer to IrConnect structure to managed connection.
 *
 *                packet - pointer to a packet that contains connection data.
 *                Even if no connection data is needed the packet must point
 *                to a valid IrPacket structure. The packet will be returned
 *                via the callback with the LEVENT_PACKET_HANDLED event if no
 *                errors occur. The maximum size of the packet is
 *                IR_MAX_CON_PACKET for an IrLMP connection or
 *                IR_MAX_TTP_CON_PACKET for a Tiny TP connection.
 *
 *                credit - initial amount of credit advanced to the other side.
 *                Must be less than 127. It is ANDed with 0x7f so if it is
 *                greater than 127 unexpected results will occur. This
 *                parameter is ignored if the Connection is an IrLMP connection.
 *
 * Return Values: IR_STATUS_PENDING - response has been started successfully
 *                and the packet is returned via the callback with the event
 *                LEVENT_PACKET_HANDLED.
 *
 *                IR_STATUS_FAILED - operation failed because of one of the
 *                reasons below . Note that the packet is
 *                available immediately:
 *                   - Connection is not in the proper state to require a
 *                     response.
 *                   - IrConnect structure is not bound to the stack
 *                   - Packet size exceeds maximum allowed.
 *
 *                IR_STATUS_NO_IRLAP - operation failed because there is no
 *                IrLAP connection (Packet is available immediately).
 *)

function IrConnectRsp(refNum: UInt16; var con: IrConnect; var packet: IrPacket; credit: UInt8): IrStatus; syscall irLibTrapConnectRsp;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IR_DataReq(IrConnect* con,
 *                                    IrPacket* packet);
 *
 * Description:   Send a data packet. The packet is owned by the stack until
 *                it is returned via the callback with event
 *                LEVENT_PACKET_HANDLED. The largest packet that can be sent
 *                is found by calling IR_MaxTxSize().
 *
 * Parameters:    refNum - ir library refNum
 *
 *                con - pointer to IrConnect structure that specifies the
 *                connection over which the packet should be sent.
 *
 *                packet - pointer to a packet that contains data to send.
 *                The packet should exceed the max size found with
 *                IR_MaxTxSize().
 *
 * Return Values: IR_STATUS_PENDING - packet has been queued by the stack.
 *                The packet will be returned via the callback with event
 *                LEVENT_PACKET_HANDLED.
 *
 *
 *                IR_STATUS_FAILED - operation failed and packet is available
 *                immediately. Operation failed for one of the following
 *                reasons:
 *                  - IrConnect structure is not bound to the stack (error
 *                    checking only)
 *                  - packet exceeds the maximum size (error checking only)
 *                  - IrConnect does not represent an active connection
 *)

function IrDataReq(refNum: UInt16; var con: IrConnect; var packet: IrPacket): IrStatus; syscall irLibTrapDataReq;

(*---------------------------------------------------------------------------
 *
 * Prototype:     void IrAdvanceCredit(IrConnect* con,
 *                                      UInt8 credit);
 *
 * Description:   Advance credit to the other side. The total amount of
 *                credit should not exceed 127. The credit passed by this
 *                function is added to existing available credit which is
 *                the number that must not exceed 127. This function
 *                only makes sense for a Tiny TP connection.
 *
 * Parameters:    con - pointer to IrConnect structure representing
 *                connection to which credit is advanced.
 *
 *                credit - number of credit to advance.
 *
 * Return Values: void
 *)

// IrAdvanceCredit(con, credit) {(con)->availCredit += (credit);}

(*---------------------------------------------------------------------------
 *
 * Prototype:     void IrLocalBusy(UInt16 refNum,BOOL flag);
 *
 * Description:   Set the IrLAP local busy flag. If local busy is set to true
 *                then the local IrLAP layer will send RNR frames to the other
 *                side indicating it cannot receive any more data. If the
 *                local busy is set to false IrLAP is ready to receive frames.
 *                This function should not be used when using Tiny TP or when
 *                multiple connections exist. It takes affect the next time
 *                IrLAP sends an RR frame. If IrLAP has data to send the data
 *                will be sent first so it should be used carefully.
 *
 * Parameters:    refNum - ir library refNum
 *
 *                flag - value (true or false) to set IrLAP's local busy flag.
 *
 * Return Values: void
 *)

procedure IrLocalBusy(refNum: UInt16; flag: BOOL); syscall irLibTrapLocalBusy;

(*---------------------------------------------------------------------------
 *
 * Prototype:     void IrSetConTypeTTP(IrConnect* con)
 *
 * Description:   Set the type of the connection to Tiny TP. This function
 *                must be called after the IrConnect structure is bound to
 *                the stack.
 *
 * Parameters:    con - pointer to IrConnect structure.
 *
 * Return Values: void
 *)

// IrSetConTypeTTP(con) { ((con)->flags |= LCON_FLAGS_TTP); }

(*---------------------------------------------------------------------------
 *
 * Prototype:     void IrSetConTypeLMP(IrConnect* con)
 *
 * Description:   Set the type of the connection to IrLMP. This function
 *                must be called after the IrConnect structure is bound to
 *                the stack.
 *
 * Parameters:    con - pointer to IrConnect structure.
 *
 * Return Values: void
 *)

// IrSetConTypeLMP(con) { ((con)->flags &= ~LCON_FLAGS_TTP); }

(*---------------------------------------------------------------------------
 *
 * Prototype:     UInt16 IrMaxTxSize(UInt16 refNum,IrConnect* con);
 *
 * Description:   Returns the maximum size allowed for a transmit packet.
 *                The value returned is only valid for active connections.
 *                The maximum size will vary for each connection and is based
 *                on the negotiated IrLAP parameters and the type of the
 *                connection.
 *
 * Parameters:    refNum - ir library refNum
 *
 *                con - pointer to IrConnect structure which represents
 *                an active connection.
 *
 * Return Values: Maxmum number of bytes for a transmit packet.
 *)

function IrMaxTxSize(refNum: UInt16; var con: IrConnect): UInt16; syscall irLibTrapMaxTxSize;

(*---------------------------------------------------------------------------
 *
 * Prototype:    IrMaxRxSize(UInt16 refNum,IrConnect* con);
 *
 * Description:   Returns the maximum size buffer that can be sent by the
 *                the other device. The value returned is only valid for
 *                active connections. The maximum size will vary for
 *                each connection and is based on the negotiated IrLAP
 *                parameters and the type of the connection.
 *
 * Parameters:    refNum - ir library refNum
 *
 *                con - pointer to IrConnect structure which represents
 *                an active connection.
 *
 * Return Values: Maxmum number of bytes that can be sent by the other
 *                device (maximum bytes that can be received).
 *)

function IrMaxRxSize(refNum: UInt16; var con: IrConnect): UInt16; syscall irLibTrapMaxRxSize;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrSetDeviceInfo(UInt16 refNum,UInt8 *info,UInt8 len);
 *
 * Description:   Set the XID info string used during discovery to the given
 *                string and length. The XID info string contains hints and
 *                the nickname of the device. The size cannot exceed
 *                IR_MAX_DEVICE_INFO bytes.
 *
 * Parameters:    refNum - ir library refNum
 *
 *                info - pointer to array of bytes
 *
 *                len - number of bytes pointed to by info
 *
 * Return Values: IR_STATUS_SUCCESS - operation is successful.
 *
 *                IR_STATUS_FAILED - operation failed because info is too
 *                big (Error Checking only).
 *)

function IrSetDeviceInfo(refNum: UInt16; var info: UInt8; len: UInt8): IrStatus; syscall irLibTrapSetDeviceInfo;

(*---------------------------------------------------------------------------
 *
 * Prototype:     BOOL IrIsNoProgress(UInt16 refNum);
 *
 * Description:   Return true if IrLAP is not making progress otherwise
 *                return false (this is an optional function).
 *
 * Parameters:    refNum - ir library refNum
 *
 * Return Values: true if IrLAP is not making progress, false otherwise.
 *)

function IrIsNoProgress(refNum: UInt16): BOOL; syscall irLibTrapIsNoProgress;

(*---------------------------------------------------------------------------
 *
 * Prototype:     Boolean IrIsRemoteBusy(UInt16 refNum)
 *
 * Description:   Return true if the other device's IrLAP is busy otherwise
 *                return false (this is an optional function).
 *
 * Parameters:    refNum - ir library refNum
 *
 * Return Values: true if the other device's IrLAP is busy, false otherwise.
 *)

function IrIsRemoteBusy(refNum: UInt16): BOOL; syscall irLibTrapIsRemoteBusy;

(*---------------------------------------------------------------------------
 *
 * Prototype:     BOOL IrIsMediaBusy(UInt16 refNum);
 *
 * Description:   Return true if the IR media is busy. Otherwise return false
 *                (this is an optional function).
 *
 * Parameters:    refNum - ir library refNum
 *
 * Return Values: true if IR media is busy, false otherwise.
 *)

function IrIsMediaBusy(refNum: UInt16): BOOL; syscall irLibTrapIsMediaBusy;

(*---------------------------------------------------------------------------
 *
 * Prototype:     BOOL IrIsIrLapConnected(UInt16 refNum);
 *
 * Description:   Return true if an IrLAP connection exists (this is an
 *                optional function). Only available if IR_IS_LAP_FUNCS is
 *                defined.
 *
 * Parameters:    refNum - ir library refNum
 *
 * Return Values: true if IrLAP is connected, false otherwise.
 *)

function IrIsIrLapConnected(refNum: UInt16): BOOL; syscall irLibTrapIsIrLapConnected;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IR_TestReq(IrDeviceAddr devAddr,
 *                                    IrConnect* con,
 *                                    IrPacket* packet)
 *
 * Description:   Request a TEST command frame be sent in the NDM state. The
 *                result is signaled via the callback specified in the
 *                IrConnect structure. The callback event is LEVENT_TEST_CNF
 *                and the status field indates the result of the operation.
 *                IR_STATUS_SUCCESS indicates success and IR_STATUS_FAILED
 *                indicates no response was received. A packet must be passed
 *                containing the data to send in the TEST frame. The packet
 *                is returned when the LEVENT_TEST_CNF event is given.
 *
 *
 * Parameters:    refNum - ir library refNum
 *
 *                devAddr - device address of device where TEST will be
 *                sent. This address is not checked so it can be the
 *                broadcast address or 0.
 *
 *                con - pointer to IrConnect structure specifying the
 *                callback function to call to report the result.
 *
 *                packet - pointer to a packet that contains the data to
 *                send in the TEST command packet. The maximum size data
 *                that can be sent is IR_MAX_TEST_PACKET. Even if no
 *                data is to be sent a valid packet must be passed.
 *
 *
 * Return Values: IR_STATUS_PENDING - operation has been started successfully
 *                and the result will be returned via the callback function with
 *                the event LEVENT_TEST_CNF. This is also the indication
 *                returning the packet.
 *
 *                IR_STATUS_FAILED - operation failed because of one of the
 *                reasons below. Note that the packet is
 *                available immediately:
 *                   - IrConnect structure is not bound to the stack
 *                   - Packet size exceeds maximum allowed.
 *
 *                IR_STATUS_MEDIA_BUSY - operation failed because the media is
 *                busy or the stack is not in the NDM state (the packet is
 *                available immediately).
 *)

function IrTestReq(refNum: UInt16; devAddr: IrDeviceAddr; var con: IrConnect; var packet: IrPacket): IrStatus; syscall irLibTrapTestReq;

(****************************************************************************
 *
 * Callback Reference
 *
 ****************************************************************************)

(*---------------------------------------------------------------------------
 *
 * The stack calls the application via a callback function stored in each
 * IrConnect structure. The callback function is called with a pointer to
 * the IrConnect structure and a pointer to a parameter structure. The
 * parameter structure contains an event field which indicates the reason
 * the callback is called and other parameters which have meaning based
 * on the event.
 *
 * The meaning of the events are as follows:
 *
 * LEVENT_LM_CON_IND - Other device has initiated a connection. IR_ConnectRsp
 * should be called to accept the connection. Any data associated with the
 * connection request can be found using fields rxBuff and rxLen for the
 * data pointer and length respectively.
 *
 * LEVENT_LM_DISCON_IND - The IrLMP/Tiny TP connection has been disconnected.
 * Any data associated with the disconnect indication can be found using
 * fields rxBuff and rxLen for the data pointer and length respectively.
 *
 * LEVENT_DATA_IND - Data has been received. The received data is accessed
 * using fields rxBuff and rxLen;
 *
 * LEVENT_PACKET_HANDLED - A packet is being returned. A pointer to the
 * packet exists in field packet.
 *
 * LEVENT_LAP_CON_IND - Indicates that the IrLAP connection has come up. The
 * callback of all bound IrConnect structures is called.
 *
 * LEVENT_LAP_DISCON_IND - Indicates that the IrLAP connection has gone
 * down. This means that all IrLMP connections are also down. A callback
 * with event LEVENT_LM_CON_IND will not be given. The callback function
 * of all bound IrConnect structures is called.
 *
 * LEVENT_DISCOVERY_CNF - Indicates the completion of a discovery operation.
 * The field deviceList points to the discovery list.
 *
 * LEVENT_LAP_CON_CNF - The requested IrLAP connection has been made
 * successfully. The callback function of all bound IrConnect structures
 * is called.
 *
 * LEVENT_LM_CON_CNF - The requested IrLMP/Tiny TP connection has been made
 * successfully. Connection data from the other side is found using fields
 * rxBuff and rxLen.
 *
 * LEVENT_STATUS_IND - Indicates that a status event from the stack has
 * occured. The status field indicates the status generating the event.
 * Possible statuses are as follows. Note this event is optional:
 *    IR_STATUS_NO_PROGRESS - means that IrLAP has no progress for 3 seconds
 *    threshold time (e.g. beam is blocked).
 *
 *    IR_STATUS_LINK_OK - indicates that the no progress condition has
 *    cleared.
 *
 *    IR_STATUS_MEDIA_NOT_BUSY - indicates that the IR media has
 *    transitioned from busy to not busy.
 *
 * LEVENT_TEST_IND - Indicates that a TEST command frame has been received.
 * A pointer to the received data is in rxBuff and rxLen. A pointer to the
 * packet that will be sent in response to the test command is in the packet
 * field. The packet is currently setup to respond with the same data sent
 * in the command TEST frame. If different data is desired as a response
 * then modify the packet structure. This event is sent to the callback
 * function in all bound IrConnect structures. The IAS connections ignore
 * this event.
 *
 * LEVENT_TEST_CNF - Indicates that a TEST command has completed. The status
 * field indicates if the test was successful. IR_STATUS_SUCCESS indicates
 * that operation was successful and the data in the test response can be
 * found by using the rxBuff and rxLen fields. IR_STATUS_FAILED indicates
 * that no TEST response was received. The packet passed to perform the test
 * command is passed back in the packet field and is now available (no
 * separate packet handled event will occur).
 *)
(* The following functions are used to extract U16 and U32 bit numbers
 * from an IAS result. Only IasGetU16 is used internal by the stack
 * but they are part of some of the IAS Query result macros. To enable
 * the function versions define IR_IAS_GET_AS_FUNC
 *)

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrIAS_Add(UInt16 refNum,IrIasObject* obj)
 *
 * Description:   Add an IAS Object to the IAS Database. The Object is
 *                is not copied so the memory for the object must exist
 *                for as long as the object is in the data base. The
 *                IAS database is designed to only allow objects with unique
 *                class names. The error checking version checks for this.
 *                Class names and attributes names must not exceed
 *                IR_MAX_IAS_NAME. Also attribute values must not exceed
 *                IR_MAX_IAS_ATTR_SIZE.
 *
 * Parameters:    refNum - ir library reference number
 *
 *                obj - pointer to an IrIasObject structure.
 *
 * Return Values: IR_STATUS_SUCCESS - operation is successful.
 *
 *                IR_STATUS_FAILED - operation failed for one of the
 *                following reasons:
 *                  - No space in the data base (see irconfig.h to
 *                    increase the size of the IAS database).
 *                  - An entry with the same class name already exists.
 *                    Error check only.
 *                  - The attributes of the object violate the IrDA Lite
 *                    rules (attribute name exceeds IR_MAX_IAS_NAME or
 *                    attribute value exceeds IR_MAX_IAS_ATTR_SIZE).
 *                    Error check only.
 *                  - The class name exceeds IR_MAX_IAS_NAME. Error check
 *                    only
 *)

function IrIAS_Add(refNum: UInt16; var obj: IrIasObject): IrStatus; syscall irLibTrapIAS_Add;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrIAS_Query(UInt16 refNum,IrIasQuery* token)
 *
 * Description:   Make an IAS query of another devices IAS database. An IrLAP
 *                connection must exist to the other device. The IAS query
 *                token must be initialized as described below. The result is
 *                signaled by calling the callback function whose pointer
 *                exists in the IrIasQuery structure. Only one Query can be
 *                made at a time.
 *
 * Parameters:    refNum - ir library reference number
 *
 *                token - pointer to an IrIasQuery structure initialized
 *                as follows:
 *                   - pointer to a callback function in which the result will
 *                     signaled.
 *                   - result points to a buffer large enough to hold the
 *                     result of the query.
 *                   - resultBufSize is set to the size of the result buffer.
 *                   - queryBuf must point to a valid query.
 *                   - queryLen is set to the number of bytes in queryBuf.
 *                     The length must not exceed IR_MAX_QUERY_LEN.
 *
 * Return Values: IR_STATUS_PENDING - operation is started successfully and
 *                the result will be signaled via the calback function.
 *
 *                IR_STATUS_FAILED - operation failed for one of the
 *                following reasons (Error check only):
 *                   - The query exceeds IR_MAX_QUERY_LEN.
 *                   - The result field of token is 0.
 *                   - The resultBuffSize field of token is 0.
 *                   - The callback field of token is 0.
 *                   - A query is already in progress.
 *
 *                IR_STATUS_NO_IRLAP - operation failed because there is no
 *                IrLAP connection.
 *)

function IrIAS_Query(refNum: UInt16; var token: IrIasQuery): IrStatus; syscall irLibTrapIAS_Query;

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrStatus IrIAS_SetDeviceName(UInt16 refNum,UInt8 *name, UInt8 len)
 *
 * Description:   Set the value field of the device name attribute of the
 *                "Device" object in the IAS Database. This function is only
 *                available if IR_IAS_DEVICE_NAME is defined.
 *
 * Parameters:    name - pointer to an IAS value field for the device name
 *                attribute of the device object. It includes the attribute
 *                type, character set and device name. This value field should
 *                be a constant and the pointer must remain valid until
 *                IRIAS_SetDeviceName() is called with another pointer.
 *
 *                len - total length of the value field. Maximum size allowed
 *                is IR_MAX_IAS_ATTR_SIZE.
 *
 * Return Values: IR_STATUS_SUCCESS - operation is successful.
 *
 *                IR_STATUS_FAILED - len is too big or the value field is not
 *                a valid user string (Error Checking only).
 *)

function IrIAS_SetDeviceName(refNum: UInt16; var name: UInt8; len: UInt8): IrStatus; syscall irLibTrapIAS_SetDeviceName;

(*---------------------------------------------------------------------------
 *
 * Below are some functions and macros for parsing the results buffer
 * after a successfull IAS Query.
 *)

(*---------------------------------------------------------------------------
 *
 * Prototype:     void IrIAS_StartResult(IrIasQuery* token)
 *
 * Description:   Put the internal pointer to the start of the
 *                result buffer.
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: void
 *)

// IrIAS_StartResult(t) ((t)->offset = 0)

(*---------------------------------------------------------------------------
 *
 * Prototype:     U16 IRIAS_GetObjectID(IrIasQuery* token)
 *
 * Description:   Return the unique object ID of the current result item.
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: object ID
 *)

// IrIAS_GetObjectID(t) IasGetU16((t)->result + (t)->offset)

(*---------------------------------------------------------------------------
 *
 * Prototype:     U8 IrIAS_GetType(IrIasQuery* token)
 *
 * Description:   Return the type of the current result item
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: Type of result item such as IAS_ATTRIB_INTEGER,
 *                IAS_ATTRIB_OCTET_STRING or IAS_ATTRIB_USER_STRING.
 *)

// IrIAS_GetType(t) ((t)->result[(t)->offset + 2])

(*---------------------------------------------------------------------------
 *
 * Prototype:     U32 IrIAS_GetInteger(IrIasQuery* token)
 *
 * Description:   Return an integer value assuming that the current result
 *                item is of type IAS_ATTRIB_INTEGER (call IRIAS_GetType() to
 *                determine the type of the current result item).
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: Integer value.
 *)

// IrIAS_GetInteger(t) IasGetU32((t)->result + (t)->offset + 3)

(*---------------------------------------------------------------------------
 *
 * Prototype:     U8 IrIAS_GetIntLsap(IrIasQuery* token)
 *
 * Description:   Return an integer value that represents an LSAP assuming
 *                that the current result item is of type IAS_ATTRIB_INTEGER
 *                (call IRIAS_GetType() to determine the type of the current
 *                result item). Usually integer values returned in a query
 *                are LSAP selectors.
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: Integer value.
 *)

// IrIAS_GetIntLsap(t) ((t)->result[(t)->offset + 6])

(*---------------------------------------------------------------------------
 *
 * Prototype:     U16 IrIAS_GetOctetStringLen(IrIasQuery* token)
 *
 * Description:   Get the length of an octet string assuming that the current
 *                result item is of type IAS_ATTRIB_OCTET_STRING (call
 *                IRIAS_GetType() to determine the type of the current result
 *                item).
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: Length of octet string
 *)

// IrIAS_GetOctetStringLen(t) IasGetU16((t)->result + (t)->offset + 3)

(*---------------------------------------------------------------------------
 *
 * Prototype:     U8* IrIAS_GetOctetString(IrIasQuery* token)
 *
 * Description:   Return a pointer to an octet string assuming that the
 *                current result item is of type IAS_ATTRIB_OCTET_STRING (call
 *                IRIAS_GetType() to determine the type of the current result
 *                item).
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: pointer to octet string
 *)

// IrIAS_GetOctetString(t) ((t)->result + (t)->offset + 5)

(*---------------------------------------------------------------------------
 *
 * Prototype:     U8 IrIAS_GetUserStringLen(IrIasQuery* token)
 *
 * Description:   Return the length of a user string assuming that the
 *                current result item is of type IAS_ATTRIB_USER_STRING (call
 *                IRIAS_GetType() to determine the type of the current result
 *                item).
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: Length of user string
 *)

// IrIAS_GetUserStringLen(t) ((t)->result[(t)->offset + 4])

(*---------------------------------------------------------------------------
 *
 * Prototype:     IrCharSet IrIAS_GetUserStringCharSet(IrIasQuery* token)
 *
 * Description:   Return the character set of the user string assuming that
 *                the current result item is of type IAS_ATTRIB_USER_STRING
 *                (call IRIAS_GetType() to determine the type of the current
 *                result item).
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: Character set
 *)

// IrIAS_GetUserStringCharSet(t) ((t)->result[(t)->offset + 3])

(*---------------------------------------------------------------------------
 *
 * Prototype:     U8* IrIAS_GetUserString(IrIasQuery* token)
 *
 * Description:   Return a pointer to a user string assuming that the
 *                current result item is of type IAS_ATTRIB_USER_STRING (call
 *                IRIAS_GetType() to determine the type of the current result
 *                item).
 *
 * Parameters:    token - pointer to an IrIasQuery structure
 *
 * Return Values: Pointer to result string
 *)

// IrIAS_GetUserString(t) ((t)->result + (t)->offset + 5)

 (*---------------------------------------------------------------------------
 *
 * Prototype:     UInt8 *IrIAS_Next(UInt16 refNum,IrIasQuery* token)
 *
 * Description:   Move the internal pointer to the next result item. This
 *                function returns a pointer to the start of the next result
 *                item. If the poiinter is 0 then there are no more result
 *                items. Only available if IR_IAS_NEXT is defined.
 *
 * Parameters:    refNum - library reference number
 *
 *               token - pointer to an IrIasQuery structure
 *
 * Return Values: Pointer to the next result item or 0 if no more items.
 *)

function IrIAS_Next(refNum: UInt16; var token: IrIasQuery): UInt8Ptr; syscall irLibTrapIAS_Next;

(****************************************************************************
 *
 * IAS Callback Reference
 *
 ****************************************************************************)

(*---------------------------------------------------------------------------
 *
 * The result of IAS query is signaled by calling the callback function
 * pointed to by the callBack field of IrIasQuery structure. The callback
 * has the following prototype:
 *
 *  void callBack(IrStatus);
 *
 * The callback is called with a status as follows:
 *
 *    IR_STATUS_SUCCESS - the query operation finished successfully and
 *    the results can be parsed
 *
 *    IR_STATUS_DISCONNECT - the link or IrLMP connection was disconnected
 *    during the query so the results are not valid.

=========================================================================== *)

// The following two functions are only for advances uses - do not use these.

 (*---------------------------------------------------------------------------
 *
 * Prototype:     IrHandleEvent(UInt16 refnum)
 *
 * Description:   MemHandle background task event (ony used for special cases)
 *                  Normally you will not use this function
 *
 * Parameters:    refNum - library reference number
 **
 * Return Values: Pointer to the next result item or 0 if no more items.
 *)

function IrHandleEvent(refnum: UInt16): Boolean; syscall irLibTrapHandleEvent;

 (*---------------------------------------------------------------------------
 *
 * Prototype:     IrWaitForEvent(UInt16 libRefnum,Int32 timeout)
 *
 * Description:   Wait for background task event (ony used for special cases)
 *                  Normally you will not use this function
 *
 * Parameters:    refNum - library reference number
 *
 *                timeout - number of ticks to wait
 *
 * Return Values: Pointer to the next result item or 0 if no more items.
 *)

function IrWaitForEvent(libRefnum: UInt16; timeout: Int32): Err; syscall irLibTrapWaitForEvent;

implementation

end.
