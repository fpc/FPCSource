{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SerialLinkMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Source for Serial Link Routines on Pilot
 *
 * History:
 *    2/6/95 replaces DSerial.h from Debugger
 *
 *****************************************************************************)

unit seriallinkmgr;

interface

uses palmos, coretraps, errorbase;

//*************************************************************************
//   Pre-defined, fixxed  Socket ID's
//*************************************************************************

const
  slkSocketDebugger     = 0; // Debugger Socket
  slkSocketConsole      = 1; // Console Socket
  slkSocketRemoteUI     = 2; // Remote UI Socket
  slkSocketDLP          = 3; // Desktop Link Socket
  slkSocketFirstDynamic = 4; // first dynamic socket ID

//*************************************************************************
//  Packet Types
//*************************************************************************

const
  slkPktTypeSystem       = 0; // System packets
  slkPktTypeUnused1      = 1; // used to be: Connection Manager packets
  slkPktTypePAD          = 2; // PAD Protocol packets
  slkPktTypeLoopBackTest = 3; // Loop-back test packets

//*************************************************************************
//
// Packet structure:
//      header
//      body (0-dbgMaxPacketBodyLength bytes of data)
//      footer
//
//*************************************************************************

//----------------------------------------------------------------------
// packet header
// Fields marked with -> must be filled in by caller
// Fields marked with X  will be filled in by SlkSendPacket.
//----------------------------------------------------------------------

type
  SlkPktHeaderChecksum = UInt8;

  SlkPktHeaderType = record
    signature1: UInt16;             // X  first 2 bytes of signature
    signature2: UInt8;              // X  3 and final byte of signature
    dest: UInt8;                    // -> destination socket Id
    src: UInt8;                     // -> src socket Id
    type_: UInt8;                   // -> packet type
    bodySize: UInt16;               // X  size of body
    transId: UInt8;                 // -> transaction Id
                                    //    if 0 specified, it will be replaced
    checksum: SlkPktHeaderChecksum; // X  check sum of header
  end;

  SlkPktHeaderPtr = ^SlkPktHeaderType;

const
  slkPktHeaderSignature1 = $BEEF;
  slkPktHeaderSignature2 = $ED;

  slkPktHeaderSigFirst   = $BE; // First byte
  slkPktHeaderSigSecond  = $EF; // second byte
  slkPktHeaderSigThird   = $ED; // third byte

//----------------------------------------------------------------------
// packet footer
//----------------------------------------------------------------------

type
  SlkPktFooterType = record
    crc16: UInt16; // header and body crc
  end;

  SlkPktFooterPtr = ^SlkPktFooterType;

//*************************************************************************
//
// Write Data Structure passed to SlkSendPacket. This structure
//  Tells SlkSendPacket where each of the chunks that comprise the body are
//  and the size of each. SlkSendPacket accepts a pointer to an array
//  of SlkWriteDataTypes, the last one has a size field of 0.
//
//*************************************************************************

  SlkWriteDataType = record
    size: UInt16;   // last one has size of 0
    dataP: Pointer; // pointer to data
  end;

  SlkWriteDataPtr = ^SlkWriteDataType;

(*******************************************************************
 * Serial Link Manager Errors
 * the constant slkErrorClass is defined in SystemMgr.h
 *******************************************************************)

const
  slkErrChecksum        = slkErrorClass or 1;
  slkErrFormat          = slkErrorClass or 2;
  slkErrBuffer          = slkErrorClass or 3;
  slkErrTimeOut         = slkErrorClass or 4;
  slkErrHandle          = slkErrorClass or 5;
  slkErrBodyLimit       = slkErrorClass or 6;
  slkErrTransId         = slkErrorClass or 7;
  slkErrResponse        = slkErrorClass or 8;
  slkErrNoDefaultProc   = slkErrorClass or 9;
  slkErrWrongPacketType = slkErrorClass or 10;
  slkErrBadParam        = slkErrorClass or 11;
  slkErrAlreadyOpen     = slkErrorClass or 12;
  slkErrOutOfSockets    = slkErrorClass or 13;
  slkErrSocketNotOpen   = slkErrorClass or 14;
  slkErrWrongDestSocket = slkErrorClass or 15;
  slkErrWrongPktType    = slkErrorClass or 16;
  slkErrBusy            = slkErrorClass or 17; // called while sending a packet
                                               // only returned on single-threaded
                                               // emulation implementations
  slkErrNotOpen         = slkErrorClass or 18;

(*******************************************************************
 * Type definition for a Serial Link Socket Listener
 *
 *******************************************************************)

type
  SlkSocketListenerProcPtr = procedure(headerP: SlkPktHeaderPtr; bodyP: Pointer);

  SlkSocketListenType = record
    listenerP: SlkSocketListenerProcPtr;
    headerBufferP: SlkPktHeaderPtr;      // App allocated buffer for header
    bodyBufferP: Pointer;                // App allocated buffer for body
    bodyBufferSize: UInt32;
  end;

  SlkSocketListenPtr = ^SlkSocketListenType;

(*******************************************************************
 * Prototypes
 *******************************************************************)

//-------------------------------------------------------------------
// Initializes the Serial Link Manager
//-------------------------------------------------------------------

function SlkOpen: Err; syscall sysTrapSlkOpen;

//-------------------------------------------------------------------
// Close down the Serial Link Manager
//-------------------------------------------------------------------

function SlkClose: Err; syscall sysTrapSlkClose;

//-------------------------------------------------------------------
// Open up another Serial Link socket. The caller must have already
//  opened the comm library and set it to the right settings.
//-------------------------------------------------------------------

function SlkOpenSocket(portID: UInt16; var socketP: UInt16; staticSocket: Boolean): Err; syscall sysTrapSlkOpenSocket;

//-------------------------------------------------------------------
// Close up a Serial Link socket.
//  Warning: This routine is assymetrical with SlkOpenSocket because it
//   WILL CLOSE the library for the caller (unless the refNum is the
//   refNum of the debugger comm library).
//-------------------------------------------------------------------

function SlkCloseSocket(socket: UInt16): Err; syscall sysTrapSlkCloseSocket;

//-------------------------------------------------------------------
// Get the library refNum for a particular Socket
//-------------------------------------------------------------------

function SlkSocketPortID(socket: UInt16; var portIDP: UInt16): Err; syscall sysTrapSlkSocketRefNum;

//-------------------------------------------------------------------
// Set the in-packet timeout for a socket
//-------------------------------------------------------------------

function SlkSocketSetTimeout(socket: UInt16; timeout: Int32): Err; syscall sysTrapSlkSocketSetTimeout;

//-------------------------------------------------------------------
// Flush a Socket
//-------------------------------------------------------------------

function SlkFlushSocket(socket: UInt16; timeout: Int32): Err; syscall sysTrapSlkFlushSocket;

//-------------------------------------------------------------------
// Set up a Socket Listener
//-------------------------------------------------------------------

function SlkSetSocketListener(socket: UInt16; socketP: SlkSocketListenPtr): Err; syscall sysTrapSlkSetSocketListener;

//-------------------------------------------------------------------
// Sends a packet's header, body, footer.  Stuffs the header's
// magic number and checksum fields.  Expects all other
// header fields to be filled in by caller.
// errors returned: dseHandle, dseLine, dseIO, dseParam, dseBodyLimit,
//                  dseOther
//-------------------------------------------------------------------

function SlkSendPacket(headerP: SlkPktHeaderPtr; writeList: SlkWriteDataPtr): Err; syscall sysTrapSlkSendPacket;

//-------------------------------------------------------------------
// Receives and validates an entire packet.
// errors returned: dseHandle, dseParam, dseLine, dseIO, dseFormat,
//                  dseChecksum, dseBuffer, dseBodyLimit, dseTimeOut,
//                  dseOther
//-------------------------------------------------------------------

function SlkReceivePacket(socket: UInt16; andOtherSockets: Boolean;
                          headerP: SlkPktHeaderPtr; bodyP: Pointer; bodySize: UInt16;
                          timeout: Int32): Err; syscall sysTrapSlkReceivePacket;

//-------------------------------------------------------------------
// Do Default processing of a System packet
//-------------------------------------------------------------------

function SlkSysPktDefaultResponse(headerP: SlkPktHeaderPtr; bodyP: Pointer): Err; syscall sysTrapSlkSysPktDefaultResponse;

//-------------------------------------------------------------------
// Do RPC call
//-------------------------------------------------------------------

function SlkProcessRPC(headerP: SlkPktHeaderPtr; bodyP: Pointer): Err; syscall sysTrapSlkProcessRPC;

implementation

end.
