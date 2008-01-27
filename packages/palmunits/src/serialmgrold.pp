{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SerialMgrOld.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Serial manager
 *
 * History:
 *    2/7/95 Created by Ron Marianetti
 *    7/6/95   vmk   added serDefaultSettings
 *    1/28/98  scl   added Serial Port Definitions
 *
 *****************************************************************************)

unit serialmgrold;

interface

uses palmos, coretraps, libtraps, errorbase, systemmgr;

(********************************************************************
 * Serial Manager Errors
 * the constant serErrorClass is defined in SystemMgr.h
 ********************************************************************)

const
  serErrBadParam     = serErrorClass or 1;
  serErrBadPort      = serErrorClass or 2;
  serErrNoMem        = serErrorClass or 3;
  serErrBadConnID    = serErrorClass or 4;
  serErrTimeOut      = serErrorClass or 5;
  serErrLineErr      = serErrorClass or 6;
  serErrAlreadyOpen  = serErrorClass or 7;
  serErrStillOpen    = serErrorClass or 8;
  serErrNotOpen      = serErrorClass or 9;
  serErrNotSupported = serErrorClass or 10; // functionality not supported

(********************************************************************
 * Serial Port Definitions
 ********************************************************************)

const
  serPortDefault      = $0000; // Use prefDefSerialPlugIn
  serPortLocalHotSync = $8000; // Use physical HotSync port
  serPortMaskLocal    = $7FFF; // Mask off HotSync "hint" (for SerialMgr)

(********************************************************************
 * Serial Settings Descriptor
 ********************************************************************)

type
  SerSettingsType = record
   baudRate: UInt32;  // baud rate
   flags: UInt32;     // miscellaneous settings
   ctsTimeout: Int32; // max # of ticks to wait for CTS to become asserted
                      // before transmitting; used only when
                      // configured with serSettingsFlagCTSAutoM.
  end;
  SerSettingsPtr = ^SerSettingsType;

const
  serSettingsFlagStopBitsM    = $00000001; // mask for stop bits field
  serSettingsFlagStopBits1    = $00000000; //  1 stop bits
  serSettingsFlagStopBits2    = $00000001; //  2 stop bits
  serSettingsFlagParityOnM    = $00000002; // mask for parity on
  serSettingsFlagParityEvenM  = $00000004; // mask for parity even
  serSettingsFlagXonXoffM     = $00000008; // (NOT IMPLEMENTED) mask for Xon/Xoff flow control
  serSettingsFlagRTSAutoM     = $00000010; // mask for RTS rcv flow control
  serSettingsFlagCTSAutoM     = $00000020; // mask for CTS xmit flow control
  serSettingsFlagBitsPerCharM = $000000C0; // mask for bits/char
  serSettingsFlagBitsPerChar5 = $00000000; //  5 bits/char
  serSettingsFlagBitsPerChar6 = $00000040; //  6 bits/char
  serSettingsFlagBitsPerChar7 = $00000080; //  7 bits/char
  serSettingsFlagBitsPerChar8 = $000000C0; //  8 bits/char

// Default settings
  serDefaultSettings = serSettingsFlagBitsPerChar8 or
                       serSettingsFlagStopBits1 or
                       serSettingsFlagRTSAutoM;

//!!!  serDefaultCTSTimeout = 5 * sysTicksPerSecond;

//
// mask values for the lineErrors  from SerGetStatus
//

const
  serLineErrorParity      = $0001; // parity error
  serLineErrorHWOverrun   = $0002; // HW overrun
  serLineErrorFraming     = $0004; // framing error
  serLineErrorBreak       = $0008; // break signal asserted
  serLineErrorHShake      = $0010; // line hand-shake error
  serLineErrorSWOverrun   = $0020; // HW overrun
  serLineErrorCarrierLost = $0040; // CD dropped

(********************************************************************
 * Type of a wakeup handler procedure which can be installed through the
 *   SerSetWakeupHandler() call.
 ********************************************************************)

type
  SerWakeupHandler = procedure (refCon: UInt32);

(********************************************************************
 * Type of an emulator-mode only blocking hook routine installed via
 * SerControl function serCtlEmuSetBlockingHook.  This is supported only
 * under emulation mode.  The argument to the function is the value
 * specified in the SerCallbackEntryType structure.  The intention of the
 * return value is to return false if serial manager should abort the
 * current blocking action, such as when an app quit event has been received;
 * otherwise, it should return true.  However, in the current implementation,
 * this return value is ignored.  The callback can additionally process
 * events to enable user interaction with the UI, such as interacting with the
 * debugger.
 ********************************************************************)

type
  SerBlockingHookHandler = function (userRef: UInt32): Boolean;

(********************************************************************
 * Serial Library Control Enumerations (Pilot 2.0)
 ********************************************************************)

(********************************************************************
 * Structure for specifying callback routines.
 ********************************************************************)

type
  SerCallbackEntryType = record
    funcP: MemPtr;   // function pointer
    userRef: UInt32; // ref value to pass to callback
  end;
  SerCallbackEntryPtr = ^SerCallbackEntryType;

// v2.0 extension
type
  SerCtlEnum = Enum;

const
  serCtlFirstReserved = 0;                                   // RESERVE 0

  serCtlStartBreak = Succ(serCtlFirstReserved);              // turn RS232 break signal on:
                                                             // users are responsible for ensuring that the break is set
                                                             // long enough to genearate a valie BREAK!
                                                             // valueP = 0, valueLenP = 0

  serCtlStopBreak = Succ(serCtlStartBreak);                  // turn RS232 break signal off:
                                                             // valueP = 0, valueLenP = 0

  serCtlBreakStatus = Succ(serCtlStopBreak);                 // Get RS232 break signal status(on or off):
                                                             // valueP = pointer to UInt16 for returning status(0 = off, !0 = on)
                                                             // *valueLenP = sizeof(UInt16)

  serCtlStartLocalLoopback = Succ(serCtlBreakStatus);        // Start local loopback test
                                                             // valueP = 0, valueLenP = 0

  serCtlStopLocalLoopback = Succ(serCtlStartLocalLoopback);  // Stop local loopback test
                                                             // valueP = 0, valueLenP = 0

  serCtlMaxBaud = Succ(serCtlStopLocalLoopback);             // Get maximum supported baud rate:
                                                             // valueP = pointer to UInt32 for returned baud
                                                             // *valueLenP = sizeof(UInt32)

  serCtlHandshakeThreshold = Succ(serCtlMaxBaud);            // retrieve HW handshake threshold; this is the maximum baud rate
                                                             // which does not require hardware handshaking
                                                             // valueP = pointer to UInt32 for returned baud
                                                             // *valueLenP = sizeof(UInt32)

  serCtlEmuSetBlockingHook = Succ(serCtlHandshakeThreshold); // Set a blocking hook routine FOR EMULATION
                                                             // MODE ONLY - NOT SUPPORTED ON THE PILOT
                                                             //PASS:
                                                             // valueP = pointer to SerCallbackEntryType
                                                             // *valueLenP = sizeof(SerCallbackEntryType)
                                                             //RETURNS:
                                                             // the old settings in the first argument


  serCtlIrDAEnable = Succ(serCtlEmuSetBlockingHook);         // Enable  IrDA connection on this serial port
                                                             // valueP = 0, valueLenP = 0

  serCtlIrDADisable = Succ(serCtlIrDAEnable);                // Disable  IrDA connection on this serial port
                                                             // valueP = 0, valueLenP = 0

  serCtlIrScanningOn = Succ(serCtlIrDADisable);              // Start Ir Scanning mode

  serCtlIrScanningOff = Succ(serCtlIrScanningOn);            // Stop Ir Scanning mode

  serCtlRxEnable = Succ(serCtlIrScanningOff);                // enable receiver  ( for IrDA )

  serCtlRxDisable = Succ(serCtlRxEnable);                    // disable receiver ( for IrDA )

  serCtlLAST = Succ(serCtlRxDisable);                        // ADD NEW ENTRIES BEFORE THIS ONE


// Start of a custom op code range for licensees that wrote old serial
// manager replacements.  Note that the serial compatiblity library
// does not pass these op codes to new serial manager plugins.
const
  serCtlFirstCustomEntry = $A800;

(********************************************************************
 * Serial Library Routines
 * These are define as syscall calls only under emulation mode or
 *  under native mode from the module that actually installs the trap
 *  vectors
 ********************************************************************)

// Used by mac applications to map the pilot serial port to a particular
// macintosh port.
//!!!function SerSetMapPort(pilotPort, macPort: UInt16): UInt16;

// Acquires and opens a serial port with given baud and default settings.
function SerOpen(refNum: UInt16; port: UInt16; baud: UInt32): Err; syscall sysLibTrapOpen;

// Used by debugger to re-initialize serial port if necessary
//!!!function SerDbgAssureOpen(refNum: UInt16; port: UInt16; baud: UInt32): Err;

// Closes the serial connection previously opened with SerOpen.
function SerClose(refNum: UInt16): Err; syscall sysLibTrapClose;

// Puts serial library to sleep
function SerSleep(refNum: UInt16): Err; syscall sysLibTrapSleep;

// Wake Serial library
function SerWake(refNum: UInt16): Err; syscall sysLibTrapWake;

// Get attributes of the serial connection
function SerGetSettings(refNum: UInt16; settingsP: SerSettingsPtr): Err; syscall sysLibTrapCustom;

// Set attributes of the serial connection
function SerSetSettings(refNum: UInt16; settingsP: SerSettingsPtr): Err; syscall sysLibTrapCustom + 1;

// Return status of serial connection
function SerGetStatus(refNum: UInt16; var ctsOnP, dsrOnP: Boolean): UInt16; syscall sysLibTrapCustom + 2;

// Reset error condition of serial connection
function SerClearErr(refNum: UInt16): Err; syscall sysLibTrapCustom + 3;

// Sends a buffer of data (may queue it up and return).
function SerSend10(refNum: UInt16; const bufP: Pointer; size: UInt32): Err; syscall sysLibTrapCustom + 4;

// Waits until the serial transmit buffer empties.
// The timeout arg is ignored; CTS timeout is used
function SerSendWait(refNum: UInt16; timeout: Int32): Err; syscall sysLibTrapCustom + 5;

// Returns how many characters are left in the send queue waiting
//  for transmission
function SerSendCheck(refNum: UInt16; var numBytesP: UInt32): Err; syscall sysLibTrapCustom + 6;

// Flushes the data out of the transmit buffer
function SerSendFlush(refNum: UInt16): Err; syscall sysLibTrapCustom + 7;

// Receives a buffer of data of the given size.
function SerReceive10(refNum: UInt16; bufP: Pointer; bytes, timeout: UInt32): Err; syscall sysLibTrapCustom + 8;

// Waits for at least 'bytes' bytes of data to arrive at the serial input.
//  but does not read them in
function SerReceiveWait(refNum: UInt16; bytes, timeout: UInt32): Err; syscall sysLibTrapCustom + 9;

// Returns how many characters are in the receive queue
function SerReceiveCheck(refNum: UInt16; var numBytesP: UInt32): Err; syscall sysLibTrapCustom + 10;

// Flushes any data coming into the serial port, discarding the data.
procedure SerReceiveFlush(refNum: UInt16; timeout: Int32); syscall sysLibTrapCustom + 11;

// Specify a new input buffer.  To restore the original buffer, pass
// bufSize = 0.
function SerSetReceiveBuffer(refNum: UInt16; bufP: Pointer; bufSize: UInt16): Err; syscall sysLibTrapCustom + 12;

// The receive character interrupt service routine, called by kernel when
//  a UART interrupt is detected.
function SerReceiveISP: Boolean; syscall sysTrapSerReceiveISP;

// "Back Door" into the serial receive queue. Used by applications (like TCP Media layers)
//  that need faster access to received characters
function SerReceiveWindowOpen(refNum: UInt16; var bufPP: UInt8Ptr; var sizeP: UInt32): Err; syscall sysLibTrapCustom + 13;

function SerReceiveWindowClose(refNum: UInt16; bytesPulled: UInt32): Err; syscall sysLibTrapCustom + 14;

// Can be called by applications that need an alternate wakeup mechanism
//  when characters get enqueued by the interrupt routine.
function SerSetWakeupHandler(refNum: UInt16; procP: SerWakeupHandler; refCon: UInt32): Err; syscall sysLibTrapCustom + 15;

// Called to prime wakeup handler
function SerPrimeWakeupHandler(refNum: UInt16; minBytes: UInt16): Err; syscall sysLibTrapCustom + 16;

// Called to perform a serial manager control operation
// (v2.0 extension)
function SerControl(refNum: UInt16; op: UInt16; valueP: Pointer; var valueLenP: UInt16): Err; syscall sysLibTrapCustom + 17;

// Sends a buffer of data (may queue it up and return).
function SerSend(refNum: UInt16; const bufP: Pointer; count: UInt32; var errP: Err): UInt32; syscall sysLibTrapCustom + 18;

// Receives a buffer of data of the given size.
function SerReceive(refNum: UInt16; bufP: Pointer; count: UInt32; timeout: Int32; var errP: Err): UInt32; syscall sysLibTrapCustom + 19;

implementation

end.
