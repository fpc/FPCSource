{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SerialMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Serial manager
 *
 * History:
 *    1/14/98     SerialMgr.h created by Ben Manuto
 *
 *****************************************************************************)
unit serialmgr;
interface
uses palmos, coretraps, errorbase, systemresources, systemmgr, netmgr;
// New Serial manager feature numbers
const
  sysFtrNewSerialPresent = 1;
  sysFtrNewSerialVersion = 2;
  serMgrVersion = 2;
(********************************************************************
 * Serial Manager Errors
 * the constant serErrorClass is defined in SystemMgr.h
 ********************************************************************)
  serErrBadParam       = serErrorClass or 1;
  serErrBadPort        = serErrorClass or 2;
  serErrNoMem          = serErrorClass or 3;
  serErrBadConnID      = serErrorClass or 4;
  serErrTimeOut        = serErrorClass or 5;
  serErrLineErr        = serErrorClass or 6;
  serErrAlreadyOpen    = serErrorClass or 7;
  serErrStillOpen      = serErrorClass or 8;
  serErrNotOpen        = serErrorClass or 9;
  serErrNotSupported   = serErrorClass or 10; // functionality not supported
  serErrNoDevicesAvail = serErrorClass or 11; // No serial devices were loaded or are available.
 // New error codes for USB support
  serErrConfigurationFailed = serErrorClass or 12;
//
// mask values for the lineErrors  from SerGetStatus
//
  serLineErrorParity      = $0001; // parity error
  serLineErrorHWOverrun   = $0002; // HW overrun
  serLineErrorFraming     = $0004; // framing error
  serLineErrorBreak       = $0008; // break signal asserted
  serLineErrorHShake      = $0010; // line hand-shake error
  serLineErrorSWOverrun   = $0020; // HW overrun
  serLineErrorCarrierLost = $0040; // CD dropped
(********************************************************************
 * Serial Port Definitions
 ********************************************************************)
const
  serPortLocalHotSync    = $8000; // Use physical HotSync port
  serPortCradlePort      = $8000; // Cradle port. (Auto detect cradle type)
  serPortIrPort          = $8001; // Use available IR port.
  serPortConsolePort     = $8002; // Console port
  serPortCradleRS232Port = $8003; // Cradle RS232 Port
  serPortCradleUSBPort   = $8004; // Cradle USB Port

// This constant is used by the Serial Link Mgr only
  serPortIDMask          = $C000;
(********************************************************************
 * Serial Settings Descriptor
 ********************************************************************)
  srmSettingsFlagStopBitsM     = $00000001; // mask for stop bits field
  srmSettingsFlagStopBits1     = $00000000; //  1 stop bits
  srmSettingsFlagStopBits2     = $00000001; //  2 stop bits
  srmSettingsFlagParityOnM     = $00000002; // mask for parity on
  srmSettingsFlagParityEvenM   = $00000004; // mask for parity even
  srmSettingsFlagXonXoffM      = $00000008; // (NOT IMPLEMENTED) mask for Xon/Xoff flow control
  srmSettingsFlagRTSAutoM      = $00000010; // mask to prevent UART input overflow using RTS (NOTE: this flag
                                            // alone does not prevent software overruns from the serial input buffer)
  srmSettingsFlagCTSAutoM      = $00000020; // mask for CTS xmit flow control (see srmSettingsFlagFlowControlIn below)
  srmSettingsFlagBitsPerCharM  = $000000C0; // mask for bits/char
  srmSettingsFlagBitsPerChar5  = $00000000; //  5 bits/char
  srmSettingsFlagBitsPerChar6  = $00000040; //  6 bits/char
  srmSettingsFlagBitsPerChar7  = $00000080; //  7 bits/char
  srmSettingsFlagBitsPerChar8  = $000000C0; //  8 bits/char
  srmSettingsFlagFlowControlIn = $00000100; // mask to prevent the serial input buffer overflow, using RTS. Use in
                                            // conjunction with srmSettingsFlagRTSAutoM for a fully flow controlled input.
  srmSettingsFlagRTSInactive   = $00000200; // if set and srmSettingsFlagRTSAutoM==0, RTS is held in the inactive (flow off) state forever.
// Default settings
  srmDefaultSettings = srmSettingsFlagBitsPerChar8 or srmSettingsFlagStopBits1 or srmSettingsFlagRTSAutoM or srmSettingsFlagRTSInactive;
  srmDefaultCTSTimeout = 5 * sysTicksPerSecond_;
// Status bitfield constants
  srmStatusCtsOn      = $00000001;
  srmStatusRtsOn      = $00000002;
  srmStatusDsrOn      = $00000004;
  srmStatusBreakSigOn = $00000008;
//
// Info fields describing serial HW capabilities.
//
  serDevCradlePort    = $00000001; // Serial HW controls RS-232 serial from cradle connector of handheld.
  serDevRS232Serial   = $00000002; // Serial HW has RS-232 line drivers
  serDevIRDACapable   = $00000004; // Serial Device has IR line drivers and generates IRDA mode serial.
  serDevModemPort     = $00000008; // Serial deivce drives modem connection.
  serDevCncMgrVisible = $00000010; // Serial device port name string to be displayed in Connection Mgr panel.
  serDevConsolePort   = $00000020; // Serial device is the default console port.
  serDevUSBCapable    = $00000040; // USB driver for USB hardware connected to the cradle connector of the handheld.
type
  DeviceInfoType = record
    serDevCreator: UInt32;             // Four Character creator type for serial driver ('sdrv')
    serDevFtrInfo: UInt32;             // Flags defining features of this serial hardware.
    serDevMaxBaudRate: UInt32;         // Maximum baud rate for this device.
    serDevHandshakeBaud: UInt32;       // HW Handshaking is reccomended for baud rates over this
    serDevPortInfoStr: PChar;          // Description of serial HW device or virtual device.
    reserved: array [0..8-1] of UInt8; // Reserved.
  end;
 DeviceInfoPtr = ^DeviceInfoType;
// Function IDs
//
// Standard set of function ids for the SrmOpen.  Out of convenience, function ids
// use the same namespace as creator ids.  Custom functions can be defined by
// using your app's creator id.  The driver must have knowledge of that creator
// id for it to be of any use.  A driver should handle an unknown function id
// gracefully, either use default functionality or return a serErrBadParam error.
const
  serFncUndefined   = 0;                 // Undefined function
  serFncPPPSession  = netIFCreatorPPP;   // NetLib PPP Interface
  serFncSLIPSession = netIFCreatorSLIP;  // NetLib SLIP Interface
  serFncDebugger    = sysFileCSystem;    // PalmOS Debugger
  serFncHotSync     = sysFileCSync;      // HotSync function
  serFncConsole     = sysFileCSystem;    // PalmOS Console
  serFncTelephony   = sysFileCTelMgrLib; // Telephony Library
//
// Open Configuration Structure
//
type
  SrmOpenConfigType = record
    baud: UInt32;         // Baud rate that the connection is to be opened at.
                          // Applications that use drivers that do not require
                          // baud rates can set this to zero or any other value.
                          // Drivers that do not require a baud rate should
                          // ignore this field
    function_: UInt32;    // Designates the function of the connection. A value
                          // of zero indictates default behavior for the protocol.
                          // Drivers that do not support multiple functions should
                          // ignore this field.
    drvrDataP: MemPtr;    // Pointer to driver specific data.
    drvrDataSize: UInt16; // Size of the driver specific data block.
    sysReserved1: UInt32; // System Reserved
    sysReserved2: UInt32; // System Reserved
  end;
  SrmOpenConfigPtr = ^SrmOpenConfigType;
(********************************************************************
 * Transfer modes for USB
 ********************************************************************)
type
  SrmTransferModeType = Enum;
const
   srmTransferFirstReserved = 0; // RESERVE 0
   srmUSBInterruptMode = Succ(srmTransferFirstReserved);
   srmUSBBulkMode = Succ(srmUSBInterruptMode);
   srmUSBIsochronous = Succ(srmUSBBulkMode);
(********************************************************************
 * Type of a wakeup handler procedure which can be installed through the
 *   SerSetWakeupHandler() call.
 ********************************************************************)
type
  WakeupHandlerProcPtr = procedure(refCon: UInt32);
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
  BlockingHookProcPtr = function(userRef: UInt32): Boolean;
(********************************************************************
 * Serial Library Control Enumerations (Pilot 2.0)
 ********************************************************************)
(********************************************************************
 * Structure for specifying callback routines.
 ********************************************************************)
  SrmCallbackEntryType = record
    funcP: BlockingHookProcPtr; // function pointer
    userRef: UInt32;            // ref value to pass to callback
  end;
  SrmCallbackEntryPtr = ^SrmCallbackEntryType;
type
  SrmCtlEnum = Enum;
const
  srmCtlFirstReserved = 0; // RESERVE 0
  srmCtlSetBaudRate = Succ(srmCtlFirstReserved); // Sets the current baud rate for the HW.
                                                 // valueP = Pointer to Int32, valueLenP = Pointer to sizeof(Int32)
  srmCtlGetBaudRate = Succ(srmCtlSetBaudRate);   // Gets the current baud rate for the HW.
  srmCtlSetFlags = Succ(srmCtlGetBaudRate);      // Sets the current flag settings for the serial HW.
  srmCtlGetFlags = Succ(srmCtlSetFlags);         // Gets the current flag settings the serial HW.
  srmCtlSetCtsTimeout = Succ(srmCtlGetFlags);    // Sets the current Cts timeout value.
  srmCtlGetCtsTimeout = Succ(srmCtlSetCtsTimeout); // Gets the current Cts timeout value.
  srmCtlStartBreak = Succ(srmCtlGetCtsTimeout);    // turn RS232 break signal on:
                                                   // users are responsible for ensuring that the break is set
                                                   // long enough to genearate a valid BREAK!
                                                   // valueP = 0, valueLenP = 0
  srmCtlStopBreak = Succ(srmCtlStartBreak);        // turn RS232 break signal off:
                                                   // valueP = 0, valueLenP = 0
  srmCtlStartLocalLoopback = Succ(srmCtlStopBreak); // Start local loopback test
                                                    // valueP = 0, valueLenP = 0
  srmCtlStopLocalLoopback = Succ(srmCtlStartLocalLoopback); // Stop local loopback test
                                                            // valueP = 0, valueLenP = 0
  srmCtlIrDAEnable = Succ(srmCtlStopLocalLoopback); // Enable  IrDA connection on this serial port
                                                    // valueP = 0, valueLenP = 0
  srmCtlIrDADisable = Succ(srmCtlIrDAEnable);       // Disable  IrDA connection on this serial port
                                                    // valueP = 0, valueLenP = 0
  srmCtlRxEnable = Succ(srmCtlIrDADisable);         // enable receiver  ( for IrDA )
  srmCtlRxDisable = Succ(srmCtlRxEnable);           // disable receiver ( for IrDA )
  srmCtlEmuSetBlockingHook = Succ(srmCtlRxDisable); // Set a blocking hook routine FOR EMULATION
                                                    // MODE ONLY - NOT SUPPORTED ON THE PILOT
                                                    //PASS:
                                                    // valueP = Pointer to SerCallbackEntryType
                                                    // *valueLenP = sizeof(SerCallbackEntryType)
                                                    //RETURNS:
                                                    // the old settings in the first argument
  srmCtlUserDef = Succ(srmCtlEmuSetBlockingHook);   // Specifying this opCode passes through a user-defined
                                                    //  function to the DrvControl function. This is for use
                                                    //  specifically by serial driver developers who need info
                                                    //  from the serial driver that may not be available through the
                                                    //  standard SrmMgr interface.
  srmCtlGetOptimalTransmitSize = Succ(srmCtlUserDef); // This function will ask the port for the most efficient buffer size
                                                      // for transmitting data packets.  This opCode returns serErrNotSupported
                                                      // if the physical or virtual device does not support this feature.
                                                      // The device can return a transmit size of 0, if send buffering is
                                                      // requested, but the actual size is up to the caller to choose.
                                                      // valueP = Pointer to UInt32 --> return optimal buf size
                                                      // ValueLenP = sizeof(UInt32)
  srmCtlSetDTRAsserted = Succ(srmCtlGetOptimalTransmitSize); // Enable or disable DTR.
  srmCtlGetDTRAsserted = Succ(srmCtlSetDTRAsserted); // Determine if DTR is enabled or disabled.
  srmCtlSetYieldPortCallback = Succ(srmCtlGetDTRAsserted); // Set the yield port callback
  srmCtlSetYieldPortRefCon = Succ(srmCtlSetYieldPortCallback); // Set the yield port refNum
                                 // ***** ADD NEW ENTRIES BEFORE THIS ONE
  srmCtlSystemReserved = $7000; // Reserve control op code space for system use.
  srmCtlCustom = $8000; // Reserve control op code space for licensee use.
  srmCtlLAST = Succ(srmCtlCustom);
const
  srmCtlSystemStart = $7000; // Start poitn for system op codes.
  srmCtlCustomStart = $8000; // Start point for custom op codes.
(********************************************************************
 * Serial Hardware Library Routines
 ********************************************************************)
// *****************************************************************
// * New Serial Manager trap selectors
// *****************************************************************
type
  sysSerialSelector = Enum; // The order of this enum *MUST* match the sysSerialSelector in SerialMgr.c
const
  sysSerialInstall = 0;
  sysSerialOpen = Succ(sysSerialInstall);
  sysSerialOpenBkgnd = Succ(sysSerialOpen);
  sysSerialClose = Succ(sysSerialOpenBkgnd);
  sysSerialSleep = Succ(sysSerialClose);
  sysSerialWake = Succ(sysSerialSleep);
  sysSerialGetDeviceCount = Succ(sysSerialWake);
  sysSerialGetDeviceInfo = Succ(sysSerialGetDeviceCount);
  sysSerialGetStatus = Succ(sysSerialGetDeviceInfo);
  sysSerialClearErr = Succ(sysSerialGetStatus);
  sysSerialControl = Succ(sysSerialClearErr);
  sysSerialSend = Succ(sysSerialControl);
  sysSerialSendWait = Succ(sysSerialSend);
  sysSerialSendCheck = Succ(sysSerialSendWait);
  sysSerialSendFlush = Succ(sysSerialSendCheck);
  sysSerialReceive = Succ(sysSerialSendFlush);
  sysSerialReceiveWait = Succ(sysSerialReceive);
  sysSerialReceiveCheck = Succ(sysSerialReceiveWait);
  sysSerialReceiveFlush = Succ(sysSerialReceiveCheck);
  sysSerialSetRcvBuffer = Succ(sysSerialReceiveFlush);
  sysSerialRcvWindowOpen = Succ(sysSerialSetRcvBuffer);
  sysSerialRcvWindowClose = Succ(sysSerialRcvWindowOpen);
  sysSerialSetWakeupHandler = Succ(sysSerialRcvWindowClose);
  sysSerialPrimeWakeupHandler = Succ(sysSerialSetWakeupHandler);
  sysSerialOpenV4 = Succ(sysSerialPrimeWakeupHandler);
  sysSerialOpenBkgndV4 = Succ(sysSerialOpenV4);
  sysSerialCustomControl = Succ(sysSerialOpenBkgndV4);
// Used by SerialMgrDispatch.c
  maxSerialSelector = sysSerialCustomControl;

function SerialMgrInstall: Err; syscall sysTrapSerialDispatch, sysSerialInstall;
function SrmOpen(port, baud: UInt32; var newPortIdP: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialOpen;
function SrmExtOpen(port: UInt32; var configP: SrmOpenConfigType; configSize: UInt16; var newPortIdP: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialOpenV4;
function SrmExtOpenBackground(port: UInt32; var configP: SrmOpenConfigType; configSize: UInt16; var newPortIdP: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialOpenBkgndV4;
function SrmOpenBackground(port, baud: UInt32; var newPortIdP: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialOpenBkgnd;
function SrmClose(portId: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialClose;
function SrmSleep: Err; syscall sysTrapSerialDispatch, sysSerialSleep;
function SrmWake: Err; syscall sysTrapSerialDispatch, sysSerialWake;
function SrmGetDeviceCount(var numOfDevicesP: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialGetDeviceCount;
function SrmGetDeviceInfo(deviceID: UInt32; var deviceInfoP: DeviceInfoType): Err; syscall sysTrapSerialDispatch, sysSerialGetDeviceInfo;
function SrmGetStatus(portId: UInt16; var statusFieldP: UInt32; var lineErrsP: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialGetStatus;
function SrmClearErr(portId: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialClearErr;
function SrmControl(portId, op: UInt16; valueP: Pointer; var valueLenP: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialControl;
function SrmCustomControl(portId, opCode: UInt16; creator: UInt32; valueP: Pointer; var valueLenP: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialCustomControl;
function SrmSend(portId: UInt16; const bufP: Pointer; count: UInt32; var errP: Err): UInt32; syscall sysTrapSerialDispatch, sysSerialSend;
function SrmSendWait(portId: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialSendWait;
function SrmSendCheck(portId: UInt16; var numBytesP: UInt32): Err; syscall sysTrapSerialDispatch, sysSerialSendCheck;
function SrmSendFlush(portId: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialSendFlush;
function SrmReceive(portId: UInt16; rcvBufP: Pointer; count: UInt32; timeout: Int32; var errP: Err): UInt32; syscall sysTrapSerialDispatch, sysSerialReceive;
function SrmReceiveWait(portId: UInt16; bytes: UInt32; timeout: Int32): Err; syscall sysTrapSerialDispatch, sysSerialReceiveWait;
function SrmReceiveCheck(portId: UInt16; var numBytesP: UInt32): Err; syscall sysTrapSerialDispatch, sysSerialReceiveCheck;
function SrmReceiveFlush(portId: UInt16; timeout: Int32): Err; syscall sysTrapSerialDispatch, sysSerialReceiveFlush;
function SrmSetReceiveBuffer(portId: UInt16; bufP: Pointer; bufSize: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialSetRcvBuffer;
function SrmReceiveWindowOpen(portId: UInt16; var bufPP: UInt8Ptr; var sizeP: UInt32): Err; syscall sysTrapSerialDispatch, sysSerialRcvWindowOpen;
function SrmReceiveWindowClose(portId: UInt16; bytesPulled: UInt32): Err; syscall sysTrapSerialDispatch, sysSerialRcvWindowClose;
function SrmSetWakeupHandler(portId: UInt16; procP: WakeupHandlerProcPtr; refCon: UInt32): Err; syscall sysTrapSerialDispatch, sysSerialSetWakeupHandler;
function SrmPrimeWakeupHandler(portId: UInt16; minBytes: UInt16): Err; syscall sysTrapSerialDispatch, sysSerialPrimeWakeupHandler;

//procedure SrmSelectorErrPrv(UInt16 serialSelector);  // used only by SerialMgrDispatch.c

implementation

end.
