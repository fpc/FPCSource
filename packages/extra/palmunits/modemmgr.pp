{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: ModemMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Modem Manager
 *
 * History:
 *    9/20/95  VMK - Created by Vitaly Kruglikov
 *
 *****************************************************************************)

unit modemmgr;

interface

uses  palmos, coretraps, errorbase;

(************************************************************
 * Modem Manager constants
 *************************************************************)

const
  mdmMaxStringSize    = 40;

  mdmCmdBufSize       = 81;      // command buffer capacity (including null)
  mdmRespBufSize      = 81;      // reply buffer capacity (including null)
  mdmCmdSize          = 8;       // max storage needed for smartmodem command

  mdmDefCmdTimeOut    = 500000;  // in micro-seconds

  mdmDefDTWaitSec     = 4;
  mdmDefDCDWaitSec    = 70;
  mdmDefSpeakerVolume = 1;
  mdmResetStrInCmdBuf = $01;

// Speaker volume settings
const
  mdmVolumeOff = 0;
  mdmVolumeLow = 1;
  mdmVolumeMed = 2;
  mdmVolumeHigh = 3;

// Modem connection stages (NEW for Pilot 2.0)
type
  MdmStageEnum = Enum;

const
  mdmStageInvalid = 0;                                 // invalid state
  mdmStageReserved = 1;                                // reserved for 1.0 compatibility
  mdmStageFindingModem = Succ(mdmStageReserved);       // checking if modem is present
  mdmStageInitializing = Succ(mdmStageFindingModem);   // initializing the modem
  mdmStageDialing = Succ(mdmStageInitializing);        // dialing the modem
  mdmStageWaitingForCarrier = Succ(mdmStageDialing);   // waiting for carrier detect
  mdmStageHangingUp = Succ(mdmStageWaitingForCarrier); // hanging up the modem

(************************************************************
 * Modem Manager data structures
 *************************************************************)

// Prototype for the "user cancel" check callback function

type
  MdmUserCanProcPtr = function(userRef: UInt32): Int16;

  MdmInfoType = record
    portID: UInt16;                               // serial port ID number.   [NewSerialMgr; replaces serRefNum]
    initialBaud: UInt32;                          // initial baud rate to use
    cmdTimeOut: UInt32;                           // number of micro-sec to wait after a cmd
    dtWaitSec: Int16;                             // dialtone wait (sec) (-1 for modem's default)
    dcdWaitSec: Int16;                            // dcd timeout wait (sec) (-1 for modem's default)
    volume: Int16;                                // speaker volume(see mdmVolume... constants)
    pulse: Boolean;                               // pulse or tone dialing
    hwHShake: Boolean;                            // enable cts/rts handshaking
    autoBaud: Boolean;                            // enable/disable auto-baud to connected baud rate
    telConnection: UInt8;                         // Boolean true if connecting to a mobile phone
                                                  // false otherwise.
    canProcP: MdmUserCanProcPtr;                  // ptr to user-cancel function
    userRef: UInt32;                              // parameter for canProcP()
    cmdBuf: array [0..mdmCmdBufSize-1] of Char;   // build all commands here
    respBuf: array [0..mdmRespBufSize-1] of Char; // response buffer
    connectBaud: UInt32;                          // baud at which connection was established
                                                  // (0 = unknown)
    curStage: UInt8;                              // set by ModemMgr to report current MdmStageEnum
    strInCmdBuf: UInt8;                           // Set to mdmResetStrInCmdBuf if the reset string is
                                                  // stored in the command buffer cmdBuf.  This is to
                                                  // get around a compatibility problem with not being
                                                  // able pass in a reset string.  The reset string
                                                  // must be prefixed with AT.  Set to zero otherwise
  end;

  MdmInfoPtr = ^MdmInfoType;

(************************************************************
 * Modem Manager result codes
 * (mdmErrorClass is defined in ErrorBase.h)
 *************************************************************)

const
  mdmErrNoTone   = mdmErrorClass or 1; // no dial tone
  mdmErrNoDCD    = mdmErrorClass or 2; // no carrier / timeout
  mdmErrBusy     = mdmErrorClass or 3; // busy signal heard
  mdmErrUserCan  = mdmErrorClass or 4; // cancelled by user
  mdmErrCmdError = mdmErrorClass or 5; // command error
  mdmErrNoModem  = mdmErrorClass or 6; // no modem detected
  mdmErrMemory   = mdmErrorClass or 7; // not enough memory
  mdmErrPrefs    = mdmErrorClass or 8; // modem preferences have not been
                                       // setup - (app should take user to modem prefs panel)
  mdmErrDial     = mdmErrorClass or 9; // dial command error - most likely the dial
                                       // string is too long for the modem's buffer or
                                       // contains invalid characters
// <chg 3-7-98 RM> New error code for empty phone number which is only invalid if
//  the modem type is not a "Direct Connect" modem
  mdmErrNoPhoneNum = mdmErrorClass or 10; // No phone number and not "Direct Connect"

(********************************************************************
 * Modem Manager Routines
 * These are define as syscall calls only under emulation mode or
 *  under native mode from the module that actually installs the trap
 *  vectors
 ********************************************************************)

//-------------------------------------------------------------------
// API
//-------------------------------------------------------------------

function MdmDial(modemP: MdmInfoPtr; okDialP, userInitP, phoneNumP: PChar): Err; syscall sysTrapMdmDial;

function MdmHangUp(modemP: MdmInfoPtr): Err; syscall sysTrapMdmHangUp;

(************************************************************
 * Modem Manager Macros
 *************************************************************)

implementation

end.
