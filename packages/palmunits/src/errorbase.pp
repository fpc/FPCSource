(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: ErrorBase.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Error Management
 *
 * History:
 *    10/25/94 RM    Created by Ron Marianetti
 *    10/09/98 Bob   Fill in all macros, fix defns w/ do{}while(0)
 *    08/05/99 kwk   Added menuErrorClass from Gavin's Menu.c
 *    05/10/00 kwk   Added intlErrorClass.
 *    08/24/00 SCL   Added hwrErrorClass.
 *
 *-----------------------------------------------------------------------
 * Exception Handling
 *
 *    This unit implements an exception handling mechanism that is similar
 *    to "real" C++ Exceptions. Our Exceptions are untyped, and there
 *    must be one and only one Catch block for each Try block.
 *
 * Try/Catch Syntax:
 *
 *    ErrTry {
 *       // Do something which may fail.
 *       // Call ErrThrow() to signal failure and force jump
 *       // to the following Catch block.
 *    }
 *
 *    ErrCatch(inErr) {
 *       // Recover or cleanup after a failure in the above Try block.
 *       // "inErr" is an ExceptionCode identifying the reason
 *       // for the failure.
 *
 *       // You may call Throw() if you want to jump out to
 *       // the next Catch block.
 *
 *       // The code in this Catch block does not execute if
 *       // the above Try block completes without a Throw.
 *
 *    } ErrEndCatch
 *
 *    You must structure your code exactly as above. You can't have a
 *    ErrTry { } without a ErrCatch { } ErrEndCatch, or vice versa.
 *
 *
 * ErrThrow
 *
 *    To signal failure, call ErrThrow() from within a Try block. The
 *    Throw can occur anywhere in the Try block, even within functions
 *    called from the Try block. A ErrThrow() will jump execution to the
 *    start of the nearest Catch block, even across function calls.
 *    Destructors for stack-based objects which go out of scope as
 *    a result of the ErrThrow() are called.
 *
 *    You can call ErrThrow() from within a Catch block to "rethrow"
 *    the exception to the next nearest Catch block.
 *
 *
 * Exception Codes
 *
 *    An ExceptionCode is a 32-bit number. You will normally use
 *    Pilot error codes, which are 16-bit numbers. This allows
 *    plently of room for defining codes for your own kinds of errors.
 *
 *
 * Limitations
 *
 *    Try/Catch and Throw are based on setjmp/longjmp. At the
 *    beginning of a Try block, setjmp saves the machine registers.
 *    Throw calls longjmp, which restores the registers and jumps
 *    to the beginning of the Catch block. Therefore, any changes
 *    in the Try block to variables stored in registers will not
 *    be retained when entering the Catch block.
 *
 *    The solution is to declare variables that you want to use
 *    in both the Try and Catch blocks as "volatile". For example:
 *
 *    volatile long  x = 1;      // Declare volatile local variable
 *    ErrTry {
 *       x = 100;                // Set local variable in Try
 *       ErrThrow(-1);
 *    }
 *
 *    ErrCatch(inErr) {
 *       if (x > 1) {            // Use local variable in Catch
 *          SysBeep(1);
 *       }
 *    } ErrEndCatch
 *
 *****************************************************************************)

unit errorbase;

interface

uses palmos, coretraps;

// Max message length supported by ErrCustomAlert
const
  errMaxMsgLength = 511;

(************************************************************
 * Error Classes for each manager
 *************************************************************)

  errNone         = $0000; // No error

  memErrorClass   = $0100; // Memory Manager
  dmErrorClass    = $0200; // Data Manager
  serErrorClass   = $0300; // Serial Manager
  slkErrorClass   = $0400; // Serial Link Manager
  sysErrorClass   = $0500; // System Manager
  fplErrorClass   = $0600; // Floating Point Library
  flpErrorClass   = $0680; // New Floating Point Library
  evtErrorClass   = $0700; // System Event Manager
  sndErrorClass   = $0800; // Sound Manager
  almErrorClass   = $0900; // Alarm Manager
  timErrorClass   = $0A00; // Time Manager
  penErrorClass   = $0B00; // Pen Manager
  ftrErrorClass   = $0C00; // Feature Manager
  cmpErrorClass   = $0D00; // Connection Manager (HotSync)
  dlkErrorClass   = $0E00; // Desktop Link Manager
  padErrorClass   = $0F00; // PAD Manager
  grfErrorClass   = $1000; // Graffiti Manager
  mdmErrorClass   = $1100; // Modem Manager
  netErrorClass   = $1200; // Net Library
  htalErrorClass  = $1300; // HTAL Library
  inetErrorClass  = $1400; // INet Library
  exgErrorClass   = $1500; // Exg Manager
  fileErrorClass  = $1600; // File Stream Manager
  rfutErrorClass  = $1700; // RFUT Library
  txtErrorClass   = $1800; // Text Manager
  tsmErrorClass   = $1900; // Text Services Library
  webErrorClass   = $1A00; // Web Library
  secErrorClass   = $1B00; // Security Library
  emuErrorClass   = $1C00; // Emulator Control Manager
  flshErrorClass  = $1D00; // Flash Manager
  pwrErrorClass   = $1E00; // Power Manager
  cncErrorClass   = $1F00; // Connection Manager (Serial Communication)
  actvErrorClass  = $2000; // Activation application
  radioErrorClass = $2100; // Radio Manager (Library)
  dispErrorClass  = $2200; // Display Driver Errors.
  bltErrorClass   = $2300; // Blitter Driver Errors.
  winErrorClass   = $2400; // Window manager.
  omErrorClass    = $2500; // Overlay Manager
  menuErrorClass  = $2600; // Menu Manager

  lz77ErrorClass  = $2700; // Lz77 Library
  smsErrorClass   = $2800; // Sms Library
  expErrorClass   = $2900; // Expansion Manager and Slot Driver Library
  vfsErrorClass   = $2A00; // Virtual Filesystem Manager and Filesystem library
  lmErrorClass    = $2B00; // Locale Manager
  intlErrorClass  = $2C00; // International Manager
  pdiErrorClass   = $2D00; // PDI Library
  attnErrorClass  = $2E00; // Attention Manager
  telErrorClass   = $2F00; // Telephony Manager
  hwrErrorClass   = $3000; // Hardware Manager (HAL)
  blthErrorClass  = $3100; // Bluetooth Library Error Class
  udaErrorClass   = $3200; // UDA Manager Error Class

  oemErrorClass   = $7000; // OEM/Licensee errors (0x7000-0x7EFF shared among ALL partners)
  errInfoClass    = $7F00; // special class shows information w/o error code
  appErrorClass   = $8000; // Application-defined errors

(********************************************************************
 * Try / Catch / Throw support
 *
 * ---------------------------------------------------------------------
 * Exception Handler structure
 *
 *  An ErrExceptionType object is created for each ErrTry & ErrCatch block.
 *  At any point in the program, there is a linked list of
 *  ErrExceptionType objects. GErrFirstException points to the
 *  most recently entered block. A ErrExceptionType blocks stores
 *  information about the state of the machine (register values)
 *  at the start of the Try block
 ********************************************************************)

type
  ErrJumpBuf = array [0..12-1] of ^Integer; // D3-D7,PC,A2-A7

// Structure used to store Try state.
type
  ErrExceptionPtr = ^ErrExceptionType;
  ErrExceptionType = record
    nextP: ErrExceptionPtr;   // next exception type
    state: ErrJumpBuf;        // setjmp/longjmp storage
    err: Int32;               // Error code
  end;

// Try & Catch macros
(*
#define ErrTry
    {
        ErrExceptionType    _TryObject;
        _TryObject.err = 0;
        _TryObject.nextP = (ErrExceptionPtr)*ErrExceptionList();
        *ErrExceptionList() = (MemPtr)&_TryObject;
        if (ErrSetJump(_TryObject.state) == 0) {
*)

// NOTE: All variables referenced in and after the ErrCatch must
// be declared volatile.  Here's how for variables and pointers:
// volatile UInt16                  oldMode;
//  ShlDBHdrTablePtr volatile hdrTabP = nil;
// If you have many local variables after the ErrCatch you may
// opt to put the ErrTry and ErrCatch in a separate enclosing function.
(*
#define ErrCatch(theErr)
            *ErrExceptionList() = (MemPtr)_TryObject.nextP;
            }
        else {
            Int32   theErr = _TryObject.err;
            *ErrExceptionList() = (MemPtr)_TryObject.nextP;
*)

(*
#define ErrEndCatch
            }
    }
*)

(********************************************************************
 * Error Manager Routines
 ********************************************************************)

//function ErrSetJump(buf: ErrJumpBuf): Int16; syscall sysTrapErrSetJump;

//procedure ErrLongJump(buf: ErrJumpBuf; result: Int16); syscall sysTrapErrLongJump;

function ErrExceptionList: MemPtrPtr; syscall sysTrapErrExceptionList;

procedure ErrThrow(err_: Int32); syscall sysTrapErrThrow;

procedure ErrDisplayFileLineMsg(const filename: PChar; lineNo: UInt16; const msg: PChar); syscall sysTrapErrDisplayFileLineMsg;

//---------------------------------------------------------------------
// 2/25/98 - New routine for PalmOS >3.0 to display a UI alert for
// run-time errors. This is most likely to be used by network applications
// that are likely to encounter run-time errors like can't find the server,
//  network down, etc. etc.
//
// This routine will lookup the text associated with 'errCode' and display
//  it in an alert. If errMsgP is not NULL, then that text will be used
//  instead of the associated 'errCode' text. If 'preMsgP' or 'postMsgP'
//  is not null, then that text will be pre-pended or post-pended
//  respectively.
//
// Apps that don't use the extra parameters may want to just use the
//  macro below 'ErrAlert'
//---------------------------------------------------------------------

function ErrAlertCustom(errCode: Err; errMsgP, preMsgP, postMsgP: PChar): UInt16; syscall sysTrapErrAlertCustom;

function ErrAlert(err: Err): UInt16;

implementation

function ErrAlert(err: Err): UInt16;
begin
  ErrAlert := ErrAlertCustom(err, nil, nil, nil);
end;

end.
