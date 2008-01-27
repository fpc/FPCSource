{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: PenMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Pen manager
 *
 * History:
 *    6/5/96 Created by Ron Marianetti
 *
 *****************************************************************************)

unit penmgr;

interface

uses palmos, coretraps, rect, errorbase;

(********************************************************************
 * Pen Manager Errors
 * the constant serErrorClass is defined in ErrorBase.h
 ********************************************************************)

const
  penErrBadParam    = penErrorClass or 1;
  penErrIgnorePoint = penErrorClass or 2;

(********************************************************************
 * Pen manager Routines
 ********************************************************************)

// Initializes the Pen Manager
function PenOpen: Err; syscall sysTrapPenOpen;

// Closes the Pen Manager and frees whatever memory it allocated
function PenClose: Err; syscall sysTrapPenClose;


// Put pen to sleep
function PenSleep: Err; syscall sysTrapPenSleep;

// Wake pen
function PenWake: Err; syscall sysTrapPenWake;


// Get the raw pen coordinates from the hardware.
function PenGetRawPen(var penP: PointType): Err; syscall sysTrapPenGetRawPen;

// Reset calibration in preparation for setting it again
function PenResetCalibration: Err; syscall sysTrapPenResetCalibration;

// Set calibration settings for the pen
function PenCalibrate(var digTopLeftP, digBotRightP, scrTopLeftP, scrBotRightP: PointType): Err; syscall sysTrapPenCalibrate;

// Scale a raw pen coordinate into screen coordinates
function PenRawToScreen(var penP: PointType): Err; syscall sysTrapPenRawToScreen;

// Scale a screen pen coordinate back into a raw coordinate
function PenScreenToRaw(var penP: PointType): Err; syscall sysTrapPenScreenToRaw;

(************************************************************
 * Assembly Function Prototypes
 *************************************************************)

// _PenGetRawPen ASM_SYS_TRAP(sysTrapPenGetRawPen)

implementation

end.
