{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: InsPoint.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *        This file defines insertion point routines.
 *
 * History:
 *    Jan 25, 1995   Created by Art Lamb
 *
 *****************************************************************************)

unit inspoint;

interface

uses palmos, coretraps, systemmgr;

// Blink interval is half of a second
const
  insPtBlinkInterval = sysTicksPerSecond_ div 2;
  insPtWidth = 2;

procedure InsPtInitialize; syscall sysTrapInsPtInitialize;

procedure InsPtSetLocation(const x, y: Int16); syscall sysTrapInsPtSetLocation;

procedure InsPtGetLocation(var x, y: Int16); syscall sysTrapInsPtGetLocation;

procedure InsPtEnable(enableIt: Boolean); syscall sysTrapInsPtEnable;

function InsPtEnabled: Boolean; syscall sysTrapInsPtEnabled;

procedure InsPtSetHeight(const height: Int16); syscall sysTrapInsPtSetHeight;

function InsPtGetHeight: Int16; syscall sysTrapInsPtGetHeight;

procedure InsPtCheckBlink; syscall sysTrapInsPtCheckBlink;

implementation

end.
