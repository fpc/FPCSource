{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: FatalAlert.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *        This file defines the system Fatal Alert support.
 *
 * History:
 *    September 12, 1994   Created by Art Lamb
 *
 *****************************************************************************)

unit fatalalert;

interface

uses palmos, coretraps;

// Value returned by SysFatalAlert
const
  fatalReset = 0;
  fatalEnterDebugger = 1;
  fatalDoNothing = $FFFF;

function SysFatalAlert(const msg: PChar): UInt16; syscall sysTrapSysFatalAlert;

procedure SysFatalAlertInit; syscall sysTrapSysFatalAlertInit;

implementation

end.
