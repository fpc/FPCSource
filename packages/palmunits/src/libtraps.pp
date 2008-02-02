(******************************************************************************
 *
 * Copyright (c) 1999-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: LibTraps.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *      Palm OS Shared Library 'default' traps.
 *
 * History:
 *    7/15/99  Created by Bob Ebert
 *      mm/dd/yy   initials - brief revision comment
 *
 *****************************************************************************)

unit libtraps;

interface

uses palmos;

//--------------------------------------------------------------------
// Define Library Trap Numbers
//--------------------------------------------------------------------
// Library traps start here and go up by 1's
const
  sysLibTrapBase = $A800;

type
  SysLibTrapNumber = WordEnum;

const
  sysLibTrapName = sysLibTrapBase;
  sysLibTrapOpen = Succ(sysLibTrapName);
  sysLibTrapClose = Succ(sysLibTrapOpen);
  sysLibTrapSleep = Succ(sysLibTrapClose);
  sysLibTrapWake = Succ(sysLibTrapSleep);
  sysLibTrapCustom = Succ(sysLibTrapWake);

implementation

end.
