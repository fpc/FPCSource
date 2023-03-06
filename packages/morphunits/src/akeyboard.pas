{
  This file is part of the Free Pascal MorphOS support package
  Copyright (c) 2015 the Free Pascal Development Team

  keyboard.device interface unit

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit akeyboard;
{$ENDIF FPC_DOTTEDUNITS}

{
   Contents of this file is based on keyboard.h from the MorphOS SDK:

   keyboard.device include
   Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved
}

{ Pascal conversion based on fpc-triforce repo by Magorium }

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Amiga.Core.Exec;
{$ELSE FPC_DOTTEDUNITS}
uses
  exec;
{$ENDIF FPC_DOTTEDUNITS}

const
  KBD_READEVENT               = (CMD_NONSTD + 0);
  KBD_READMATRIX              = (CMD_NONSTD + 1);
  KBD_ADDRESETHANDLER         = (CMD_NONSTD + 2);
  KBD_REMRESETHANDLER         = (CMD_NONSTD + 3);
  KBD_RESETHANDLERDONE        = (CMD_NONSTD + 4);

implementation

end.
