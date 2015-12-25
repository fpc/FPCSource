{
  This file is part of the Free Pascal MorphOS support package
  Copyright (c) 2015 the Free Pascal Development Team

  input.device interface unit

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$MODE FPC}
unit input;

{
   Contents of this file is based on input.h from the MorphOS SDK:

   input.device include (V50)
   Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved
}

{ Pascal conversion based on fpc-triforce repo by Magorium }

interface

uses
  exec;

const
  IND_ADDHANDLER    = CMD_NONSTD + 0;
  IND_REMHANDLER    = CMD_NONSTD + 1;
  IND_WRITEEVENT    = CMD_NONSTD + 2;
  IND_SETTHRESH     = CMD_NONSTD + 3;
  IND_SETPERIOD     = CMD_NONSTD + 4;
  IND_SETMPORT      = CMD_NONSTD + 5;
  IND_SETMTYPE      = CMD_NONSTD + 6;
  IND_SETMTRIG      = CMD_NONSTD + 7;


var
  InputBase: pDevice;

function PeekQualifier: UWORD; syscall InputBase 042;


implementation

end.
