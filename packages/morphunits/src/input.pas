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
{$PACKRECORDS 2}
unit input;

{
   Contents of this file is based on input.h from the MorphOS SDK:

   input.device include (V50)
   Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved
}

{ Pascal conversion based on fpc-triforce repo by Magorium }

interface

uses 
  exec, utility;

const
  IND_ADDHANDLER    = CMD_NONSTD + 0;
  IND_REMHANDLER    = CMD_NONSTD + 1;
  IND_WRITEEVENT    = CMD_NONSTD + 2;
  IND_SETTHRESH     = CMD_NONSTD + 3;
  IND_SETPERIOD     = CMD_NONSTD + 4;
  IND_SETMPORT      = CMD_NONSTD + 5;
  IND_SETMTYPE      = CMD_NONSTD + 6;
  IND_SETMTRIG      = CMD_NONSTD + 7;

type
  TInputDeviceData = record
    Device  : PChar;
    unit_   : ULONG;
    flags   : ULONG;
  end;

  {
  * GetInputEventAttr(),SetInputEventAttr() attributes of functons that
  * doesn't seem to exist.. !?
  *
  * This may be a mistake in the comment in the MorphOS SDK. Since
  * InputEvents are BOOPSI objects in MorphOS, it's possible that you
  * can use GetAttr/SetAttrs in theory..? 
  * This is pending for answer by the MorphOS Team. (KB)
  }
const
  INPUTEVENTATTR_TagBase    = TAG_USER + $8000000;
  INPUTEVENTATTR_NEXTEVENT  = (INPUTEVENTATTR_TagBase + 0);
  INPUTEVENTATTR_CLASS      = (INPUTEVENTATTR_TagBase + 1);
  INPUTEVENTATTR_SUBCLASS   = (INPUTEVENTATTR_TagBase + 2);
  INPUTEVENTATTR_CODE       = (INPUTEVENTATTR_TagBase + 3);
  INPUTEVENTATTR_QUALIFIER  = (INPUTEVENTATTR_TagBase + 4);
  INPUTEVENTATTR_X          = (INPUTEVENTATTR_TagBase + 5);
  INPUTEVENTATTR_Y          = (INPUTEVENTATTR_TagBase + 6);
  INPUTEVENTATTR_ADDR       = (INPUTEVENTATTR_TagBase + 7);
  INPUTEVENTATTR_DOWNKEYS   = (INPUTEVENTATTR_TagBase + 8);
  INPUTEVENTATTR_TIMESTAMP  = (INPUTEVENTATTR_TagBase + 9);

var
  InputBase: pDevice;

function PeekQualifier: UWORD; syscall InputBase 042;


implementation

end.
