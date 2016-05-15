{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    History:
    Removed the var for all functions.
    06 Sep 2000.

    Added the define use_amiga_smartlink.
    13 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se

}

unit timer;

interface

uses
  exec;

const

{ unit defintions }
  UNIT_MICROHZ        = 0;
  UNIT_VBLANK         = 1;
  UNIT_ECLOCK         = 2;
  UNIT_WAITUNTIL      = 3;
  UNIT_WAITECLOCK     = 4;

  TIMERNAME: PChar = 'timer.device';

type
  PTimeval = ^TTimeval;
  TTimeval = record
    tv_secs: LongWord;
    tv_micro: LongWord;
  end;

  PTimerequest = ^TTimerequest;
  TTimerequest = record
    tr_node: TIORequest;
    tr_time: TTimeval;
  end;

  PEClockVal = ^TEClockVal;
  TEClockVal = record
    ev_hi : LongWord;
    ev_lo : LongWord;
  end;


const

{ IO_COMMAND to use for adding a timer }
    TR_ADDREQUEST = CMD_NONSTD;
    TR_GETSYSTIME = CMD_NONSTD + 1;
    TR_SETSYSTIME = CMD_NONSTD + 2;

{  To use any of the routines below, TimerBase must be set to point
   to the timer.device, either by calling CreateTimer or by pulling
   the device pointer from a valid TimeRequest, i.e.

        TimerBase := TimeRequest.io_Device;

    _after_ you have called OpenDevice on the timer.
 }

var
  TimerBase: Pointer;

procedure AddTime(Dest, Source: PTimeVal); syscall TimerBase 7;
function CmpTime(Dest, Source: PTimeVal): LongWord; syscall TimerBase 9;
procedure SubTime(Dest, Source: PTimeVal); syscall TimerBase 8;
function ReadEClock(Dest: PEClockVal): LongInt; syscall TimerBase 10;
procedure GetSysTime(Dest: PTimeVal); syscall TimerBase 11;

implementation

end.
