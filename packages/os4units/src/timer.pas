{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    dos.library functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$PACKRECORDS 2}

unit timer;

interface

uses
  exec;

const
// unit defintions
// enTimerUnits
  UNIT_MICROHZ    = 0; // 1/1000000 second granularity
  UNIT_VBLANK     = 1; // 1/60 second granularity
  UNIT_ECLOCK     = 2; // system dependant number of ticks/second
  UNIT_WAITUNTIL  = 3; // wait until a certain point of time
  UNIT_WAITECLOCK = 4; // wait until a certain point of time (in EClock ticks)
  UNIT_ENTROPY    = 5; // Read entropy data

  TIMERNAME : PChar   = 'timer.device';

type
  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_secs: LongWord;
    tv_micro: LongWord;
  end;

  PTimeRequest = ^TTimeRequest;
  TTimeRequest = record
    tr_node: TIORequest;
    tr_time: TTimeVal;
  end;

  PEClockVal = ^TEClockVal;
  TEClockVal = record
    ev_hi: LongWord;
    ev_lo: LongWord;
  end;

const
// enTimerCmd
  TR_ADDREQUEST = CMD_NONSTD;
  TR_GETSYSTIME = CMD_NONSTD + 1;
  TR_SETSYSTIME = CMD_NONSTD + 2;

{ To use any of the routines below, TimerBase must be set to point
  to the timer.device, either by calling CreateTimer or by pulling
  the device pointer from a valid TimeRequest, and get the
  Interface i.e.
    TimerBase := TimeRequest.io_Device;
    ITimer := GetInterface(TimerBase, 'main', 1, nil);
  _after_ you have called OpenDevice on the timer.}
var
  TimerBase: Pointer = nil;
  ITimer: Pointer = nil;

function TimerObtain(): LongWord; syscall ITimer 60;
function TimerRelease(): LongWord; syscall ITimer 64;
procedure TimerExpunge(); syscall ITimer 68;
function TimerClone(): PInterface; syscall ITimer 72;
procedure AddTime(Dest: PTimeVal; Src: PTimeVal); syscall TimerBase 76;
function CmpTime(Dest: PTimeVal; Src: PTimeVal): LongInt; syscall TimerBase 80;
procedure SubTime(Dest: PTimeVal; Src: PTimeVal); syscall TimerBase 84;
function ReadEClock(Dest: PEClockVal): LongWord; syscall TimerBase 88;
procedure GetSysTime(Dest: PTimeVal); syscall TimerBase 92;
procedure GetUpTime(Dest: PTimeVal); syscall TimerBase 96;
procedure MicroDelay(MicroSeconds: LongWord); syscall TimerBase 100;

implementation

end.
