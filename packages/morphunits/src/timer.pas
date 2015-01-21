{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    timer.device interface unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit timer;

interface

uses
  exec;

var
  TimerBase : Pointer;


{ * timer.device definitions (V50)
  *********************************************************************
  * }


const
  UNIT_MICROHZ      = 0;
  UNIT_VBLANK       = 1;
  UNIT_ECLOCK       = 2;
  UNIT_WAITUNTIL    = 3;
  UNIT_WAITECLOCK   = 4;
  { *** V50 *** }
  UNIT_CPUCLOCK     = 5;
  UNIT_WAITCPUCLOCK = 6;

const
  TIMERNAME = 'timer.device';


type
  PTimeVal = ^TTimeVal;
  TTimeVal = packed record
    tv_secs : DWord;
    tv_micro: DWord;
  end;

type
  PEClockVal = ^TEClockVal;
  TEClockVal = packed record
    ev_hi: DWord;
    ev_lo: DWord;
  end;

type
  PTimeRequest = ^TTimeRequest;
  TTimeRequest = packed record
    tr_node: TIORequest;
    tr_time: TTimeVal;
  end;


const
   TR_ADDREQUEST = (CMD_NONSTD);
   TR_GETSYSTIME = (CMD_NONSTD + 1);
   TR_SETSYSTIME = (CMD_NONSTD + 2);


procedure AddTime(Dest  : PTimeVal location 'a0';
                  Source: PTimeVal location 'a1');
SysCall TimerBase 42;

procedure SubTime(Dest  : PTimeVal location 'a0';
                  Source: PTimeVal location 'a1');
SysCall TimerBase 48;

function CmpTime(Dest  : PTimeVal location 'a0';
                 Source: PTimeVal location 'a1'): LongInt;
SysCall TimerBase 54;

function ReadEClock(Dest: PTimeVal location 'a0'): DWord;
SysCall TimerBase 60;

procedure GetSysTime(Dest: PTimeVal location 'a0');
SysCall TimerBase 66;

{.$include timerd.inc}
{.$include timerf.inc}

implementation

begin
end.
