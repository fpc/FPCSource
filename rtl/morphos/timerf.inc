{
    This file is part of the Free Pascal run time library.

    timer.device functions (V50) for MorphOS/PowerPC
    Copyright (c) 2002-3 The MorphOS Development Team, All Rights Reserved.

    Free Pascal conversion
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


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


