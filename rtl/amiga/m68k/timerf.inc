{
    This file is part of the Free Pascal run time library.

    timer.device functions for AmigaOS/m68k
    Copyright (c) 2020 Karoly Balogh

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ these functions are only available v36+ (OS Release 2.0+) }
{$IFNDEF AMIGA_V1_0_ONLY}
{$IFNDEF AMIGA_V1_2_ONLY}

procedure GetSysTime( Dest : ptimeval location 'a0'); syscall TimerBase 066;

{$ENDIF AMIGA_V1_2_ONLY}
{$ENDIF AMIGA_V1_0_ONLY}
