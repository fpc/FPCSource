{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    Platform specific debug functions for Amiga-like systems

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure SysDebug(const s: RawByteString);
procedure SysDebugLn(const s: RawByteString);
procedure SysDebug(const s: ShortString);
procedure SysDebugLn(const s: ShortString);
procedure SysDebugLn;
