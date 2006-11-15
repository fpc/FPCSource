{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Karoly Balogh

    utility.library functions for AmigaOS 4.x/PowerPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


procedure Amiga2Date(date_amiga: longword; cd: PClockData); syscall IUtility 172;
function CheckDate(date: PClockData): longword; syscall IUtility 176;
function Date2Amiga(date: PClockData): longword; syscall IUtility 180;
