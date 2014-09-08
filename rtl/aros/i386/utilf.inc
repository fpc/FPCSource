{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Marcus Sackrow

    utility.library functions for AROS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure Amiga2Date(date_amiga: longword; cd: PClockData); syscall AOS_UtilityBase 20;
function Date2Amiga(date: PClockData): longword; syscall AOS_UtilityBase 21;

