{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort.

    A few general purpose routines. General purpose enough for *BSD
     and Linux at least.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function CreateShellArgV(const prog:string):ppchar; deprecated;

function CreateShellArgV(const prog:Ansistring):ppchar; deprecated;

procedure FreeShellArgV(p:ppchar); deprecated;
