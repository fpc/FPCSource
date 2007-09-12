{
    Copyright (c) 2002 by Marco van de Voort

    deeper calls exported by unit unix

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';

function  fpfStatFS (Fd: cint; Info:pstatfs):cint;
function  fpStatFS  (Path:pchar; Info:pstatfs):cint;
function  fpfsync (fd : cint) : cint;
Function  fpFlock   (fd,mode : cint)   : cint ;


