{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit romboot_base;

INTERFACE

uses exec;

Type

    pRomBootBase = ^tRomBootBase;
    tRomBootBase = record
        LibNode         : tLibrary;
        ExecBase        : pExecBase;
        BootList        : tList;
        Reserved        : Array [0..3] of Longint;
                                { for future expansion }
    end;

Const

    ROMBOOT_NAME : PChar = 'romboot.library';

IMPLEMENTATION

end.
