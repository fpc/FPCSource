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

{$IFNDEF FPC_DOTTEDUNITS}
unit romboot_base;
{$ENDIF FPC_DOTTEDUNITS}

INTERFACE

{$IFDEF FPC_DOTTEDUNITS}
uses Amiga.Core.Exec;
{$ELSE FPC_DOTTEDUNITS}
uses exec;
{$ENDIF FPC_DOTTEDUNITS}

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

    ROMBOOT_NAME : PAnsiChar = 'romboot.library';

IMPLEMENTATION

end.
