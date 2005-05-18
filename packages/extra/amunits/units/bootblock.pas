{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2000 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit bootblock;

INTERFACE

type
    pBootBlock = ^tBootBlock;
    tBootBlock = record
        bb_id           : Array [0..3] of Byte; { 4 character identifier }
        bb_chksum       : Longint;              { boot block checksum (balance) }
        bb_dosblock     : Longint;              { reserved for DOS patch }
    end;

const
    BOOTSECTS   = 2;    { 1K bootstrap }

    BBID_DOS    : PChar = 'DOS';
    BBID_KICK   : PChar = 'KICK';

    BBNAME_DOS  = $444F5300;    { DOS\0 as an Integer }
    BBNAME_KICK = $4B49434B;    { KICK as an Integer }

IMPLEMENTATION

end.
