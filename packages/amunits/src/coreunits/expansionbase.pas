{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2002 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:

    Typo in ExpansionBase Record.
    11 Nov. 2002

    nils.sjoholm@mailbox.swipnet.se
}


unit expansionbase;

INTERFACE

uses exec, configvars;

Const

    TOTALSLOTS  = 256;

Type

{ BootNodes are scanned by dos.library at startup.  Items found on the
   list are started by dos. BootNodes are added with the AddDosNode() or
   the V36 AddBootNode() calls. }

   pBootNode = ^tBootNode;
   tBootNode = record
    bn_Node     : tNode;
    bn_Flags    : Word;
    bn_DeviceNode  : Pointer;
   END;

    pExpansionBase = ^tExpansionBase;
    tExpansionBase = record
        LibNode         : tLibrary;
        Flags           : Byte;
        eb_Private01    : Byte;
        eb_Private02    : ULONG;
        eb_Private03    : ULONG;
        eb_Private04    : tCurrentBinding;
        eb_Private05    : tList;
        MountList       : tList;
        { private }
    end;

CONST
{ error codes }
     EE_OK          = 0 ;
     EE_LASTBOARD   = 40;  { could not shut him up }
     EE_NOEXPANSION = 41;  { not enough expansion mem; board shut up }
     EE_NOMEMORY    = 42;  { not enough normal memory }
     EE_NOBOARD     = 43;  { no board at that address }
     EE_BADMEM      = 44;  { tried to add bad memory card }

{ Flags }
     EBB_CLOGGED    = 0;       { someone could not be shutup }
     EBF_CLOGGED    = 1;
     EBB_SHORTMEM   = 1;       { ran out of expansion mem }
     EBF_SHORTMEM   = 2;
     EBB_BADMEM     = 2;       { tried to add bad memory card }
     EBF_BADMEM     = 4;
     EBB_DOSFLAG    = 3;       { reserved for use by AmigaDOS }
     EBF_DOSFLAG    = 8;
     EBB_KICKBACK33 = 4;       { reserved for use by AmigaDOS }
     EBF_KICKBACK33 = 16;
     EBB_KICKBACK36 = 5;       { reserved for use by AmigaDOS }
     EBF_KICKBACK36 = 32;
{ If the following flag is set by a floppy's bootblock code, the initial
   open of the initial shell window will be delayed until the first output
   to that shell.  Otherwise the 1.3 compatible behavior applies. }
     EBB_SILENTSTART = 6;
     EBF_SILENTSTART = 64;

{ Magic kludge for CC0 use }
    EBB_START_CC0    = 7;
    EBF_START_CC0    = 128;

IMPLEMENTATION

end.
