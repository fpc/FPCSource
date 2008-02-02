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

unit configvars;

INTERFACE

uses exec, configregs;

Type

    pConfigDev = ^tConfigDev;
    tConfigDev = record
        cd_Node         : tNode;
        cd_Flags        : Byte;
        cd_Pad          : Byte;
        cd_Rom          : tExpansionRom; { image of expansion rom area }
        cd_BoardAddr    : Pointer;       { where in memory the board is }
        cd_BoardSize    : ULONG;         { size in bytes }
        cd_SlotAddr     : Word;          { which slot number }
        cd_SlotSize     : Word;          { number of slots the board takes }
        cd_Driver       : Pointer;       { pointer to node of driver }
        cd_NextCD       : pConfigDev;    { linked list of drivers to config }
        cd_Unused       : Array [0..3] of ULONG;
                                         { for whatever the driver whats }
    end;


Const

{ cd_Flags }
    CDB_SHUTUP          = 0;    { this board has been shut up }
    CDB_CONFIGME        = 1;    { this board needs a driver to claim it }
    CDB_BADMEMORY       = 2;
    CDB_PROCESSED       = 3;

    CDF_SHUTUP          = $01;
    CDF_CONFIGME        = $02;
    CDF_BADMEMORY       = $04;
    CDF_PROCESSED       = $08;

Type

{ this structure is used by GetCurrentBinding() and SetCurrentBinding() }

    pCurrentBinding = ^tCurrentBinding;
    tCurrentBinding = record
        cb_ConfigDev    : pConfigDev;           { first configdev in chain }
        cb_FileName     : STRPTR;               { file name of driver }
        cb_ProductString : STRPTR;              { product # string }
        cb_ToolTypes    : POINTER;              { tooltypes from disk object }
    end;

IMPLEMENTATION

end.
