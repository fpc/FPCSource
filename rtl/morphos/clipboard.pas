{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    clipboard.device interface unit for MorphOS/PowerPC

    Based on work of Nils Sjoholm member of the Amiga RTL
    development team.

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$PACKRECORDS 2}

unit clipboard;

interface

uses exec;

const
    CBD_POST            = CMD_NONSTD + 0;
    CBD_CURRENTREADID   = CMD_NONSTD + 1;
    CBD_CURRENTWRITEID  = CMD_NONSTD + 2;
    CBD_CHANGEHOOK      = CMD_NONSTD + 3;

    CBERR_OBSOLETEID    = 1;

type

    pClipboardUnitPartial = ^tClipboardUnitPartial;
    tClipboardUnitPartial = record
        cu_Node         : tNode;         { list of units }
        cu_UnitNum      : DWord;      { unit number for this unit }
    { the remaining unit data is private to the device }
    end;


    pIOClipReq = ^tIOClipReq;
    tIOClipReq = record
        io_Message      : tMessage;
        io_Device       : pDevice;      { device node pointer   }
        io_Unit         : pClipboardUnitPartial;      { unit (driver private) }
        io_Command      : Word;        { device command        }
        io_Flags        : Byte;         { including QUICK and SATISFY }
        io_Error        : Shortint;     { error or warning num  }
        io_Actual       : DWord;        { number of bytes transferred }
        io_Length       : DWord;        { number of bytes requested }
        io_Data         : PChar;        { either clip stream or post port }
        io_Offset       : DWord;        { offset in clip stream }
        io_ClipID       : Longint;      { ordinal clip identifier }
    end;

const
    PRIMARY_CLIP        = 0;    { primary clip unit }

type

    pSatisfyMsg = ^tSatisfyMsg;
    tSatisfyMsg = record
        sm_Msg  : tMessage;      { the length will be 6 }
        sm_Unit : Word;          { which clip unit this is }
        sm_ClipID : Longint;     { the clip identifier of the post }
    end;

   pClipHookMsg = ^tClipHookMsg;
   tClipHookMsg = record
    chm_Type   : DWord;          { zero for this structure format }
    chm_ChangeCmd,               { command that caused this hook invocation: }
                                 { either CMD_UPDATE OR CBD_POST }
    chm_ClipID : Longint;        { the clip identifier of the new data }
   END;

implementation

end.
