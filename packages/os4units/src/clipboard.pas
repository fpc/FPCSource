{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    clipboard device functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit clipboard;
{$PACKRECORDS 2}
interface

uses
  exec;

const
  CBD_POST           = CMD_NONSTD + 0;
  CBD_CURRENTREADID  = CMD_NONSTD + 1;
  CBD_CURRENTWRITEID = CMD_NONSTD + 2;
  CBD_CHANGEHOOK     = CMD_NONSTD + 3;

  CBERR_OBSOLETEID   = 1;

type
  PClipboardUnitPartial = ^TClipboardUnitPartial;
  TClipboardUnitPartial = record
    cu_Node: TNode;       // list of units
    cu_UnitNum: LongWord; // unit number for this unit
    // the remaining unit data is private to the device
  end;


  PIOClipReq = ^TIOClipReq;
  TIOClipReq = record
    io_Message: TMessage;
    io_Device: PDevice;             // device node pointer
    io_Unit: PClipboardUnitPartial; // unit (driver private)
    io_Command: Word;               // device command
    io_Flags: Byte;                 // including QUICK and SATISFY
    io_Error: Shortint;             // error or warning num
    io_Actual: LongWord;            // number of bytes transferred
    io_Length: LongWord;            // number of bytes requested
    io_Data: STRPTR;                // either clip stream or post port
    io_Offset: LongWord;            // offset in clip stream
    io_ClipID: Longint;             // ordinal clip identifier
  end;

const
  PRIMARY_CLIP = 0; // primary clip unit

type
  PSatisfyMsg = ^TSatisfyMsg;
  TSatisfyMsg = record
    sm_Msg: TMessage;   // the length will be 6
    sm_Unit: Word;      // which clip unit this is
    sm_ClipID: Longint; // the clip identifier of the post
  end;

  PClipHookMsg = ^TClipHookMsg;
  TClipHookMsg = record
    chm_Type: LongWord;     // zero for this structure format
    chm_ChangeCmd: LongInt; // command that caused this hook invocation: either CMD_UPDATE OR CBD_POST
    chm_ClipID : Longint;   // the clip identifier of the new data
  end;

implementation

end.
