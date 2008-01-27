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

{
        external declarations for Parallel Port Driver
}

unit parallel;

INTERFACE

uses exec;


Type

    pIOPArray = ^tIOPArray;
    tIOPArray = record
        PTermArray0     : ULONG;
        PTermArray1     : ULONG;
    end;


{****************************************************************}
{ CAUTION !!  IF YOU ACCESS the parallel.device, you MUST (!!!!) use
   an IOExtPar-sized structure or you may overlay innocent memory !! }
{****************************************************************}

    pIOExtPar = ^tIOExtPar;
    tIOExtPar = record
        IOPar           : tIOStdReq;

{     STRUCT    MsgNode
*   0   APTR     Succ
*   4   APTR     Pred
*   8   UBYTE    Type
*   9   UBYTE    Pri
*   A   APTR     Name
*   E   APTR     ReplyPort
*  12   UWORD    MNLength
*     STRUCT   IOExt
*  14   APTR     io_Device
*  18   APTR     io_Unit
*  1C   UWORD    io_Command
*  1E   UBYTE    io_Flags
*  1F   UBYTE    io_Error
*     STRUCT   IOStdExt
*  20   ULONG    io_Actual
*  24   ULONG    io_Length
*  28   APTR     io_Data
*  2C   ULONG    io_Offset
*  30 }
        io_PExtFlags    : ULONG;      { (not used) flag extension area }
        io_Status       : Byte;         { status of parallel port and registers }
        io_ParFlags     : Byte;         { see PARFLAGS bit definitions below }
        io_PTermArray   : tIOPArray;     { termination character array }
    end;

Const

    PARB_SHARED         = 5;    { ParFlags non-exclusive access bit }
    PARF_SHARED         = 32;   {     "     non-exclusive access mask }
    PARB_RAD_BOOGIE     = 3;    {     "     (not yet implemented) }
    PARF_RAD_BOOGIE     = 8;    {     "     (not yet implemented) }
    PARB_EOFMODE        = 1;    {     "     EOF mode enabled bit }
    PARF_EOFMODE        = 2;    {     "     EOF mode enabled mask }
    IOPARB_QUEUED       = 6;    { IO_FLAGS rqst-queued bit }
    IOPARF_QUEUED       = 64;   {     "     rqst-queued mask }
    IOPARB_ABORT        = 5;    {     "     rqst-aborted bit }
    IOPARF_ABORT        = 32;   {     "     rqst-aborted mask }
    IOPARB_ACTIVE       = 4;    {     "     rqst-qued-or-current bit }
    IOPARF_ACTIVE       = 16;   {     "     rqst-qued-or-current mask }
    IOPTB_RWDIR         = 3;    { IO_STATUS read=0,write=1 bit }
    IOPTF_RWDIR         = 8;    {     "     read=0,write=1 mask }
    IOPTB_PARSEL        = 2;    {     "     printer selected on the A1000 }
    IOPTF_PARSEL        = 4;    { printer selected & serial "Ring Indicator"
                                  on the A500 & A2000.  Be careful when
                                  making cables }
    IOPTB_PAPEROUT      = 1;    {     "     paper out bit }
    IOPTF_PAPEROUT      = 2;    {     "     paper out mask }
    IOPTB_PARBUSY       = 0;    {     "     printer in busy toggle bit }
    IOPTF_PARBUSY       = 1;    {     "     printer in busy toggle mask }

{ Note: previous versions of this include files had bits 0 and 2 swapped }

    PARALLELNAME        : PChar = 'parallel.device';

    PDCMD_QUERY         = CMD_NONSTD;
    PDCMD_SETPARAMS     = CMD_NONSTD + 1;

    ParErr_DevBusy      = 1;
    ParErr_BufTooBig    = 2;
    ParErr_InvParam     = 3;
    ParErr_LineErr      = 4;
    ParErr_NotOpen      = 5;
    ParErr_PortReset    = 6;
    ParErr_InitErr      = 7;

IMPLEMENTATION

end.
