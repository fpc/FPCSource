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

unit serial;

INTERFACE

uses exec;


Type

                   { array of termination char's }
                   { to use,see serial.doc setparams }

    pIOTArray = ^tIOTArray;
    tIOTArray = record
        TermArray0 : ULONG;
        TermArray1 : ULONG;
    end;

Const

    SER_DEFAULT_CTLCHAR = $11130000;    { default chars for xON,xOFF }

{ You may change these via SETPARAMS.   At this time, parity is not
   calculated for xON/xOFF characters.  You must supply them with the
   desired parity. }

{****************************************************************}
{ CAUTION !!  IF YOU ACCESS the serial.device, you MUST (!!!!) use an
   IOExtSer-sized structure or you may overlay innocent memory !! }
{****************************************************************}

Type

    pIOExtSer = ^tIOExtSer;
    tIOExtSer = record
        IOSer   : tIOStdReq;

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
*
*  30 }

        io_CtlChar      : ULONG; { control char's (order = xON,xOFF,INQ,ACK) }
        io_RBufLen      : ULONG; { length in bytes of serial port's read buffer }
        io_ExtFlags     : ULONG; { additional serial flags (see bitdefs below) }
        io_Baud         : ULONG; { baud rate requested (true baud) }
        io_BrkTime      : ULONG; { duration of break signal in MICROseconds }
        io_TermArray    : tIOTArray; { termination character array }
        io_ReadLen      : Byte;   { bits per read character (# of bits) }
        io_WriteLen     : Byte;   { bits per write character (# of bits) }
        io_StopBits     : Byte;   { stopbits for read (# of bits) }
        io_SerFlags     : Byte;   { see SerFlags bit definitions below   }
        io_Status       : Word;
    end;

   { status of serial port, as follows:
*                  BIT  ACTIVE  FUNCTION
*                   0    ---    reserved
*                   1    ---    reserved
*                   2    high   Connected to parallel "select" on the A1000.
*                               Connected to both the parallel "select" and
*                               serial "ring indicator" pins on the A500 &
*                               A2000.  Take care when making cables.
*                   3    low    Data Set Ready
*                   4    low    Clear To Send
*                   5    low    Carrier Detect
*                   6    low    Ready To Send
*                   7    low    Data Terminal Ready
*                   8    high   read overrun
*                   9    high   break sent
*                  10    high   break received
*                  11    high   transmit x-OFFed
*                  12    high   receive x-OFFed
*               13-15           reserved
}

Const

    SDCMD_QUERY         = CMD_NONSTD;
    SDCMD_BREAK         = CMD_NONSTD + 1;
    SDCMD_SETPARAMS     = CMD_NONSTD + 2;


    SERB_XDISABLED      = 7;    { io_SerFlags xOn-xOff feature disabled bit }
    SERF_XDISABLED      = 128;  {    "      xOn-xOff feature disabled mask }
    SERB_EOFMODE        = 6;    {    "      EOF mode enabled bit }
    SERF_EOFMODE        = 64;   {    "      EOF mode enabled mask }
    SERB_SHARED         = 5;    {    "      non-exclusive access bit }
    SERF_SHARED         = 32;   {    "      non-exclusive access mask }
    SERB_RAD_BOOGIE     = 4;    {    "      high-speed mode active bit }
    SERF_RAD_BOOGIE     = 16;   {    "      high-speed mode active mask }
    SERB_QUEUEDBRK      = 3;    {    "      queue this Break ioRqst }
    SERF_QUEUEDBRK      = 8;    {    "      queue this Break ioRqst }
    SERB_7WIRE          = 2;    {    "      RS232 7-wire protocol }
    SERF_7WIRE          = 4;    {    "      RS232 7-wire protocol }
    SERB_PARTY_ODD      = 1;    {    "      parity feature enabled bit }
    SERF_PARTY_ODD      = 2;    {    "      parity feature enabled mask }
    SERB_PARTY_ON       = 0;    {    "      parity-enabled bit }
    SERF_PARTY_ON       = 1;    {    "      parity-enabled mask }

{ These now refect the actual bit positions in the io_Status UWORD }

    IO_STATB_XOFFREAD   = 12;      { io_Status receive currently xOFF'ed bit }
    IO_STATF_XOFFREAD   = $1000;   {     "     receive currently xOFF'ed mask }
    IO_STATB_XOFFWRITE  = 11;      {     "     transmit currently xOFF'ed bit }
    IO_STATF_XOFFWRITE  = $0800;   {     "     transmit currently xOFF'ed mask }
    IO_STATB_READBREAK  = 10;      {     "     break was latest input bit }
    IO_STATF_READBREAK  = $0400;   {     "     break was latest input mask }
    IO_STATB_WROTEBREAK = 9;       {     "     break was latest output bit }
    IO_STATF_WROTEBREAK = $0200;   {     "     break was latest output mask }
    IO_STATB_OVERRUN    = 8;       {     "     status word RBF overrun bit }
    IO_STATF_OVERRUN    = $0100;   {     "     status word RBF overrun mask }


    SEXTB_MSPON         = 1;    { io_ExtFlags. Use mark-space parity, }
                                {           instead of odd-even. }
    SEXTF_MSPON         = 2;    {    "      mark-space parity mask }
    SEXTB_MARK          = 0;    {    "      if mark-space, use mark }
    SEXTF_MARK          = 1;    {    "      if mark-space, use mark mask }


    SerErr_DevBusy      = 1;
    SerErr_BaudMismatch = 2;    { baud rate not supported by hardware }
    SerErr_BufErr       = 4;    { Failed to allocate new read buffer }
    SerErr_InvParam     = 5;
    SerErr_LineErr      = 6;
    SerErr_ParityErr    = 9;
    SerErr_TimerErr     = 11;   {(See the serial/OpenDevice autodoc)}
    SerErr_BufOverflow  = 12;
    SerErr_NoDSR        = 13;
    SerErr_DetectedBreak = 15;


{ These defines refer to the HIGH ORDER byte of io_Status.  They have
   been replaced by the new, corrected ones above }
    IOSTB_XOFFREAD  = 4;       { iost_hob receive currently xOFF'ed bit }
    IOSTF_XOFFREAD  = 16;      {    "     receive currently xOFF'ed mask }
    IOSTB_XOFFWRITE = 3;       {    "     transmit currently xOFF'ed bit }
    IOSTF_XOFFWRITE = 8;       {    "     transmit currently xOFF'ed mask }
    IOSTB_READBREAK = 2;       {    "     break was latest input bit }
    IOSTF_READBREAK = 4;       {    "     break was latest input mask }
    IOSTB_WROTEBREAK= 1;       {    "     break was latest output bit }
    IOSTF_WROTEBREAK= 2;       {    "     break was latest output mask }
    IOSTB_OVERRUN   = 0;       {    "     status word RBF overrun bit }
    IOSTF_OVERRUN   = 1;       {    "     status word RBF overrun mask }

    IOSERB_BUFRREAD = 7  ;     { io_Flags from read buffer bit }
    IOSERF_BUFRREAD = 128;  {    "     from read buffer mask }
    IOSERB_QUEUED   = 6  ;     {    "     rqst-queued bit }
    IOSERF_QUEUED   = 64 ; {    "     rqst-queued mask }
    IOSERB_ABORT    = 5  ;     {    "     rqst-aborted bit }
    IOSERF_ABORT    = 32 ; {    "     rqst-aborted mask }
    IOSERB_ACTIVE   = 4  ;     {    "     rqst-qued-OR-current bit }
    IOSERF_ACTIVE   = 16 ; {    "     rqst-qued-OR-current mask }



    SERIALNAME          : PChar = 'serial.device';

IMPLEMENTATION

end.
