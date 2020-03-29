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

interface
{$PACKRECORDS 2}

uses
  exec;

type

  // array of termination char's to use,see serial.doc setparams
  PIOTArray = ^TIOTArray;
  TIOTArray = record
    TermArray0: LongWord;
    TermArray1: LongWord;
  end;

const
  SER_DEFAULT_CTLCHAR = $11130000; // default chars for xON,xOFF

{ You may change these via SETPARAMS.   At this time, parity is not
   calculated for xON/xOFF characters.  You must supply them with the
   desired parity. }

{****************************************************************}
{ CAUTION !!  IF YOU ACCESS the serial.device, you MUST (!!!!) use an
   IOExtSer-sized structure or you may overlay innocent memory !! }
{****************************************************************}

type
  TIOExtSer = record
    IOSer: TIOStdReq;
    io_CtlChar: LongWord;    // control characters
    io_RBufLen: LongWord;    // length in bytes of serial read buffer
    io_ExtFlags: LongWord;   // additional serial flags (SEXTB_*)
    io_Baud: LongWord;       // baud rate
    io_BrkTime: LongWord;    // duration of break in microseconds
    io_TermArray: TIOTArray; // termination character array
    io_ReadLen: Byte;        // number of bits per read character
    io_WriteLen: Byte;       // number of bits per write character
    io_StopBits: Byte;       // number of stopbits for read
    io_SerFlags: Byte;       // serial device flags (SERB_*)
    io_Status: Word;         // status of serial port and lines (IO_STATF_*, IOSTF_*)
  end;
  PIOExtSer = ^TIOExtSer;

 { status of serial port, as follows:
      BIT  ACTIVE  FUNCTION
       0    ---    reserved
       1    ---    reserved
       2    high   Connected to parallel "select" on the A1000.
                   Connected to both the parallel "select" and
                   serial "ring indicator" pins on the A500 &
                   A2000.  Take care when making cables.
       3    low    Data Set Ready
       4    low    Clear To Send
       5    low    Carrier Detect
       6    low    Ready To Send
       7    low    Data Terminal Ready
       8    high   read overrun
       9    high   break sent
      10    high   break received
      11    high   transmit x-OFFed
      12    high   receive x-OFFed
   13-15           reserved}

const
  SDCMD_QUERY     = CMD_NONSTD;
  SDCMD_BREAK     = CMD_NONSTD + 1;
  SDCMD_SETPARAMS = CMD_NONSTD + 2;

  // flags of TIOExtSer.io_SerFlags
  SERB_XDISABLED      = 7;                     // xOn-xOff feature disabled
  SERF_XDISABLED      = 1 shl SERB_XDISABLED;
  SERB_EOFMODE        = 6;                     // EOF mode enabled
  SERF_EOFMODE        = 1 shl SERB_EOFMODE;
  SERB_SHARED         = 5;                     // non-exclusive access
  SERF_SHARED         = 1 shl SERB_SHARED;
  SERB_RAD_BOOGIE     = 4;                     // high-speed mode active
  SERF_RAD_BOOGIE     = 1 shl SERB_RAD_BOOGIE;
  SERB_QUEUEDBRK      = 3;                     // queue this Break ioRqst
  SERF_QUEUEDBRK      = 1 shl SERB_QUEUEDBRK;
  SERB_7WIRE          = 2;                     // RS232 7-wire protocol
  SERF_7WIRE          = 1 shl SERB_7WIRE;
  SERB_PARTY_ODD      = 1;                     // parity feature enabled
  SERF_PARTY_ODD      = 1 shl SERB_PARTY_ODD;
  SERB_PARTY_ON       = 0;                     // parity-enabled
  SERF_PARTY_ON       = 1 shl SERB_PARTY_ON;

// These now refect the actual bit positions in the TIOExtSer.io_Status LongWord
  IO_STATB_XOFFREAD   = 12;                       // receive currently xOFF'ed
  IO_STATF_XOFFREAD   = 1 shl IO_STATB_XOFFREAD;
  IO_STATB_XOFFWRITE  = 11;                       // transmit currently xOFF'ed
  IO_STATF_XOFFWRITE  = 1 shl IO_STATB_XOFFWRITE;
  IO_STATB_READBREAK  = 10;                       // break was latest input
  IO_STATF_READBREAK  = 1 shl IO_STATB_READBREAK;
  IO_STATB_WROTEBREAK = 9;                        // break was latest output
  IO_STATF_WROTEBREAK = 1 shl IO_STATB_WROTEBREAK;
  IO_STATB_OVERRUN    = 8;                        // status word RBF overrun
  IO_STATF_OVERRUN    = 1 shl IO_STATB_OVERRUN;

// TIOExtSer.io_ExtFlags
  SEXTB_MSPON = 1;                 // Use mark-space parity, instead of odd-even.
  SEXTF_MSPON = 1 shl SEXTB_MSPON;
  SEXTB_MARK  = 0;                 // if mark-space, use mark
  SEXTF_MARK  = 1 shl SEXTB_MARK;

  SerErr_DevBusy       = 1;
  SerErr_BaudMismatch  = 2;  // baud rate not supported by hardware
  SerErr_BufErr        = 4;  // Failed to allocate new read buffer
  SerErr_InvParam      = 5;
  SerErr_LineErr       = 6;
  SerErr_ParityErr     = 9;
  SerErr_TimerErr      = 11; // (See the serial/OpenDevice autodoc)
  SerErr_BufOverflow   = 12;
  SerErr_NoDSR         = 13;
  SerErr_DetectedBreak = 15;


{ These defines refer to the HIGH ORDER byte of io_Status.  They have
   been replaced by the new, corrected ones above }
  IOSTB_XOFFREAD  = 4;                      // receive currently xOFF'ed
  IOSTF_XOFFREAD  = 1 shl IOSTB_XOFFREAD;
  IOSTB_XOFFWRITE = 3;                      // transmit currently xOFF'ed
  IOSTF_XOFFWRITE = 1 shl IOSTB_XOFFWRITE;
  IOSTB_READBREAK = 2;                      // break was latest input
  IOSTF_READBREAK = 1 shl IOSTB_READBREAK;
  IOSTB_WROTEBREAK= 1;                      // break was latest output
  IOSTF_WROTEBREAK= 1 shl IOSTB_WROTEBREAK;
  IOSTB_OVERRUN   = 0;                      // status word RBF overrun
  IOSTF_OVERRUN   = 1 shl IOSTB_OVERRUN;

// TIOExtSer.io_Flags
  IOSERB_BUFRREAD = 7;                     // from read buffer
  IOSERF_BUFRREAD = 1 shl IOSERB_BUFRREAD;
  IOSERB_QUEUED   = 6;                     // rqst-queued
  IOSERF_QUEUED   = 1 shl IOSERB_QUEUED;
  IOSERB_ABORT    = 5;                     // rqst-aborted
  IOSERF_ABORT    = 1 shl IOSERB_ABORT;
  IOSERB_ACTIVE   = 4;                     // rqst-qued-OR-current
  IOSERF_ACTIVE   = 1 shl IOSERB_ACTIVE;

  SERIALNAME: PChar = 'serial.device';

implementation



end.
