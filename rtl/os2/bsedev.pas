{****************************************************************************

                   Copyright (c) 1999-2000 by Florian Kl„mpfl
                  
 ****************************************************************************}
unit bsedev;

  interface
  
type
(* The following line is only due to problems with cardinal *)
(* arithmetics and should be removed as soon as possible.   *)
 cardinal = longint;
 PCardinal = ^cardinal;
 PByte = ^byte;

    const
       IOCTL_ASYNC = $0001;
       IOCTL_SCR_AND_PTRDRAW = $0003;
       IOCTL_KEYBOARD = $0004;
       IOCTL_PRINTER = $0005;
       IOCTL_LIGHTPEN = $0006;
       IOCTL_POINTINGDEVICE = $0007;
       IOCTL_DISK = $0008;
       IOCTL_PHYSICALDISK = $0009;
       IOCTL_MONITOR = $000A;
       IOCTL_GENERAL = $000B;
       ASYNC_SETBAUDRATE = $0041;
       ASYNC_SETLINECTRL = $0042;
       ASYNC_EXTSETBAUDRATE = $0043;
       ASYNC_SETEXTBAUDRATE = $0043;
       ASYNC_TRANSMITIMM = $0044;
       ASYNC_SETBREAKOFF = $0045;
       ASYNC_SETMODEMCTRL = $0046;
       ASYNC_SETBREAKON = $004B;
       ASYNC_STOPTRANSMIT = $0047;
       ASYNC_STARTTRANSMIT = $0048;
       ASYNC_SETDCBINFO = $0053;
       ASYNC_GETBAUDRATE = $0061;
       ASYNC_GETLINECTRL = $0062;
       ASYNC_EXTGETBAUDRATE = $0063;
       ASYNC_GETEXTBAUDRATE = $0063;
       ASYNC_GETCOMMSTATUS = $0064;
       ASYNC_GETLINESTATUS = $0065;
       ASYNC_GETMODEMOUTPUT = $0066;
       ASYNC_GETMODEMINPUT = $0067;
       ASYNC_GETINQUECOUNT = $0068;
       ASYNC_GETOUTQUECOUNT = $0069;
       ASYNC_GETCOMMERROR = $006D;
       ASYNC_GETCOMMEVENT = $0072;
       ASYNC_GETDCBINFO = $0073;
       SCR_ALLOCLDT = $0070;
       SCR_DEALLOCLDT = $0071;
       PTR_GETPTRDRAWADDRESS = $0072;
       SCR_ALLOCLDTOFF = $0075;
       KBD_SETTRANSTABLE = $0050;
       KBD_SETINPUTMODE = $0051;
       KBD_SETINTERIMFLAG = $0052;
       KBD_SETSHIFTSTATE = $0053;
       KBD_SETTYPAMATICRATE = $0054;
       KBD_SETFGNDSCREENGRP = $0055;
       KBD_SETSESMGRHOTKEY = $0056;
       KBD_SETFOCUS = $0057;
       KBD_SETKCB = $0058;
       KBD_SETNLS = $005C;
       KBD_CREATE = $005D;
       KBD_DESTROY = $005E;
       KBD_GETINPUTMODE = $0071;
       KBD_GETINTERIMFLAG = $0072;
       KBD_GETSHIFTSTATE = $0073;
       KBD_READCHAR = $0074;
       KBD_PEEKCHAR = $0075;
       KBD_GETSESMGRHOTKEY = $0076;
       KBD_GETKEYBDTYPE = $0077;
       KBD_GETCODEPAGEID = $0078;
       KBD_XLATESCAN = $0079;
       PRT_QUERYJOBHANDLE = $0021;
       PRT_SETFRAMECTL = $0042;
       PRT_SETINFINITERETRY = $0044;
       PRT_INITPRINTER = $0046;
       PRT_ACTIVATEFONT = $0048;
       PRT_GETFRAMECTL = $0062;
       PRT_GETINFINITERETRY = $0064;
       PRT_GETPRINTERSTATUS = $0066;
       PRT_QUERYACTIVEFONT = $0069;
       PRT_VERIFYFONT = $006A;
       MOU_ALLOWPTRDRAW = $0050;
       MOU_UPDATEDISPLAYMODE = $0051;
       MOU_SCREENSWITCH = $0052;
       MOU_SETSCALEFACTORS = $0053;
       MOU_SETEVENTMASK = $0054;
       MOU_SETHOTKEYBUTTON = $0055;
       MOU_SETPTRSHAPE = $0056;
       MOU_DRAWPTR = $0057;
       MOU_REMOVEPTR = $0058;
       MOU_SETPTRPOS = $0059;
       MOU_SETPROTDRAWADDRESS = $005A;
       MOU_SETREALDRAWADDRESS = $005B;
       MOU_SETMOUSTATUS = $005C;
       MOU_DISPLAYMODECHANGE = $005D;
       MOU_GETBUTTONCOUNT = $0060;
       MOU_GETMICKEYCOUNT = $0061;
       MOU_GETMOUSTATUS = $0062;
       MOU_READEVENTQUE = $0063;
       MOU_GETQUESTATUS = $0064;
       MOU_GETEVENTMASK = $0065;
       MOU_GETSCALEFACTORS = $0066;
       MOU_GETPTRPOS = $0067;
       MOU_GETPTRSHAPE = $0068;
       MOU_GETHOTKEYBUTTON = $0069;
       MOU_VER = $006A;
       DSK_LOCKDRIVE = $0000;
       DSK_UNLOCKDRIVE = $0001;
       DSK_REDETERMINEMEDIA = $0002;
       DSK_SETLOGICALMAP = $0003;
       DSK_BLOCKREMOVABLE = $0020;
       DSK_GETLOGICALMAP = $0021;
       DSK_SETDEVICEPARAMS = $0043;
       DSK_WRITETRACK = $0044;
       DSK_FORMATVERIFY = $0045;
       DSK_GETDEVICEPARAMS = $0063;
       DSK_READTRACK = $0064;
       DSK_VERIFYTRACK = $0065;
       PDSK_LOCKPHYSDRIVE = $0000;
       PDSK_UNLOCKPHYSDRIVE = $0001;
       PDSK_WRITEPHYSTRACK = $0044;
       PDSK_GETPHYSDEVICEPARAMS = $0063;
       PDSK_READPHYSTRACK = $0064;
       PDSK_VERIFYPHYSTRACK = $0065;
       MON_REGISTERMONITOR = $0040;
       DEV_FLUSHINPUT = $0001;
       DEV_FLUSHOUTPUT = $0002;
       DEV_QUERYMONSUPPORT = $0060;
       RX_QUE_OVERRUN = $0001;
       RX_HARDWARE_OVERRUN = $0002;
       PARITY_ERROR = $0004;
       FRAMING_ERROR = $0008;
       CHAR_RECEIVED = $0001;
       LAST_CHAR_SENT = $0004;
       CTS_CHANGED = $0008;
       DSR_CHANGED = $0010;
       DCD_CHANGED = $0020;
       BREAK_DETECTED = $0040;
       ERROR_OCCURRED = $0080;
       RI_DETECTED = $0100;
       TX_WAITING_FOR_CTS = $0001;
       TX_WAITING_FOR_DSR = $0002;
       TX_WAITING_FOR_DCD = $0004;
       TX_WAITING_FOR_XON = $0008;
       TX_WAITING_TO_SEND_XON = $0010;
       TX_WAITING_WHILE_BREAK_ON = $0020;
       TX_WAITING_TO_SEND_IMM = $0040;
       RX_WAITING_FOR_DSR = $0080;
       WRITE_REQUEST_QUEUED = $0001;
       DATA_IN_TX_QUE = $0002;
       HARDWARE_TRANSMITTING = $0004;
       CHAR_READY_TO_SEND_IMM = $0008;
       WAITING_TO_SEND_XON = $0010;
       WAITING_TO_SEND_XOFF = $0020;
       CTS_ON = $10;
       DSR_ON = $20;
       RI_ON = $40;
       DCD_ON = $80;
       BUILD_BPB_FROM_MEDIUM = $00;
       REPLACE_BPB_FOR_DEVICE = $01;
       REPLACE_BPB_FOR_MEDIUM = $02;
       ASCII_MODE = $00;
       BINARY_MODE = $80;
       CONVERSION_REQUEST = $20;
       INTERIM_CHAR = $80;
       HOTKEY_MAX_COUNT = $0000;
       HOTKEY_CURRENT_COUNT = $0001;
       KBD_DATA_RECEIVED = $0001;
       KBD_DATA_BINARY = $8000;
       KBD_READ_WAIT = $0000;
       KBD_READ_NOWAIT = $8000;
       SHIFT_REPORT_MODE = $01;
       MOUSE_MOTION = $0001;
       MOUSE_MOTION_WITH_BN1_DOWN = $0002;
       MOUSE_BN1_DOWN = $0004;
       MOUSE_MOTION_WITH_BN2_DOWN = $0008;
       MOUSE_BN2_DOWN = $0010;
       MOUSE_MOTION_WITH_BN3_DOWN = $0020;
       MOUSE_BN3_DOWN = $0040;
       MHK_BUTTON1 = $0001;
       MHK_BUTTON2 = $0002;
       MHK_BUTTON3 = $0004;
       MOU_NOWAIT = $0000;
       MOU_WAIT = $0001;
       MHK_NO_HOTKEY = $0000;
       MOUSE_QUEUEBUSY = $0001;
       MOUSE_BLOCKREAD = $0002;
       MOUSE_FLUSH = $0004;
       MOUSE_UNSUPPORTED_MODE = $0008;
       MOUSE_DISABLED = $0100;
       MOUSE_MICKEYS = $0200;
       PRINTER_TIMEOUT = $0001;
       PRINTER_IO_ERROR = $0008;
       PRINTER_SELECTED = $0010;
       PRINTER_OUT_OF_PAPER = $0020;
       PRINTER_ACKNOWLEDGED = $0040;
       PRINTER_NOT_BUSY = $0080;
       MODE_DTR_CONTROL = $01;
       MODE_DTR_HANDSHAKE = $02;
       MODE_CTS_HANDSHAKE = $08;
       MODE_DSR_HANDSHAKE = $10;
       MODE_DCD_HANDSHAKE = $20;
       MODE_DSR_SENSITIVITY = $40;
       MODE_AUTO_TRANSMIT = $01;
       MODE_AUTO_RECEIVE = $02;
       MODE_ERROR_CHAR = $04;
       MODE_NULL_STRIPPING = $08;
       MODE_BREAK_CHAR = $10;
       MODE_RTS_CONTROL = $40;
       MODE_RTS_HANDSHAKE = $80;
       MODE_TRANSMIT_TOGGLE = $C0;
       MODE_NO_WRITE_TIMEOUT = $01;
       MODE_READ_TIMEOUT = $02;
       MODE_WAIT_READ_TIMEOUT = $04;
       MODE_NOWAIT_READ_TIMEOUT = $06;

    type
       DCBINFO = record
          usWriteTimeout : word;
          usReadTimeout : word;
          fbCtlHndShake : BYTE;
          fbFlowReplace : BYTE;
          fbTimeout : BYTE;
          bErrorReplacementChar : BYTE;
          bBreakReplacementChar : BYTE;
          bXONChar : BYTE;
          bXOFFChar : BYTE;
       end;

       PDCBINFO = ^DCBINFO;

{$PACKRECORDS 1}

    const
       DEVTYPE_48TPI = $0000;
       DEVTYPE_96TPI = $0001;
       DEVTYPE_35 = $0002;
       DEVTYPE_8SD = $0003;
       DEVTYPE_8DD = $0004;
       DEVTYPE_FIXED = $0005;
       DEVTYPE_TAPE = $0006;
       DEVTYPE_UNKNOWN = $0007;
{$PACKRECORDS 1}


    type
       BIOSPARAMETERBLOCK = record
          usBytesPerSector : word;
          bSectorsPerCluster : BYTE;
          usReservedSectors : word;
          cFATs : BYTE;
          cRootEntries : word;
          cSectors : word;
          bMedia : BYTE;
          usSectorsPerFAT : word;
          usSectorsPerTrack : word;
          cHeads : word;
          cHiddenSectors : cardinal;
          cLargeSectors : cardinal;
          abReserved : array[0..6-1] of BYTE;
          cCylinders : word;
          bDeviceType : BYTE;
          fsDeviceAttr : word;
       end;

       PBIOSPARAMETERBLOCK = ^BIOSPARAMETERBLOCK;

       SCREENGROUP = record
          idScreenGrp : word;
          fTerminate : word;
       end;

       PSCREENGROUP = ^SCREENGROUP;

       FRAME = record
          bCharsPerLine : BYTE;
          bLinesPerInch : BYTE;
       end;

       PFRAME = ^FRAME;

       KBDTYPE = record
          usType : word;
          reserved1 : word;
          reserved2 : word;
       end;

       PKBDTYPE = ^KBDTYPE;

       LINECONTROL = record
          bDataBits : BYTE;
          bParity : BYTE;
          bStopBits : BYTE;
          fTransBreak : BYTE;
       end;

       PLINECONTROL = ^LINECONTROL;

    const
       DTR_ON = $01;
       RTS_ON = $02;
       DTR_OFF = $FE;
       RTS_OFF = $FD;

    type
       MODEMSTATUS = record
          fbModemOn : BYTE;
          fbModemOff : BYTE;
       end;

       PMODEMSTATUS = ^MODEMSTATUS;

{$PACKRECORDS 1}

       RXQUEUE = record
          cch : word;
          cb : word;
       end;

       PRXQUEUE = ^RXQUEUE;

       DEVICEPARAMETERBLOCK = record
          reserved1 : word;
          cCylinders : word;
          cHeads : word;
          cSectorsPerTrack : word;
          reserved2 : word;
          reserved3 : word;
          reserved4 : word;
          reserved5 : word;
       end;

       PDEVICEPARAMETERBLOCK = ^DEVICEPARAMETERBLOCK;

{$PACKRECORDS 2}

       PTRDRAWFUNCTION = record
          usReturnCode : word;
          pfnDraw : pointer;
          {!!!!!!!! pfnDraw : PFN; }
          pchDataSeg : pointer;
       end;

       PPTRDRAWFUNCTION = ^PTRDRAWFUNCTION;

       PTRDRAWADDRESS = record
          reserved : word;
          ptrdfnc : PTRDRAWFUNCTION;
       end;

       PPTRDRAWADDRESS = ^PTRDRAWADDRESS;

       SHIFTSTATE = record
          fsState : word;
          fNLS : BYTE;
       end;

       PSHIFTSTATE = ^SHIFTSTATE;

    const
       RIGHTSHIFT = $0001;
       LEFTSHIFT = $0002;
       CONTROL = $0004;
       ALT = $0008;
       SCROLLLOCK_ON = $0010;
       NUMLOCK_ON = $0020;
       CAPSLOCK_ON = $0040;
       INSERT_ON = $0080;
       LEFTCONTROL = $0100;
       LEFTALT = $0200;
       RIGHTCONTROL = $0400;
       RIGHTALT = $0800;
       SCROLLLOCK = $1000;
       NUMLOCK = $2000;
       CAPSLOCK = $4000;
       SYSREQ = $8000;

    type
       HOTKEY = record
          fsHotKey : word;
          uchScancodeMake : byte;
          uchScancodeBreak : byte;
          idHotKey : word;
       end;

       PHOTKEY = ^HOTKEY;

       MONITORPOSITION = record
          fPosition : word;
          index : word;
          pbInBuf : cardinal;
          offOutBuf : word;
       end;

       PMONITORPOSITION = ^MONITORPOSITION;

       RATEDELAY = record
          usDelay : word;
          usRate : word;
       end;

       PRATEDELAY = ^RATEDELAY;

       CODEPAGEINFO = record
          pbTransTable : PBYTE;
          idCodePage : word;
          idTable : word;
       end;

       PCODEPAGEINFO = ^CODEPAGEINFO;

       CPID = record
          idCodePage : word;
          Reserved : word;
       end;

       PCPID = ^CPID;

       LDTADDRINFO = record
          pulPhysAddr : PCardinal;
          cb : word;
       end;

       PLDTADDRINFO = ^LDTADDRINFO;

       PTRDRAWDATA = record
          cb : word;
          usConfig : word;
          usFlag : word;
       end;

{$PACKRECORDS NORMAL}
       PPTRDRAWDATA = ^PTRDRAWDATA;


  implementation

end.
