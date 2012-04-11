{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Errno.inc : define all error numbers, AIX 5.3
}


Const

  ESysEPERM             = 1; { Operation not permitted }
  ESysENOENT            = 2; { No such file or directory }
  ESysESRCH             = 3; { No such process }
  ESysEINTR             = 4; { interrupted system call }
  ESysEIO               = 5; { I/O error }
  ESysENXIO             = 6; { No such device or address }
  ESysE2BIG             = 7; { Arg list too long }
  ESysENOEXEC           = 8; { Exec format error }
  ESysEBADF             = 9; { Bad file descriptor }
  ESysECHILD            = 10; { No child processes }
  ESysEAGAIN            = 11; { Resource temporarily unavailable }
  ESysENOMEM            = 12; { Not enough space }
  ESysEACCES            = 13; { Permission denied }
  ESysEFAULT            = 14; { Bad address }
  ESysENOTBLK           = 15; { Block device required }
  ESysEBUSY             = 16; { Resource busy }
  ESysEEXIST            = 17; { File exists }
  ESysEXDEV             = 18; { Improper link }
  ESysENODEV            = 19; { No such device }
  ESysENOTDIR           = 20; { Not a directory }
  ESysEISDIR            = 21; { Is a directory }
  ESysEINVAL            = 22; { Invalid argument }
  ESysENFILE            = 23; { Too many open files in system }
  ESysEMFILE            = 24; { Too many open files }
  ESysENOTTY            = 25; { Inappropriate I/O control operation }
  ESysETXTBSY           = 26; { Text file busy }
  ESysEFBIG             = 27; { File too large }
  ESysENOSPC            = 28; { No space left on device }
  ESysESPIPE            = 29; { Invalid seek }
  ESysEROFS             = 30; { Read only file system }
  ESysEMLINK            = 31; { Too many links }
  ESysEPIPE             = 32; { Broken pipe }
  ESysEDOM              = 33; { Domain error within math function }
  ESysERANGE            = 34; { Result too large }
  ESysENOMSG            = 35; { No message of desired type }
  ESysEIDRM             = 36; { Identifier removed }
  ESysECHRNG            = 37; { Channel number out of range }
  ESysEL2NSYNC          = 38; { Level 2 not synchronized }
  ESysEL3HLT            = 39; { Level 3 halted }
  ESysEL3RST            = 40; { Level 3 reset }
  ESysELNRNG            = 41; { Link number out of range }
  ESysEUNATCH           = 42; { Protocol driver not attached }
  ESysENOCSI            = 43; { No CSI structure available }
  ESysEL2HLT            = 44; { Level 2 halted }
  ESysEDEADLK           = 45; { Resource deadlock avoided }

  ESysENOTREADY         = 46; { Device not ready }
  ESysEWRPROTECT        = 47; { Write-protected media }
  ESysEFORMAT           = 48; { Unformatted media }

  ESysENOLCK            = 49; { No locks available }

  ESysENOCONNECT            = 50; { no connection }
  ESysESTALE            = 52; { no filesystem }
  ESysEDIST             = 53; { old, currently unused AIX errno }

  (* non-blocking and interrupt i/o *)
  (*
   * AIX returns EAGAIN where 4.3BSD used EWOULDBLOCK;
   * but, the standards insist on unique errno values for each errno.
   * A unique value is reserved for users that want to code case
   * statements for systems that return either EAGAIN or EWOULDBLOCK.
   *)
  ESysEWOULDBLOCK = ESysEAGAIN; { Operation would block }

  ESysEINPROGRESS   = 55; { Operation now in progress }
  ESysEALREADY      = 56; { Operation already in progress }

  (* ipc/network software *)

    (* argument errors *)
  ESysENOTSOCK          = 57; { Socket operation on non-socket }
  ESysEDESTADDRREQ      = 58; { Destination address required }
  ESysEDESTADDREQ       = ESysEDESTADDRREQ; { Destination address required }
  ESysEMSGSIZE          = 59; { Message too long }
  ESysEPROTOTYPE        = 60; { Protocol wrong type for socket }
  ESysENOPROTOOPT       = 61; { Protocol not available }
  ESysEPROTONOSUPPORT   = 62; { Protocol not supported }
  ESysESOCKTNOSUPPORT   = 63; { Socket type not supported }
  ESysEOPNOTSUPP        = 64; { Operation not supported on socket }
  ESysEPFNOSUPPORT      = 65; { Protocol family not supported }
  ESysEAFNOSUPPORT      = 66; { Address family not supported by protocol family }
  ESysEADDRINUSE        = 67; { Address already in use }
  ESysEADDRNOTAVAIL     = 68; { Can't assign requested address }

    (* operational errors *)
  ESysENETDOWN          = 69; { Network is down }
  ESysENETUNREACH       = 70; { Network is unreachable }
  ESysENETRESET         = 71; { Network dropped connection on reset }
  ESysECONNABORTED      = 72; { Software caused connection abort }
  ESysECONNRESET        = 73; { Connection reset by peer }
  ESysENOBUFS           = 74; { No buffer space available }
  ESysEISCONN           = 75; { Socket is already connected }
  ESysENOTCONN          = 76; { Socket is not connected }
  ESysESHUTDOWN         = 77; { Can't send after socket shutdown }

  ESysETIMEDOUT         = 78; { Connection timed out }
  ESysECONNREFUSED      = 79; { Connection refused }

  ESysEHOSTDOWN         = 80; { Host is down }
  ESysEHOSTUNREACH      = 81; { No route to host }

  (* ERESTART is used to determine if the system call is restartable *)
  ESysERESTART          = 82; { restart the system call }

  (* quotas and limits *)
  ESysEPROCLIM          = 83; { Too many processes }
  ESysEUSERS            = 84; { Too many users }
  ESysELOOP             = 85; { Too many levels of symbolic links }
  ESysENAMETOOLONG      = 86; { File name too long }

  (*
   * AIX returns EEXIST where 4.3BSD used ENOTEMPTY;
   * but, the standards insist on unique errno values for each errno.
   * A unique value is reserved for users that want to code case
   * statements for systems that return either EEXIST or ENOTEMPTY.
   *)
  ESysENOTEMPTY         = ESysEEXIST; { Directory not empty }

  (* disk quotas *)
  ESysEDQUOT            = 88; { Disc quota exceeded }

  ESysECORRUPT          = 89; { Invalid file system control data }

  (* errnos 90-92 reserved for future use compatible with AIX PS/2 *)

  (* network file system *)
  ESysEREMOTE           = 93; { Item is not local to host }

  (* errnos 94-108 reserved for future use compatible with AIX PS/2 *)

  ESysENOSYS            = 109; { Function not implemented  POSIX }

  (* disk device driver *)
  ESysEMEDIA            = 110; { media surface error }
  ESysESOFT             = 111; { I/O completed, but needs relocation }

  (* security *)
  ESysENOATTR           = 112; { no attribute found }
  ESysESAD              = 113; { security authentication denied }
  ESysENOTRUST          = 114; { not a trusted program }

  (* BSD 4.3 RENO *)
  ESysETOOMANYREFS      = 115; { Too many references: can't splice }

  ESysEILSEQ            = 116; { Invalid wide character }
  ESysECANCELED         = 117; { asynchronous i/o cancelled }

  (* SVR4 STREAMS *)
  ESysENOSR             = 118; { temp out of streams resources }
  ESysETIME             = 119; { I_STR ioctl timed out }
  ESysEBADMSG           = 120; { wrong message type at stream head }
  ESysEPROTO            = 121; { STREAMS protocol error }
  ESysENODATA           = 122; { no message ready at stream head }
  ESysENOSTR            = 123; { fd is not a stream }

  ESysECLONEME          = ESysERESTART; { this is the way we clone a stream ... }

  ESysENOTSUP           = 124; { POSIX threads unsupported value }

  ESysEMULTIHOP         = 125; { multihop is not allowed }
  ESysENOLINK           = 126; { the link has been severed }
  ESysEOVERFLOW         = 127; { value too large to be stored in data type }


