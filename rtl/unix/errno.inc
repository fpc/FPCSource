{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{

  Errno.inc : define all error numbers, kernel version 1.2.13

}
Const

ESysEPERM       = 1;    { Operation not permitted }
ESysENOENT      = 2;    { No such file or directory }
ESysESRCH       = 3;    { No such process }
ESysEINTR       = 4;    { Interrupted system call }
ESysEIO = 5;    { I/O error }
ESysENXIO       = 6;    { No such device or address }
ESysE2BIG       = 7;    { Arg list too long }
ESysENOEXEC     = 8;    { Exec format error }
ESysEBADF       = 9;    { Bad file number }
ESysECHILD      = 10;   { No child processes }
{$ifdef FreeBSD}
ESysEDEADLK     = 11;   { Deadlock avoided}
{$else}
ESysEAGAIN      = 11;   { Try again }
{$endif}
ESysENOMEM      = 12;   { Out of memory }
ESysEACCES      = 13;   { Permission denied }
ESysEFAULT      = 14;   { Bad address }
ESysENOTBLK     = 15;   { Block device required, NOT POSIX! }
ESysEBUSY       = 16;   { Device or resource busy }
ESysEEXIST      = 17;   { File exists }
ESysEXDEV       = 18;   { Cross-device link }
ESysENODEV      = 19;   { No such device }
ESysENOTDIR     = 20;   { Not a directory }
ESysEISDIR      = 21;   { Is a directory }
ESysEINVAL      = 22;   { Invalid argument }
ESysENFILE      = 23;   { File table overflow }
ESysEMFILE      = 24;   { Too many open files }
ESysENOTTY      = 25;   { Not a typewriter }
ESysETXTBSY     = 26;   { Text file busy }
ESysEFBIG       = 27;   { File too large }
ESysENOSPC      = 28;   { No space left on device }
ESysESPIPE      = 29;   { Illegal seek }
ESysEROFS       = 30;   { Read-only file system }
ESysEMLINK      = 31;   { Too many links }
ESysEPIPE       = 32;   { Broken pipe }
ESysEDOM        = 33;   { Math argument out of domain of func }
ESysERANGE      = 34;   { Math result not representable }
{$ifdef FreeBSD}
{ non-blocking and interrupt i/o }
ESysEAGAIN      = 35;           { Resource temporarily unavailable }

// non posix from here on.

ESysEWOULDBLOCK = ESYSEAGAIN;   { Operation would block }
ESysEINPROGRESS = 36;   { Operation now in progress }
ESysEALREADY    = 37;   { Operation already in progress }

{ ipc/network software -- argument errors }
ESysENOTSOCK    = 38;   { Socket operation on non-socket }
ESysEDESTADDRREQ= 39;   { Destination address required }
ESysEMSGSIZE    = 40;   { Message too long }
ESysEPROTOTYPE  = 41;   { Protocol wrong type for socket }
ESysENOPROTOOPT = 42;   { Protocol not available }
ESysEPROTONOSUPPORT= 43;{ Protocol not supported }
ESysESOCKTNOSUPPORT= 44;{ Socket type not supported }
ESysEOPNOTSUPP  = 45;   { Operation not supported }
ESysENOTSUP     = ESYSEOPNOTSUPP;       { Operation not supported }
ESysEPFNOSUPPORT= 46;   { Protocol family not supported }
ESysEAFNOSUPPORT= 47;   { Address family not supported by protocol family }
ESysEADDRINUSE  = 48;   { Address already in use }
ESysEADDRNOTAVAIL= 49;  { Can't assign requested address }

{ ipc/network software -- operational errors }
ESysENETDOWN    = 50;   { Network is down }
ESysENETUNREACH = 51;   { Network is unreachable }
ESysENETRESET   = 52;   { Network dropped connection on reset }
ESysECONNABORTED= 53;   { Software caused connection abort }
ESysECONNRESET  = 54;   { Connection reset by peer }
ESysENOBUFS     = 55;   { No buffer space available }
ESysEISCONN     = 56;   { Socket is already connected }
ESysENOTCONN    = 57;   { Socket is not connected }
ESysESHUTDOWN   = 58;   { Can't send after socket shutdown }
ESysETOOMANYREFS= 59;   { Too many references: can't splice }
ESysETIMEDOUT   = 60;   { Operation timed out }
ESysECONNREFUSED= 61;   { Connection refused }
ESysELOOP       = 62;   { Too many levels of symbolic links }
ESysENAMETOOLONG= 63;   { File name too long }
ESysEHOSTDOWN   = 64;   { Host is down }
ESysEHOSTUNREACH= 65;   { No route to host }
ESysENOTEMPTY   = 66;   { Directory not empty }
{ quotas & mush }
ESysEPROCLIM    = 67;   { Too many processes }
ESysEUSERS      = 68;   { Too many users }
ESysEDQUOT      = 69;   { Disc quota exceeded }
{ Network File System }
ESysESTALE      = 70;   { Stale NFS file handle }
ESysEREMOTE     = 71;   { Too many levels of remote in path }
ESysEBADRPC     = 72;   { RPC struct is bad }
ESysERPCMISMATCH= 73;   { RPC version wrong }
ESysEPROGUNAVAIL= 74;   { RPC prog. not avail }
ESysEPROGMISMATCH= 75;  { Program version wrong }
ESysEPROCUNAVAIL= 76;   { Bad procedure for program }
ESysENOLCK      = 77;   { No locks available }
ESysENOSYS      = 78;   { Function not implemented }
ESysEFTYPE      = 79;   { Inappropriate file type or format }
ESysEAUTH       = 80;   { Authentication error }
ESysENEEDAUTH   = 81;   { Need authenticator }
ESysEIDRM       = 82;   { Identifier removed }
ESysENOMSG      = 83;   { No message of desired type }
ESysEOVERFLOW   = 84;   { Value too large to be stored in data type }
ESysECANCELED   = 85;   { Operation canceled }
ESysEILSEQ      = 86;   { Illegal byte sequence }
ESysELAST       = 86;   { Must be equal largest errno }

{$else}
ESysEDEADLK     = 35;   { Resource deadlock would occur }
ESysENAMETOOLONG= 36;   { File name too long }
ESysENOLCK      = 37;   { No record locks available }
ESysENOSYS      = 38;   { Function not implemented }
ESysENOTEMPTY= 39;      { Directory not empty }
ESysELOOP       = 40;   { Too many symbolic links encountered }
ESysEWOULDBLOCK = ESysEAGAIN;   { Operation would block }
ESysENOMSG      = 42;   { No message of desired type }
ESysEIDRM       = 43;   { Identifier removed }
ESysECHRNG      = 44;   { Channel number out of range }
ESysEL2NSYNC= 45;       { Level 2 not synchronized }
ESysEL3HLT      = 46;   { Level 3 halted }
ESysEL3RST      = 47;   { Level 3 reset }
ESysELNRNG      = 48;   { Link number out of range }
ESysEUNATCH     = 49;   { Protocol driver not attached }
ESysENOCSI      = 50;   { No CSI structure available }
ESysEL2HLT      = 51;   { Level 2 halted }
ESysEBADE       = 52;   { Invalid exchange }
ESysEBADR       = 53;   { Invalid request descriptor }
ESysEXFULL      = 54;   { Exchange full }
ESysENOANO      = 55;   { No anode }
ESysEBADRQC     = 56;   { Invalid request code }
ESysEBADSLT     = 57;   { Invalid slot }
ESysEDEADLOCK= 58;      { File locking deadlock error }
ESysEBFONT      = 59;   { Bad font file format }
ESysENOSTR      = 60;   { Device not a stream }
ESysENODATA     = 61;   { No data available }
ESysETIME       = 62;   { Timer expired }
ESysENOSR       = 63;   { Out of streams resources }
ESysENONET      = 64;   { Machine is not on the network }
ESysENOPKG      = 65;   { Package not installed }
ESysEREMOTE     = 66;   { Object is remote }
ESysENOLINK     = 67;   { Link has been severed }
ESysEADV        = 68;   { Advertise error }
ESysESRMNT      = 69;   { Srmount error }
ESysECOMM       = 70;   { Communication error on send }
ESysEPROTO      = 71;   { Protocol error }
ESysEMULTIHOP= 72;      { Multihop attempted }
ESysEDOTDOT     = 73;   { RFS specific error }
ESysEBADMSG     = 74;   { Not a data message }
ESysEOVERFLOW= 75;      { Value too large for defined data type }
ESysENOTUNIQ= 76;       { Name not unique on network }
ESysEBADFD      = 77;   { File descriptor in bad state }
ESysEREMCHG     = 78;   { Remote address changed }
ESysELIBACC     = 79;   { Can not access a needed shared library }
ESysELIBBAD     = 80;   { Accessing a corrupted shared library }
ESysELIBSCN     = 81;   { .lib section in a.out corrupted }
ESysELIBMAX     = 82;   { Attempting to link in too many shared libraries }
ESysELIBEXEC= 83;       { Cannot exec a shared library directly }
ESysEILSEQ      = 84;   { Illegal byte sequence }
ESysERESTART= 85;       { Interrupted system call should be restarted }
ESysESTRPIPE= 86;       { Streams pipe error }
ESysEUSERS      = 87;   { Too many users }
ESysENOTSOCK= 88;       { Socket operation on non-socket }
ESysEDESTADDRREQ= 89;   { Destination address required }
ESysEMSGSIZE= 90;       { Message too long }
ESysEPROTOTYPE= 91;     { Protocol wrong type for socket }
ESysENOPROTOOPT= 92;    { Protocol not available }
ESysEPROTONOSUPPORT= 93;        { Protocol not supported }
ESysESOCKTNOSUPPORT= 94;        { Socket type not supported }
ESysEOPNOTSUPP= 95;     { Operation not supported on transport endpoint }
ESysEPFNOSUPPORT= 96;   { Protocol family not supported }
ESysEAFNOSUPPORT= 97;   { Address family not supported by protocol }
ESysEADDRINUSE= 98;     { Address already in use }
ESysEADDRNOTAVAIL= 99;  { Cannot assign requested address }
ESysENETDOWN= 100;      { Network is down }
ESysENETUNREACH= 101;   { Network is unreachable }
ESysENETRESET= 102;     { Network dropped connection because of reset }
ESysECONNABORTED= 103;  { Software caused connection abort }
ESysECONNRESET= 104;    { Connection reset by peer }
ESysENOBUFS     = 105;  { No buffer space available }
ESysEISCONN     = 106;  { Transport endpoint is already connected }
ESysENOTCONN= 107;      { Transport endpoint is not connected }
ESysESHUTDOWN= 108;     { Cannot send after transport endpoint shutdown }
ESysETOOMANYREFS= 109;  { Too many references: cannot splice }
ESysETIMEDOUT= 110;     { Connection timed out }
ESysECONNREFUSED= 111;  { Connection refused }
ESysEHOSTDOWN= 112;     { Host is down }
ESysEHOSTUNREACH= 113;  { No route to host }
ESysEALREADY= 114;      { Operation already in progress }
ESysEINPROGRESS= 115;   { Operation now in progress }
ESysESTALE      = 116;  { Stale NFS file handle }
ESysEUCLEAN     = 117;  { Structure needs cleaning }
ESysENOTNAM     = 118;  { Not a XENIX named type file }
ESysENAVAIL     = 119;  { No XENIX semaphores available }
ESysEISNAM      = 120;  { Is a named type file }
ESysEREMOTEIO= 121;     { Remote I/O error }
ESysEDQUOT      = 122;  { Quota exceeded }
{$endif}

{
  $Log$
  Revision 1.6  2003-01-02 23:57:32  marco
   * small patch

  Revision 1.5  2002/12/18 16:50:39  marco
   * Unix RTL generic parts. Linux working, *BSD will follow shortly

  Revision 1.4  2002/09/07 16:01:27  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/03/11 19:10:33  peter
    * Regenerated with updated fpcmake

}
