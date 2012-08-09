{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{

  Errno.inc :
    - define all error numbers, kernel version 1.2.13
    - updated for sparc from kernel version 2.6.8.1

}
const
{$ifdef CPUSPARC}
{$define FPC_HAS_ESYS}
{$i errno-sparc.inc}
{$endif CPUSPARC}

{$ifdef CPUMIPS}
{$define FPC_HAS_ESYS}
{$i errno-mips.inc}
{$endif CPUMIPS}

{$ifndef FPC_HAS_ESYS}
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
  ESysEAGAIN      = 11;   { Try again }
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
  ESysETXTBSY     = 26;   { Text file busy. The new process was
                            a pure procedure (shared text) file which was
                            open for writing by another process, or file
                            which was open for writing by another process,
                            or while the pure procedure file was being
                            executed an open(2) call requested write access
                            requested write access.}
  ESysEFBIG       = 27;   { File too large }
  ESysENOSPC      = 28;   { No space left on device }
  ESysESPIPE      = 29;   { Illegal seek }
  ESysEROFS       = 30;   { Read-only file system }
  ESysEMLINK      = 31;   { Too many links }
  ESysEPIPE       = 32;   { Broken pipe }
  ESysEDOM        = 33;   { Math argument out of domain of func }
  ESysERANGE      = 34;   { Math result not representable }


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
  ESysEDEADLOCK= ESysEDEADLK; { number 58 is missing }
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

{ List of missing system error numbers
  added using check_errno.sh script }
  ESysENOMEDIUM = 123;
  ESysEMEDIUMTYPE = 124;
  ESysECANCELED = 125;
  ESysENOKEY = 126;
  ESysEKEYEXPIRED = 127;
  ESysEKEYREVOKED = 128;
  ESysEKEYREJECTED = 129;
  ESysEOWNERDEAD = 130;
  ESysENOTRECOVERABLE = 131;
  ESysERFKILL = 132;
{$endif FPC_HAS_ESYS}
