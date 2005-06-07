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
  { sparc uses Solaris compatible errnos }
  { comments are stripped here, they can be found in the
    generic section }
  ESysEPERM = 1;
  ESysENOENT = 2;
  ESysESRCH = 3;
  ESysEINTR = 4;
  ESysEIO = 5;
  ESysENXIO = 6;
  ESysE2BIG = 7;
  ESysENOEXEC = 8;
  ESysEBADF = 9;
  ESysECHILD = 10;
  ESysEAGAIN = 11;
  ESysENOMEM = 12;
  ESysEACCES = 13;
  ESysEFAULT = 14;
  ESysENOTBLK = 15;
  ESysEBUSY = 16;
  ESysEEXIST = 17;
  ESysEXDEV = 18;
  ESysENODEV = 19;
  ESysENOTDIR = 20;
  ESysEISDIR = 21;
  ESysEINVAL = 22;
  ESysENFILE = 23;
  ESysEMFILE = 24;
  ESysENOTTY = 25;
  ESysETXTBSY = 26;
  ESysEFBIG = 27;
  ESysENOSPC = 28;
  ESysESPIPE = 29;
  ESysEROFS = 30;
  ESysEMLINK = 31;
  ESysEPIPE = 32;
  ESysEDOM = 33;
  ESysERANGE = 34;
  ESysEWOULDBLOCK = ESysEAGAIN;
  ESysEINPROGRESS = 36;
  ESysEALREADY = 37;
  ESysENOTSOCK = 38;
  ESysEDESTADDRREQ = 39;
  ESysEMSGSIZE = 40;
  ESysEPROTOTYPE = 41;
  ESysENOPROTOOPT = 42;
  ESysEPROTONOSUPPORT = 43;
  ESysESOCKTNOSUPPORT = 44;
  ESysEOPNOTSUPP = 45;
  ESysEPFNOSUPPORT = 46;
  ESysEAFNOSUPPORT = 47;
  ESysEADDRINUSE = 48;
  ESysEADDRNOTAVAIL = 49;
  ESysENETDOWN = 50;
  ESysENETUNREACH = 51;
  ESysENETRESET = 52;
  ESysECONNABORTED = 53;
  ESysECONNRESET = 54;
  ESysENOBUFS = 55;
  ESysEISCONN = 56;
  ESysENOTCONN = 57;
  ESysESHUTDOWN = 58;
  ESysETOOMANYREFS = 59;
  ESysETIMEDOUT = 60;
  ESysECONNREFUSED = 61;
  ESysELOOP = 62;
  ESysENAMETOOLONG = 63;
  ESysEHOSTDOWN = 64;
  ESysEHOSTUNREACH = 65;
  ESysENOTEMPTY = 66;
  ESysEPROCLIM = 67;
  ESysEUSERS = 68;
  ESysEDQUOT = 69;
  ESysESTALE = 70;
  ESysEREMOTE = 71;
  ESysENOSTR = 72;
  ESysETIME = 73;
  ESysENOSR = 74;
  ESysENOMSG = 75;
  ESysEBADMSG = 76;
  ESysEIDRM = 77;
  ESysEDEADLK = 78;
  ESysENOLCK = 79;
  ESysENONET = 80;
  ESysERREMOTE = 81;
  ESysENOLINK = 82;
  ESysEADV = 83;
  ESysESRMNT = 84;
  ESysECOMM = 85;
  ESysEPROTO = 86;
  ESysEMULTIHOP = 87;
  ESysEDOTDOT = 88;
  ESysEREMCHG = 89;
  ESysENOSYS = 90;
  ESysESTRPIPE = 91;
  ESysEOVERFLOW = 92;
  ESysEBADFD = 93;
  ESysECHRNG = 94;
  ESysEL2NSYNC = 95;
  ESysEL3HLT = 96;
  ESysEL3RST = 97;
  ESysELNRNG = 98;
  ESysEUNATCH = 99;
  ESysENOCSI = 100;
  ESysEL2HLT = 101;
  ESysEBADE = 102;
  ESysEBADR = 103;
  ESysEXFULL = 104;
  ESysENOANO = 105;
  ESysEBADRQC = 106;
  ESysEBADSLT = 107;
  ESysEDEADLOCK = 108;
  ESysEBFONT = 109;
  ESysELIBEXEC = 110;
  ESysENODATA = 111;
  ESysELIBBAD = 112;
  ESysENOPKG = 113;
  ESysELIBACC = 114;
  ESysENOTUNIQ = 115;
  ESysERESTART = 116;
  ESysEUCLEAN = 117;
  ESysENOTNAM = 118;
  ESysENAVAIL = 119;
  ESysEISNAM = 120;
  ESysEREMOTEIO = 121;
  ESysEILSEQ = 122;
  ESysELIBMAX = 123;
  ESysELIBSCN = 124;
  ESysENOMEDIUM = 125;
  ESysEMEDIUMTYPE = 126;
{$endif CPUSPARC}

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
{$endif FPC_HAS_ESYS}

