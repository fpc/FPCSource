{
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

  With advent of Solaris target specific. From Unix to Linux and FreeBSD
  dirs.
}


{MvdV:

The variable Errno was also moved here, but I undid that. The conventions
  for the FPC Errno and the libc errno differ too much

var
    Errno : longint; external name 'errno';}

// These are Linux ErrNo - NOT TRANSLATED
// Most shoul be equal
// There seem to be some translation, because from 35 upward it is differnt

Const

ESysEPERM       = 1;    { Operation not permitted }
ESysENOENT      = 2;    { No such file or directory }
ESysESRCH       = 3;    { No such process }
ESysEINTR       = 4;    { Interrupted system call }
ESysEIO         = 5;    { I/O error }
ESysENXIO       = 6;    { No such device or address }
ESysE2BIG       = 7;    { Arg list too long }
ESysENOEXEC     = 8;    { Exec format error }
ESysEBADF       = 9;    { Bad file number }
ESysECHILD      = 10;   { No child processes }
ESysEAGAIN      = 11;   { Try again }
ESysENOMEM      = 12;   { Out of memory }
ESysEACCES      = 13;   { Permission denied }
ESysEFAULT      = 14;   { Bad address }
ESysENOTBLK     = 15;   { Block device required }
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

ESysENOMSG      = 35;   { No message of desired type }
ESysEIDRM       = 36;   { Identifier removed }
ESysECHRNG      = 37;   { Channel number out of range }
ESysEL2NSYNC    = 38;   { Level 2 not synchronized }
ESysEL3HLT      = 39;   { Level 3 halted }
ESysEL3RST      = 40;   { Level 3 reset }
ESysELNRNG      = 41;   { Link number out of range }
ESysEUNATCH     = 42;   { Protocol driver not attached }
ESysENOCSI      = 43;   { No CSI structure available }
ESysEL2HLT      = 44;   { Level 2 halted }
ESysEDEADLK     = 45;   { Resource deadlock would occur }
ESysENOLCK      = 46;   { No record locks available }
ESysECANCELED   = 47;   { Operation canceled }
ESysENOTSUP     = 48;   { Operation not supported }
ESysEDQUOT      = 49;   { Quota exceeded }
ESysEBADE       = 50;   { Invalid exchange }
ESysEBADR       = 51;   { Invalid request descriptor }
ESysEXFULL      = 52;   { Exchange full }
ESysENOANO      = 53;   { No anode }
ESysEBADRQC     = 54;   { Invalid request code }
ESysEBADSLT     = 55;   { Invalid slot }
ESysEDEADLOCK   = 56;   { File locking deadlock error }
ESysEBFONT      = 57;   { Bad font file format }
ESysEOWNERDEAD  = 58;
ESysENOTRECOVERABLE = 59;
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
ESysELOCKUNMAPPED = 72;
ESysENOTACTIVE  = 73;
ESysEMULTIHOP   = 74;   { Multihop attempted }
ESysEBADMSG     = 77;   { Not a data message }

ESysENAMETOOLONG= 78;   { File name too long }
ESysEOVERFLOW   = 79;   { Value too large for defined data type }
ESysENOTUNIQ    = 80;   { Name not unique on network }
ESysEBADFD      = 81;   { File descriptor in bad state }
ESysEREMCHG     = 82;   { Remote address changed }
ESysELIBACC     = 83;   { Can not access a needed shared library }
ESysELIBBAD     = 84;   { Accessing a corrupted shared library }
ESysELIBSCN     = 85;   { .lib section in a.out corrupted }
ESysELIBMAX     = 86;   { Attempting to link in too many shared libraries }
ESysELIBEXEC    = 87;   { Cannot exec a shared library directly }
ESysEILSEQ      = 88;   { Illegal byte sequence }
ESysENOSYS      = 89;   { Unsupported file system operation }
ESysELOOP       = 90;   { Too many symbolic links encountered }
ESysERESTART    = 91;   { Interrupted system call should be restarted }
ESysESTRPIPE    = 92;   { Streams pipe error }
ESysENOTEMPTY   = 93;   { Directory not empty }
ESysEUSERS      = 94;   { Too many users }
ESysENOTSOCK    = 95;   { Socket operation on non-socket }
ESysEDESTADDRREQ= 96;   { Destination address required }
ESysEMSGSIZE    = 97;   { Message too long }
ESysEPROTOTYPE  = 98;   { Protocol wrong type for socket }
ESysENOPROTOOPT = 99;   { Protocol not available }
ESysEPROTONOSUPPORT= 120;       { Protocol not supported }
ESysESOCKTNOSUPPORT= 121;       { Socket type not supported }
ESysEOPNOTSUPP  = 122;  { Operation not supported on transport endpoint }
ESysEPFNOSUPPORT= 123;  { Protocol family not supported }
ESysEAFNOSUPPORT= 124;  { Address family not supported by protocol }
ESysEADDRINUSE  = 125;  { Address already in use }
ESysEADDRNOTAVAIL= 126; { Cannot assign requested address }
ESysENETDOWN    = 127;  { Network is down }
ESysENETUNREACH = 128;  { Network is unreachable }
ESysENETRESET   = 129;  { Network dropped connection because of reset }
ESysECONNABORTED= 130;  { Software caused connection abort }
ESysECONNRESET  = 131;  { Connection reset by peer }
ESysENOBUFS     = 132;  { No buffer space available }
ESysEISCONN     = 133;  { Transport endpoint is already connected }
ESysENOTCONN    = 134;  { Transport endpoint is not connected }
ESysESHUTDOWN   = 143;  { Cannot send after transport endpoint shutdown }
ESysETOOMANYREFS= 144;  { Too many references: cannot splice }
ESysETIMEDOUT   = 145;  { Connection timed out }
ESysECONNREFUSED= 146;  { Connection refused }
ESysEHOSTDOWN   = 147;  { Host is down }
ESysEHOSTUNREACH= 148;  { No route to host }
ESysEWOULDBLOCK = ESysEAGAIN;   { Operation would block }
ESysEALREADY    = 149;  { Operation already in progress }
ESysEINPROGRESS = 150;  { Operation now in progress }
ESysESTALE      = 151;  { Stale NFS file handle }


(* Linux Errors not supported in Solaris:
ESysEDOTDOT     = 73;   { RFS specific error }
ESysEUCLEAN     = 117;  { Structure needs cleaning }
ESysENOTNAM     = 118;  { Not a XENIX named type file }
ESysENAVAIL     = 119;  { No XENIX semaphores available }
ESysEISNAM      = 120;  { Is a named type file }
ESysEREMOTEIO   = 121;  { Remote I/O error }
*)

