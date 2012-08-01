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

}
Const


        ESysEPERM               = 1;            { Operation not permitted }
        ESysENOENT              = 2;            { No such file or directory }
        ESysESRCH               = 3;            { No such process }
        ESysEINTR               = 4;            { Interrupted system call }
        ESysEIO                 = 5;            { Input/output error }
        ESysENXIO               = 6;            { Device not configured }
        ESysE2BIG               = 7;            { Argument list too long }
        ESysENOEXEC             = 8;            { Exec format error }
        ESysEBADF               = 9;            { Bad file descriptor }
        ESysECHILD              = 10;           { No child processes }
        ESysEDEADLK             = 11;           { Resource deadlock avoided }
                                        { 11 was EAGAIN }
        ESysENOMEM              = 12;           { Cannot allocate memory }
        ESysEACCES              = 13;           { Permission denied }
        ESysEFAULT              = 14;           { Bad address }
        ESysENOTBLK             = 15;           { Block device required }
        ESysEBUSY               = 16;           { Device busy }
        ESysEEXIST              = 17;           { File exists }
        ESysEXDEV               = 18;           { Cross-device link }
        ESysENODEV              = 19;           { Operation not supported by device }
        ESysENOTDIR             = 20;           { Not a directory }
        ESysEISDIR              = 21;           { Is a directory }
        ESysEINVAL              = 22;           { Invalid argument }
        ESysENFILE              = 23;           { Too many open files in system }
        ESysEMFILE              = 24;           { Too many open files }
        ESysENOTTY              = 25;           { Inappropriate ioctl for device }
        ESysETXTBSY             = 26;           { Text file busy. The new process was
                                      a pure procedure (shared text) file which was
                                      open for writing by another process, or file
                                      which was open for writing by another process,
                                      or while the pure procedure file was being
                                      executed an open(2) call requested write access
                                      requested write access.}
        ESysEFBIG               = 27;           { File too large }
        ESysENOSPC              = 28;           { No space left on device }
        ESysESPIPE              = 29;           { Illegal seek }
        ESysEROFS               = 30;           { Read-only file system }
        ESysEMLINK              = 31;           { Too many links }
        ESysEPIPE               = 32;           { Broken pipe }

{ math software }
        ESysEDOM                = 33;           { Numerical argument out of domain }
        ESysERANGE              = 34;           { Result too large }

{ non-blocking and interrupt i/o }
        ESysEAGAIN              = 35;           { Resource temporarily unavailable }
        ESysEWOULDBLOCK         = ESysEAGAIN;   { Operation would block }
        ESysEINPROGRESS         = 36;           { Operation now in progress }
        ESysEALREADY            = 37;           { Operation already in progress }

{ ipc/network software -  - argument errors }
        ESysENOTSOCK            = 38;           { Socket operation on non-socket }
        ESysEDESTADDRREQ        = 39;           { Destination address required }
        ESysEMSGSIZE            = 40;           { Message too long }
        ESysEPROTOTYPE          = 41;           { Protocol wrong type for socket }
        ESysENOPROTOOPT         = 42;           { Protocol not available }
        ESysEPROTONOSUPPORT     = 43;           { Protocol not supported }
        ESysESOCKTNOSUPPORT     = 44;           { Socket type not supported }
        ESysEOPNOTSUPP          = 45;           { Operation not supported }
        ESysEPFNOSUPPORT        = 46;           { Protocol family not supported }
        ESysEAFNOSUPPORT        = 47;           { Address family not supported by protocol family }
        ESysEADDRINUSE          = 48;           { Address already in use }
        ESysEADDRNOTAVAIL       = 49;           { Can't assign requested address }

{ ipc/network software -  - operational errors }
        ESysENETDOWN            = 50;           { Network is down }
        ESysENETUNREACH         = 51;           { Network is unreachable }
        ESysENETRESET           = 52;           { Network dropped connection on reset }
        ESysECONNABORTED        = 53;           { Software caused connection abort }
        ESysECONNRESET          = 54;           { Connection reset by peer }
        ESysENOBUFS             = 55;           { No buffer space available }
        ESysEISCONN             = 56;           { Socket is already connected }
        ESysENOTCONN            = 57;           { Socket is not connected }
        ESysESHUTDOWN           = 58;           { Can't send after socket shutdown }
        ESysETOOMANYREFS        = 59;           { Too many references: can't splice }
        ESysETIMEDOUT           = 60;           { Operation timed out }
        ESysECONNREFUSED        = 61;           { Connection refused }

        ESysELOOP               = 62;           { Too many levels of symbolic links }
        ESysENAMETOOLONG        = 63;           { File name too long }

{ should be rearranged }
        ESysEHOSTDOWN           = 64;           { Host is down }
        ESysEHOSTUNREACH        = 65;           { No route to host }
        ESysENOTEMPTY           = 66;           { Directory not empty }

{ quotas & mush }
        ESysEPROCLIM            = 67;           { Too many processes }
        ESysEUSERS              = 68;           { Too many users }
        ESysEDQUOT              = 69;           { Disc quota exceeded }

{ Network File System }
        ESysESTALE              = 70;           { Stale NFS file handle }
        ESysEREMOTE             = 71;           { Too many levels of remote in path }
        ESysEBADRPC             = 72;           { RPC struct is bad }
        ESysERPCMISMATCH        = 73;           { RPC version wrong }
        ESysEPROGUNAVAIL        = 74;           { RPC prog. not avail }
        ESysEPROGMISMATCH       = 75;           { Program version wrong }
        ESysEPROCUNAVAIL        = 76;           { Bad procedure for program }

        ESysENOLCK              = 77;           { No locks available }
        ESysENOSYS              = 78;           { Function not implemented }

        ESysEFTYPE              = 79;           { Inappropriate file type or format }
        ESysEAUTH               = 80;           { Authentication error }
        ESysENEEDAUTH           = 81;           { Need authenticator }
        ESysEIPSEC              = 82;           { IPsec processing failure }
        ESysENOATTR             = 83;           { Attribute not found }
        ESysEILSEQ              = 84;           { Illegal byte sequence }
        ESysENOMEDIUM           = 85;           { No medium found }
        ESysEMEDIUMTYPE         = 86;           { Wrong Medium Type }
        ESysEOVERFLOW           = 87;           { Value too large to be stored in data type }
        ESysECANCELED           = 88;           { Operation canceled }
        ESysEIDRM               = 89;           { Identifier removed }
        ESysENOMSG              = 90;           { No message of desired type }
        ESysENOTSUP             = 91;           { Not supported }
        ESysELAST               = ESysENOTSUP;  { Must be equal largest errno }

