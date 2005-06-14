  {

     Copyright (c) 2000 Apple Computer, Inc. All rights reserved.

     @APPLE_LICENSE_HEADER_START@

     The contents of this file constitute Original Code as defined in and
     are subject to the Apple Public Source License Version 1.1 (the
     "License").  You may not use this file except in compliance with the
     License.  Please obtain a copy of the License at
     http://www.apple.com/publicsource and read it before using this file.

     This Original Code and all software distributed under the License are
     distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
     EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
     INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
     FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
     License for the specific language governing rights and limitations
     under the License.

     @APPLE_LICENSE_HEADER_END@
    }
  { Copyright (c) 1995 NeXT Computer, Inc. All Rights Reserved  }
  {
     Copyright (c) 1982, 1986, 1989, 1993
        The Regents of the University of California.  All rights reserved.
     (c) UNIX System Laboratories, Inc.
     All or some portions of this file are derived from material licensed
     to the University of California by American Telephone and Telegraph
     Co. or Unix System Laboratories, Inc. and are reproduced herein with
     the permission of UNIX System Laboratories, Inc.

     Redistribution and use in source and binary forms, with or without
     modification, are permitted provided that the following conditions
     are met:
     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
     3. All advertising materials mentioning features or use of this software
        must display the following acknowledgement:
        This product includes software developed by the University of
        California, Berkeley and its contributors.
     4. Neither the name of the University nor the names of its contributors
        may be used to endorse or promote products derived from this software
        without specific prior written permission.

     THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
     ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
     IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
     ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
     FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
     OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
     LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
     OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
     SUCH DAMAGE.

        @(#)errno.h     8.5 (Berkeley) 1/21/94
    }

    const
    { Operation not permitted  }
       ESysEPERM = 1;
    { No such file or directory  }
       ESysENOENT = 2;
    { No such process  }
       ESysESRCH = 3;
    { Interrupted system call  }
       ESysEINTR = 4;
    { Input/output error  }
       ESysEIO = 5;
    { Device not configured  }
       ESysENXIO = 6;
    { Argument list too long  }
       ESysE2BIG = 7;
    { Exec format error  }
       ESysENOEXEC = 8;
    { Bad file descriptor  }
       ESysEBADF = 9;
    { No child processes  }
       ESysECHILD = 10;
    { Resource deadlock avoided  }
       ESysEDEADLK = 11;
    { 11 was EAGAIN  }
    { Cannot allocate memory  }
       ESysENOMEM = 12;
    { Permission denied  }
       ESysEACCES = 13;
    { Bad address  }
       ESysEFAULT = 14;
{$ifndef _POSIX_SOURCE}
    { Block device required  }
       ESysENOTBLK = 15;
{$endif}
    { Device busy  }
       ESysEBUSY = 16;
    { File exists  }
       ESysEEXIST = 17;
    { Cross-device link  }
       ESysEXDEV = 18;
    { Operation not supported by device  }
       ESysENODEV = 19;
    { Not a directory  }
       ESysENOTDIR = 20;
    { Is a directory  }
       ESysEISDIR = 21;
    { Invalid argument  }
       ESysEINVAL = 22;
    { Too many open files in system  }
       ESysENFILE = 23;
    { Too many open files  }
       ESysEMFILE = 24;
    { Inappropriate ioctl for device  }
       ESysENOTTY = 25;
{$ifndef _POSIX_SOURCE}
    { Text file busy.  The new process was a pure procedure (shared text) file
          which was open for writing by another process, or file which was open for
          writing by another process, or while the pure procedure file was being
          executed an open(2) call requested write access  requested write access.}
       ESysETXTBSY = 26;
{$endif}
    { File too large  }
       ESysEFBIG = 27;
    { No space left on device  }
       ESysENOSPC = 28;
    { Illegal seek  }
       ESysESPIPE = 29;
    { Read-only file system  }
       ESysEROFS = 30;
    { Too many links  }
       ESysEMLINK = 31;
    { Broken pipe  }
       ESysEPIPE = 32;
    { math software  }
    { Numerical argument out of domain  }
       ESysEDOM = 33;
    { Result too large  }
       ESysERANGE = 34;
    { non-blocking and interrupt i/o  }
    { Resource temporarily unavailable  }
       ESysEAGAIN = 35;
{$ifndef _POSIX_SOURCE}
    { Operation would block  }
       ESysEWOULDBLOCK = ESysEAGAIN;
    { Operation now in progress  }
       ESysEINPROGRESS = 36;
    { Operation already in progress  }
       ESysEALREADY = 37;
    { ipc/network software -- argument errors  }
    { Socket operation on non-socket  }
       ESysENOTSOCK = 38;
    { Destination address required  }
       ESysEDESTADDRREQ = 39;
    { Message too long  }
       ESysEMSGSIZE = 40;
    { Protocol wrong type for socket  }
       ESysEPROTOTYPE = 41;
    { Protocol not available  }
       ESysENOPROTOOPT = 42;
    { Protocol not supported  }
       ESysEPROTONOSUPPORT = 43;
    { Socket type not supported  }
       ESysESOCKTNOSUPPORT = 44;
{$endif}
    { ! _POSIX_SOURCE  }
    { Operation not supported  }
       ESysENOTSUP = 45;
{$ifndef _POSIX_SOURCE}
    { Operation not supported  }
       ESysEOPNOTSUPP = ESysENOTSUP;
    { Protocol family not supported  }
       ESysEPFNOSUPPORT = 46;
    { Address family not supported by protocol family  }
       ESysEAFNOSUPPORT = 47;
    { Address already in use  }
       ESysEADDRINUSE = 48;
    { Can't assign requested address  }
       ESysEADDRNOTAVAIL = 49;
    { ipc/network software -- operational errors  }
    { Network is down  }
       ESysENETDOWN = 50;
    { Network is unreachable  }
       ESysENETUNREACH = 51;
    { Network dropped connection on reset  }
       ESysENETRESET = 52;
    { Software caused connection abort  }
       ESysECONNABORTED = 53;
    { Connection reset by peer  }
       ESysECONNRESET = 54;
    { No buffer space available  }
       ESysENOBUFS = 55;
    { Socket is already connected  }
       ESysEISCONN = 56;
    { Socket is not connected  }
       ESysENOTCONN = 57;
    { Can't send after socket shutdown  }
       ESysESHUTDOWN = 58;
    { Too many references: can't splice  }
       ESysETOOMANYREFS = 59;
    { Operation timed out  }
       ESysETIMEDOUT = 60;
    { Connection refused  }
       ESysECONNREFUSED = 61;
    { Too many levels of symbolic links  }
       ESysELOOP = 62;
{$endif}
    { _POSIX_SOURCE  }
    { File name too long  }
       ESysENAMETOOLONG = 63;
    { should be rearranged  }
{$ifndef _POSIX_SOURCE}
    { Host is down  }
       ESysEHOSTDOWN = 64;
    { No route to host  }
       ESysEHOSTUNREACH = 65;
{$endif}
    { _POSIX_SOURCE  }
    { Directory not empty  }
       ESysENOTEMPTY = 66;
    { quotas & mush  }
{$ifndef _POSIX_SOURCE}
    { Too many processes  }
       ESysEPROCLIM = 67;
    { Too many users  }
       ESysEUSERS = 68;
    { Disc quota exceeded  }
       ESysEDQUOT = 69;
    { Network File System  }
    { Stale NFS file handle  }
       ESysESTALE = 70;
    { Too many levels of remote in path  }
       ESysEREMOTE = 71;
    { RPC struct is bad  }
       ESysEBADRPC = 72;
    { RPC version wrong  }
       ESysERPCMISMATCH = 73;
    { RPC prog. not avail  }
       ESysEPROGUNAVAIL = 74;
    { Program version wrong  }
       ESysEPROGMISMATCH = 75;
    { Bad procedure for program  }
       ESysEPROCUNAVAIL = 76;
{$endif}
    { _POSIX_SOURCE  }
    { No locks available  }
       ESysENOLCK = 77;
    { Function not implemented  }
       ESysENOSYS = 78;
{$ifndef _POSIX_SOURCE}
    { Inappropriate file type or format  }
       ESysEFTYPE = 79;
    { Authentication error  }
       ESysEAUTH = 80;
    { Need authenticator  }
       ESysENEEDAUTH = 81;
{$endif}
    { _POSIX_SOURCE  }
    { Intelligent device errors  }
    { Device power is off  }
       ESysEPWROFF = 82;
    { Device error, e.g. paper out  }
       ESysEDEVERR = 83;
{$ifndef _POSIX_SOURCE}
    { Value too large to be stored in data type  }
       ESysEOVERFLOW = 84;
    { Program loading errors  }
    { Bad executable  }
       ESysEBADEXEC = 85;
    { Bad CPU type in executable  }
       ESysEBADARCH = 86;
    { Shared library version mismatch  }
       ESysESHLIBVERS = 87;
    { Malformed Macho file  }
       ESysEBADMACHO = 88;
    { Must be equal largest errno  }
    	   ESysELAST = 88;
{$endif}
    { _POSIX_SOURCE  }

    { Operation canceled }
       ESysECANCELED = 89;
    { Identifier removed }
       ESysEIDRM = 90;
    { No message of desired type }
       ESysENOMSG = 91;   
    { Illegal byte sequence }
       ESysEILSEQ = 92;
    { Attribute not found }
       ESysENOATTR = 93;
    { Bad message }
       ESysEBADMSG = 94;
    { Reserved }
       ESysEMULTIHOP = 95;
    { No message available on STREAM }
       ESysENODATA = 96;
    { Reserved }
       ESysENOLINK = 97;
    { No STREAM resources }
       ESysENOSR = 98;
    { Not a STREAM }
       ESysENOSTR = 99;
    { Protocol error }
       ESysEPROTO = 100;
    { STREAM ioctl timeout }
       ESysETIME = 101;

