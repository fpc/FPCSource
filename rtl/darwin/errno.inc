  {
     $Id$

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
       Sys_EPERM = 1;
    { No such file or directory  }
       Sys_ENOENT = 2;
    { No such process  }
       Sys_ESRCH = 3;
    { Interrupted system call  }
       Sys_EINTR = 4;
    { Input/output error  }
       Sys_EIO = 5;
    { Device not configured  }
       Sys_ENXIO = 6;
    { Argument list too long  }
       Sys_E2BIG = 7;
    { Exec format error  }
       Sys_ENOEXEC = 8;
    { Bad file descriptor  }
       Sys_EBADF = 9;
    { No child processes  }
       Sys_ECHILD = 10;
    { Resource deadlock avoided  }
       Sys_EDEADLK = 11;
    { 11 was EAGAIN  }
    { Cannot allocate memory  }
       Sys_ENOMEM = 12;
    { Permission denied  }
       Sys_EACCES = 13;
    { Bad address  }
       Sys_EFAULT = 14;
{$ifndef _POSIX_SOURCE}
    { Block device required  }
       Sys_ENOTBLK = 15;
{$endif}
    { Device busy  }
       Sys_EBUSY = 16;
    { File exists  }
       Sys_EEXIST = 17;
    { Cross-device link  }
       Sys_EXDEV = 18;
    { Operation not supported by device  }
       Sys_ENODEV = 19;
    { Not a directory  }
       Sys_ENOTDIR = 20;
    { Is a directory  }
       Sys_EISDIR = 21;
    { Invalid argument  }
       Sys_EINVAL = 22;
    { Too many open files in system  }
       Sys_ENFILE = 23;
    { Too many open files  }
       Sys_EMFILE = 24;
    { Inappropriate ioctl for device  }
       Sys_ENOTTY = 25;
{$ifndef _POSIX_SOURCE}
    { Text file busy  }
       Sys_ETXTBSY = 26;
{$endif}
    { File too large  }
       Sys_EFBIG = 27;
    { No space left on device  }
       Sys_ENOSPC = 28;
    { Illegal seek  }
       Sys_ESPIPE = 29;
    { Read-only file system  }
       Sys_EROFS = 30;
    { Too many links  }
       Sys_EMLINK = 31;
    { Broken pipe  }
       Sys_EPIPE = 32;
    { math software  }
    { Numerical argument out of domain  }
       Sys_EDOM = 33;
    { Result too large  }
       Sys_ERANGE = 34;
    { non-blocking and interrupt i/o  }
    { Resource temporarily unavailable  }
       Sys_EAGAIN = 35;
{$ifndef _POSIX_SOURCE}
    { Operation would block  }
       Sys_EWOULDBLOCK = EAGAIN;
    { Operation now in progress  }
       Sys_EINPROGRESS = 36;
    { Operation already in progress  }
       Sys_EALREADY = 37;
    { ipc/network software -- argument errors  }
    { Socket operation on non-socket  }
       Sys_ENOTSOCK = 38;
    { Destination address required  }
       Sys_EDESTADDRREQ = 39;
    { Message too long  }
       Sys_EMSGSIZE = 40;
    { Protocol wrong type for socket  }
       Sys_EPROTOTYPE = 41;
    { Protocol not available  }
       Sys_ENOPROTOOPT = 42;
    { Protocol not supported  }
       Sys_EPROTONOSUPPORT = 43;
    { Socket type not supported  }
       Sys_ESOCKTNOSUPPORT = 44;
{$endif}
    { ! _POSIX_SOURCE  }
    { Operation not supported  }
       Sys_ENOTSUP = 45;
{$ifndef _POSIX_SOURCE}
    { Operation not supported  }
       Sys_EOPNOTSUPP = ENOTSUP;
    { Protocol family not supported  }
       Sys_EPFNOSUPPORT = 46;
    { Address family not supported by protocol family  }
       Sys_EAFNOSUPPORT = 47;
    { Address already in use  }
       Sys_EADDRINUSE = 48;
    { Can't assign requested address  }
       Sys_EADDRNOTAVAIL = 49;
    { ipc/network software -- operational errors  }
    { Network is down  }
       Sys_ENETDOWN = 50;
    { Network is unreachable  }
       Sys_ENETUNREACH = 51;
    { Network dropped connection on reset  }
       Sys_ENETRESET = 52;
    { Software caused connection abort  }
       Sys_ECONNABORTED = 53;
    { Connection reset by peer  }
       Sys_ECONNRESET = 54;
    { No buffer space available  }
       Sys_ENOBUFS = 55;
    { Socket is already connected  }
       Sys_EISCONN = 56;
    { Socket is not connected  }
       Sys_ENOTCONN = 57;
    { Can't send after socket shutdown  }
       Sys_ESHUTDOWN = 58;
    { Too many references: can't splice  }
       Sys_ETOOMANYREFS = 59;
    { Operation timed out  }
       Sys_ETIMEDOUT = 60;
    { Connection refused  }
       Sys_ECONNREFUSED = 61;
    { Too many levels of symbolic links  }
       Sys_ELOOP = 62;
{$endif}
    { _POSIX_SOURCE  }
    { File name too long  }
       Sys_ENAMETOOLONG = 63;
    { should be rearranged  }
{$ifndef _POSIX_SOURCE}
    { Host is down  }
       Sys_EHOSTDOWN = 64;
    { No route to host  }
       Sys_EHOSTUNREACH = 65;
{$endif}
    { _POSIX_SOURCE  }
    { Directory not empty  }
       Sys_ENOTEMPTY = 66;
    { quotas & mush  }
{$ifndef _POSIX_SOURCE}
    { Too many processes  }
       Sys_EPROCLIM = 67;
    { Too many users  }
       Sys_EUSERS = 68;
    { Disc quota exceeded  }
       Sys_EDQUOT = 69;
    { Network File System  }
    { Stale NFS file handle  }
       Sys_ESTALE = 70;
    { Too many levels of remote in path  }
       Sys_EREMOTE = 71;
    { RPC struct is bad  }
       Sys_EBADRPC = 72;
    { RPC version wrong  }
       Sys_ERPCMISMATCH = 73;
    { RPC prog. not avail  }
       Sys_EPROGUNAVAIL = 74;
    { Program version wrong  }
       Sys_EPROGMISMATCH = 75;
    { Bad procedure for program  }
       Sys_EPROCUNAVAIL = 76;
{$endif}
    { _POSIX_SOURCE  }
    { No locks available  }
       Sys_ENOLCK = 77;
    { Function not implemented  }
       Sys_ENOSYS = 78;
{$ifndef _POSIX_SOURCE}
    { Inappropriate file type or format  }
       Sys_EFTYPE = 79;
    { Authentication error  }
       Sys_EAUTH = 80;
    { Need authenticator  }
       Sys_ENEEDAUTH = 81;
{$endif}
    { _POSIX_SOURCE  }
    { Intelligent device errors  }
    { Device power is off  }
       Sys_EPWROFF = 82;
    { Device error, e.g. paper out  }
       Sys_EDEVERR = 83;
{$ifndef _POSIX_SOURCE}
    { Value too large to be stored in data type  }
       Sys_EOVERFLOW = 84;
    { Program loading errors  }
    { Bad executable  }
       Sys_EBADEXEC = 85;
    { Bad CPU type in executable  }
       Sys_EBADARCH = 86;
    { Shared library version mismatch  }
       Sys_ESHLIBVERS = 87;
    { Malformed Macho file  }
       Sys_EBADMACHO = 88;
    { Must be equal largest errno  }
       Sys_ELAST = 88;
{$endif}
    { _POSIX_SOURCE  }

{
  $Log$
  Revision 1.1  2002-09-08 09:00:17  jonas
    + initial revision

}