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

Sys_EPERM       = 1;    { Operation not permitted }
Sys_ENOENT      = 2;    { No such file or directory }
Sys_ESRCH       = 3;    { No such process }
Sys_EINTR       = 4;    { Interrupted system call }
Sys_EIO         = 5;    { I/O error }
Sys_ENXIO       = 6;    { No such device or address }
Sys_E2BIG       = 7;    { Arg list too long }
Sys_ENOEXEC     = 8;    { Exec format error }
Sys_EBADF       = 9;    { Bad file number }
Sys_ECHILD      = 10;   { No child processes }
Sys_EAGAIN      = 11;   { Try again }
Sys_ENOMEM      = 12;   { Out of memory }
Sys_EACCES      = 13;   { Permission denied }
Sys_EFAULT      = 14;   { Bad address }
Sys_ENOTBLK     = 15;   { Block device required }
Sys_EBUSY       = 16;   { Device or resource busy }
Sys_EEXIST      = 17;   { File exists }
Sys_EXDEV       = 18;   { Cross-device link }
Sys_ENODEV      = 19;   { No such device }
Sys_ENOTDIR     = 20;   { Not a directory }
Sys_EISDIR      = 21;   { Is a directory }
Sys_EINVAL      = 22;   { Invalid argument }
Sys_ENFILE      = 23;   { File table overflow }
Sys_EMFILE      = 24;   { Too many open files }
Sys_ENOTTY      = 25;   { Not a typewriter }
Sys_ETXTBSY     = 26;   { Text file busy }
Sys_EFBIG       = 27;   { File too large }
Sys_ENOSPC      = 28;   { No space left on device }
Sys_ESPIPE      = 29;   { Illegal seek }
Sys_EROFS       = 30;   { Read-only file system }
Sys_EMLINK      = 31;   { Too many links }
Sys_EPIPE       = 32;   { Broken pipe }
Sys_EDOM        = 33;   { Math argument out of domain of func }
Sys_ERANGE      = 34;   { Math result not representable }

Sys_ENOMSG      = 35;   { No message of desired type }
Sys_EIDRM       = 36;   { Identifier removed }
Sys_ECHRNG      = 37;   { Channel number out of range }
Sys_EL2NSYNC    = 38;   { Level 2 not synchronized }
Sys_EL3HLT      = 39;   { Level 3 halted }
Sys_EL3RST      = 40;   { Level 3 reset }
Sys_ELNRNG      = 41;   { Link number out of range }
Sys_EUNATCH     = 42;   { Protocol driver not attached }
Sys_ENOCSI      = 43;   { No CSI structure available }
Sys_EL2HLT      = 44;   { Level 2 halted }
Sys_EDEADLK     = 45;   { Resource deadlock would occur }
Sys_ENOLCK      = 46;   { No record locks available }
Sys_ECANCELED   = 47;
Sys_ENOTSUP     = 48;
Sys_EDQUOT      = 49;
Sys_EBADE       = 50;   { Invalid exchange }
Sys_EBADR       = 51;   { Invalid request descriptor }
Sys_EXFULL      = 52;   { Exchange full }
Sys_ENOANO      = 53;   { No anode }
Sys_EBADRQC     = 54;   { Invalid request code }
Sys_EBADSLT     = 55;   { Invalid slot }
Sys_EDEADLOCK   = 56;   { File locking deadlock error }
Sys_EBFONT      = 57;   { Bad font file format }
Sys_EOWNERDEAD  = 58;
Sys_ENOTRECOVERABLE = 59;
Sys_ENOSTR      = 60;   { Device not a stream }
Sys_ENODATA     = 61;   { No data available }
Sys_ETIME       = 62;   { Timer expired }
Sys_ENOSR       = 63;   { Out of streams resources }
Sys_ENONET      = 64;   { Machine is not on the network }
Sys_ENOPKG      = 65;   { Package not installed }
Sys_EREMOTE     = 66;   { Object is remote }
Sys_ENOLINK     = 67;   { Link has been severed }
Sys_EADV        = 68;   { Advertise error }
Sys_ESRMNT      = 69;   { Srmount error }
Sys_ECOMM       = 70;   { Communication error on send }
Sys_EPROTO      = 71;   { Protocol error }
Sys_ELOCKUNMAPPED = 72;
Sys_ENOTACTIVE  = 73;
Sys_EMULTIHOP   = 74;   { Multihop attempted }
Sys_EBADMSG     = 77;   { Not a data message }

Sys_ENAMETOOLONG= 78;
Sys_EOVERFLOW   = 79;   { Value too large for defined data type }
Sys_ENOTUNIQ    = 80;   { Name not unique on network }
Sys_EBADFD      = 81;   { File descriptor in bad state }
Sys_EREMCHG     = 82;   { Remote address changed }
Sys_ELIBACC     = 83;   { Can not access a needed shared library }
Sys_ELIBBAD     = 84;   { Accessing a corrupted shared library }
Sys_ELIBSCN     = 85;   { .lib section in a.out corrupted }
Sys_ELIBMAX     = 86;   { Attempting to link in too many shared libraries }
Sys_ELIBEXEC    = 87;   { Cannot exec a shared library directly }
Sys_EILSEQ      = 88;   { Illegal byte sequence }
Sys_ELOOP       = 90;   { Too many symbolic links encountered }
Sys_ERESTART    = 91;   { Interrupted system call should be restarted }
Sys_ESTRPIPE    = 92;   { Streams pipe error }
Sys_ENOTEMPTY   = 93;   { Directory not empty }
Sys_EUSERS      = 94;   { Too many users }
Sys_ENOTSOCK    = 95;   { Socket operation on non-socket }
Sys_EDESTADDRREQ= 96;   { Destination address required }
Sys_EMSGSIZE    = 97;   { Message too long }
Sys_EPROTOTYPE  = 98;   { Protocol wrong type for socket }
Sys_ENOPROTOOPT = 99;   { Protocol not available }
Sys_EPROTONOSUPPORT= 120;       { Protocol not supported }
Sys_ESOCKTNOSUPPORT= 121;       { Socket type not supported }
Sys_EOPNOTSUPP  = 122;  { Operation not supported on transport endpoint }
Sys_EPFNOSUPPORT= 123;  { Protocol family not supported }
Sys_EAFNOSUPPORT= 124;  { Address family not supported by protocol }
Sys_EADDRINUSE  = 125;  { Address already in use }
Sys_EADDRNOTAVAIL= 126; { Cannot assign requested address }
Sys_ENETDOWN    = 127;  { Network is down }
Sys_ENETUNREACH = 128;  { Network is unreachable }
Sys_ENETRESET   = 129;  { Network dropped connection because of reset }
Sys_ECONNABORTED= 130;  { Software caused connection abort }
Sys_ECONNRESET  = 131;  { Connection reset by peer }
Sys_ENOBUFS     = 132;  { No buffer space available }
Sys_EISCONN     = 133;  { Transport endpoint is already connected }
Sys_ENOTCONN    = 134;  { Transport endpoint is not connected }
Sys_ESHUTDOWN   = 143;  { Cannot send after transport endpoint shutdown }
Sys_ETOOMANYREFS= 144;  { Too many references: cannot splice }
Sys_ETIMEDOUT   = 145;  { Connection timed out }
Sys_ECONNREFUSED= 146;  { Connection refused }
Sys_EHOSTDOWN   = 147;  { Host is down }
Sys_EHOSTUNREACH= 148;  { No route to host }
Sys_EWOULDBLOCK = Sys_EAGAIN;   { Operation would block }
Sys_EALREADY    = 149;  { Operation already in progress }
Sys_EINPROGRESS = 150;  { Operation now in progress }
Sys_ESTALE      = 151;  { Stale NFS file handle }


(* Linux Errors not supported in Solaris:
Sys_ENAMETOOLONG= 36;   { File name too long }
Sys_ENOSYS      = 38;   { Function not implemented }
Sys_EDOTDOT     = 73;   { RFS specific error }
Sys_EUCLEAN     = 117;  { Structure needs cleaning }
Sys_ENOTNAM     = 118;  { Not a XENIX named type file }
Sys_ENAVAIL     = 119;  { No XENIX semaphores available }
Sys_EISNAM      = 120;  { Is a named type file }
Sys_EREMOTEIO   = 121;  { Remote I/O error }
Sys_EDQUOT      = 122;  { Quota exceeded }
*)

{
  $Log$
  Revision 1.2  2002-09-07 16:01:26  peter
    * old logs removed and tabs fixed

}
