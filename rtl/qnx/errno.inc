{
    This file is part of the Free Pascal run time library.

    
    Taken from QNX RTP Include files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  const
     Sys_EOK = 0;   
     Sys_EPERM = 1; 
     Sys_ENOENT = 2;
     Sys_ESRCH = 3; 
     Sys_EINTR = 4; 
     Sys_EIO = 5;
     Sys_ENXIO = 6;
     Sys_E2BIG = 7;
     Sys_ENOEXEC = 8;
     Sys_EBADF = 9;
     Sys_ECHILD = 10;
     Sys_EAGAIN = 11;
     Sys_ENOMEM = 12;
     Sys_EACCES = 13;
     Sys_EFAULT = 14;
     Sys_ENOTBLK = 15;
     Sys_EBUSY = 16;
     Sys_EEXIST = 17;
     Sys_EXDEV = 18;
     Sys_ENODEV = 19;
     Sys_ENOTDIR = 20;
     Sys_EISDIR = 21;
     Sys_EINVAL = 22;
     Sys_ENFILE = 23;
     Sys_EMFILE = 24;
     Sys_ENOTTY = 25;
     Sys_ETXTBSY = 26;
     Sys_EFBIG = 27;
     Sys_ENOSPC = 28;
     Sys_ESPIPE = 29;
     Sys_EROFS = 30;
     Sys_EMLINK = 31;
     Sys_EPIPE = 32;
     Sys_EDOM = 33;
     Sys_ERANGE = 34;
     Sys_ENOMSG = 35;
     Sys_EIDRM = 36;
     Sys_ECHRNG = 37;
     Sys_EL2NSYNC = 38;
     Sys_EL3HLT = 39;
     Sys_EL3RST = 40;
     Sys_ELNRNG = 41;
     Sys_EUNATCH = 42;
     Sys_ENOCSI = 43;
     Sys_EL2HLT = 44;
     Sys_EDEADLK = 45;
     Sys_ENOLCK = 46;
     Sys_ECANCELED = 47;
  { Not supported (1003.1b-1993)              }
     Sys_ENOTSUP = 48;
     Sys_EDQUOT = 49;

     Sys_EBADE = 50;
     Sys_EBADR = 51;
     Sys_EXFULL = 52;
     Sys_ENOANO = 53;
     Sys_EBADRQC = 54;
     Sys_EBADSLT = 55;
     Sys_EDEADLOCK = 56;
     Sys_EBFONT = 57;
     Sys_ENOSTR = 60;
     Sys_ENODATA = 61;
     Sys_ETIME = 62;
     Sys_ENOSR = 63;
     Sys_ENONET = 64;
     Sys_ENOPKG = 65;
     Sys_EREMOTE = 66;
     Sys_ENOLINK = 67;
     Sys_EADV = 68;
     Sys_ESRMNT = 69;
     Sys_ECOMM = 70;
     Sys_EPROTO = 71;
     Sys_EMULTIHOP = 74;
     Sys_EBADMSG = 77;
     Sys_ENAMETOOLONG = 78;
     Sys_EOVERFLOW = 79;
     Sys_ENOTUNIQ = 80;
     Sys_EBADFD = 81;
     Sys_EREMCHG = 82;
     Sys_ELIBACC = 83;
     Sys_ELIBBAD = 84;
     Sys_ELIBSCN = 85;
     Sys_ELIBMAX = 86;
     Sys_ELIBEXEC = 87;
     Sys_EILSEQ = 88;
     Sys_ENOSYS = 89;
     Sys_ELOOP = 90;
     Sys_ERESTART = 91;
     Sys_ESTRPIPE = 92;
     Sys_ENOTEMPTY = 93;
     Sys_EUSERS = 94;
     Sys_EOPNOTSUPP = 103;
     Sys_EFPOS = 110;
     Sys_ESTALE = 122;
     Sys_EWOULDBLOCK = Sys_EAGAIN;
     Sys_EINPROGRESS = 236;
     Sys_EALREADY = Sys_EBUSY;
     Sys_ENOTSOCK = 238;
     Sys_EDESTADDRREQ = 239;
     Sys_EMSGSIZE = 240;
     Sys_EPROTOTYPE = 241;
     Sys_ENOPROTOOPT = 242;
     Sys_EPROTONOSUPPORT = 243;
     Sys_ESOCKTNOSUPPORT = 244;
     Sys_EPFNOSUPPORT = 246;
     Sys_EAFNOSUPPORT = 247;
     Sys_EADDRINUSE = 248;
     Sys_EADDRNOTAVAIL = 249;
     Sys_ENETDOWN = 250;
     Sys_ENETUNREACH = 251;
     Sys_ENETRESET = 252;
     Sys_ECONNABORTED = 253;
     Sys_ECONNRESET = 254;
     Sys_ENOBUFS = 255;
     Sys_EISCONN = 256;
     Sys_ENOTCONN = 257;
     Sys_ESHUTDOWN = 258;
     Sys_ETOOMANYREFS = 259;
     Sys_ETIMEDOUT = 260;
     Sys_ECONNREFUSED = 261;
     Sys_EHOSTDOWN = 264;
     Sys_EHOSTUNREACH = 265;
     Sys_EBADRPC = 272;
     Sys_ERPCMISMATCH = 273;
     Sys_EPROGUNAVAIL = 274;
     Sys_EPROGMISMATCH = 275;
     Sys_EPROCUNAVAIL = 276;
     { --- QNX specific ---  }
  
     Sys_ENOREMOTE = 300;{ Must be done on local machine            }
     Sys_ENONDP    = 301;   { Need an NDP (8087...) to run             }
     Sys_EBADFSYS  = 302;   { Corrupted file system detected           }
     Sys_EMORE     = 309;  { More to do, send message again           }
     Sys_ECTRLTERM = 310;   { Remap to the controlling terminal        }
     Sys_ENOLIC    = 311;  { No license                               }
     Sys_ESRVRFAULT = 312;   { Server fault on msg pass                 }
     Sys_EENDIAN    = 313;   { Endian not supported                     }

