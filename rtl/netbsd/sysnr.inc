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

const
{$ifdef FPC_ABI_EABI}
    syscall_nr_base = $a00000;
{$else FPC_ABI_EABI}
    syscall_nr_base = 0;
{$endif FPC_ABI_EABI}

const

  {
  Converted from NetBSD CVS tree:
  syscall.h,v 1.120 2001/07/01 18:07:33
  }

     syscall_nr_syscall = syscall_nr_base + 0;
  { syscall: "exit" ret: "void" args: "int"  }
     syscall_nr_exit = syscall_nr_base + 1;
  { syscall: "fork" ret: "int" args:  }
     syscall_nr_fork = syscall_nr_base + 2;
  { syscall: "read" ret: "ssize_t" args: "int" "void  " "size_t"  }
     syscall_nr_read = syscall_nr_base + 3;
  { syscall: "write" ret: "ssize_t" args: "int" "const void  " "size_t"  }
     syscall_nr_write = syscall_nr_base + 4;
  { syscall: "open" ret: "int" args: "const char  " "int" "..."  }
     syscall_nr_open = syscall_nr_base + 5;
  { syscall: "close" ret: "int" args: "int"  }
     syscall_nr_close = syscall_nr_base + 6;
  { syscall: "wait4" ret: "int" args: "int" "int  " "int" "struct rusage  "  }
     syscall_nr_wait4 = syscall_nr_base + 7;
     syscall_nr_compat_43_ocreat = syscall_nr_base + 8;
  { syscall: "link" ret: "int" args: "const char  " "const char  "  }
     syscall_nr_link = syscall_nr_base + 9;
  { syscall: "unlink" ret: "int" args: "const char  "  }
     syscall_nr_unlink = syscall_nr_base + 10;
  { 11 is obsolete execv  }
  { syscall: "chdir" ret: "int" args: "const char  "  }
     syscall_nr_chdir = syscall_nr_base + 12;
  { syscall: "fchdir" ret: "int" args: "int"  }
     syscall_nr_fchdir = syscall_nr_base + 13;
  { syscall: "mknod" ret: "int" args: "const char  " "mode_t" "dev_t"  }
     syscall_nr_mknod = syscall_nr_base + 14;
  { syscall: "chmod" ret: "int" args: "const char  " "mode_t"  }
     syscall_nr_chmod = syscall_nr_base + 15;
  { syscall: "chown" ret: "int" args: "const char  " "uid_t" "gid_t"  }
     syscall_nr_chown = syscall_nr_base + 16;
  { syscall: "break" ret: "int" args: "char  "  }
     syscall_nr_break = syscall_nr_base + 17;
  { syscall: "getfsstat" ret: "int" args: "struct statfs  " "long" "int"  }
     syscall_nr_getfsstat = syscall_nr_base + 18;
     syscall_nr_compat_43_olseek = syscall_nr_base + 19;
  { syscall: "getpid" ret: "pid_t" args:  }
     syscall_nr_getpid = syscall_nr_base + 20;
  { syscall: "mount" ret: "int" args: "const char  " "const char  " "int" "void  "  }
     syscall_nr_mount = syscall_nr_base + 21;
  { syscall: "unmount" ret: "int" args: "const char  " "int"  }
     syscall_nr_unmount = syscall_nr_base + 22;
  { syscall: "setuid" ret: "int" args: "uid_t"  }
     syscall_nr_setuid = syscall_nr_base + 23;
  { syscall: "getuid" ret: "uid_t" args:  }
     syscall_nr_getuid = syscall_nr_base + 24;
  { syscall: "geteuid" ret: "uid_t" args:  }
     syscall_nr_geteuid = syscall_nr_base + 25;
  { syscall: "ptrace" ret: "int" args: "int" "pid_t" "caddr_t" "int"  }
     syscall_nr_ptrace = syscall_nr_base + 26;
  { syscall: "recvmsg" ret: "ssize_t" args: "int" "struct msghdr  " "int"  }
     syscall_nr_recvmsg = syscall_nr_base + 27;
  { syscall: "sendmsg" ret: "ssize_t" args: "int" "const struct msghdr  " "int"  }
     syscall_nr_sendmsg = syscall_nr_base + 28;
  { syscall: "recvfrom" ret: "ssize_t" args: "int" "void  " "size_t" "int" "struct sockaddr  " "unsigned int  "  }
     syscall_nr_recvfrom = syscall_nr_base + 29;
  { syscall: "accept" ret: "int" args: "int" "struct sockaddr  " "unsigned int  "  }
     syscall_nr_accept = syscall_nr_base + 30;
  { syscall: "getpeername" ret: "int" args: "int" "struct sockaddr  " "unsigned int  "  }
     syscall_nr_getpeername = syscall_nr_base + 31;
  { syscall: "getsockname" ret: "int" args: "int" "struct sockaddr  " "unsigned int  "  }
     syscall_nr_getsockname = syscall_nr_base + 32;
  { syscall: "access" ret: "int" args: "const char  " "int"  }
     syscall_nr_access = syscall_nr_base + 33;
  { syscall: "chflags" ret: "int" args: "const char  " "u_long"  }
     syscall_nr_chflags = syscall_nr_base + 34;
  { syscall: "fchflags" ret: "int" args: "int" "u_long"  }
     syscall_nr_fchflags = syscall_nr_base + 35;
  { syscall: "sync" ret: "void" args:  }
     syscall_nr_sync = syscall_nr_base + 36;
  { syscall: "kill" ret: "int" args: "int" "int"  }
     syscall_nr_kill = syscall_nr_base + 37;
     syscall_nr_compat_43_stat43 = syscall_nr_base + 38;
  { syscall: "getppid" ret: "pid_t" args:  }
     syscall_nr_getppid = syscall_nr_base + 39;
     syscall_nr_compat_43_lstat43 = syscall_nr_base + 40;
  { syscall: "dup" ret: "int" args: "int"  }
     syscall_nr_dup = syscall_nr_base + 41;
  { syscall: "pipe" ret: "int" args:  }
     syscall_nr_pipe = syscall_nr_base + 42;
  { syscall: "getegid" ret: "gid_t" args:  }
     syscall_nr_getegid = syscall_nr_base + 43;
  { syscall: "profil" ret: "int" args: "caddr_t" "size_t" "u_long" "u_int"  }
     syscall_nr_profil = syscall_nr_base + 44;
  { syscall: "ktrace" ret: "int" args: "const char  " "int" "int" "int"  }
     syscall_nr_ktrace = syscall_nr_base + 45;
  { 45 is excluded ktrace  }
     syscall_nr_compat_13_sigaction13 = syscall_nr_base + 46;
  { syscall: "getgid" ret: "gid_t" args:  }
     syscall_nr_getgid = syscall_nr_base + 47;
     syscall_nr_compat_13_sigprocmask13 = syscall_nr_base + 48;
  { syscall: "__getlogin" ret: "int" args: "char  " "size_t"  }
     syscall_nr___getlogin = syscall_nr_base + 49;
  { syscall: "setlogin" ret: "int" args: "const char  "  }
     syscall_nr_setlogin = syscall_nr_base + 50;
  { syscall: "acct" ret: "int" args: "const char  "  }
     syscall_nr_acct = syscall_nr_base + 51;
     syscall_nr_compat_13_sigpending13 = syscall_nr_base + 52;
     syscall_nr_compat_13_sigaltstack13 = syscall_nr_base + 53;
  { syscall: "ioctl" ret: "int" args: "int" "u_long" "..."  }
     syscall_nr_ioctl = syscall_nr_base + 54;
     syscall_nr_compat_12_oreboot = syscall_nr_base + 55;
  { syscall: "revoke" ret: "int" args: "const char  "  }
     syscall_nr_revoke = syscall_nr_base + 56;
  { syscall: "symlink" ret: "int" args: "const char  " "const char  "  }
     syscall_nr_symlink = syscall_nr_base + 57;
  { syscall: "readlink" ret: "int" args: "const char  " "char  " "size_t"  }
     syscall_nr_readlink = syscall_nr_base + 58;
  { syscall: "execve" ret: "int" args: "const char  " "char  const  " "char  const  "  }
     syscall_nr_execve = syscall_nr_base + 59;
  { syscall: "umask" ret: "mode_t" args: "mode_t"  }
     syscall_nr_umask = syscall_nr_base + 60;
  { syscall: "chroot" ret: "int" args: "const char  "  }
     syscall_nr_chroot = syscall_nr_base + 61;
     syscall_nr_compat_43_fstat43 = syscall_nr_base + 62;
     syscall_nr_compat_43_ogetkerninfo = syscall_nr_base + 63;
     syscall_nr_compat_43_ogetpagesize = syscall_nr_base + 64;
     syscall_nr_compat_12_msync = syscall_nr_base + 65;
  { syscall: "vfork" ret: "int" args:  }
     syscall_nr_vfork = syscall_nr_base + 66;
  { 67 is obsolete vread  }
  { 68 is obsolete vwrite  }
  { syscall: "sbrk" ret: "int" args: "intptr_t"  }
     syscall_nr_sbrk = syscall_nr_base + 69;
  { syscall: "sstk" ret: "int" args: "int"  }
     syscall_nr_sstk = syscall_nr_base + 70;
     syscall_nr_compat_43_ommap = syscall_nr_base + 71;
  { syscall: "vadvise" ret: "int" args: "int"  }
     syscall_nr_vadvise = syscall_nr_base + 72;
  { syscall: "munmap" ret: "int" args: "void  " "size_t"  }
     syscall_nr_munmap = syscall_nr_base + 73;
  { syscall: "mprotect" ret: "int" args: "void  " "size_t" "int"  }
     syscall_nr_mprotect = syscall_nr_base + 74;
  { syscall: "madvise" ret: "int" args: "void  " "size_t" "int"  }
     syscall_nr_madvise = syscall_nr_base + 75;
  { 76 is obsolete vhangup  }
  { 77 is obsolete vlimit  }
  { syscall: "mincore" ret: "int" args: "void  " "size_t" "char  "  }
     syscall_nr_mincore = syscall_nr_base + 78;
  { syscall: "getgroups" ret: "int" args: "int" "gid_t  "  }
     syscall_nr_getgroups = syscall_nr_base + 79;
  { syscall: "setgroups" ret: "int" args: "int" "const gid_t  "  }
     syscall_nr_setgroups = syscall_nr_base + 80;
  { syscall: "getpgrp" ret: "int" args:  }
     syscall_nr_getpgrp = syscall_nr_base + 81;
  { syscall: "setpgid" ret: "int" args: "int" "int"  }
     syscall_nr_setpgid = syscall_nr_base + 82;
  { syscall: "setitimer" ret: "int" args: "int" "const struct itimerval  " "struct itimerval  "  }
     syscall_nr_setitimer = syscall_nr_base + 83;
     syscall_nr_compat_43_owait = syscall_nr_base + 84;
     syscall_nr_compat_12_oswapon = syscall_nr_base + 85;
  { syscall: "getitimer" ret: "int" args: "int" "struct itimerval  "  }
     syscall_nr_getitimer = syscall_nr_base + 86;
     syscall_nr_compat_43_ogethostname = syscall_nr_base + 87;
     syscall_nr_compat_43_osethostname = syscall_nr_base + 88;
     syscall_nr_compat_43_ogetdtablesize = syscall_nr_base + 89;
  { syscall: "dup2" ret: "int" args: "int" "int"  }
     syscall_nr_dup2 = syscall_nr_base + 90;
  { syscall: "fcntl" ret: "int" args: "int" "int" "..."  }
     syscall_nr_fcntl = syscall_nr_base + 92;
  { syscall: "select" ret: "int" args: "int" "fd_set  " "fd_set  " "fd_set  " "struct timeval  "  }
     syscall_nr_select = syscall_nr_base + 93;
  { syscall: "fsync" ret: "int" args: "int"  }
     syscall_nr_fsync = syscall_nr_base + 95;
  { syscall: "setpriority" ret: "int" args: "int" "int" "int"  }
     syscall_nr_setpriority = syscall_nr_base + 96;
  { syscall: "socket" ret: "int" args: "int" "int" "int"  }
     syscall_nr_socket = syscall_nr_base + 97;
  { syscall: "connect" ret: "int" args: "int" "const struct sockaddr  " "unsigned int"  }
     syscall_nr_connect = syscall_nr_base + 98;
     syscall_nr_compat_43_oaccept = syscall_nr_base + 99;
  { syscall: "getpriority" ret: "int" args: "int" "int"  }
     syscall_nr_getpriority = syscall_nr_base + 100;
     syscall_nr_compat_43_osend = syscall_nr_base + 101;
     syscall_nr_compat_43_orecv = syscall_nr_base + 102;
     syscall_nr_compat_13_sigreturn13 = syscall_nr_base + 103;
  { syscall: "bind" ret: "int" args: "int" "const struct sockaddr  " "unsigned int"  }
     syscall_nr_bind = syscall_nr_base + 104;
  { syscall: "setsockopt" ret: "int" args: "int" "int" "int" "const void  " "unsigned int"  }
     syscall_nr_setsockopt = syscall_nr_base + 105;
  { syscall: "listen" ret: "int" args: "int" "int"  }
     syscall_nr_listen = syscall_nr_base + 106;
  { 107 is obsolete vtimes  }
     syscall_nr_compat_43_osigvec = syscall_nr_base + 108;
     syscall_nr_compat_43_osigblock = syscall_nr_base + 109;
     syscall_nr_compat_43_osigsetmask = syscall_nr_base + 110;
     syscall_nr_compat_13_sigsuspend13 = syscall_nr_base + 111;
     syscall_nr_compat_43_osigstack = syscall_nr_base + 112;
     syscall_nr_compat_43_orecvmsg = syscall_nr_base + 113;
     syscall_nr_compat_43_osendmsg = syscall_nr_base + 114;
  { 115 is obsolete vtrace  }
  { syscall: "gettimeofday" ret: "int" args: "struct timeval  " "struct timezone  "  }
     syscall_nr_compat_50_gettimeofday = syscall_nr_base + 116;
  { syscall: "getrusage" ret: "int" args: "int" "struct rusage  "  }
     syscall_nr_getrusage = syscall_nr_base + 117;
  { syscall: "getsockopt" ret: "int" args: "int" "int" "int" "void  " "unsigned int  "  }
     syscall_nr_getsockopt = syscall_nr_base + 118;
  { 119 is obsolete resuba  }
  { syscall: "readv" ret: "ssize_t" args: "int" "const struct iovec  " "int"  }
     syscall_nr_readv = syscall_nr_base + 120;
  { syscall: "writev" ret: "ssize_t" args: "int" "const struct iovec  " "int"  }
     syscall_nr_writev = syscall_nr_base + 121;
  { syscall: "settimeofday" ret: "int" args: "const struct timeval  " "const struct timezone  "  }
     syscall_nr_compat_50_settimeofday = syscall_nr_base + 122;
  { syscall: "fchown" ret: "int" args: "int" "uid_t" "gid_t"  }
     syscall_nr_fchown = syscall_nr_base + 123;
  { syscall: "fchmod" ret: "int" args: "int" "mode_t"  }
     syscall_nr_fchmod = syscall_nr_base + 124;
     syscall_nr_compat_43_orecvfrom = syscall_nr_base + 125;
  { syscall: "setreuid" ret: "int" args: "uid_t" "uid_t"  }
     syscall_nr_setreuid = syscall_nr_base + 126;
  { syscall: "setregid" ret: "int" args: "gid_t" "gid_t"  }
     syscall_nr_setregid = syscall_nr_base + 127;
  { syscall: "rename" ret: "int" args: "const char  " "const char  "  }
     syscall_nr_rename = syscall_nr_base + 128;
     syscall_nr_compat_43_otruncate = syscall_nr_base + 129;
     syscall_nr_compat_43_oftruncate = syscall_nr_base + 130;
  { syscall: "flock" ret: "int" args: "int" "int"  }
     syscall_nr_flock = syscall_nr_base + 131;
  { syscall: "mkfifo" ret: "int" args: "const char  " "mode_t"  }
     syscall_nr_mkfifo = syscall_nr_base + 132;
  { syscall: "sendto" ret: "ssize_t" args: "int" "const void  " "size_t" "int" "const struct sockaddr  " "unsigned int"  }
     syscall_nr_sendto = syscall_nr_base + 133;
  { syscall: "shutdown" ret: "int" args: "int" "int"  }
     syscall_nr_shutdown = syscall_nr_base + 134;
  { syscall: "socketpair" ret: "int" args: "int" "int" "int" "int  "  }
     syscall_nr_socketpair = syscall_nr_base + 135;
  { syscall: "mkdir" ret: "int" args: "const char  " "mode_t"  }
     syscall_nr_mkdir = syscall_nr_base + 136;
  { syscall: "rmdir" ret: "int" args: "const char  "  }
     syscall_nr_rmdir = syscall_nr_base + 137;
  { syscall: "utimes" ret: "int" args: "const char  " "const struct timeval  "  }
     syscall_nr_utimes = syscall_nr_base + 138;
  { 139 is obsolete 4.2 sigreturn  }
  { syscall: "adjtime" ret: "int" args: "const struct timeval  " "struct timeval  "  }
     syscall_nr_adjtime = syscall_nr_base + 140;
     syscall_nr_compat_43_ogetpeername = syscall_nr_base + 141;
     syscall_nr_compat_43_ogethostid = syscall_nr_base + 142;
     syscall_nr_compat_43_osethostid = syscall_nr_base + 143;
     syscall_nr_compat_43_ogetrlimit = syscall_nr_base + 144;
     syscall_nr_compat_43_osetrlimit = syscall_nr_base + 145;
     syscall_nr_compat_43_okillpg = syscall_nr_base + 146;
  { syscall: "setsid" ret: "int" args:  }
     syscall_nr_setsid = syscall_nr_base + 147;
  { syscall: "quotactl" ret: "int" args: "const char  " "int" "int" "caddr_t"  }
     syscall_nr_quotactl = syscall_nr_base + 148;
     syscall_nr_compat_43_oquota = syscall_nr_base + 149;
     syscall_nr_compat_43_ogetsockname = syscall_nr_base + 150;
  { syscall: "nfssvc" ret: "int" args: "int" "void  "  }
     syscall_nr_nfssvc = syscall_nr_base + 155;
  { 155 is excluded nfssvc  }
     syscall_nr_compat_43_ogetdirentries = syscall_nr_base + 156;
  { syscall: "statfs" ret: "int" args: "const char  " "struct statfs  "  }
     syscall_nr_statfs = syscall_nr_base + 157;
  { syscall: "fstatfs" ret: "int" args: "int" "struct statfs  "  }
     syscall_nr_fstatfs = syscall_nr_base + 158;
  { syscall: "getfh" ret: "int" args: "const char  " "fhandle_t  "  }
     syscall_nr_getfh = syscall_nr_base + 161;
     syscall_nr_compat_09_ogetdomainname = syscall_nr_base + 162;
     syscall_nr_compat_09_osetdomainname = syscall_nr_base + 163;
     syscall_nr_compat_09_ouname = syscall_nr_base + 164;
  { syscall: "sysarch" ret: "int" args: "int" "void  "  }
     syscall_nr_sysarch = syscall_nr_base + 165;
     syscall_nr_compat_10_osemsys = syscall_nr_base + 169;
  { 169 is excluded 1.0 semsys  }
     syscall_nr_compat_10_omsgsys = syscall_nr_base + 170;
  { 170 is excluded 1.0 msgsys  }
     syscall_nr_compat_10_oshmsys = syscall_nr_base + 171;
  { 171 is excluded 1.0 shmsys  }
  { syscall: "pread" ret: "ssize_t" args: "int" "void  " "size_t" "int" "off_t"  }
     syscall_nr_pread = syscall_nr_base + 173;
  { syscall: "pwrite" ret: "ssize_t" args: "int" "const void  " "size_t" "int" "off_t"  }
     syscall_nr_pwrite = syscall_nr_base + 174;
  { syscall: "ntp_gettime" ret: "int" args: "struct ntptimeval  "  }
     syscall_nr_ntp_gettime = syscall_nr_base + 175;
  { syscall: "ntp_adjtime" ret: "int" args: "struct timex  "  }
     syscall_nr_ntp_adjtime = syscall_nr_base + 176;
  { 176 is excluded ntp_adjtime  }
  { syscall: "setgid" ret: "int" args: "gid_t"  }
     syscall_nr_setgid = syscall_nr_base + 181;
  { syscall: "setegid" ret: "int" args: "gid_t"  }
     syscall_nr_setegid = syscall_nr_base + 182;
  { syscall: "seteuid" ret: "int" args: "uid_t"  }
     syscall_nr_seteuid = syscall_nr_base + 183;
  { syscall: "lfs_bmapv" ret: "int" args: "fsid_t  " "struct block_info  " "int"  }
     syscall_nr_lfs_bmapv = syscall_nr_base + 184;
  { syscall: "lfs_markv" ret: "int" args: "fsid_t  " "struct block_info  " "int"  }
     syscall_nr_lfs_markv = syscall_nr_base + 185;
  { syscall: "lfs_segclean" ret: "int" args: "fsid_t  " "u_long"  }
     syscall_nr_lfs_segclean = syscall_nr_base + 186;
  { syscall: "lfs_segwait" ret: "int" args: "fsid_t  " "struct timeval  "  }
     syscall_nr_lfs_segwait = syscall_nr_base + 187;
  { 184 is excluded lfs_bmapv  }
  { 185 is excluded lfs_markv  }
  { 186 is excluded lfs_segclean  }
  { 187 is excluded lfs_segwait  }
     syscall_nr_compat_12_stat12 = syscall_nr_base + 188;
     syscall_nr_compat_12_fstat12 = syscall_nr_base + 189;
     syscall_nr_compat_12_lstat12 = syscall_nr_base + 190;
  { syscall: "pathconf" ret: "long" args: "const char  " "int"  }
     syscall_nr_pathconf = syscall_nr_base + 191;
  { syscall: "fpathconf" ret: "long" args: "int" "int"  }
     syscall_nr_fpathconf = syscall_nr_base + 192;
  { syscall: "getrlimit" ret: "int" args: "int" "struct rlimit  "  }
     syscall_nr_getrlimit = syscall_nr_base + 194;
  { syscall: "setrlimit" ret: "int" args: "int" "const struct rlimit  "  }
     syscall_nr_setrlimit = syscall_nr_base + 195;
     syscall_nr_compat_12_getdirentries = syscall_nr_base + 196;
  { syscall: "mmap" ret: "void  " args: "void  " "size_t" "int" "int" "int" "long" "off_t"  }
     syscall_nr_mmap = syscall_nr_base + 197;
  { syscall: "__syscall" ret: "quad_t" args: "quad_t" "..."  }
     syscall_nr___syscall = syscall_nr_base + 198;
  { syscall: "lseek" ret: "off_t" args: "int" "int" "off_t" "int"  }
     syscall_nr_lseek = syscall_nr_base + 199;
  { syscall: "truncate" ret: "int" args: "const char  " "int" "off_t"  }
     syscall_nr_truncate = syscall_nr_base + 200;
  { syscall: "ftruncate" ret: "int" args: "int" "int" "off_t"  }
     syscall_nr_ftruncate = syscall_nr_base + 201;
  { syscall: "__sysctl" ret: "int" args: "int  " "u_int" "void  " "size_t  " "void  " "size_t"  }
     syscall_nr___sysctl = syscall_nr_base + 202;
  { syscall: "mlock" ret: "int" args: "const void  " "size_t"  }
     syscall_nr_mlock = syscall_nr_base + 203;
  { syscall: "munlock" ret: "int" args: "const void  " "size_t"  }
     syscall_nr_munlock = syscall_nr_base + 204;
  { syscall: "undelete" ret: "int" args: "const char  "  }
     syscall_nr_undelete = syscall_nr_base + 205;
  { syscall: "futimes" ret: "int" args: "int" "const struct timeval  "  }
     syscall_nr_futimes = syscall_nr_base + 206;
  { syscall: "getpgid" ret: "pid_t" args: "pid_t"  }
     syscall_nr_getpgid = syscall_nr_base + 207;
  { syscall: "reboot" ret: "int" args: "int" "char  "  }
     syscall_nr_reboot = syscall_nr_base + 208;
  { syscall: "poll" ret: "int" args: "struct pollfd  " "u_int" "int"  }
     syscall_nr_poll = syscall_nr_base + 209;
  { 210 is excluded lkmnosys  }
  { 211 is excluded lkmnosys  }
  { 212 is excluded lkmnosys  }
  { 213 is excluded lkmnosys  }
  { 214 is excluded lkmnosys  }
  { 215 is excluded lkmnosys  }
  { 216 is excluded lkmnosys  }
  { 217 is excluded lkmnosys  }
  { 218 is excluded lkmnosys  }
  { 219 is excluded lkmnosys  }
     syscall_nr_compat_14___semctl = syscall_nr_base + 220;
  { syscall: "semget" ret: "int" args: "key_t" "int" "int"  }
     syscall_nr_semget = syscall_nr_base + 221;
  { syscall: "semop" ret: "int" args: "int" "struct sembuf  " "size_t"  }
     syscall_nr_semop = syscall_nr_base + 222;
  { syscall: "semconfig" ret: "int" args: "int"  }
     syscall_nr_semconfig = syscall_nr_base + 223;
  { 220 is excluded compat_14_semctl  }
  { 221 is excluded semget  }
  { 222 is excluded semop  }
  { 223 is excluded semconfig  }
     syscall_nr_compat_14_msgctl = syscall_nr_base + 224;
  { syscall: "msgget" ret: "int" args: "key_t" "int"  }
     syscall_nr_msgget = syscall_nr_base + 225;
  { syscall: "msgsnd" ret: "int" args: "int" "const void  " "size_t" "int"  }
     syscall_nr_msgsnd = syscall_nr_base + 226;
  { syscall: "msgrcv" ret: "ssize_t" args: "int" "void  " "size_t" "long" "int"  }
     syscall_nr_msgrcv = syscall_nr_base + 227;
  { 224 is excluded compat_14_msgctl  }
  { 225 is excluded msgget  }
  { 226 is excluded msgsnd  }
  { 227 is excluded msgrcv  }
  { syscall: "shmat" ret: "void  " args: "int" "const void  " "int"  }
     syscall_nr_shmat = syscall_nr_base + 228;
     syscall_nr_compat_14_shmctl = syscall_nr_base + 229;
  { syscall: "shmdt" ret: "int" args: "const void  "  }
     syscall_nr_shmdt = syscall_nr_base + 230;
  { syscall: "shmget" ret: "int" args: "key_t" "size_t" "int"  }
     syscall_nr_shmget = syscall_nr_base + 231;
  { 228 is excluded shmat  }
  { 229 is excluded compat_14_shmctl  }
  { 230 is excluded shmdt  }
  { 231 is excluded shmget  }
  { syscall: "clock_gettime" ret: "int" args: "clockid_t" "struct timespec  "  }
     syscall_nr_clock_gettime = syscall_nr_base + 232;
  { syscall: "clock_settime" ret: "int" args: "clockid_t" "const struct timespec  "  }
     syscall_nr_clock_settime = syscall_nr_base + 233;
  { syscall: "clock_getres" ret: "int" args: "clockid_t" "struct timespec  "  }
     syscall_nr_clock_getres = syscall_nr_base + 234;
  { syscall: "nanosleep" ret: "int" args: "const struct timespec  " "struct timespec  "  }
     syscall_nr_nanosleep = syscall_nr_base + 240;
  { syscall: "fdatasync" ret: "int" args: "int"  }
     syscall_nr_fdatasync = syscall_nr_base + 241;
  { syscall: "mlockall" ret: "int" args: "int"  }
     syscall_nr_mlockall = syscall_nr_base + 242;
  { syscall: "munlockall" ret: "int" args:  }
     syscall_nr_munlockall = syscall_nr_base + 243;
  { syscall: "__sigtimedwait" ret:"int" args: "const sigset_t *" "siginfo_t *" 
             "struct timespec *" }
     syscall_nr_sigtimedwait = syscall_nr_base + 244;
  { syscall: "__posix_rename" ret: "int" args: "const char  " "const char  "  }
     syscall_nr___posix_rename = syscall_nr_base + 270;
  { syscall: "swapctl" ret: "int" args: "int" "const void  " "int"  }
     syscall_nr_swapctl = syscall_nr_base + 271;
  { syscall: "getdents" ret: "int" args: "int" "char  " "size_t"  }
     syscall_nr_getdents = syscall_nr_base + 272;
  { syscall: "minherit" ret: "int" args: "void  " "size_t" "int"  }
     syscall_nr_minherit = syscall_nr_base + 273;
  { syscall: "lchmod" ret: "int" args: "const char  " "mode_t"  }
     syscall_nr_lchmod = syscall_nr_base + 274;
  { syscall: "lchown" ret: "int" args: "const char  " "uid_t" "gid_t"  }
     syscall_nr_lchown = syscall_nr_base + 275;
  { syscall: "lutimes" ret: "int" args: "const char  " "const struct timeval  "  }
     syscall_nr_lutimes = syscall_nr_base + 276;
  { syscall: "__msync13" ret: "int" args: "void  " "size_t" "int"  }
     syscall_nr___msync13 = syscall_nr_base + 277;
  { syscall: "__stat13" ret: "int" args: "const char  " "struct stat  "  }
     syscall_nr___stat13 = syscall_nr_base + 278;
  { syscall: "__fstat13" ret: "int" args: "int" "struct stat  "  }
     syscall_nr___fstat13 = syscall_nr_base + 279;
  { syscall: "__lstat13" ret: "int" args: "const char  " "struct stat  "  }
     syscall_nr___lstat13 = syscall_nr_base + 280;
  { syscall: "__sigaltstack14" ret: "int" args: "const struct sigaltstack  " "struct sigaltstack  "  }
     syscall_nr___sigaltstack14 = syscall_nr_base + 281;
  { syscall: "__vfork14" ret: "int" args:  }
     syscall_nr___vfork14 = syscall_nr_base + 282;
  { syscall: "__posix_chown" ret: "int" args: "const char  " "uid_t" "gid_t"  }
     syscall_nr___posix_chown = syscall_nr_base + 283;
  { syscall: "__posix_fchown" ret: "int" args: "int" "uid_t" "gid_t"  }
     syscall_nr___posix_fchown = syscall_nr_base + 284;
  { syscall: "__posix_lchown" ret: "int" args: "const char  " "uid_t" "gid_t"  }
     syscall_nr___posix_lchown = syscall_nr_base + 285;
  { syscall: "getsid" ret: "pid_t" args: "pid_t"  }
     syscall_nr_getsid = syscall_nr_base + 286;
  { syscall: "__clone" ret: "pid_t" args: "int" "void  "  }
     syscall_nr___clone = syscall_nr_base + 287;
  { syscall: "fktrace" ret: "int" args: "const int" "int" "int" "int"  }
     syscall_nr_fktrace = syscall_nr_base + 288;
  { 288 is excluded ktrace  }
  { syscall: "preadv" ret: "ssize_t" args: "int" "const struct iovec  " "int" "int" "off_t"  }
     syscall_nr_preadv = syscall_nr_base + 289;
  { syscall: "pwritev" ret: "ssize_t" args: "int" "const struct iovec  " "int" "int" "off_t"  }
     syscall_nr_pwritev = syscall_nr_base + 290;
  { syscall: "__sigaction14" ret: "int" args: "int" "const struct sigaction  " "struct sigaction  "  }
     syscall_nr___sigaction14 = syscall_nr_base + 291;
  { syscall: "__sigpending14" ret: "int" args: "sigset_t  "  }
     syscall_nr___sigpending14 = syscall_nr_base + 292;
  { syscall: "__sigprocmask14" ret: "int" args: "int" "const sigset_t  " "sigset_t  "  }
     syscall_nr___sigprocmask14 = syscall_nr_base + 293;
  { syscall: "__sigsuspend14" ret: "int" args: "const sigset_t  "  }
     syscall_nr___sigsuspend14 = syscall_nr_base + 294;
  { syscall: "__sigreturn14" ret: "int" args: "struct sigcontext  "  }
     syscall_nr___sigreturn14 = syscall_nr_base + 295;
  { syscall: "__getcwd" ret: "int" args: "char  " "size_t"  }
     syscall_nr___getcwd = syscall_nr_base + 296;
  { syscall: "fchroot" ret: "int" args: "int"  }
     syscall_nr_fchroot = syscall_nr_base + 297;
  { syscall: "fhopen" ret: "int" args: "const fhandle_t  " "int"  }
     syscall_nr_fhopen = syscall_nr_base + 298;
  { syscall: "fhstat" ret: "int" args: "const fhandle_t  " "struct stat  "  }
     syscall_nr_fhstat = syscall_nr_base + 299;
  { syscall: "fhstatfs" ret: "int" args: "const fhandle_t  " "struct statfs  "  }
     syscall_nr_fhstatfs = syscall_nr_base + 300;
  { syscall: "____semctl13" ret: "int" args: "int" "int" "int" "..."  }
     syscall_nr_____semctl13 = syscall_nr_base + 301;
  { 301 is excluded ____semctl13  }
  { syscall: "__msgctl13" ret: "int" args: "int" "int" "struct msqid_ds  "  }
     syscall_nr___msgctl13 = syscall_nr_base + 302;
  { 302 is excluded __msgctl13  }
  { syscall: "__shmctl13" ret: "int" args: "int" "int" "struct shmid_ds  "  }
     syscall_nr___shmctl13 = syscall_nr_base + 303;
  { 303 is excluded __shmctl13  }
  { syscall: "lchflags" ret: "int" args: "const char  " "u_long"  }
     syscall_nr_lchflags = syscall_nr_base + 304;
  { syscall: "issetugid" ret: "int" args:  }
     syscall_nr_issetugid = syscall_nr_base + 305;
  { syscall: "utrace" ret: "int" args: "const char  " "void  " "size_t"  }
     syscall_nr_utrace = syscall_nr_base + 306;
  { syscall: "__sigaction_sigtramp" ret: "int" args: "int" "const struct sigaction *"
             "struct sigaction *" "const void *" "int"  }
     syscall_nr___sigaction_sigtramp = syscall_nr_base + 340;
{$if defined(CPUX86_64) or defined(CPUARM)}
  {$define USE_SIGACTION_SIGTRAMP}
{$endif}
  { syscall: "kqueue" ret: "int" args: }
     syscall_nr_kqueue = syscall_nr_base + 344;
  { syscall: "kevent" ret: "int" args: "int" "const struct kevent *" "size_t" "struct kevent *" "size_t" "const struct timespec *" }
    syscall_nr_kevent = syscall_nr_base + 345;
  { syscall: "__stat30" ret: "int" args: "const char *" "struct stat *" }
    syscall_nr___stat30 = syscall_nr_base + 387;

  { syscall: "__fstat30" ret: "int" args: "int" "struct stat *" }
    syscall_nr___fstat30 = syscall_nr_base + 388;

  { syscall: "__lstat30" ret: "int" args: "const char *" "struct stat *" }
    syscall_nr___lstat30 = syscall_nr_base + 389;

  { syscall: "__gettimeofday50" ret: "int" args: "struct timeval *" "void *" }
    syscall_nr___gettimeofday50 = syscall_nr_base + 418;
  { syscall: "__settimeofday50" ret: "int" args: "const struct timeval *" "const void *" }
    syscall_nr___settimeofday50 = syscall_nr_base + 419;

  { syscall: "__stat50" ret: "int" args: "const char *" "struct stat *" }
    syscall_nr___stat50  = syscall_nr_base + 439;
  { syscall: "__fstat50" ret: "int" args: "int" "struct stat *" }
    syscall_nr___fstat50 = syscall_nr_base + 440;
  { syscall: "__lstat50" ret: "int" args: "const char *" "struct stat *" }
    syscall_nr___lstat50 = syscall_nr_base + 441;
  { 472	STD  RUMP ( int|sys||futimens(int fd, const struct timespec *tptr); ) }
  { Added in rev 1.252 dated 2011/08/17 }
    syscall_nr_futimens = syscall_nr_base + 472;

    syscall_nr_MAXSYSCALL = syscall_nr_base + 472;
    syscall_nr_NSYSENT = syscall_nr_base + 512;


{ More or less checked BSD syscalls }
{ I checked the values of these constant
  and removed those who are equal
  and added some equivalents, unchecked PM }
{$ifdef FPC_ABI_EABI}
{$define NETBSD_USE_CALLS50}
{$endif}

{$ifdef NETBSD_USE_CALLS50}
 syscall_nr_fstat                       = syscall_nr___fstat50;
 syscall_nr_lstat                       = syscall_nr___lstat50;
 syscall_nr_stat                        = syscall_nr___stat50;
 syscall_nr_gettimeofday                = syscall_nr___gettimeofday50;
 syscall_nr_settimeofday                = syscall_nr___settimeofday50;
{$else}
{$ifdef NETBSD_USE_STAT30}
{ syscall_nr_compat_12_fstat12 = 189 }
{ syscall_nr___fstat13 = 279 }
 syscall_nr_fstat                       = syscall_nr___fstat30;
{ syscall_nr_compat_12_lstat12 = 190 }
{  syscall_nr___lstat13 = 280 }
 syscall_nr_lstat                       = syscall_nr___lstat30;
{ syscall_nr_compat_12_stat12 = 188; }
{ syscall_nr___stat13 = 278 }
 syscall_nr_stat                        = syscall_nr___stat30;
{$else}
 syscall_nr_fstat                       = syscall_nr___fstat13;
 syscall_nr_lstat                       = syscall_nr___lstat13;
 syscall_nr_stat                        = syscall_nr___stat13;
{$endif}
 syscall_nr_gettimeofday                = syscall_nr_compat_50_gettimeofday;
 syscall_nr_settimeofday                = syscall_nr_compat_50_settimeofday;
{$endif}
{ syscall_nr_sigaction                   = 342; MISSING }
{ syscall: "__sigaction14" ret: "int" args: "int" "const struct sigaction  " "struct sigaction  "  }
 syscall_nr_sigaction = syscall_nr___sigaction14;
{ syscall_nr_sigpending                  = 343; MISSING }
{ syscall: "__sigpending14" ret: "int" args: "sigset_t  "  }
 syscall_nr_sigpending = syscall_nr___sigpending14;
{ syscall_nr_sigprocmask                 = 340; MISSING }
{ syscall: "__sigprocmask14" ret: "int" args: "int" "const sigset_t  " "sigset_t  "  }
 syscall_nr_sigprocmask = syscall_nr___sigprocmask14;
{ syscall_nr_sigsuspend                  = 341; MISSING }
{ syscall: "__sigsuspend14" ret: "int" args: "const sigset_t  "  }
 syscall_nr_sigsuspend = syscall_nr___sigsuspend14;
{ syscall: "__sigreturn14" ret: "int" args: "struct sigcontext  "  }
 syscall_nr_sigreturn = syscall_nr___sigreturn14;
{ Note: syscall_nr_sigreturn was not on the checked list for FreeBSD }

{ syscall_nr_wait4 = 7 }
 syscall_nr_waitpid                     =   syscall_nr_wait4;
{ This one seems to be missing !!!!
 syscall_nr_rfork                       = 251;}
{$define  FPC_HAS_NO_SYSCALL_NR_RFORK }
{ syscall_nr_compat_12_getdirentries = 196 }
 syscall_nr_getdirentries = syscall_nr_compat_12_getdirentries;

