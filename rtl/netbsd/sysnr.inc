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

  {
  Converted from NetBSD CVS tree:
  syscall.h,v 1.120 2001/07/01 18:07:33
  }

     syscall_nr_syscall = 0;
  { syscall: "exit" ret: "void" args: "int"  }
     syscall_nr_exit = 1;
  { syscall: "fork" ret: "int" args:  }
     syscall_nr_fork = 2;
  { syscall: "read" ret: "ssize_t" args: "int" "void  " "size_t"  }
     syscall_nr_read = 3;
  { syscall: "write" ret: "ssize_t" args: "int" "const void  " "size_t"  }
     syscall_nr_write = 4;
  { syscall: "open" ret: "int" args: "const char  " "int" "..."  }
     syscall_nr_open = 5;
  { syscall: "close" ret: "int" args: "int"  }
     syscall_nr_close = 6;
  { syscall: "wait4" ret: "int" args: "int" "int  " "int" "struct rusage  "  }
     syscall_nr_wait4 = 7;
     syscall_nr_compat_43_ocreat = 8;
  { syscall: "link" ret: "int" args: "const char  " "const char  "  }
     syscall_nr_link = 9;
  { syscall: "unlink" ret: "int" args: "const char  "  }
     syscall_nr_unlink = 10;
  { 11 is obsolete execv  }
  { syscall: "chdir" ret: "int" args: "const char  "  }
     syscall_nr_chdir = 12;
  { syscall: "fchdir" ret: "int" args: "int"  }
     syscall_nr_fchdir = 13;
  { syscall: "mknod" ret: "int" args: "const char  " "mode_t" "dev_t"  }
     syscall_nr_mknod = 14;
  { syscall: "chmod" ret: "int" args: "const char  " "mode_t"  }
     syscall_nr_chmod = 15;
  { syscall: "chown" ret: "int" args: "const char  " "uid_t" "gid_t"  }
     syscall_nr_chown = 16;
  { syscall: "break" ret: "int" args: "char  "  }
     syscall_nr_break = 17;
  { syscall: "getfsstat" ret: "int" args: "struct statfs  " "long" "int"  }
     syscall_nr_getfsstat = 18;
     syscall_nr_compat_43_olseek = 19;
  { syscall: "getpid" ret: "pid_t" args:  }
     syscall_nr_getpid = 20;
  { syscall: "mount" ret: "int" args: "const char  " "const char  " "int" "void  "  }
     syscall_nr_mount = 21;
  { syscall: "unmount" ret: "int" args: "const char  " "int"  }
     syscall_nr_unmount = 22;
  { syscall: "setuid" ret: "int" args: "uid_t"  }
     syscall_nr_setuid = 23;
  { syscall: "getuid" ret: "uid_t" args:  }
     syscall_nr_getuid = 24;
  { syscall: "geteuid" ret: "uid_t" args:  }
     syscall_nr_geteuid = 25;
  { syscall: "ptrace" ret: "int" args: "int" "pid_t" "caddr_t" "int"  }
     syscall_nr_ptrace = 26;
  { syscall: "recvmsg" ret: "ssize_t" args: "int" "struct msghdr  " "int"  }
     syscall_nr_recvmsg = 27;
  { syscall: "sendmsg" ret: "ssize_t" args: "int" "const struct msghdr  " "int"  }
     syscall_nr_sendmsg = 28;
  { syscall: "recvfrom" ret: "ssize_t" args: "int" "void  " "size_t" "int" "struct sockaddr  " "unsigned int  "  }
     syscall_nr_recvfrom = 29;
  { syscall: "accept" ret: "int" args: "int" "struct sockaddr  " "unsigned int  "  }
     syscall_nr_accept = 30;
  { syscall: "getpeername" ret: "int" args: "int" "struct sockaddr  " "unsigned int  "  }
     syscall_nr_getpeername = 31;
  { syscall: "getsockname" ret: "int" args: "int" "struct sockaddr  " "unsigned int  "  }
     syscall_nr_getsockname = 32;
  { syscall: "access" ret: "int" args: "const char  " "int"  }
     syscall_nr_access = 33;
  { syscall: "chflags" ret: "int" args: "const char  " "u_long"  }
     syscall_nr_chflags = 34;
  { syscall: "fchflags" ret: "int" args: "int" "u_long"  }
     syscall_nr_fchflags = 35;
  { syscall: "sync" ret: "void" args:  }
     syscall_nr_sync = 36;
  { syscall: "kill" ret: "int" args: "int" "int"  }
     syscall_nr_kill = 37;
     syscall_nr_compat_43_stat43 = 38;
  { syscall: "getppid" ret: "pid_t" args:  }
     syscall_nr_getppid = 39;
     syscall_nr_compat_43_lstat43 = 40;
  { syscall: "dup" ret: "int" args: "int"  }
     syscall_nr_dup = 41;
  { syscall: "pipe" ret: "int" args:  }
     syscall_nr_pipe = 42;
  { syscall: "getegid" ret: "gid_t" args:  }
     syscall_nr_getegid = 43;
  { syscall: "profil" ret: "int" args: "caddr_t" "size_t" "u_long" "u_int"  }
     syscall_nr_profil = 44;
  { syscall: "ktrace" ret: "int" args: "const char  " "int" "int" "int"  }
     syscall_nr_ktrace = 45;
  { 45 is excluded ktrace  }
     syscall_nr_compat_13_sigaction13 = 46;
  { syscall: "getgid" ret: "gid_t" args:  }
     syscall_nr_getgid = 47;
     syscall_nr_compat_13_sigprocmask13 = 48;
  { syscall: "__getlogin" ret: "int" args: "char  " "size_t"  }
     syscall_nr___getlogin = 49;
  { syscall: "setlogin" ret: "int" args: "const char  "  }
     syscall_nr_setlogin = 50;
  { syscall: "acct" ret: "int" args: "const char  "  }
     syscall_nr_acct = 51;
     syscall_nr_compat_13_sigpending13 = 52;
     syscall_nr_compat_13_sigaltstack13 = 53;
  { syscall: "ioctl" ret: "int" args: "int" "u_long" "..."  }
     syscall_nr_ioctl = 54;
     syscall_nr_compat_12_oreboot = 55;
  { syscall: "revoke" ret: "int" args: "const char  "  }
     syscall_nr_revoke = 56;
  { syscall: "symlink" ret: "int" args: "const char  " "const char  "  }
     syscall_nr_symlink = 57;
  { syscall: "readlink" ret: "int" args: "const char  " "char  " "size_t"  }
     syscall_nr_readlink = 58;
  { syscall: "execve" ret: "int" args: "const char  " "char  const  " "char  const  "  }
     syscall_nr_execve = 59;
  { syscall: "umask" ret: "mode_t" args: "mode_t"  }
     syscall_nr_umask = 60;
  { syscall: "chroot" ret: "int" args: "const char  "  }
     syscall_nr_chroot = 61;
     syscall_nr_compat_43_fstat43 = 62;
     syscall_nr_compat_43_ogetkerninfo = 63;
     syscall_nr_compat_43_ogetpagesize = 64;
     syscall_nr_compat_12_msync = 65;
  { syscall: "vfork" ret: "int" args:  }
     syscall_nr_vfork = 66;
  { 67 is obsolete vread  }
  { 68 is obsolete vwrite  }
  { syscall: "sbrk" ret: "int" args: "intptr_t"  }
     syscall_nr_sbrk = 69;
  { syscall: "sstk" ret: "int" args: "int"  }
     syscall_nr_sstk = 70;
     syscall_nr_compat_43_ommap = 71;
  { syscall: "vadvise" ret: "int" args: "int"  }
     syscall_nr_vadvise = 72;
  { syscall: "munmap" ret: "int" args: "void  " "size_t"  }
     syscall_nr_munmap = 73;
  { syscall: "mprotect" ret: "int" args: "void  " "size_t" "int"  }
     syscall_nr_mprotect = 74;
  { syscall: "madvise" ret: "int" args: "void  " "size_t" "int"  }
     syscall_nr_madvise = 75;
  { 76 is obsolete vhangup  }
  { 77 is obsolete vlimit  }
  { syscall: "mincore" ret: "int" args: "void  " "size_t" "char  "  }
     syscall_nr_mincore = 78;
  { syscall: "getgroups" ret: "int" args: "int" "gid_t  "  }
     syscall_nr_getgroups = 79;
  { syscall: "setgroups" ret: "int" args: "int" "const gid_t  "  }
     syscall_nr_setgroups = 80;
  { syscall: "getpgrp" ret: "int" args:  }
     syscall_nr_getpgrp = 81;
  { syscall: "setpgid" ret: "int" args: "int" "int"  }
     syscall_nr_setpgid = 82;
  { syscall: "setitimer" ret: "int" args: "int" "const struct itimerval  " "struct itimerval  "  }
     syscall_nr_setitimer = 83;
     syscall_nr_compat_43_owait = 84;
     syscall_nr_compat_12_oswapon = 85;
  { syscall: "getitimer" ret: "int" args: "int" "struct itimerval  "  }
     syscall_nr_getitimer = 86;
     syscall_nr_compat_43_ogethostname = 87;
     syscall_nr_compat_43_osethostname = 88;
     syscall_nr_compat_43_ogetdtablesize = 89;
  { syscall: "dup2" ret: "int" args: "int" "int"  }
     syscall_nr_dup2 = 90;
  { syscall: "fcntl" ret: "int" args: "int" "int" "..."  }
     syscall_nr_fcntl = 92;
  { syscall: "select" ret: "int" args: "int" "fd_set  " "fd_set  " "fd_set  " "struct timeval  "  }
     syscall_nr_select = 93;
  { syscall: "fsync" ret: "int" args: "int"  }
     syscall_nr_fsync = 95;
  { syscall: "setpriority" ret: "int" args: "int" "int" "int"  }
     syscall_nr_setpriority = 96;
  { syscall: "socket" ret: "int" args: "int" "int" "int"  }
     syscall_nr_socket = 97;
  { syscall: "connect" ret: "int" args: "int" "const struct sockaddr  " "unsigned int"  }
     syscall_nr_connect = 98;
     syscall_nr_compat_43_oaccept = 99;
  { syscall: "getpriority" ret: "int" args: "int" "int"  }
     syscall_nr_getpriority = 100;
     syscall_nr_compat_43_osend = 101;
     syscall_nr_compat_43_orecv = 102;
     syscall_nr_compat_13_sigreturn13 = 103;
  { syscall: "bind" ret: "int" args: "int" "const struct sockaddr  " "unsigned int"  }
     syscall_nr_bind = 104;
  { syscall: "setsockopt" ret: "int" args: "int" "int" "int" "const void  " "unsigned int"  }
     syscall_nr_setsockopt = 105;
  { syscall: "listen" ret: "int" args: "int" "int"  }
     syscall_nr_listen = 106;
  { 107 is obsolete vtimes  }
     syscall_nr_compat_43_osigvec = 108;
     syscall_nr_compat_43_osigblock = 109;
     syscall_nr_compat_43_osigsetmask = 110;
     syscall_nr_compat_13_sigsuspend13 = 111;
     syscall_nr_compat_43_osigstack = 112;
     syscall_nr_compat_43_orecvmsg = 113;
     syscall_nr_compat_43_osendmsg = 114;
  { 115 is obsolete vtrace  }
  { syscall: "gettimeofday" ret: "int" args: "struct timeval  " "struct timezone  "  }
     syscall_nr_gettimeofday = 116;
  { syscall: "getrusage" ret: "int" args: "int" "struct rusage  "  }
     syscall_nr_getrusage = 117;
  { syscall: "getsockopt" ret: "int" args: "int" "int" "int" "void  " "unsigned int  "  }
     syscall_nr_getsockopt = 118;
  { 119 is obsolete resuba  }
  { syscall: "readv" ret: "ssize_t" args: "int" "const struct iovec  " "int"  }
     syscall_nr_readv = 120;
  { syscall: "writev" ret: "ssize_t" args: "int" "const struct iovec  " "int"  }
     syscall_nr_writev = 121;
  { syscall: "settimeofday" ret: "int" args: "const struct timeval  " "const struct timezone  "  }
     syscall_nr_settimeofday = 122;
  { syscall: "fchown" ret: "int" args: "int" "uid_t" "gid_t"  }
     syscall_nr_fchown = 123;
  { syscall: "fchmod" ret: "int" args: "int" "mode_t"  }
     syscall_nr_fchmod = 124;
     syscall_nr_compat_43_orecvfrom = 125;
  { syscall: "setreuid" ret: "int" args: "uid_t" "uid_t"  }
     syscall_nr_setreuid = 126;
  { syscall: "setregid" ret: "int" args: "gid_t" "gid_t"  }
     syscall_nr_setregid = 127;
  { syscall: "rename" ret: "int" args: "const char  " "const char  "  }
     syscall_nr_rename = 128;
     syscall_nr_compat_43_otruncate = 129;
     syscall_nr_compat_43_oftruncate = 130;
  { syscall: "flock" ret: "int" args: "int" "int"  }
     syscall_nr_flock = 131;
  { syscall: "mkfifo" ret: "int" args: "const char  " "mode_t"  }
     syscall_nr_mkfifo = 132;
  { syscall: "sendto" ret: "ssize_t" args: "int" "const void  " "size_t" "int" "const struct sockaddr  " "unsigned int"  }
     syscall_nr_sendto = 133;
  { syscall: "shutdown" ret: "int" args: "int" "int"  }
     syscall_nr_shutdown = 134;
  { syscall: "socketpair" ret: "int" args: "int" "int" "int" "int  "  }
     syscall_nr_socketpair = 135;
  { syscall: "mkdir" ret: "int" args: "const char  " "mode_t"  }
     syscall_nr_mkdir = 136;
  { syscall: "rmdir" ret: "int" args: "const char  "  }
     syscall_nr_rmdir = 137;
  { syscall: "utimes" ret: "int" args: "const char  " "const struct timeval  "  }
     syscall_nr_utimes = 138;
  { 139 is obsolete 4.2 sigreturn  }
  { syscall: "adjtime" ret: "int" args: "const struct timeval  " "struct timeval  "  }
     syscall_nr_adjtime = 140;
     syscall_nr_compat_43_ogetpeername = 141;
     syscall_nr_compat_43_ogethostid = 142;
     syscall_nr_compat_43_osethostid = 143;
     syscall_nr_compat_43_ogetrlimit = 144;
     syscall_nr_compat_43_osetrlimit = 145;
     syscall_nr_compat_43_okillpg = 146;
  { syscall: "setsid" ret: "int" args:  }
     syscall_nr_setsid = 147;
  { syscall: "quotactl" ret: "int" args: "const char  " "int" "int" "caddr_t"  }
     syscall_nr_quotactl = 148;
     syscall_nr_compat_43_oquota = 149;
     syscall_nr_compat_43_ogetsockname = 150;
  { syscall: "nfssvc" ret: "int" args: "int" "void  "  }
     syscall_nr_nfssvc = 155;
  { 155 is excluded nfssvc  }
     syscall_nr_compat_43_ogetdirentries = 156;
  { syscall: "statfs" ret: "int" args: "const char  " "struct statfs  "  }
     syscall_nr_statfs = 157;
  { syscall: "fstatfs" ret: "int" args: "int" "struct statfs  "  }
     syscall_nr_fstatfs = 158;
  { syscall: "getfh" ret: "int" args: "const char  " "fhandle_t  "  }
     syscall_nr_getfh = 161;
     syscall_nr_compat_09_ogetdomainname = 162;
     syscall_nr_compat_09_osetdomainname = 163;
     syscall_nr_compat_09_ouname = 164;
  { syscall: "sysarch" ret: "int" args: "int" "void  "  }
     syscall_nr_sysarch = 165;
     syscall_nr_compat_10_osemsys = 169;
  { 169 is excluded 1.0 semsys  }
     syscall_nr_compat_10_omsgsys = 170;
  { 170 is excluded 1.0 msgsys  }
     syscall_nr_compat_10_oshmsys = 171;
  { 171 is excluded 1.0 shmsys  }
  { syscall: "pread" ret: "ssize_t" args: "int" "void  " "size_t" "int" "off_t"  }
     syscall_nr_pread = 173;
  { syscall: "pwrite" ret: "ssize_t" args: "int" "const void  " "size_t" "int" "off_t"  }
     syscall_nr_pwrite = 174;
  { syscall: "ntp_gettime" ret: "int" args: "struct ntptimeval  "  }
     syscall_nr_ntp_gettime = 175;
  { syscall: "ntp_adjtime" ret: "int" args: "struct timex  "  }
     syscall_nr_ntp_adjtime = 176;
  { 176 is excluded ntp_adjtime  }
  { syscall: "setgid" ret: "int" args: "gid_t"  }
     syscall_nr_setgid = 181;
  { syscall: "setegid" ret: "int" args: "gid_t"  }
     syscall_nr_setegid = 182;
  { syscall: "seteuid" ret: "int" args: "uid_t"  }
     syscall_nr_seteuid = 183;
  { syscall: "lfs_bmapv" ret: "int" args: "fsid_t  " "struct block_info  " "int"  }
     syscall_nr_lfs_bmapv = 184;
  { syscall: "lfs_markv" ret: "int" args: "fsid_t  " "struct block_info  " "int"  }
     syscall_nr_lfs_markv = 185;
  { syscall: "lfs_segclean" ret: "int" args: "fsid_t  " "u_long"  }
     syscall_nr_lfs_segclean = 186;
  { syscall: "lfs_segwait" ret: "int" args: "fsid_t  " "struct timeval  "  }
     syscall_nr_lfs_segwait = 187;
  { 184 is excluded lfs_bmapv  }
  { 185 is excluded lfs_markv  }
  { 186 is excluded lfs_segclean  }
  { 187 is excluded lfs_segwait  }
     syscall_nr_compat_12_stat12 = 188;
     syscall_nr_compat_12_fstat12 = 189;
     syscall_nr_compat_12_lstat12 = 190;
  { syscall: "pathconf" ret: "long" args: "const char  " "int"  }
     syscall_nr_pathconf = 191;
  { syscall: "fpathconf" ret: "long" args: "int" "int"  }
     syscall_nr_fpathconf = 192;
  { syscall: "getrlimit" ret: "int" args: "int" "struct rlimit  "  }
     syscall_nr_getrlimit = 194;
  { syscall: "setrlimit" ret: "int" args: "int" "const struct rlimit  "  }
     syscall_nr_setrlimit = 195;
     syscall_nr_compat_12_getdirentries = 196;
  { syscall: "mmap" ret: "void  " args: "void  " "size_t" "int" "int" "int" "long" "off_t"  }
     syscall_nr_mmap = 197;
  { syscall: "__syscall" ret: "quad_t" args: "quad_t" "..."  }
     syscall_nr___syscall = 198;
  { syscall: "lseek" ret: "off_t" args: "int" "int" "off_t" "int"  }
     syscall_nr_lseek = 199;
  { syscall: "truncate" ret: "int" args: "const char  " "int" "off_t"  }
     syscall_nr_truncate = 200;
  { syscall: "ftruncate" ret: "int" args: "int" "int" "off_t"  }
     syscall_nr_ftruncate = 201;
  { syscall: "__sysctl" ret: "int" args: "int  " "u_int" "void  " "size_t  " "void  " "size_t"  }
     syscall_nr___sysctl = 202;
  { syscall: "mlock" ret: "int" args: "const void  " "size_t"  }
     syscall_nr_mlock = 203;
  { syscall: "munlock" ret: "int" args: "const void  " "size_t"  }
     syscall_nr_munlock = 204;
  { syscall: "undelete" ret: "int" args: "const char  "  }
     syscall_nr_undelete = 205;
  { syscall: "futimes" ret: "int" args: "int" "const struct timeval  "  }
     syscall_nr_futimes = 206;
  { syscall: "getpgid" ret: "pid_t" args: "pid_t"  }
     syscall_nr_getpgid = 207;
  { syscall: "reboot" ret: "int" args: "int" "char  "  }
     syscall_nr_reboot = 208;
  { syscall: "poll" ret: "int" args: "struct pollfd  " "u_int" "int"  }
     syscall_nr_poll = 209;
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
     syscall_nr_compat_14___semctl = 220;
  { syscall: "semget" ret: "int" args: "key_t" "int" "int"  }
     syscall_nr_semget = 221;
  { syscall: "semop" ret: "int" args: "int" "struct sembuf  " "size_t"  }
     syscall_nr_semop = 222;
  { syscall: "semconfig" ret: "int" args: "int"  }
     syscall_nr_semconfig = 223;
  { 220 is excluded compat_14_semctl  }
  { 221 is excluded semget  }
  { 222 is excluded semop  }
  { 223 is excluded semconfig  }
     syscall_nr_compat_14_msgctl = 224;
  { syscall: "msgget" ret: "int" args: "key_t" "int"  }
     syscall_nr_msgget = 225;
  { syscall: "msgsnd" ret: "int" args: "int" "const void  " "size_t" "int"  }
     syscall_nr_msgsnd = 226;
  { syscall: "msgrcv" ret: "ssize_t" args: "int" "void  " "size_t" "long" "int"  }
     syscall_nr_msgrcv = 227;
  { 224 is excluded compat_14_msgctl  }
  { 225 is excluded msgget  }
  { 226 is excluded msgsnd  }
  { 227 is excluded msgrcv  }
  { syscall: "shmat" ret: "void  " args: "int" "const void  " "int"  }
     syscall_nr_shmat = 228;
     syscall_nr_compat_14_shmctl = 229;
  { syscall: "shmdt" ret: "int" args: "const void  "  }
     syscall_nr_shmdt = 230;
  { syscall: "shmget" ret: "int" args: "key_t" "size_t" "int"  }
     syscall_nr_shmget = 231;
  { 228 is excluded shmat  }
  { 229 is excluded compat_14_shmctl  }
  { 230 is excluded shmdt  }
  { 231 is excluded shmget  }
  { syscall: "clock_gettime" ret: "int" args: "clockid_t" "struct timespec  "  }
     syscall_nr_clock_gettime = 232;
  { syscall: "clock_settime" ret: "int" args: "clockid_t" "const struct timespec  "  }
     syscall_nr_clock_settime = 233;
  { syscall: "clock_getres" ret: "int" args: "clockid_t" "struct timespec  "  }
     syscall_nr_clock_getres = 234;
  { syscall: "nanosleep" ret: "int" args: "const struct timespec  " "struct timespec  "  }
     syscall_nr_nanosleep = 240;
  { syscall: "fdatasync" ret: "int" args: "int"  }
     syscall_nr_fdatasync = 241;
  { syscall: "mlockall" ret: "int" args: "int"  }
     syscall_nr_mlockall = 242;
  { syscall: "munlockall" ret: "int" args:  }
     syscall_nr_munlockall = 243;
  { syscall: "__posix_rename" ret: "int" args: "const char  " "const char  "  }
     syscall_nr___posix_rename = 270;
  { syscall: "swapctl" ret: "int" args: "int" "const void  " "int"  }
     syscall_nr_swapctl = 271;
  { syscall: "getdents" ret: "int" args: "int" "char  " "size_t"  }
     syscall_nr_getdents = 272;
  { syscall: "minherit" ret: "int" args: "void  " "size_t" "int"  }
     syscall_nr_minherit = 273;
  { syscall: "lchmod" ret: "int" args: "const char  " "mode_t"  }
     syscall_nr_lchmod = 274;
  { syscall: "lchown" ret: "int" args: "const char  " "uid_t" "gid_t"  }
     syscall_nr_lchown = 275;
  { syscall: "lutimes" ret: "int" args: "const char  " "const struct timeval  "  }
     syscall_nr_lutimes = 276;
  { syscall: "__msync13" ret: "int" args: "void  " "size_t" "int"  }
     syscall_nr___msync13 = 277;
  { syscall: "__stat13" ret: "int" args: "const char  " "struct stat  "  }
     syscall_nr___stat13 = 278;
  { syscall: "__fstat13" ret: "int" args: "int" "struct stat  "  }
     syscall_nr___fstat13 = 279;
  { syscall: "__lstat13" ret: "int" args: "const char  " "struct stat  "  }
     syscall_nr___lstat13 = 280;
  { syscall: "__sigaltstack14" ret: "int" args: "const struct sigaltstack  " "struct sigaltstack  "  }
     syscall_nr___sigaltstack14 = 281;
  { syscall: "__vfork14" ret: "int" args:  }
     syscall_nr___vfork14 = 282;
  { syscall: "__posix_chown" ret: "int" args: "const char  " "uid_t" "gid_t"  }
     syscall_nr___posix_chown = 283;
  { syscall: "__posix_fchown" ret: "int" args: "int" "uid_t" "gid_t"  }
     syscall_nr___posix_fchown = 284;
  { syscall: "__posix_lchown" ret: "int" args: "const char  " "uid_t" "gid_t"  }
     syscall_nr___posix_lchown = 285;
  { syscall: "getsid" ret: "pid_t" args: "pid_t"  }
     syscall_nr_getsid = 286;
  { syscall: "__clone" ret: "pid_t" args: "int" "void  "  }
     syscall_nr___clone = 287;
  { syscall: "fktrace" ret: "int" args: "const int" "int" "int" "int"  }
     syscall_nr_fktrace = 288;
  { 288 is excluded ktrace  }
  { syscall: "preadv" ret: "ssize_t" args: "int" "const struct iovec  " "int" "int" "off_t"  }
     syscall_nr_preadv = 289;
  { syscall: "pwritev" ret: "ssize_t" args: "int" "const struct iovec  " "int" "int" "off_t"  }
     syscall_nr_pwritev = 290;
  { syscall: "__sigaction14" ret: "int" args: "int" "const struct sigaction  " "struct sigaction  "  }
     syscall_nr___sigaction14 = 291;
  { syscall: "__sigpending14" ret: "int" args: "sigset_t  "  }
     syscall_nr___sigpending14 = 292;
  { syscall: "__sigprocmask14" ret: "int" args: "int" "const sigset_t  " "sigset_t  "  }
     syscall_nr___sigprocmask14 = 293;
  { syscall: "__sigsuspend14" ret: "int" args: "const sigset_t  "  }
     syscall_nr___sigsuspend14 = 294;
  { syscall: "__sigreturn14" ret: "int" args: "struct sigcontext  "  }
     syscall_nr___sigreturn14 = 295;
  { syscall: "__getcwd" ret: "int" args: "char  " "size_t"  }
     syscall_nr___getcwd = 296;
  { syscall: "fchroot" ret: "int" args: "int"  }
     syscall_nr_fchroot = 297;
  { syscall: "fhopen" ret: "int" args: "const fhandle_t  " "int"  }
     syscall_nr_fhopen = 298;
  { syscall: "fhstat" ret: "int" args: "const fhandle_t  " "struct stat  "  }
     syscall_nr_fhstat = 299;
  { syscall: "fhstatfs" ret: "int" args: "const fhandle_t  " "struct statfs  "  }
     syscall_nr_fhstatfs = 300;
  { syscall: "____semctl13" ret: "int" args: "int" "int" "int" "..."  }
     syscall_nr_____semctl13 = 301;
  { 301 is excluded ____semctl13  }
  { syscall: "__msgctl13" ret: "int" args: "int" "int" "struct msqid_ds  "  }
     syscall_nr___msgctl13 = 302;
  { 302 is excluded __msgctl13  }
  { syscall: "__shmctl13" ret: "int" args: "int" "int" "struct shmid_ds  "  }
     syscall_nr___shmctl13 = 303;
  { 303 is excluded __shmctl13  }
  { syscall: "lchflags" ret: "int" args: "const char  " "u_long"  }
     syscall_nr_lchflags = 304;
  { syscall: "issetugid" ret: "int" args:  }
     syscall_nr_issetugid = 305;
  { syscall: "utrace" ret: "int" args: "const char  " "void  " "size_t"  }
     syscall_nr_utrace = 306;
     syscall_nr_MAXSYSCALL = 340;
     syscall_nr_NSYSENT = 512;


{ More or less checked BSD syscalls }
{ I checked the values of these constant
  and removed those who are equal
  and added some equivalents, unchecked PM }

{ syscall_nr_compat_12_fstat12 = 189 }
{ syscall_nr___fstat13 = 279 }
 syscall_nr_fstat                       = syscall_nr___fstat13;
{ syscall_nr_compat_12_lstat12 = 190 }
{  syscall_nr___lstat13 = 280 }
 syscall_nr_lstat                       = syscall_nr___lstat13;
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

{ syscall_nr_compat_12_stat12 = 188; }
{ syscall_nr___stat13 = 278 }
 syscall_nr_stat                        = syscall_nr___stat13;
{ syscall_nr_wait4 = 7 }
 syscall_nr_waitpid                     =   syscall_nr_wait4;
{ This one seems to be missing !!!!
 syscall_nr_rfork                       = 251;}
{$define  FPC_HAS_NO_SYSCALL_NR_RFORK }
{ syscall_nr_compat_12_getdirentries = 196 }
 syscall_nr_getdirentries = syscall_nr_compat_12_getdirentries;

