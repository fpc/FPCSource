  {
    $Id$
    This file is part of the Free Pascal run time library.


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
  { Copyright (c) 1992, 1995-1999 Apple Computer, Inc. All Rights Reserved  }
  {

     The NEXTSTEP Software License Agreement specifies the terms
     and conditions for redistribution.

    }

  const
     sys_nr_syscall = 0;
     sys_nr_exit = 1;
     sys_nr_fork = 2;
     sys_nr_read = 3;
     sys_nr_write = 4;
     sys_nr_open = 5;
     sys_nr_close = 6;
     sys_nr_wait4 = 7;
  { 8 is old creat  }
     sys_nr_link = 9;
     sys_nr_unlink = 10;
  { 11 is obsolete execv  }
     sys_nr_chdir = 12;
     sys_nr_fchdir = 13;
     sys_nr_mknod = 14;
     sys_nr_chmod = 15;
     sys_nr_chown = 16;
  { 17 is obsolete sbreak  }
{$if COMPAT_GETFSSTAT}
  { 18 is old getfsstat  }
{$else}

  const
     sys_nr_getfsstat = 18;
{$endif}
  { 19 is old lseek  }

  const
     sys_nr_getpid = 20;
  { 21 is obsolete mount  }
  { 22 is obsolete umount  }
     sys_nr_setuid = 23;
     sys_nr_getuid = 24;
     sys_nr_geteuid = 25;
     sys_nr_ptrace = 26;
     sys_nr_recvmsg = 27;
     sys_nr_sendmsg = 28;
     sys_nr_recvfrom = 29;
     sys_nr_accept = 30;
     sys_nr_getpeername = 31;
     sys_nr_getsockname = 32;
     sys_nr_access = 33;
     sys_nr_chflags = 34;
     sys_nr_fchflags = 35;
     sys_nr_sync = 36;
     sys_nr_kill = 37;
  { 38 is old stat  }
     sys_nr_getppid = 39;
  { 40 is old lstat  }
     sys_nr_dup = 41;
     sys_nr_pipe = 42;
     sys_nr_getegid = 43;
     sys_nr_profil = 44;
     sys_nr_ktrace = 45;
     sys_nr_sigaction = 46;
     sys_nr_getgid = 47;
     sys_nr_sigprocmask = 48;
     sys_nr_getlogin = 49;
     sys_nr_setlogin = 50;
     sys_nr_acct = 51;
     sys_nr_sigpending = 52;
     sys_nr_sigaltstack = 53;
     sys_nr_ioctl = 54;
     sys_nr_reboot = 55;
     sys_nr_revoke = 56;
     sys_nr_symlink = 57;
     sys_nr_readlink = 58;
     sys_nr_execve = 59;
     sys_nr_umask = 60;
     sys_nr_chroot = 61;
  { 62 is old fstat  }
  { 63 is unused  }
  { 64 is old getpagesize  }
     sys_nr_msync = 65;
     sys_nr_vfork = 66;
  { 67 is obsolete vread  }
  { 68 is obsolete vwrite  }
     sys_nr_sbrk = 69;
     sys_nr_sstk = 70;
  { 71 is old mmap  }
  { 72 is obsolete vadvise  }
     sys_nr_munmap = 73;
     sys_nr_mprotect = 74;
     sys_nr_madvise = 75;
  { 76 is obsolete vhangup  }
  { 77 is obsolete vlimit  }
     sys_nr_mincore = 78;
     sys_nr_getgroups = 79;
     sys_nr_setgroups = 80;
     sys_nr_getpgrp = 81;
     sys_nr_setpgid = 82;
     sys_nr_setitimer = 83;
  { 84 is old wait  }
     sys_nr_swapon = 85;
     sys_nr_getitimer = 86;
  { 87 is old gethostname  }
  { 88 is old sethostname  }
     sys_nr_getdtablesize = 89;
     sys_nr_dup2 = 90;
     sys_nr_fcntl = 92;
     sys_nr_select = 93;
  { 94 is obsolete setdopt  }
     sys_nr_fsync = 95;
     sys_nr_setpriority = 96;
     sys_nr_socket = 97;
     sys_nr_connect = 98;
  { 99 is old accept  }
     sys_nr_getpriority = 100;
  { 101 is old send  }
  { 102 is old recv  }
     sys_nr_sigreturn = 103;
     sys_nr_bind = 104;
     sys_nr_setsockopt = 105;
     sys_nr_listen = 106;
  { 107 is obsolete vtimes  }
  { 108 is old sigvec  }
  { 109 is old sigblock  }
  { 110 is old sigsetmask  }
     sys_nr_sigsuspend = 111;
  { 112 is old sigstack  }
  { 113 is old recvmsg  }
  { 114 is old sendmsg  }
  { 115 is obsolete vtrace  }
     sys_nr_gettimeofday = 116;
     sys_nr_getrusage = 117;
     sys_nr_getsockopt = 118;
  { 119 is obsolete resuba  }
     sys_nr_readv = 120;
     sys_nr_writev = 121;
     sys_nr_settimeofday = 122;
     sys_nr_fchown = 123;
     sys_nr_fchmod = 124;
  { 125 is old recvfrom  }
  { 126 is old setreuid  }
  { 127 is old setregid  }
     sys_nr_rename = 128;
  { 129 is old truncate  }
  { 130 is old ftruncate  }
     sys_nr_flock = 131;
     sys_nr_mkfifo = 132;
     sys_nr_sendto = 133;
     sys_nr_shutdown = 134;
     sys_nr_socketpair = 135;
     sys_nr_mkdir = 136;
     sys_nr_rmdir = 137;
     sys_nr_utimes = 138;
  { 139 is unused  }
     sys_nr_adjtime = 140;
  { 141 is old getpeername  }
  { 142 is old gethostid  }
  { 143 is old sethostid  }
  { 144 is old getrlimit  }
  { 145 is old setrlimit  }
  { 146 is old killpg  }
     sys_nr_setsid = 147;
  { 148 is obsolete setquota  }
  { 149 is obsolete quota  }
  { 150 is old getsockname  }
  { 151 is reserved  }
     sys_nr_setprivexec = 152;
  { 153 is reserved  }
  { 154 is reserved  }
     sys_nr_nfssvc = 155;
  { 156 is old getdirentries  }
     sys_nr_statfs = 157;
     sys_nr_fstatfs = 158;
     sys_nr_unmount = 159;
  { 160 is obsolete async_daemon  }
     sys_nr_getfh = 161;
  { 162 is old getdomainname  }
  { 163 is old setdomainname  }
  { 164 is obsolete pcfs_mount  }
     sys_nr_quotactl = 165;
  { 166 is obsolete exportfs     }
     sys_nr_mount = 167;
  { 168 is obsolete ustat  }
  { 169 is unused  }
     sys_nr_table = 170;
  { 171 is old wait_3  }
  { 172 is obsolete rpause  }
  { 173 is unused  }
  { 174 is obsolete getdents  }
     sys_nr_gc_control = 175;
     sys_nr_add_profil = 176;
  { 177 is unused  }
  { 178 is unused  }
  { 179 is unused  }
     sys_nr_kdebug_trace = 180;
     sys_nr_setgid = 181;
     sys_nr_setegid = 182;
     sys_nr_seteuid = 183;
     sys_nr_lfs_bmapv = 184;
     sys_nr_lfs_markv = 185;
     sys_nr_lfs_segclean = 186;
     sys_nr_lfs_segwait = 187;
     sys_nr_stat = 188;
     sys_nr_fstat = 189;
     sys_nr_lstat = 190;
     sys_nr_pathconf = 191;
     sys_nr_fpathconf = 192;
{$if COMPAT_GETFSSTAT}

  const
     sys_nr_getfsstat = 193;
{$endif}

  const
     sys_nr_getrlimit = 194;
     sys_nr_setrlimit = 195;
     sys_nr_getdirentries = 196;
     sys_nr_mmap = 197;
     sys_nr___syscall = 198;
     sys_nr_lseek = 199;
     sys_nr_truncate = 200;
     sys_nr_ftruncate = 201;
     sys_nr___sysctl = 202;
     sys_nr_mlock = 203;
     sys_nr_munlock = 204;
     sys_nr_undelete = 205;
     sys_nr_ATsocket = 206;
     sys_nr_ATgetmsg = 207;
     sys_nr_ATputmsg = 208;
     sys_nr_ATPsndreq = 209;
     sys_nr_ATPsndrsp = 210;
     sys_nr_ATPgetreq = 211;
     sys_nr_ATPgetrsp = 212;
  { 213-215 are reserved for AppleTalk  }
     sys_nr_mkcomplex = 216;
     sys_nr_statv = 217;
     sys_nr_lstatv = 218;
     sys_nr_fstatv = 219;
     sys_nr_getattrlist = 220;
     sys_nr_setattrlist = 221;
     sys_nr_getdirentriesattr = 222;
     sys_nr_exchangedata = 223;
     sys_nr_checkuseraccess = 224;
     sys_nr_searchfs = 225;
  { 226 - 230 are reserved for HFS expansion  }
  { 231 - 249 are reserved   }
     sys_nr_minherit = 250;
     sys_nr_semsys = 251;
     sys_nr_msgsys = 252;
     sys_nr_shmsys = 253;
     sys_nr_semctl = 254;
     sys_nr_semget = 255;
     sys_nr_semop = 256;
     sys_nr_semconfig = 257;
     sys_nr_msgctl = 258;
     sys_nr_msgget = 259;
     sys_nr_msgsnd = 260;
     sys_nr_msgrcv = 261;
     sys_nr_shmat = 262;
     sys_nr_shmctl = 263;
     sys_nr_shmdt = 264;
     sys_nr_shmget = 265;
     sys_nr_shm_open = 266;
     sys_nr_shm_unlink = 267;
     sys_nr_sem_open = 268;
     sys_nr_sem_close = 269;
     sys_nr_sem_unlink = 270;
     sys_nr_sem_wait = 271;
     sys_nr_sem_trywait = 272;
     sys_nr_sem_post = 273;
     sys_nr_sem_getvalue = 274;
     sys_nr_sem_init = 275;
     sys_nr_sem_destroy = 276;
  { 277 - 295 are reserved   }
     sys_nr_load_shared_file = 296;
     sys_nr_reset_shared_file = 297;
  { 298 - 323 are reserved   }
v     sys_nr_mlockall = 324;
     sys_nr_munlockall = 325;
  { 326 is reserved  }
     sys_nr_issetugid = 327;

{
  $Log$
  Revision 1.3  2002-09-07 16:01:17  peter
    * old logs removed and tabs fixed

  Revision 1.2  2002/09/06 18:35:59  jonas
    * implemented most syscalls, except readdir because the getdents
      syscall is declared obsolete in Darwin...

  Revision 1.1  2002/09/06 17:12:55  jonas
    + initial implementation, h2pas of /usr/include/sys/syscall.h on a
      Mac OS X 10.1.5 machine

}
