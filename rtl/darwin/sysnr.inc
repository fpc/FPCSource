{$PACKRECORDS C}

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
     SYS_syscall = 0;
     SYS_exit = 1;
     SYS_fork = 2;
     SYS_read = 3;
     SYS_write = 4;
     SYS_open = 5;
     SYS_close = 6;
     SYS_wait4 = 7;
  { 8 is old creat  }
     SYS_link = 9;
     SYS_unlink = 10;
  { 11 is obsolete execv  }
     SYS_chdir = 12;
     SYS_fchdir = 13;
     SYS_mknod = 14;
     SYS_chmod = 15;
     SYS_chown = 16;
  { 17 is obsolete sbreak  }
{$if COMPAT_GETFSSTAT}
  { 18 is old getfsstat  }
{$else}

  const
     SYS_getfsstat = 18;
{$endif}
  { 19 is old lseek  }

  const
     SYS_getpid = 20;
  { 21 is obsolete mount  }
  { 22 is obsolete umount  }
     SYS_setuid = 23;
     SYS_getuid = 24;
     SYS_geteuid = 25;
     SYS_ptrace = 26;
     SYS_recvmsg = 27;
     SYS_sendmsg = 28;
     SYS_recvfrom = 29;
     SYS_accept = 30;
     SYS_getpeername = 31;
     SYS_getsockname = 32;
     SYS_access = 33;
     SYS_chflags = 34;
     SYS_fchflags = 35;
     SYS_sync = 36;
     SYS_kill = 37;
  { 38 is old stat  }
     SYS_getppid = 39;
  { 40 is old lstat  }
     SYS_dup = 41;
     SYS_pipe = 42;
     SYS_getegid = 43;
     SYS_profil = 44;
     SYS_ktrace = 45;
     SYS_sigaction = 46;
     SYS_getgid = 47;
     SYS_sigprocmask = 48;
     SYS_getlogin = 49;
     SYS_setlogin = 50;
     SYS_acct = 51;
     SYS_sigpending = 52;
     SYS_sigaltstack = 53;
     SYS_ioctl = 54;
     SYS_reboot = 55;
     SYS_revoke = 56;
     SYS_symlink = 57;
     SYS_readlink = 58;
     SYS_execve = 59;
     SYS_umask = 60;
     SYS_chroot = 61;
  { 62 is old fstat  }
  { 63 is unused  }
  { 64 is old getpagesize  }
     SYS_msync = 65;
     SYS_vfork = 66;
  { 67 is obsolete vread  }
  { 68 is obsolete vwrite  }
     SYS_sbrk = 69;
     SYS_sstk = 70;
  { 71 is old mmap  }
  { 72 is obsolete vadvise  }
     SYS_munmap = 73;
     SYS_mprotect = 74;
     SYS_madvise = 75;
  { 76 is obsolete vhangup  }
  { 77 is obsolete vlimit  }
     SYS_mincore = 78;
     SYS_getgroups = 79;
     SYS_setgroups = 80;
     SYS_getpgrp = 81;
     SYS_setpgid = 82;
     SYS_setitimer = 83;
  { 84 is old wait  }
     SYS_swapon = 85;
     SYS_getitimer = 86;
  { 87 is old gethostname  }
  { 88 is old sethostname  }
     SYS_getdtablesize = 89;
     SYS_dup2 = 90;
     SYS_fcntl = 92;
     SYS_select = 93;
  { 94 is obsolete setdopt  }
     SYS_fsync = 95;
     SYS_setpriority = 96;
     SYS_socket = 97;
     SYS_connect = 98;
  { 99 is old accept  }
     SYS_getpriority = 100;
  { 101 is old send  }
  { 102 is old recv  }
     SYS_sigreturn = 103;
     SYS_bind = 104;
     SYS_setsockopt = 105;
     SYS_listen = 106;
  { 107 is obsolete vtimes  }
  { 108 is old sigvec  }
  { 109 is old sigblock  }
  { 110 is old sigsetmask  }
     SYS_sigsuspend = 111;
  { 112 is old sigstack  }
  { 113 is old recvmsg  }
  { 114 is old sendmsg  }
  { 115 is obsolete vtrace  }
     SYS_gettimeofday = 116;
     SYS_getrusage = 117;
     SYS_getsockopt = 118;
  { 119 is obsolete resuba  }
     SYS_readv = 120;
     SYS_writev = 121;
     SYS_settimeofday = 122;
     SYS_fchown = 123;
     SYS_fchmod = 124;
  { 125 is old recvfrom  }
  { 126 is old setreuid  }
  { 127 is old setregid  }
     SYS_rename = 128;
  { 129 is old truncate  }
  { 130 is old ftruncate  }
     SYS_flock = 131;
     SYS_mkfifo = 132;
     SYS_sendto = 133;
     SYS_shutdown = 134;
     SYS_socketpair = 135;
     SYS_mkdir = 136;
     SYS_rmdir = 137;
     SYS_utimes = 138;
  { 139 is unused  }
     SYS_adjtime = 140;
  { 141 is old getpeername  }
  { 142 is old gethostid  }
  { 143 is old sethostid  }
  { 144 is old getrlimit  }
  { 145 is old setrlimit  }
  { 146 is old killpg  }
     SYS_setsid = 147;
  { 148 is obsolete setquota  }
  { 149 is obsolete quota  }
  { 150 is old getsockname  }
  { 151 is reserved  }
     SYS_setprivexec = 152;
  { 153 is reserved  }
  { 154 is reserved  }
     SYS_nfssvc = 155;
  { 156 is old getdirentries  }
     SYS_statfs = 157;
     SYS_fstatfs = 158;
     SYS_unmount = 159;
  { 160 is obsolete async_daemon  }
     SYS_getfh = 161;
  { 162 is old getdomainname  }
  { 163 is old setdomainname  }
  { 164 is obsolete pcfs_mount  }
     SYS_quotactl = 165;
  { 166 is obsolete exportfs	 }
     SYS_mount = 167;
  { 168 is obsolete ustat  }
  { 169 is unused  }
     SYS_table = 170;
  { 171 is old wait_3  }
  { 172 is obsolete rpause  }
  { 173 is unused  }
  { 174 is obsolete getdents  }
     SYS_gc_control = 175;
     SYS_add_profil = 176;
  { 177 is unused  }
  { 178 is unused  }
  { 179 is unused  }
     SYS_kdebug_trace = 180;
     SYS_setgid = 181;
     SYS_setegid = 182;
     SYS_seteuid = 183;
     SYS_lfs_bmapv = 184;
     SYS_lfs_markv = 185;
     SYS_lfs_segclean = 186;
     SYS_lfs_segwait = 187;
     SYS_stat = 188;
     SYS_fstat = 189;
     SYS_lstat = 190;
     SYS_pathconf = 191;
     SYS_fpathconf = 192;
{$if COMPAT_GETFSSTAT}

  const
     SYS_getfsstat = 193;
{$endif}

  const
     SYS_getrlimit = 194;
     SYS_setrlimit = 195;
     SYS_getdirentries = 196;
     SYS_mmap = 197;
     SYS___syscall = 198;
     SYS_lseek = 199;
     SYS_truncate = 200;
     SYS_ftruncate = 201;
     SYS___sysctl = 202;
     SYS_mlock = 203;
     SYS_munlock = 204;
     SYS_undelete = 205;
     SYS_ATsocket = 206;
     SYS_ATgetmsg = 207;
     SYS_ATputmsg = 208;
     SYS_ATPsndreq = 209;
     SYS_ATPsndrsp = 210;
     SYS_ATPgetreq = 211;
     SYS_ATPgetrsp = 212;
  { 213-215 are reserved for AppleTalk  }
     SYS_mkcomplex = 216;
     SYS_statv = 217;
     SYS_lstatv = 218;
     SYS_fstatv = 219;
     SYS_getattrlist = 220;
     SYS_setattrlist = 221;
     SYS_getdirentriesattr = 222;
     SYS_exchangedata = 223;
     SYS_checkuseraccess = 224;
     SYS_searchfs = 225;
  { 226 - 230 are reserved for HFS expansion  }
  { 231 - 249 are reserved   }
     SYS_minherit = 250;
     SYS_semsys = 251;
     SYS_msgsys = 252;
     SYS_shmsys = 253;
     SYS_semctl = 254;
     SYS_semget = 255;
     SYS_semop = 256;
     SYS_semconfig = 257;
     SYS_msgctl = 258;
     SYS_msgget = 259;
     SYS_msgsnd = 260;
     SYS_msgrcv = 261;
     SYS_shmat = 262;
     SYS_shmctl = 263;
     SYS_shmdt = 264;
     SYS_shmget = 265;
     SYS_shm_open = 266;
     SYS_shm_unlink = 267;
     SYS_sem_open = 268;
     SYS_sem_close = 269;
     SYS_sem_unlink = 270;
     SYS_sem_wait = 271;
     SYS_sem_trywait = 272;
     SYS_sem_post = 273;
     SYS_sem_getvalue = 274;
     SYS_sem_init = 275;
     SYS_sem_destroy = 276;
  { 277 - 295 are reserved   }
     SYS_load_shared_file = 296;
     SYS_reset_shared_file = 297;
  { 298 - 323 are reserved   }
     SYS_mlockall = 324;
     SYS_munlockall = 325;
  { 326 is reserved  }
     SYS_issetugid = 327;

{
  $Log$
  Revision 1.1  2002-09-06 17:12:55  jonas
    + initial implementation, h2pas of /usr/include/sys/syscall.h on a
      Mac OS X 10.1.5 machine

}
