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

{ include Mach trap numbers }
{$i machnr.inc}

  const
     syscall_nr_syscall = 0;
     syscall_nr_exit = 1;
     syscall_nr_fork = 2;
     syscall_nr_read = 3;
     syscall_nr_write = 4;
     syscall_nr_open = 5;
     syscall_nr_close = 6;
     syscall_nr_wait4 = 7;
  { 8 is old creat  }
     syscall_nr_link = 9;
     syscall_nr_unlink = 10;
  { 11 is obsolete execv  }
     syscall_nr_chdir = 12;
     syscall_nr_fchdir = 13;
     syscall_nr_mknod = 14;
     syscall_nr_chmod = 15;
     syscall_nr_chown = 16;
  { 17 is obsolete sbreak  }
{$if COMPAT_GETFSSTAT}
  { 18 is old getfsstat  }
{$else}

  const
     syscall_nr_getfsstat = 18;
{$endif}
  { 19 is old lseek  }

  const
     syscall_nr_getpid = 20;
  { 21 is obsolete mount  }
  { 22 is obsolete umount  }
     syscall_nr_setuid = 23;
     syscall_nr_getuid = 24;
     syscall_nr_geteuid = 25;
     syscall_nr_ptrace = 26;
     syscall_nr_recvmsg = 27;
     syscall_nr_sendmsg = 28;
     syscall_nr_recvfrom = 29;
     syscall_nr_accept = 30;
     syscall_nr_getpeername = 31;
     syscall_nr_getsockname = 32;
     syscall_nr_access = 33;
     syscall_nr_chflags = 34;
     syscall_nr_fchflags = 35;
     syscall_nr_sync = 36;
     syscall_nr_kill = 37;
  { 38 is old stat  }
     syscall_nr_getppid = 39;
  { 40 is old lstat  }
     syscall_nr_dup = 41;
     syscall_nr_pipe = 42;
     syscall_nr_getegid = 43;
     syscall_nr_profil = 44;
     syscall_nr_ktrace = 45;
     syscall_nr_sigaction = 46;
     syscall_nr_getgid = 47;
     syscall_nr_sigprocmask = 48;
     syscall_nr_getlogin = 49;
     syscall_nr_setlogin = 50;
     syscall_nr_acct = 51;
     syscall_nr_sigpending = 52;
     syscall_nr_sigaltstack = 53;
     syscall_nr_ioctl = 54;
     syscall_nr_reboot = 55;
     syscall_nr_revoke = 56;
     syscall_nr_symlink = 57;
     syscall_nr_readlink = 58;
     syscall_nr_execve = 59;
     syscall_nr_umask = 60;
     syscall_nr_chroot = 61;
  { 62 is old fstat  }
  { 63 is unused  }
  { 64 is old getpagesize  }
     syscall_nr_msync = 65;
     syscall_nr_vfork = 66;
  { 67 is obsolete vread  }
  { 68 is obsolete vwrite  }
     syscall_nr_sbrk = 69;
     syscall_nr_sstk = 70;
  { 71 is old mmap  }
  { 72 is obsolete vadvise  }
     syscall_nr_munmap = 73;
     syscall_nr_mprotect = 74;
     syscall_nr_madvise = 75;
  { 76 is obsolete vhangup  }
  { 77 is obsolete vlimit  }
     syscall_nr_mincore = 78;
     syscall_nr_getgroups = 79;
     syscall_nr_setgroups = 80;
     syscall_nr_getpgrp = 81;
     syscall_nr_setpgid = 82;
     syscall_nr_setitimer = 83;
  { 84 is old wait  }
     syscall_nr_swapon = 85;
     syscall_nr_getitimer = 86;
  { 87 is old gethostname  }
  { 88 is old sethostname  }
     syscall_nr_getdtablesize = 89;
     syscall_nr_dup2 = 90;
     syscall_nr_fcntl = 92;
     syscall_nr_select = 93;
  { 94 is obsolete setdopt  }
     syscall_nr_fsync = 95;
     syscall_nr_setpriority = 96;
     syscall_nr_socket = 97;
     syscall_nr_connect = 98;
  { 99 is old accept  }
     syscall_nr_getpriority = 100;
  { 101 is old send  }
  { 102 is old recv  }
     syscall_nr_sigreturn = 103;
     syscall_nr_bind = 104;
     syscall_nr_setsockopt = 105;
     syscall_nr_listen = 106;
  { 107 is obsolete vtimes  }
  { 108 is old sigvec  }
  { 109 is old sigblock  }
  { 110 is old sigsetmask  }
     syscall_nr_sigsuspend = 111;
  { 112 is old sigstack  }
  { 113 is old recvmsg  }
  { 114 is old sendmsg  }
  { 115 is obsolete vtrace  }
     syscall_nr_gettimeofday = 116;
     syscall_nr_getrusage = 117;
     syscall_nr_getsockopt = 118;
  { 119 is obsolete resuba  }
     syscall_nr_readv = 120;
     syscall_nr_writev = 121;
     syscall_nr_settimeofday = 122;
     syscall_nr_fchown = 123;
     syscall_nr_fchmod = 124;
  { 125 is old recvfrom  }
  { 126 is old setreuid  }
  { 127 is old setregid  }
     syscall_nr_rename = 128;
  { 129 is old truncate  }
  { 130 is old ftruncate  }
     syscall_nr_flock = 131;
     syscall_nr_mkfifo = 132;
     syscall_nr_sendto = 133;
     syscall_nr_shutdown = 134;
     syscall_nr_socketpair = 135;
     syscall_nr_mkdir = 136;
     syscall_nr_rmdir = 137;
     syscall_nr_utimes = 138;
  { 139 is unused  }
     syscall_nr_adjtime = 140;
  { 141 is old getpeername  }
  { 142 is old gethostid  }
  { 143 is old sethostid  }
  { 144 is old getrlimit  }
  { 145 is old setrlimit  }
  { 146 is old killpg  }
     syscall_nr_setsid = 147;
  { 148 is obsolete setquota  }
  { 149 is obsolete quota  }
  { 150 is old getsockname  }
  { 151 is reserved  }
     syscall_nr_setprivexec = 152;
  { 153 is reserved  }
  { 154 is reserved  }
     syscall_nr_nfssvc = 155;
  { 156 is old getdirentries  }
     syscall_nr_statfs = 157;
     syscall_nr_fstatfs = 158;
     syscall_nr_unmount = 159;
  { 160 is obsolete async_daemon  }
     syscall_nr_getfh = 161;
  { 162 is old getdomainname  }
  { 163 is old setdomainname  }
  { 164 is obsolete pcfs_mount  }
     syscall_nr_quotactl = 165;
  { 166 is obsolete exportfs     }
     syscall_nr_mount = 167;
  { 168 is obsolete ustat  }
  { 169 is unused  }
     syscall_nr_table = 170;
  { 171 is old wait_3  }
  { 172 is obsolete rpause  }
  { 173 is unused  }
  { 174 is obsolete getdents  }
     syscall_nr_gc_control = 175;
     syscall_nr_add_profil = 176;
  { 177 is unused  }
  { 178 is unused  }
  { 179 is unused  }
     syscall_nr_kdebug_trace = 180;
     syscall_nr_setgid = 181;
     syscall_nr_setegid = 182;
     syscall_nr_seteuid = 183;
     syscall_nr_lfs_bmapv = 184;
     syscall_nr_lfs_markv = 185;
     syscall_nr_lfs_segclean = 186;
     syscall_nr_lfs_segwait = 187;
     syscall_nr_stat = 188;
     syscall_nr_fstat = 189;
     syscall_nr_lstat = 190;
     syscall_nr_pathconf = 191;
     syscall_nr_fpathconf = 192;
{$if COMPAT_GETFSSTAT}

  const
     syscall_nr_getfsstat = 193;
{$endif}

  const
     syscall_nr_getrlimit = 194;
     syscall_nr_setrlimit = 195;
     syscall_nr_getdirentries = 196;
     syscall_nr_mmap = 197;
     syscall_nr___syscall = 198;
     syscall_nr_lseek = 199;
     syscall_nr_truncate = 200;
     syscall_nr_ftruncate = 201;
     syscall_nr___sysctl = 202;
     syscall_nr_mlock = 203;
     syscall_nr_munlock = 204;
     syscall_nr_undelete = 205;
     syscall_nr_ATsocket = 206;
     syscall_nr_ATgetmsg = 207;
     syscall_nr_ATputmsg = 208;
     syscall_nr_ATPsndreq = 209;
     syscall_nr_ATPsndrsp = 210;
     syscall_nr_ATPgetreq = 211;
     syscall_nr_ATPgetrsp = 212;
  { 213-215 are reserved for AppleTalk  }
     syscall_nr_mkcomplex = 216;
     syscall_nr_statv = 217;
     syscall_nr_lstatv = 218;
     syscall_nr_fstatv = 219;
     syscall_nr_getattrlist = 220;
     syscall_nr_setattrlist = 221;
     syscall_nr_getdirentriesattr = 222;
     syscall_nr_exchangedata = 223;
     syscall_nr_checkuseraccess = 224;
     syscall_nr_searchfs = 225;
  { 226 - 230 are reserved for HFS expansion  }
  { 231 - 249 are reserved   }
     syscall_nr_minherit = 250;
     syscall_nr_semsys = 251;
     syscall_nr_msgsys = 252;
     syscall_nr_shmsys = 253;
     syscall_nr_semctl = 254;
     syscall_nr_semget = 255;
     syscall_nr_semop = 256;
     syscall_nr_semconfig = 257;
     syscall_nr_msgctl = 258;
     syscall_nr_msgget = 259;
     syscall_nr_msgsnd = 260;
     syscall_nr_msgrcv = 261;
     syscall_nr_shmat = 262;
     syscall_nr_shmctl = 263;
     syscall_nr_shmdt = 264;
     syscall_nr_shmget = 265;
     syscall_nr_shm_open = 266;
     syscall_nr_shm_unlink = 267;
     syscall_nr_sem_open = 268;
     syscall_nr_sem_close = 269;
     syscall_nr_sem_unlink = 270;
     syscall_nr_sem_wait = 271;
     syscall_nr_sem_trywait = 272;
     syscall_nr_sem_post = 273;
     syscall_nr_sem_getvalue = 274;
     syscall_nr_sem_init = 275;
     syscall_nr_sem_destroy = 276;
  { 277 - 295 are reserved   }
     syscall_nr_load_shared_file = 296;
     syscall_nr_reset_shared_file = 297;
  { 298 - 323 are reserved   }
     syscall_nr_mlockall = 324;
     syscall_nr_munlockall = 325;
  { 326 is reserved  }
     syscall_nr_issetugid = 327;

{
  $Log$
  Revision 1.4  2003-05-25 12:59:57  jonas
    * several fixes, addition of Mach trap numbers (thye are simply syscalls
      with a negative number)

  Revision 1.3  2002/09/07 16:01:17  peter
    * old logs removed and tabs fixed

  Revision 1.2  2002/09/06 18:35:59  jonas
    * implemented most syscalls, except readdir because the getdents
      syscall is declared obsolete in Darwin...

  Revision 1.1  2002/09/06 17:12:55  jonas
    + initial implementation, h2pas of /usr/include/sys/syscall.h on a
      Mac OS X 10.1.5 machine

}
