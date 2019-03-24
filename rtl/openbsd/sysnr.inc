{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2010 Jasper Lievisse Adriaanse
    Copyright (c) 2004 Marco van de Voort
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
 * System call numbers.
 *
 * created from; OpenBSD: syscalls.master,v 1.104 2010/10/28 15:02:41 millert Exp
 *  (OpenBSD/i386 4.9 release)

  Checked with syscall.h release tags from 2.0 to 6.4 2018/07/30

}

Const
(* 0 *)    syscall_nr_syscall = 0;
(* 1 *)    syscall_nr_exit = 1;
(* 2 *)    syscall_nr_fork = 2;
(* 3 *)    syscall_nr_read = 3;
(* 4 *)    syscall_nr_write = 4;
(* 5 *)    syscall_nr_open = 5;
(* 6 *)    syscall_nr_close = 6;
(* 7 *)    syscall_nr_getentropy = 7; (* from 5.6 release *)
(* 8 *)    syscall_nr___tfork = 8; (* from 5.2 release *)
(* 9 *)    syscall_nr_link = 9;
(* 10 *)    syscall_nr_unlink = 10;
(* 11 *)    syscall_nr_wait4 = 11; (* from 5.5 release *)
(* 12 *)    syscall_nr_chdir = 12;
(* 13 *)    syscall_nr_fchdir = 13;
(* 14 *)    syscall_nr_mknod = 14;
(* 15 *)    syscall_nr_chmod = 15;
(* 16 *)    syscall_nr_chown = 16;
(* 17 *)    syscall_nr_break = 17;
(* 18 *)    syscall_nr_getdtablecount = 18; (* from 5.2 release *)
(* 19 *)    syscall_nr_getrusage = 19; (* from 5.5 release *)
(* 20 *)    syscall_nr_getpid = 20;
(* 21 *)    syscall_nr_mount = 21;
(* 22 *)    syscall_nr_unmount = 22;
(* 23 *)    syscall_nr_setuid = 23;
(* 24 *)    syscall_nr_getuid = 24;
(* 25 *)    syscall_nr_geteuid = 25;
(* 26 *)    syscall_nr_ptrace = 26;
(* 27 *)    syscall_nr_recvmsg = 27;
(* 28 *)    syscall_nr_sendmsg = 28;
(* 29 *)    syscall_nr_recvfrom = 29;
(* 30 *)    syscall_nr_accept = 30;
(* 31 *)    syscall_nr_getpeername = 31;
(* 32 *)    syscall_nr_getsockname = 32;
(* 33 *)    syscall_nr_access = 33;
(* 34 *)    syscall_nr_chflags = 34;
(* 35 *)    syscall_nr_fchflags = 35;
(* 36 *)    syscall_nr_sync = 36;
(* 38 *)    syscall_nr_stat = 38; (* from 5.5 release *)
(* 39 *)    syscall_nr_getppid = 39;
(* 40 *)    syscall_nr_lstat = 40; (* from 5.5 release *)
(* 41 *)    syscall_nr_dup = 41;
(* 42 *)    syscall_nr_fstatat = 42; (* from 5.5 release *)
(* 43 *)    syscall_nr_getegid = 43;
(* 44 *)    syscall_nr_profil = 44;
(* 45 *)    syscall_nr_ktrace = 45;
(* 46 *)    syscall_nr_sigaction = 46;
(* 47 *)    syscall_nr_getgid = 47;
(* 48 *)    syscall_nr_sigprocmask = 48;
(* 50 *)    syscall_nr_setlogin = 50;
(* 51 *)    syscall_nr_acct = 51;
(* 52 *)    syscall_nr_sigpending = 52;
(* 53 *)    syscall_nr_fstat = 53; (* from 5.5 release *)
(* 54 *)    syscall_nr_ioctl = 54;
(* 55 *)    syscall_nr_reboot = 55;
(* 56 *)    syscall_nr_revoke = 56;
(* 57 *)    syscall_nr_symlink = 57;
(* 58 *)    syscall_nr_readlink = 58;
(* 59 *)    syscall_nr_execve = 59;
(* 60 *)    syscall_nr_umask = 60;
(* 61 *)    syscall_nr_chroot = 61;
(* 62 *)    syscall_nr_getfsstat = 62; (* from 5.4 release *)
(* 63 *)    syscall_nr_statfs = 63; (* from 5.4 release *)
(* 64 *)    syscall_nr_fstatfs = 64; (* from 5.4 release *)
(* 65 *)    syscall_nr_fhstatfs = 65; (* from 5.4 release *)
(* 66 *)    syscall_nr_vfork = 66;
(* 67 *)    syscall_nr_gettimeofday = 67; (* from 5.5 release *)
(* 68 *)    syscall_nr_settimeofday = 68; (* from 5.5 release *)
(* 69 *)    syscall_nr_setitimer = 69; (* from 5.5 release *)
(* 70 *)    syscall_nr_getitimer = 70; (* from 5.5 release *)
(* 71 *)    syscall_nr_select = 71; (* from 5.5 release *)
(* 72 *)    syscall_nr_kevent = 72; (* from 5.5 release *)
(* 73 *)    syscall_nr_munmap = 73;
(* 74 *)    syscall_nr_mprotect = 74;
(* 75 *)    syscall_nr_madvise = 75;
(* 76 *)    syscall_nr_utimes = 76; (* from 5.5 release *)
(* 77 *)    syscall_nr_futimes = 77; (* from 5.5 release *)
(* 78 *)    syscall_nr_mincore = 78;
(* 79 *)    syscall_nr_getgroups = 79;
(* 80 *)    syscall_nr_setgroups = 80;
(* 81 *)    syscall_nr_getpgrp = 81;
(* 82 *)    syscall_nr_setpgid = 82;
(* 83 *)    syscall_nr_futex = 83; (* from 6.2 release *)
(* 84 *)    syscall_nr_utimensat = 84; (* from 5.5 release *)
(* 85 *)    syscall_nr_futimens = 85; (* from  5.5 release *)
(* 86 *)    syscall_nr_kbind = 86; (* from 5.8 release *)
(* 87 *)    syscall_nr_clock_gettime = 87; (* from  5.5 release *)
(* 88 *)    syscall_nr_clock_settime = 88; (* from  5.5 release *)
(* 89 *)    syscall_nr_clock_getres = 89; (* from  5.5 release *)
(* 90 *)    syscall_nr_dup2 = 90;
(* 91 *)    syscall_nr_nanosleep = 91; (* from  5.5 release *)
(* 92 *)    syscall_nr_fcntl = 92;
(* 93 *)    syscall_nr_accept4 = 93;  (* from 5.7 release *)
(* 94 *)    syscall_nr___thrsleep = 94; (* from 5.5 release *)
(* 95 *)    syscall_nr_fsync = 95;
(* 96 *)    syscall_nr_setpriority = 96;
(* 97 *)    syscall_nr_socket = 97;
(* 98 *)    syscall_nr_connect = 98;
(* 99 *)    syscall_nr_getdents = 99;
(* 100 *)    syscall_nr_getpriority = 100;
(* 101 *)    syscall_nr_pipe2 = 101; (* from 5.7 release *)
(* 102 *)    syscall_nr_dup3 = 102; (* from 5.7 release *)
(* 103 *)    syscall_nr_sigreturn = 103;
(* 104 *)    syscall_nr_bind = 104;
(* 105 *)    syscall_nr_setsockopt = 105;
(* 106 *)    syscall_nr_listen = 106;
(* 107 *)    syscall_nr_chflagsat = 107; (* from 5.7 release *)
(* 108 *)    syscall_nr_pledge = 108; (* from 5.9 release *)
(* 109 *)    syscall_nr_ppoll = 109;
(* 110 *)    syscall_nr_pselect = 110;
(* 111 *)    syscall_nr_sigsuspend = 111;
(* 112 *)    syscall_nr_sendsyslog = 112; (* from 6.0 release *)
(* 113 *)    syscall_nr_fktrace = 113; (* from 6.2 release *)
(* 114 *)    syscall_nr_unveil = 114; (* from 6.4 release *)
(* 118 *)    syscall_nr_getsockopt = 118;
(* 119 *)    syscall_nr_thrkill = 119; (* from 5.9 release *)
(* 120 *)    syscall_nr_readv = 120;
(* 121 *)    syscall_nr_writev = 121;
(* 122 *)    syscall_nr_kill = 122; (* from 5.9 release *)
(* 123 *)    syscall_nr_fchown = 123;
(* 124 *)    syscall_nr_fchmod = 124;
(* 126 *)    syscall_nr_setreuid = 126;
(* 127 *)    syscall_nr_setregid = 127;
(* 128 *)    syscall_nr_rename = 128;
(* 131 *)    syscall_nr_flock = 131;
(* 132 *)    syscall_nr_mkfifo = 132;
(* 133 *)    syscall_nr_sendto = 133;
(* 134 *)    syscall_nr_shutdown = 134;
(* 135 *)    syscall_nr_socketpair = 135;
(* 136 *)    syscall_nr_mkdir = 136;
(* 137 *)    syscall_nr_rmdir = 137;
(* 140 *)    syscall_nr_adjtime = 140;
(* 141 *)    syscall_nr_getlogin_r = 141; (* from 6.0 release *)
(* 147 *)    syscall_nr_setsid = 147;
(* 148 *)    syscall_nr_quotactl = 148;
(* 155 *)    syscall_nr_nfssvc = 155;
(* 161 *)    syscall_nr_getfh = 161;
(* 165 *)    syscall_nr_sysarch = 165;
(* 169 *)    syscall_nr_semsys = 169; (* old compat_10, but required for ipcbsd.inc rtl-extra compilation *)
(* 170 *)    syscall_nr_msgsys = 170; (* old compat_10, but required for ipcbsd.inc rtl-extra compilation *)
(* 171 *)    syscall_nr_shmsys = 171; (* old compat_10, but required for ipcbsd.inc rtl-extra compilation *)
(* 173 *)    syscall_nr_pread = 173;
(* 174 *)    syscall_nr_pwrite = 174;
(* 175 *)    syscall_nr_ntp_gettime = 175;
(* 176 *)    syscall_nr_ntp_adjtime = 176;
(* 181 *)    syscall_nr_setgid = 181;
(* 182 *)    syscall_nr_setegid = 182;
(* 183 *)    syscall_nr_seteuid = 183;
(* 191 *)    syscall_nr_pathconf = 191;
(* 192 *)    syscall_nr_fpathconf = 192;
(* 193 *)    syscall_nr_swapctl = 193;
(* 194 *)    syscall_nr_getrlimit = 194;
(* 195 *)    syscall_nr_setrlimit = 195;
(* 197 *)    syscall_nr_mmap = 197;
(* 198 *)    syscall_nr___syscall = 198;
(* 199 *)    syscall_nr_lseek = 199;
(* 200 *)    syscall_nr_truncate = 200;
(* 201 *)    syscall_nr_ftruncate = 201;
(* 202 *)    syscall_nr_sysctl = 202; (* from 5.9 release *)
(* 203 *)    syscall_nr_mlock = 203;
(* 204 *)    syscall_nr_munlock = 204;
(* 205 *)    syscall_nr_undelete = 205;
(* 207 *)    syscall_nr_getpgid = 207;
(* 209 *)    syscall_nr_utrace = 209;
(* 221 *)    syscall_nr_semget = 221;
(* 225 *)    syscall_nr_msgget = 225;
(* 226 *)    syscall_nr_msgsnd = 226;
(* 227 *)    syscall_nr_msgrcv = 227;
(* 228 *)    syscall_nr_shmat = 228;
(* 230 *)    syscall_nr_shmdt = 230;
(* 250 *)    syscall_nr_minherit = 250;
(* 252 *)    syscall_nr_poll = 252;
(* 253 *)    syscall_nr_issetugid = 253;
(* 254 *)    syscall_nr_lchown = 254;
(* 255 *)    syscall_nr_getsid = 255;
(* 256 *)    syscall_nr_msync = 256;
(* 263 *)    syscall_nr_pipe = 263; (* from 2.6 release *)
(* 264 *)    syscall_nr_fhopen = 264;
(* 267 *)    syscall_nr_preadv = 267;
(* 268 *)    syscall_nr_pwritev = 268;
(* 269 *)    syscall_nr_kqueue = 269;
(* 271 *)    syscall_nr_mlockall = 271;
(* 272 *)    syscall_nr_munlockall = 272;
(* 273 *)    syscall_nr_getpeereid = 273;
(* 274 *)    syscall_nr_extattrctl = 274;
(* 275 *)    syscall_nr_extattr_set_file = 275;
(* 276 *)    syscall_nr_extattr_get_file = 276;
(* 277 *)    syscall_nr_extattr_delete_file = 277;
(* 278 *)    syscall_nr_extattr_set_fd = 278;
(* 279 *)    syscall_nr_extattr_get_fd = 279;
(* 280 *)    syscall_nr_extattr_delete_fd = 280;
(* 281 *)    syscall_nr_getresuid = 281;
(* 282 *)    syscall_nr_setresuid = 282;
(* 283 *)    syscall_nr_getresgid = 283;
(* 284 *)    syscall_nr_setresgid = 284;
(* 286 *)    syscall_nr_mquery = 286;
(* 287 *)    syscall_nr_closefrom = 287;
(* 288 *)    syscall_nr_sigaltstack = 288; (* from 3.5 release *)
(* 289 *)    syscall_nr_shmget = 289; (* from 3.6 release *)
(* 290 *)    syscall_nr_semop = 290; (* from 3.6 release *)
(* 294 *)    syscall_nr_fhstat = 294; (* from 3.6 release *)
(* 295 *)    syscall_nr___semctl = 295; (* from 3.6 release *)
(* 296 *)    syscall_nr_shmctl = 296; (* from 3.6 release *)
(* 297 *)    syscall_nr_msgctl = 297; (* from 3.6 release *)
(* 298 *)    syscall_nr_sched_yield = 298;
(* 299 *)    syscall_nr_getthrid = 299;
(* 301 *)    syscall_nr___thrwakeup = 301; (* from 5.1 release *)
(* 302 *)    syscall_nr___threxit = 302; (* from 5.1 release *)
(* 303 *)    syscall_nr___thrsigdivert = 303; (* from 5.1 release *)
(* 304 *)    syscall_nr___getcwd = 304;
(* 305 *)    syscall_nr_adjfreq = 305;
(* 310 *)    syscall_nr_setrtable = 310; (* from 4.8 release *)
(* 311 *)    syscall_nr_getrtable = 311; (* from 4.8 release *)
(* 313 *)    syscall_nr_faccessat = 313;
(* 314 *)    syscall_nr_fchmodat = 314;
(* 315 *)    syscall_nr_fchownat = 315;
(* 317 *)    syscall_nr_linkat = 317;
(* 318 *)    syscall_nr_mkdirat = 318;
(* 319 *)    syscall_nr_mkfifoat = 319;
(* 320 *)    syscall_nr_mknodat = 320;
(* 321 *)    syscall_nr_openat = 321;
(* 322 *)    syscall_nr_readlinkat = 322;
(* 323 *)    syscall_nr_renameat = 323;
(* 324 *)    syscall_nr_symlinkat = 324;
(* 325 *)    syscall_nr_unlinkat = 325;
(* 329 *)    syscall_nr___set_tcb = 329;
(* 330 *)    syscall_nr___get_tcb = 330;


{ Aliases }
var
  syscall_nr_waitpid : longint = syscall_nr_wait4;  // 7, added: ease of notation purposes
  
{ The variables above need to be changed at startup }

{$define FPC_HAS_SETSYSNR_INC}

