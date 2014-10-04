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
}

{ Append 35 suffix to system call numbers
  for compatibility to 3.5 versions }

Const
        syscall_nr_syscall      = 0  ;
        syscall_nr_exit         = 1  ;
        syscall_nr_fork         = 2  ;
        syscall_nr_read         = 3  ;
        syscall_nr_write        = 4  ;
        syscall_nr_open         = 5  ;
        syscall_nr_close        = 6  ;
        { 8 is marked as compta 43 ocreat  }
        syscall_nr_link         = 9  ;
        syscall_nr_unlink       = 10 ;
        syscall_nr_wait4        = 11 ;
        syscall_nr_chdir        = 12 ;
        syscall_nr_fchdir       = 13 ;
        syscall_nr_mknod        = 14 ;
        syscall_nr_chmod        = 15 ;
        syscall_nr_chown        = 16 ;
        syscall_nr_break        = 17 ;
        syscall_nr_getrusage    = 19 ;
        syscall_nr_getpid       = 20 ;
        syscall_nr_mount        = 21 ;
        syscall_nr_unmount      = 22 ;
        syscall_nr_setuid       = 23 ;
        syscall_nr_getuid       = 24 ;
        syscall_nr_geteuid      = 25 ;
        syscall_nr_ptrace       = 26 ;
        syscall_nr_recvmsg      = 27 ;
        syscall_nr_sendmsg      = 28 ;
        syscall_nr_recvfrom     = 29 ;
        syscall_nr_accept       = 30 ;
        syscall_nr_getpeername  = 31 ;
        syscall_nr_getsockname  = 32 ;
        syscall_nr_access       = 33 ;
        syscall_nr_chflags      = 34 ;
        syscall_nr_fchflags     = 35 ;
        syscall_nr_sync         = 36 ;
        syscall_nr_kill         = 37 ;
        syscall_nr_stat         = 38 ;
        syscall_nr_getppid      = 39 ;
        syscall_nr_lstat        = 40 ;
        syscall_nr_dup          = 41 ;
        syscall_nr_fstatat      = 42 ;
        syscall_nr_getegid      = 43 ;
        syscall_nr_profil       = 44 ;
        syscall_nr_ktrace       = 45 ;
        syscall_nr_sigaction    = 46 ;
        syscall_nr_getgid       = 47 ;
        syscall_nr_sigprocmask  = 48 ;
        syscall_nr_getlogin     = 49 ;
        syscall_nr_setlogin     = 50 ;
        syscall_nr_acct         = 51 ;
        syscall_nr_sigpending   = 52 ;
        syscall_nr_fstat        = 53 ;
        syscall_nr_ioctl        = 54 ;
        syscall_nr_reboot       = 55 ;
        syscall_nr_revoke       = 56 ;
        syscall_nr_symlink      = 57 ;
        syscall_nr_readlink     = 58 ;
        syscall_nr_execve       = 59 ;
        syscall_nr_umask        = 60 ;
        syscall_nr_chroot       = 61 ;
        syscall_nr_getfsstat    = 62 ;
        syscall_nr_statfs       = 63 ;
        syscall_nr_fstatfs      = 64 ;
        syscall_nr_fhstatfs     = 65 ;
        syscall_nr_vfork        = 66 ;
        syscall_nr_gettimeofday = 67 ;
        syscall_nr_settimeofday = 68 ;
        syscall_nr_setitimer    = 69 ;
        syscall_nr_getitimer    = 70 ;
        syscall_nr_select       = 71 ;
        syscall_nr_kevent       = 72 ;
        syscall_nr_munmap       = 73 ;
        syscall_nr_mprotect     = 74 ;
        syscall_nr_madvise      = 75 ;
        syscall_nr_utimes       = 76 ;
        syscall_nr_futimes      = 77 ;
        syscall_nr_mincore      = 78 ;
        syscall_nr_getgroups    = 79 ;
        syscall_nr_setgroups    = 80 ;
        syscall_nr_getpgrp      = 81 ;
        syscall_nr_setpgid      = 82 ;
        syscall_nr_utimensat    = 84 ;
        syscall_nr_futimens     = 85 ;
        syscall_nr_clock_gettime= 87 ;
        syscall_nr_clock_settime= 88 ;
        syscall_nr_clock_getres = 89 ;
        syscall_nr_dup2         = 90 ;
        syscall_nr_nanosleep    = 91 ;
        syscall_nr_fcntl        = 92 ;
        syscall_nr_thrsleep     = 94 ;
        syscall_nr_fsync        = 95 ;
        syscall_nr_setpriority  = 96 ;
        syscall_nr_socket       = 97 ;
        syscall_nr_connect      = 98 ;
        syscall_nr_getdents     = 99 ;
        syscall_nr_getpriority  = 100;
        syscall_nr_sigreturn    = 103;
        syscall_nr_bind         = 104;
        syscall_nr_setsockopt   = 105;
        syscall_nr_listen       = 106;
        syscall_nr_sigsuspend   = 111;
        syscall_nr_getsockopt   = 118;
        syscall_nr_readv        = 120;
        syscall_nr_writev       = 121;
        syscall_nr_fchown       = 123;
        syscall_nr_fchmod       = 124;
        syscall_nr_setreuid     = 126;
        syscall_nr_setregid     = 127;
        syscall_nr_rename       = 128;
        syscall_nr_flock        = 131;
        syscall_nr_mkfifo       = 132;
        syscall_nr_sendto       = 133;
        syscall_nr_shutdown     = 134;
        syscall_nr_socketpair   = 135;
        syscall_nr_mkdir        = 136;
        syscall_nr_rmdir        = 137;
        syscall_nr_adjtime      = 140;
        syscall_nr_setsid       = 147;
        syscall_nr_quotactl     = 148;
        syscall_nr_nfssvc       = 155;
        syscall_nr_getfh        = 161;
        syscall_nr_sysarch      = 165;
        { if defined(SYSVSEM) && !defined(__LP64__)
          "compat_10_osemsys", }
        syscall_nr_semsys       = 169;
        { if defined(SYSVMSG) && !defined(__LP64__)
          "compat_10_omsgsys", }
        syscall_nr_msgsys       = 170;
        { if defined(SYSVSHM) && !defined(__LP64__)
          "compat_10_oshmsys", }
        syscall_nr_shmsys       = 170;

        syscall_nr_pread        = 173;
        syscall_nr_pwrite       = 174;
        syscall_nr_setgid       = 181;
        syscall_nr_setegid      = 182;
        syscall_nr_seteuid      = 183;
        syscall_nr_lfs_bmapv    = 184;
        syscall_nr_lfs_markv    = 185;
        syscall_nr_lfs_segclean = 186;
        syscall_nr_lfs_segwait  = 187;
        syscall_nr_pathconf     = 191;
        syscall_nr_fpathconf    = 192;
        syscall_nr_swapctl      = 193;
        syscall_nr_getrlimit    = 194;
        syscall_nr_setrlimit    = 195;
        syscall_nr_mmap         = 197;
        syscall_nr___syscall    = 198;
        syscall_nr_lseek        = 199;
        syscall_nr_truncate     = 200;
        syscall_nr_ftruncate    = 201;
        syscall_nr___sysctl     = 202;
        syscall_nr_mlock        = 203;
        syscall_nr_munlock      = 204;
        syscall_nr_getpgid      = 207;
        syscall_nr_semget       = 221;
        syscall_nr_msgget       = 225;
        syscall_nr_msgsnd       = 226;
        syscall_nr_msgrcv       = 227;
        syscall_nr_shmat        = 228;
        syscall_nr_shmdt        = 230;
        syscall_nr_minherit     = 250;
        syscall_nr_poll         = 252;
        syscall_nr_issetugid    = 253;
        syscall_nr_lchown       = 254;
        syscall_nr_getsid       = 255;
        syscall_nr_msync        = 256;
        syscall_nr_pipe         = 263;
        syscall_nr_fhopen       = 264;
        syscall_nr_preadv       = 267;
        syscall_nr_pwritev      = 268;
        syscall_nr_kqueue       = 269;
        syscall_nr_mlockall     = 271;
        syscall_nr_munlockall   = 272;
        syscall_nr_getresuid    = 281;
        syscall_nr_setresuid    = 282;
        syscall_nr_getresgid    = 283;
        syscall_nr_setresgid    = 284;
        syscall_nr_mquery       = 286;
        syscall_nr_closefrom    = 287;
        syscall_nr_sigaltstack  = 288;
        { left for compatibility }
        syscall_nr_signalstack  = syscall_nr_sigaltstack;
        syscall_nr_shmget       = 289;
        syscall_nr_semop        = 290;
        syscall_nr_fhstat       = 294;
        syscall_nr___semctl     = 295;
        syscall_nr_shmctl       = 296;
        syscall_nr_msgctl       = 297;
        syscall_nr_sched_yield  = 298;
        syscall_nr_getthrid     = 299;
        syscall_nr_thrwakeup    = 301;
        syscall_nr_threxit      = 302;
        syscall_nr_thrsigdivert = 303;
        syscall_nr___getcwd     = 304;
        syscall_nr_adjfreq      = 305;
        syscall_nr_setrtable    = 310;
        syscall_nr_getrtable    = 311;
        syscall_nr_MAXSYSCALL   = 331;

{ Aliases }
        syscall_nr_waitpid      = syscall_nr_wait4;  // 7, added: ease of notation purposes
