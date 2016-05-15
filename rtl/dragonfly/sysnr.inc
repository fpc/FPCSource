{
    This file is part of the Free Pascal run time library.
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
 syscall_nr_exit                        =   1;
 syscall_nr_fork                        =   2;
 syscall_nr_read                        =   3;
 syscall_nr_write                       =   4;
 syscall_nr_open                        =   5;
 syscall_nr_close                       =   6;
 syscall_nr_waitpid                     =   7;
                                        {   8  is old creat }
 syscall_nr_link                        =   9;
 syscall_nr_unlink                      =  10;
                                        {  11  obsolete execv }
 syscall_nr_chdir                       =  12;
 syscall_nr_fchdir                      =  13;
 syscall_nr_mknod                       =  14;
 syscall_nr_chmod                       =  15;
 syscall_nr_chown                       =  16;
 syscall_nr_break                       =  17;
 syscall_nr_getfsstat                   =  18;  
                                        {  19  is old lseek }
 syscall_nr_getpid                      =  20;
 syscall_nr_mount                       =  21;
 syscall_nr_umount                      =  22;
 syscall_nr_setuid                      =  23;
 syscall_nr_getuid                      =  24;
 syscall_nr_geteuid                     =  25;
 syscall_nr_ptrace                      =  26;
 syscall_nr_recvmsg                     =  27;
 syscall_nr_sendmsg                     =  28;
 syscall_nr_recvfrom                    =  29;
 syscall_nr_accept                      =  30;
 syscall_nr_getpeername                 =  31;
 syscall_nr_getsockname                 =  32;
 syscall_nr_access                      =  33;
 syscall_nr_chflags                     =  34;
 syscall_nr_fchflags                    =  35;
 syscall_nr_sync                        =  36;
 syscall_nr_kill                        =  37;
                                        {  38  is old stat }
 syscall_nr_getppid                     =  39;
                                        {  40  is old lstat }
 syscall_nr_dup                         =  41;
 syscall_nr_pipe                        =  42;
 syscall_nr_getegid                     =  43;
 syscall_nr_profil                      =  44;
 syscall_nr_ktrace                      =  45;
                                        {  46  is obsolete fbsd3_sigaction }
 syscall_nr_getgid                      =  47;
                                        {  48  is obsolete fbsd3_sigprocmask }
 syscall_nr_getlogin                    =  49;
 syscall_nr_setlogin                    =  50;
 syscall_nr_acct                        =  51;
                                        {  52  is obsolete fbsd3_sigpending }
 syscall_nr_sigaltstack                 =  53;
 syscall_nr_ioctl                       =  54;
 syscall_nr_reboot                      =  55;
 syscall_nr_revoke                      =  56;
 syscall_nr_symlink                     =  57;
 syscall_nr_readlink                    =  58;
 syscall_nr_execve                      =  59;
 syscall_nr_umask                       =  60;
 syscall_nr_chroot                      =  61;
                                        {  62  is old fstat }
                                        {  63  is old getkerninfo }
                                        {  64  is old getpagesize }
 syscall_nr_msync                       =  65;
 syscall_nr_vfork                       =  66;
                                        {  67  is obsolete vread }
                                        {  68  is obsolete vwrite }
 syscall_nr_sbrk                        =  69;
 syscall_nr_sstk                        =  70;
                                        {  71  is old map }
                                        {  72  is old vadvise }
 syscall_nr_munmap                      =  73;
 syscall_nr_mprotect                    =  74;
 syscall_nr_madvise                     =  75;
                                        {  76  is obsolete vhangup }
                                        {  77  is obsolete vlimit }
 syscall_nr_mincore                     =  78;
 syscall_nr_getgroups                   =  79;
 syscall_nr_setgroups                   =  80;
 syscall_nr_getpgrp                     =  81;
 syscall_nr_setpgid                     =  82;
 syscall_nr_setitimer                   =  83;
                                        {  84  is old wait }
 syscall_nr_swapon                      =  85;
 syscall_nr_getitimer                   =  86;
                                        {  87  is old gethostname }
                                        {  88  is old sethostname }
 syscall_nr_getdtablesize               =  89;
 syscall_nr_dup2                        =  90;
                                        {  91  empty }
 syscall_nr_fcntl                       =  92;
 syscall_nr_select                      =  93;
                                        {  94  empty }
 syscall_nr_fsync                       =  95;
 syscall_nr_setpriority                 =  96;
 syscall_nr_socket                      =  97;
 syscall_nr_connect                     =  98;
                                        {  99  is old accept }
 syscall_nr_getpriority                 = 100;
                                        { 101  is old send }
                                        { 102  is old recv }
                                        { 103  is obsolete fbsd3_sigreturn }
 syscall_nr_bind                        = 104;
 syscall_nr_setsockopt                  = 105;
 syscall_nr_listen                      = 106;
                                        { 107  is obsolete vtimes }
                                        { 108  is old sigvec }
                                        { 109  is old sigblock }
                                        { 110  is old sigsetmask }
                                        { 111  is old fbsd3_sigsuspend }
                                        { 112  is old sigstack }
                                        { 113  is old recvmsg }
                                        { 114  is old sendmsg }
                                        { 115  is obsolete vtract }
 syscall_nr_gettimeofday                = 116;
 syscall_nr_getrusage                   = 117;
 syscall_nr_getsockopt                  = 118;
                                        { 119  empty }
 syscall_nr_readv                       = 120;
 syscall_nr_writev                      = 121;
 syscall_nr_settimeofday                = 122;
 syscall_nr_fchown                      = 123;
 syscall_nr_fchmod                      = 124;
                                        { 125  is old recfrom }
 syscall_nr_setreuid                    = 126;
 syscall_nr_setregid                    = 127;
 syscall_nr_rename                      = 128;
                                        { 129  is old truncate }
                                        { 130  is old ftruncate }
 syscall_nr_flock                       = 131;
 syscall_nr_mkfifo                      = 132;
 syscall_nr_sendto                      = 133;
 syscall_nr_shutdown                    = 134;
 syscall_nr_socketpair                  = 135;
 syscall_nr_mkdir                       = 136;
 syscall_nr_rmdir                       = 137;
 syscall_nr_utimes                      = 138;
                                        { 139  is obsolete 4.2 sigreturn }
 syscall_nr_adjtime                     = 140;
                                        { 141  is old getpeername }
                                        { 142  is old gethostid }
                                        { 143  is old sethostid }
                                        { 144  is old getrlimit }
                                        { 145  is old setrlimit }
                                        { 146  is old killpg }
 syscall_nr_setsid                      = 147;
 syscall_nr_quotactl                    = 148;
                                        { 149  is old quota }
                                        { 150  is old getsockname }
                                        { 151  -- 154 empty }
 syscall_nr_nfssvc                      = 155;
                                        { 156  is old getdirentries }
 syscall_nr_statfs                      = 157;
 syscall_nr_fstatfs                     = 158;
                                        { 159  empty }
                                        { 160  empty }
 syscall_nr_getfh                       = 161;
 syscall_nr_getdomainname               = 162;
 syscall_nr_setdomainname               = 163;
 syscall_nr_uname                       = 164;
 syscall_nr_sysarch                     = 165;
                                        { 166  --- 168 empty }
                                        { 169  is old semsys }
                                        { 170  is old msgsys }
                                        { 171  is old shmsys }
                                        { 172  empty }
 syscall_nr_pread                       = 173; { extpread }
 syscall_nr_pwrite                      = 174; { extpwrite }
                                        { 175  empty }
 syscall_nr_ntp_adjtime                 = 176;
                                        { 180  empty }
 syscall_nr_setgid                      = 181;
 syscall_nr_setegid                     = 182;
 syscall_nr_seteuid                     = 183;
                                        { 184  --- 187 empty }
                                        { 188  is old stat }
                                        { 189  is old fstat }
                                        { 190  is old lstat }
 syscall_nr_pathconf                    = 191;
 syscall_nr_fpathconf                   = 192;
                                        { 193  empty }
 syscall_nr_getrlimit                   = 194;
 syscall_nr_setrlimit                   = 195;
                                        { 196  is old getdirentries }
 syscall_nr_mmap                        = 197;
 syscall_nr___syscall                   = 198;
 syscall_nr_lseek                       = 199;
 syscall_nr_truncate                    = 200;
 syscall_nr_ftruncate                   = 201;
 syscall_nr___sysctl                    = 202;
 syscall_nr_mlock                       = 203;
 syscall_nr_munlock                     = 204;
 syscall_nr_undelete                    = 205;
 syscall_nr_futimes                     = 206;
 syscall_nr_getpgid                     = 207;
                                        { 208  empty }
 syscall_nr_poll                        = 209;
                                        { 210  ---  219 empty }
 syscall_nr___semctl                    = 220;
 syscall_nr_semget                      = 221;
 syscall_nr_semop                       = 222;
                                        { 223  empty }
 syscall_nr_msgctl                      = 224;
 syscall_nr_msgget                      = 225;
 syscall_nr_msgsnd                      = 226;
 syscall_nr_msgrcv                      = 227;
 syscall_nr_shmat                       = 228;
 syscall_nr_shmctl                      = 229;
 syscall_nr_shmdt                       = 230;
 syscall_nr_shmget                      = 231;
 syscall_nr_clock_gettime               = 232;
 syscall_nr_clock_settime               = 233;
 syscall_nr_clock_getres                = 234;
                                        { 235  ---  239 empty }
 syscall_nr_nanosleep                   = 240;
                                        { 241  ---  249 empty }
 syscall_nr_minherit                    = 250;
 syscall_nr_rfork                       = 251;
 syscall_nr_openbsd_poll                = 252;
 syscall_nr_issetugid                   = 253;
 syscall_nr_lchown                      = 254;
                                        { 255  ---  271 empty }
                                        { 272  is old getdents }
                                        { 273  empty }
 syscall_nr_lchmod                      = 274;
 syscall_nr_netbsd_lchown               = 275;
 syscall_nr_lutimes                     = 276;
 syscall_nr_netbsd_msync                = 277;
                                        { 278  ---  288 empty }
 syscall_nr_extpreadv                   = 289;
 syscall_nr_extwritev                   = 290;
                                        { 291  ---  296 empty }
 syscall_nr_fhstatfs                    = 297;
 syscall_nr_fhopen                      = 298;
                                        { 299  is old fhstat }
 syscall_nr_modnext                     = 300;
 syscall_nr_modstat                     = 301;
 syscall_nr_modfnext                    = 302;
 syscall_nr_modofind                    = 303;
 syscall_nr_kldload                     = 304;
 syscall_nr_kldunload                   = 305;
 syscall_nr_kldfind                     = 306;
 syscall_nr_kldnext                     = 307;
 syscall_nr_kldstat                     = 308;
 syscall_nr_kldfirstmod                 = 309;
 syscall_nr_getsid			= 310;
 syscall_nr_setresuid                   = 311;
 syscall_nr_setresgid                   = 312;
                                        { 313  is obsolete signanosleep }
                                        { 314  --  320  aio/lio syscalls }
 syscall_nr_yield                       = 321;
                                        { 322  -- 323 empty }
 syscall_nr_mlockall                    = 324;
 syscall_nr_munlockall                  = 325;
 syscall_nr___getcwd                    = 326;
                                        { 327  -- 334 scheduler syscalls }
 syscall_nr_utrace                      = 335;
                                        { 336  is obsolete fbsd4_sendfile }
 syscall_nr_kldsym                      = 337;
 syscall_nr_jail                        = 338;
                                        { 339  empty }
 syscall_nr_sigprocmask                 = 340;
 syscall_nr_sigsuspend                  = 341;
 syscall_nr_sigaction                   = 342;
 syscall_nr_sigpending                  = 343;
 syscall_nr_sigreturn                   = 344;
 syscall_nr_sigtimedwait                = 345;
 syscall_nr_sigwaitinfo                 = 346;
                                        { 347  ---  354 acl syscalls }
                                        { 355  ---  358 extattr syscalls }
 syscall_nr_aio_waitcomplete            = 359;
 syscall_nr_getresuid                   = 360;
 syscall_nr_getresgid                   = 361;
 syscall_nr_kqueue 			= 362;
 syscall_nr_kevent 			= 363;
 syscall_nr_sctp_peeloff                = 364;
                                        { 365  ---  390 empty }
 syscall_lchflags                       = 391;
 syscall_nr_uuidgen			= 392;
 syscall_nr_sendfile 			= 393;
                                        { 394  ---  449 empty }
                                        { 450  ---  452 varsym syscalls }
                                        { 453  ---  464 obsolete caps }
 syscall_nr_exec_sys_register           = 465;
 syscall_nr_exec_sys_unregister         = 466;
 syscall_nr_sys_checkpoint              = 467;
 syscall_nr_mountctl                    = 468;
 syscall_nr_umtx_sleep                  = 469;
 syscall_nr_umtx_wakeup                 = 470;
 syscall_nr_jail_attach                 = 471;
 syscall_nr_set_tls_area                = 472;
 syscall_nr_get_tls_area                = 473;
 syscall_nr_closefrom                   = 474;
 syscall_nr_stat                        = 475;
 syscall_nr_fstat                       = 476;
 syscall_nr_lstat                       = 477;
 syscall_nr_fhstat                      = 478;
 syscall_nr_getdirentries               = 479;
 syscall_nr_getdents                    = 480;
 syscall_nr_usched_set                  = 481;
 syscall_nr_extaccept                   = 482;
 syscall_nr_extconnect                  = 483;
                                        { 484  is obsolete syslink }
                                        { 485  ---  498 vmspace/lwp syscalls }
 syscall_nr_pselect                     = 499;
 syscall_nr_statvfs                     = 500;
 syscall_nr_fstatvfs                    = 501;
 syscall_nr_fhstatvfs                   = 502;
 syscall_nr_getvfsstat                  = 503;
 syscall_nr_openat                      = 504;
 syscall_nr_fstatat                     = 505;
 syscall_nr_fchmodat                    = 506;
 syscall_nr_fchownat                    = 507;
 syscall_nr_unlinkat                    = 508;
 syscall_nr_faccessat                   = 509;
                                        { 510  ---  521 mq/ioprio syscalls }
 syscall_nr_chroot_kernel               = 522;
 syscall_nr_renameat                    = 523;
 syscall_nr_mkdirat                     = 524;
 syscall_nr_mkfifoat                    = 525;
 syscall_nr_mknodat                     = 526;
 syscall_nr_readlinkat                  = 527;
 syscall_nr_symlinkat                   = 528;
 syscall_nr_swapoff                     = 529;
 syscall_nr_vquotactl                   = 530;
 syscall_nr_linkat                      = 531;
 syscall_nr_eaccess                     = 532;
 syscall_nr_lpathconf                   = 533;
