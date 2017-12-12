{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003-2004 by Florian Klaempfl and David Zhang

    Syscall nrs for mips-linux O32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{
* This file contains the system call numbers.
}

Const
  __NR_Linux                            =  4000;
  syscall_nr_exit                       =  4000+ 1;
  syscall_nr_fork                       =  4000+ 2;
  syscall_nr_read                       =  4000+ 3;
  syscall_nr_write                      =  4000+ 4;
  syscall_nr_open                       =  4000+ 5;
  syscall_nr_close                      =  4000+ 6;
  syscall_nr_waitpid                    =  4000+ 7;
  syscall_nr_creat                      =  4000+ 8;
  syscall_nr_link                       =  4000+ 9;
  syscall_nr_unlink                     =  4000+10;
  syscall_nr_execve                     =  4000+11;
  syscall_nr_chdir                      =  4000+12;
  syscall_nr_time                       =  4000+13;
  syscall_nr_mknod                      =  4000+14;
  syscall_nr_chmod                      =  4000+15;
  syscall_nr_lchown                     =  4000+16;
  syscall_nr_break                      =  4000+17;

  syscall_NR_unused18                   = __NR_Linux +  18;

  syscall_nr_lseek                      =  4000+19;
  syscall_nr_getpid                     =  4000+20;
  syscall_nr_mount                      =  4000+21;
  syscall_nr_umount                     =  4000+22;
  syscall_nr_setuid                     =  4000+23;
  syscall_nr_getuid                     =  4000+24;
  syscall_nr_stime                      =  4000+25;
  syscall_nr_ptrace                     =  4000+26;
  syscall_nr_alarm                      =  4000+27;

  syscall_NR_unused28                   = __NR_Linux +  28;

  syscall_nr_pause                      =  4000+29;
  syscall_nr_utime                      =  4000+30;
  syscall_nr_stty                       =  4000+31;
  syscall_nr_gtty                       =  4000+32;
  syscall_nr_access                     =  4000+33;
  syscall_nr_nice                       =  4000+34;
  syscall_nr_ftime                      =  4000+35;
  syscall_nr_sync                       =  4000+36;
  syscall_nr_kill                       =  4000+37;
  syscall_nr_rename                     =  4000+38;
  syscall_nr_mkdir                      =  4000+39;
  syscall_nr_rmdir                      =  4000+40;
  syscall_nr_dup                        =  4000+41;
  syscall_nr_pipe                       =  4000+42;
  syscall_nr_times                      =  4000+43;
  syscall_nr_prof                       =  4000+44;
  syscall_nr_brk                        =  4000+45;
  syscall_nr_setgid                     =  4000+46;
  syscall_nr_getgid                     =  4000+47;
  syscall_nr_signal                     =  4000+48;
  syscall_nr_geteuid                    =  4000+49;
  syscall_nr_getegid                    =  4000+50;
  syscall_nr_acct                       =  4000+51;
  syscall_nr_umount2                    =  4000+52;
  syscall_nr_lock                       =  4000+53;
  syscall_nr_ioctl                      =  4000+54;
  syscall_nr_fcntl                      =  4000+55;
  syscall_nr_mpx                        =  4000+56;
  syscall_nr_setpgid                    =  4000+57;
  syscall_nr_ulimit                     =  4000+58;

  syscall_NR_unused59                   = __NR_Linux +  59;

  syscall_nr_umask                      =  4000+60;
  syscall_nr_chroot                     =  4000+61;
  syscall_nr_ustat                      =  4000+62;
  syscall_nr_dup2                       =  4000+63;
  syscall_nr_getppid                    =  4000+64;
  syscall_nr_getpgrp                    =  4000+65;
  syscall_nr_setsid                     =  4000+66;
  syscall_nr_sigaction                  =  4000+67;
  syscall_nr_sgetmask                   =  4000+68;
  syscall_nr_ssetmask                   =  4000+69;
  syscall_nr_setreuid                   =  4000+70;
  syscall_nr_setregid                   =  4000+71;
  syscall_nr_sigsuspend                 =  4000+72;
  syscall_nr_sigpending                 =  4000+73;
  syscall_nr_sethostname                =  4000+74;
  syscall_nr_setrlimit                  =  4000+75;
  syscall_nr_getrlimit                  =  4000+76;
  syscall_nr_getrusage                  =  4000+77;
  syscall_nr_gettimeofday               =  4000+78;
  syscall_nr_settimeofday               =  4000+79;
  syscall_nr_getgroups                  =  4000+80;
  syscall_nr_setgroups                  =  4000+81;

//  syscall_nr_select                     =  4000+82;
  syscall_NR_reserved82                 =  __NR_Linux +  82;

  syscall_nr_symlink                    =  4000+83;

  syscall_NR_unused84                   = __NR_Linux +  84;

  syscall_nr_readlink                   =  4000+85;
  syscall_nr_uselib                     =  4000+86;
  syscall_nr_swapon                     =  4000+87;
  syscall_nr_reboot                     =  4000+88;
  syscall_nr_readdir                    =  4000+89;
  syscall_nr_mmap                       =  4000+90;
  syscall_nr_munmap                     =  4000+91;
  syscall_nr_truncate                   =  4000+92;
  syscall_nr_ftruncate                  =  4000+93;
  syscall_nr_fchmod                     =  4000+94;
  syscall_nr_fchown                     =  4000+95;
  syscall_nr_getpriority                =  4000+96;
  syscall_nr_setpriority                =  4000+97;
  syscall_nr_profil                     =  4000+98;
  syscall_nr_statfs                     =  4000+99;
  syscall_nr_fstatfs                    = 4000+100;
  syscall_nr_ioperm                     = 4000+101;
  syscall_nr_socketcall                 = 4000+102;
  syscall_nr_syslog                     = 4000+103;
  syscall_nr_setitimer                  = 4000+104;
  syscall_nr_getitimer                  = 4000+105;
  syscall_nr_stat                       = 4000+106;
  syscall_nr_lstat                      = 4000+107;
  syscall_nr_fstat                      = 4000+108;

  syscall_NR_unused109                   = __NR_Linux +  109;


  syscall_NR_iopl                       = __NR_Linux + 110;

  syscall_nr_vhangup                    = 4000+111;
  syscall_nr_idle                       = 4000+112;
//  syscall_nr_syscall                    = 4000+113;
  syscall_NR_vm86                       = __NR_Linux + 113;

  syscall_nr_wait4                      = 4000+114;
  syscall_nr_swapoff                    = 4000+115;
  syscall_nr_sysinfo                    = 4000+116;
  syscall_nr_ipc                        = 4000+117;
  syscall_nr_fsync                      = 4000+118;
  syscall_nr_sigreturn                  = 4000+119;
  syscall_nr_clone                      = 4000+120;
  syscall_nr_setdomainname              = 4000+121;
  syscall_nr_uname                      = 4000+122;
  syscall_nr_modify_ldt                 = 4000+123;
  syscall_nr_adjtimex                   = 4000+124;
  syscall_nr_mprotect                   = 4000+125;
  syscall_nr_sigprocmask                = 4000+126;
  syscall_nr_create_module              = 4000+127;
  syscall_nr_init_module                = 4000+128;
  syscall_nr_delete_module              = 4000+129;
  syscall_nr_get_kernel_syms            = 4000+130;
  syscall_nr_quotactl                   = 4000+131;
  syscall_nr_getpgid                    = 4000+132;
  syscall_nr_fchdir                     = 4000+133;
  syscall_nr_bdflush                    = 4000+134;
  syscall_nr_sysfs                      = 4000+135;
  syscall_nr_personality                = 4000+136;
  syscall_nr_afs_syscall                = 4000+137;

  syscall_nr_setfsuid                   = 4000+138;
  syscall_nr_setfsgid                   = 4000+139;
  syscall_nr__llseek                    = 4000+140;
  syscall_nr_getdents                   = 4000+141;
  syscall_nr__newselect                 = 4000+142;
  syscall_nr_flock                      = 4000+143;
  syscall_nr_msync                      = 4000+144;
  syscall_nr_readv                      = 4000+145;
  syscall_nr_writev                     = 4000+146;


  syscall_NR_cacheflush                 =  __NR_Linux + 147;
  syscall_NR_cachectl                   =  __NR_Linux + 148;


  syscall_NR_sysmips                    = __NR_Linux + 149;

  syscall_NR_unused150                  = __NR_Linux +  150;

  syscall_nr_getsid                     = 4000+ 151; // 147;
  syscall_nr_fdatasync                  = 4000+ 152; // 148;
  syscall_nr__sysctl                    = 4000+ 153; // 149;

  syscall_nr_mlock                      = 4000+ 154; // 150;

  syscall_nr_munlock                    = 4000+155;
  syscall_nr_mlockall                   = 4000+156;
  syscall_nr_munlockall                 = 4000+157;

  syscall_nr_sched_setparam             = 4000+158;
  syscall_nr_sched_getparam             = 4000+159;

  syscall_nr_sched_setscheduler         = 4000+160;
  syscall_nr_sched_getscheduler         = 4000+161;
  syscall_nr_sched_yield                = 4000+162;


  syscall_nr_sched_get_priority_max     = 4000+163;
  syscall_nr_sched_get_priority_min     = 4000+164;
  syscall_nr_sched_rr_get_interval      = 4000+165;

  syscall_nr_nanosleep                  = 4000+166; // 162;
  syscall_nr_mremap                     = 4000+167;

  syscall_NR_accept      =   __NR_Linux + 168;
  syscall_NR_bind        =   __NR_Linux + 169;
  syscall_NR_connect     =   __NR_Linux + 170;
  syscall_NR_getpeername =   __NR_Linux + 171;
  syscall_NR_getsockname =   __NR_Linux + 172;
  syscall_NR_getsockopt  =   __NR_Linux + 173;
  syscall_NR_listen      =   __NR_Linux + 174;
  syscall_NR_recv        =   __NR_Linux + 175;
  syscall_NR_recvfrom    =   __NR_Linux + 176;
  syscall_NR_recvmsg     =   __NR_Linux + 177;
  syscall_NR_send        =   __NR_Linux + 178;
  syscall_NR_sendmsg     =   __NR_Linux + 179;
  syscall_NR_sendto      =   __NR_Linux + 180;
  syscall_NR_setsockopt  =   __NR_Linux + 181;
  syscall_NR_shutdown    =   __NR_Linux + 182;
  syscall_NR_socket      =   __NR_Linux + 183;
  syscall_NR_socketpair  =   __NR_Linux + 184;

  syscall_nr_setresuid                  = 4000+185;
  syscall_nr_getresuid                  = 4000+186;
//  syscall_nr_vm86                       = 4000+166;
  syscall_nr_query_module               = 4000+187;
  syscall_nr_poll                       = 4000+188;
  syscall_nr_nfsservctl                 = 4000+189;
  syscall_nr_setresgid                  = 4000+190;
  syscall_nr_getresgid                  = 4000+191;
  syscall_nr_prctl                      = 4000+192; // 172;
  syscall_nr_rt_sigreturn               = 4000+193;
  syscall_nr_rt_sigaction               = 4000+194;
  syscall_nr_rt_sigprocmask             = 4000+195;
  syscall_nr_rt_sigpending              = 4000+196;
  syscall_nr_rt_sigtimedwait            = 4000+197;
  syscall_nr_rt_sigqueueinfo            = 4000+198;
  syscall_nr_rt_sigsuspend              = 4000+199;

  syscall_nr_pread64                      = 4000+200;
  syscall_nr_pwrite64                     = 4000+201;

  syscall_nr_chown                      = 4000+202; // 182;
  syscall_nr_getcwd                     = 4000+203; // 183;
  syscall_nr_capget                     = 4000+204; // 184;
  syscall_nr_capset                     = 4000+205; // 185;
  syscall_nr_sigaltstack                = 4000+206; // 186;
  syscall_nr_sendfile                   = 4000+207; // 187;


//  syscall_nr_vfork                      = 4000+190;
//  syscall_nr_ugetrlimit                 = 4000+191;

// the following are new syscall, ......
  syscall_NR_getpmsg     = __NR_Linux + 208;
  syscall_NR_putpmsg     = __NR_Linux + 209;

  syscall_nr_mmap2                      = 4000+210; // 192;
  syscall_nr_truncate64                 = 4000+211; // 193;
  syscall_nr_ftruncate64                = 4000+212; // 194;
  syscall_nr_stat64                     = 4000+213; // 195;
  syscall_nr_lstat64                    = 4000+214; // 196;
  syscall_nr_fstat64                    = 4000+215; // 197;

{
  syscall_nr_lchown32                   = 4000+198;
  syscall_nr_getuid32                   = 4000+199;
  syscall_nr_getgid32                   = 4000+200;
  syscall_nr_geteuid32                  = 4000+201;
  syscall_nr_getegid32                  = 4000+202;
  syscall_nr_setreuid32                 = 4000+203;
  syscall_nr_setregid32                 = 4000+204;
  syscall_nr_getgroups32                = 4000+205;
  syscall_nr_setgroups32                = 4000+206;
  syscall_nr_fchown32                   = 4000+207;
  syscall_nr_setresuid32                = 4000+208;
  syscall_nr_getresuid32                = 4000+209;
  syscall_nr_setresgid32                = 4000+210;
  syscall_nr_getresgid32                = 4000+211;
  syscall_nr_chown32                    = 4000+212;
  syscall_nr_setuid32                   = 4000+213;
  syscall_nr_setgid32                   = 4000+214;
  syscall_nr_setfsuid32                 = 4000+215;
  syscall_nr_setfsgid32                 = 4000+216;
 }

  syscall_nr_pivot_root                 = 4000+216; // 218;
  syscall_nr_mincore                    = 4000+217; // 219;
  syscall_nr_madvise                    = 4000+218; // 220;
  syscall_nr_getdents64                 = 4000+219; // 217;
  syscall_nr_fcntl64                    = 4000+220; // 221;
//  syscall_nr_security                   = 4000+223;

// syscall_NR_reserved221    (__NR_Linux + 221)

  syscall_nr_gettid                     = 4000+222; // 224;
  syscall_nr_readahead                  = 4000+223; // 225;
  syscall_nr_setxattr                   = 4000+224; // 226;
  syscall_nr_lsetxattr                  = 4000+225; // 227;
  syscall_nr_fsetxattr                  = 4000+226; // 228;
  syscall_nr_getxattr                   = 4000+227; // 229;
  syscall_nr_lgetxattr                  = 4000+228; // 230;
  syscall_nr_fgetxattr                  = 4000+229; // 231;
  syscall_nr_listxattr                  = 4000+230; // 232;
  syscall_nr_llistxattr                 = 4000+231; // 233;
  syscall_nr_flistxattr                 = 4000+232; // 234;
  syscall_nr_removexattr                = 4000+233; // 235;
  syscall_nr_lremovexattr               = 4000+234; // 236;
  syscall_nr_fremovexattr               = 4000+235; // 237;
  syscall_nr_tkill                      = 4000+236; // 238;
  syscall_nr_sendfile64                 = 4000+237; // 239;
  syscall_nr_futex                      = 4000+238; // 240;
  syscall_nr_sched_setaffinity          = 4000+239; // 241;
  syscall_nr_sched_getaffinity          = 4000+240; // 242;
  syscall_nr_io_setup                   = 4000+241; // 243;
  syscall_nr_io_destroy                 = 4000+242; // 244;
  syscall_nr_io_getevents               = 4000+243; // 245;
  syscall_nr_io_submit                  = 4000+244; // 246;
  syscall_nr_io_cancel                  = 4000+245; // 247;
  syscall_nr_exit_group                 = 4000+246; // 248;
  syscall_nr_lookup_dcookie             = 4000+247; // 249;
  syscall_nr_epoll_create               = 4000+248; // 250;
  syscall_nr_epoll_ctl                  = 4000+249; // 251;
  syscall_nr_epoll_wait                 = 4000+250; // 252;
  syscall_nr_remap_file_pages           = 4000+251; // 253;

  syscall_NR_set_tid_address  = __NR_Linux + 252;
  syscall_NR_restart_syscall  = __NR_Linux + 253;
  syscall_NR_fadvise64        = __NR_Linux + 254;
  syscall_NR_statfs64         = __NR_Linux + 255;
  syscall_NR_fstatfs64        = __NR_Linux + 256;
  syscall_NR_timer_create     = __NR_Linux + 257;
  syscall_NR_timer_settime    = __NR_Linux + 258;
  syscall_NR_timer_gettime    = __NR_Linux + 259;
  syscall_NR_timer_getoverrun = __NR_Linux + 260;
  syscall_NR_timer_delete     = __NR_Linux + 261;
  syscall_NR_clock_settime    = __NR_Linux + 262;
  syscall_NR_clock_gettime    = __NR_Linux + 263;
  syscall_NR_clock_getres     = __NR_Linux + 264;
  syscall_NR_clock_nanosleep  = __NR_Linux + 265;
  syscall_NR_tgkill           = __NR_Linux + 266;
  syscall_NR_utimes           = __NR_Linux + 267;
  syscall_NR_mbind            = __NR_Linux + 268;
  syscall_NR_get_mempolicy    = __NR_Linux + 269;
  syscall_NR_set_mempolicy    = __NR_Linux + 270;
  syscall_NR_mq_open          = __NR_Linux + 271;
  syscall_NR_mq_unlink        = __NR_Linux + 272;
  syscall_NR_mq_timedsend     = __NR_Linux + 273;
  syscall_NR_mq_timedreceive  = __NR_Linux + 274;
  syscall_NR_mq_notify        = __NR_Linux + 275;
  syscall_NR_mq_getsetattr    = __NR_Linux + 276;
  syscall_NR_vserver          = __NR_Linux + 277;
  syscall_NR_waitid           = __NR_Linux + 278;
//  /* syscall_NR_sys_setaltroot    (__NR_Linux + 279) */
  syscall_NR_add_key          = __NR_Linux + 280;
  syscall_NR_request_key      = __NR_Linux + 281;
  syscall_NR_keyctl           = __NR_Linux + 282;
  syscall_NR_set_thread_area  = __NR_Linux + 283;
  syscall_NR_inotify_init     = __NR_Linux + 284;
  syscall_NR_inotify_add_watch =    (__NR_Linux + 285);
  syscall_NR_inotify_rm_watch = __NR_Linux + 286;
  syscall_NR_migrate_pages    = __NR_Linux + 287;
  syscall_NR_openat           = __NR_Linux + 288;
  syscall_NR_mkdirat          = __NR_Linux + 289;
  syscall_NR_mknodat          = __NR_Linux + 290;
  syscall_NR_fchownat         = __NR_Linux + 291;
  syscall_NR_futimesat        = __NR_Linux + 292;
  syscall_NR_fstatat          = __NR_Linux + 293;
  syscall_NR_unlinkat         = __NR_Linux + 294;
  syscall_NR_renameat         = __NR_Linux + 295;
  syscall_NR_linkat           = __NR_Linux + 296;
  syscall_NR_symlinkat        = __NR_Linux + 297;
  syscall_NR_readlinkat       = __NR_Linux + 298;
  syscall_NR_fchmodat         = __NR_Linux + 299;
  syscall_NR_faccessat        = __NR_Linux + 300;
  syscall_NR_pselect6         = __NR_Linux + 301;
  syscall_NR_ppoll            = __NR_Linux + 302;
  syscall_NR_unshare          = __NR_Linux + 303;
  syscall_NR_splice           = __NR_Linux + 304;
  syscall_NR_sync_file_range  = __NR_Linux + 305;
  syscall_NR_tee              = __NR_Linux + 306;
  syscall_NR_vmsplice         = __NR_Linux + 307;
  syscall_NR_move_pages       = __NR_Linux + 308;
