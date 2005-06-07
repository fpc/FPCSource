{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003-2004 by Florian Klaempfl,
    member of the Free Pascal development team.

    Syscall nrs for arm-linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{
* This file contains the system call numbers.
  Last update from 2.4.22 kernel sources according to the sources it contains already the 2.5 extensions
}

Const
  syscall_nr_exit                       =  $900000+ 1;
  syscall_nr_fork                       =  $900000+ 2;
  syscall_nr_read                       =  $900000+ 3;
  syscall_nr_write                      =  $900000+ 4;
  syscall_nr_open                       =  $900000+ 5;
  syscall_nr_close                      =  $900000+ 6;
  // not supported: syscall_nr_waitpid                    =  $900000+ 7;
  syscall_nr_creat                      =  $900000+ 8;
  syscall_nr_link                       =  $900000+ 9;
  syscall_nr_unlink                     =  $900000+10;
  syscall_nr_execve                     =  $900000+11;
  syscall_nr_chdir                      =  $900000+12;
  syscall_nr_time                       =  $900000+13;
  syscall_nr_mknod                      =  $900000+14;
  syscall_nr_chmod                      =  $900000+15;
  syscall_nr_lchown                     =  $900000+16;
  syscall_nr_break                      =  $900000+17;

  syscall_nr_lseek                      =  $900000+19;
  syscall_nr_getpid                     =  $900000+20;
  syscall_nr_mount                      =  $900000+21;
  syscall_nr_umount                     =  $900000+22;
  syscall_nr_setuid                     =  $900000+23;
  syscall_nr_getuid                     =  $900000+24;
  syscall_nr_stime                      =  $900000+25;
  syscall_nr_ptrace                     =  $900000+26;
  syscall_nr_alarm                      =  $900000+27;

  syscall_nr_pause                      =  $900000+29;
  syscall_nr_utime                      =  $900000+30;
  syscall_nr_stty                       =  $900000+31;
  syscall_nr_gtty                       =  $900000+32;
  syscall_nr_access                     =  $900000+33;
  syscall_nr_nice                       =  $900000+34;
  syscall_nr_ftime                      =  $900000+35;
  syscall_nr_sync                       =  $900000+36;
  syscall_nr_kill                       =  $900000+37;
  syscall_nr_rename                     =  $900000+38;
  syscall_nr_mkdir                      =  $900000+39;
  syscall_nr_rmdir                      =  $900000+40;
  syscall_nr_dup                        =  $900000+41;
  syscall_nr_pipe                       =  $900000+42;
  syscall_nr_times                      =  $900000+43;
  syscall_nr_prof                       =  $900000+44;
  syscall_nr_brk                        =  $900000+45;
  syscall_nr_setgid                     =  $900000+46;
  syscall_nr_getgid                     =  $900000+47;
  syscall_nr_signal                     =  $900000+48;
  syscall_nr_geteuid                    =  $900000+49;
  syscall_nr_getegid                    =  $900000+50;
  syscall_nr_acct                       =  $900000+51;
  syscall_nr_umount2                    =  $900000+52;
  syscall_nr_lock                       =  $900000+53;
  syscall_nr_ioctl                      =  $900000+54;
  syscall_nr_fcntl                      =  $900000+55;
  syscall_nr_mpx                        =  $900000+56;
  syscall_nr_setpgid                    =  $900000+57;
  syscall_nr_ulimit                     =  $900000+58;

  syscall_nr_umask                      =  $900000+60;
  syscall_nr_chroot                     =  $900000+61;
  syscall_nr_ustat                      =  $900000+62;
  syscall_nr_dup2                       =  $900000+63;
  syscall_nr_getppid                    =  $900000+64;
  syscall_nr_getpgrp                    =  $900000+65;
  syscall_nr_setsid                     =  $900000+66;
  syscall_nr_sigaction                  =  $900000+67;
  syscall_nr_sgetmask                   =  $900000+68;
  syscall_nr_ssetmask                   =  $900000+69;
  syscall_nr_setreuid                   =  $900000+70;
  syscall_nr_setregid                   =  $900000+71;
  syscall_nr_sigsuspend                 =  $900000+72;
  syscall_nr_sigpending                 =  $900000+73;
  syscall_nr_sethostname                =  $900000+74;
  syscall_nr_setrlimit                  =  $900000+75;
  syscall_nr_getrlimit                  =  $900000+76;
  syscall_nr_getrusage                  =  $900000+77;
  syscall_nr_gettimeofday               =  $900000+78;
  syscall_nr_settimeofday               =  $900000+79;
  syscall_nr_getgroups                  =  $900000+80;
  syscall_nr_setgroups                  =  $900000+81;
  syscall_nr_select                     =  $900000+82;
  syscall_nr_symlink                    =  $900000+83;

  syscall_nr_readlink                   =  $900000+85;
  syscall_nr_uselib                     =  $900000+86;
  syscall_nr_swapon                     =  $900000+87;
  syscall_nr_reboot                     =  $900000+88;
  syscall_nr_readdir                    =  $900000+89;
  syscall_nr_mmap                       =  $900000+90;
  syscall_nr_munmap                     =  $900000+91;
  syscall_nr_truncate                   =  $900000+92;
  syscall_nr_ftruncate                  =  $900000+93;
  syscall_nr_fchmod                     =  $900000+94;
  syscall_nr_fchown                     =  $900000+95;
  syscall_nr_getpriority                =  $900000+96;
  syscall_nr_setpriority                =  $900000+97;
  syscall_nr_profil                     =  $900000+98;
  syscall_nr_statfs                     =  $900000+99;
  syscall_nr_fstatfs                    = $900000+100;
  syscall_nr_ioperm                     = $900000+101;
  syscall_nr_socketcall                 = $900000+102;
  syscall_nr_syslog                     = $900000+103;
  syscall_nr_setitimer                  = $900000+104;
  syscall_nr_getitimer                  = $900000+105;
  syscall_nr_stat                       = $900000+106;
  syscall_nr_lstat                      = $900000+107;
  syscall_nr_fstat                      = $900000+108;
  syscall_nr_vhangup                    = $900000+111;
  syscall_nr_idle                       = $900000+112;
  syscall_nr_syscall                    = $900000+113;
  syscall_nr_wait4                      = $900000+114;
  syscall_nr_swapoff                    = $900000+115;
  syscall_nr_sysinfo                    = $900000+116;
  syscall_nr_ipc                        = $900000+117;
  syscall_nr_fsync                      = $900000+118;
  syscall_nr_sigreturn                  = $900000+119;
  syscall_nr_clone                      = $900000+120;
  syscall_nr_setdomainname              = $900000+121;
  syscall_nr_uname                      = $900000+122;
  syscall_nr_modify_ldt                 = $900000+123;
  syscall_nr_adjtimex                   = $900000+124;
  syscall_nr_mprotect                   = $900000+125;
  syscall_nr_sigprocmask                = $900000+126;
  syscall_nr_create_module              = $900000+127;
  syscall_nr_init_module                = $900000+128;
  syscall_nr_delete_module              = $900000+129;
  syscall_nr_get_kernel_syms            = $900000+130;
  syscall_nr_quotactl                   = $900000+131;
  syscall_nr_getpgid                    = $900000+132;
  syscall_nr_fchdir                     = $900000+133;
  syscall_nr_bdflush                    = $900000+134;
  syscall_nr_sysfs                      = $900000+135;
  syscall_nr_personality                = $900000+136;
  syscall_nr_afs_syscall                = $900000+137;
  syscall_nr_setfsuid                   = $900000+138;
  syscall_nr_setfsgid                   = $900000+139;
  syscall_nr__llseek                    = $900000+140;
  syscall_nr_getdents                   = $900000+141;
  syscall_nr__newselect                 = $900000+142;
  syscall_nr_flock                      = $900000+143;
  syscall_nr_msync                      = $900000+144;
  syscall_nr_readv                      = $900000+145;
  syscall_nr_writev                     = $900000+146;
  syscall_nr_getsid                     = $900000+147;
  syscall_nr_fdatasync                  = $900000+148;
  syscall_nr__sysctl                    = $900000+149;
  syscall_nr_mlock                      = $900000+150;
  syscall_nr_munlock                    = $900000+151;
  syscall_nr_mlockall                   = $900000+152;
  syscall_nr_munlockall                 = $900000+153;
  syscall_nr_sched_setparam             = $900000+154;
  syscall_nr_sched_getparam             = $900000+155;
  syscall_nr_sched_setscheduler         = $900000+156;
  syscall_nr_sched_getscheduler         = $900000+157;
  syscall_nr_sched_yield                = $900000+158;
  syscall_nr_sched_get_priority_max     = $900000+159;
  syscall_nr_sched_get_priority_min     = $900000+160;
  syscall_nr_sched_rr_get_interval      = $900000+161;
  syscall_nr_nanosleep                  = $900000+162;
  syscall_nr_mremap                     = $900000+163;
  syscall_nr_setresuid                  = $900000+164;
  syscall_nr_getresuid                  = $900000+165;
  syscall_nr_vm86                       = $900000+166;
  syscall_nr_query_module               = $900000+167;
  syscall_nr_poll                       = $900000+168;
  syscall_nr_nfsservctl                 = $900000+169;
  syscall_nr_setresgid                  = $900000+170;
  syscall_nr_getresgid                  = $900000+171;
  syscall_nr_prctl                      = $900000+172;
  syscall_nr_rt_sigreturn               = $900000+173;
  syscall_nr_rt_sigaction               = $900000+174;
  syscall_nr_rt_sigprocmask             = $900000+175;
  syscall_nr_rt_sigpending              = $900000+176;
  syscall_nr_rt_sigtimedwait            = $900000+177;
  syscall_nr_rt_sigqueueinfo            = $900000+178;
  syscall_nr_rt_sigsuspend              = $900000+179;
  syscall_nr_pread                      = $900000+180;
  syscall_nr_pwrite                     = $900000+181;
  syscall_nr_chown                      = $900000+182;
  syscall_nr_getcwd                     = $900000+183;
  syscall_nr_capget                     = $900000+184;
  syscall_nr_capset                     = $900000+185;
  syscall_nr_sigaltstack                = $900000+186;
  syscall_nr_sendfile                   = $900000+187;
  syscall_nr_vfork                      = $900000+190;
  syscall_nr_ugetrlimit                 = $900000+191;
  syscall_nr_mmap2                      = $900000+192;
  syscall_nr_truncate64                 = $900000+193;
  syscall_nr_ftruncate64                = $900000+194;
  syscall_nr_stat64                     = $900000+195;
  syscall_nr_lstat64                    = $900000+196;
  syscall_nr_fstat64                    = $900000+197;
  syscall_nr_lchown32                   = $900000+198;
  syscall_nr_getuid32                   = $900000+199;
  syscall_nr_getgid32                   = $900000+200;
  syscall_nr_geteuid32                  = $900000+201;
  syscall_nr_getegid32                  = $900000+202;
  syscall_nr_setreuid32                 = $900000+203;
  syscall_nr_setregid32                 = $900000+204;
  syscall_nr_getgroups32                = $900000+205;
  syscall_nr_setgroups32                = $900000+206;
  syscall_nr_fchown32                   = $900000+207;
  syscall_nr_setresuid32                = $900000+208;
  syscall_nr_getresuid32                = $900000+209;
  syscall_nr_setresgid32                = $900000+210;
  syscall_nr_getresgid32                = $900000+211;
  syscall_nr_chown32                    = $900000+212;
  syscall_nr_setuid32                   = $900000+213;
  syscall_nr_setgid32                   = $900000+214;
  syscall_nr_setfsuid32                 = $900000+215;
  syscall_nr_setfsgid32                 = $900000+216;
  syscall_nr_getdents64                 = $900000+217;
  syscall_nr_pivot_root                 = $900000+218;
  syscall_nr_mincore                    = $900000+219;
  syscall_nr_madvise                    = $900000+220;
  syscall_nr_fcntl64                    = $900000+221;
  syscall_nr_security                   = $900000+223;
  syscall_nr_gettid                     = $900000+224;
  syscall_nr_readahead                  = $900000+225;
  syscall_nr_setxattr                   = $900000+226;
  syscall_nr_lsetxattr                  = $900000+227;
  syscall_nr_fsetxattr                  = $900000+228;
  syscall_nr_getxattr                   = $900000+229;
  syscall_nr_lgetxattr                  = $900000+230;
  syscall_nr_fgetxattr                  = $900000+231;
  syscall_nr_listxattr                  = $900000+232;
  syscall_nr_llistxattr                 = $900000+233;
  syscall_nr_flistxattr                 = $900000+234;
  syscall_nr_removexattr                = $900000+235;
  syscall_nr_lremovexattr               = $900000+236;
  syscall_nr_fremovexattr               = $900000+237;
  syscall_nr_tkill                      = $900000+238;
  syscall_nr_sendfile64                 = $900000+239;
  syscall_nr_futex                      = $900000+240;
  syscall_nr_sched_setaffinity          = $900000+241;
  syscall_nr_sched_getaffinity          = $900000+242;
  syscall_nr_io_setup                   = $900000+243;
  syscall_nr_io_destroy                 = $900000+244;
  syscall_nr_io_getevents               = $900000+245;
  syscall_nr_io_submit                  = $900000+246;
  syscall_nr_io_cancel                  = $900000+247;
  syscall_nr_exit_group                 = $900000+248;
  syscall_nr_lookup_dcookie             = $900000+249;
  syscall_nr_epoll_create               = $900000+250;
  syscall_nr_epoll_ctl                  = $900000+251;
  syscall_nr_epoll_wait                 = $900000+252;
  syscall_nr_remap_file_pages           = $900000+253;

