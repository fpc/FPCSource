{
    $Id$
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
 syscall_nr_setup                       = 0;
 syscall_nr_exit                        = 1;
 syscall_nr_fork                        = 2;
 syscall_nr_read                        = 3;
 syscall_nr_write                       = 4;
 syscall_nr_open                        = 5;
 syscall_nr_close                       = 6;
 syscall_nr_waitpid                     = 7;
 syscall_nr_creat                       = 8;
 syscall_nr_link                        = 9;
 syscall_nr_unlink                      = 10;
 syscall_nr_execve                      = 11;
 syscall_nr_chdir                       = 12;
 syscall_nr_time                        = 13;
 syscall_nr_mknod                       = 14;
 syscall_nr_chmod                       = 15;
 syscall_nr_chown                       = 16;
 syscall_nr_break                       = 17;
 syscall_nr_oldstat                     = 18;
 syscall_nr_lseek                       = 19;
 syscall_nr_getpid                      = 20;
 syscall_nr_mount                       = 21;
 syscall_nr_umount                      = 22;
 syscall_nr_setuid                      = 23;
 syscall_nr_getuid                      = 24;
 syscall_nr_stime                       = 25;
 syscall_nr_ptrace                      = 26;
 syscall_nr_alarm                       = 27;
 syscall_nr_oldfstat                    = 28;
 syscall_nr_pause                       = 29;
 syscall_nr_utime                       = 30;
 syscall_nr_stty                        = 31;
 syscall_nr_gtty                        = 32;
 syscall_nr_access                      = 33;
 syscall_nr_nice                        = 34;
 syscall_nr_ftime                       = 35;
 syscall_nr_sync                        = 36;
 syscall_nr_kill                        = 37;
 syscall_nr_rename                      = 38;
 syscall_nr_mkdir                       = 39;
 syscall_nr_rmdir                       = 40;
 syscall_nr_dup                         = 41;
 syscall_nr_pipe                        = 42;
 syscall_nr_times                       = 43;
 syscall_nr_prof                        = 44;
 syscall_nr_brk                         = 45;
 syscall_nr_setgid                      = 46;
 syscall_nr_getgid                      = 47;
 syscall_nr_signal                      = 48;
 syscall_nr_geteuid                     = 49;
 syscall_nr_getegid                     = 50;
 syscall_nr_acct                        = 51;
 syscall_nr_phys                        = 52;
 syscall_nr_lock                        = 53;
 syscall_nr_ioctl                       = 54;
 syscall_nr_fcntl                       = 55;
 syscall_nr_mpx                         = 56;
 syscall_nr_setpgid                     = 57;
 syscall_nr_ulimit                      = 58;
 syscall_nr_oldolduname                 = 59;
 syscall_nr_umask                       = 60;
 syscall_nr_chroot                      = 61;
 syscall_nr_ustat                       = 62;
 syscall_nr_dup2                        = 63;
 syscall_nr_getppid                     = 64;
 syscall_nr_getpgrp                     = 65;
 syscall_nr_setsid                      = 66;
 syscall_nr_sigaction                   = 67;
 syscall_nr_sgetmask                    = 68;
 syscall_nr_ssetmask                    = 69;
 syscall_nr_setreuid                    = 70;
 syscall_nr_setregid                    = 71;
 syscall_nr_sigsuspend                  = 72;
 syscall_nr_sigpending                  = 73;
 syscall_nr_sethostname                 = 74;
 syscall_nr_setrlimit                   = 75;
 syscall_nr_getrlimit                   = 76;
 syscall_nr_getrusage                   = 77;
 syscall_nr_gettimeofday                = 78;
 syscall_nr_settimeofday                = 79;
 syscall_nr_getgroups                   = 80;
 syscall_nr_setgroups                   = 81;
 syscall_nr_select                      = 82;
 syscall_nr_symlink                     = 83;
 syscall_nr_oldlstat                    = 84;
 syscall_nr_readlink                    = 85;
 syscall_nr_uselib                      = 86;
 syscall_nr_swapon                      = 87;
 syscall_nr_reboot                      = 88;
 syscall_nr_readdir                     = 89;
 syscall_nr_mmap                        = 90;
 syscall_nr_munmap                      = 91;
 syscall_nr_truncate                    = 92;
 syscall_nr_ftruncate                   = 93;
 syscall_nr_fchmod                      = 94;
 syscall_nr_fchown                      = 95;
 syscall_nr_getpriority                 = 96;
 syscall_nr_setpriority                 = 97;
 syscall_nr_profil                      = 98;
 syscall_nr_statfs                      = 99;
 syscall_nr_fstatfs                     = 100;
 syscall_nr_ioperm                      = 101;
 syscall_nr_socketcall                  = 102;
 syscall_nr_syslog                      = 103;
 syscall_nr_setitimer                   = 104;
 syscall_nr_getitimer                   = 105;
 syscall_nr_stat                        = 106;
 syscall_nr_lstat                       = 107;
 syscall_nr_fstat                       = 108;
 syscall_nr_olduname                    = 109;
 syscall_nr_iopl                        = 110;
 syscall_nr_vhangup                     = 111;
 syscall_nr_idle                        = 112;
 syscall_nr_vm86old                     = 113;
 syscall_nr_wait4                       = 114;
 syscall_nr_swapoff                     = 115;
 syscall_nr_sysinfo                     = 116;
 syscall_nr_ipc                         = 117;
 syscall_nr_fsync                       = 118;
 syscall_nr_sigreturn                   = 119;
 syscall_nr_clone                       = 120;
 syscall_nr_setdomainname               = 121;
 syscall_nr_uname                       = 122;
 syscall_nr_modify_ldt                  = 123;
 syscall_nr_adjtimex                    = 124;
 syscall_nr_mprotect                    = 125;
 syscall_nr_sigprocmask                 = 126;
 syscall_nr_create_module               = 127;
 syscall_nr_init_module                 = 128;
 syscall_nr_delete_module               = 129;
 syscall_nr_get_kernel_syms             = 130;
 syscall_nr_quotactl                    = 131;
 syscall_nr_getpgid                     = 132;
 syscall_nr_fchdir                      = 133;
 syscall_nr_bdflush                     = 134;
 syscall_nr_sysfs                       = 135;
 syscall_nr_personality                 = 136;
 syscall_nr_afs_syscall                 = 137;
 syscall_nr_setfsuid                    = 138;
 syscall_nr_setfsgid                    = 139;
 syscall_nr__llseek                     = 140;
 syscall_nr_getdents                    = 141;
 syscall_nr__newselect                  = 142;
 syscall_nr_flock                       = 143;
 syscall_nr_msync                       = 144;
 syscall_nr_readv                       = 145;
 syscall_nr_writev                      = 146;
 syscall_nr_getsid                      = 147;
 syscall_nr_fdatasync                   = 148;
 syscall_nr__sysctl                     = 149;
 syscall_nr_mlock                       = 150;
 syscall_nr_munlock                     = 151;
 syscall_nr_mlockall                    = 152;
 syscall_nr_munlockall                  = 153;
 syscall_nr_sched_setparam              = 154;
 syscall_nr_sched_getparam              = 155;
 syscall_nr_sched_setscheduler          = 156;
 syscall_nr_sched_getscheduler          = 157;
 syscall_nr_sched_yield                 = 158;
 syscall_nr_sched_get_priority_max      = 159;
 syscall_nr_sched_get_priority_min      = 160;
 syscall_nr_sched_rr_get_interval       = 161;
 syscall_nr_nanosleep                   = 162;
 syscall_nr_mremap                      = 163;
 syscall_nr_setresuid                   = 164;
 syscall_nr_getresuid                   = 165;
 syscall_nr_vm86                        = 166;
 syscall_nr_query_module                = 167;
 syscall_nr_poll                        = 168;

{$IFDEF SYSCALL_DEBUG}
const
  Sys_nr_txt : array[0..168] of string[15]=(
   'Setup',             {   0 }
   'Exit',              {   1 }
   'Fork',              {   2 }
   'Read',              {   3 }
   'Write',             {   4 }
   'Open',              {   5 }
   'Close',             {   6 }
   'WaitPid',           {   7 }
   'Create',            {   8 }
   'Link',              {   9 }
   'UnLink',            {  10 }
   'ExecVe',            {  11 }
   'ChDir',             {  12 }
   'Time',              {  13 }
   'MkNod',             {  14 }
   'ChMod',             {  15 }
   'ChOwn',             {  16 }
   'Break',             {  17 }
   'OldState',          {  18 }
   'LSeek',             {  19 }
   'GetPid',            {  20 }
   'Mount',             {  21 }
   'UMount',            {  22 }
   'SetUid',            {  23 }
   'GetUid',            {  24 }
   'STime',             {  25 }
   'PTrace',            {  26 }
   'Alarm',             {  27 }
   'OldFStat',          {  28 }
   'Pause',             {  29 }
   'UTime',             {  30 }
   'STTY',              {  31 }
   'GTTY',              {  32 }
   'Access',            {  33 }
   'Nice',              {  34 }
   'FTime',             {  35 }
   'Sync',              {  36 }
   'Kill',              {  37 }
   'Rename',            {  38 }
   'MkDir',             {  39 }
   'RmDir',             {  40 }
   'Dup',               {  41 }
   'Pipe',              {  42 }
   'Times',             {  43 }
   'Prof',              {  44 }
   'Break',             {  45 }
   'SetGid',            {  46 }
   'GetGid',            {  47 }
   'Signal',            {  48 }
   'GetEUid',           {  49 }
   'GetEGid',           {  50 }
   'Acct',              {  51 }
   'Phys',              {  52 }
   'Lock',              {  53 }
   'IOCtl',             {  54 }
   'FCNtl',             {  55 }
   'Mpx',               {  56 }
   'SetPGid',           {  57 }
   'ULimit',            {  58 }
   'OldOldUName',       {  59 }
   'UMask',             {  60 }
   'ChRoot',            {  61 }
   'UStat',             {  62 }
   'Dup2',              {  63 }
   'GetPPid',           {  64 }
   'GetPGrp',           {  65 }
   'SetSid',            {  66 }
   'SigAction',         {  67 }
   'SGetMask',          {  68 }
   'SSetMask',          {  69 }
   'SetReUid',          {  70 }
   'SetReGid',          {  71 }
   'SigSuspend',        {  72 }
   'SigPending',        {  73 }
   'SetHostName',       {  74 }
   'SetRLimit',         {  75 }
   'GetRLimit',         {  76 }
   'GetRUsage',         {  77 }
   'GetTimeOfDay',      {  78 }
   'SetTimeOfDay',      {  79 }
   'GetGroups',         {  80 }
   'SetGroups',         {  81 }
   'Select',            {  82 }
   'SymLink',           {  83 }
   'OldLStat',          {  84 }
   'ReadLink',          {  85 }
   'UseLib',            {  86 }
   'SwapOn',            {  87 }
   'Reboot',            {  88 }
   'ReadDir',           {  89 }
   'MMap',              {  90 }
   'MunMap',            {  91 }
   'Truncate',          {  92 }
   'FTruncate',         {  93 }
   'FChMod',            {  94 }
   'FChOwn',            {  95 }
   'GetPriority',       {  96 }
   'SetPriority',       {  97 }
   'Profile',           {  98 }
   'StatFs',            {  99 }
   'FStatFs',           { 100 }
   'IOPerm',            { 101 }
   'SocketCall',        { 102 }
   'SysLog',            { 103 }
   'SetITimer',         { 104 }
   'GetITimer',         { 105 }
   'Stat',              { 106 }
   'LStat',             { 107 }
   'FStat',             { 108 }
   'OldUName',          { 109 }
   'IOPl',              { 110 }
   'VHangup',           { 111 }
   'Idle',              { 112 }
   'VM86',              { 113 }
   'Wait4',             { 114 }
   'SwapOff',           { 115 }
   'SysInfo',           { 116 }
   'IPC',               { 117 }
   'FSync',             { 118 }
   'SigReturn',         { 119 }
   'Clone',             { 120 }
   'SetDomainName',     { 121 }
   'UName',             { 122 }
   'Modify_Ldt',        { 123 }
   'AdjTimeX',          { 124 }
   'MProtect',          { 125 }
   'SigProcMask',       { 126 }
   'Create_Module',     { 127 }
   'Init_Module',       { 128 }
   'Delete_Module',     { 129 }
   'Get_Kernel_Syms',   { 130 }
   'QuotaCtl',          { 131 }
   'GetPGid',           { 132 }
   'FChDir',            { 133 }
   'BDFlush',           { 134 }
   'SysFs',             { 135 }
   'Personality',       { 136 }
   'AFS_SysCall',       { 137 }
   'SetFsUid',          { 138 }
   'SetFsGid',          { 139 }
   '__LLSeek',          { 140 }
   'GetDents',          { 141 }
   '__NewSelect',       { 142 }
   'FLock',             { 143 }
   'MSync',             { 144 }
   'ReadV',             { 145 }
   'WriteV',            { 146 }
   'GetSid',            { 147 }
   'FDataSync',         { 148 }
   '__SysCtl',          { 149 }
   'MLock',             { 150 }
   'MUnLock',           { 151 }
   'MLockAll',          { 152 }
   'MUnLockAll',        { 153 }
   'MSchdSetParam',     { 154 }
   'MSchdGetParam',     { 155 }
   'MSchdSetSchd',      { 156 }
   'MSchdGetSchd',      { 157 }
   'MSchdYield',        { 158 }
   'MSchdGetPriMax',    { 159 }
   'MSchdGetPriMin',    { 160 }
   'MSchdRRGetInt',     { 161 }
   'NanoSleep',         { 162 }
   'MRemap',            { 163 }
   'SetReSuid',         { 164 }
   'GetReSuid',         { 165 }
   'vm86',              { 166 }
   'QueryModule',       { 167 }
   'Poll');             { 168 }
{$ENDIF}

{
  $Log$
  Revision 1.3  2002-09-07 16:01:19  peter
    * old logs removed and tabs fixed

}
