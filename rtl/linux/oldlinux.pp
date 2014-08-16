{
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
   BSD parts (c) 2000 by Marco van de Voort
   members of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
unit oldlinux deprecated 'Use Baseunix/Unix';

Interface

Const
  { Things for LSEEK call }
  Seek_set = 0;
  Seek_Cur = 1;
  Seek_End = 2;
  { Things for OPEN call - after linux/fcntl.h }
  Open_Accmode   = 3;
  Open_RdOnly    = 0;
  Open_WrOnly    = 1;
  Open_RdWr      = 2;
  Open_Creat     = 1 shl 6;
  Open_Excl      = 2 shl 6;
  Open_NoCtty    = 4 shl 6;
  Open_Trunc     = 1 shl 9;
  Open_Append    = 2 shl 9;
  Open_NonBlock  = 4 shl 9;
  Open_NDelay    = Open_NonBlock;
  Open_Sync      = 1 shl 12;
  Open_Direct    = 4 shl 12;
  Open_LargeFile = 1 shl 15;
  Open_Directory = 2 shl 15;
  Open_NoFollow  = 4 shl 15;
  { The waitpid uses the following options:}
  Wait_NoHang   = 1;
  Wait_UnTraced = 2;
  Wait_Any      = -1;
  Wait_MyPGRP   = 0;
  Wait_Clone    = $80000000;
  { Constants to check stat.mode }
  STAT_IFMT   = $f000; {00170000}
  STAT_IFSOCK = $c000; {0140000}
  STAT_IFLNK  = $a000; {0120000}
  STAT_IFREG  = $8000; {0100000}
  STAT_IFBLK  = $6000; {0060000}
  STAT_IFDIR  = $4000; {0040000}
  STAT_IFCHR  = $2000; {0020000}
  STAT_IFIFO  = $1000; {0010000}
  STAT_ISUID  = $0800; {0004000}
  STAT_ISGID  = $0400; {0002000}
  STAT_ISVTX  = $0200; {0001000}
  { Constants to check permissions }
  STAT_IRWXO = $7;
  STAT_IROTH = $4;
  STAT_IWOTH = $2;
  STAT_IXOTH = $1;

  STAT_IRWXG = STAT_IRWXO shl 3;
  STAT_IRGRP = STAT_IROTH shl 3;
  STAT_IWGRP = STAT_IWOTH shl 3;
  STAT_IXGRP = STAT_IXOTH shl 3;

  STAT_IRWXU = STAT_IRWXO shl 6;
  STAT_IRUSR = STAT_IROTH shl 6;
  STAT_IWUSR = STAT_IWOTH shl 6;
  STAT_IXUSR = STAT_IXOTH shl 6;

  { Constants to test the type of filesystem }
  fs_old_ext2 = $ef51;
  fs_ext2     = $ef53;
  fs_ext      = $137d;
  fs_iso      = $9660;
  fs_minix    = $137f;
  fs_minix_30 = $138f;
  fs_minux_V2 = $2468;
  fs_msdos    = $4d44;
  fs_nfs      = $6969;
  fs_proc     = $9fa0;
  fs_xia      = $012FD16D;

  { Constansts for MMAP }
  MAP_PRIVATE   =2;
{$if defined(cpumips) or defined(cpumipsel)}
  MAP_ANONYMOUS =$800;
{$else cpumips}
  MAP_ANONYMOUS =$20;
{$endif cpumips}

  {Constansts Termios/Ioctl (used in Do_IsDevice) }
  IOCtl_TCGETS=$5401; // TCGETS is also in termios.inc, but the sysunix needs only this

type

{
 Linux system calls take arguments as follows :

   cpui386/m68k:

   %eax/%d0 : System call number
   %ebx/%d1 : first argument
   %ecx/%d2 : second argument
   %edx/%d3 : third argumens
   %esi/%d3 : fourth argument
   %edi/%d4 : fifth argument

  That is why we define a special type, with only these arguments
  To make it processor independent, we don't give any system dependent
  names, but the rather abstract reg1,reg2 etc;
}
  SysCallRegs=record
    reg1,reg2,reg3,reg4,reg5,reg6 : longint;
  end;
  PSysCallRegs=^SysCallRegs;
  TSysCallRegs=SysCallRegs;

{ The following are records for system calls }
  dirent = packed record
    ino,
    off    : longint;
    reclen : word;
    name   : array [0..255] of char;
  end;
  pdirent =^dirent;
  TDirEnt = dirent;

  TDir = packed record
    fd     : integer;
    loc    : longint;
    size   : integer;
    buf    : pdirent;
  {The following are used in libc, but NOT in the linux kernel sources ??}
    nextoff: longint;
    dd_max : integer; {size of buf. Irrelevant, as buf is of type dirent}
    lock   : pointer;
  end;
  PDir =^TDir;

  dev_t = word;

  Stat = packed record
    dev     : dev_t;
    pad1    : word;
    ino     : longint;
    mode,
    nlink,
    uid,
    gid     : word;
    rdev    : dev_t;
    pad2    : word;
    size,
    blksize,
    blocks,
    atime,
    unused1,
    mtime,
    unused2,
    ctime,
    unused3,
    unused4,
    unused5 : longint;
  end;
  PStat=^Stat;
  TStat=Stat;

  Statfs = packed record
    fstype,            { File system type }
    bsize,             { Optimal block trensfer size }
    blocks,            { Data blocks in system }
    bfree,             { free blocks in system }
    bavail,            { Available free blocks to non-root users }
    files,             { File nodes in system }
    ffree,             { Free file nodes in system }
    fsid,              { File system ID }
    namelen : longint; { Maximum name length in system }
    spare   : array [0..6] of longint; { For later use }
  end;
  PStatFS=^StatFS;
  TStatFS=StatFS;

  fdSet=array[0..7] of longint;{=256 bits}
  pfdset=^fdset;
  TFDSet=fdset;

  timeval = packed record
    sec,usec:longint
  end;
  ptimeval=^timeval;
  TTimeVal=timeval;

  timespec = packed record
    tv_sec,tv_nsec:longint;
  end;

  timezone = packed record
    minuteswest,dsttime:longint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

  utsname = packed record
    sysname,
    nodename,
    release,
    version,
    machine,
    domainname : Array[0..64] of char;
  end;
  PUTSName=^UTSName;
  TUTSName=UTSName;

{ Get System call numbers and error-numbers}

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
 syscall_nr_sigaltstack                 = 186;

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

Const

Sys_EPERM       = 1;    { Operation not permitted }
Sys_ENOENT      = 2;    { No such file or directory }
Sys_ESRCH       = 3;    { No such process }
Sys_EINTR       = 4;    { Interrupted system call }
Sys_EIO = 5;    { I/O error }
Sys_ENXIO       = 6;    { No such device or address }
Sys_E2BIG       = 7;    { Arg list too long }
Sys_ENOEXEC     = 8;    { Exec format error }
Sys_EBADF       = 9;    { Bad file number }
Sys_ECHILD      = 10;   { No child processes }
Sys_EAGAIN      = 11;   { Try again }
Sys_ENOMEM      = 12;   { Out of memory }
Sys_EACCES      = 13;   { Permission denied }
Sys_EFAULT      = 14;   { Bad address }
Sys_ENOTBLK     = 15;   { Block device required, NOT POSIX! }
Sys_EBUSY       = 16;   { Device or resource busy }
Sys_EEXIST      = 17;   { File exists }
Sys_EXDEV       = 18;   { Cross-device link }
Sys_ENODEV      = 19;   { No such device }
Sys_ENOTDIR     = 20;   { Not a directory }
Sys_EISDIR      = 21;   { Is a directory }
Sys_EINVAL      = 22;   { Invalid argument }
Sys_ENFILE      = 23;   { File table overflow }
Sys_EMFILE      = 24;   { Too many open files }
Sys_ENOTTY      = 25;   { Not a typewriter }
Sys_ETXTBSY     = 26;   { Text file busy. The new process was
                      a pure procedure (shared text) file which was
                      open for writing by another process, or file
                      which was open for writing by another process,
                      or while the pure procedure file was being
                      executed an open(2) call requested write access
                      requested write access.}
Sys_EFBIG       = 27;   { File too large }
Sys_ENOSPC      = 28;   { No space left on device }
Sys_ESPIPE      = 29;   { Illegal seek }
Sys_EROFS       = 30;   { Read-only file system }
Sys_EMLINK      = 31;   { Too many links }
Sys_EPIPE       = 32;   { Broken pipe }
Sys_EDOM        = 33;   { Math argument out of domain of func }
Sys_ERANGE      = 34;   { Math result not representable }
Sys_EDEADLK     = 35;   { Resource deadlock would occur }
Sys_ENAMETOOLONG= 36;   { File name too long }
Sys_ENOLCK      = 37;   { No record locks available }
Sys_ENOSYS      = 38;   { Function not implemented }
Sys_ENOTEMPTY= 39;      { Directory not empty }
Sys_ELOOP       = 40;   { Too many symbolic links encountered }
Sys_EWOULDBLOCK = Sys_EAGAIN;   { Operation would block }
Sys_ENOMSG      = 42;   { No message of desired type }
Sys_EIDRM       = 43;   { Identifier removed }
Sys_ECHRNG      = 44;   { Channel number out of range }
Sys_EL2NSYNC= 45;       { Level 2 not synchronized }
Sys_EL3HLT      = 46;   { Level 3 halted }
Sys_EL3RST      = 47;   { Level 3 reset }
Sys_ELNRNG      = 48;   { Link number out of range }
Sys_EUNATCH     = 49;   { Protocol driver not attached }
Sys_ENOCSI      = 50;   { No CSI structure available }
Sys_EL2HLT      = 51;   { Level 2 halted }
Sys_EBADE       = 52;   { Invalid exchange }
Sys_EBADR       = 53;   { Invalid request descriptor }
Sys_EXFULL      = 54;   { Exchange full }
Sys_ENOANO      = 55;   { No anode }
Sys_EBADRQC     = 56;   { Invalid request code }
Sys_EBADSLT     = 57;   { Invalid slot }
Sys_EDEADLOCK= 58;      { File locking deadlock error }
Sys_EBFONT      = 59;   { Bad font file format }
Sys_ENOSTR      = 60;   { Device not a stream }
Sys_ENODATA     = 61;   { No data available }
Sys_ETIME       = 62;   { Timer expired }
Sys_ENOSR       = 63;   { Out of streams resources }
Sys_ENONET      = 64;   { Machine is not on the network }
Sys_ENOPKG      = 65;   { Package not installed }
Sys_EREMOTE     = 66;   { Object is remote }
Sys_ENOLINK     = 67;   { Link has been severed }
Sys_EADV        = 68;   { Advertise error }
Sys_ESRMNT      = 69;   { Srmount error }
Sys_ECOMM       = 70;   { Communication error on send }
Sys_EPROTO      = 71;   { Protocol error }
Sys_EMULTIHOP= 72;      { Multihop attempted }
Sys_EDOTDOT     = 73;   { RFS specific error }
Sys_EBADMSG     = 74;   { Not a data message }
Sys_EOVERFLOW= 75;      { Value too large for defined data type }
Sys_ENOTUNIQ= 76;       { Name not unique on network }
Sys_EBADFD      = 77;   { File descriptor in bad state }
Sys_EREMCHG     = 78;   { Remote address changed }
Sys_ELIBACC     = 79;   { Can not access a needed shared library }
Sys_ELIBBAD     = 80;   { Accessing a corrupted shared library }
Sys_ELIBSCN     = 81;   { .lib section in a.out corrupted }
Sys_ELIBMAX     = 82;   { Attempting to link in too many shared libraries }
Sys_ELIBEXEC= 83;       { Cannot exec a shared library directly }
Sys_EILSEQ      = 84;   { Illegal byte sequence }
Sys_ERESTART= 85;       { Interrupted system call should be restarted }
Sys_ESTRPIPE= 86;       { Streams pipe error }
Sys_EUSERS      = 87;   { Too many users }
Sys_ENOTSOCK= 88;       { Socket operation on non-socket }
Sys_EDESTADDRREQ= 89;   { Destination address required }
Sys_EMSGSIZE= 90;       { Message too long }
Sys_EPROTOTYPE= 91;     { Protocol wrong type for socket }
Sys_ENOPROTOOPT= 92;    { Protocol not available }
Sys_EPROTONOSUPPORT= 93;        { Protocol not supported }
Sys_ESOCKTNOSUPPORT= 94;        { Socket type not supported }
Sys_EOPNOTSUPP= 95;     { Operation not supported on transport endpoint }
Sys_EPFNOSUPPORT= 96;   { Protocol family not supported }
Sys_EAFNOSUPPORT= 97;   { Address family not supported by protocol }
Sys_EADDRINUSE= 98;     { Address already in use }
Sys_EADDRNOTAVAIL= 99;  { Cannot assign requested address }
Sys_ENETDOWN= 100;      { Network is down }
Sys_ENETUNREACH= 101;   { Network is unreachable }
Sys_ENETRESET= 102;     { Network dropped connection because of reset }
Sys_ECONNABORTED= 103;  { Software caused connection abort }
Sys_ECONNRESET= 104;    { Connection reset by peer }
Sys_ENOBUFS     = 105;  { No buffer space available }
Sys_EISCONN     = 106;  { Transport endpoint is already connected }
Sys_ENOTCONN= 107;      { Transport endpoint is not connected }
Sys_ESHUTDOWN= 108;     { Cannot send after transport endpoint shutdown }
Sys_ETOOMANYREFS= 109;  { Too many references: cannot splice }
Sys_ETIMEDOUT= 110;     { Connection timed out }
Sys_ECONNREFUSED= 111;  { Connection refused }
Sys_EHOSTDOWN= 112;     { Host is down }
Sys_EHOSTUNREACH= 113;  { No route to host }
Sys_EALREADY= 114;      { Operation already in progress }
Sys_EINPROGRESS= 115;   { Operation now in progress }
Sys_ESTALE      = 116;  { Stale NFS file handle }
Sys_EUCLEAN     = 117;  { Structure needs cleaning }
Sys_ENOTNAM     = 118;  { Not a XENIX named type file }
Sys_ENAVAIL     = 119;  { No XENIX semaphores available }
Sys_EISNAM      = 120;  { Is a named type file }
Sys_EREMOTEIO= 121;     { Remote I/O error }
Sys_EDQUOT      = 122;  { Quota exceeded }


{ This value was suggested by Daniel
  based on infos from www.linuxassembly.org }

Sys_ERROR_MAX = $fff;

{$packrecords C}

{********************
      Signal
********************}
type
  SigSet  = Longint;
  PSigSet = ^SigSet;

Const
  { For sending a signal }
  SA_NOCLDSTOP = 1;
  SA_SHIRQ     = $04000000;
  SA_STACK     = $08000000;
  SA_RESTART   = $10000000;
  SA_INTERRUPT = $20000000;
  SA_NOMASK    = $40000000;
  SA_ONESHOT   = $80000000;
  SA_ONSTACK   = SA_STACK;

  SIG_BLOCK   = 0;
  SIG_UNBLOCK = 1;
  SIG_SETMASK = 2;

  SIG_DFL = 0 ;
  SIG_IGN = 1 ;
  SIG_ERR = -1 ;

  SIGHUP     = 1;
  SIGINT     = 2;
  SIGQUIT    = 3;
  SIGILL     = 4;
  SIGTRAP    = 5;
  SIGABRT    = 6;
  SIGIOT     = 6;
  SIGBUS     = 7;
  SIGFPE     = 8;
  SIGKILL    = 9;
  SIGUSR1    = 10;
  SIGSEGV    = 11;
  SIGUSR2    = 12;
  SIGPIPE    = 13;
  SIGALRM    = 14;
  SIGTerm    = 15;
  SIGSTKFLT  = 16;
  SIGCHLD    = 17;
  SIGCONT    = 18;
  SIGSTOP    = 19;
  SIGTSTP    = 20;
  SIGTTIN    = 21;
  SIGTTOU    = 22;
  SIGURG     = 23;
  SIGXCPU    = 24;
  SIGXFSZ    = 25;
  SIGVTALRM  = 26;
  SIGPROF    = 27;
  SIGWINCH   = 28;
  SIGIO      = 29;
  SIGPOLL    = SIGIO;
  SIGPWR     = 30;
  SIGUNUSED  = 31;


const
  SI_PAD_SIZE   = ((128/sizeof(longint)) - 3);

type
  Size_T = cardinal;

  tfpreg = record
          significand: array[0..3] of word;
          exponent: word;
  end;

  pfpstate = ^tfpstate;
  tfpstate = record
           cw, sw, tag, ipoff, cssel, dataoff, datasel: cardinal;
           st: array[0..7] of tfpreg;
           status: cardinal;
  end;

  PSigContextRec = ^SigContextRec;
  SigContextRec = record
    gs, __gsh: word;
    fs, __fsh: word;
    es, __esh: word;
    ds, __dsh: word;
    edi: cardinal;
    esi: cardinal;
    ebp: cardinal;
    esp: cardinal;
    ebx: cardinal;
    edx: cardinal;
    ecx: cardinal;
    eax: cardinal;
    trapno: cardinal;
    err: cardinal;
    eip: cardinal;
    cs, __csh: word;
    eflags: cardinal;
    esp_at_signal: cardinal;
    ss, __ssh: word;
    fpstate: pfpstate;
    oldmask: cardinal;
    cr2: cardinal;
  end;

(*
  PSigInfoRec = ^SigInfoRec;
  SigInfoRec = record
    si_signo: longint;
    si_errno: longint;
    si_code: longint;

    case longint of
      0:
        (pad: array[SI_PAD_SIZE] of longint);
      1: { kill }
        ( kill: record
            pid: longint;  { sender's pid }
            uid : longint; { sender's uid }
          end );
      2: { POSIX.1b timers }
        ( timer : record
            timer1 : cardinal;
            timer2 : cardinal;
           end );
      3: { POSIX.1b signals }
        ( rt : record
            pid : longint;    { sender's pid }
            uid : longint;    { sender's uid }
            sigval : longint;
         end );
      4: { SIGCHLD }
        ( sigchld : record
          pid : longint;    { which child }
          uid : longint;    { sender's uid }
          status : longint; { exit code }
          utime : timeval;
          stime : timeval;
         end );
      5: { SIGILL, SIGFPE, SIGSEGV, SIGBUS }
        ( sigfault : record
            addr : pointer;{ faulting insn/memory ref. }
          end );
      6:
        ( sigpoll : record
            band : longint; { POLL_IN, POLL_OUT, POLL_MSG }
            fd : longint;
          end );
  end;
*)

  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
  TSigAction = procedure(Sig: Longint; SigContext: SigContextRec);cdecl;

  SigActionRec = packed record
    Handler  : record
      case byte of
        0: (Sh: SignalHandler);
        1: (Sa: TSigAction);
      end;
    Sa_Mask     : SigSet;
    Sa_Flags    : Longint;
    Sa_restorer : SignalRestorer; { Obsolete - Don't use }
  end;
  PSigActionRec = ^SigActionRec;

const
  SS_ONSTACK = 1;
  SS_DISABLE = 2;
  MINSIGSTKSZ = 2048;
  SIGSTKSZ    = 8192;

type
  SigAltStack = record
    ss_sp : pointer;
    ss_flags : longint;
    ss_size : size_t;
  end;

  stack_t = sigaltstack;

  PSigAltStack = ^SigAltStack;

  pstack_t = ^stack_t;

var
  ErrNo,
  LinuxError : Longint;


{********************
      Process
********************}
const
  {Checked for BSD using Linuxthreads port}
  { cloning flags }
  CSIGNAL       = $000000ff; // signal mask to be sent at exit
  CLONE_VM      = $00000100; // set if VM shared between processes
  CLONE_FS      = $00000200; // set if fs info shared between processes
  CLONE_FILES   = $00000400; // set if open files shared between processes
  CLONE_SIGHAND = $00000800; // set if signal handlers shared
  CLONE_PID     = $00001000; // set if pid shared
type
  TCloneFunc=function(args:pointer):longint;cdecl;

const
  { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;

{$ifdef Solaris}
  WNOHANG   = $100;
  WUNTRACED = $4;
{$ELSE}
  WNOHANG   = $1;
  WUNTRACED = $2;
  __WCLONE  = $80000000;
{$ENDIF}

{********************
      File
********************}

Const
  P_IN  = 1;
  P_OUT = 2;

Const
  LOCK_SH = 1;
  LOCK_EX = 2;
  LOCK_UN = 8;
  LOCK_NB = 4;


Type
  Tpipe = array[1..2] of longint;

  pglob = ^tglob;
  tglob = record
    name : pchar;
    next : pglob;
  end;

  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];

const

  { For testing  access rights }
  R_OK = 4;
  W_OK = 2;
  X_OK = 1;
  F_OK = 0;

{$ifndef newreaddir}
  { For File control mechanism }
  F_GetFd  = 1;
  F_SetFd  = 2;
  F_GetFl  = 3;
  F_SetFl  = 4;

{$ifdef Solaris}
  F_DupFd  = 0;
  F_Dup2Fd = 9;
  F_GetOwn = 23;
  F_SetOwn = 24;
  F_GetLk  = 14;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_FreeSp = 11;
{$else}
  F_GetLk  = 5;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_SetOwn = 8;
  F_GetOwn = 9;
{$endif}
{$endif}

{********************
   IOCtl(TermIOS)
********************}

{Is too freebsd/Linux specific}

{********************
   IOCtl(TermIOS)
********************}

Const
  { Amount of Control Chars }
  NCCS = 32;
  NCC = 8;

{$Ifndef BSD}
  { For Terminal handling }
  TCGETS          = $5401;
  TCSETS          = $5402;
  TCSETSW         = $5403;
  TCSETSF         = $5404;
  TCGETA          = $5405;
  TCSETA          = $5406;
  TCSETAW         = $5407;
  TCSETAF         = $5408;
  TCSBRK          = $5409;
  TCXONC          = $540A;
  TCFLSH          = $540B;
  TIOCEXCL        = $540C;
  TIOCNXCL        = $540D;
  TIOCSCTTY       = $540E;
  TIOCGPGRP       = $540F;
  TIOCSPGRP       = $5410;
  TIOCOUTQ        = $5411;
  TIOCSTI         = $5412;
  TIOCGWINSZ      = $5413;
  TIOCSWINSZ      = $5414;
  TIOCMGET        = $5415;
  TIOCMBIS        = $5416;
  TIOCMBIC        = $5417;
  TIOCMSET        = $5418;
  TIOCGSOFTCAR    = $5419;
  TIOCSSOFTCAR    = $541A;
  FIONREAD        = $541B;
  TIOCINQ         = FIONREAD;
  TIOCLINUX       = $541C;
  TIOCCONS        = $541D;
  TIOCGSERIAL     = $541E;
  TIOCSSERIAL     = $541F;
  TIOCPKT         = $5420;
  FIONBIO         = $5421;
  TIOCNOTTY       = $5422;
  TIOCSETD        = $5423;
  TIOCGETD        = $5424;
  TCSBRKP         = $5425;
  TIOCTTYGSTRUCT  = $5426;
  FIONCLEX        = $5450;
  FIOCLEX         = $5451;
  FIOASYNC        = $5452;
  TIOCSERCONFIG   = $5453;
  TIOCSERGWILD    = $5454;
  TIOCSERSWILD    = $5455;
  TIOCGLCKTRMIOS  = $5456;
  TIOCSLCKTRMIOS  = $5457;
  TIOCSERGSTRUCT  = $5458;
  TIOCSERGETLSR   = $5459;
  TIOCSERGETMULTI = $545A;
  TIOCSERSETMULTI = $545B;

  TIOCMIWAIT      = $545C;
  TIOCGICOUNT     = $545D;
  TIOCPKT_DATA       = 0;
  TIOCPKT_FLUSHREAD  = 1;
  TIOCPKT_FLUSHWRITE = 2;
  TIOCPKT_STOP       = 4;
  TIOCPKT_START      = 8;
  TIOCPKT_NOSTOP     = 16;
  TIOCPKT_DOSTOP     = 32;
{$else}

{$endif}
Type
  winsize = packed record
    ws_row,
    ws_col,
    ws_xpixel,
    ws_ypixel : word;
  end;
  TWinSize=winsize;

  Termio = packed record
    c_iflag,                             { input mode flags }
    c_oflag,                             { output mode flags }
    c_cflag,                             { control mode flags }
    c_lflag  : Word;                    { local mode flags }
    c_line   : Word;                    { line discipline - careful, only High byte in use}
    c_cc     : array [0..NCC-1] of char;{ control characters }
  end;
  TTermio=Termio;

{$PACKRECORDS C}
  Termios = record
    c_iflag,
    c_oflag,
    c_cflag,
    c_lflag  : Cardinal;
    c_line   : char;
    c_cc     : array[0..NCCS-1] of byte;
    c_ispeed,
    c_ospeed : longint;
  end;
  TTermios=Termios;
{$PACKRECORDS Default}


{const
  InitCC:array[0..NCCS-1] of byte=(3,34,177,25,4,0,1,0,21,23,32,0,22,17,27,26,0,0,0);}

const
{c_cc characters}
   VINTR    = 0;
   VQUIT    = 1;
   VERASE   = 2;
   VKILL    = 3;
   VEOF     = 4;
   VTIME    = 5;
   VMIN     = 6;
   VSWTC    = 7;
   VSTART   = 8;
   VSTOP    = 9;
   VSUSP    = 10;
   VEOL     = 11;
   VREPRINT = 12;
   VDISCARD = 13;
   VWERASE  = 14;
   VLNEXT   = 15;
   VEOL2    = 16;

{c_iflag bits}
   IGNBRK  = $0000001;
   BRKINT  = $0000002;
   IGNPAR  = $0000004;
   PARMRK  = $0000008;
   INPCK   = $0000010;
   ISTRIP  = $0000020;
   INLCR   = $0000040;
   IGNCR   = $0000080;
   ICRNL   = $0000100;
   IUCLC   = $0000200;
   IXON    = $0000400;
   IXANY   = $0000800;
   IXOFF   = $0001000;
   IMAXBEL = $0002000;

{c_oflag bits}
   OPOST  = $0000001;
   OLCUC  = $0000002;
   ONLCR  = $0000004;
   OCRNL  = $0000008;
   ONOCR  = $0000010;
   ONLRET = $0000020;
   OFILL  = $0000040;
   OFDEL  = $0000080;
   NLDLY  = $0000100;
     NL0  = $0000000;
     NL1  = $0000100;
   CRDLY  = $0000600;
     CR0  = $0000000;
     CR1  = $0000200;
     CR2  = $0000400;
     CR3  = $0000600;
   TABDLY = $0001800;
     TAB0 = $0000000;
     TAB1 = $0000800;
     TAB2 = $0001000;
     TAB3 = $0001800;
    XTABS = $0001800;
   BSDLY  = $0002000;
     BS0  = $0000000;
     BS1  = $0002000;
   VTDLY  = $0004000;
     VT0  = $0000000;
     VT1  = $0004000;
   FFDLY  = $0008000;
     FF0  = $0000000;
     FF1  = $0008000;

{c_cflag bits}
   CBAUD   = $000100F;
   B0      = $0000000;
   B50     = $0000001;
   B75     = $0000002;
   B110    = $0000003;
   B134    = $0000004;
   B150    = $0000005;
   B200    = $0000006;
   B300    = $0000007;
   B600    = $0000008;
   B1200   = $0000009;
   B1800   = $000000A;
   B2400   = $000000B;
   B4800   = $000000C;
   B9600   = $000000D;
   B19200  = $000000E;
   B38400  = $000000F;
   EXTA    = B19200;
   EXTB    = B38400;
   CSIZE   = $0000030;
     CS5   = $0000000;
     CS6   = $0000010;
     CS7   = $0000020;
     CS8   = $0000030;
   CSTOPB  = $0000040;
   CREAD   = $0000080;
   PARENB  = $0000100;
   PARODD  = $0000200;
   HUPCL   = $0000400;
   CLOCAL  = $0000800;
   CBAUDEX = $0001000;
   B57600  = $0001001;
   B115200 = $0001002;
   B230400 = $0001003;
   B460800 = $0001004;
   CIBAUD  = $100F0000;
   CMSPAR  = $40000000;
   CRTSCTS = $80000000;

{c_lflag bits}
   ISIG    = $0000001;
   ICANON  = $0000002;
   XCASE   = $0000004;
   ECHO    = $0000008;
   ECHOE   = $0000010;
   ECHOK   = $0000020;
   ECHONL  = $0000040;
   NOFLSH  = $0000080;
   TOSTOP  = $0000100;
   ECHOCTL = $0000200;
   ECHOPRT = $0000400;
   ECHOKE  = $0000800;
   FLUSHO  = $0001000;
   PENDIN  = $0004000;
   IEXTEN  = $0008000;

{c_line bits}
   TIOCM_LE   = $001;
   TIOCM_DTR  = $002;
   TIOCM_RTS  = $004;
   TIOCM_ST   = $008;
   TIOCM_SR   = $010;
   TIOCM_CTS  = $020;
   TIOCM_CAR  = $040;
   TIOCM_RNG  = $080;
   TIOCM_DSR  = $100;
   TIOCM_CD   = TIOCM_CAR;
   TIOCM_RI   = TIOCM_RNG;
   TIOCM_OUT1 = $2000;
   TIOCM_OUT2 = $4000;

{TCSetAttr}
   TCSANOW   = 0;
   TCSADRAIN = 1;
   TCSAFLUSH = 2;

{TCFlow}
   TCOOFF = 0;
   TCOON  = 1;
   TCIOFF = 2;
   TCION  = 3;

{TCFlush}
   TCIFLUSH  = 0;
   TCOFLUSH  = 1;
   TCIOFLUSH = 2;



{********************
      Info
********************}

Type

  UTimBuf = packed record{in BSD array[0..1] of timeval, but this is
                                backwards compatible with linux version}
    actime,
    modtime
         : longint;
  end;
  UTimeBuf=UTimBuf;
  TUTimeBuf=UTimeBuf;
  PUTimeBuf=^UTimeBuf;

  TSysinfo = packed record
    uptime    : longint;
    loads     : array[1..3] of longint;
    totalram,
    freeram,
    sharedram,
    bufferram,
    totalswap,
    freeswap  : longint;
    procs     : integer;
    s         : string[18];
  end;
  PSysInfo = ^TSysInfo;

{******************************************************************************
                            Procedure/Functions
******************************************************************************}

Function SysCall(callnr:longint;var regs:SysCallregs):longint;

{**************************
     Time/Date Handling
***************************}

var
  tzdaylight : boolean;
  tzseconds  : longint;
  tzname     : array[boolean] of pchar;

{ timezone support }
procedure GetLocalTimezone(timer:longint;var leap_correct,leap_hit:longint);
procedure GetLocalTimezone(timer:longint);
procedure ReadTimezoneFile(fn:string);
function  GetTimezoneFile:string;

Procedure GetTimeOfDay(var tv:timeval);
Function  GetTimeOfDay:longint;
Function  GetEpochTime: longint;
Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
Function  LocalToEpoch(year,month,day,hour,minute,second:Word):Longint;
procedure GetTime(var hour,min,sec,msec,usec:word);
procedure GetTime(var hour,min,sec,sec100:word);
procedure GetTime(var hour,min,sec:word);
Procedure GetDate(Var Year,Month,Day:Word);
Procedure GetDateTime(Var Year,Month,Day,hour,minute,second:Word);
function SetTime(Hour,Min,Sec:word) : Boolean;
function SetDate(Year,Month,Day:Word) : Boolean;
function SetDateTime(Year,Month,Day,hour,minute,second:Word) : Boolean;

{**************************
     Process Handling
***************************}

function  CreateShellArgV(const prog:string):ppchar;
function  CreateShellArgV(const prog:Ansistring):ppchar;
Procedure Execve(Path: pathstr;args:ppchar;ep:ppchar);
Procedure Execve(Path: AnsiString;args:ppchar;ep:ppchar);
Procedure Execve(path: pchar;args:ppchar;ep:ppchar);
Procedure Execv(const path:pathstr;args:ppchar);
Procedure Execv(const path: AnsiString;args:ppchar);
Procedure Execvp(Path: Pathstr;Args:ppchar;Ep:ppchar);
Procedure Execvp(Path: AnsiString; Args:ppchar;Ep:ppchar);
Procedure Execl(const Todo: String);
Procedure Execl(const Todo: Ansistring);
Procedure Execle(Todo: String;Ep:ppchar);
Procedure Execle(Todo: AnsiString;Ep:ppchar);
Procedure Execlp(Todo: string;Ep:ppchar);
Procedure Execlp(Todo: Ansistring;Ep:ppchar);
Function  Shell(const Command:String):Longint;
Function  Shell(const Command:AnsiString):Longint;
Function  Fork:longint;
{Clone for FreeBSD is copied from the LinuxThread port, and rfork based}
function  Clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
Procedure ExitProcess(val:longint);
Function  WaitPid(Pid:longint;Status:pointer;Options:Longint):Longint;  {=>PID (Status Valid), 0 (No Status), -1: Error, special case errno=EINTR }
Function  WaitProcess(Pid:longint):Longint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}
Procedure Nice(N:integer);
Function  GetPriority(Which,Who:Integer):integer;
Procedure SetPriority(Which:Integer;Who:Integer;What:Integer);
function WEXITSTATUS(Status: Integer): Integer;
function WTERMSIG(Status: Integer): Integer;
function WSTOPSIG(Status: Integer): Integer;
Function WIFEXITED(Status: Integer): Boolean;
Function WIFSTOPPED(Status: Integer): Boolean;
Function WIFSIGNALED(Status: Integer): Boolean;
Function W_EXITCODE(ReturnCode, Signal: Integer): Integer;
Function W_STOPCODE(Signal: Integer): Integer;

Function  GetPid:LongInt;
Function  GetPPid:LongInt;
Function  GetUid:Longint;
Function  GetEUid:Longint;
Function  GetGid:Longint;
Function  GetEGid:Longint;

{**************************
     File Handling
***************************}

Function  fdOpen(pathname:string;flags:longint):longint;
Function  fdOpen(pathname:string;flags,mode:longint):longint;
Function  fdOpen(pathname:pchar;flags:longint):longint;
Function  fdOpen(pathname:pchar;flags,mode:longint):longint;
Function  fdClose(fd:longint):boolean;
Function  fdRead(fd:longint;var buf;size:longint):longint;
Function  fdWrite(fd:longint;const buf;size:longint):longint;
Function  fdTruncate(fd,size:longint):boolean;
Function  fdSeek (fd,pos,seektype :longint): longint;
Function  fdFlush (fd : Longint) : Boolean;
Function  Link(OldPath,NewPath:pathstr):boolean;
Function  SymLink(OldPath,NewPath:pathstr):boolean;
Function  ReadLink(name,linkname:pchar;maxlen:longint):longint;
Function  ReadLink(name:pathstr):pathstr;
Function  UnLink(Path:pathstr):boolean;
Function  UnLink(Path:pchar):Boolean;
Function  FReName (OldName,NewName : Pchar) : Boolean;
Function  FReName (OldName,NewName : String) : Boolean;
Function  Chown(path:pathstr;NewUid,NewGid:longint):boolean;
Function  Chmod(path:pathstr;Newmode:longint):boolean;
Function  Utime(const path:pathstr;utim:utimebuf):boolean;
Function  Access(Path:Pathstr ;mode:integer):boolean;
Function  Umask(Mask:Integer):integer;
Function  Flock (fd,mode : longint) : boolean;
Function  Flock (var T : text;mode : longint) : boolean;
Function  Flock (var F : File;mode : longint) : boolean;
Function  FStat(Path:Pathstr;Var Info:stat):Boolean;
Function  FStat(Fd:longint;Var Info:stat):Boolean;
Function  FStat(var F:Text;Var Info:stat):Boolean;
Function  FStat(var F:File;Var Info:stat):Boolean;
Function  Lstat(Filename: PathStr;var Info:stat):Boolean;
Function  FSStat(Path:Pathstr;Var Info:statfs):Boolean;
Function  FSStat(Fd: Longint;Var Info:statfs):Boolean;
Function  Fcntl(Fd:longint;Cmd:longint):longint;
Procedure Fcntl(Fd:longint;Cmd:longint;Arg:Longint);
Function  Fcntl(var Fd:Text;Cmd:longint):longint;
Procedure Fcntl(var Fd:Text;Cmd:longint;Arg:Longint);
Function  Dup(oldfile:longint;var newfile:longint):Boolean;
Function  Dup(var oldfile,newfile:text):Boolean;
Function  Dup(var oldfile,newfile:file):Boolean;
Function  Dup2(oldfile,newfile:longint):Boolean;
Function  Dup2(var oldfile,newfile:text):Boolean;
Function  Dup2(var oldfile,newfile:file):Boolean;
Function  Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:PTimeVal):longint;
Function  Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:Longint):longint;
Function  SelectText(var T:Text;TimeOut :PTimeVal):Longint;
Function  SelectText(var T:Text;TimeOut :Longint):Longint;

{**************************
   Directory Handling
***************************}

{$ifndef newreaddir}    {only for FreeBSD, temporary solution}

Function  OpenDir(f:pchar):pdir;
Function  OpenDir(f: String):pdir;
function  CloseDir(p:pdir):integer;
Function  ReadDir(p:pdir):pdirent;
procedure SeekDir(p:pdir;off:longint);
function  TellDir(p:pdir):longint;
{$else}
Function  OpenDir(name:pchar):pdir;
Function  OpenDir(f: String):pdir;
function  CloseDir(dirp:pdir):integer;
Function  ReadDir(p:pdir):pdirent;
procedure SeekDir(dirp:pdir;loc:longint);
function  TellDir(dirp:pdir):longint;

{$endif}

{**************************
    Pipe/Fifo/Stream
***************************}

Function  AssignPipe(var pipe_in,pipe_out:longint):boolean;
Function  AssignPipe(var pipe_in,pipe_out:text):boolean;
Function  AssignPipe(var pipe_in,pipe_out:file):boolean;
Function  PClose(Var F:text) : longint;
Function  PClose(Var F:file) : longint;
Procedure POpen(var F:text;const Prog:String;rw:char);
Procedure POpen(var F:file;const Prog:String;rw:char);

Function  mkFifo(pathname:string;mode:longint):boolean;

function AssignStream(Var StreamIn,Streamout:text;Const Prog:String) : longint;
function AssignStream(var StreamIn, StreamOut, StreamErr: Text; const prog: String): LongInt;

{**************************
    General information
***************************}

Function  GetEnv(P:string):Pchar;

Function  GetDomainName:String;
Function  GetHostName:String;
Function  Sysinfo(var Info:TSysinfo):Boolean;
Function  Uname(var unamerec:utsname):Boolean;
{**************************
        Signal
***************************}

Procedure SigAction(Signum:longint;Act,OldAct:PSigActionRec );
Procedure SigProcMask (How:longint;SSet,OldSSet:PSigSet);
Function  SigPending:SigSet;
Procedure SigSuspend(Mask:Sigset);
Function  Signal(Signum:longint;Handler:SignalHandler):SignalHandler;
Function  Kill(Pid:longint;Sig:longint):integer;
Procedure SigRaise(Sig:integer);
  Function  Alarm(Sec : Longint) : longint;

Procedure Pause;
Function NanoSleep(const req : timespec;var rem : timespec) : longint;

{**************************
  IOCtl/Termios Functions
***************************}

Function  IOCtl(Handle,Ndx: Longint;Data: Pointer):boolean;
Function  TCGetAttr(fd:longint;var tios:TermIOS):boolean;
Function  TCSetAttr(fd:longint;OptAct:longint;const tios:TermIOS):boolean;
Procedure CFSetISpeed(var tios:TermIOS;speed:Cardinal);
Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal);
Procedure CFMakeRaw(var tios:TermIOS);
Function  TCSendBreak(fd,duration:longint):boolean;
Function  TCSetPGrp(fd,id:longint):boolean;
Function  TCGetPGrp(fd:longint;var id:longint):boolean;
Function  TCFlush(fd,qsel:longint):boolean;
Function  TCDrain(fd:longint):boolean;
Function  TCFlow(fd,act:longint):boolean;
Function  IsATTY(Handle:Longint):Boolean;
Function  IsATTY(var f:text):Boolean;
function  TTYname(Handle:Longint):string;
function  TTYname(var F:Text):string;

{**************************
     Memory functions
***************************}

const
  PROT_READ  = $1;             { page can be read }
  PROT_WRITE = $2;             { page can be written }
  PROT_EXEC  = $4;             { page can be executed }
  PROT_NONE  = $0;             { page can not be accessed }

  MAP_SHARED    = $1;          { Share changes }
//  MAP_PRIVATE   = $2;          { Changes are private }
  MAP_TYPE      = $f;          { Mask for type of mapping }
  MAP_FIXED     = $10;         { Interpret addr exactly }
//  MAP_ANONYMOUS = $20;         { don't use a file }

{$if defined(cpumips) or defined(cpumipsel)}
  MAP_GROWSDOWN  = $1000;       { stack-like segment }
  MAP_DENYWRITE  = $2000;       { ETXTBSY }
  MAP_EXECUTABLE = $4000;      { mark it as an executable }
  MAP_LOCKED     = $8000;      { pages are locked }
  MAP_NORESERVE  = $4000;      { don't check for reservations }
{$else cpumips}
  MAP_GROWSDOWN  = $100;       { stack-like segment }
  MAP_DENYWRITE  = $800;       { ETXTBSY }
  MAP_EXECUTABLE = $1000;      { mark it as an executable }
  MAP_LOCKED     = $2000;      { pages are locked }
  MAP_NORESERVE  = $4000;      { don't check for reservations }
{$endif cpumips}


type
  tmmapargs=record
    address : longint;
    size    : longint;
    prot    : longint;
    flags   : longint;
    fd      : longint;
    offset  : longint;
  end;

function MMap(const m:tmmapargs):longint;
function MUnMap (P : Pointer; Size : Longint) : Boolean;

{**************************
     Port IO functions
***************************}

Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
Function IoPL(Level : longint) : Boolean;
{$ifdef cpui386}
Procedure WritePort (Port : Longint; Value : Byte);oldfpccall;
Procedure WritePort (Port : Longint; Value : Word);oldfpccall;
Procedure WritePort (Port : Longint; Value : Longint);oldfpccall;
Procedure WritePortB (Port : Longint; Value : Byte);oldfpccall;
Procedure WritePortW (Port : Longint; Value : Word);oldfpccall;
Procedure WritePortL (Port : Longint; Value : Longint);oldfpccall;
Procedure WritePortL (Port : Longint; Var Buf; Count: longint);oldfpccall;
Procedure WritePortW (Port : Longint; Var Buf; Count: longint);oldfpccall;
Procedure WritePortB (Port : Longint; Var Buf; Count: longint);oldfpccall;
Procedure ReadPort (Port : Longint; Var Value : Byte);oldfpccall;
Procedure ReadPort (Port : Longint; Var Value : Word);oldfpccall;
Procedure ReadPort (Port : Longint; Var Value : Longint);oldfpccall;
function  ReadPortB (Port : Longint): Byte;oldfpccall;
function  ReadPortW (Port : Longint): Word;oldfpccall;
function  ReadPortL (Port : Longint): LongInt;oldfpccall;
Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);oldfpccall;
Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);oldfpccall;
Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);oldfpccall;
{$endif}

{**************************
    Utility functions
***************************}

Function  Octal(l:longint):longint;
Function  FExpand(Const Path: PathStr):PathStr;
Function  FSearch(const path:pathstr;dirlist:string):pathstr;
Procedure FSplit(const Path:PathStr;Var Dir:DirStr;Var Name:NameStr;Var Ext:ExtStr);
Function  Dirname(Const path:pathstr):pathstr;
Function  Basename(Const path:pathstr;Const suf:pathstr):pathstr;
Function  FNMatch(const Pattern,Name:string):Boolean;
Function  Glob(Const path:pathstr):pglob;
Procedure Globfree(var p:pglob);
Function  StringToPPChar(Var S:String):ppchar;
Function  StringToPPChar(Var S:AnsiString):ppchar;
Function  StringToPPChar(S : Pchar):ppchar;
Function  GetFS(var T:Text):longint;
Function  GetFS(Var F:File):longint;
{Filedescriptorsets}
Procedure FD_Zero(var fds:fdSet);
Procedure FD_Clr(fd:longint;var fds:fdSet);
Procedure FD_Set(fd:longint;var fds:fdSet);
Function  FD_IsSet(fd:longint;var fds:fdSet):boolean;
{Stat.Mode Types}
Function S_ISLNK(m:word):boolean;
Function S_ISREG(m:word):boolean;
Function S_ISDIR(m:word):boolean;

Function S_ISCHR(m:word):boolean;
Function S_ISBLK(m:word):boolean;
Function S_ISFIFO(m:word):boolean;
Function S_ISSOCK(m:word):boolean;


{******************************************************************************
                            Implementation
******************************************************************************}

Implementation

Uses Strings;

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}


{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{$ifdef FPC_PROFILE}
  {$define PROFILE_WAS_ACTIVE}
  {$profile off}
{$else}
  {$undef PROFILE_WAS_ACTIVE}
{$endif}


Procedure Do_SysCall( callnr:longint;var regs : SysCallregs );oldfpccall;assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
{$ifdef cpui386}
{$ASMMODE ATT}
asm
{ load the registers... }
  movl 12(%ebp),%eax
  movl 4(%eax),%ebx
  movl 8(%eax),%ecx
  movl 12(%eax),%edx
  movl 16(%eax),%esi
  movl 20(%eax),%edi
{ set the call number }
  movl 8(%ebp),%eax
{ Go ! }
  int $0x80
{ Put back the registers... }
  pushl %eax
  movl 12(%ebp),%eax
  movl %edi,20(%eax)
  movl %esi,16(%eax)
  movl %edx,12(%eax)
  movl %ecx,8(%eax)
  movl %ebx,4(%eax)
  popl %ebx
  movl %ebx,(%eax)
end;
{$ASMMODE DEFAULT}
{$else}
{$ifdef cpum68k}
asm
{ load the registers... }
  move.l 12(a6),a0
  move.l 4(a0),d1
  move.l 8(a0),d2
  move.l 12(a0),d3
  move.l 16(a0),d4
  move.l 20(a0),d5
{ set the call number }
  move.l 8(a6),d0
{ Go ! }
  trap #0
{ Put back the registers... }
  move.l d0,-(sp)
  move.l 12(a6),a0
  move.l d5,20(a0)
  move.l d4,16(a0)
  move.l d3,12(a0)
  move.l d2,8(a0)
  move.l d1,4(a0)
  move.l (sp)+,d1
  move.l d1,(a0)
end;
{$else}
{$error Cannot decide which processor you have ! define cpui386 or m68k }
{$endif}
{$endif}

{$IFDEF SYSCALL_DEBUG}
Const
  DoSysCallDebug : Boolean = False;

var
  LastCnt,
  LastEax,
  LastCall : longint;
  DebugTxt : string[20];
{$ENDIF}
Function SysCall( callnr:longint;var regs : SysCallregs ):longint;
{
  This function serves as an interface to do_SysCall.
  If the SysCall returned a negative number, it returns -1, and puts the
  SysCall result in errno. Otherwise, it returns the SysCall return value
}
begin
  do_SysCall(callnr,regs);
  if (regs.reg1<0) and (regs.reg1>=-Sys_ERROR_MAX) then
   begin
{$IFDEF SYSCALL_DEBUG}
     If DoSysCallDebug then
       debugtxt:=' syscall error: ';
{$endif}
     ErrNo:=-regs.reg1;
     SysCall:=-1;
   end
  else
   begin
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
       debugtxt:=' syscall returned: ';
{$endif}
     SysCall:=regs.reg1;
     errno:=0
   end;
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
    begin
    inc(lastcnt);
    if (callnr<>lastcall) or (regs.reg1<>lasteax) then
      begin
      if lastcnt>1 then
        writeln(sys_nr_txt[lastcall],debugtxt,lasteax,' (',lastcnt,'x)');
      lastcall:=callnr;
      lasteax:=regs.reg1;
      lastcnt:=0;
      writeln(sys_nr_txt[lastcall],debugtxt,lasteax);
      end;
    end;
{$endif}
end;

{$ifdef PROFILE_WAS_ACTIVE}
  {$profile on}
  {$undef PROFILE_WAS_ACTIVE}
{$endif}


Function Sys_Time:longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=0;
  Sys_Time:=SysCall(SysCall_nr_time,regs);
end;


{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}


Function Sys_Open(f:pchar;flags:longint;mode:integer):longint;
var
  regs : SysCallregs;
Begin
  regs.reg2:=longint(f);
  regs.reg3:=flags;
  regs.reg4:=mode;
  Sys_Open:=SysCall(SysCall_nr_open,regs);
End;



Function Sys_Close(f:longint):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=f;
  Sys_Close:=SysCall(SysCall_nr_close,regs);
end;



Function Sys_Lseek(F:longint;Off:longint;Whence:longint):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=f;
  regs.reg3:=off;
  regs.reg4:=Whence;
  Sys_lseek:=SysCall(SysCall_nr_lseek,regs);
end;



Function Sys_Read(f:longint;buffer:pchar;count:longint):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=f;
  regs.reg3:=longint(buffer);
  regs.reg4:=count;
  Sys_Read:=SysCall(SysCall_nr_read,regs);
end;



Function Sys_Write(f:longint;buffer:pchar;count:longint):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=f;
  regs.reg3:=longint(buffer);
  regs.reg4:=count;
  Sys_Write:=SysCall(SysCall_nr_write,regs);
end;



Function Sys_Unlink(Filename:pchar):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  Sys_Unlink:=SysCall(SysCall_nr_unlink,regs);
end;


Function Sys_fstat(fd : longint;var Info:stat):Longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=fd;
  regs.reg3:=longint(@Info);
  Sys_fStat:=SysCall(SysCall_nr_fstat,regs);
end;


Function Sys_Rename(Oldname,Newname:pchar):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(oldname);
  regs.reg3:=longint(newname);
  Sys_Rename:=SysCall(SysCall_nr_rename,regs);
end;



Function Sys_Stat(Filename:pchar;var Buffer: stat):longint;
{
   We need this for getcwd
}
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  regs.reg3:=longint(@buffer);
  Sys_Stat:=SysCall(SysCall_nr_stat,regs);
end;


Function Sys_Symlink(oldname,newname:pchar):longint;
{
  We need this for erase
}
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(oldname);
  regs.reg3:=longint(newname);
  Sys_symlink:=SysCall(SysCall_nr_symlink,regs);
end;


Function Sys_ReadLink(name,linkname:pchar;maxlen:longint):longint;
var
  regs : SysCallRegs;
begin
  regs.reg2:=longint(name);
  regs.reg3:=longint(linkname);
  regs.reg4:=maxlen;
  Sys_ReadLink:=SysCall(Syscall_nr_readlink,regs);
end;


{*****************************************************************************
               --- Directory:Directory related calls ---
*****************************************************************************}


Function Sys_Chdir(Filename:pchar):longint;
var
  regs : SysCallregs;

begin
  regs.reg2:=longint(filename);
  Sys_ChDir:=SysCall(SysCall_nr_chdir,regs);
end;



Function Sys_Mkdir(Filename:pchar;mode:longint):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  regs.reg3:=mode;
  Sys_MkDir:=SysCall(SysCall_nr_mkdir,regs);
end;



Function Sys_Rmdir(Filename:pchar):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  Sys_Rmdir:=SysCall(SysCall_nr_rmdir,regs);
end;



{ we need this for getcwd }
Function OpenDir(f:pchar):pdir;
var
  fd:integer;
  st:stat;
  ptr:pdir;
begin
  opendir:=nil;
  if sys_stat(f,st)<0 then
   exit;
{ Is it a dir ? }
  if not((st.mode and $f000)=$4000)then
   begin
     errno:=sys_enotdir;
     exit
   end;
{ Open it}
  fd:=sys_open(f,OPEN_RDONLY,438);
  if fd<0 then
   exit;
  new(ptr);
  if ptr=nil then
   exit;
  new(ptr^.buf);
  if ptr^.buf=nil then
   exit;
  ptr^.fd:=fd;
  ptr^.loc:=0;
  ptr^.size:=0;
  ptr^.dd_max:=sizeof(ptr^.buf^);
  opendir:=ptr;
end;



function CloseDir(p:pdir):integer;
begin
  closedir:=sys_close(p^.fd);
  dispose(p^.buf);
  dispose(p);
end;



Function Sys_ReadDir(p:pdir):pdirent;
var
  regs :SysCallregs;
  dummy:longint;
begin
  regs.reg3:=longint(p^.buf);
  regs.reg2:=p^.fd;
  regs.reg4:=1;
  dummy:=SysCall(SysCall_nr_readdir,regs);
{ the readdir system call returns the number of bytes written }
  if dummy=0 then
   sys_readdir:=nil
  else
   sys_readdir:=p^.buf
end;


{*****************************************************************************
        --- Process:Process & program handling - related calls ---
*****************************************************************************}

Function Sys_GetPid:LongInt;
var
  regs : SysCallregs;
begin
  Sys_GetPid:=SysCall(SysCall_nr_getpid,regs);
end;


Procedure Sys_Exit(ExitCode:Integer);
var
  regs : SysCallregs;
begin
  regs.reg2:=exitcode;
  SysCall(SysCall_nr_exit,regs)
end;

Procedure SigAction(Signum:longint;Act,OldAct:PSigActionRec );
{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}
Var
  sr : Syscallregs;
begin
  sr.reg2:=Signum;
  sr.reg3:=Longint(act);
  sr.reg4:=Longint(oldact);
  SysCall(Syscall_nr_sigaction,sr);
end;

function Sys_FTruncate(Handle,Pos:longint):longint;  //moved from sysunix.inc Do_Truncate
var
  sr : syscallregs;
begin
  sr.reg2:=Handle;
  sr.reg3:=Pos;
  Sys_FTruncate:=syscall(syscall_nr_ftruncate,sr);
end;

Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint; // moved from sysunix.inc, used in sbrk
type
  tmmapargs=packed record
    address : longint;
    size    : longint;
    prot    : longint;
    flags   : longint;
    fd      : longint;
    offset  : longint;
  end;
var
  t     : syscallregs;
  mmapargs : tmmapargs;
begin
  mmapargs.address:=adr;
  mmapargs.size:=len;
  mmapargs.prot:=prot;
  mmapargs.flags:=flags;
  mmapargs.fd:=fdes;
  mmapargs.offset:=off;
  t.reg2:=longint(@mmapargs);
  do_syscall(syscall_nr_mmap,t);
  Sys_mmap:=t.reg1;
  if t.reg1=-1 then
    errno:=-1;
end;

{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
Function Sys_IOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  // This was missing here, instead hardcode in Do_IsDevice
var
  sr: SysCallRegs;
begin
  sr.reg2:=Handle;
  sr.reg3:=Ndx;
  sr.reg4:=Longint(Data);
  Sys_IOCtl:=SysCall(Syscall_nr_ioctl,sr);
end;


Function Sys_SigAltStack(ss, oss :psigaltstack):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(ss);
  regs.reg3:=longint(oss);
  sys_sigaltstack:=SysCall(syscall_nr_sigaltstack,regs);
end;

Function Fork:longint;
{
  This function issues the 'fork' System call. the program is duplicated in memory
  and Execution continues in parent and child process.
  In the parent process, fork returns the PID of the child. In the child process,
  zero is returned.
  A negative value indicates that an error has occurred, the error is returned in
  LinuxError.
}
var
  regs:SysCallregs;
begin
  Fork:=SysCall(SysCall_nr_fork,regs);
  LinuxError:=Errno;
End;


function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
begin
  if (pointer(func)=nil) or (sp=nil) then
   begin
     LinuxError:=Sys_EInval;
     exit(-1); // give an error result
   end;
{$ifdef cpui386}
{$ASMMODE ATT}
  asm
        { Insert the argument onto the new stack. }
        movl    sp,%ecx
        subl    $8,%ecx
        movl    args,%eax
        movl    %eax,4(%ecx)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movl    func,%eax
        movl    %eax,0(%ecx)

        { Do the system call }
        pushl   %ebx
        movl    flags,%ebx
        movl    SysCall_nr_clone,%eax
        int     $0x80
        popl    %ebx
        test    %eax,%eax
        jnz     .Lclone_end

        { We're in the new thread }
        subl    %ebp,%ebp       { terminate the stack frame }
        call    *%ebx
        { exit process }
        movl    %eax,%ebx
        movl    $1,%eax
        int     $0x80

.Lclone_end:
        movl    %eax,__RESULT
  end;
{$endif cpui386}
{$ifdef cpum68k}
  { No yet translated, my m68k assembler is too weak for such things PM }
(*
  asm
        { Insert the argument onto the new stack. }
        movl    sp,%ecx
        subl    $8,%ecx
        movl    args,%eax
        movl    %eax,4(%ecx)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movl    func,%eax
        movl    %eax,0(%ecx)

        { Do the system call }
        pushl   %ebx
        movl    flags,%ebx
        movl    SysCall_nr_clone,%eax
        int     $0x80
        popl    %ebx
        test    %eax,%eax
        jnz     .Lclone_end

        { We're in the new thread }
        subl    %ebp,%ebp       { terminate the stack frame }
        call    *%ebx
        { exit process }
        movl    %eax,%ebx
        movl    $1,%eax
        int     $0x80

.Lclone_end:
        movl    %eax,__RESULT
  end;
  *)
{$endif cpum68k}
end;


Procedure Execve(path:pathstr;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}
var
  regs:SysCallregs;
begin
  path:=path+#0;
  regs.reg2:=longint(@path[1]);
  regs.reg3:=longint(args);
  regs.reg4:=longint(ep);
  SysCall(SysCall_nr_Execve,regs);
{ This only gets set when the call fails, otherwise we don't get here ! }
  Linuxerror:=errno;
end;


Procedure Execve(path:pchar;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}
var
  regs:SysCallregs;
begin
  regs.reg2:=longint(path);
  regs.reg3:=longint(args);
  regs.reg4:=longint(ep);
  SysCall(SysCall_nr_Execve,regs);
{ This only gets set when the call fails, otherwise we don't get here ! }
  Linuxerror:=errno;
end;

Procedure ExitProcess(val:longint);
var
  regs : SysCallregs;
begin
  regs.reg2:=val;
  SysCall(SysCall_nr_exit,regs);
end;


Function WaitPid(Pid:longint;Status:pointer;Options:Longint):Longint;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}
var
  regs : SysCallregs;
begin
  regs.reg2:=pid;
  regs.reg3:=longint(status);
  regs.reg4:=options;
  WaitPid:=SysCall(SysCall_nr_waitpid,regs);
  LinuxError:=errno;
end;


Procedure GetTimeOfDay(var tv:timeval);
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(@tv);
  regs.reg3:=0;
  SysCall(SysCall_nr_gettimeofday,regs);
  LinuxError:=Errno;
end;

Function GetPriority(Which,Who:Integer):integer;
{
  Get Priority of process, process group, or user.
   Which : selects what kind of priority is used.
           can be one of the following predefined Constants :
              Prio_User.
              Prio_PGrp.
              Prio_Process.
   Who : depending on which, this is , respectively :
              Uid
              Pid
              Process Group id
   Errors are reported in linuxerror _only_. (priority can be negative)
}
var
  sr : Syscallregs;
begin
  errno:=0;
  if (which<prio_process) or (which>prio_user) then
   begin
     { We can save an interrupt here }
     getpriority:=0;
     linuxerror:=Sys_einval;
   end
  else
   begin
     sr.reg2:=which;
     sr.reg3:=who;
     getpriority:=SysCall(Syscall_nr_getpriority,sr);
     linuxerror:=errno;
   end;
end;



Procedure SetPriority(Which:Integer;Who:Integer;What:Integer);
{
 Set Priority of process, process group, or user.
   Which : selects what kind of priority is used.
           can be one of the following predefined Constants :
              Prio_User.
              Prio_PGrp.
              Prio_Process.
   Who : depending on value of which, this is, respectively :
              Uid
              Pid
              Process Group id
   what : A number between -20 and 20. -20 is most favorable, 20 least.
          0 is the default.
}
var
  sr : Syscallregs;
begin
  errno:=0;
  if ((which<prio_process) or (which>prio_user)) or ((what<-20) or (what>20)) then
   linuxerror:=Sys_einval  { We can save an interrupt here }
  else
   begin
     sr.reg2:=which;
     sr.reg3:=who;
     sr.reg4:=what;
     SysCall(Syscall_nr_setpriority,sr);
     linuxerror:=errno;
   end;
end;


Procedure Nice(N:integer);
{
  Set process priority. A positive N means a lower priority.
  A negative N decreases priority.
}
var
  sr : Syscallregs;
begin
  sr.reg2:=n;
  SysCall(Syscall_nr_nice,sr);
  linuxerror:=errno;
end;



Function GetPid:LongInt;
{
  Get Process ID.
}
var
  regs : SysCallregs;
begin
  GetPid:=SysCall(SysCall_nr_getpid,regs);
  linuxerror:=errno;
end;



Function GetPPid:LongInt;
{
  Get Process ID of parent process.
}
var
  regs : SysCallregs;
begin
  GetPpid:=SysCall(SysCall_nr_getppid,regs);
  linuxerror:=errno;
end;



Function GetUid:Longint;
{
  Get User ID.
}
var
  regs : SysCallregs;
begin
  GetUid:=SysCall(SysCall_nr_getuid,regs);
  Linuxerror:=errno;
end;



Function GetEUid:Longint;
{
  Get _effective_ User ID.
}
var
  regs : SysCallregs;
begin
  GetEuid:=SysCall(SysCall_nr_geteuid,regs);
  Linuxerror:=errno;
end;



Function GetGid:Longint;
{
  Get Group ID.
}
var
  regs : SysCallregs;
begin
  Getgid:=SysCall(SysCall_nr_getgid,regs);
  Linuxerror:=errno;
end;



Function GetEGid:Longint;
{
  Get _effective_ Group ID.
}
var
  regs : SysCallregs;
begin
  GetEgid:=SysCall(SysCall_nr_getegid,regs);
  Linuxerror:=errno;
end;


Function GetTimeOfDay: longint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
var
  regs : SysCallregs;
  tv   : timeval;
begin
  regs.reg2:=longint(@tv);
  regs.reg3:=0;
  SysCall(SysCall_nr_gettimeofday,regs);
  LinuxError:=Errno;
  GetTimeOfDay:=tv.sec;
end;


Function fdTruncate(fd,size:longint):boolean;
var
  Regs : SysCallRegs;
begin
  Regs.reg2:=fd;
  Regs.reg3:=size;
  fdTruncate:=(SysCall(Syscall_nr_ftruncate,regs)=0);
  LinuxError:=Errno;
end;



Function  fdFlush (fd : Longint) : Boolean;
var
  SR: SysCallRegs;
begin
  SR.reg2 := fd;
  fdFlush := (SysCall(syscall_nr_fsync, SR)=0);
  LinuxError:=Errno;
end;



Function Fcntl(Fd:longint;Cmd:longint): longint;
{
  Read or manipulate a file.(See also fcntl (2) )
  Possible values for Cmd are :
    F_GetFd,F_GetFl,F_GetOwn
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=Sys_eninval.
}
var
  sr : Syscallregs;
begin
  if (cmd in [F_GetFd,F_GetFl,F_GetOwn]) then
   begin
     sr.reg2:=Fd;
     sr.reg3:=cmd;
     Linuxerror:=SysCall(Syscall_nr_fcntl,sr);
     if linuxerror=-1 then
      begin
        linuxerror:=errno;
        fcntl:=0;
      end
     else
      begin
        fcntl:=linuxerror;
        linuxerror:=0;
      end;
   end
  else
   begin
     linuxerror:=Sys_einval;
     Fcntl:=0;
   end;
end;



Procedure Fcntl(Fd:longint;Cmd:LongInt;Arg:Longint);
{
  Read or manipulate a file. (See also fcntl (2) )
  Possible values for Cmd are :
    F_setFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkW,F_SetOwn;
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=Sys_eninval.
  F_DupFD is not allowed, due to the structure of Files in Pascal.
}
var
  sr : Syscallregs;
begin
  if (cmd in [F_SetFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkw,F_SetOwn]) then
   begin
     sr.reg2:=Fd;
     sr.reg3:=cmd;
     sr.reg4:=arg;
     SysCall(Syscall_nr_fcntl,sr);
     linuxerror:=errno;
   end
  else
   linuxerror:=Sys_einval;
end;


Function Chmod(path:pathstr;Newmode:longint):Boolean;
{
  Changes the permissions of a file.
}
var
  sr : Syscallregs;
begin
  path:=path+#0;
  sr.reg2:=longint(@(path[1]));
  sr.reg3:=newmode;
  Chmod:=(SysCall(Syscall_nr_chmod,sr)=0);
  linuxerror:=errno;
end;



Function Chown(path:pathstr;NewUid,NewGid:longint):boolean;
{
  Change the owner and group of a file.
  A user can only change the group to a group of which he is a member.
  The super-user can change uid and gid of any file.
}
var
  sr : Syscallregs;
begin
  path:=path+#0;
  sr.reg2:=longint(@(path[1]));
  sr.reg3:=newuid;
  sr.reg4:=newgid;
  ChOwn:=(Syscall(Syscall_nr_chown,sr)=0);
  linuxerror:=errno;
end;



Function Utime(const path:pathstr;utim:utimebuf):boolean;
var
  sr : Syscallregs;
  buf : pathstr;
begin
  buf:=path+#0;
  sr.reg2:=longint(@(buf[1]));
  sr.reg3:=longint(@utim);
  Utime:=SysCall(Syscall_nr_utime,sr)=0;
  linuxerror:=errno;
end;



Function  Flock (fd,mode : longint) : boolean;
var
  sr : Syscallregs;
begin
  sr.reg2:=fd;
  sr.reg3:=mode;
  flock:=Syscall(Syscall_nr_flock,sr)=0;
  LinuxError:=errno;
end;



Function Fstat(Fd:Longint;var Info:stat):Boolean;
{
  Get all information on a file descriptor, and return it in info.
}
var
 regs : SysCallregs;
begin
  regs.reg2:=Fd;
  regs.reg3:=longint(@Info);
  FStat:=(SysCall(SysCall_nr_fstat,regs)=0);
  LinuxError:=Errno;
end;


Function Lstat(Filename: PathStr;var Info:stat):Boolean;
{
  Get all information on a link (the link itself), and return it in info.
}
var
  regs : SysCallregs;
begin
  FileName:=FileName+#0;
  regs.reg2:=longint(@filename[1]);
  regs.reg3:=longint(@Info);
  LStat:=(SysCall(SysCall_nr_lstat,regs)=0);
  LinuxError:=Errno;
end;



Function FSStat(Path:Pathstr;Var Info:statfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}
var
  regs : SysCallregs;
begin
  path:=path+#0;
  regs.reg2:=longint(@path[1]);
  regs.reg3:=longint(@Info);
  FSStat:=(SysCall(SysCall_nr_statfs,regs)=0);
  LinuxError:=errno;
end;



Function FSStat(Fd:Longint;Var Info:statfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}
var
  regs : SysCallregs;
begin
  regs.reg2:=Fd;
  regs.reg3:=longint(@Info);
  FSStat:=(SysCall(SysCall_nr_fstatfs,regs)=0);
  LinuxError:=errno;
end;



Function Link(OldPath,NewPath:pathstr):boolean;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
var
  regs : SysCallregs;
begin
  oldpath:=oldpath+#0;
  newpath:=newpath+#0;
  regs.reg2:=longint(@oldpath[1]);
  regs.reg3:=longint(@newpath[1]);
  Link:=SysCall(SysCall_nr_link,regs)=0;
  linuxerror:=errno;
end;




Function Umask(Mask:Integer):integer;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}
var
  sr : Syscallregs;
begin
  sr.reg2:=mask;
  Umask:=SysCall(Syscall_nr_umask,sr);
  linuxerror:=0;
end;



Function Access(Path:Pathstr ;mode:integer):boolean;
{
  Test users access rights on the specified file.
  Mode is a mask xosisting of one or more of R_OK, W_OK, X_OK, F_OK.
  R,W,X stand for read,write and Execute access, simultaneously.
  F_OK checks whether the test would be allowed on the file.
  i.e. It checks the search permissions in all directory components
  of the path.
  The test is done with the real user-ID, instead of the effective.
  If access is denied, or an error occurred, false is returned.
  If access is granted, true is returned.
  Errors other than no access,are reported in linuxerror.
}
var
  sr : Syscallregs;
begin
  path:=path+#0;
  sr.reg2:=longint(@(path[1]));
  sr.reg3:=mode;
  access:=(SysCall(Syscall_nr_access,sr)=0);
  linuxerror:=errno;
end;


Function  Dup(oldfile:longint;var newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}
var
  sr : Syscallregs;
begin
  sr.reg2:=oldfile;
  newfile:=Syscall(Syscall_nr_dup,sr);
  linuxerror:=errno;
  Dup:=(LinuxError=0);
end;


Function Dup2(oldfile,newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}
var
  sr : Syscallregs;
begin
  sr.reg2:=oldfile;
  sr.reg3:=newfile;
  SysCall(Syscall_nr_dup2,sr);
  linuxerror:=errno;
  Dup2:=(LinuxError=0);
end;


Function Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:PTimeVal):longint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
}
Var
  SelectArray : Array[1..5] of longint;
  Sr : Syscallregs;
begin
  SelectArray[1]:=n;
  SelectArray[2]:=longint(Readfds);
  Selectarray[3]:=longint(Writefds);
  selectarray[4]:=longint(exceptfds);
  Selectarray[5]:=longint(TimeOut);
  sr.reg2:=longint(@selectarray);
  Select:=SysCall(Syscall_nr_select,sr);
  LinuxError:=Errno;
end;



Function AssignPipe(var pipe_in,pipe_out:longint):boolean;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  pip  : tpipe;
  regs : SysCallregs;
begin
  regs.reg2:=longint(@pip);
  SysCall(SysCall_nr_pipe,regs);
  pipe_in:=pip[1];
  pipe_out:=pip[2];
  linuxerror:=errno;
  AssignPipe:=(LinuxError=0);
end;




Function PClose(Var F:text) :longint;
var
  sr  : syscallregs;
  pl  : ^longint;
  res : longint;
begin
  sr.reg2:=Textrec(F).Handle;
  SysCall (syscall_nr_close,sr);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=plongint(@(textrec(f).userdata[2]));
  waitpid(pl^,@res,0);
  pclose:=res shr 8;
end;


Function PClose(Var F:file) : longint;
var
  sr : syscallregs;
  pl : ^longint;
  res : longint;
begin
  sr.reg2:=FileRec(F).Handle;
  SysCall (Syscall_nr_close,sr);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=plongint(@(filerec(f).userdata[2]));
  waitpid(pl^,@res,0);
  pclose:=res shr 8;
end;


Function Sysinfo(var Info:TSysinfo):Boolean;
{
  Get system info
}
var
  regs : SysCallregs;
Begin
  regs.reg2:=longint(@info);
  Sysinfo:=SysCall(SysCall_nr_Sysinfo,regs)=0;
End;

Function mkFifo(pathname:string;mode:longint):boolean;
var
  regs : SysCallRegs;
begin
  pathname:=pathname+#0;
  regs.reg2:=longint(@pathname[1]);
  regs.reg3:=mode or STAT_IFIFO;
  regs.reg4:=0;
  mkFifo:=(SysCall(syscall_nr_mknod,regs)=0);
end;




Function Uname(var unamerec:utsname):Boolean;
{
  Get machine's names
}
var
  regs : SysCallregs;
Begin
  regs.reg2:=longint(@unamerec);
  Uname:=SysCall(SysCall_nr_uname,regs)=0;
  LinuxError:=Errno;
End;


Function Kill(Pid:longint;Sig:longint):integer;
{
  Send signal 'sig' to a process, or a group of processes.
  If Pid >  0 then the signal is sent to pid
     pid=-1                         to all processes except process 1
     pid < -1                         to process group -pid
  Return value is zero, except for case three, where the return value
  is the number of processes to which the signal was sent.
}
var
  regs : Syscallregs;
begin
  regs.reg2:=Pid;
  regs.reg3:=Sig;
  kill:=SysCall(Syscall_nr_kill,regs);
  if kill<0 then
   Kill:=0;
  linuxerror:=errno;
end;


Procedure SigProcMask(How:longint;SSet,OldSSet:PSigSet);
{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}
Var
  sr : SyscallRegs;
begin
  sr.reg2:=how;
  sr.reg3:=longint(SSet);
  sr.reg4:=longint(OldSSet);
  SysCall(Syscall_nr_sigprocmask,sr);
  linuxerror:=errno;
end;



Function SigPending:SigSet;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
Var
  sr    : SyscallRegs;
  dummy : Sigset;
begin
  sr.reg2:=longint(@dummy);
  SysCall(Syscall_nr_sigpending,sr);
  linuxerror:=errno;
  Sigpending:=dummy;
end;



Procedure SigSuspend(Mask:Sigset);
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}
Var
  sr : SyscallRegs;
begin
  sr.reg2:=mask;
  SysCall(Syscall_nr_sigsuspend,sr);
  linuxerror:=errno;
end;



Function Signal(Signum:longint;Handler:SignalHandler):SignalHandler;
{
  Install a new handler for signal Signum.
  The old signal handler is returned.
  This call does, in fact, the same as SigAction.
}
var
  sr : Syscallregs;
begin
  sr.reg2:=signum;
  sr.reg3:=longint(handler);
  Linuxerror:=SysCall(Syscall_nr_signal,sr);
  If linuxerror=Sig_Err then
   begin
     Signal:=nil;
     Linuxerror:=errno;
   end
  else
   begin
     Signal:=signalhandler(Linuxerror);
     linuxerror:=0;
   end;
end;

Function  Alarm(Sec : Longint) : longint;

Var Sr : Syscallregs;

begin
  sr.reg2:=Sec;
  Alarm:=Syscall(syscall_nr_alarm,sr);
end;

Procedure Pause;

Var Sr : Syscallregs;

begin
  syscall(syscall_nr_pause,sr);
end;

Function NanoSleep(const req : timespec;var rem : timespec) : longint;

var Sr : Syscallregs;

begin
  sr.reg2:=longint(@req);
  sr.reg3:=longint(@rem);
  NanoSleep:=Syscall(syscall_nr_nanosleep,sr);
  LinuxError:=Errno;
end;

Function IOCtl(Handle,Ndx: Longint;Data: Pointer):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
var
  sr: SysCallRegs;
begin
  sr.reg2:=Handle;
  sr.reg3:=Ndx;
  sr.reg4:=Longint(Data);
  IOCtl:=(SysCall(Syscall_nr_ioctl,sr)=0);
  LinuxError:=Errno;
end;


function MMap(const m:tmmapargs):longint;
Var
  Sr : Syscallregs;
begin
  Sr.reg2:=longint(@m);
  MMap:=syscall(syscall_nr_mmap,sr);
  LinuxError:=Errno;
end;

function MUnMap (P : Pointer; Size : Longint) : Boolean;
Var
  Sr : Syscallregs;
begin
  Sr.reg2:=longint(P);
  sr.reg3:=Size;
  MUnMap:=syscall(syscall_nr_munmap,sr)=0;
  LinuxError:=Errno;
end;

{--------------------------------
      Port IO functions
--------------------------------}

Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
{
  Set permissions on NUM ports starting with port FROM to VALUE
  this works ONLY as root.
}

Var
  Sr : Syscallregs;
begin
  Sr.Reg2:=From;
  Sr.Reg3:=Num;
  Sr.Reg4:=Value;
  IOPerm:=Syscall(Syscall_nr_ioperm,sr)=0;
  LinuxError:=Errno;
end;

Function IoPL(Level : longint) : Boolean;

Var
  Sr : Syscallregs;
begin
  Sr.Reg2:=Level;
  IOPL:=Syscall(Syscall_nr_iopl,sr)=0;
  LinuxError:=Errno;
end;

{******************************************************************************
                          Process related calls
******************************************************************************}

{ Most calls of WaitPID do not handle the result correctly, this funktion treats errors more correctly }
Function  WaitProcess(Pid:longint):Longint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}
var     r,s     : LongInt;
begin
  repeat
    s:=$7F00;
    r:=WaitPid(Pid,@s,0);
  until (r<>-1) or (LinuxError<>Sys_EINTR);
  if (r=-1) or (r=0) then // 0 is not a valid return and should never occur (it means status invalid when using WNOHANG)
    WaitProcess:=-1 // return -1 to indicate an error
  else
   begin
{$ifdef solaris}
     if (s and $FF)=0 then // Only this is a valid returncode
{$else solaris}
     { the following is at least correct for Linux and Darwin (JM) }
     if (s and $7F)=0 then
{$endif solaris}
      WaitProcess:=s shr 8
     else if (s>0) then  // Until now there is not use of the highest bit , but check this for the future
      WaitProcess:=-s // normal case
     else
      WaitProcess:=s; // s<0 should not occur, but wie return also a negativ value
   end;
end;

function InternalCreateShellArgV(cmd:pChar; len:longint):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
}
const   Shell   = '/bin/sh'#0'-c'#0;
var
  pp,p : ppchar;
//  temp : string; !! Never pass a local var back!!
begin
  getmem(pp,4*4);
  p:=pp;
  p^:=@Shell[1];
  inc(p);
  p^:=@Shell[9];
  inc(p);
  getmem(p^,len+1);
  move(cmd^,p^^,len);
  pchar(p^)[len]:=#0;
  inc(p);
  p^:=Nil;
  InternalCreateShellArgV:=pp;
end;

function CreateShellArgV(const prog:string):ppchar;
begin
  CreateShellArgV:=InternalCreateShellArgV(@prog[1],length(prog));
end;

function CreateShellArgV(const prog:Ansistring):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
  using a AnsiString;
}
begin
  CreateShellArgV:=InternalCreateShellArgV(@prog[1],length(prog)); // if ppc works like delphi this also work when @prog[1] is invalid (len=0)
end;

procedure FreeShellArgV(p:ppchar);
begin
  if (p<>nil) then begin
    freemem(p[2]);
    freemem(p);
   end;
end;

Procedure Execve(Path: AnsiString;args:ppchar;ep:ppchar);
{
  overloaded ansistring version.
}
begin
  ExecVE(PChar(Path),args,ep);
end;

Procedure Execv(const path: AnsiString;args:ppchar);
{
  Overloaded ansistring version.
}
begin
  ExecVe(Path,Args,envp)
end;

Procedure Execvp(Path: AnsiString; Args:ppchar;Ep:ppchar);
{
  Overloaded ansistring version
}
var
  thepath : Ansistring;
begin
  if path[1]<>'/' then
   begin
     Thepath:=strpas(getenv('PATH'));
     if thepath='' then
      thepath:='.';
     Path:=FSearch(path,thepath)
   end
  else
   Path:='';
  if Path='' then
   linuxerror:=Sys_enoent
  else
   Execve(Path,args,ep);{On error linuxerror will get set there}
end;

Procedure Execv(const path:pathstr;args:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  the current environment is passed on.
}
begin
  Execve(path,args,envp); {On error linuxerror will get set there}
end;

Procedure Execvp(Path:Pathstr;Args:ppchar;Ep:ppchar);
{
  This does the same as Execve, only it searches the PATH environment
  for the place of the Executable, except when Path starts with a slash.
  if the PATH environment variable is unavailable, the path is set to '.'
}
var
  thepath : string;
begin
  if path[1]<>'/' then
   begin
     Thepath:=strpas(getenv('PATH'));
     if thepath='' then
      thepath:='.';
     Path:=FSearch(path,thepath)
   end
  else
   Path:='';
  if Path='' then
   linuxerror:=Sys_enoent
  else
   Execve(Path,args,ep);{On error linuxerror will get set there}
end;

Procedure Execle(Todo:string;Ep:ppchar);
{
  This procedure takes the string 'Todo', parses it for command and
  command options, and Executes the command with the given options.
  The string 'Todo' shoud be of the form 'command options', options
  separated by commas.
  the PATH environment is not searched for 'command'.
  The specified environment(in 'ep') is passed on to command
}
var
  p : ppchar;
begin
  p:=StringToPPChar(ToDo);
  if (p=nil) or (p^=nil) then
   exit;
  ExecVE(p^,p,EP);
end;

Procedure Execle(Todo:AnsiString;Ep:ppchar);
{
  This procedure takes the string 'Todo', parses it for command and
  command options, and Executes the command with the given options.
  The string 'Todo' shoud be of the form 'command options', options
  separated by commas.
  the PATH environment is not searched for 'command'.
  The specified environment(in 'ep') is passed on to command
}
var
  p : ppchar;
begin
  p:=StringToPPChar(ToDo);
  if (p=nil) or (p^=nil) then
   exit;
  ExecVE(p^,p,EP);
end;

Procedure Execl(const Todo:string);
{
  This procedure takes the string 'Todo', parses it for command and
  command options, and Executes the command with the given options.
  The string 'Todo' shoud be of the form 'command options', options
  separated by commas.
  the PATH environment is not searched for 'command'.
  The current environment is passed on to command
}
begin
  ExecLE(ToDo,EnvP);
end;

Procedure Execl(const Todo:Ansistring);

{
  Overloaded AnsiString Version of ExecL.
}

begin
  ExecLE(ToDo,EnvP);
end;


Procedure Execlp(Todo:string;Ep:ppchar);
{
  This procedure takes the string 'Todo', parses it for command and
  command options, and Executes the command with the given options.
  The string 'Todo' shoud be of the form 'command options', options
  separated by commas.
  the PATH environment is searched for 'command'.
  The specified environment (in 'ep') is passed on to command
}
var
  p : ppchar;
begin
  p:=StringToPPchar(todo);
  if (p=nil) or (p^=nil) then
   exit;
  ExecVP(StrPas(p^),p,EP);
end;

Procedure Execlp(Todo: Ansistring;Ep:ppchar);
{
  Overloaded ansistring version.
}
var
  p : ppchar;
begin
  p:=StringToPPchar(todo);
  if (p=nil) or (p^=nil) then
   exit;
  ExecVP(StrPas(p^),p,EP);
end;

Function Shell(const Command:String):Longint;
{
  Executes the shell, and passes it the string Command. (Through /bin/sh -c)
  The current environment is passed to the shell.
  It waits for the shell to exit, and returns its exit status.
  If the Exec call failed exit status 127 is reported.
}
{ Changed the structure:
- the previous version returns an undefinied value if fork fails
- it returns the status of Waitpid instead of the Process returnvalue (see the doc to Shell)
- it uses exit(127) not ExitProc (The Result in pp386: going on Compiling in 2 processes!)
- ShellArgs are now released
- The Old CreateShellArg gives back pointers to a local var
}
var
  p       : ppchar;
  pid,r,s  : longint;
begin
  p:=CreateShellArgv(command);
  pid:=fork;
  if pid=0 then // We are in the Child
   begin
     {This is the child.}
     Execve(p^,p,envp);
     ExitProcess(127);  // was Exit(127)
   end
  else if (pid<>-1) then // Successfull started
    begin
      repeat
      s:=$7F00;
      r:=WaitPid(Pid,@s,0);
    until (r<>-1) or (LinuxError<>Sys_EINTR);
    if (r=-1) or (r=0) then
      Shell:=-1
    else
      Shell:=s;
    end
  else // no success
   Shell:=-1; // indicate an error
  FreeShellArgV(p);
end;

Function Shell(const Command:AnsiString):Longint;
{
  AnsiString version of Shell
}
var
  p     : ppchar;
  pid   : longint;
begin { Changes as above }
  p:=CreateShellArgv(command);
  pid:=fork;
  if pid=0 then // We are in the Child
   begin
     Execve(p^,p,envp);
     ExitProcess(127); // was exit(127)!! We must exit the Process, not the function
   end
  else if (pid<>-1) then // Successfull started
   Shell:=WaitProcess(pid) {Linuxerror is set there}
  else // no success
   Shell:=-1;
  FreeShellArgV(p);
end;

function WEXITSTATUS(Status: Integer): Integer;
begin
  WEXITSTATUS:=(Status and $FF00) shr 8;
end;

function WTERMSIG(Status: Integer): Integer;
begin
  WTERMSIG:=(Status and $7F);
end;

function WSTOPSIG(Status: Integer): Integer;
begin
  WSTOPSIG:=WEXITSTATUS(Status);
end;

Function WIFEXITED(Status: Integer): Boolean;
begin
  WIFEXITED:=(WTERMSIG(Status)=0);
end;

Function WIFSTOPPED(Status: Integer): Boolean;
begin
  WIFSTOPPED:=((Status and $FF)=$7F);
end;

Function WIFSIGNALED(Status: Integer): Boolean;
begin
  WIFSIGNALED:=(not WIFSTOPPED(Status)) and
               (not WIFEXITED(Status));
end;

Function W_EXITCODE(ReturnCode, Signal: Integer): Integer;
begin
  W_EXITCODE:=(ReturnCode shl 8) or Signal;
end;

Function W_STOPCODE(Signal: Integer): Integer;

begin
  W_STOPCODE:=(Signal shl 8) or $7F;
end;


{******************************************************************************
                       Date and Time related calls
******************************************************************************}

Const
{Date Translation}
  C1970=2440588;
  D0   =   1461;
  D1   = 146097;
  D2   =1721119;

Function GregorianToJulian(Year,Month,Day:Longint):LongInt;
Var
  Century,XYear: LongInt;
Begin
  If Month<=2 Then
   Begin
     Dec(Year);
     Inc(Month,12);
   End;
  Dec(Month,3);
  Century:=(longint(Year Div 100)*D1) shr 2;
  XYear:=(longint(Year Mod 100)*D0) shr 2;
  GregorianToJulian:=((((Month*153)+2) div 5)+Day)+D2+XYear+Century;
End;



Procedure JulianToGregorian(JulianDN:LongInt;Var Year,Month,Day:Word);
Var
  YYear,XYear,Temp,TempMonth : LongInt;
Begin
  Temp:=((JulianDN-D2) shl 2)-1;
  JulianDN:=Temp Div D1;
  XYear:=(Temp Mod D1) or 3;
  YYear:=(XYear Div D0);
  Temp:=((((XYear mod D0)+4) shr 2)*5)-3;
  Day:=((Temp Mod 153)+5) Div 5;
  TempMonth:=Temp Div 153;
  If TempMonth>=10 Then
   Begin
     inc(YYear);
     dec(TempMonth,12);
   End;
  inc(TempMonth,3);
  Month := TempMonth;
  Year:=YYear+(JulianDN*100);
end;

Function GetEpochTime: longint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
begin
  GetEpochTime:=GetTimeOfDay;
end;


Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
{
  Transforms Epoch time into local time (hour, minute,seconds)
}
Var
  DateNum: LongInt;
Begin
  inc(Epoch,TZSeconds);
  Datenum:=(Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch:=Abs(Epoch Mod 86400);
  Hour:=Epoch Div 3600;
  Epoch:=Epoch Mod 3600;
  Minute:=Epoch Div 60;
  Second:=Epoch Mod 60;
End;


Function LocalToEpoch(year,month,day,hour,minute,second:Word):Longint;
{
  Transforms local time (year,month,day,hour,minutes,second) to Epoch time
   (seconds since 00:00, january 1 1970, corrected for local time zone)
}
Begin
  LocalToEpoch:=((GregorianToJulian(Year,Month,Day)-c1970)*86400)+
                (LongInt(Hour)*3600)+(Minute*60)+Second-TZSeconds;
End;


procedure GetTime(var hour,min,sec,msec,usec:word);
{
  Gets the current time, adjusted to local time
}
var
  year,day,month:Word;
  t : timeval;
begin
  gettimeofday(t);
  EpochToLocal(t.sec,year,month,day,hour,min,sec);
  msec:=t.usec div 1000;
  usec:=t.usec mod 1000;
end;


procedure GetTime(var hour,min,sec,sec100:word);
{
  Gets the current time, adjusted to local time
}
var
  usec : word;
begin
  gettime(hour,min,sec,sec100,usec);
  sec100:=sec100 div 10;
end;


Procedure GetTime(Var Hour,Min,Sec:Word);
{
  Gets the current time, adjusted to local time
}
var
  msec,usec : Word;
Begin
  gettime(hour,min,sec,msec,usec);
End;


Procedure GetDate(Var Year,Month,Day:Word);
{
  Gets the current date, adjusted to local time
}
var
  hour,minute,second : word;
Begin
  EpochToLocal(GetTimeOfDay,year,month,day,hour,minute,second);
End;


Procedure GetDateTime(Var Year,Month,Day,hour,minute,second:Word);
{
  Gets the current date, adjusted to local time
}
Begin
  EpochToLocal(GetTimeOfDay,year,month,day,hour,minute,second);
End;

{$ifndef BSD}          {Fix for 1.0.x starting compiler only}
{$ifdef linux}
Function stime (t : longint) : Boolean;
var
  sr : Syscallregs;
begin
  sr.reg2:=longint(@t);
  SysCall(Syscall_nr_stime,sr);
  linuxerror:=errno;
   stime:=linuxerror=0;
end;
{$endif}
{$endif}

{$ifdef BSD}
Function stime (t : longint) : Boolean;
begin
  stime:=false;
end;
{$endif}

Function SetTime(Hour,Min,Sec:word) : boolean;
var
  Year, Month, Day : Word;
begin
  GetDate (Year, Month, Day);
  SetTime:=stime ( LocalToEpoch ( Year, Month, Day, Hour, Min, Sec ) );
end;

Function SetDate(Year,Month,Day:Word) : boolean;
var
  Hour, Minute, Second, Sec100 : Word;
begin
  GetTime ( Hour, Minute, Second, Sec100 );
  SetDate:=stime ( LocalToEpoch ( Year, Month, Day, Hour, Minute, Second ) );
end;

Function SetDateTime(Year,Month,Day,hour,minute,second:Word) : Boolean;

begin
  SetDateTime:=stime ( LocalToEpoch ( Year, Month, Day, Hour, Minute, Second ) );
end;

{ Include timezone handling routines which use /usr/share/timezone info }

type
  plongint=^longint;
  pbyte=^byte;

  ttzhead=packed record
    tzh_reserved : array[0..19] of byte;
    tzh_ttisgmtcnt,
    tzh_ttisstdcnt,
    tzh_leapcnt,
    tzh_timecnt,
    tzh_typecnt,
    tzh_charcnt  : longint;
  end;

  pttinfo=^tttinfo;
  tttinfo=packed record
    offset : longint;
    isdst  : boolean;
    idx    : byte;
    isstd  : byte;
    isgmt  : byte;
  end;

  pleap=^tleap;
  tleap=record
    transition : longint;
    change     : longint;
  end;

var
  num_transitions,
  num_leaps,
  num_types    : longint;

  transitions  : plongint;
  type_idxs    : pbyte;
  types        : pttinfo;
  zone_names   : pchar;
  leaps        : pleap;

function find_transition(timer:longint):pttinfo;
var
  i : longint;
begin
  if (num_transitions=0) or (timer<transitions[0]) then
   begin
     i:=0;
     while (i<num_types) and (types[i].isdst) do
      inc(i);
     if (i=num_types) then
      i:=0;
   end
  else
   begin
     for i:=1 to num_transitions do
      if (timer<transitions[i]) then
       break;
     i:=type_idxs[i-1];
   end;
  find_transition:=@types[i];
end;


procedure GetLocalTimezone(timer:longint;var leap_correct,leap_hit:longint);
var
  info : pttinfo;
  i    : longint;
begin
{ reset }
  TZDaylight:=false;
  TZSeconds:=0;
  TZName[false]:=nil;
  TZName[true]:=nil;
  leap_correct:=0;
  leap_hit:=0;
{ get info }
  info:=find_transition(timer);
  if not assigned(info) then
   exit;
  TZDaylight:=info^.isdst;
  TZSeconds:=info^.offset;
  i:=0;
  while (i<num_types) do
   begin
     tzname[types[i].isdst]:=@zone_names[types[i].idx];
     inc(i);
   end;
  tzname[info^.isdst]:=@zone_names[info^.idx];
  i:=num_leaps;
  repeat
    if i=0 then
     exit;
    dec(i);
  until (timer>leaps[i].transition);
  leap_correct:=leaps[i].change;
  if (timer=leaps[i].transition) and
     (((i=0) and (leaps[i].change>0)) or
      (leaps[i].change>leaps[i-1].change)) then
   begin
     leap_hit:=1;
     while (i>0) and
           (leaps[i].transition=leaps[i-1].transition+1) and
           (leaps[i].change=leaps[i-1].change+1) do
      begin
        inc(leap_hit);
        dec(i);
      end;
   end;
end;


procedure GetLocalTimezone(timer:longint);
var
  lc,lh : longint;
begin
  GetLocalTimezone(timer,lc,lh);
end;


procedure ReadTimezoneFile(fn:string);

  procedure decode(var l:longint);
  var
    k : longint;
    p : pbyte;
  begin
    p:=pbyte(@l);
    if (p[0] and (1 shl 7))<>0 then
     k:=not 0
    else
     k:=0;
    k:=(k shl 8) or p[0];
    k:=(k shl 8) or p[1];
    k:=(k shl 8) or p[2];
    k:=(k shl 8) or p[3];
    l:=k;
  end;

var
  f      : longint;
  tzdir  : string;
  tzhead : ttzhead;
  i      : longint;
  chars  : longint;
  buf    : pbyte;
begin
  if fn='' then
   fn:='localtime';
  if fn[1]<>'/' then
   begin
     tzdir:=getenv('TZDIR');
     if tzdir='' then
      tzdir:='/usr/share/zoneinfo';
     if tzdir[length(tzdir)]<>'/' then
      tzdir:=tzdir+'/';
     fn:=tzdir+fn;
   end;
  f:=fdopen(fn,Open_RdOnly);
  if f<0 then
   exit;
  i:=fdread(f,tzhead,sizeof(tzhead));
  if i<>sizeof(tzhead) then
   exit;
  decode(tzhead.tzh_timecnt);
  decode(tzhead.tzh_typecnt);
  decode(tzhead.tzh_charcnt);
  decode(tzhead.tzh_leapcnt);
  decode(tzhead.tzh_ttisstdcnt);
  decode(tzhead.tzh_ttisgmtcnt);

  num_transitions:=tzhead.tzh_timecnt;
  num_types:=tzhead.tzh_typecnt;
  chars:=tzhead.tzh_charcnt;

  reallocmem(transitions,num_transitions*sizeof(longint));
  reallocmem(type_idxs,num_transitions);
  reallocmem(types,num_types*sizeof(tttinfo));
  reallocmem(zone_names,chars);
  reallocmem(leaps,num_leaps*sizeof(tleap));

  fdread(f,transitions^,num_transitions*4);
  fdread(f,type_idxs^,num_transitions);

  for i:=0 to num_transitions-1 do
   decode(transitions[i]);

  for i:=0 to num_types-1 do
   begin
     fdread(f,types[i].offset,4);
     fdread(f,types[i].isdst,1);
     fdread(f,types[i].idx,1);
     decode(types[i].offset);
     types[i].isstd:=0;
     types[i].isgmt:=0;
   end;

  fdread(f,zone_names^,chars);

  for i:=0 to num_leaps-1 do
   begin
     fdread(f,leaps[i].transition,4);
     fdread(f,leaps[i].change,4);
     decode(leaps[i].transition);
     decode(leaps[i].change);
   end;

  getmem(buf,tzhead.tzh_ttisstdcnt);
  fdread(f,buf^,tzhead.tzh_ttisstdcnt);
  for i:=0 to tzhead.tzh_ttisstdcnt-1 do
   types[i].isstd:=byte(buf[i]<>0);
  freemem(buf);

  getmem(buf,tzhead.tzh_ttisgmtcnt);
  fdread(f,buf^,tzhead.tzh_ttisgmtcnt);
  for i:=0 to tzhead.tzh_ttisgmtcnt-1 do
   types[i].isgmt:=byte(buf[i]<>0);
  freemem(buf);
  fdclose(f);
end;

Const
  // Debian system; contains location of timezone file.
  TimeZoneLocationFile = '/etc/timezone';
  // SuSE has link in /usr/lib/zoneinfo/localtime to /etc/localtime
  // RedHat uses /etc/localtime
  TimeZoneFile = '/usr/lib/zoneinfo/localtime';
  AltTimeZoneFile = '/etc/localtime';

function GetTimezoneFile:string;
var
  f,len : longint;
  s : string;
  info : stat;

begin
  GetTimezoneFile:='';
  f:=fdopen(TimeZoneLocationFile,Open_RdOnly);
  if f>0 then
    begin
    len:=fdread(f,s[1],high(s));
    s[0]:=chr(len);
    len:=pos(#10,s);
    if len<>0 then
     s[0]:=chr(len-1);
    fdclose(f);
    GetTimezoneFile:=s;
    end
  // Try SuSE
  else if fstat(TimeZoneFile,info) then
    GetTimeZoneFile:=TimeZoneFile
  // Try RedHat
  else If fstat(AltTimeZoneFile,Info) then
      GetTimeZoneFile:=AltTimeZoneFile;
end;


procedure InitLocalTime;
begin
  ReadTimezoneFile(GetTimezoneFile);
  GetLocalTimezone(GetTimeOfDay);
end;


procedure DoneLocalTime;
begin
  if assigned(transitions) then
   freemem(transitions);
  if assigned(type_idxs) then
   freemem(type_idxs);
  if assigned(types) then
   freemem(types);
  if assigned(zone_names) then
   freemem(zone_names);
  if assigned(leaps) then
   freemem(leaps);
  num_transitions:=0;
  num_leaps:=0;
  num_types:=0;
end;




{******************************************************************************
                           FileSystem calls
******************************************************************************}

Function fdOpen(pathname:string;flags:longint):longint;
begin
  pathname:=pathname+#0;
  fdOpen:=Sys_Open(@pathname[1],flags,438);
  LinuxError:=Errno;
end;


Function fdOpen(pathname:string;flags,mode:longint):longint;
begin
  pathname:=pathname+#0;
  fdOpen:=Sys_Open(@pathname[1],flags,mode);
  LinuxError:=Errno;
end;



Function  fdOpen(pathname:pchar;flags:longint):longint;
begin
  fdOpen:=Sys_Open(pathname,flags,0);
  LinuxError:=Errno;
end;



Function  fdOpen(pathname:pchar;flags,mode:longint):longint;
begin
  fdOpen:=Sys_Open(pathname,flags,mode);
  LinuxError:=Errno;
end;



Function fdClose(fd:longint):boolean;
begin
  fdClose:=(Sys_Close(fd)=0);
  LinuxError:=Errno;
end;



Function fdRead(fd:longint;var buf;size:longint):longint;
begin
  fdRead:=Sys_Read(fd,pchar(@buf),size);
  LinuxError:=Errno;
end;



Function fdWrite(fd:longint;const buf;size:longint):longint;
begin
  fdWrite:=Sys_Write(fd,pchar(@buf),size);
  LinuxError:=Errno;
end;




Function  fdSeek (fd,pos,seektype :longint): longint;
{
  Do a Seek on a file descriptor fd to position pos, starting from seektype

}
begin
   fdseek:=Sys_LSeek (fd,pos,seektype);
   LinuxError:=Errno;
end;

{$ifdef BSD}
Function Fcntl(Fd:longint;Cmd:longint):longint;
{
  Read or manipulate a file.(See also fcntl (2) )
  Possible values for Cmd are :
    F_GetFd,F_GetFl,F_GetOwn
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=Sys_eninval.
}

begin
  if (cmd in [F_GetFd,F_GetFl,F_GetOwn]) then
   begin
     Linuxerror:=sys_fcntl(fd,cmd,0);
     if linuxerror=-1 then
      begin
        linuxerror:=errno;
        fcntl:=0;
      end
     else
      begin
        fcntl:=linuxerror;
        linuxerror:=0;
      end;
   end
  else
   begin
     linuxerror:=Sys_einval;
     Fcntl:=0;
   end;
end;


Procedure Fcntl(Fd:longint;Cmd:longint;Arg:Longint);
{
  Read or manipulate a file. (See also fcntl (2) )
  Possible values for Cmd are :
    F_setFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkW,F_SetOwn;
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=Sys_eninval.
  F_DupFD is not allowed, due to the structure of Files in Pascal.
}
begin
  if (cmd in [F_SetFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkw,F_SetOwn]) then
   begin
     sys_fcntl(fd,cmd,arg);
     LinuxError:=ErrNo;
   end
  else
   linuxerror:=Sys_einval;
end;
{$endif}


Function Fcntl(var Fd:Text;Cmd:longint):longint;
begin
  Fcntl := Fcntl(textrec(Fd).handle, Cmd);
end;

Procedure Fcntl(var Fd:Text;Cmd,Arg:Longint);

begin
  Fcntl(textrec(Fd).handle, Cmd, Arg);
end;


Function Flock (var T : text;mode : longint) : boolean;
begin
  Flock:=Flock(TextRec(T).Handle,mode);
end;



Function  Flock (var F : File;mode : longint) : boolean;
begin
  Flock:=Flock(FileRec(F).Handle,mode);
end;



Function FStat(Path:Pathstr;Var Info:stat):Boolean;
{
  Get all information on a file, and return it in Info.
}
begin
  path:=path+#0;
  FStat:=(Sys_stat(@(path[1]),Info)=0);
  LinuxError:=errno;
end;




Function  FStat(var F:Text;Var Info:stat):Boolean;
{
  Get all information on a text file, and return it in info.
}
begin
  FStat:=Fstat(TextRec(F).Handle,INfo);
end;



Function  FStat(var F:File;Var Info:stat):Boolean;
{
  Get all information on a untyped file, and return it in info.
}
begin
  FStat:=Fstat(FileRec(F).Handle,Info);
end;

Function SymLink(OldPath,newPath:pathstr):boolean;
{
  Proceduces a soft link from new to old.
}
begin
  oldpath:=oldpath+#0;
  newpath:=newpath+#0;
  Symlink:=Sys_symlink(pchar(@(oldpath[1])),pchar(@(newpath[1])))=0;
  linuxerror:=errno;
end;


Function ReadLink(name,linkname:pchar;maxlen:longint):longint;
{
  Read a link (where it points to)
}
begin
  Readlink:=Sys_readlink(Name,LinkName,maxlen);
  linuxerror:=errno;
end;


Function ReadLink(Name:pathstr):pathstr;
{
  Read a link (where it points to)
}
var
  LinkName : pathstr;
  i : longint;
begin
  Name:=Name+#0;
  i:=ReadLink(@Name[1],@LinkName[1],high(linkname));
  if i>0 then
   begin
     linkname[0]:=chr(i);
     ReadLink:=LinkName;
   end
  else
   ReadLink:='';
end;


Function UnLink(Path:pathstr):boolean;
{
  Removes the file in 'Path' (that is, it decreases the link count with one.
  if the link count is zero, the file is removed from the disk.
}
begin
  path:=path+#0;
  Unlink:=Sys_unlink(pchar(@(path[1])))=0;
  linuxerror:=errno;
end;


Function  UnLink(Path:pchar):Boolean;
{
  Removes the file in 'Path' (that is, it decreases the link count with one.
  if the link count is zero, the file is removed from the disk.
}
begin
  Unlink:=(Sys_unlink(path)=0);
  linuxerror:=errno;
end;


Function  FRename (OldName,NewName : Pchar) : Boolean;
begin
  FRename:=Sys_rename(OldName,NewName)=0;
  LinuxError:=Errno;
end;


Function  FRename (OldName,NewName : String) : Boolean;
begin
  OldName:=OldName+#0;
  NewName:=NewName+#0;
  FRename:=FRename (@OldName[1],@NewName[1]);
end;

Function Dup(var oldfile,newfile:text):Boolean;
{
  Copies the filedescriptor oldfile to newfile, after flushing the buffer of
  oldfile.
  After which the two textfiles are, in effect, the same, except
  that they don't share the same buffer, and don't share the same
  close_on_exit flag.
}
begin
  flush(oldfile);{ We cannot share buffers, so we flush them. }
  textrec(newfile):=textrec(oldfile);
  textrec(newfile).bufptr:=@(textrec(newfile).buffer);{ No shared buffer. }
  Dup:=Dup(textrec(oldfile).handle,textrec(newfile).handle);
end;


Function Dup(var oldfile,newfile:file):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}
begin
  filerec(newfile):=filerec(oldfile);
  Dup:=Dup(filerec(oldfile).handle,filerec(newfile).handle);
end;



Function Dup2(var oldfile,newfile:text):Boolean;
{
  Copies the filedescriptor oldfile to newfile, after flushing the buffer of
  oldfile. It closes newfile if it was still open.
  After which the two textfiles are, in effect, the same, except
  that they don't share the same buffer, and don't share the same
  close_on_exit flag.
}
var
  tmphandle : word;
begin
  case TextRec(oldfile).mode of
    fmOutput, fmInOut, fmAppend :
      flush(oldfile);{ We cannot share buffers, so we flush them. }
  end;
  case TextRec(newfile).mode of
    fmOutput, fmInOut, fmAppend :
      flush(newfile);
  end;
  tmphandle:=textrec(newfile).handle;
  textrec(newfile):=textrec(oldfile);
  textrec(newfile).handle:=tmphandle;
  textrec(newfile).bufptr:=@(textrec(newfile).buffer);{ No shared buffer. }
  Dup2:=Dup2(textrec(oldfile).handle,textrec(newfile).handle);
end;


Function Dup2(var oldfile,newfile:file):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}
begin
  filerec(newfile):=filerec(oldfile);
  Dup2:=Dup2(filerec(oldfile).handle,filerec(newfile).handle);
end;



Function  Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:Longint):longint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
  This function allows specification of a timeout as a longint.
}
var
  p  : PTimeVal;
  tv : TimeVal;
begin
  if TimeOut=-1 then
   p:=nil
  else
   begin
     tv.Sec:=Timeout div 1000;
     tv.Usec:=(Timeout mod 1000)*1000;
     p:=@tv;
   end;
  Select:=Select(N,Readfds,WriteFds,ExceptFds,p);
end;



Function SelectText(var T:Text;TimeOut :PTimeval):Longint;
Var
  F:FDSet;
begin
  if textrec(t).mode=fmclosed then
   begin
     LinuxError:=Sys_EBADF;
     exit(-1);
   end;
  FD_Zero(f);
  FD_Set(textrec(T).handle,f);
  if textrec(T).mode=fminput then
   SelectText:=select(textrec(T).handle+1,@f,nil,nil,TimeOut)
  else
   SelectText:=select(textrec(T).handle+1,nil,@f,nil,TimeOut);
end;


Function SelectText(var T:Text;TimeOut :Longint):Longint;
var
  p  : PTimeVal;
  tv : TimeVal;
begin
  if TimeOut=-1 then
   p:=nil
  else
   begin
     tv.Sec:=Timeout div 1000;
     tv.Usec:=(Timeout mod 1000)*1000;
     p:=@tv;
   end;
  SelectText:=SelectText(T,p);
end;


{******************************************************************************
                               Directory
******************************************************************************}

Function OpenDir(F:String):PDir;
begin
  F:=F+#0;
  OpenDir:=OpenDir(@F[1]);
  LinuxError:=ErrNo;
end;

{$ifndef newreaddir}
procedure SeekDir(p:pdir;off:longint);
begin
  if p=nil then
   begin
     errno:=Sys_EBADF;
     exit;
   end;
 {$ifndef bsd}
  p^.nextoff:=Sys_lseek(p^.fd,off,seek_set);
 {$endif}
  p^.size:=0;
  p^.loc:=0;
end;

function TellDir(p:pdir):longint;
begin
  if p=nil then
   begin
     errno:=Sys_EBADF;
     telldir:=-1;
     exit;
   end;
  telldir:=Sys_lseek(p^.fd,0,seek_cur)
  { We could try to use the nextoff field here, but on my 1.2.13
    kernel, this gives nothing... This may have to do with
    the readdir implementation of libc... I also didn't find any trace of
    the field in the kernel code itself, So I suspect it is an artifact of libc.
    Michael. }
end;
{$endif}

Function ReadDir(P:pdir):pdirent;
begin
  ReadDir:=Sys_ReadDir(p);
  LinuxError:=Errno;
end;


{******************************************************************************
                               Pipes/Fifo
******************************************************************************}

Procedure OpenPipe(var F:Text);
begin
  case textrec(f).mode of
    fmoutput :
      if textrec(f).userdata[1]<>P_OUT then
        textrec(f).mode:=fmclosed;
    fminput :
      if textrec(f).userdata[1]<>P_IN then
        textrec(f).mode:=fmclosed;
    else
      textrec(f).mode:=fmclosed;
  end;
end;


Procedure IOPipe(var F:text);
begin
  case textrec(f).mode of
    fmoutput :
      begin
        { first check if we need something to write, else we may
          get a SigPipe when Close() is called (PFV) }
        if textrec(f).bufpos>0 then
          Sys_write(textrec(f).handle,pchar(textrec(f).bufptr),textrec(f).bufpos);
      end;
    fminput :
      textrec(f).bufend:=Sys_read(textrec(f).handle,pchar(textrec(f).bufptr),textrec(f).bufsize);
  end;
  textrec(f).bufpos:=0;
end;


Procedure FlushPipe(var F:Text);
begin
  if (textrec(f).mode=fmoutput) and (textrec(f).bufpos<>0) then
   IOPipe(f);
  textrec(f).bufpos:=0;
end;


Procedure ClosePipe(var F:text);
begin
  textrec(f).mode:=fmclosed;
  Sys_close(textrec(f).handle);
end;


Function AssignPipe(var pipe_in,pipe_out:text):boolean;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  f_in,f_out : longint;
begin
  if not AssignPipe(f_in,f_out) then
   begin
     AssignPipe:=false;
     exit;
   end;
{ Set up input }
  Assign(Pipe_in,'');
  Textrec(Pipe_in).Handle:=f_in;
  Textrec(Pipe_in).Mode:=fmInput;
  Textrec(Pipe_in).userdata[1]:=P_IN;
  TextRec(Pipe_in).OpenFunc:=@OpenPipe;
  TextRec(Pipe_in).InOutFunc:=@IOPipe;
  TextRec(Pipe_in).FlushFunc:=@FlushPipe;
  TextRec(Pipe_in).CloseFunc:=@ClosePipe;
{ Set up output }
  Assign(Pipe_out,'');
  Textrec(Pipe_out).Handle:=f_out;
  Textrec(Pipe_out).Mode:=fmOutput;
  Textrec(Pipe_out).userdata[1]:=P_OUT;
  TextRec(Pipe_out).OpenFunc:=@OpenPipe;
  TextRec(Pipe_out).InOutFunc:=@IOPipe;
  TextRec(Pipe_out).FlushFunc:=@FlushPipe;
  TextRec(Pipe_out).CloseFunc:=@ClosePipe;
  AssignPipe:=true;
end;


Function AssignPipe(var pipe_in,pipe_out:file):boolean;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  f_in,f_out : longint;
begin
  if not AssignPipe(f_in,f_out) then
   begin
     AssignPipe:=false;
     exit;
   end;
{ Set up input }
  Assign(Pipe_in,'');
  Filerec(Pipe_in).Handle:=f_in;
  Filerec(Pipe_in).Mode:=fmInput;
  Filerec(Pipe_in).recsize:=1;
  Filerec(Pipe_in).userdata[1]:=P_IN;
{ Set up output }
  Assign(Pipe_out,'');
  Filerec(Pipe_out).Handle:=f_out;
  Filerec(Pipe_out).Mode:=fmoutput;
  Filerec(Pipe_out).recsize:=1;
  Filerec(Pipe_out).userdata[1]:=P_OUT;
  AssignPipe:=true;
end;

Procedure PCloseText(Var F:text);
{
  May not use @PClose due overloading
}
begin
  PClose(f);
end;



Procedure POpen(var F:text;const Prog:String;rw:char);
{
  Starts the program in 'Prog' and makes it's input or out put the
  other end of a pipe. If rw is 'w' or 'W', then whatever is written to
  F, will be read from stdin by the program in 'Prog'. The inverse is true
  for 'r' or 'R' : whatever the program in 'Prog' writes to stdout, can be
  read from 'f'.
}
var
  pipi,
  pipo : text;
  pid  : longint;
  pl   : ^longint;
  pp   : ppchar;
begin
  LinuxError:=0;
  rw:=upcase(rw);
  if not (rw in ['R','W']) then
   begin
     LinuxError:=Sys_enoent;
     exit;
   end;
  AssignPipe(pipi,pipo);
  if Linuxerror<>0 then
   exit;
  pid:=fork;
  if linuxerror<>0 then
   begin
     close(pipi);
     close(pipo);
     exit;
   end;
  if pid=0 then
   begin
     {$ifdef BSD} // FreeBSD checked only
   { We're in the child }
        close(pipi);
        if textrec(pipo).handle<>textrec(output).handle Then
          begin
           dup2(textrec(pipo).handle,textrec(output).handle);
           if rw='W' Then
            dup2(textrec(output).handle,textrec(input).handle);
          end
         else
          if (rw='W') and (textrec(pipi).handle<>textrec(input).handle) then
             dup2(textrec(output).handle,textrec(input).handle);
        close(pipo);
        if linuxerror<>0 then
         halt(127);
     pp:=createshellargv(prog);
     Execve(pp^,pp,envp);
     halt(127);
   end
   {$else}
   { We're in the child }
     if rw='W' then
      begin
        close(pipo);
        dup2(pipi,input);
        close(pipi);
        if linuxerror<>0 then
         halt(127);
      end
     else
      begin
        close(pipi);
        dup2(pipo,output);
        close(pipo);
        if linuxerror<>0 then
         halt(127);
      end;
     pp:=createshellargv(prog);
     Execve(pp^,pp,envp);
     halt(127);
   end
 {$endif}
  else
   begin
   { We're in the parent }
     if rw='W' then
      begin
        close(pipi);
        f:=pipo;
        textrec(f).bufptr:=@textrec(f).buffer;
      end
     else
      begin
        close(pipo);
        f:=pipi;
        textrec(f).bufptr:=@textrec(f).buffer;
      end;
   {Save the process ID - needed when closing }
     pl:=plongint(@(textrec(f).userdata[2]));
     pl^:=pid;
     textrec(f).closefunc:=@PCloseText;
   end;
end;


Procedure POpen(var F:file;const Prog:String;rw:char);
{
  Starts the program in 'Prog' and makes it's input or out put the
  other end of a pipe. If rw is 'w' or 'W', then whatever is written to
  F, will be read from stdin by the program in 'Prog'. The inverse is true
  for 'r' or 'R' : whatever the program in 'Prog' writes to stdout, can be
  read from 'f'.
}
var
  pipi,
  pipo : file;
  pid  : longint;
  pl   : ^longint;
  p,pp : ppchar;
  temp : string[255];
begin
  LinuxError:=0;
  rw:=upcase(rw);
  if not (rw in ['R','W']) then
   begin
     LinuxError:=Sys_enoent;
     exit;
   end;
  AssignPipe(pipi,pipo);
  if Linuxerror<>0 then
   exit;
  pid:=fork;
  if linuxerror<>0 then
   begin
     close(pipi);
     close(pipo);
     exit;
   end;
  if pid=0 then
   begin
   { We're in the child }
     if rw='W' then
      begin
        close(pipo);
        dup2(filerec(pipi).handle,stdinputhandle);
        close(pipi);
        if linuxerror<>0 then
         halt(127);
      end
     else
      begin
        close(pipi);
        dup2(filerec(pipo).handle,stdoutputhandle);
        close(pipo);
        if linuxerror<>0 then
         halt(127);
      end;
     getmem(pp,sizeof(pchar)*4);
     temp:='/bin/sh'#0'-c'#0+prog+#0;
     p:=pp;
     p^:=@temp[1];
     inc(p);
     p^:=@temp[9];
     inc(p);
     p^:=@temp[12];
     inc(p);
     p^:=Nil;
     Execve('/bin/sh',pp,envp);
     halt(127);
   end
  else
   begin
   { We're in the parent }
     if rw='W' then
      begin
        close(pipi);
        f:=pipo;
      end
     else
      begin
        close(pipo);
        f:=pipi;
      end;
   {Save the process ID - needed when closing }
     pl:=plongint(@(filerec(f).userdata[2]));
     pl^:=pid;
   end;
end;


Function AssignStream(Var StreamIn,Streamout:text;Const Prog:String) : longint;
{
  Starts the program in 'Prog' and makes its input and output the
  other end of two pipes, which are the stdin and stdout of a program
  specified in 'Prog'.
  streamout can be used to write to the program, streamin can be used to read
  the output of the program. See the following diagram :
  Parent          Child
  STreamout -->  Input
  Streamin  <--  Output
  Return value is the process ID of the process being spawned, or -1 in case of failure.
}
var
  pipi,
  pipo : text;
  pid  : longint;
  pl   : ^Longint;
begin
  LinuxError:=0;
  AssignStream:=-1;
  AssignPipe(streamin,pipo);
  if Linuxerror<>0 then
   exit;
  AssignPipe(pipi,streamout);
  if Linuxerror<>0 then
   exit;
  pid:=fork;
  if linuxerror<>0 then
   begin
     close(pipi);
     close(pipo);
     close (streamin);
     close (streamout);
     exit;
   end;
  if pid=0 then
   begin
     { We're in the child }
     { Close what we don't need }
     close(streamout);
     close(streamin);
     dup2(pipi,input);
     if linuxerror<>0 then
      halt(127);
     close(pipi);
     dup2(pipo,output);
     if linuxerror<>0 then
       halt (127);
     close(pipo);
     Execl(Prog);
     halt(127);
   end
  else
   begin
     { we're in the parent}
     close(pipo);
     close(pipi);
     {Save the process ID - needed when closing }
     pl:=plongint(@(textrec(StreamIn).userdata[2]));
     pl^:=pid;
     textrec(StreamIn).closefunc:=@PCloseText;
     {Save the process ID - needed when closing }
     pl:=plongint(@(textrec(StreamOut).userdata[2]));
     pl^:=pid;
     textrec(StreamOut).closefunc:=@PCloseText;
     AssignStream:=Pid;
   end;
end;


function AssignStream(var StreamIn, StreamOut, StreamErr: Text; const prog: String): LongInt;
{
  Starts the program in 'prog' and makes its input, output and error output the
  other end of three pipes, which are the stdin, stdout and stderr of a program
  specified in 'prog'.
  StreamOut can be used to write to the program, StreamIn can be used to read
  the output of the program, StreamErr reads the error output of the program.
  See the following diagram :
  Parent          Child
  StreamOut -->  StdIn  (input)
  StreamIn  <--  StdOut (output)
  StreamErr <--  StdErr (error output)
}
var
  PipeIn, PipeOut, PipeErr: text;
  pid: LongInt;
  pl: ^LongInt;
begin
  LinuxError := 0;
  AssignStream := -1;

  // Assign pipes
  AssignPipe(StreamIn, PipeOut);
  if LinuxError <> 0 then exit;

  AssignPipe(StreamErr, PipeErr);
  if LinuxError <> 0 then begin
    Close(StreamIn);
    Close(PipeOut);
    exit;
  end;

  AssignPipe(PipeIn, StreamOut);
  if LinuxError <> 0 then begin
    Close(StreamIn);
    Close(PipeOut);
    Close(StreamErr);
    Close(PipeErr);
    exit;
  end;

  // Fork

  pid := Fork;
  if LinuxError <> 0 then begin
    Close(StreamIn);
    Close(PipeOut);
    Close(StreamErr);
    Close(PipeErr);
    Close(PipeIn);
    Close(StreamOut);
    exit;
  end;

  if pid = 0 then begin
    // *** We are in the child ***
    // Close what we don not need
    Close(StreamOut);
    Close(StreamIn);
    Close(StreamErr);
    // Connect pipes
    dup2(PipeIn, Input);
    if LinuxError <> 0 then Halt(127);
    Close(PipeIn);
    dup2(PipeOut, Output);
    if LinuxError <> 0 then Halt(127);
    Close(PipeOut);
    dup2(PipeErr, StdErr);
    if LinuxError <> 0 then Halt(127);
    Close(PipeErr);
    // Execute program
    Execl(Prog);
    Halt(127);
  end else begin
    // *** We are in the parent ***
    Close(PipeErr);
    Close(PipeOut);
    Close(PipeIn);
    // Save the process ID - needed when closing
    pl := plongint(@(TextRec(StreamIn).userdata[2]));
    pl^ := pid;
    TextRec(StreamIn).closefunc := @PCloseText;
    // Save the process ID - needed when closing
    pl := plongint(@(TextRec(StreamOut).userdata[2]));
    pl^ := pid;
    TextRec(StreamOut).closefunc := @PCloseText;
    // Save the process ID - needed when closing
    pl := plongint(@(TextRec(StreamErr).userdata[2]));
    pl^ := pid;
    TextRec(StreamErr).closefunc := @PCloseText;
    AssignStream := pid;
  end;
end;


{******************************************************************************
                        General information calls
******************************************************************************}


Function GetEnv(P:string):Pchar;
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}
var
  ep    : ppchar;
  found : boolean;
Begin
  p:=p+'=';            {Else HOST will also find HOSTNAME, etc}
  ep:=envp;
  found:=false;
  if ep<>nil then
   begin
     while (not found) and (ep^<>nil) do
      begin
        if strlcomp(@p[1],(ep^),length(p))=0 then
         found:=true
        else
         inc(ep);
      end;
   end;
  if found then
   getenv:=ep^+length(p)
  else
   getenv:=nil;
end;


{$ifndef bsd}
Function GetDomainName:String;
{
  Get machines domain name. Returns empty string if not set.
}
Var
  Sysn : utsname;
begin
  Uname(Sysn);
  linuxerror:=errno;
  If linuxerror<>0 then
   getdomainname:=''
  else
   getdomainname:=strpas(@Sysn.domainname[0]);
end;



Function GetHostName:String;
{
  Get machines name. Returns empty string if not set.
}
Var
  Sysn : utsname;
begin
  uname(Sysn);
  linuxerror:=errno;
  If linuxerror<>0 then
   gethostname:=''
  else
   gethostname:=strpas(@Sysn.nodename[0]);
end;
{$endif}

{******************************************************************************
                          Signal handling calls
******************************************************************************}

procedure SigRaise(sig:integer);
begin
  Kill(GetPid,Sig);
end;


{******************************************************************************
                         IOCtl and Termios calls
******************************************************************************}


Function TCGetAttr(fd:longint;var tios:TermIOS):boolean;
begin
 {$ifndef BSD}
  TCGetAttr:=IOCtl(fd,TCGETS,@tios);
 {$else}
  TCGETAttr:=IoCtl(Fd,TIOCGETA,@tios);
 {$endif}
end;



Function TCSetAttr(fd:longint;OptAct:longint;const tios:TermIOS):boolean;
var
  nr:longint;
begin
 {$ifndef BSD}
  case OptAct of
   TCSANOW   : nr:=TCSETS;
   TCSADRAIN : nr:=TCSETSW;
   TCSAFLUSH : nr:=TCSETSF;
 {$else}
  case OptAct of
   TCSANOW   : nr:=TIOCSETA;
   TCSADRAIN : nr:=TIOCSETAW;
   TCSAFLUSH : nr:=TIOCSETAF;
  {$endif}
  else
   begin
     ErrNo:=Sys_EINVAL;
     TCSetAttr:=false;
     exit;
   end;
  end;
  TCSetAttr:=IOCtl(fd,nr,@Tios);
end;



Procedure CFSetISpeed(var tios:TermIOS;speed:Cardinal);
begin
 {$ifndef BSD}
  tios.c_cflag:=Cardinal(tios.c_cflag and cardinal(not CBAUD)) or speed;
 {$else}
  tios.c_ispeed:=speed; {Probably the Bxxxx speed constants}
 {$endif}
end;



Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal);
begin
  {$ifndef BSD}
   CFSetISpeed(tios,speed);
  {$else}
   tios.c_ospeed:=speed;
  {$endif}
end;




Procedure CFMakeRaw(var tios:TermIOS);
begin
 {$ifndef BSD}
  with tios do
   begin
     c_iflag:=c_iflag and cardinal(not (IGNBRK or BRKINT or PARMRK or ISTRIP or
                                INLCR or IGNCR or ICRNL or IXON));
     c_oflag:=c_oflag and cardinal(not OPOST);
     c_lflag:=c_lflag and cardinal(not (ECHO or ECHONL or ICANON or ISIG or IEXTEN));
     c_cflag:=(c_cflag and cardinal(not (CSIZE or PARENB))) or CS8;
   end;
 {$else}
  with tios do
   begin
     c_iflag:=c_iflag and (not (IMAXBEL or IXOFF or INPCK or BRKINT or
                PARMRK or ISTRIP or INLCR or IGNCR or ICRNL or IXON or
                IGNPAR));
     c_iflag:=c_iflag OR IGNBRK;
     c_oflag:=c_oflag and (not OPOST);
     c_lflag:=c_lflag and (not (ECHO or ECHOE or ECHOK or ECHONL or ICANON or
                                ISIG or IEXTEN or NOFLSH or TOSTOP or PENDIN));
     c_cflag:=(c_cflag and (not (CSIZE or PARENB))) or (CS8 OR cread);
     c_cc[VMIN]:=1;
     c_cc[VTIME]:=0;
   end;
 {$endif}
end;


Function TCSendBreak(fd,duration:longint):boolean;
begin
  {$ifndef BSD}
  TCSendBreak:=IOCtl(fd,TCSBRK,pointer(duration));
  {$else}
  TCSendBreak:=IOCtl(fd,TIOCSBRK,0);
  {$endif}
end;



Function TCSetPGrp(fd,id:longint):boolean;
begin
  TCSetPGrp:=IOCtl(fd,TIOCSPGRP,pointer(id));
end;



Function TCGetPGrp(fd:longint;var id:longint):boolean;
begin
  TCGetPGrp:=IOCtl(fd,TIOCGPGRP,@id);
end;


Function TCDrain(fd:longint):boolean;
begin
 {$ifndef BSD}
  TCDrain:=IOCtl(fd,TCSBRK,pointer(1));
 {$else}
  TCDrain:=IOCtl(fd,TIOCDRAIN,0); {Should set timeout to 1 first?}
 {$endif}
end;



Function TCFlow(fd,act:longint):boolean;
begin
  {$ifndef BSD}
   TCFlow:=IOCtl(fd,TCXONC,pointer(act));
  {$else}
    case act OF
     TCOOFF :  TCFlow:=Ioctl(fd,TIOCSTOP,0);
     TCOOn  :  TCFlow:=IOctl(Fd,TIOCStart,0);
     TCIOFF :  {N/I}
    end;
  {$endif}
end;



Function TCFlush(fd,qsel:longint):boolean;

begin
 {$ifndef BSD}
  TCFlush:=IOCtl(fd,TCFLSH,pointer(qsel));
 {$else}
  TCFlush:=IOCtl(fd,TIOCFLUSH,pointer(qsel));
 {$endif}
end;

Function IsATTY(Handle:Longint):Boolean;
{
  Check if the filehandle described by 'handle' is a TTY (Terminal)
}
var
  t : Termios;
begin
 IsAtty:=TCGetAttr(Handle,t);
end;



Function IsATTY(var f: text):Boolean;
{
  Idem as previous, only now for text variables.
}
begin
  IsATTY:=IsaTTY(textrec(f).handle);
end;



function TTYName(Handle:Longint):string;
{
  Return the name of the current tty described by handle f.
  returns empty string in case of an error.
}
{$ifdef BSD}
var
  mydev,
  myino     : cardinal;
{$else not BSD}
var
  mydev,
  myino     : longint;
{$endif not BSD}
  st        : stat;

  function mysearch(n:string): boolean;
  {searches recursively for the device in the directory given by n,
    returns true if found and sets the name of the device in ttyname}
  var dirstream : pdir;
      d         : pdirent;
      name      : string;
      st        : stat;
  begin
    dirstream:=opendir(n);
    if (linuxerror<>0) then
     exit;
    d:=Readdir(dirstream);
    while (d<>nil) do
     begin
       name:=n+'/'+strpas(@(d^.name[0]));
       fstat(name,st);
       if linuxerror=0 then
        begin
          if ((st.mode and $E000)=$4000) and  { if it is a directory }
             (strpas(@(d^.name[0]))<>'.') and    { but not ., .. and fd subdirs }
             (strpas(@(d^.name[0]))<>'..') and
             (strpas(@(d^.name[0]))<>'') and
             (strpas(@(d^.name[0]))<>'fd') then
           begin                      {we found a directory, search inside it}
             if mysearch(name) then
              begin                 {the device is here}
                closedir(dirstream);  {then don't continue searching}
                mysearch:=true;
                exit;
              end;
           end
          else if (d^.ino=myino) and (st.dev=mydev) then
           begin
             closedir(dirstream);
             ttyname:=name;
             mysearch:=true;
             exit;
           end;
        end;
       d:=Readdir(dirstream);
     end;
    closedir(dirstream);
    mysearch:=false;
  end;

begin
  TTYName:='';
  fstat(handle,st);
  if (errno<>0) and isatty (handle) then
   exit;
  mydev:=st.dev;
  myino:=st.ino;
  mysearch('/dev');
end;

function TTYName(var F:Text):string;
{
  Idem as previous, only now for text variables;
}
begin
  TTYName:=TTYName(textrec(f).handle);
end;



{******************************************************************************
                             Utility calls
******************************************************************************}

Function Octal(l:longint):longint;
{
  Convert an octal specified number to decimal;
}
var
  octnr,
  oct : longint;
begin
  octnr:=0;
  oct:=0;
  while (l>0) do
   begin
     oct:=oct or ((l mod 10) shl octnr);
     l:=l div 10;
     inc(octnr,3);
   end;
  Octal:=oct;
end;

Function StringToPPChar(S: PChar):ppchar;
var
  nr  : longint;
  Buf : ^char;
  p   : ppchar;

begin
  buf:=s;
  nr:=0;
  while(buf^<>#0) do
   begin
     while (buf^ in [' ',#9,#10]) do
      inc(buf);
     inc(nr);
     while not (buf^ in [' ',#0,#9,#10]) do
      inc(buf);
   end;
  getmem(p,(nr+1)*4);
  StringToPPChar:=p;
  if p=nil then
   begin
     LinuxError:=sys_enomem;
     exit;
   end;
  buf:=s;
  while (buf^<>#0) do
   begin
     while (buf^ in [' ',#9,#10]) do
      begin
        buf^:=#0;
        inc(buf);
      end;
     p^:=buf;
     inc(p);
     p^:=nil;
     while not (buf^ in [' ',#0,#9,#10]) do
      inc(buf);
   end;
end;

Function StringToPPChar(Var S:String):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially useful for creating an ArgV for Exec-calls
  Note that the string S is destroyed by this call.
}

begin
  S:=S+#0;
  StringToPPChar:=StringToPPChar(@S[1]);
end;

Function StringToPPChar(Var S:AnsiString):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially useful for creating an ArgV for Exec-calls
}

begin
  StringToPPChar:=StringToPPChar(PChar(S));
end;


{
function FExpand (const Path: PathStr): PathStr;
- declared in fexpand.inc
}

{$DEFINE FPC_FEXPAND_TILDE} { Tilde is expanded to home }
{$DEFINE FPC_FEXPAND_GETENVPCHAR} { GetEnv result is a PChar }

const
  LFNSupport = true;
  FileNameCaseSensitive = true;
  FileNameCasePreserving = true;

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_GETENVPCHAR}
{$UNDEF FPC_FEXPAND_TILDE}



Function FSearch(const path:pathstr;dirlist:string):pathstr;
{
  Searches for a file 'path' in the list of direcories in 'dirlist'.
  returns an empty string if not found. Wildcards are NOT allowed.
  If dirlist is empty, it is set to '.'
}
Var
  NewDir : PathStr;
  p1     : Longint;
  Info   : Stat;
Begin
{Replace ':' with ';'}
  for p1:=1to length(dirlist) do
   if dirlist[p1]=':' then
    dirlist[p1]:=';';
{Check for WildCards}
  If (Pos('?',Path) <> 0) or (Pos('*',Path) <> 0) Then
   FSearch:='' {No wildcards allowed in these things.}
  Else
   Begin
     Dirlist:='.;'+dirlist;{Make sure current dir is first to be searched.}
     Repeat
       p1:=Pos(';',DirList);
       If p1=0 Then
        p1:=255;
       NewDir:=Copy(DirList,1,P1 - 1);
       if NewDir[Length(NewDir)]<>'/' then
        NewDir:=NewDir+'/';
       NewDir:=NewDir+Path;
       Delete(DirList,1,p1);
       if FStat(NewDir,Info) then
        Begin
          If Pos('./',NewDir)=1 Then
           Delete(NewDir,1,2);
        {DOS strips off an initial .\}
        End
       Else
        NewDir:='';
     Until (DirList='') or (Length(NewDir) > 0);
     FSearch:=NewDir;
   End;
End;



Procedure FSplit(const Path:PathStr;Var Dir:DirStr;Var Name:NameStr;Var Ext:ExtStr);
Var
  DotPos,SlashPos,i : longint;
Begin
  SlashPos:=0;
  DotPos:=256;
  i:=Length(Path);
  While (i>0) and (SlashPos=0) Do
   Begin
     If (DotPos=256) and (Path[i]='.') Then
      begin
        DotPos:=i;
      end;
     If (Path[i]='/') Then
      SlashPos:=i;
     Dec(i);
   End;
  Ext:=Copy(Path,DotPos,255);
  Dir:=Copy(Path,1,SlashPos);
  Name:=Copy(Path,SlashPos + 1,DotPos - SlashPos - 1);
End;



Function Dirname(Const path:pathstr):pathstr;
{
  This function returns the directory part of a complete path.
  Unless the directory is root '/', The last character is not
  a slash.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if length(Dir)>1 then
   Delete(Dir,length(Dir),1);
  DirName:=Dir;
end;



Function Basename(Const path:pathstr;Const suf:pathstr):pathstr;
{
  This function returns the filename part of a complete path. If suf is
  supplied, it is cut off the filename.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if Suf<>Ext then
   Name:=Name+Ext;
  BaseName:=Name;
end;



Function FNMatch(const Pattern,Name:string):Boolean;
Var
  LenPat,LenName : longint;

  Function DoFNMatch(i,j:longint):Boolean;
  Var
    Found : boolean;
  Begin
  Found:=true;
  While Found and (i<=LenPat) Do
   Begin
     Case Pattern[i] of
      '?' : Found:=(j<=LenName);
      '*' : Begin
            {find the next character in pattern, different of ? and *}
              while Found and (i<LenPat) do
                begin
                inc(i);
                case Pattern[i] of
                  '*' : ;
                  '?' : begin
                          inc(j);
                          Found:=(j<=LenName);
                        end;
                else
                  Found:=false;
                end;
               end;
            {Now, find in name the character which i points to, if the * or ?
             wasn't the last character in the pattern, else, use up all the
             chars in name}
              Found:=true;
              if (i<=LenPat) then
                begin
                repeat
                {find a letter (not only first !) which maches pattern[i]}
                while (j<=LenName) and (name[j]<>pattern[i]) do
                  inc (j);
                 if (j<LenName) then
                  begin
                    if DoFnMatch(i+1,j+1) then
                     begin
                       i:=LenPat;
                       j:=LenName;{we can stop}
                       Found:=true;
                     end
                    else
                     inc(j);{We didn't find one, need to look further}
                  end;
               until (j>=LenName);
                end
              else
                j:=LenName;{we can stop}
            end;
     else {not a wildcard character in pattern}
       Found:=(j<=LenName) and (pattern[i]=name[j]);
     end;
     inc(i);
     inc(j);
   end;
  DoFnMatch:=Found and (j>LenName);
  end;

Begin {start FNMatch}
  LenPat:=Length(Pattern);
  LenName:=Length(Name);
  FNMatch:=DoFNMatch(1,1);
End;



Procedure Globfree(var p : pglob);
{
  Release memory occupied by pglob structure, and names in it.
  sets p to nil.
}
var
  temp : pglob;
begin
  while assigned(p) do
   begin
     temp:=p^.next;
     if assigned(p^.name) then
      freemem(p^.name);
     dispose(p);
     p:=temp;
   end;
end;



Function Glob(Const path:pathstr):pglob;
{
  Fills a tglob structure with entries matching path,
  and returns a pointer to it. Returns nil on error,
  linuxerror is set accordingly.
}
var
  temp,
  temp2   : string[255];
  thedir  : pdir;
  buffer  : pdirent;
  root,
  current : pglob;
begin
{ Get directory }
  temp:=dirname(path);
  if temp='' then
   temp:='.';
  temp:=temp+#0;
  thedir:=opendir(@temp[1]);
  if thedir=nil then
   begin
     glob:=nil;
     linuxerror:=errno;
     exit;
   end;
  temp:=basename(path,''); { get the pattern }
  if thedir^.fd<0 then
   begin
     linuxerror:=errno;
     glob:=nil;
     exit;
   end;
{get the entries}
  root:=nil;
  current:=nil;
  repeat
    buffer:=Sys_readdir(thedir);
    if buffer=nil then
     break;
    temp2:=strpas(@(buffer^.name[0]));
    if fnmatch(temp,temp2) then
     begin
       if root=nil then
        begin
          new(root);
          current:=root;
        end
       else
        begin
          new(current^.next);
          current:=current^.next;
        end;
       if current=nil then
        begin
          linuxerror:=Sys_ENOMEM;
          globfree(root);
          break;
        end;
       current^.next:=nil;
       getmem(current^.name,length(temp2)+1);
       if current^.name=nil then
        begin
          linuxerror:=Sys_ENOMEM;
          globfree(root);
          break;
        end;
       move(buffer^.name[0],current^.name^,length(temp2)+1);
     end;
  until false;
  closedir(thedir);
  glob:=root;
end;


{--------------------------------
      FiledescriptorSets
--------------------------------}

Procedure FD_Zero(var fds:fdSet);
{
  Clear the set of filedescriptors
}
begin
  FillChar(fds,sizeof(fdSet),0);
end;



Procedure FD_Clr(fd:longint;var fds:fdSet);
{
  Remove fd from the set of filedescriptors
}
begin
  fds[fd shr 5]:=fds[fd shr 5] and (not (1 shl (fd and 31)));
end;



Procedure FD_Set(fd:longint;var fds:fdSet);
{
  Add fd to the set of filedescriptors
}
begin
  fds[fd shr 5]:=fds[fd shr 5] or (1 shl (fd and 31));
end;



Function FD_IsSet(fd:longint;var fds:fdSet):boolean;
{
  Test if fd is part of the set of filedescriptors
}
begin
  FD_IsSet:=((fds[fd shr 5] and (1 shl (fd and 31)))<>0);
end;



Function GetFS (var T:Text):longint;
{
  Get File Descriptor of a text file.
}
begin
  if textrec(t).mode=fmclosed then
   exit(-1)
  else
   GETFS:=textrec(t).Handle
end;



Function GetFS(Var F:File):longint;
{
  Get File Descriptor of an unTyped file.
}
begin
  { Handle and mode are on the same place in textrec and filerec. }
  if filerec(f).mode=fmclosed then
   exit(-1)
  else
   GETFS:=filerec(f).Handle
end;


{--------------------------------
      Stat.Mode Macro's
--------------------------------}

Function S_ISLNK(m:word):boolean;
{
  Check mode field of inode for link.
}
begin
  S_ISLNK:=(m and STAT_IFMT)=STAT_IFLNK;
end;



Function S_ISREG(m:word):boolean;
{
  Check mode field of inode for regular file.
}
begin
  S_ISREG:=(m and STAT_IFMT)=STAT_IFREG;
end;



Function S_ISDIR(m:word):boolean;

{
  Check mode field of inode for directory.
}
begin
  S_ISDIR:=(m and STAT_IFMT)=STAT_IFDIR;
end;



Function S_ISCHR(m:word):boolean;
{
  Check mode field of inode for character device.
}
begin
  S_ISCHR:=(m and STAT_IFMT)=STAT_IFCHR;
end;



Function S_ISBLK(m:word):boolean;
{
  Check mode field of inode for block device.
}
begin
  S_ISBLK:=(m and STAT_IFMT)=STAT_IFBLK;
end;



Function S_ISFIFO(m:word):boolean;
{
  Check mode field of inode for named pipe (FIFO).
}
begin
  S_ISFIFO:=(m and STAT_IFMT)=STAT_IFIFO;
end;



Function S_ISSOCK(m:word):boolean;
{
  Check mode field of inode for socket.
}
begin
  S_ISSOCK:=(m and STAT_IFMT)=STAT_IFSOCK;
end;


Procedure WritePort (Port : Longint; Value : Byte);oldfpccall;
{
  Writes 'Value' to port 'Port'
}
begin
        asm
        movl port,%edx
        movb value,%al
        outb %al,%dx
        end ['EAX','EDX'];
end;

Procedure WritePort (Port : Longint; Value : Word);oldfpccall;
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movw value,%ax
        outw %ax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePort (Port : Longint; Value : Longint);oldfpccall;
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movl value,%eax
        outl %eax,%dx
        end ['EAX','EDX'];
end;


Procedure WritePortB (Port : Longint; Value : Byte);oldfpccall;
{
  Writes 'Value' to port 'Port'
}
begin
        asm
        movl port,%edx
        movb value,%al
        outb %al,%dx
        end ['EAX','EDX'];
end;

Procedure WritePortW (Port : Longint; Value : Word);oldfpccall;
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movw value,%ax
        outw %ax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePortL (Port : Longint; Value : Longint);oldfpccall;
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movl value,%eax
        outl %eax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePortl (Port : Longint; Var Buf; Count: longint);oldfpccall;
{
  Writes 'Count' longints from 'Buf' to Port
}
begin
  asm
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsl
  end ['ECX','ESI','EDX'];
end;



Procedure WritePortW (Port : Longint; Var Buf; Count: longint);oldfpccall;
{
  Writes 'Count' words from 'Buf' to Port
}
begin
  asm
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsw
  end ['ECX','ESI','EDX'];
end;



Procedure WritePortB (Port : Longint; Var Buf; Count: longint);oldfpccall;
{
  Writes 'Count' bytes from 'Buf' to Port
}
begin
  asm
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsb
  end ['ECX','ESI','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Byte);oldfpccall;
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl port,%edx
        inb %dx,%al
        movl value,%edx
        movb %al,(%edx)
        end ['EAX','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Word);oldfpccall;
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl port,%edx
        inw %dx,%ax
        movl value,%edx
        movw %ax,(%edx)
        end ['EAX','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Longint);oldfpccall;
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl port,%edx
        inl %dx,%eax
        movl value,%edx
        movl %eax,(%edx)
        end ['EAX','EDX'];
end;



function ReadPortB (Port : Longint): Byte;oldfpccall; assembler;
{
  Reads a byte from port 'Port'
}

asm
  xorl %eax,%eax
  movl port,%edx
  inb %dx,%al
end ['EAX','EDX'];



function ReadPortW (Port : Longint): Word;oldfpccall; assembler;
{
  Reads a word from port 'Port'
}
asm
  xorl %eax,%eax
  movl port,%edx
  inw %dx,%ax
end ['EAX','EDX'];



function ReadPortL (Port : Longint): LongInt;oldfpccall; assembler;
{
  Reads a LongInt from port 'Port'
}
asm
  movl port,%edx
  inl %dx,%eax
end ['EAX','EDX'];



Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);oldfpccall;
{
  Reads 'Count' longints from port 'Port' to 'Buf'.
}
begin
  asm
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
        insl
  end ['ECX','EDI','EDX'];
end;



Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);oldfpccall;
{
  Reads 'Count' words from port 'Port' to 'Buf'.
}
begin
  asm
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
        insw
  end ['ECX','EDI','EDX'];
end;



Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);oldfpccall;
{
  Reads 'Count' bytes from port 'Port' to 'Buf'.
}
begin
  asm
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
         insb
  end ['ECX','EDI','EDX'];
end;

{--------------------------------
      Memory functions
--------------------------------}

Initialization
  InitLocalTime;

finalization
  DoneLocalTime;

End.
