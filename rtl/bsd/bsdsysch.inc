
Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint;  external name  'FPC_SYSC_MMAP';
Function Sys_munmap(adr:longint;len:size_t):longint;  external name 'FPC_SYSC_MUNMAP';
Function Sys_IOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  external name  'FPC_SYSC_IOCTL';
Function sys_GetPid:LongInt;   external name  'FPC_SYSC_GETPID';
Function Sys_ReadLink(name,linkname:pchar;maxlen:longint):longint;  external name  'FPC_SYSC_READLINK';

{ Needed in both POSIX (for implementation of sleep()) as POSIX realtime extensions or  Unix/freebsd}
Function sys_NanoSleep (const req : timespec;var rem : timespec) : longint; external name 'FPC_SYSC_NANOSLEEP';

{ can be used for getdir?}
Function sys_getcwd (pt:pchar; _size:size_t):pchar; external name 'FPC_SYSC_GETCWD';