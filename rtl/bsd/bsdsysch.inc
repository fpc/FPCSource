
Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint;  external name  'FPC_SYSC_MMAP';
Function Sys_IOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  external name  'FPC_SYSC_IOCTL';
Function sys_GetPid:LongInt;   external name  'FPC_SYSC_GETPID';
Function Sys_ReadLink(name,linkname:pchar;maxlen:longint):longint;  external name  'FPC_SYSC_READLINK';
