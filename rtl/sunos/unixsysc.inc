{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
     member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

//- libc funktions
const _MKNOD_VER=2;

Function cFork:longint;cdecl; external name 'fork1'; // fork1 is better here then fork
Procedure cExecve(path:pchar;args:ppchar;ep:ppchar); cdecl; external name 'execve';
Function cWaitPid(Pid:longint;Status:pointer;Options:Longint):Longint; cdecl; external name 'waitpid';
Function cGetTimeOfDay(var tv:timeval;var tz:timezone):integer;cdecl;external name 'gettimeofday';
Function cNice(n:LongInt):LongInt; cdecl; external name 'nice';
Function cGetPid:LongInt;cdecl; external name 'getpid';
Function cGetPPid:LongInt;cdecl; external name 'getppid';
Function cGetUid:Longint;cdecl; external name 'getuid';
Function cGetEUid:Longint;cdecl; external name 'geteuid';
Function cGetGid:Longint;cdecl; external name 'getgid';
Function cGetEGid:Longint;cdecl; external name 'getgid';
Function cSetUid(aUID:longint):longint;cdecl; external name 'setuid';
Function cSetGid(aGID:longint):longint;cdecl; external name 'setuid';
function cSetreUid(aRealUID,aEffUid:Longint):Longint; cdecl; external name 'setreuid';
function cSetreGid(aRealGID,aEffGid:Longint):Longint; cdecl; external name 'setreuid';
Function cfTruncate(fd,size:longint):Longint;cdecl; external name 'ftruncate';
Function cfSync (fd : Longint) : Longint; cdecl; external name 'fsync';
Function cChmod(path:pathstr;Newmode:longint):Longint; cdecl; external name 'chmod';
Function cChown(path:pathstr;NewUid,NewGid:longint):Longint;cdecl; external name 'chown';
Function cUmask(Mask:Longint):Longint;cdecl;external name 'umask';
//Function  cFlock (fd,mode : longint) : longint; cdecl; external name 'flock';
Function cDup(oldfile:longint):longint;cdecl;external name 'dup';
Function cDup2(oldfile,newfile:longint):longint;cdecl;external name 'dup2';
Function cGetPriority(Which,Who:LongInt):LongInt;cdecl; external name 'getpriority';
Function cSetPriority(Which:LongInt;Who:LongInt;What:LongInt):LongInt;cdecl; external name 'setpriority';
Function cFcntl(Fd:longint;Cmd:LongInt):LongInt;cdecl;external name 'fcntl';
Function cFcntlArg(Fd:longint;Cmd:LongInt; arg:LongInt):LongInt;cdecl;external name 'fcntl';
Function cAccess(Path:pCHar; mode:LongInt):LongInt; cdecl; external name 'access';
Function cPipe(var pip:tpipe):LongInt; cdecl; external name 'pipe';
Function cUtime(path:pchar; var utim:utimebuf):LongInt; cdecl; external name 'utime';
Function cSelect(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:PTimeVal):LongInt;cdecl; external name 'select';
Function cKill(Pid:longint;Sig:longint):LongINt;cdecl; external name 'kill';
Function cIOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt; cdecl; external name 'ioctl';
Function cAlarm(Sec : Longint) : longint;cdecl; external name 'alarm';
Function cmknod(Vers:LongInt;pathname:pchar;mode,dev:longint):longint; cdecl; external name '_xmknod';
Function clStat(Vers:LongInt; Filename:pchar;var Buffer: Stat):longint; cdecl; external name '_lxstat';
Function cfStatfs(fd:LongInt; var Info:StatFs):LongInt; cdecl; external name 'fstatvfs';
Function cStatfs(Filename:pchar;var Buffer: StatFs):longint; cdecl; external name 'statvfs';
function cMUnMap(p:pointer;size:longint):integer;cdecl;external name 'munmap';
function cNanoSleep(const req : timespec;var rem : timespec) : longint; cdecl;external name 'nanosleep';
function cPause:longint; cdecl; external name 'pause';
function cSigProcMask(How:longint;SSet,OldSSet:PSigSet):longint; cdecl; external name 'sigprocmask';
function cSigPending(var s:SigSet):integer;cdecl; external name 'sigpending';
function cSigSuspend(s:PSigSet):longint;cdecl;external name 'sigsuspend';
function _cSignal(Signum:longint;Handler:Longint):SignalHandler; cdecl; external name 'signal';
function cSysInfo(cmd:longint; buff:pchar; len:longint):longint; cdecl; external name 'sysinfo';
{$LinkLib rt} // nanosleep

procedure libcerrorfix(fl:boolean); inline;

begin
 if fl then
  begin
   Linuxerror:=libcerrno;
   errno:=liberrno;
  end
 else
  begin
    Linuxerror:=0;
    ErrNo:=0;
  end;
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
var     r       : LongInt;
begin
  r:=cFork; Fork:=r;
  libcerrorfix((r=-1));
end;

{ Solaris has no clone, there thread funktion (libthread), like thr_create, but they haven't
the same options with flags and return a TID istead of a PID.
If one is interestet he might look to a Hack for lxrun which is contributed as diff.
Allthough the lxrun-hack dos not work at all, it shows what to take care of }

function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
var     pid     : Longint;
begin // Quick Hack, never tested, but should work if func does not believe on the Stack
  if (pointer(func)=nil) or (sp=nil) then
   begin
     LinuxError:=Sys_EInval;
     Errno:=sys_einval;
     exit(-1);
   end;
  pid:=fork;
  if (pid=0) then begin //Child
    func(args) ;
    ExitProcess(0);
   end;
  clone:=pid;
end;


Procedure Execve(path:pchar;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}
begin
  cExecve(path,args,ep);
{ This only gets set when the call fails, otherwise we don't get here ! }
  Linuxerror:=libcerrno;
  errno:=libcerrno;
end;


Procedure Execve(path:pathstr;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}
begin
  path:=path+#0; cExecve(@path[1],args,ep);
{ This only gets set when the call fails, otherwise we don't get here ! }
  Linuxerror:=libcerrno;
  errno:=libcerrno;
end;


Procedure ExitProcess(val:longint);external name '_exit'; // not 'exit' ('exit' close the shared handle)


Function WaitPid(Pid:longint;Status:pointer;Options:Longint):Longint;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}
begin
  WaitPid:=cWaitPid(Pid,Status,Options); { =>PID, -1+errno=eintr: Signal, -1+errno,  0=Ok }
  libcerrorfix(WaitPid=-1));
end;



Procedure GetTimeOfDay(var tv:timeval);
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
var     tz      : timezone;
        r       : Integer;
begin
  r:=cGetTimeOfDay(tv,tz);
  libcerrorfix (r=-1);
end;

Function GetTimeOfDay: longint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
var     tz      : timezone;
        tv      : timeval;
begin
  libcerrorfix(cGetTimeOfDay(tv,tz)=-1);
  GetTimeOfDay:=tv.sec;
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
begin
  errno:=0;
  if (which<prio_process) or (which>prio_user) then
   begin
     { We can save an interrupt here }
     getpriority:=0;
     linuxerror:=Sys_einval;
     Errno:=sys_einval;
   end
  else
   begin
     GetPriority:=cGetPriority(Which,Who);
     libcerrorfix(getpriority=-1);
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
var     r       : Integer;
begin
  errno:=0;
  if ((which<prio_process) or (which>prio_user)) or ((what<-20) or (what>20)) then
   begin
   linuxerror:=Sys_einval  { We can save an interrupt here }
   errno:=libcerrno;
   end
  else
   begin
     r:=cSetPriority(Which,Who,What);
     libcerrorfix(r=-1);
   end;
end;

Procedure Nice(N:integer);
{
  Set process priority. A positive N means a lower priority.
  A negative N decreases priority.
}
begin
  libcerrorfix(cNice(n)=-1);
end;


Function GetPid:LongInt;
{
  Get Process ID.
}
begin
  GetPid:=cGetPid;
  libcerrorfix(GetPID=-1);
end;

Function GetPPid:LongInt;
{
  Get Process ID of parent process.
}
begin
  GetPPid:=cGetPPid;
  libcerrorfix(GetPPID=-1);
end;


Function GetUid:Longint;
{
  Get User ID.
}
begin
  GetUid:=cGetUid;
  libcerrorfix (GetUid=-1);
end;


Function GetEUid:Longint;
{
  Get _effective_ User ID.
}
begin
  GetEUid:=cGetEUid;
  libcerrorfix(GetEUid=-1);
end;


Function GetGid:Longint;
{
  Get Group ID.
}
begin
  GetGid:=cGetGid;
  libcerrorfix(GetGid=-1);
end;

Function GetEGid:Longint;
{
  Get _effective_ Group ID.
}
begin
  GetEGid:=cGetEGid;
  libcerrorfix (GetEGid=-1);
end;

// Set the real userid/groupid (uid/gid from calling process)
function SetUid(aUID:Longint):Boolean;
begin
  SetUid:=(cSetUid(aUid)=0);
  libcerrorfix( not(SetUid));
end;

function SetGid(aGID:Longint):Boolean;
begin
  SetGid:=(cSetGid(aGid)=0);
  libcerrorfix( not(SetGid));
end;

// Set the real and effective userid/groupid (like setuid/setgid bit in file permissions)
function SetreUid(aRealUID,aEffUid:Longint):Boolean;
begin
  SetreUid:=(cSetreUid(aRealUID,aEffUID)=0);
  libcerrorfix( not(SetreUid));
end;

function SetreUid(aUID:Longint):Boolean;
 begin
   SetreUid:=SetreUid(aUID,aUID);
 end;

function SetreGid(aRealGid,aEffGid:Longint):Boolean; overload;
begin
  SetreGid:=(cSetreUid(aRealGID,aEffGID)=0);
  libcerrorfix(not(SetreGid));
end;

function SetreGid(aGid:Longint):Boolean;overload;
begin
  SetreGid:=SetreGid(aGID,aGID);
end;

Function fdTruncate(fd,size:longint):boolean;
begin
  fdTruncate:=cfTruncate(fd,size)<>-1;
  libcerrorfix(not fdTruncate);
end;

Function  fdFlush (fd : Longint) : Boolean;
begin
  fdFlush:=cfSync(fd)<>-1;
  libcerrorfix( not fdFlush);
end;

Function Fcntl(Fd:longint;Cmd:integer):integer;
{
  Read or manipulate a file.(See also fcntl (2) )
  Possible values for Cmd are :
    F_GetFd,F_GetFl,F_GetOwn F_DUPFd, F_Dup2FD...
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=Sys_eninval.
}
begin
  // the retun is not compatible to the linux-definition (returning 0 on -1 (err)), but 0 may be a valid return
  if (cmd in [F_GetFd,F_GetFl,F_GetOwn, {solaris:} F_DupFd]) then
   begin
     Fcntl:=cFcntl(fd,Cmd);
     libcerrorfix(Fcntl=-1);
   end
  else
   begin
     linuxerror:=Sys_einval;
     Errno:=sys_einval;
     Fcntl:=-1;
   end;
end;

Procedure Fcntl(Fd:longint;Cmd:Integer;Arg:Longint);
{
  Read or manipulate a file. (See also fcntl (2) )
  Possible values for Cmd are :
    F_setFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkW,F_SetOwn;
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=Sys_eninval.
  F_DupFD is not allowed, due to the structure of Files in Pascal.
}

begin
  if (cmd in [F_SetFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkw,F_SetOwn {Solaris:} ,F_Dup2Fd,F_FreeSp{,F_GetLk64,F_SetLk64,F_SetLkw64}]) then
   begin
     libcerrorfix( cFcntlArg(fd,Cmd,Arg)=-1);
   end
  else begin
   linuxerror:=Sys_einval;
   errno:=sys_einval;
  end;
end;


Function Chmod(path:pathstr;Newmode:longint):Boolean;
{
  Changes the permissions of a file.
}
begin
  Chmod:=cChmod(path,NewMode)=0;
  libcerrorfix( not Chmod);
end;


Function Chown(path:pathstr;NewUid,NewGid:longint):boolean;
{
  Change the owner and group of a file.
  A user can only change the group to a group of which he is a member.
  The super-user can change uid and gid of any file.
}
begin
  Chown:=cChown(path,NewUid,NewGid)=0;
  libcerrorfix(not Chown);
end;


Function Utime(path:pathstr;utim:utimebuf):boolean;
begin
  path:=path+#0;
  UTime:=cUtime(@Path[1],utim)=0;
  libcerrorfix( not UTime);
end;


Function  Flock (fd,mode : longint) : boolean;
begin
  FLock:=TRUE;
//  FLock:=cFLock(fd,mode)=0;
  libcerrorfix(not FLock);
end;



Function Fstat(Fd:Longint;var Info:stat):Boolean;
{
  Get all information on a file descriptor, and return it in info.
}
begin
  FStat:=Sys_fstat(fd,Info)=0;
  libcerrorfix( not FStat);
end;


Function Lstat(Filename: PathStr;var Info:stat):Boolean;
{
  Get all information on a link (the link itself), and return it in info.
}
begin
  FileName:=FileName+#0;
  LStat:=clstat(STAT_VERS,@FileName[1],Info)=0;
  libcerrorfix( not LStat);
end;



Function FSStat(Path:Pathstr;Var Info:statfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}
begin
  path:=path+#0;
  FSStat:=cstatfs(@path[1],Info)=0;
  libcerrorfix(not FSStat);
end;



Function FSStat(Fd:Longint;Var Info:statfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}
begin
  FSStat:=cfstatfs(fd,Info)=0;
  libcerrorfix( not FSStat);
end;



Function Link(OldPath,NewPath:pathstr):boolean;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
begin
  OldPath:=OldPath+#0; NewPath:=NewPath+#0;
  Link:=Sys_SymLink(@OldPath[1],@NewPath[1])<>-1;
  libcerrorfix( not Link);
end;


Function Umask(Mask:Integer):integer;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}
begin
  Umask:=cUmask(Mask);
  libcerrorfix (Umask=-1);
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
begin
  path:=path+#0;
  Access:=cAccess(@Path[1],mode)=0;
  libcerrorfix( not Access);
end;

Function  Dup(oldfile:longint;var newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}
begin
  NewFile:=cDup(OldFile);
  Dup:=(NewFile<>-1);
  libcerrorfix( not Dup);
end;

Function Dup2(oldfile,newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}
begin
  Dup2:=cDup2(OldFile,NewFile)<>-1;
  libcerrorfix( not Dup2);
end;


Function Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:PTimeVal):longint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
}
Var
  SelectArray : Array[1..5] of longint;
begin
  Select:=cSelect(N,readfds,writefds,exceptfds,TimeOut);
  libcerrorfix( Select=-1);
end;

Function AssignPipe(var pipe_in,pipe_out:longint):boolean;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  pip  : tpipe;
begin
  AssignPipe:=cPipe(pip)=0;
  if AssignPipe then begin
    pipe_in:=pip[1];
    pipe_out:=pip[2];
    LinuxError:=0;
    Errno:=0;
   end
  else
   begin
    LinuxError:=libcErrNo;
    Errno:=libcerrno;
   end;
end;

Function PClose(Var F:text) :longint;
var
  pl : ^longint;
begin
  Sys_Close(Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  pclose:=WaitProcess(pl^);
end;


Function PClose(Var F:file) : longint;
var
  pl : ^longint;
begin
  Sys_Close(Filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  pclose:=WaitProcess(pl^);
end;


Function Sysinfo(var Info:TSysinfo):Boolean; // ToDO
{
  Get system info
  (Mvdv:Linux specific, not implemented under FreeBSD too.
   Under FreeBSD I will simply implement a sysctl unit)
}
var
  regs : SysCallregs;
Begin
(*  regs.reg2:=longint(@info);
  Sysinfo:=SysCall(SysCall_nr_Sysinfo,regs)=0;*)
  WriteLN('SysInfo not supported yet ');
End;


Function mkFifo(pathname:string;mode:longint):boolean;
begin
  pathname:=pathname+#0;
  mode:=mode or STAT_IFIFO;
  mkFifo:=cmknod(_MKNOD_VER,@pathname[1],mode,0)=0;
  libcerrorfix(not mkFifo);
end;

{
Function Uname(var unamerec:utsname):Boolean; // ToDo
{
  Get machine's names
}
var
  regs : SysCallregs;
Begin
  Errno:=0;
  cSysInfo(SI_SYSNAME,@unamerec.sysname,SizeOf(unamerec.sysname));
  cSysInfo(SI_HOSTNAME,@unamerec.nodename,SizeOf(unamerec.nodename));
  cSysInfo(SI_RELEASE,@unamerec.release,SizeOf(unamerec.release));
  cSysInfo(SI_VERSION,@unamerec.version,SizeOf(unamerec.version));
  cSysInfo(SI_MACHINE,@unamerec.machine,SizeOf(unamerec.machine));
  cSysInfo(SI_SRPC_DOMAIN,@unamerec.domainname,SizeOf(unamerec.domainname));
  LinuxError:=Errno;
End;
}

Function Kill(Pid:longint;Sig:longint):integer;
{
  Send signal 'sig' to a process, or a group of processes.
  If Pid >  0 then the signal is sent to pid
     pid=-1                         to all processes except process 1
     pid < -1                         to process group -pid
  Return value is zero, except for case three, where the return value
  is the number of processes to which the signal was sent.
}
begin
  Kill:=cKill(PID,Sig);
  libcerrorfix( Kill=-1);
  if kill<0 then Kill:=0; // from the linux source
end;



Procedure SigProcMask(How:longint;SSet,OldSSet:PSigSet); //ToDo
{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}
begin
  libcerrorfix( cSigProcMask(How,SSet,OldSSet)=0);
end;


Function SigPending:SigSet;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
Var
  dummy : Sigset;
begin
  libcerrorfix(cSigPending(dummy)=0);
  Sigpending:=dummy;
end;


Procedure SigSuspend(Mask:Sigset); // ToDo
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}
begin
  libcerrorfix (cSigSuspend(@Mask)=0);
end;

Function Signal(Signum:longint;Handler:SignalHandler):SignalHandler;
{
  Install a new handler for signal Signum.
  The old signal handler is returned.
  This call does, in fact, the same as SigAction.
}
var
  r :LongInt;
begin
  Signal:=NIL;
  r:=longint(_cSignal(Signum,longint(Handler)));
  if (r=-1) then begin
     Signal:=nil;
     LinuxError:=libcerrno;
     Errno:=libcerrno;
    end
   else
   begin
     Signal:=signalhandler(r);
     LinuxError:=0;
     ErrNo:=0;
    end;
  exit;
end;


Function  Alarm(Sec : Longint) : longint;
begin
  Alarm:=cAlarm(Sec);
  LinuxError:=0; // no error
  Errno:=0;
end;

Procedure Pause;
begin cPause;end;


Function NanoSleep(const req : timespec;var rem : timespec) : longint;
begin
  NanoSleep:=cNanoSleep(req,rem);
  Libcerrorfix( NanoSleep=-1);
end;


Function IOCtl(Handle,Ndx: Longint;Data: Pointer):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
begin
  IOCtl:=cIOCtl(Handle,Ndx,Data)<>-1;
  libcerrorfix( not IOCtl);
end;

function MUnMap (P : Pointer; Size : Longint) : Boolean;
begin
  MUnMap:=cMUnMap(p,size)=0;
  libcerrorfix( not MUnMap);
end;

{--------------------------------
      Port IO functions
--------------------------------}
{
// all of them has to be checked for soalris
Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
{
  Set permissions on NUM ports starting with port FROM to VALUE
  this works ONLY as root.
}

Var
  Sr : Syscallregs;
begin
(*  Sr.Reg2:=From;
  Sr.Reg3:=Num;
  Sr.Reg4:=Value;
  IOPerm:=Syscall(Syscall_nr_ioperm,sr)=0;
  LinuxError:=Errno;*)
  WriteLN('IOperm not suppoted yet');
end;

Function IoPL(Level : longint) : Boolean;

Var
  Sr : Syscallregs;
begin
(*  Sr.Reg2:=Level;
  IOPL:=Syscall(Syscall_nr_iopl,sr)=0;
  LinuxError:=Errno;*)
  WriteLN('IoPL not suppoted yet');
end;

{
  $Log$
  Revision 1.3  2002-09-07 16:01:26  peter
    * old logs removed and tabs fixed

}
