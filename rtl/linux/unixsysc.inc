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
     linuxerror:=ESyseinval;
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
   linuxerror:=ESyseinval  { We can save an interrupt here }
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



Function Fcntl(Fd:longint;Cmd:longint):longint;
{
  Read or manipulate a file.(See also fcntl (2) )
  Possible values for Cmd are :
    F_GetFd,F_GetFl,F_GetOwn
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=ESyseninval.
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
     linuxerror:=ESyseinval;
     Fcntl:=0;
   end;
end;



Procedure Fcntl(Fd:longint;Cmd : longint;Arg:Longint);
{
  Read or manipulate a file. (See also fcntl (2) )
  Possible values for Cmd are :
    F_setFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkW,F_SetOwn;
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=ESyseninval.
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
   linuxerror:=ESyseinval;
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



Function Utime(path:pathstr;utim:utimebuf):boolean;
var
  sr : Syscallregs;
begin
  path:=path+#0;
  sr.reg2:=longint(@(path[1]));
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



Function StatFS(Path:Pathstr;Var Info:tstatfs):Boolean;
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
  StatFS:=(SysCall(SysCall_nr_statfs,regs)=0);
  LinuxError:=errno;
end;



Function StatFS(Fd:Longint;Var Info:tstatfs):Boolean;
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
  StatFS:=(SysCall(SysCall_nr_fstatfs,regs)=0);
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
  pl:=@(textrec(f).userdata[2]);
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
  pl:=@(filerec(f).userdata[2]);
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
  sr.reg2:=mask[0];
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


{
  $Log$
  Revision 1.8  2002-12-18 16:43:26  marco
   * new unix rtl, linux part.....

  Revision 1.7  2002/09/07 16:01:20  peter
    * old logs removed and tabs fixed

  Revision 1.6  2002/03/05 20:04:25  michael
  + Patch from Sebastian for FCNTL call

}
