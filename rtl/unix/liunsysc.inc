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

begin
  Fork:=fpfork;
  LinuxError:=fpgetErrno;
End;

Procedure Execve(path:pathstr;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

begin
  fpexecve(path,args,ep);
  Linuxerror:=fpgeterrno;
end;

Procedure Execve(path:pchar;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}
begin
  fpexecve(path,args,ep);
  Linuxerror:=fpgeterrno;
end;

Procedure ExitProcess(val:longint);
begin
  fpexit(val);
end;

Function WaitPid(Pid:longint;Status:pointer;Options:Longint):Longint;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}
begin
  WaitPid:=fpWaitPid(pid,pcint(status),options);
  LinuxError:=fpgeterrno;
end;

Procedure GetTimeOfDay(var tv:timeval);
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
begin
  gettimeofday(tv);
  LinuxError:=fpgetErrno;
end;

Function GetPriority(Which,Who:longint):longint;
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
  fpseterrno(0);
  if (which<prio_process) or (which>prio_user) then
   begin
     { We can save an interrupt here }
     getpriority:=0;
     linuxerror:=ESyseinval;
   end
  else
   begin
     getpriority:=fpgetpriority(which,who);
     linuxerror:=fpgeterrno;
   end;
end;

Procedure SetPriority(Which,Who,What:Longint);
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
begin
  fpseterrno(0);
  if ((which<prio_process) or (which>prio_user)) or ((what<-20) or (what>20)) then
   linuxerror:=ESyseinval  { We can save an interrupt here }
  else
   begin
     fpsetpriority(which,who,what);
     linuxerror:=fpgeterrno;
   end;
end;

Procedure Nice(N:integer);
{
  Set process priority. A positive N means a lower priority.
  A negative N decreases priority.
}
begin
  fpnice(n);
end;

Function GetPid:LongInt;
{
  Get Process ID.
}
begin
  GetPid:=fpgetpid;
  linuxerror:=fpgeterrno;
end;

Function GetPPid:LongInt;
{
  Get Process ID of parent process.
}

begin
  GetPpid:=fpgetppid;
  linuxerror:=fpgeterrno;
end;

Function GetUid:Longint;
{
  Get User ID.
}
begin
  GetUid:=fpgetuid;
  Linuxerror:=fpgeterrno;
end;

Function GetEUid:Longint;
{
  Get _effective_ User ID.
}
begin
  GetEuid:=fpgeteuid;
  Linuxerror:=fpgeterrno;
end;

Function GetGid:Longint;
{
  Get Group ID.
}
begin
  Getgid:=fpgetgid;
  Linuxerror:=fpgeterrno;
end;

Function GetEGid:Longint;
{
  Get _effective_ Group ID.
}
begin
  GetEgid:=fpgetegid;
  Linuxerror:=fpgeterrno;
end;

Function GetTimeOfDay: longint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
//var
//  tv   : timeval;
begin
  gettimeofday:=fptime;
//  regs.reg2:=longint(@tv);
// regs.reg3:=0;
//  SysCall(SysCall_nr_gettimeofday,regs);
  LinuxError:=fpgetErrno;
//  GetTimeOfDay:=tv.tv_sec;
end;

Function fdTruncate(fd,size:longint):boolean;
begin
//  Regs.reg2:=fd;
//  Regs.reg3:=size;
  fdTruncate:=fpftruncate(fd,size)=0;
//(SysCall(Syscall_nr_ftruncate,regs)=0);
  LinuxError:=fpgetErrno;
end;

Function  fdFlush (fd : Longint) : Boolean;
begin
  fdFlush :=unix.fdflush(fd);
  LinuxError:=fpgetErrno;
end;

Function Fcntl(Fd:longint;Cmd:longint):longint;
{
  Read or manipulate a file.(See also fcntl (2) )
  Possible values for Cmd are :
    F_GetFd,F_GetFl,F_GetOwn
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=ESyseninval.
}
begin
  if (cmd in [F_GetFd,F_GetFl,F_GetOwn]) then
   begin
     Linuxerror:=fpfcntl(fd,cmd);
     if linuxerror=-1 then
      begin
        linuxerror:=fpgeterrno;
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
begin
  if (cmd in [F_SetFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkw,F_SetOwn]) then
   begin
     fpfcntl(fd,cmd,arg);
     linuxerror:=fpgeterrno;
   end
  else
   linuxerror:=ESyseinval;
end;

Function Chmod(path:pathstr;Newmode:longint):Boolean;
{
  Changes the permissions of a file.
}
begin
  Chmod:=fpchmod(path,newmode)=0;
  linuxerror:=fpgeterrno;
end;

Function Chown(path:pathstr;NewUid,NewGid:longint):boolean;
{
  Change the owner and group of a file.
  A user can only change the group to a group of which he is a member.
  The super-user can change uid and gid of any file.
}
begin
  ChOwn:=fpChOwn(path,newuid,newgid)=0;
  linuxerror:=fpgeterrno;
end;

Function Utime(path:pathstr;utim:utimebuf):boolean;
begin
  utime:=fputime(path,@utim)=0;
  linuxerror:=fpgeterrno;
end;

Function  Flock (fd,mode : longint) : boolean;
begin
  flock:=unix.flock(fd,mode);
  LinuxError:=fpgeterrno;
end;

Function Fstat(Fd:Longint;var Info:stat):Boolean;
{
  Get all information on a file descriptor, and return it in info.
}
begin
  FStat:=fpfstat(fd,{$ifdef oldlinuxstat}baseunix.stat(info){$else}info{$endif})=0;
  LinuxError:=fpgetErrno;
end;

Function Lstat(Filename: PathStr;var Info:stat):Boolean;
{
  Get all information on a link (the link itself), and return it in info.
}
begin
  LStat:=fplstat(filename,@Info)=0;
  LinuxError:=fpgetErrno;
end;

Function StatFS(Path:Pathstr;Var Info:tstatfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}
begin
 statfs:=unix.statfs(path,info);
 // path:=path+#0;
 // regs.reg2:=longint(@path[1]);
 // regs.reg3:=longint(@Info);
 // StatFS:=(SysCall(SysCall_nr_statfs,regs)=0);
  LinuxError:=fpgeterrno;
end;

Function StatFS(Fd:Longint;Var Info:tstatfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}
begin
  statfs:=unix.statfs(fd,info);
//  regs.reg2:=Fd;
//  regs.reg3:=longint(@Info);
//  StatFS:=(SysCall(SysCall_nr_fstatfs,regs)=0);
  LinuxError:=fpgeterrno;
end;

Function Link(OldPath,NewPath:pathstr):boolean;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
begin
 { oldpath:=oldpath+#0;
  newpath:=newpath+#0;
  regs.reg2:=longint(@oldpath[1]);
  regs.reg3:=longint(@newpath[1]);
  Link:=SysCall(SysCall_nr_link,regs)=0;}
  Link:=Fplink(oldpath,newpath)=0;
  linuxerror:=fpgeterrno;
end;

Function Umask(Mask:Integer):integer;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}

begin
  Umask:=fpumask(mask);
  linuxerror:=0;
end;

Function Access(Path:Pathstr ;mode:longint):boolean;
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
  access:=fpaccess(path,mode)=0;
{  path:=path+#0;
  sr.reg2:=longint(@(path[1]));
  sr.reg3:=mode;
  access:=(SysCall(Syscall_nr_access,sr)=0);
}
  linuxerror:=fpgeterrno;
end;

Function  Dup(oldfile:longint;var newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}

begin
  newfile:=fpdup(oldfile);
  linuxerror:=fpgeterrno;
  Dup:=(LinuxError=0);
end;

Function Dup2(oldfile,newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}

begin
  fpdup2(oldfile,newfile);
  linuxerror:=fpgeterrno;
  Dup2:=(LinuxError=0);
end;

Function AssignPipe(var pipe_in,pipe_out:longint):boolean; external name 'FPC_SYSC_ASSIGNPIPE';

Function PClose(Var F:text) :longint;
var
  pl  : ^longint;
  res : longint;
begin
  pclose:=fpclose(Textrec(F).Handle);
//sr.reg2:=Textrec(F).Handle;
//SysCall (syscall_nr_close,sr);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

Function PClose(Var F:file) : longint;
var
  pl : ^longint;
  res : longint;
begin
//  sr.reg2:=FileRec(F).Handle;
//  SysCall (Syscall_nr_close,sr);
  fpclose(FileRec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

Function mkFifo(pathname:string;mode:longint):boolean;
begin
  mkfifo:=fpmkfifo(pathname,mode)=0;
//  pathname:=pathname+#0;
//  regs.reg2:=longint(@pathname[1]);
//  regs.reg3:=mode or STAT_IFIFO;
//  regs.reg4:=0;
//  mkFifo:=(SysCall(syscall_nr_mknod,regs)=0);
end;

Function Uname(var unamerec:utsname):Boolean;
{
  Get machine's names
}
Begin
  Uname:=fpuname(unamerec)=0;
  LinuxError:=fpgetErrno;
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
begin
  kill:=fpkill(pid,sig);
  if kill<0 then
   Kill:=0;
  linuxerror:=fpgeterrno;
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
begin
  fpsigprocmask(how,sset^,oldsset^);
  linuxerror:=fpgeterrno;
end;

Function SigPending:SigSet;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
var
  dummy : Sigset;
begin
  fpsigpending(dummy);
  linuxerror:=fpgeterrno;
  Sigpending:=dummy;
end;

Procedure SigSuspend(Mask:Sigset);
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}
begin
  fpsigsuspend(mask);
  linuxerror:=fpgeterrno;
end;

Function Signal(Signum:longint;Handler:SignalHandler):SignalHandler;
{
  Install a new handler for signal Signum.
  The old signal handler is returned.
  This call does, in fact, the same as SigAction.
}

var ret:signalhandler;

begin
  ret:=fpsignal(signum,handler);
  Linuxerror:=fpgeterrno;
  If longint(ret)=Sig_Err then
   begin
     Signal:=nil;
     Linuxerror:=fpgeterrno;
   end
  else
   begin
     Signal:=signalhandler(ret);
     linuxerror:=0;
   end;
end;

Function  Alarm(Sec : Longint) : longint;

begin
  Alarm:=FPAlarm(sec);
end;

Procedure Pause;

begin
  fppause;
end;

{
Function NanoSleep(const req : timespec;var rem : timespec) : longint;

begin
  sr.reg2:=longint(@req);
  sr.reg3:=longint(@rem);
  NanoSleep:=Syscall(syscall_nr_nanosleep,sr);
  LinuxError:=Errno;
end;
}
Function IOCtl(Handle,Ndx: Longint;Data: Pointer):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}

begin
  ioctl:=fpioctl(handle,ndx,data)=0;
  LinuxError:=fpgetErrno;
end;


function MMap(const m:tmmapargs):pointer;
begin
  mmap:=fpmmap(pointer(m.address),m.size,m.prot, m.flags,m.fd, m.offset);
  LinuxError:=fpgetErrno;
end;

function MUnMap (P : Pointer; Size : Longint) : Boolean;

begin
  MUnMap:=fpmunmap(p,size)=0;
  LinuxError:=fpgetErrno;
end;


{--------------------------------
      Port IO functions
--------------------------------}

{$ifdef i386}
{
Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
{
  Set permissions on NUM ports starting with port FROM to VALUE
  this works ONLY as root.
}


begin
  Sr.Reg2:=From;
  Sr.Reg3:=Num;
  Sr.Reg4:=Value;
  IOPerm:=Syscall(Syscall_nr_ioperm,sr)=0;
  LinuxError:=fpgetErrno;
end;

Function IoPL(Level : longint) : Boolean;


begin
  Sr.Reg2:=Level;
  IOPL:=Syscall(Syscall_nr_iopl,sr)=0;
  LinuxError:=fpgetErrno;
end;
}
{$endif i386}

{
  $Log$
  Revision 1.5  2003-09-20 17:27:05  marco
   * mmap fix.

  Revision 1.4  2003/09/16 20:52:24  marco
   * small cleanups. Mostly killing of already commented code in unix etc

  Revision 1.3  2003/09/16 16:06:02  peter
    * add typecasts for oldlinuxstat

  Revision 1.2  2003/09/15 21:07:41  marco
   * second round of linux fixes. oldlinux now works

  Revision 1.1  2003/09/15 20:30:49  marco
   * syscalls

  Revision 1.10  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.9  2003/07/08 21:23:24  peter
    * sparc fixes

  Revision 1.8  2002/12/18 16:43:26  marco
   * new unix rtl, linux part.....

  Revision 1.7  2002/09/07 16:01:20  peter
    * old logs removed and tabs fixed

  Revision 1.6  2002/03/05 20:04:25  michael
  + Patch from Sebastian for FCNTL call

}
