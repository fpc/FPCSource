{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 2000 by Marco van de Voort
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

Begin
 fork:=Do_syscall(SysCall_nr_fork);
 LinuxError:=ErrNo;
End;

{
function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
{NOT IMPLEMENTED YET UNDER BSD}
begin
 HALT;
END;

  if (pointer(func)=nil) or (sp=nil) then
   begin
     LinuxError:=Sys_EInval;
     exit;
   end;
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
        pushl   %ebx
      //  movl    flags,%ebx
        movl    $251,%eax
        int     $0x80
        popl    %ebx
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
end;
}

Procedure Execve(path:pathstr;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

Begin
  path:=path+#0;
  do_syscall(syscall_nr_Execve,longint(@path[1]),longint(Args),longint(ep));
 LinuxError:=ErrNo;
End;

Function Umask(Mask:Integer):integer;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}
begin
 UMask:=Do_syscall(syscall_nr_umask,mask);
 LinuxError:=0;
end;

Procedure Nice(N:integer);
{
  Set process priority. A positive N means a lower priority.
  A negative N decreases priority.

Doesn't exist in BSD. Linux emu uses setpriority in a construct as below:
}

begin
  SetPriority(Prio_Process,0,N);
end;

Procedure Execve(path:pchar;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

Begin
  do_syscall(syscall_nr_Execve,longint(path),longint(Args),longint(ep));
 LinuxError:=ErrNo;
End;

Procedure ExitProcess(val:longint);

begin
 do_syscall(Syscall_nr_exit,val);
 LinuxError:=ErrNo;
end;

Function WaitPid(Pid:longint;Status:pointer;Options:Integer):Longint;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin
 WaitPID:=do_syscall(syscall_nr_WaitPID,PID,longint(Status),options,0);
 LinuxError:=ErrNo;
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
  errno:=0;
  if (which<prio_process) or (which>prio_user) then
   begin
     { We can save an interrupt here }
     getpriority:=0;
     linuxerror:=Sys_einval;
   end
  else
   begin
     GetPriority:=do_syscall(syscall_nr_GetPriority,which,who);
     LinuxError:=ErrNo;
   end;
end;

Procedure SetPriority(Which,Who,What:longint);
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
  errno:=0;
  if ((which<prio_process) or (which>prio_user)) or ((what<-20) or (what>20)) then
   linuxerror:=Sys_einval  { We can save an interrupt here }
  else
   begin
     do_syscall(Syscall_nr_Setpriority,which,who,what);
     LinuxError:=ErrNo;
   end;
end;


Function GetPid:LongInt;
{
  Get Process ID.
}

begin
 GetPID:=do_syscall(Syscall_nr_GetPID);
 LinuxError:=errno;
end;

Function GetPPid:LongInt;
{
  Get Process ID of parent process.
}


begin
  GetPPid:=do_syscall(Syscall_nr_GetPPid);
  LinuxError:=errno;
end;

Function GetUid:Longint;
{
  Get User ID.
}

begin
  GetUID:=do_syscall(Syscall_nr_GetUID);
  LinuxError:=ErrNo;
end;

Function GetEUid:Longint;
{
  Get _effective_ User ID.
}

begin
  GetEUID:=do_syscall(Syscall_nr_GetEUID);
  LinuxError:=ErrNo;
end;


Function GetGid:Longint;
{
  Get Group ID.
}

begin
  GetGID:=do_syscall(Syscall_nr_getgid);
  LinuxError:=ErrNo;
end;


Function GetEGid:Longint;
{
  Get _effective_ Group ID.
}

begin
 GetEGID:=do_syscall(syscall_nr_getegid);
 LinuxError:=ErrNo;
end;

Procedure GetTimeOfDay(var tv:timeval);
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}

var  tz : timezone;

begin
 do_syscall(syscall_nr_gettimeofday,longint(@tv),longint(@tz));
 LinuxError:=Errno;
end;

Function GetTimeOfDay: longint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
begin
  GetTimeOfDay:=Sys_time;
  LinuxError:=Errno;
end;

Function fdTruncate(fd,size:longint):boolean;

begin
 fdtruncate:=do_syscall(syscall_nr_ftruncate,fd,size,0)=0;
 LinuxError:=Errno;
end;

Function  fdFlush (fd : Longint) : Boolean;

begin
  fdflush:=do_syscall(syscall_nr_fsync,fd)=0;
  LinuxError:=Errno;
end;

function sys_fcntl(Fd:longint;Cmd:longint;Arg:Longint):longint;

begin
 sys_fcntl:=do_syscall(syscall_nr_fcntl,fd,cmd,arg);
 LinuxError:=Errno;
end;

Function Chmod(path:pathstr;Newmode:longint):Boolean;
{
  Changes the permissions of a file.
}

begin
  path:=path+#0;
  chmod:=do_syscall(syscall_nr_chmod,longint(@path[1]),newmode)=0;
  LinuxError:=Errno;
end;

Function Chown(path:pathstr;NewUid,NewGid:longint):boolean;
{
  Change the owner and group of a file.
  A user can only change the group to a group of which he is a member.
  The super-user can change uid and gid of any file.
}

begin
  path:=path+#0;
  ChOwn:=do_syscall(syscall_nr_chown,longint(@path[1]),newuid,newgid)=0;
 LinuxError:=Errno;
end;

Function Utime(path:pathstr;utim:utimebuf):boolean;

begin
  UTime:=do_syscall(syscall_nr_utimes,longint(@path[1]),longint(@utim))=0;
  LinuxError:=Errno;
end;

Function  Flock (fd,mode : longint) : boolean;

begin
 Flock:=do_syscall(syscall_nr_flock,fd,mode)=0;
 LinuxError:=Errno;
end;

Function Lstat(Filename: PathStr;var Info:stat):Boolean;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
 FileName:=FileName+#0;
 LStat:=do_syscall(syscall_nr_lstat,longint(@filename[1]),longint(@info))=0;
 LinuxError:=Errno;
end;

Function Fstat(Fd:Longint;var Info:stat):Boolean;
{
  Get all information on a file descriptor, and return it in info.
}

begin
 FStat:=do_syscall(syscall_nr_fstat,fd,longint(@info))=0;
 LinuxError:=Errno;
end;


Function FSStat(Path:Pathstr;Var Info:statfs):Boolean;

{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}

begin
  path:=path+#0;
  FSStat:=Do_Syscall(syscall_nr_statfs,longint(@path[1]),longint(@info))=0;
  LinuxError:=Errno;
end;

Function FSStat(Fd:Longint;Var Info:statfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}

begin
 FSStat:=do_syscall(syscall_nr_fstatfs,fd,longint(@info))=0;
 LinuxError:=Errno;
end;

Function Link(OldPath,NewPath:pathstr):boolean;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
begin
  oldpath:=oldpath+#0;
  newpath:=newpath+#0;
  Link:=Do_Syscall(syscall_nr_link,longint(@oldpath[1]),longint(@newpath[1]))=0;
 LinuxError:=Errno;
end;

{
Function SymLink(OldPath,newPath:pathstr):boolean;
{
  Proceduces a soft link from new to old.
}

begin
  oldpath:=oldpath+#0;
  newpath:=newpath+#0;
  SymLink:=Do_Syscall(syscall_nr_symlink,longint(@oldpath[1]),longint(@newpath[1]))=0;
  LinuxError:=Errno;
end;
}

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
  path:=path+#0;
 Access:=do_syscall(syscall_nr_access,mode,longint(@path[1]))=0;
 LinuxError:=Errno;
end;

Function  Dup(oldfile:longint;var newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}

begin
 newfile:=Do_syscall(syscall_nr_dup,oldfile);
 LinuxError:=Errno;
 Dup:=(LinuxError=0);
end;


Function Dup2(oldfile,newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}

begin
 do_syscall(syscall_nr_dup2,oldfile,newfile);
 LinuxError:=Errno;
 Dup2:=(LinuxError=0);
end;


Function Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:PTimeVal):longint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
}

begin
 Select:=do_syscall(syscall_nr_select,n,longint(readfds),longint(writefds),longint(exceptfds),longint(timeout));
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

begin
 do_syscall(syscall_nr_pipe,longint(@pip));
 LinuxError:=Errno;
 pipe_in:=pip[1];
 pipe_out:=pip[2];
 AssignPipe:=(LinuxError=0);
end;


Function PClose(Var F:text) :longint;
var
  pl  : ^longint;
  res : longint;

begin
  do_syscall(syscall_nr_close,Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  waitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

Function PClose(Var F:file) : longint;
var
  pl : ^longint;
  res : longint;

begin
  do_syscall(syscall_nr_close,filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  waitpid(pl^,@res,0);
  pclose:=res shr 8;
end;


Function mkFifo(pathname:string;mode:longint):boolean;

begin
  pathname:=pathname+#0;
  mkfifo:=do_syscall(syscall_nr_mknod,longint(@pathname[1]),mode or STAT_IFIFO,0)=0;
  LinuxError:=Errno;
end;


Function Kill(Pid:longint;Sig:integer):integer;
{
  Send signal 'sig' to a process, or a group of processes.
  If Pid >  0 then the signal is sent to pid
     pid=-1                         to all processes except process 1
     pid < -1                         to process group -pid
  Return value is zero, except for case three, where the return value
  is the number of processes to which the signal was sent.
}

begin
 kill:=do_syscall(syscall_nr_kill,pid,sig);
 if kill<0 THEN
  Kill:=0;
 LinuxError:=Errno;
end;

Procedure SigAction(Signum:Integer;Var Act,OldAct:PSigActionRec );
{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

begin
 do_syscall(syscall_nr_sigaction,longint(signum),longint(act),longint(oldact));
 LinuxError:=Errno;
end;

Procedure SigProcMask(How:Integer;SSet,OldSSet:PSigSet);
{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}

begin
  do_syscall(syscall_nr_sigprocmask,longint(how),longint(sset),longint(oldsset));
 LinuxError:=Errno;
end;

Function SigPending:SigSet;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
Var
  dummy : Sigset;
begin
  do_syscall(syscall_nr_sigpending,longint(@dummy));
  LinuxError:=Errno;
  sigpending:=dummy;
end;

Procedure SigSuspend(Mask:Sigset);
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}

begin
  do_syscall(syscall_nr_sigsuspend,longint(@mask));
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

begin
  IOCtl:=Do_Syscall(syscall_nr_ioctl,handle,ndx,longint(data))=0;
  LinuxError:=Errno;
end;

function MMap(const m:tmmapargs):longint;

begin
  {Last argument (offset) is actually 64-bit under BSD. Therefore extra 0}
 MMap:=do_syscall(syscall_nr_mmap,m.address,m.size,m.prot,m.flags,m.fd,m.offset,0);
 LinuxError:=Errno;
end;

function MUnMap (P : Pointer; Size : Longint) : Boolean;
Var
  Sr : Syscallregs;
begin
  Sr.reg2:=longint(P);
  sr.reg3:=Size;
  MUnMap:=syscall(syscall_nr_munmap,P,Size,0)=0;
  LinuxError:=Errno;
end;
{
  $Log$
  Revision 1.2  2000-07-13 11:33:47  michael
  + removed logs
 
}
