{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2003 Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

function fpNice(N:cint):cint;
{
  Set process priority. A positive N means a lower priority.
  A negative N decreases priority.

Doesn't exist in BSD. Linux emu uses setpriority in a construct as below:
}

begin
  fpNice:=fpSetPriority(Prio_Process,0,N);
end;


Function fpGetPriority(Which,Who:cint):cint;
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
  if (which<prio_process) or (which>prio_user) then
   begin
     { We can save an interrupt here }
     fpgetpriority:=0;
     fpseterrno(ESysEinval);
   end
  else
   begin
    {$warning TODO BeOS fpGetPriority implementation}      
//     fpGetPriority:=do_syscall(syscall_nr_GetPriority,which,who);
   end;
end;


Function fpSetPriority(Which,Who,What:cint):cint;
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
  if ((which<prio_process) or (which>prio_user)) or ((what<-20) or (what>20)) then
   fpseterrno(ESyseinval)  { We can save an interrupt here }
  else
   begin
    {$warning TODO BeOS fpSetPriority implementation}   
//     fpSetPriority:=do_syscall(Syscall_nr_Setpriority,which,who,what);
   end;
end;

Function fpLstat(path:pchar;Info:pstat):cint;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
  {$warning TODO BeOS fpLstat implementation}
// fpLStat:=do_syscall(syscall_nr_lstat,TSysParam(path),TSysParam(info));
end;


Function fpLstat(Filename: PathStr;Info:pstat):cint;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
  {$warning TODO BeOS fpLstat implementation}
 FileName:=FileName+#0;
// fpLStat:=do_syscall(syscall_nr_lstat,TSysParam(@filename[1]),TSysParam(info));
end;

Function fpSymlink(oldname,newname:pchar):cint;
{
  We need this for erase
}

begin
  {$warning TODO BeOS fpSymlink implementation}
// fpsymlink:=do_syscall(syscall_nr_symlink,TSysParam(oldname),TSysParam(newname));
end;

Function fpReadLink(name,linkname:pchar;maxlen:cint):cint;

begin
  {$warning TODO BeOS fsync implementation}
//  fpreadlink:=do_syscall(syscall_nr_readlink, TSysParam(name),TSysParam(linkname),maxlen);
end;

{$ifndef FPC_USE_LIBC}
Function  fsync (fd : cint) : cint;

begin
  {$warning TODO BeOS fsync implementation}
//  fsync:=do_syscall(syscall_nr_fsync,fd);
end;



Function  fpFlock (fd,mode : longint) : cint;

begin
  {$warning TODO BeOS fpFlock implementation}
// Flock:=do_syscall(syscall_nr_flock,fd,mode);
end;


Function fStatFS(Fd:Longint;Var Info:tstatfs):cint;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}

begin
  {$warning TODO BeOS fStatFS implementation}
// fStatFS:=do_syscall(syscall_nr_fstatfs,fd,longint(@info));
end;

Function StatFS(path:pchar;Var Info:tstatfs):cint;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}

begin
  {$warning TODO BeOS StatFS implementation}
// StatFS:=do_syscall(syscall_nr_statfs,longint(path),longint(@info));
end;

{$endif}
