{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Some calls for the unix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ***********************************************************************}

function fpNice(N:cint):cint;
{
  Set process priority. A positive N means a lower priority.
  A negative N increases priority.

Doesn't exist in BSD. Linux emu uses setpriority in a construct as below:
}

{$ifdef cpux86_64}
var
  oldprio : cint;
{$endif}  
begin
{$ifdef cpux86_64}
  oldprio:=fpGetPriority(Prio_Process,0);
  fpNice:=fpSetPriority(Prio_Process,0,oldprio+N);
  if fpNice=0 then
    fpNice:=fpGetPriority(Prio_Process,0);
{$else}  
  fpNice:=do_syscall(Syscall_nr_nice,N);
{$endif}
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
     fpgetpriority:=-1;
     fpsetErrno(ESysEinval);
   end
  else
     fpGetPriority:=do_syscall(syscall_nr_GetPriority,which,who);
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
     fpSetPriority:=do_syscall(Syscall_nr_Setpriority,which,who,what);
   end;
end;

Function fpLstat(path:pchar;Info:pstat):cint;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
 fpLStat:=do_syscall(syscall_nr_lstat,TSysParam(path),TSysParam(info));
end;

Function fpLstat(Filename: ansistring;Info:pstat):cint;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
 fpLStat:=do_syscall(syscall_nr_lstat,TSysParam(pchar(filename)),TSysParam(info));
end;

Function fpSymlink(oldname,newname:pchar):cint;
{
  We need this for erase
}

begin
 fpsymlink:=do_syscall(syscall_nr_symlink,TSysParam(oldname),TSysParam(newname));
end;

{
 $Log$
 Revision 1.6  2004-11-03 15:00:43  marco
  * Pathstr eliminated

 Revision 1.5  2004/04/22 17:17:23  peter
   * x86-64 fixes

 Revision 1.4  2004/01/01 16:10:23  marco
  * fpreadlink(pathstr) moved to unxovl (since not platform specific),
 	small fixes for "make all OPT='-dFPC_USE_LIBC'

 Revision 1.3  2003/11/13 13:11:55  marco
  * Linuxerror remove + hdr+log added


}