
function fpNice(N:cint):cint;
{
  Set process priority. A positive N means a lower priority.
  A negative N decreases priority.

Doesn't exist in BSD. Linux emu uses setpriority in a construct as below:
}

begin
  fpNice:=do_syscall(Syscall_nr_nice,N);
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
     linuxerror:=ESysEinval;
   end
  else
   begin
     fpGetPriority:=do_syscall(syscall_nr_GetPriority,which,who);
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

Function fpLstat(Filename: PathStr;Info:pstat):cint;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
 FileName:=FileName+#0;
 fpLStat:=do_syscall(syscall_nr_lstat,TSysParam(@filename[1]),TSysParam(info));
end;

Function fpSymlink(oldname,newname:pchar):cint;
{
  We need this for erase
}

begin
 fpsymlink:=do_syscall(syscall_nr_symlink,TSysParam(oldname),TSysParam(newname));
end;

{
Function fpReadLink(name,linkname:pchar;maxlen:cint):cint;

begin
  fpreadlink:=do_syscall(syscall_nr_readlink, TSysParam(name),TSysParam(linkname),maxlen);
end;
}

Function fpReadLink(Name:pathstr):pathstr;
{
  Read a link (where it points to)
}
var
  LinkName : pathstr;
  i : cint;
begin
  Name:=Name+#0;
  i:=fpReadLink(@Name[1],@LinkName[1],high(linkname));
  if i>0 then
   begin
     linkname[0]:=chr(i);
     fpReadLink:=LinkName;
   end
  else
   fpReadLink:='';
end;
