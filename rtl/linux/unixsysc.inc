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


Function  fdFlush (fd : Longint) : Boolean;
var
  SR: SysCallRegs;
begin
  SR.reg2 := fd;
  fdFlush := (SysCall(syscall_nr_fsync, SR)=0);
  LinuxError:=fpgetErrno;
end;

Function  Flock (fd,mode : longint) : boolean;
var
  sr : Syscallregs;
begin
  sr.reg2:=fd;
  sr.reg3:=mode;
  flock:=Syscall(Syscall_nr_flock,sr)=0;
  LinuxError:=fpgeterrno;
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
  LinuxError:=fpgeterrno;
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
  LinuxError:=fpgeterrno;
end;

Function AssignPipe(var pipe_in,pipe_out:longint):boolean; [public, alias : 'FPC_SYSC_ASSIGNPIPE'];

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
  linuxerror:=fpgeterrno;
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
  fpwaitpid(pl^,@res,0);
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
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

{--------------------------------
      Port IO functions
--------------------------------}

{$ifdef i386}

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
  LinuxError:=fpgetErrno;
end;

Function IoPL(Level : longint) : Boolean;

Var
  Sr : Syscallregs;
begin
  Sr.Reg2:=Level;
  IOPL:=Syscall(Syscall_nr_iopl,sr)=0;
  LinuxError:=fpgetErrno;
end;

{$endif i386}

{
  $Log$
  Revision 1.13  2003-09-20 12:45:34  marco
   * Small fix. Cycle works

  Revision 1.12  2003/09/16 21:46:27  marco
   * small fixes, checking things on linux

  Revision 1.11  2003/09/15 21:07:32  marco
   * second round of linux fixes. oldlinux now works

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
