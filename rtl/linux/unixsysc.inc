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
begin
  fdFlush := (do_SysCall(syscall_nr_fsync, fd)=0);
end;

Function  Flock (fd,mode : longint) : boolean;
begin
  flock:=do_Syscall(Syscall_nr_flock,fd,mode)=0;
end;

Function StatFS(Path:Pathstr;Var Info:tstatfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}
begin
  path:=path+#0;
  StatFS:=(do_SysCall(SysCall_nr_statfs,longint(@path[1]),longint(@Info))=0);
end;

Function StatFS(Fd:Longint;Var Info:tstatfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}
begin
  StatFS:=(do_SysCall(SysCall_nr_fstatfs,fd,longint(@info))=0);
end;

Function AssignPipe(var pipe_in,pipe_out:longint):cint; [public, alias : 'FPC_SYSC_ASSIGNPIPE'];

{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  pip  : tpipe;
begin
  assignPipe:=do_SysCall(SysCall_nr_pipe,longint(@pip));
  pipe_in:=pip[1];
  pipe_out:=pip[2];
end;

Function PClose(Var F:text) :longint;
var
  pl  : ^longint;
  res : longint;
begin
  do_SysCall (syscall_nr_close,Textrec(F).Handle);
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
  do_SysCall (Syscall_nr_close,filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

{--------------------------------
      Port IO functions
--------------------------------}

{$ifdef cpui386}

Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
{
  Set permissions on NUM ports starting with port FROM to VALUE
  this works ONLY as root.
}

begin
  IOPerm:=do_Syscall(Syscall_nr_ioperm,from,num,value)=0;
end;

Function IoPL(Level : longint) : Boolean;

begin
  IOPL:=do_Syscall(Syscall_nr_iopl,level)=0;
end;

{$endif cpui386}

{
  $Log$
  Revision 1.18  2003-11-13 17:40:12  marco
   * small fixes

  Revision 1.17  2003/11/13 13:36:23  marco
   * Linuxerror removed

  Revision 1.16  2003/11/09 13:48:55  marco
   * small fix

  Revision 1.15  2003/10/30 16:42:25  marco
   * Killing off old syscall convention anywhere except for oldlinux

  Revision 1.14  2003/10/17 22:11:28  olle
    * changed i386 to cpui386

  Revision 1.13  2003/09/20 12:45:34  marco
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
