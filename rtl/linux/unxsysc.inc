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

Function  fsync (fd : cint) : cint;
begin
  fsync := do_SysCall(syscall_nr_fsync, fd);
end;

Function  fpFlock (fd,mode : cint) : cint;
begin
  fpflock:=do_Syscall(Syscall_nr_flock,fd,mode);
end;


Function StatFS(Path:Pchar;Var Info:tstatfs):cint;
{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}
begin
  StatFS:=(do_SysCall(SysCall_nr_statfs,TSysParam(path),TSysParam(@Info)));
end;

Function fStatFS(Fd:cint;Var Info:tstatfs):cint;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}
begin
  fStatFS:=(do_SysCall(SysCall_nr_fstatfs,fd,TSysParam(@info)));
end;


{--------------------------------
      Port IO functions
--------------------------------}

{$ifdef cpui386}

Function IOperm (From,Num : cuint; Value : cint) : boolean;
{
  Set permissions on NUM ports starting with port FROM to VALUE
  this works ONLY as root.
}

begin
  IOPerm:=do_Syscall(Syscall_nr_ioperm,from,num,value)=0;
end;

Function IoPL(Level : cint) : Boolean;

begin
  IOPL:=do_Syscall(Syscall_nr_iopl,level)=0;
end;

{$endif cpui386}

{
 $Log$
 Revision 1.8  2005-02-13 20:01:38  peter
   * include file cleanup

 Revision 1.7  2004/11/14 12:21:08  marco
  * moved some calls from unix to baseunix. Darwin untested.

 Revision 1.6  2004/11/03 15:00:43  marco
  * Pathstr eliminated

 Revision 1.5  2004/04/22 17:17:23  peter
   * x86-64 fixes

 Revision 1.4  2004/01/01 16:10:23  marco
  * fpreadlink(pathstr) moved to unxovl (since not platform specific),
 	small fixes for "make all OPT='-dFPC_USE_LIBC'

 Revision 1.3  2003/11/13 13:11:55  marco
  * Linuxerror remove + hdr+log added


}