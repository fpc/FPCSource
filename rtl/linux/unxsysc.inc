{
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

