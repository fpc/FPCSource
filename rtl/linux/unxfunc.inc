{
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
     member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

Function AssignPipe(var pipe_in,pipe_out:cint):cint; [public, alias : 'FPC_SYSC_ASSIGNPIPE'];

{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  pip  : tpipe;
begin
  {$ifdef FPC_USE_LIBC}
  assignpipe:=pipe(pip);
  {$else}
  assignPipe:=fppipe(pip);
  {$endif}
  pipe_in:=pip[0];
  pipe_out:=pip[1];
end;

Function PClose(Var F:text) :cint;
var
  pl  : pcint;
  res : cint;
  pid : cint;
begin
  fpclose(Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=pcint(@(textrec(f).userdata[2]));
  { avoid alignment error on sparc }
  move(pl^,pid,sizeof(pid));
  fpwaitpid(pid,@res,0);
  pclose:=res shr 8;
end;

Function PClose(Var F:file) : cint;
var
  pl : ^cint;
  res : cint;
  pid : cint;
begin
  fpclose(filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=pcint(@(filerec(f).userdata[2]));
  { avoid alignment error on sparc }
  move(pl^,pid,sizeof(pid));
  fpwaitpid(pid,@res,0);
  pclose:=res shr 8;
end;


