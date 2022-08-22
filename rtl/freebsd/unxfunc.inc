{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2000 by Marco van de Voort
     member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$ifndef HAS_LIBC_PIPING}
Function PClose(Var F:file) : cint;
var
  pl : ^cint;
  res : cint;

begin
  fpclose(filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

Function PClose(Var F:text) :cint;
var
  pl  : ^longint;
  res : longint;

begin
  fpclose(Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;
{$ENDIF}


Function AssignPipe(var pipe_in,pipe_out:cint):cint; [public, alias : 'FPC_SYSC_ASSIGNPIPE'];
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, errno is set.
}
var
  pip  : tfildes;
  flags : cint;
begin
  assignPipe:=fppipe(pip); // default parameter adds 0, if necessary. This abstracts between pipe(2) and pipe(3)
  pipe_in:=pip[0];
  pipe_out:=pip[1];
end;

