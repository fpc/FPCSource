{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Peter Vreman

    Darwin temporary pclose/assignpipe implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



Function PClose(Var F:file) : cint;
var
  pl : ^cint;

begin
  fpclose(filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  pclose := WaitProcess(pl^);
end;

Function PClose(Var F:text) :cint;
var
  pl  : ^longint;

begin
  fpclose(Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  pclose:= WaitProcess(pl^);
end;


// can't have oldfpccall here, linux doesn't need it.
Function AssignPipe(var pipe_in,pipe_out:cint):cint; [public, alias : 'FPC_SYSC_ASSIGNPIPE'];
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  ret  : longint;
  errn : cint;
  fdis : array[0..1] of cint;
begin
 fdis[0]:=pipe_in;
 fdis[1]:=pipe_out;
 ret:=pipe(fdis);
 pipe_in:=fdis[0];
 pipe_out:=fdis[1];
 AssignPipe:=ret;
end;


{
  $Log$
  Revision 1.3  2004-07-01 18:34:53  jonas
    * adapted second pclose as well

  Revision 1.2  2004/07/01 18:28:15  jonas
    * fixed returning of proper exit status after pclose

  Revision 1.1  2004/01/04 20:05:38  jonas
    * first working version of the Darwin/Mac OS X (for PowerPC) RTL
      Several non-essential units are still missing, but make cycle works

}
