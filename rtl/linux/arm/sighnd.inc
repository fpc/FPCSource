{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    Signal handler is arch dependant due to processor to language
    exception conversion.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


procedure SignalToRunerror(Sig: longint; _a2,_a3,_a4 : dword; SigContext: PSigInfoRec; uContext : PuContext); cdecl;

var
  res,fpustate : word;
begin
  res:=0;
  case sig of
    SIGFPE :
        begin
          { don't know how to find the different causes, maybe via xer? }
          res := 207;
        end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        res:=216;
  end;
{ give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,pointer(uContext^.uc_mcontext.arm_ip),pointer(uContext^.uc_mcontext.arm_fp));
end;

{
  $Log$
  Revision 1.2  2004-03-27 19:20:11  florian
    * signal handling fixed

  Revision 1.1  2003/11/21 00:40:06  florian
    * some arm issues fixed
}

