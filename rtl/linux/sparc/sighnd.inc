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


procedure SignalToRunerror(Sig: longint; SigContext: SigContextRec); cdecl;

var
  res : word;
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
    HandleError(res);
//    HandleErrorAddrFrame(res,pointer(SigContext.uc.uc_mcontext.pt_regs^.nip),pointer(SigContext.uc.uc_mcontext.pt_regs^.gpr[1]));
end;

{
  $Log$
  Revision 1.3  2004-05-31 20:25:04  peter
    * removed warnings

  Revision 1.2  2003/11/06 16:28:52  peter
    * compile fix

  Revision 1.1  2003/11/06 16:22:01  peter
    * sparc

  Revision 1.1  2003/11/02 14:53:06  jonas
    + sighand and associated record definitions for ppc. Untested.

  Revision 1.2  2003/11/01 01:58:11  marco
   * more small fixes.

  Revision 1.1  2003/11/01 01:27:20  marco
   * initial version from 1.0.x branch


}

