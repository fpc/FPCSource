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

const
  FPE_INTDIV = 1;
  FPE_INTOVF = 2;
  FPE_FLTDIV = 3;
  FPE_FLTOVF = 4;
  FPE_FLTUND = 5;
  FPE_FLTRES = 6;
  FPE_FLTINV = 7;
  FPE_FLTSUB = 8;


procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext);cdecl;
var
  res : word;
  addr : pointer;
begin
  res:=0;
  addr:=nil;
  case sig of
    SIGFPE :
        begin
          addr := siginfo^._sifields._sigfault._addr;
          res := 207;
          case  siginfo^.si_code of
            FPE_INTDIV:
              res:=200;
            FPE_INTOVF:
              res:=205;
            FPE_FLTDIV:
              res:=200;
            FPE_FLTOVF:
              res:=205;
            FPE_FLTUND:
              res:=206;
            FPE_FLTRES,
            FPE_FLTINV,
            FPE_FLTSUB:
              res:=216;
            else
              res:=207;
          end;
        end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        begin
          addr := siginfo^._sifields._sigfault._addr;
          res:=216;
        end;
  end;
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,addr,nil);
end;

{
  $Log$
  Revision 1.1  2005-02-13 22:13:20  peter
    * get solaris back in shape

  Revision 1.8  2005/02/05 23:46:12  peter
    * set addr:=nil for other signals

  Revision 1.7  2005/02/05 23:45:38  peter
    * sigcontext is invalid, use siginfo only

  Revision 1.6  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

  Revision 1.5  2004/11/06 22:48:16  florian
    * fixed errno setting in mt sparc/linux mt programs

  Revision 1.4  2004/08/04 19:27:10  florian
    * fixed floating point and integer exception handling on sparc/linux

  Revision 1.3  2004/05/31 20:25:04  peter
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

