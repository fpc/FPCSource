{
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
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,addr,nil);
end;

{
  $Log: sighnd.inc,v $
  Revision 1.10  2005/04/24 21:19:22  peter
    * unblock signal in signalhandler, remove the sigprocmask call
      from setjmp

  Revision 1.9  2005/02/14 17:13:30  peter
    * truncate log

  Revision 1.8  2005/02/05 23:46:12  peter
    * set addr:=nil for other signals

  Revision 1.7  2005/02/05 23:45:38  peter
    * sigcontext is invalid, use siginfo only

  Revision 1.6  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

}

