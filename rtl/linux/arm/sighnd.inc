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


procedure SignalToRunerror(Sig: longint; _a2,_a3,_a4 : dword; SigContext: PSigInfo; uContext : PuContext); cdecl;

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
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,pointer(uContext^.uc_mcontext.arm_ip),pointer(uContext^.uc_mcontext.arm_fp));
end;

{
  $Log: sighnd.inc,v $
  Revision 1.5  2005/04/24 21:19:22  peter
    * unblock signal in signalhandler, remove the sigprocmask call
      from setjmp

  Revision 1.4  2005/02/14 17:13:30  peter
    * truncate log

  Revision 1.3  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

}

