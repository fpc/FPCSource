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
  FPU_All = $7f;

function GetFPUState(const SigContext : SigContextRec) : word;
  begin
    if assigned(SigContext.fpstate) then
      GetfpuState:=SigContext.fpstate^.swd;
  {$ifdef SYSTEM_DEBUG}
    writeln('xx:',sigcontext.en_tw,' ',sigcontext.en_cw);
  {$endif SYSTEM_DEBUG}
  {$ifdef SYSTEM_DEBUG}
    Writeln(stderr,'FpuState = ',result);
  {$endif SYSTEM_DEBUG}
  end;

function  reenable_signal(sig : longint) : boolean;
var
  e,olde : TSigSet;
  i,j : byte;
begin
  fillchar(e,sizeof(e),#0);
  { set is 1 based PM }
  dec(sig);
  i:=sig mod 32;
  j:=sig div 32;
  e[j]:=1 shl i;
  fpsigprocmask(SIG_UNBLOCK,@e,nil);
  reenable_signal:=geterrno=0;
end;

procedure SignalToRunerror(sig : longint; SigContext: SigContextRec); cdecl;
  var
    res,fpustate : word;
  begin
    res:=0;
    case sig of
      SIGFPE :
        begin
          { this is not allways necessary but I don't know yet
            how to tell if it is or not PM }
          res:=200;
          fpustate:=GetFPUState(SigContext);
          if (FpuState and FPU_All) <> 0 then
            begin
              { first check the more precise options }
              if (FpuState and FPU_DivisionByZero)<>0 then
                res:=200
              else if (FpuState and FPU_Overflow)<>0 then
                res:=205
              else if (FpuState and FPU_Underflow)<>0 then
                res:=206
              else if (FpuState and FPU_Denormal)<>0 then
                res:=216
              else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow))<>0 Then
                res:=207
              else if (FpuState and FPU_Invalid)<>0 then
                res:=216
              else
                res:=207;  {'Coprocessor Error'}
            end;
            SysResetFPU;
        end;
      SIGILL,
      SIGBUS,
      SIGSEGV:
        res:=216;
    end;
    reenable_signal(sig);
    if res<>0 then
      HandleErrorAddrFrame(res,pointer(SigContext.rip),pointer(SigContext.rbp));
  end;

{
  $Log$
  Revision 1.2  2004-05-01 15:59:17  florian
    * x86_64 exception handling fixed

  Revision 1.1  2004/02/05 01:16:12  florian
    + completed x86-64/linux system unit

  Revision 1.2  2003/11/01 01:58:11  marco
   * more small fixes.

  Revision 1.1  2003/11/01 01:27:20  marco
   * initial version from 1.0.x branch
}

