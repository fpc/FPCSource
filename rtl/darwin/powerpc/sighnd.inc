{  
   $Id$
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Signalhandler for FreeBSD/i386

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

procedure SignalToRunerror(Sig: cint; var info : tsiginfo_t;Var SigContext:SigContextRec); cdecl;

var
  res : word;

begin
  res:=0;
  { have to verify whether this is necessary }
  fpc_enable_ppc_fpu_exceptions;
  case sig of
    SIGFPE :
      begin
        Case Info.si_code Of
          FPE_INTDIV : Res:=200;  {integer divide fault. Div0?}
          FPE_FLTOVF : Res:=205;  {Overflow trap}
          FPE_FLTUND : Res:=206;  {Stack over/underflow}
          FPE_FLTRES : Res:=208;  {Device not available}
          FPE_FLTINV : Res:=207;  {Invalid floating point operation}
          Else
              Res:=208; {coprocessor error}
        end;
      end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        res:=216;
  end;
  {$ifdef FPC_USE_SIGPROCMASK}
   reenable_signal(sig);
  {$endif }
{ give runtime error at the position where the signal was raised }
  if res<>0 then
   begin
     HandleErrorAddrFrame(res,Pointer(SigContext.sc_ir),pointer(SigContext.sc_sp));
   end;
end;

{
  $Log$
  Revision 1.1  2004-01-04 20:05:38  jonas
    * first working version of the Darwin/Mac OS X (for PowerPC) RTL
      Several non-essential units are still missing, but make cycle works

  Revision 1.1  2004/01/03 12:29:36  marco
   * now separately.

}
