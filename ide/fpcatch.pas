{
    $Id$
    Copyright (c) 1997-98 by Michael Van Canneyt

    Unit to catch segmentation faults and Ctrl-C and exit gracefully
    under linux and go32v2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit fpcatch;
interface

{$i globdir.inc}

{$ifdef Unix}
uses
  {$ifdef VER1_0}
    linux;
  {$else}
    unix;
  {$endif}
{$endif}
{$ifdef go32v2}
uses
  dpmiexcp;
{$endif}
{$ifdef win32}
uses
  windows, signals;
{$endif}

{$ifdef HasSignal}
Var
  NewSignal,OldSigSegm,OldSigILL,
  OldSigInt,OldSigFPE : SignalHandler;
{$endif}

Const
  CtrlCPressed : Boolean = false;

Procedure EnableCatchSignals;
Procedure DisableCatchSignals;

{$ifdef DEBUG}
procedure Generate_SIGSEGV;
procedure Generate_SIGFPE;
{$endif DEBUG}

{$ifndef GABOR}
var
  StopJmp : Jmp_Buf;
const
  StopJmpValid : boolean = false;
{$endif}

Implementation

uses
{$ifdef FPC}
  keyboard,
  drivers,
{$endif FPC}
{$ifdef FVISION}
  FVConsts,
{$else}
  Commands,
{$endif}
  dos,app,msgbox,
  FPString,FPCompil,FPIDE;

Const
  LastCtrlC : longint = 0;

{$ifdef DEBUG}

procedure Generate_SIGSEGV;
var
  l : plongint;
begin
  { Force a SIGSEGV }
  l:=$ffffffff;
  l^:=1;
end;

procedure Generate_SIGFPE;
var
  x,y : real;
begin
  { Force a SIGFPE }
  y:=-5;
  x:=sqrt(y);
end;

{$endif DEBUG}

{$ifdef HasSignal}
{$ifdef Unix}
Procedure CatchSignal(Sig : Longint);cdecl;
{$else}
Function CatchSignal(Sig : longint):longint;
{$endif}
var MustQuit: boolean;
begin
  case Sig of
   SIGSEGV : begin
               if StopJmpValid then
                 LongJmp(StopJmp,SIGSEGV);
               if Assigned(Application) then IDEApp.Done;
               Writeln('Internal SIGSEGV Error caught');
{$ifndef DEBUG}
               Halt;
{$else DEBUG}
               RunError(216);
{$endif DEBUG}
             end;
    SIGFPE : begin
                if StopJmpValid then
                  LongJmp(StopJmp,SIGFPE);
               if Assigned(Application) then IDEApp.Done;
               Writeln('Internal SIGFPE Error caught');
{$ifndef DEBUG}
               Halt;
{$else DEBUG}
               RunError(207);
{$endif DEBUG}
             end;
    SIGILL : begin
                if StopJmpValid then
                  LongJmp(StopJmp,SIGILL);
               if Assigned(Application) then IDEApp.Done;
               Writeln('Internal SIGILL Error caught');
{$ifndef DEBUG}
               Halt;
{$else DEBUG}
               RunError(216);
{$endif DEBUG}
             end;
    SIGINT : begin
               if StopJmpValid then
                 LongJmp(StopJmp,SIGINT);
               IF NOT CtrlCPressed and Assigned(Application) then
                 begin
                   MustQuit:=false;
{$ifdef FPC}
                   if GetDosTicks>LastCtrlC+10 then
                     begin
                       CtrlCPressed:=true;
                       Keyboard.PutKeyEvent((kbCtrl shl 16) or kbCtrlC);
                       LastCtrlC:=GetDosTicks;
                     end;
{$endif FPC}
                 end
               else
                 begin
                   if Assigned(Application) then
                     MustQuit:=MessageBox(#3+msg_QuitConfirm,nil,mferror+mfyesbutton+mfnobutton)=cmYes
                   else
                     MustQuit:=true;
                 end;
               if MustQuit then
                begin
                  if Assigned(Application) then IDEApp.Done;
{$ifndef DEBUG}
                  Halt;
{$else DEBUG}
                  RunError(216);
{$endif DEBUG}
                end;
             end;
  end;
{$ifndef Unix}
  CatchSignal:=0;
{$endif}
end;
{$endif def HasSignal}


Const
  CatchSignalsEnabled : boolean = false;

Procedure EnableCatchSignals;
{$ifdef win32}
  var Mode: DWORD;
{$endif win32}
begin
  if CatchSignalsEnabled then
    exit;
{$ifdef win32}
  if GetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), @Mode) then
    SetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), (Mode or ENABLE_MOUSE_INPUT) and not ENABLE_PROCESSED_INPUT);
{$endif win32}
{$ifdef go32v2}
  djgpp_set_ctrl_c(false);
{$endif go32v2}
{$ifdef HasSignal}
{$ifndef TP}
  NewSignal:=SignalHandler(@CatchSignal);
{$else TP}
  NewSignal:=SignalHandler(CatchSignal);
{$endif TP}
  OldSigSegm:=Signal (SIGSEGV,NewSignal);
  OldSigInt:=Signal (SIGINT,NewSignal);
  OldSigFPE:=Signal (SIGFPE,NewSignal);
  OldSigILL:=Signal (SIGILL,NewSignal);
  CatchSignalsEnabled:=true;
{$endif}
end;

Procedure DisableCatchSignals;
begin
{$ifdef HasSignal}
  if not CatchSignalsEnabled then
    exit;
  Signal (SIGSEGV,OldSigSegm);
  Signal (SIGINT,OldSigInt);
  Signal (SIGFPE,OldSigFPE);
  Signal (SIGILL,OldSigILL);
  CatchSignalsEnabled:=false;
{$endif}
end;

end.

{
  $Log$
  Revision 1.7  2003-04-23 09:49:26  peter
    * unix signal handler needs longint

  Revision 1.6  2002/09/07 21:04:41  carl
    * fix range check errors for version 1.1 compilation

  Revision 1.5  2002/09/07 15:40:42  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/03/20 14:48:27  pierre
   * moved StopJmp buffer to fpcatch unit

}
