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
Procedure CatchSignal(Sig : Integer);cdecl;
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
  if GetConsoleMode(GetStdHandle(Std_Input_Handle), @Mode) then
    SetConsoleMode(GetStdHandle(Std_Input_Handle), (Mode or ENABLE_MOUSE_INPUT) and not ENABLE_PROCESSED_INPUT);
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
  Revision 1.4  2002-03-20 14:48:27  pierre
   * moved StopJmp buffer to fpcatch unit

  Revision 1.3  2001/10/24 14:17:27  pierre
   * try to fix the Win2000 mouse problem

  Revision 1.2  2001/08/05 02:01:47  peter
    * FVISION define to compile with fvision units

  Revision 1.1  2001/08/04 11:30:22  peter
    * ide works now with both compiler versions

  Revision 1.1.2.4  2000/11/30 13:04:01  pierre
   * fix for bug 1205

  Revision 1.1.2.3  2000/11/29 00:54:44  pierre
   + preserve window number and save special windows

  Revision 1.1.2.2  2000/11/14 09:23:55  marco
   * Second batch

  Revision 1.1.2.1  2000/10/31 07:52:55  pierre
   * recover gracefully if compiler generates a signal

  Revision 1.1  2000/07/13 09:48:34  michael
  + Initial import

  Revision 1.6  2000/06/22 09:07:11  pierre
   * Gabor changes: see fixes.txt

  Revision 1.5  2000/05/02 08:42:26  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.4  2000/03/07 21:09:20  pierre
    * Use globdir.inc HasSignal conditional
    + Uses PutKeyEvent for CtrlC

  Revision 1.3  1999/12/20 14:23:16  pierre
    * MyApp renamed IDEApp
    * TDebugController.ResetDebuggerRows added to
      get resetting of debugger rows

  Revision 1.2  1999/04/07 21:55:42  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.1  1999/02/20 15:18:28  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

}
