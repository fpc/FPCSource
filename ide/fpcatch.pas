{
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
    baseunix,
    unix;
  {$endif}
{$endif}
{$ifdef go32v2}
uses
  dpmiexcp;
{$endif}
{$ifdef Windows}
uses
  windows
  {$ifdef HasSignal}
    ,signals
  {$endif}
  ;
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

var
  StopJmp : Jmp_Buf;
const
  StopJmpValid : boolean = false;

{$IFNDEF HASSIGNAL}
const
  SIGABRT   = 288;
  SIGFPE    = 289;
  SIGILL    = 290;
  SIGSEGV   = 291;
  SIGTERM   = 292;
  SIGALRM   = 293;
  SIGHUP    = 294;
  SIGINT    = 295;
  SIGKILL   = 296;
  SIGPIPE   = 297;
  SIGQUIT   = 298;
  SIGUSR1   = 299;
  SIGUSR2   = 300;
  SIGNOFP   = 301;
  SIGTRAP   = 302;
  SIGTIMR   = 303;    { Internal for setitimer (SIGALRM, SIGPROF) }
  SIGPROF   = 304;
  SIGMAX    = 320;

  SIG_BLOCK   = 1;
  SIG_SETMASK = 2;
  SIG_UNBLOCK = 3;
{$ENDIF HASSIGNAL}


Implementation

uses
  keyboard,
  drivers,
  FVConsts,
  dos,app,msgbox,
  FPCompil,FPIDE;

Const
  LastCtrlC : longint = 0;


{$ifdef useresstrings}
resourcestring
{$else}
const
{$endif}
      msg_quitconfirm         = 'Do You really want to quit?';

{$ifdef DEBUG}

procedure Generate_SIGSEGV;
var
  l : plongint;
begin
  { Force a SIGSEGV }
  l:=pointer (ptrint ($ffffffff));
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
{$ifndef SignalIsFunction}
Procedure Catchsignal(Sig : Longint);cdecl;
{$else SignalIsFunction}
  {$ifdef SignalIsCdecl}
  Function Catchsignal(Sig : longint):longint; cdecl;
  {$else not SignalIsCdecl}
  Function Catchsignal(Sig : longint):longint;
  {$endif not SignalIsCdecl}
{$endif SignalIsFunction}
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
{$ifdef SignalIsFunction}
  CatchSignal:=0;
{$endif SignalIsFunction}
end;
{$endif def HasSignal}


Const
  CatchSignalsEnabled : boolean = false;

Procedure EnableCatchSignals;
{$ifdef Windows}
  var Mode: DWORD;
{$endif Windows}
begin
  if CatchSignalsEnabled then
    exit;
{$ifdef Windows}
  if GetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), @Mode) then
    begin
{$ifdef DEBUG}
      Writeln(stderr,'Starting value of ConsoleMode is $',hexstr(Mode,8));
{$endif DEBUG}
      SetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)),
        (Mode or ENABLE_MOUSE_INPUT) and not ENABLE_PROCESSED_INPUT);
{$ifdef DEBUG}
    end
  else
    begin
      Writeln(stderr,'Call to GetConsoleMode failed, GetLastError=',
        GetLastError);
{$endif DEBUG}
    end;
{$endif Windows}
{$ifdef go32v2}
  {
    I think that it was an error to put that here PM
    djgpp_set_ctrl_c(false);
    at least since that this is now handled in fpusrscr.pas unit
  }
{$endif go32v2}
{$ifdef HasSignal}
{$ifndef TP}
  NewSignal:=@CatchSignal;
{$else TP}
  NewSignal:=SignalHandler(CatchSignal);
{$endif TP}
  OldSigSegm:={$ifdef unix}{$ifdef ver1_0}Signal{$else}fpSignal{$endif}{$else}Signal{$endif}(SIGSEGV,NewSignal);
  OldSigInt:={$ifdef unix}{$ifdef ver1_0}Signal{$else}fpSignal{$endif}{$else}Signal{$endif}(SIGINT,NewSignal);
  OldSigFPE:={$ifdef unix}{$ifdef ver1_0}Signal{$else}fpSignal{$endif}{$else}Signal{$endif}(SIGFPE,NewSignal);
  OldSigILL:={$ifdef unix}{$ifdef ver1_0}Signal{$else}fpSignal{$endif}{$else}Signal{$endif}(SIGILL,NewSignal);
  CatchSignalsEnabled:=true;
{$endif}
end;

Procedure DisableCatchSignals;
begin
{$ifdef HasSignal}
  if not CatchSignalsEnabled then
    exit;
  {$ifdef unix}{$ifdef ver1_0}Signal{$else}fpSignal{$endif}{$else}Signal{$endif}(SIGSEGV,OldSigSegm);
  {$ifdef unix}{$ifdef ver1_0}Signal{$else}fpSignal{$endif}{$else}Signal{$endif}(SIGINT,OldSigInt);
  {$ifdef unix}{$ifdef ver1_0}Signal{$else}fpSignal{$endif}{$else}Signal{$endif}(SIGFPE,OldSigFPE);
  {$ifdef unix}{$ifdef ver1_0}Signal{$else}fpSignal{$endif}{$else}Signal{$endif}(SIGILL,OldSigILL);
  CatchSignalsEnabled:=false;
{$endif}
end;

end.
