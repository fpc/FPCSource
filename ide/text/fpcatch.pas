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

{$ifdef linux}
uses
  linux;
{$endif}
{$ifdef go32v2}
uses
  dpmiexcp;
{$endif}

{$ifdef HasSignal}
Var
  NewSignal,OldSigSegm,OldSigInt : SignalHandler;
{$endif}

Const
  CtrlCPressed : Boolean = false;

Implementation

uses
{$ifdef FPC}
  keyboard,
  drivers,
{$endif FPC}
  app,commands,msgbox,
  fpide,fpviews;


{$ifdef HasSignal}
{$ifdef linux}
Procedure CatchSignal(Sig : Integer);cdecl;
{$else}
Function CatchSignal(Sig : longint):longint;
{$endif}
var MustQuit: boolean;
begin
  case Sig of
   SIGSEGV : begin
               if Assigned(Application) then IDEApp.Done;
               Writeln('Internal Error caught');
{$ifndef DEBUG}
               Halt;
{$else DEBUG}
               RunError(216);
{$endif DEBUG}
             end;
    SIGINT : begin
               IF NOT CtrlCPressed and Assigned(Application) then
                 begin
                   MustQuit:=false;
{$ifdef FPC}
                   CtrlCPressed:=true;
                   Keyboard.PutKeyEvent((kbCtrl shl 16) or kbCtrlC);
{$endif FPC}
                 end
               else
                 begin
                   if Assigned(Application) then
                     MustQuit:=MessageBox(#3'Do You really want to quit?',nil,mferror+mfyesbutton+mfnobutton)=cmYes
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
{$ifndef linux}
  CatchSignal:=0;
{$endif}
end;
{$endif def HasSignal}


begin
{$ifdef HasSignal}
{$ifndef TP}
  NewSignal:=SignalHandler(@CatchSignal);
{$else TP}
  NewSignal:=SignalHandler(CatchSignal);
{$endif TP}
  OldSigSegm:=Signal (SIGSEGV,NewSignal);
  OldSigInt:=Signal (SIGINT,NewSignal);
{$endif}
end.

{
  $Log$
  Revision 1.4  2000-03-07 21:09:20  pierre
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