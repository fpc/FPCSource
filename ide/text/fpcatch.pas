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

{$ifdef linux}
{$define has_signal}
uses
  linux;
{$endif}
{$ifdef go32v2}
{$define has_signal}
uses
  dpmiexcp;
{$endif}

{$ifdef has_signal}
Var
  NewSignal,OldSigSegm,OldSigInt : SignalHandler;
{$endif}


Implementation

uses
  commands,msgbox,
  fpide,fpviews;


{$ifdef has_signal}
{$ifdef linux}
Procedure CatchSignal(Sig : Integer);cdecl;
{$else}
Function CatchSignal(Sig : longint):longint;
{$endif}
begin
  case Sig of
   SIGSEGV : begin
               MyApp.Done;
               Writeln('Internal Error caught');
               Halt;
             end;
    SIGINT : begin
               if MessageBox(#3'Do You really want to quit?',nil,mferror+mfyesbutton+mfnobutton)=cmYes then
                begin
                  MyApp.Done;
                  Halt;
                end;
             end;
  end;
{$ifndef linux}
  CatchSignal:=0;
{$endif}
end;
{$endif def has_signal}


begin
{$ifdef has_signal}
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
  Revision 1.1  1999-02-20 15:18:28  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

}
