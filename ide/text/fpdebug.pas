{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Debugger call routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPDebug;
interface

uses
  GDBCon;

type
  PDebugController=^TDebugController;
  TDebugController=object(TGDBController)
    constructor Init(const exefn:string);
    destructor  Done;
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
{    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;
    procedure DoEndSession(code:longint);virtual; }
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
  end;

var
  Debugger : PDebugController;

procedure InitDebugger;
procedure DoneDebugger;


implementation

uses
  Dos,Mouse,Video,
  App,
  FPViews,FPVars,FPUtils,FPIntf,
  FPCompile,FPUsrScr;


{****************************************************************************
                            TDebugController
****************************************************************************}

constructor TDebugController.Init(const exefn:string);
  var f: string;
begin
  inherited Init;
  f := exefn;
  LoadFile(f);
end;


destructor TDebugController.Done;
begin
  inherited Done;
end;


procedure TDebugController.DoSelectSourceLine(const fn:string;line:longint);
var
  W: PSourceWindow;
begin
  Desktop^.Lock;
  if Line>0 then
   dec(Line);
  W:=TryToOpenFile(nil,fn,0,Line);
  if assigned(W) then
   begin
     W^.Editor^.SetHighlightRow(Line);
     W^.Select;
   end;  
  Desktop^.UnLock;
end;


procedure TDebugController.DoDebuggerScreen;
begin
  if assigned(FPUsrScr.UserScreen) then
    FPUsrScr.UserScreen^.SwitchBack;
end;


procedure TDebugController.DoUserScreen;
begin
  if assigned(FPUsrScr.UserScreen) then
    FPUsrScr.UserScreen^.SwitchTo;
end;


{****************************************************************************
                                 Initialize
****************************************************************************}

procedure InitDebugger;
begin
  if (not ExistsFile(ExeFile)) or (CompilationPhase<>cpDone) then
    DoCompile(cRun);
  if CompilationPhase<>cpDone then
    Exit;
  if (EXEFile='') then
   begin
     ErrorBox('Oooops, nothing to debug.',nil);
     Exit;
   end;
{ init debugcontroller }
  if assigned(Debugger) then
   dispose(Debugger,Done);
  new(Debugger,Init(ExeFile));
end;


procedure DoneDebugger;
begin
  if assigned(Debugger) then
   dispose(Debugger,Done);
end;


end.

{
  $Log$
  Revision 1.2  1999-01-22 18:14:09  pierre
   * adaptd to changes in gdbint and gdbcon for  to /

  Revision 1.1  1999/01/22 10:24:03  peter
    * first debugger things

}
