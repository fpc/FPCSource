{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Compiler call routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPCompile;

interface

uses FPViews;

type
    TCompileMode = (cBuild,cMake,cCompile);

{$ifdef OLDCOMPSTAT}
    TCompPhase = (cpCompiling,cpLinking,cpDone);

    PCompileStatusDialog = ^TCompileStatusDialog;
    TCompileStatusDialog = object(TCenterDialog)
      ST    : PAdvancedStaticText;
      KeyST : PColorStaticText;
      constructor Init;
      procedure   Update;
    end;
{$endif}

const
      PrimaryFile    : string = '';
      IsEXECompiled  : boolean = false;
      MainFile         : string = '';

{$ifdef OLDCOMPSTAT}
      CompilationPhase : TCompPhase = cpDone;
{$endif}
      ProgramInfoWindow : PProgramInfoWindow = nil;


procedure DoCompile(Mode: TCompileMode);


implementation

uses
  Video,
  Objects,Drivers,Views,App,
  CompHook,
  FPConst,FPUtils,FPIntf;

{$ifdef OLDCOMPSTAT}

const SD: PCompileStatusDialog = nil;

constructor TCompileStatusDialog.Init;
var R: TRect;
begin
  R.Assign(0,0,50,11{+7});
  inherited Init(R, 'Compiling');
  GetExtent(R); R.B.Y:=11;
  R.Grow(-3,-2);
  New(ST, Init(R, ''));
  Insert(ST);
  GetExtent(R); R.B.Y:=11;
  R.Grow(-1,-1); R.A.Y:=R.B.Y-1;
  New(KeyST, Init(R, '', Blue*16+White+longint($80+Blue*16+White)*256));
  Insert(KeyST);
{  GetExtent(R); R.Grow(-1,-1); R.A.Y:=10;
  New(MsgLB, Init(R, 1, nil));
  MsgLB^.NewList(New(PUnsortedStringCollection, Init(100,100)));
  Insert(MsgLB);}
end;


procedure TCompileStatusDialog.Update;
var StatusS,KeyS: string;
const CtrlBS = 'Press Ctrl+Break to cancel';
      SuccessS = 'Compile successful: ~Press Enter~';
      FailS = 'Compile failed';
begin
  case CompilationPhase of
    cpCompiling :
      begin
        StatusS:='Compiling '+Status.CurrentSource;
        KeyS:=CtrlBS;
      end;
    cpLinking   :
      begin
        StatusS:='Linking...';
        KeyS:=CtrlBS;
      end;
    cpDone      :
      begin
        StatusS:='Done.';
        if Status.ErrorCount=0 then KeyS:=SuccessS else KeyS:=FailS;
      end;
  end;
  ST^.SetText(
    'Main file: '+MainFile+#13+
    StatusS+#13#13+
    'Target: '+LExpand(KillTilde(GetTargetOSName(GetTargetOS)),12)+'    '+
    'Line number: '+IntToStrL(Status.CurrentLine,7)+#13+
    'Free memory: '+IntToStrL(MemAvail div 1024,6)+'K'+ '    '+
    'Total lines: '+IntToStrL(Status.CompiledLines,7)+#13+
    'Total errors: '+IntToStrL(Status.ErrorCount,5)
  );
  KeyST^.SetText(^C+KeyS);
end;

{$endif}


{****************************************************************************
                               Compiler Hooks
****************************************************************************}

function CompilerStatus: boolean; {$ifndef FPC}far;{$endif}
begin
{$ifdef OLDCOMPSTAT}
  SD^.Update;
{$endif}
  CompilerStatus:=false;
end;


procedure CompilerStop; {$ifndef FPC}far;{$endif}
begin
end;


function CompilerComment(Level:Longint; const s:string):boolean; {$ifndef FPC}far;{$endif}
begin
  ProgramInfoWindow^.AddMessage(Level,S,SmartPath(status.currentmodule),status.currentline);
  CompilerComment:=false;
end;


{****************************************************************************
                                 DoCompile
****************************************************************************}

procedure DoCompile(Mode: TCompileMode);

  function IsExitEvent(E: TEvent): boolean;
  begin
    IsExitEvent:=(E.What=evKeyDown) and
                 ((E.KeyCode=kbEnter) or (E.KeyCode=kbEsc));
  end;


var
  P: PSourceWindow;
  FileName: string;
  E: TEvent;
  WasVisible: boolean;
begin
{ Get FileName }
  P:=Message(Desktop,evBroadcast,cmSearchWindow,nil);
  if (PrimaryFile='') and (P=nil) then
    begin
      ErrorBox('Oooops, nothing to compile.',nil);
      Exit;
    end;
  if PrimaryFile<>'' then
    FileName:=PrimaryFile
  else
    begin
      if P^.Editor^.Modified and (not P^.Editor^.Save) then
       begin
         ErrorBox('Can''t compile unsaved file.',nil);
         Exit;
       end;
      FileName:=P^.Editor^.FileName;
    end;
  MainFile:=SmartPath(FileName);
{ Reset }
  CtrlBreakHit:=false;
{ Show Program Info }
  WasVisible:=ProgramInfoWindow^.GetState(sfVisible);
  ProgramInfoWindow^.LogLB^.Clear;
  if WasVisible=false then
    ProgramInfoWindow^.Show;
  ProgramInfoWindow^.MakeFirst;

{$ifdef OLDCOMPSTAT}
  CompilationPhase:=cpCompiling;
  New(SD, Init);
  Application^.Insert(SD);
  SD^.Update;
{$endif}

  do_status:=CompilerStatus;
  do_stop:=CompilerStop;
  do_comment:=CompilerComment;

  Compile(FileName);

{$ifdef OLDCOMPSTAT}
  CompilationPhase:=cpDone;
  SD^.Update;
{$endif}

  if status.errorcount=0 then
   repeat
     Application^.GetEvent(E);
     if IsExitEvent(E)=false then
      Application^.HandleEvent(E);
   until IsExitEvent(E);

{$ifdef OLDCOMPSTAT}
  Application^.Delete(SD);
  Dispose(SD, Done);
{$endif}

  if (WasVisible=false) and (status.errorcount=0) then
   ProgramInfoWindow^.Hide;
end;

end.
{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.3  1998/12/22 10:39:40  peter
    + options are now written/read
    + find and replace routines

}
