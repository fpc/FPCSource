{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Run menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


procedure TIDEApp.DoStepOver;
begin
  if not assigned(Debugger) then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      exit;
     Debugger^.StartTrace;
   end
  else
   Debugger^.TraceNext;
  While (Debugger^.InvalidSourceLine and
    Debugger^.Debugger_started and
    not Debugger^.error) do
   Debugger^.TraceNext;
  Debugger^.AnnotateError;
end;


procedure TIDEApp.DoTraceInto;
begin
  if not assigned(Debugger) then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      exit;
     Debugger^.StartTrace;
   end
  else
   Debugger^.TraceStep;
  { I think we should not try to go deeper !
    if the source is not found PM }
  While (Debugger^.InvalidSourceLine and
        Debugger^.Debugger_started and
        not Debugger^.error) do
   Debugger^.TraceStep;
  Debugger^.AnnotateError;
end;


procedure TIDEApp.DoRun;
begin
  if (not ExistsFile(ExeFile)) or (CompilationPhase<>cpDone) then
   DoCompile(cRun);
  if CompilationPhase<>cpDone then
     Exit;
  if (EXEFile='') then
   begin
     ErrorBox('Oooops, nothing to run.',nil);
     Exit;
   end;
  if not ExistsFile(ExeFile) then
    begin
      MsgParms[1].Ptr:=@EXEFile;
      ErrorBox('Invalid filename %s',@MsgParms);
      Exit;
    end;

  if not assigned(Debugger) then
    begin
      DoExecute(ExeFile,GetRunParameters,false);
      LastExitCode:=DosExitCode;
    end
  else
    Debugger^.Continue;
end;


procedure TIDEApp.Parameters;
var R,R2: TRect;
    D: PCenterDialog;
    IL: PInputLine;
begin
  R.Assign(0,0,54,4);
  New(D, Init(R, 'Program parameters'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-1); Inc(R.A.Y); R.B.Y:=R.A.Y+1;
    R2.Copy(R); R2.A.X:=14;
    New(IL, Init(R2, 255));
    IL^.Data^:=GetRunParameters;
    Insert(IL);
    R2.Copy(R); R2.B.X:=14;
    Insert(New(PLabel, Init(R2, '~P~arameter', IL)));
  end;
  InsertButtons(D);
  IL^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    SetRunParameters(IL^.Data^);
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.DoResetDebugger;
begin
  if assigned(Debugger) then
     DoneDebugger;
end;

procedure TIDEApp.DoToggleBreak;
var
  W : PSourceWindow;
  FileName : string;
  b : boolean;
  LineNr : longint;
  Info : record
          F : pstring;
          L : longint;
         end;
begin
  if not assigned(DeskTop^.First) or
     (TypeOf(DeskTop^.First^)<>TypeOf(TSourceWindow)) then
    Begin
       ErrorBox('Impossible to set breakpoints here',nil);
       Exit;
    End;
    
  W:=PSourceWindow(DeskTop^.First);
  If assigned(W) then
    begin
      FileName:=W^.Editor^.FileName;
      LineNr:=W^.Editor^.CurPos.Y+1;
      Info.F:=@FileName;
      Info.L:=LineNr;
      {InformationBox(#3'Trying to set a breakpoint at'#13#3+
         '%s : %d',@Info);}
      b:=BreakpointCollection^.ToggleFileLine(FileName,LineNr);
      W^.Editor^.SetLineBreakState(LineNr-1,b);
    end;
end;

{
  $Log$
  Revision 1.9  1999-02-05 17:21:53  pierre
    Invalid_line renamed InvalidSourceLine

  Revision 1.8  1999/02/04 17:56:17  pierre
   * Set breakpoint only possible if source window

  Revision 1.7  1999/02/04 13:32:07  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.6  1999/01/22 10:24:05  peter
    * first debugger things

  Revision 1.5  1999/01/21 11:54:20  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.4  1999/01/14 21:42:22  peter
    * source tracking from Gabor

  Revision 1.3  1999/01/12 14:29:36  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.2  1999/01/04 11:49:48  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.1  1998/12/28 15:47:50  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
