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
{$ifndef NODEBUG}
  if not assigned(Debugger) then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      exit;
   end;
  if not Debugger^.IsRunning then
    Debugger^.StartTrace
  else
    Debugger^.TraceNext;
  {While (Debugger^.InvalidSourceLine and
         Debugger^.IsRunning and
         not Debugger^.error) do
    begin
      Inc(Debugger^.HiddenStepsCount);
      Debugger^.TraceNext;
    end;}
  Debugger^.AnnotateError;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;


procedure TIDEApp.DoTraceInto;
begin
{$ifndef NODEBUG}
  if not assigned(Debugger) then
   begin
     InitDebugger;
     if not assigned(Debugger) then
       exit;
   end;
  if not debugger^.IsRunning then
    Debugger^.StartTrace
  else
   Debugger^.TraceStep;
  { I think we should not try to go deeper !
    if the source is not found PM }
  While (Debugger^.InvalidSourceLine and
         Debugger^.IsRunning and
         not Debugger^.error) do
    begin
      Inc(Debugger^.HiddenStepsCount);
      Debugger^.TraceNext;
    end;
  Debugger^.AnnotateError;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoContUntilReturn;
begin
{$ifndef NODEBUG}
  if not assigned(Debugger) then
   begin
     InitDebugger;
     if not assigned(Debugger) then
       exit;
   end;
  if not debugger^.IsRunning then
    Debugger^.Run
  else
   Debugger^.UntilReturn;
  Debugger^.AnnotateError;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoRun;
var
  RunDirect : boolean;
begin

{$ifndef NODEBUG}
  if not assigned(Debugger) or not Debugger^.IsRunning then
{$endif}
    begin
      if (not ExistsFile(ExeFile)) or (CompilationPhase<>cpDone) or
         NeedRecompile(false) then
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
      RunDirect:=true;
{$ifndef NODEBUG}
      { we use debugger if and only if there are active breakpoints
        AND the target is correct for debugging !! PM }
      if ActiveBreakpoints and (target_os.shortname=source_os.shortname) then
        begin
          if not assigned(Debugger) then
            InitDebugger;
          if assigned(Debugger) then
            begin
              Debugger^.Run;
              RunDirect:=false;
            end;
        end;
{$endif ndef NODEBUG}
      if Not RunDirect then
        exit;
      DoExecute(ExeFile,GetRunParameters,'','',exNormal);
      LastExitCode:=ExecuteResult;
      If IOStatus<>0 then
        begin
          MsgParms[1].Ptr:=@EXEFile;
          MsgParms[2].long:=IOStatus;
          InformationBox(#3'Program %s'#13#3'not run'#13#3'DosError = %d',@MsgParms);
        end
      else If LastExitCode<>0 then
        begin
          MsgParms[1].Ptr:=@EXEFile;
          MsgParms[2].long:=LastExitCode;
          InformationBox(#3'Program %s'#13#3'exited with '#13#3'exitcode = %d',@MsgParms);
        end;
    end
{$ifndef NODEBUG}
  else
    Debugger^.Continue
{$endif}
  ;
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
{$ifndef NODEBUG}
  if assigned(Debugger) then
     DoneDebugger;
  UpdateScreen(true);
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoContToCursor;
{$ifndef NODEBUG}
var
  W : PSourceWindow;
  FileName : string;
  LineNr : longint;
{$endif}
begin
{$ifndef NODEBUG}
  if (DeskTop^.Current=nil) or
     (TypeOf(DeskTop^.Current^)<>TypeOf(TSourceWindow)) then
    Begin
       ErrorBox('Impossible to reach current cursor',nil);
       Exit;
    End;

  W:=PSourceWindow(DeskTop^.Current);
  If assigned(W) then
    begin
      FileName:=W^.Editor^.FileName;
      LineNr:=W^.Editor^.CurPos.Y+1;
      If not assigned(Debugger) then
        begin
          InitDebugger;
          if not assigned(Debugger) then
            exit;
        end;
      Debugger^.Command('tbreak '+GDBFileName(NameAndExtOf(FileName))+':'+IntToStr(LineNr));
      Debugger^.Continue;
    end;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoOpenGDBWindow;
begin
{$ifndef NODEBUG}
  InitGDBWindow;
  If assigned(GDBWindow) then
    GDBWindow^.MakeFirst;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoToggleBreak;
{$ifndef NODEBUG}
var
  W : PSourceWindow;
  FileName : string;
  b : boolean;
  LineNr : longint;
{$endif}
begin
{$ifndef NODEBUG}
  if (DeskTop^.Current=nil) or
     (TypeOf(DeskTop^.Current^)<>TypeOf(TSourceWindow)) then
    Begin
       ErrorBox('Impossible to set breakpoints here',nil);
       Exit;
    End;

  W:=PSourceWindow(DeskTop^.Current);
  If assigned(W) then
    begin
      FileName:=W^.Editor^.FileName;
      LineNr:=W^.Editor^.CurPos.Y+1;
      b:=BreakpointsCollection^.ToggleFileLine(FileName,LineNr);
      {W^.Editor^.SetLineBreakState(LineNr,b);
        already done PM }
    end;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

{
  $Log$
  Revision 1.30  2000-03-21 23:27:35  pierre
   Gabor fixes to avoid unused vars

  Revision 1.29  2000/03/08 16:58:12  pierre
   Uses Debugger^.IsRunning

  Revision 1.28  2000/02/04 00:15:31  pierre
   * adapted to change in fpdebug unit

  Revision 1.27  2000/02/02 22:48:25  pierre
    * Use desktop^.current instead of desktop^.first
    * Avoid stopping at main if debugger started with CrtlF9

  Revision 1.26  2000/01/31 16:02:08  pierre
   * avoid the Impossible to set breakpoints here

  Revision 1.25  2000/01/28 22:38:21  pierre
   * CrtlF9 starts debugger if there are active breakpoints

  Revision 1.24  2000/01/10 13:23:28  pierre
   * Debugger test missing in ContTo

  Revision 1.23  1999/12/10 13:01:01  pierre
   * do not recompile a program inside Debugging session

  Revision 1.22  1999/11/10 17:17:02  pierre
   + Information box if exit code <> 0 or DosError

  Revision 1.21  1999/09/16 14:34:59  pierre
    + TBreakpoint and TWatch registering
    + WatchesCollection and BreakpointsCollection stored in desk file
    * Syntax highlighting was broken

  Revision 1.20  1999/09/09 14:11:56  pierre
   * GDB needs lowercase filenames for tbreak

  Revision 1.19  1999/08/31 16:20:37  pierre
   * DoContUntilReturn call Step instead of UnitlReturn

  Revision 1.18  1999/08/16 18:25:23  peter
    * Adjusting the selection when the editor didn't contain any line.
    * Reserved word recognition redesigned, but this didn't affect the overall
      syntax highlight speed remarkably (at least not on my Amd-K6/350).
      The syntax scanner loop is a bit slow but the main problem is the
      recognition of special symbols. Switching off symbol processing boosts
      the performance up to ca. 200%...
    * The editor didn't allow copying (for ex to clipboard) of a single character
    * 'File|Save as' caused permanently run-time error 3. Not any more now...
    * Compiler Messages window (actually the whole desktop) did not act on any
      keypress when compilation failed and thus the window remained visible
    + Message windows are now closed upon pressing Esc
    + At 'Run' the IDE checks whether any sources are modified, and recompiles
      only when neccessary
    + BlockRead and BlockWrite (Ctrl+K+R/W) implemented in TCodeEditor
    + LineSelect (Ctrl+K+L) implemented
    * The IDE had problems closing help windows before saving the desktop

  Revision 1.17  1999/07/28 23:11:20  peter
    * fixes from gabor

  Revision 1.16  1999/07/12 13:14:19  pierre
    * LineEnd bug corrected, now goes end of text even if selected
    + Until Return for debugger
    + Code for Quit inside GDB Window

  Revision 1.15  1999/04/07 21:55:50  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.14  1999/03/01 15:41:58  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.13  1999/02/18 13:44:33  peter
    * search fixed
    + backward search
    * help fixes
    * browser updates

  Revision 1.12  1999/02/11 19:07:24  pierre
    * GDBWindow redesigned :
      normal editor apart from
      that any kbEnter will send the line (for begin to cursor)
      to GDB command !
      GDBWindow opened in Debugger Menu
       still buggy :
       -echo should not be present if at end of text
       -GDBWindow becomes First after each step (I don't know why !)

  Revision 1.11  1999/02/10 09:51:59  pierre
   small cleanup

  Revision 1.10  1999/02/08 17:43:45  pierre
    * RestDebugger or multiple running of debugged program now works
    + added DoContToCursor(F4)
    * Breakpoints are now inserted correctly (was mainlyy a problem
      of directories)

  Revision 1.9  1999/02/05 17:21:53  pierre
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