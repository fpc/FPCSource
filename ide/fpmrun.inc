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

{$define MODIFIEDINDEBUG}

function TIDEApp.AskRecompileIfModified:boolean;
var
  PW,PPW : PSourceWindow;
  Checkmodifiededitor : boolean;
begin
  AskRecompileIfModified:=false;
{$ifdef MODIFIEDINDEBUG}
  if not AskRecompileIfModifiedFlag then
   exit;
  Checkmodifiededitor:=false;
  PW:=FirstEditorWindow;
  PPW:=PW;
  while assigned(PW) do
   begin
     If PW^.HelpCtx=hcSourceWindow then
      begin
        if pw^.editor^.getmodified then
         if pw^.editor^.core^.getmodifytime > LastCompileTime then
           begin
             Checkmodifiededitor:=true;
             break;
           end;
      end;
     PW:=PSourceWindow(PW^.next);
     While assigned(PW) and (PW<>PPW) and (PW^.HelpCtx<>hcSourceWindow) do
      PW:=PSourceWindow(PW^.next);
     If PW=PPW then
      break;
   end;
  if Checkmodifiededitor then
   begin
     if (MessageBox(#3+'You''ve edited a file, recompile the project?',nil,mfinformation+mfyesbutton+mfnobutton)=cmYes) then
      begin
        DoResetDebugger;
        DoRun;
        AskRecompileIfModified:=true;
      end
     else
      AskRecompileIfModifiedFlag:=false;
   end;
{$endif MODIFIEDINDEBUG}
end;


procedure TIDEApp.DoStepOver;
begin
{$ifndef NODEBUG}
  if not assigned(Debugger) or Not Debugger^.HasExe then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      begin
        NoDebugger;
        exit;
      end;
   end;
  if not Debugger^.IsRunning then
    Debugger^.StartTrace
  else
   begin
     if AskRecompileIfModified then
      exit;
     if InDisassemblyWindow then
       Debugger^.TraceNextI
     else
       Debugger^.TraceNext;
   end;
  {While (Debugger^.InvalidSourceLine and
         Debugger^.IsRunning and
         not Debugger^.error) do
    begin
      Inc(Debugger^.HiddenStepsCount);
     if InDisassemblyWindow then
       Debugger^.TraceNextI
     else
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
  if not assigned(Debugger) or Not Debugger^.HasExe then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      begin
        NoDebugger;
        exit;
      end;
   end;
  if not debugger^.IsRunning then
    Debugger^.StartTrace
  else
   begin
     if AskRecompileIfModified then
      exit;
     if InDisassemblyWindow then
       Debugger^.TraceStepI
     else
       Debugger^.TraceStep;
   end;
  { I think we should not try to go deeper !
    if the source is not found PM }
  While (Debugger^.InvalidSourceLine and
         Debugger^.IsRunning and
         not Debugger^.error) do
    begin
      Inc(Debugger^.HiddenStepsCount);
      if InDisassemblyWindow then
        Debugger^.TraceNextI
      else
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
  if not assigned(Debugger) or Not Debugger^.HasExe then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      begin
        NoDebugger;
        exit;
      end;
   end;
  if not debugger^.IsRunning then
    Debugger^.Run
  else
   begin
     if AskRecompileIfModified then
      exit;
     Debugger^.UntilReturn;
   end;
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
  if not assigned(Debugger) or not Debugger^.HasExe or not Debugger^.IsRunning then
{$endif}
    begin
      if (not ExistsFile(ExeFile)) or (CompilationPhase<>cpDone) or
         NeedRecompile(cRun,false) then
        begin
          DoCompile(cRun);
          if CompilationPhase<>cpDone then
            Exit;
          if not Status.IsExe then
            begin
              ErrorBox(msg_cannotrununit,nil);
              Exit;
            end;
          if IsLibrary then
            begin
              ErrorBox(msg_cannotrunlibrary,nil);
              Exit;
            end;
        end;
      if (EXEFile='') then
       begin
         ErrorBox(msg_nothingtorun,nil);
         Exit;
       end;
      if not ExistsFile(ExeFile) then
        begin
          MsgParms[1].Ptr:=@EXEFile;
          ErrorBox(msg_invalidfilename,@MsgParms);
          Exit;
        end;
      RunDirect:=true;
{$ifndef NODEBUG}
      { we use debugger if and only if there are active breakpoints
        AND the target is correct for debugging !! PM }
      if ActiveBreakpoints and
{$ifdef COMPILER_1_0}
        (target_os.shortname=source_os.shortname)
{$else COMPILER_1_0}
        (target_info.shortname=source_info.shortname)
{$endif COMPILER_1_0}
        then
        begin
          if not assigned(Debugger) or Not Debugger^.HasExe then
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
{$ifdef Unix}
      if (DebuggeeTTY<>'') then
        DoExecute(ExeFile,GetRunParameters,DebuggeeTTY,DebuggeeTTY,DebuggeeTTY,exNormal)
      else
{$endif Unix}
      DoExecute(ExeFile,GetRunParameters,'','','',exNormal);
      { In case we have something that the compiler touched }
      AskToReloadAllModifiedFiles;
      LastExitCode:=ExecuteResult;
      If IOStatus<>0 then
        begin
          MsgParms[1].Ptr:=@EXEFile;
          MsgParms[2].long:=IOStatus;
          InformationBox(msg_programnotrundoserroris,@MsgParms);
        end
      else If LastExitCode<>0 then
        begin
          MsgParms[1].Ptr:=@EXEFile;
          MsgParms[2].long:=LastExitCode;
          InformationBox(msg_programfileexitedwithexitcode,@MsgParms);
        end;
    end
{$ifndef NODEBUG}
  else
    Debugger^.Continue
{$endif}
  ;
end;

procedure TIDEApp.UpdateRunMenu(DebuggeeRunning : boolean);
var MenuItem : PMenuItem;
begin
  MenuItem:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cmRun);
  if assigned(MenuItem) then
    begin
      If assigned(MenuItem^.Name) then
        DisposeStr(MenuItem^.Name);
      if DebuggeeRunning then
        MenuItem^.Name:=NewStr(menu_run_continue)
      else
        MenuItem^.Name:=NewStr(menu_run_run);
    end;
  MenuItem:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cmResetDebugger);
  if assigned(MenuItem) then
    MenuItem^.Disabled:=not DebuggeeRunning;
  MenuItem:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cmUntilReturn);
  if assigned(MenuItem) then
    MenuItem^.Disabled:=not DebuggeeRunning;
end;


procedure TIDEApp.Parameters;
var R,R2: TRect;
    D: PCenterDialog;
    IL: PInputLine;
begin
  R.Assign(0,0,round(ScreenWidth*54/80),4);
  New(D, Init(R, dialog_programparameters));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-1); Inc(R.A.Y); R.B.Y:=R.A.Y+1;
    R2.Copy(R); R2.A.X:=16; Dec(R2.B.X,4);
    New(IL, Init(R2, 255));
    IL^.Data^:=GetRunParameters;
    Insert(IL);
    R2.Copy(R); R2.A.X:=R2.B.X-3; R2.B.X:=R2.A.X+3;
    Insert(New(PHistory, Init(R2, IL, hidRunParameters)));
    R2.Copy(R); R2.B.X:=16;
    Insert(New(PLabel, Init(R2, label_parameters_parameter, IL)));
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
  W : PFPWindow;
  PDL : PDisasLine;
  S,FileName : string;
  P,CurY,LineNr : longint;
{$endif}
begin
{$ifndef NODEBUG}
  if (DeskTop^.Current=nil) or
     ((TypeOf(DeskTop^.Current^)<>TypeOf(TSourceWindow)) and
      (TypeOf(DeskTop^.Current^)<>TypeOf(TDisassemblyWindow))) then
    Begin
       ErrorBox(msg_impossibletoreachcursor,nil);
       Exit;
    End;

  If not assigned(Debugger) or Not Debugger^.HasExe then
    begin
      InitDebugger;
      if not assigned(Debugger) then
        begin
          NoDebugger;
          exit;
        end;
    end;

  W:=PFPWindow(DeskTop^.Current);
  If assigned(W) then
    begin
      If TypeOf(W^)=TypeOf(TSourceWindow) then
        begin
          FileName:=PSourceWindow(W)^.Editor^.FileName;
          LineNr:=PSourceWindow(W)^.Editor^.CurPos.Y+1;
          Debugger^.Command('tbreak '+GDBFileName(NameAndExtOf(FileName))+':'+IntToStr(LineNr));
          Debugger^.Continue;
        end
      else
        begin
          CurY:=PDisassemblyWindow(W)^.Editor^.CurPos.Y;
          if CurY<PDisassemblyWindow(W)^.Editor^.GetLineCount then
            PDL:=PDisasLine(PDisassemblyWindow(W)^.Editor^.GetLine(CurY))
          else
            PDL:=nil;
          if assigned(PDL) then
            begin
              if PDL^.Address<>0 then
                begin
                  Debugger^.Command('tbreak *0x'+IntToHex(PDL^.Address,8));
                end
              else
                begin
                  S:=PDisassemblyWindow(W)^.Editor^.GetDisplayText(PDisassemblyWindow(W)^.Editor^.CurPos.Y);
                  p:=pos(':',S);
                  FileName:=Copy(S,1,p-1);
                  S:=Copy(S,p+1,high(S));
                  p:=pos(' ',S);
                  S:=Copy(S,1,p-1);
                  LineNr:=StrToInt(S);
                  Debugger^.Command('tbreak '+GDBFileName(NameAndExtOf(FileName))+':'+IntToStr(LineNr));
                end;
              Debugger^.Continue;
            end;
        end;
    end;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoOpenGDBWindow;
begin
{$ifndef NODEBUG}
  InitGDBWindow;
  if not assigned(Debugger) then
    begin
      new(Debugger,Init);
      if assigned(Debugger) then
        Debugger^.SetExe(ExeFile);
    end;
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
  WD : PDisassemblyWindow;
  PDL : PDisasLine;
  PB : PBreakpoint;
  S,FileName : string;
  b : boolean;
  CurY,P,LineNr : longint;
{$endif}
begin
{$ifndef NODEBUG}
  if (DeskTop^.Current=nil) or
     (TypeOf(DeskTop^.Current^)<>TypeOf(TSourceWindow)) and
     (TypeOf(DeskTop^.Current^)<>TypeOf(TDisassemblyWindow)) then
    Begin
       ErrorBox(msg_impossibletosetbreakpoint,nil);
       Exit;
    End;

  if assigned (DeskTop^.Current) and
     (TypeOf(DeskTop^.Current^)=TypeOf(TSourceWindow)) then
    begin
      W:=PSourceWindow(DeskTop^.Current);
      FileName:=W^.Editor^.FileName;
      LineNr:=W^.Editor^.CurPos.Y+1;
      BreakpointsCollection^.ToggleFileLine(FileName,LineNr);
    end
  else if assigned (DeskTop^.Current) and
     (TypeOf(DeskTop^.Current^)=TypeOf(TDisassemblyWindow)) then
    begin
      WD:=PDisassemblyWindow(DeskTop^.Current);
      CurY:=WD^.Editor^.CurPos.Y;
      if CurY<WD^.Editor^.GetLineCount then
        PDL:=PDisasLine(WD^.Editor^.GetLine(CurY))
      else
        PDL:=nil;
      if assigned(PDL) then
        begin
          if PDL^.Address<>0 then
            begin
              PB:=New(PBreakpoint,init_address(IntToHex(PDL^.Address,8)));
              BreakpointsCollection^.Insert(PB);
              WD^.Editor^.SetLineFlagState(CurY,lfBreakpoint,true);
            end
          else
            begin
              S:=WD^.Editor^.GetDisplayText(WD^.Editor^.CurPos.Y);
              p:=pos(':',S);
              FileName:=Copy(S,1,p-1);
              S:=Copy(S,p+1,high(S));
              p:=pos(' ',S);
              S:=Copy(S,1,p-1);
              LineNr:=StrToInt(S);
              b:=BreakpointsCollection^.ToggleFileLine(FileName,LineNr);
              WD^.Editor^.SetLineFlagState(CurY,lfBreakpoint,b);
            end;
        end;
    end;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

{
  $Log$
  Revision 1.3  2002-08-13 08:59:12  pierre
   + Run menu changes depending on wether the debuggee is running or not

  Revision 1.2  2001/11/07 00:28:53  pierre
   + Disassembly window made public

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.13  2001/03/12 17:34:55  pierre
   + Disassembly window started

  Revision 1.1.2.12  2001/03/06 21:47:59  pierre
   * avoid restart of debugger when opening GDB Window

  Revision 1.1.2.11  2001/02/19 10:40:50  pierre
   * Check for changed files after Running tool or shell

  Revision 1.1.2.10  2001/02/13 16:04:01  pierre
   * fixes for bugs 1280

  Revision 1.1.2.9  2000/12/30 22:52:27  peter
    * check modified while in debug mode. But placed it between a
      conditional again as it reports also if the file was already modified
      before the first compile.
    * remove unsaved file checks when compiling without primary file so it
      works the same as with a primary file set.

  Revision 1.1.2.8  2000/12/13 16:59:09  pierre
   * ErrFile filed added to DoExecute method

  Revision 1.1.2.7  2000/12/08 15:04:27  pierre
   * fix stupid error in DoRun

  Revision 1.1.2.6  2000/11/27 17:41:46  pierre
   * better GDB window opening if nothing compiled yet

  Revision 1.1.2.5  2000/11/19 00:23:33  pierre
   Task 23: nicer error message when trying to run unit or library

  Revision 1.1.2.4  2000/11/16 23:06:31  pierre
  * correct handling of Compile/Make if primary file is set

  Revision 1.1.2.3  2000/11/03 13:29:59  pierre
   Start GDB if opening GDB window

  Revision 1.1.2.2  2000/10/18 21:53:27  pierre
   * several Gabor fixes

  Revision 1.1.2.1  2000/10/09 16:28:25  pierre
   * several linux enhancements

  Revision 1.1  2000/07/13 09:48:35  michael
  + Initial import

  Revision 1.32  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.31  2000/04/18 11:42:37  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.30  2000/03/21 23:27:35  pierre
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
