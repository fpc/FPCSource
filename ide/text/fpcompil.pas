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

uses WViews,
     FPViews;

type
    TCompileMode = (cBuild,cMake,cCompile,cRun);

    PCompileStatusDialog = ^TCompileStatusDialog;
    TCompileStatusDialog = object(TCenterDialog)
      ST    : PAdvancedStaticText;
      KeyST : PColorStaticText;
      constructor Init;
      procedure   Update;
    private
      MsgLB: PMessageListBox;
    end;

procedure DoCompile(Mode: TCompileMode);

const SD: PCompileStatusDialog = nil;

implementation

uses
  Dos,Video,
  Objects,Drivers,Views,App,Commands,
  CompHook,
  FPRedir,
  FPConst,FPVars,FPUtils,FPIntf,FPSwitch;

const
    LastStatusUpdate : longint = 0;

constructor TCompileStatusDialog.Init;
var R: TRect;
begin
  R.Assign(0,0,50,11+7);
  inherited Init(R, 'Compiling');
  GetExtent(R); R.B.Y:=11;
  R.Grow(-3,-2);
  New(ST, Init(R, ''));
  Insert(ST);
  GetExtent(R); R.B.Y:=11;
  R.Grow(-1,-1); R.A.Y:=R.B.Y-1;
  New(KeyST, Init(R, '', Blue*16+White+longint($80+Blue*16+White)*256));
  Insert(KeyST);
  GetExtent(R); R.Grow(-1,-1); R.A.Y:=10;
  New(MsgLB, Init(R, nil, nil));
  Insert(MsgLB);
end;


procedure TCompileStatusDialog.Update;
var StatusS,KeyS: string;
const CtrlBS = 'Press Ctrl+Break to cancel';
      SuccessS = 'Compile successful: ~Press Enter~';
      FailS = 'Compile failed';
begin
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
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
        KeyS:=SuccessS;
      end;
    cpFailed    :
      begin
        StatusS:='Failed to compile...';
        KeyS:=FailS;
      end;
  end;
  ST^.SetText(
    'Main file: '+SmartPath(MainFile)+#13+
    StatusS+#13#13+
    'Target: '+LExpand(KillTilde(TargetSwitches^.ItemName(TargetSwitches^.GetCurrSel)),12)+'    '+
    'Line number: '+IntToStrL(Status.CurrentLine,7)+#13+
    'Free memory: '+IntToStrL(MemAvail div 1024,6)+'K'+ '    '+
    'Total lines: '+IntToStrL(Status.CompiledLines,7)+#13+
    'Total errors: '+IntToStrL(Status.ErrorCount,5)
  );
  KeyST^.SetText(^C+KeyS);
{$ifdef TEMPHEAP}
  switch_to_temp_heap;
{$endif TEMPHEAP}
end;


{****************************************************************************
                               Compiler Hooks
****************************************************************************}

function CompilerStatus: boolean; {$ifndef FPC}far;{$endif}
var TT: longint;
begin
  TT:=GetDosTicks;
  if abs(TT-LastStatusUpdate)>=round(CompilerStatusUpdateDelay*18.2) then
    begin
      LastStatusUpdate:=TT;
  if SD<>nil then SD^.Update;
    end;
  CompilerStatus:=false;
end;


procedure CompilerStop; {$ifndef FPC}far;{$endif}
begin
end;


function CompilerComment(Level:Longint; const s:string):boolean; {$ifndef FPC}far;{$endif}
begin
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
  CompilerComment:=false;
{$ifndef DEV}
  if (status.verbosity and Level)=Level then
{$endif}
   begin
     ProgramInfoWindow^.AddMessage(Level,S,status.currentsourcepath+status.currentsource,
       status.currentline,status.currentcolumn);
     if SD<>nil then
     SD^.MsgLB^.AddItem(
       New(PMessageItem, Init(Level, S, SD^.MsgLB^.AddModuleName(SmartPath(status.currentmodule)),
         status.currentline,status.currentcolumn)));
   end;
{$ifdef TEMPHEAP}
  switch_to_temp_heap;
{$endif TEMPHEAP}
end;

function GetExePath: string;
var Path: string;
    I: integer;
begin
  Path:='.'+DirSep;
  if DirectorySwitches<>nil then
    with DirectorySwitches^ do
    for I:=0 to ItemCount-1 do
      begin
        if Pos('EXE',KillTilde(ItemName(I)))>0 then
          begin Path:=GetStringItem(I); Break; end;
      end;
  GetExePath:=CompleteDir(FExpand(Path));
end;

{****************************************************************************
                                 DoCompile
****************************************************************************}

procedure DoCompile(Mode: TCompileMode);

  function IsExitEvent(E: TEvent): boolean;
  begin
    IsExitEvent:=(E.What=evKeyDown) and
                 ((E.KeyCode=kbEnter) or (E.KeyCode=kbEsc)) or
                 ((E.What=evCommand) and (E.command=cmClose));
  end;


var
  P: PSourceWindow;
  FileName: string;
  E: TEvent;
{  WasVisible: boolean;}
begin
{ Get FileName }
  P:=Message(Desktop,evBroadcast,cmSearchWindow,nil);
  if (PrimaryFileMain='') and (P=nil) then
    begin
      ErrorBox('Oooops, nothing to compile.',nil);
      Exit;
    end;
  if PrimaryFileMain<>'' then
    FileName:=PrimaryFileMain
  else
    begin
      if P^.Editor^.Modified and (not P^.Editor^.Save) then
       begin
         ErrorBox('Can''t compile unsaved file.',nil);
         Exit;
       end;
      FileName:=P^.Editor^.FileName;
    end;
  WriteSwitches(SwitchesPath);
  MainFile:=FixFileName(FExpand(FileName));
  If GetEXEPath<>'' then
    EXEFile:=FixFileName(GetEXEPath+NameOf(MainFile)+ExeExt)
  else
    EXEFile:=DirOf(MainFile)+NameOf(MainFile)+ExeExt;
{ Reset }
  CtrlBreakHit:=false;
{ Show Program Info }
{  WasVisible:=ProgramInfoWindow^.GetState(sfVisible);
  ProgramInfoWindow^.LogLB^.Clear;
  if WasVisible=false then
    ProgramInfoWindow^.Show;
  ProgramInfoWindow^.MakeFirst;}
  if Assigned(ProgramInfoWindow) then
    ProgramInfoWindow^.ClearMessages;

  CompilationPhase:=cpCompiling;
  New(SD, Init);
  SD^.SetState(sfModal,true);
  Application^.Insert(SD);
  SD^.Update;

  do_status:=CompilerStatus;
  do_stop:=CompilerStop;
  do_comment:=CompilerComment;

{$ifndef debug}
  { this avoids all flickers
    and allows to get assembler and linker messages
    but also forbids to use GDB inside !! }
  ChangeRedirOut('fp$$$.out',false);
  ChangeRedirError('fp$$$.err',false);
{$endif ndef debug}
{$ifdef TEMPHEAP}
  split_heap;
  switch_to_temp_heap;
{$endif TEMPHEAP}
  Compile(FileName);
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
{$ifdef go32v2}
  RestoreRedirOut;
  RestoreRedirError;
{$endif def go32v2}

  if status.errorCount=0
     then CompilationPhase:=cpDone
     else CompilationPhase:=cpFailed;
  SD^.Update;

  SD^.SetState(sfModal,false);

  if ((CompilationPhase in[cpDone,cpFailed]) or (ShowStatusOnError)) and (Mode<>cRun) then
   repeat
     SD^.GetEvent(E);
     if IsExitEvent(E)=false then
      SD^.HandleEvent(E);
   until IsExitEvent(E);

  Application^.Delete(SD);
  Dispose(SD, Done); SD:=nil;

{  if (WasVisible=false) and (status.errorcount=0) then
   ProgramInfoWindow^.Hide;}
  Message(Application,evCommand,cmUpdate,nil);
{$ifdef TEMPHEAP}
  releasetempheap;
  unsplit_heap;
{$endif TEMPHEAP}
end;

end.
{
  $Log$
  Revision 1.18  1999-03-16 12:38:07  peter
    * tools macro fixes
    + tph writer
    + first things for resource files

  Revision 1.17  1999/03/12 01:13:56  peter
    * flag if trytoopen should look for other extensions
    + browser tab in the tools-compiler

  Revision 1.16  1999/03/07 23:00:47  pierre
   * Fix for path of executable

  Revision 1.15  1999/03/01 15:41:50  peter
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

  Revision 1.14  1999/02/22 12:46:56  peter
    * small fixes for linux and grep

  Revision 1.13  1999/02/22 11:51:33  peter
    * browser updates from gabor

  Revision 1.12  1999/02/22 11:29:36  pierre
    + added col info in MessageItem
    + grep uses HighLightExts and should work for linux

  Revision 1.11  1999/02/08 09:31:00  florian
    + some split heap stuff, in $ifdef TEMPHEAP

  Revision 1.10  1999/02/05 13:51:39  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

  Revision 1.9  1999/02/05 13:06:28  pierre
   * allow cmClose for Compilation Dialog box

  Revision 1.8  1999/02/04 13:32:01  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.7  1999/01/21 11:54:11  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.6  1999/01/15 16:12:43  peter
    * fixed crash after compile

  Revision 1.5  1999/01/14 21:42:19  peter
    * source tracking from Gabor

  Revision 1.4  1999/01/12 14:29:32  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.3  1999/01/04 11:49:42  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:42  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.3  1998/12/22 10:39:40  peter
    + options are now written/read
    + find and replace routines

}
