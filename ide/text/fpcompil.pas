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

{ don't redir under linux, because all stdout (also from the ide!) will
  then be redired (PFV) }
{$ifndef debug}
  {$ifndef linux}
    {$define redircompiler}
  {$endif}
{$endif}

{ $define VERBOSETXT}

uses
  Objects,
  Drivers,Views,Dialogs,
  WViews,
  FPViews;

type
  TCompileMode = (cBuild,cMake,cCompile,cRun);

{$ifndef OLDCOMP}
type
    PCompilerMessage = ^TCompilerMessage;
    TCompilerMessage = object(TMessageItem)
      function GetText(MaxLen: Sw_Integer): String; virtual;
    end;

    PCompilerMessageListBox = ^TCompilerMessageListBox;
    TCompilerMessageListBox = object(TMessageListBox)
      function GetPalette: PPalette; virtual;
    end;

    PCompilerMessageWindow = ^TCompilerMessageWindow;
    TCompilerMessageWindow = object(TFPWindow)
      constructor Init;
      procedure   Updateinfo;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetPalette: PPalette; virtual;
      procedure   Close;virtual;
      procedure   Zoom;virtual;
      destructor  Done; virtual;
      procedure   AddMessage(AClass: longint;const Msg, Module: string; Line, Column: longint);
      procedure   ClearMessages;
      procedure   SetCompileMode(Amode:TCompileMode);
      procedure   SetCompileShow(b:boolean);
      procedure   StartCompilation;
      function    EndCompilation:boolean;
    private
      CompileShowed : boolean;
      Mode   : TCompileMode;
      MsgLB  : PCompilerMessageListBox;
      CurrST,
      InfoST : PColorStaticText;
      LineST : PStaticText;
    end;

const
    CompilerMessageWindow : PCompilerMessageWindow  = nil;

{$else}
type
    PCompileStatusDialog = ^TCompileStatusDialog;
    TCompileStatusDialog = object(TCenterDialog)
      ST    : PAdvancedStaticText;
      KeyST : PColorStaticText;
      constructor Init;
      procedure   Update;
    private
      MsgLB: PMessageListBox;
    end;

const
    SD: PCompileStatusDialog = nil;

{$endif}

procedure DoCompile(Mode: TCompileMode);


implementation

uses
  Dos,Video,
  App,Commands,
  CompHook,
  WEditor,
{$ifdef redircompiler}
  FPRedir,
{$endif}
  FPConst,FPVars,FPUtils,FPIntf,FPSwitch;

const
    LastStatusUpdate : longint = 0;

{$ifndef OLDCOMP}

{*****************************************************************************
                               TCompilerMessage
*****************************************************************************}

function TCompilerMessage.GetText(MaxLen: Sw_Integer): String;
var
  ClassS: string[20];
  S: string;
begin
  if TClass=
    V_Fatal       then ClassS:='Fatal'       else if TClass =
    V_Error       then ClassS:='Error'       else if TClass =
    V_Normal      then ClassS:=''            else if TClass =
    V_Warning     then ClassS:='Warning'     else if TClass =
    V_Note        then ClassS:='Note'        else if TClass =
    V_Hint        then ClassS:='Hint'
{$ifdef VERBOSETXT}
    else if TClass =
    V_Macro       then ClassS:='Macro'       else if TClass =
    V_Procedure   then ClassS:='Procedure'   else if TClass =
    V_Conditional then ClassS:='Conditional' else if TClass =
    V_Info        then ClassS:='Info'        else if TClass =
    V_Status      then ClassS:='Status'      else if TClass =
    V_Used        then ClassS:='Used'        else if TClass =
    V_Tried       then ClassS:='Tried'       else if TClass =
    V_Debug       then ClassS:='Debug'
  else
   ClassS:='???';
{$else}
  else
   ClassS:='';
{$endif}
  if ClassS<>'' then
   ClassS:=RExpand(ClassS,0)+': ';
  if assigned(Module) and
     (TClass<=V_ShowFile) and (status.currentsource<>'') and (status.currentline>0) then
    begin
      if Row>0 then
       begin
         if Col>0 then
          S:=Module^+'('+IntToStr(Row)+','+IntToStr(Col)+') '+ClassS
         else
          S:=Module^+'('+IntToStr(Row)+') '+ClassS;
       end
      else
       S:=Module^+'('+IntToStr(Row)+') '+ClassS
    end
  else
    S:=ClassS;
  if assigned(Text) then
    S:=S+Text^;
  if length(S)>MaxLen then
    S:=copy(S,1,MaxLen-2)+'..';
  GetText:=S;
end;


{*****************************************************************************
                             TCompilerMessageListBox
*****************************************************************************}

function TCompilerMessageListBox.GetPalette: PPalette;
const
  P: string[length(CBrowserListBox)] = CBrowserListBox;
begin
  GetPalette:=@P;
end;


{*****************************************************************************
                                TCompilerMessageWindow
*****************************************************************************}

constructor TCompilerMessageWindow.Init;
var R: TRect;
    HSB,VSB: PScrollBar;
begin
  Desktop^.GetExtent(R);
  R.A.Y:=R.B.Y-7;
  inherited Init(R,'Compiler Messages',SearchFreeWindowNo);
  HelpCtx:=hcMessagesWindow;

  HSB:=StandardScrollBar(sbHorizontal+sbHandleKeyboard);
  Insert(HSB);
  VSB:=StandardScrollBar(sbVertical+sbHandleKeyboard);
  Insert(VSB);

  GetExtent(R);
  R.Grow(-1,-1);
  New(MsgLB, Init(R, HSB, VSB));
  Insert(MsgLB);

  Updateinfo;

  CompilerMessageWindow:=@self;
end;

procedure TCompilerMessageWindow.AddMessage(AClass: longint;const Msg, Module: string; Line, Column: longint);
begin
  if AClass>=V_Info then
    Line:=0;
  MsgLB^.AddItem(New(PCompilerMessage,Init(AClass, Msg, MsgLB^.AddModuleName(Module), Line, Column)));
end;

procedure TCompilerMessageWindow.ClearMessages;
begin
  MsgLB^.Clear;
  ReDraw;
end;


procedure TCompilerMessageWindow.Updateinfo;
begin
  if CompileShowed then
   begin
     InfoST^.SetText(
       RExpand(' Main file : '#1#$7f+Copy(SmartPath(MainFile),1,39),40)+#2+
         'Total lines  : '#1#$7e+IntToStr(Status.CompiledLines)+#2#13+
       RExpand(' Target    : '#1#$7f+KillTilde(TargetSwitches^.ItemName(TargetSwitches^.GetCurrSel)),40)+#2+
         'Total errors : '#1#$7e+IntToStr(Status.ErrorCount)
     );
     if status.currentline>0 then
      CurrST^.SetText(' Status: '#1#$7e+status.currentsource+'('+IntToStr(status.currentline)+')'#2)
     else
      CurrST^.SetText(' Status: '#1#$7e+status.currentsource+#2);
   end;
  ReDraw;
end;


procedure TCompilerMessageWindow.SetCompileMode(Amode:TCompileMode);
begin
  mode:=Amode;
end;

procedure TCompilerMessageWindow.SetCompileShow(b:boolean);
var
  r : TRect;
  c : word;
begin
  r.a:=Origin;
  r.b:=Size;
  if b then
   begin
     if CompileShowed then
      exit;
     dec(r.a.y,4);
     inc(r.b.x,r.a.x);
     inc(r.b.y,r.a.y+4);
     ChangeBounds(r);
   { shrink msg listbox }
     GetExtent(R);
     R.Grow(-1,-1);
     dec(R.b.y,5);
     MsgLB^.ChangeBounds(r);
   { insert line and infost }
     C:=((Desktop^.GetColor(32+6) and $f0) or White)*256+Desktop^.GetColor(32+6);
     GetExtent(R);
     R.Grow(-1,-1);
     inc(R.a.y,5);
     r.b.y:=r.a.y+1;
     New(LineST, Init(R, CharStr('Ä', MaxViewWidth)));
     LineST^.GrowMode:=gfGrowHiX;
     Insert(LineST);
     inc(r.a.x);
     dec(r.b.x);
     inc(r.a.y);
     r.b.y:=r.a.y+2;
     New(InfoST, Init(R,'', C));
     InfoST^.GrowMode:=gfGrowHiX;
     InfoST^.DontWrap:=true;
     Insert(InfoST);
     inc(r.a.y,2);
     r.b.y:=r.a.y+1;
     New(CurrST, Init(R,'', C));
     CurrST^.GrowMode:=gfGrowHiX;
     Insert(CurrST);
   end
  else
   begin
     if not CompileShowed then
      exit;
     inc(r.a.y,4);
     inc(r.b.x,r.a.x);
     inc(r.b.y,r.a.y-4);
     ChangeBounds(r);
   { remove infost and line }
     Delete(CurrSt);
     Delete(InfoSt);
     Delete(LineSt);
   end;
  CompileShowed:=b;
{ update all windows }
  Message(Application,evCommand,cmUpdate,nil);
end;


procedure TCompilerMessageWindow.StartCompilation;
begin
  SetCompileShow(true);
  Updateinfo;
end;


function TCompilerMessageWindow.EndCompilation:boolean;
var
  doevent,
  closewin : boolean;
  E : TEvent;
begin
  { be sure that we have the latest info displayed, fake the currentsource
    and currentline to display the result }
  status.currentline:=0;
  if status.errorcount=0 then
    status.currentsource:='Compilation Succesfull'
  else
    status.currentsource:='Compilation Failed';
  Updateinfo;
  doevent:=false;
  closewin:=(status.errorcount=0);
  if (status.errorcount>0) or (Mode<>cRun) then
   begin
     repeat
       GetEvent(E);
       case E.what of
         evKeyDown :
           begin
             { only exit when not navigating trough the errors }
             case E.Keycode of
               kbEsc :
                 begin
                   closewin:=true;
                   break;
                 end;
               kbSpaceBar :
                 begin
                   closewin:=false;
                   doevent:=true;
                   break;
                 end;
               kbUp,
               kbDown,
               kbPgUp,
               kbPgDn,
               kbHome,
               kbEnd : ;
               else
                 break;
             end;
           end;
         evCommand :
           begin
             case E.command of
               cmQuit,
               cmClose,
               cmMsgGotoSource,
               cmMsgTrackSource :
                 begin
                   closewin:=false;
                   doevent:=true;
                   break;
                 end;
             end;
           end;
       end;
       HandleEvent(E);
     until false;
     SetCompileShow(false);
   { Handle the Source tracking after the window has shrunk }
     if doevent then
       HandleEvent(E);
   end;
  EndCompilation:=closewin;
end;


procedure TCompilerMessageWindow.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmListFocusChanged :
          if Event.InfoPtr=MsgLB then
            Message(Application,evBroadcast,cmClearLineHighlights,@Self);
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TCompilerMessageWindow.Close;
begin
  Hide;
end;

procedure TCompilerMessageWindow.Zoom;
begin
  SetCompileShow(false);
  inherited Zoom;
end;

function TCompilerMessageWindow.GetPalette: PPalette;
const
  S : string[length(CBrowserWindow)] = CBrowserWindow;
begin
  GetPalette:=@S;
end;

destructor TCompilerMessageWindow.Done;
begin
  CompilerMessageWindow:=nil;
  inherited Done;
end;


{****************************************************************************
                               Compiler Hooks
****************************************************************************}

function CompilerStatus: boolean; {$ifndef FPC}far;{$endif}
begin
{ only display every 50 lines }
  if (status.currentline mod 50=0) then
   begin
     { update info messages }
     if assigned(CompilerMessageWindow) then
      CompilerMessageWindow^.updateinfo;
     { update memory usage }
     HeapView^.Update;
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
     CompilerMessageWindow^.AddMessage(Level,S,status.currentsourcepath+status.currentsource,
       status.currentline,status.currentcolumn);
   end;
{$ifdef TEMPHEAP}
  switch_to_temp_heap;
{$endif TEMPHEAP}
end;


{****************************************************************************
                                 DoCompile
****************************************************************************}

function GetExePath: string;
var Path: string;
    I: Sw_integer;
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


procedure DoCompile(Mode: TCompileMode);
var
  P: PSourceWindow;
  FileName: string;
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
{ Show Compiler Info }
  if not CompilerMessageWindow^.GetState(sfVisible) then
   CompilerMessageWindow^.Show;
  CompilerMessageWindow^.MakeFirst;
  CompilerMessageWindow^.ClearMessages;

  CompilerMessageWindow^.SetCompileMode(Mode);
  CompilerMessageWindow^.StartCompilation;

  { hook compiler output }
  do_status:=CompilerStatus;
  do_stop:=CompilerStop;
  do_comment:=CompilerComment;

{$ifdef redircompiler}
  ChangeRedirOut('fp$$$.out',false);
  ChangeRedirError('fp$$$.err',false);
{$endif}
{$ifdef TEMPHEAP}
  split_heap;
  switch_to_temp_heap;
{$endif TEMPHEAP}
  Compile(FileName);
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
{$ifdef redircompiler}
  RestoreRedirOut;
  RestoreRedirError;
{$endif}

{ endcompilation returns true if the messagewindow should be removed }
  if CompilerMessageWindow^.EndCompilation then
   CompilerMessageWindow^.Hide;

  Message(Application,evCommand,cmUpdate,nil);
{$ifdef TEMPHEAP}
  releasetempheap;
  unsplit_heap;
{$endif TEMPHEAP}
end;



{$else OLDCOMP}

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

{****************************************************************************
                                 DoCompile
****************************************************************************}

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

{$ifdef redircompiler}
  ChangeRedirOut('fp$$$.out',false);
  ChangeRedirError('fp$$$.err',false);
{$endif}
{$ifdef TEMPHEAP}
  split_heap;
  switch_to_temp_heap;
{$endif TEMPHEAP}
  Compile(FileName);
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
{$ifdef redircompiler}
  RestoreRedirOut;
  RestoreRedirError;
{$endif}

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

{$endif}

end.
{
  $Log$
  Revision 1.20  1999-03-23 16:16:38  peter
    * linux fixes

  Revision 1.19  1999/03/19 16:04:27  peter
    * new compiler dialog

  Revision 1.18  1999/03/16 12:38:07  peter
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
