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
{$i globdir.inc}
unit FPCompil;

interface

{ don't redir under linux, because all stdout (also from the ide!) will
  then be redired (PFV) }
{ this should work now correctly because
  RedirDisableAll and RedirEnableAll function are added in fpredir (PM) }

{ $define VERBOSETXT}

{$mode objfpc}

uses
  Objects,
{$ifdef COMPILER_1_0}
  Files,
{$else COMPILER_1_0}
  FInput,
{$endif COMPILER_1_0}
  Drivers,Views,Dialogs,
  WUtils,WViews,WCEdit,
  FPSymbol,
  FPViews;

type
  TCompileMode = (cBuild,cMake,cCompile,cRun);

type
    PCompilerMessage = ^TCompilerMessage;
    TCompilerMessage = object(TMessageItem)
      function GetText(MaxLen: Sw_Integer): String; virtual;
    end;

    PCompilerMessageListBox = ^TCompilerMessageListBox;
    TCompilerMessageListBox = object(TMessageListBox)
      function  GetPalette: PPalette; virtual;
      procedure SelectFirstError;
    end;

    PCompilerMessageWindow = ^TCompilerMessageWindow;
    TCompilerMessageWindow = object(TFPWindow)
      constructor Init;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetPalette: PPalette; virtual;
      procedure   Close;virtual;
      destructor  Done; virtual;
      procedure   SizeLimits(var Min, Max: TPoint); virtual;
      procedure   AddMessage(AClass: longint;const Msg, Module: string; Line, Column: longint);
      procedure   ClearMessages;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   UpdateCommands; virtual;
    private
      {CompileShowed : boolean;}
      {Mode   : TCompileMode;}
      MsgLB  : PCompilerMessageListBox;
      {CurrST,
      InfoST : PColorStaticText;}
    end;

    PCompilerStatusDialog = ^TCompilerStatusDialog;
    TCompilerStatusDialog = object(TCenterDialog)
      ST    : PAdvancedStaticText;
      KeyST : PColorStaticText;
      constructor Init;
      destructor Done;virtual;
      procedure   Update;
    end;

{$ifdef COMPILER_1_0}
    PFPInputFile = ^TFPInputFile;
    TFPInputFile = object(tinputfile)
      constructor Init(AEditor: PFileEditor);
    {$ifdef FPC}protected{$else}public{$endif}
      function fileopen(const filename: string): boolean; virtual;
      function fileseek(pos: longint): boolean; virtual;
      function fileread(var databuf; maxsize: longint): longint; virtual;
      function fileeof: boolean; virtual;
      function fileclose: boolean; virtual;
    private
      Editor: PFileEditor;
      S: PStream;
    end;
{$else COMPILER_1_0}
    TFPInputFile = class(tinputfile)
      constructor Create(AEditor: PFileEditor);
    {$ifdef FPC}protected{$else}public{$endif}
      function fileopen(const filename: string): boolean; override;
      function fileseek(pos: longint): boolean; override;
      function fileread(var databuf; maxsize: longint): longint; override;
      function fileeof: boolean; override;
      function fileclose: boolean; override;
    private
      Editor: PFileEditor;
      S: PStream;
    end;
{$endif COMPILER_1_0}

const
    CompilerMessageWindow : PCompilerMessageWindow  = nil;
    CompilerStatusDialog  : PCompilerStatusDialog = nil;
    CompileStamp          : longint = 0;

procedure DoCompile(Mode: TCompileMode);
function  NeedRecompile(Mode :TCompileMode; verbose : boolean): boolean;
procedure ParseUserScreen;

procedure RegisterFPCompile;



implementation

uses
{$ifdef Unix}
  {$ifdef VER1_0}
    Linux,
  {$else}
    Unix,
  {$endif}
{$endif}
{$ifdef go32v2}
  dpmiexcp,
{$endif}
{$ifdef win32}
  signals,
{$endif}
{$ifdef HasSignal}
  fpcatch,
{$endif HasSignal}
  Dos,Video,
  StdDlg,App,tokens,
{$ifdef FVISION}
  FVConsts,
{$else}
  Commands,
{$endif}
  CompHook, Compiler, systems, browcol,
  WEditor,
  FPString,FPRedir,FPDesk,
  FPUsrScr,FPHelp,
{$ifndef NODEBUG}FPDebug,{$endif}
  FPConst,FPVars,FPUtils,FPIntf,FPSwitch;

{$ifndef NOOBJREG}
const
  RCompilerMessageListBox: TStreamRec = (
     ObjType: 1211;
     VmtLink: Ofs(TypeOf(TCompilerMessageListBox)^);
     Load:    @TCompilerMessageListBox.Load;
     Store:   @TCompilerMessageListBox.Store
  );
  RCompilerMessageWindow: TStreamRec = (
     ObjType: 1212;
     VmtLink: Ofs(TypeOf(TCompilerMessageWindow)^);
     Load:    @TCompilerMessageWindow.Load;
     Store:   @TCompilerMessageWindow.Store
  );
{$endif}


procedure ParseUserScreen;
var
  y : longint;
  Text,Attr : String;
  DisplayCompilerWindow : boolean;
  cc: integer;

    procedure SearchBackTrace;
      var AText,ModuleName,st : String;
          row : longint;
      begin
        if pos('  0x',Text)=1 then
          begin
            AText:=Text;
            Delete(Text,1,10);
            While pos(' ',Text)=1 do
              Delete(Text,1,1);
            if pos('of ',Text)>0 then
              begin
                ModuleName:=Copy(Text,pos('of ',Text)+3,255);
                While ModuleName[Length(ModuleName)]=' ' do
                  Delete(ModuleName,Length(ModuleName),1);
              end
            else
              ModuleName:='';
            if pos('line ',Text)>0 then
              begin
                Text:=Copy(Text,Pos('line ',Text)+5,255);
                st:=Copy(Text,1,Pos(' ',Text)-1);
                Val(st,row,cc);
              end
            else
              row:=0;
            CompilerMessageWindow^.AddMessage(V_Fatal,AText
                  ,ModuleName,row,1);
            DisplayCompilerWindow:=true;
          end;
      end;

    procedure InsertInMessages(Const TypeStr : String;_Type : longint;EnableDisplay : boolean);
      var p,p2,col,row : longint;
          St,ModuleName : string;

      begin
        p:=pos(TypeStr,Text);
        p2:=Pos('(',Text);
        if (p>0)  and (p2>0) and (p2<p) then
          begin
            ModuleName:=Copy(Text,1,p2-1);
            st:=Copy(Text,p2+1,255);
            Val(Copy(st,1,pos(',',st)-1),row,cc);
            st:=Copy(st,Pos(',',st)+1,255);
            Val(Copy(st,1,pos(')',st)-1),col,cc);
            CompilerMessageWindow^.AddMessage(_type,Copy(Text,pos(':',Text)+1,255)
              ,ModuleName,row,col);
            If EnableDisplay then
              DisplayCompilerWindow:=true;
          end;
      end;

begin
  if not assigned(UserScreen) then
    exit;
  DisplayCompilerWindow:=false;
  PushStatus('Parsing User Screen');
  for Y:=0 to UserScreen^.GetHeight do
    begin
      UserScreen^.GetLine(Y,Text,Attr);
      SearchBackTrace;
      InsertInMessages(' Fatal:',v_Fatal,true);
      InsertInMessages(' Error:',v_Error,true);
      InsertInMessages(' Warning:',v_Warning,false);
      InsertInMessages(' Note:',v_Note,false);
      InsertInMessages(' Info:',v_Info,false);
      InsertInMessages(' Hint:',v_Hint,false);
    end;
  if DisplayCompilerWindow then
    begin
      if not CompilerMessageWindow^.GetState(sfVisible) then
        CompilerMessageWindow^.Show;
      CompilerMessageWindow^.MakeFirst;
      CompilerMessageWindow^.MsgLB^.SelectFirstError;
    end;
  PopStatus;
end;

{*****************************************************************************
                               TCompilerMessage
*****************************************************************************}

function TCompilerMessage.GetText(MaxLen: Sw_Integer): String;
var
  ClassS: string[20];
  S: string;
begin
  if TClass=
    V_Fatal       then ClassS:=msg_class_Fatal   else if TClass =
    V_Error       then ClassS:=msg_class_Error   else if TClass =
    V_Normal      then ClassS:=msg_class_Normal  else if TClass =
    V_Warning     then ClassS:=msg_class_Warning else if TClass =
    V_Note        then ClassS:=msg_class_Note    else if TClass =
    V_Hint        then ClassS:=msg_class_Hint
{$ifdef VERBOSETXT}
    else if TClass =
    V_Macro       then ClassS:=msg_class_macro   else if TClass =
    V_Procedure   then ClassS:=msg_class_procedure else if TClass =
    V_Conditional then ClassS:=msg_class_conditional else if TClass =
    V_Info        then ClassS:=msg_class_info    else if TClass =
    V_Status      then ClassS:=msg_class_status  else if TClass =
    V_Used        then ClassS:=msg_class_used    else if TClass =
    V_Tried       then ClassS:=msg_class_tried   else if TClass =
    V_Debug       then ClassS:=msg_class_debug
  else
   ClassS:='???';
{$else}
  else
   ClassS:='';
{$endif}
  if ClassS<>'' then
   ClassS:=RExpand(ClassS,0)+': ';
  if assigned(Module) and
     (TClass<=V_ShowFile)
     {and (status.currentsource<>'') and (status.currentline>0)} then
    begin
      if Row>0 then
       begin
         if Col>0 then
          S:=NameAndExtOf(Module^)+'('+IntToStr(Row)+','+IntToStr(Col)+') '+ClassS
         else
          S:=NameAndExtOf(Module^)+'('+IntToStr(Row)+') '+ClassS;
       end
      else
       S:=NameAndExtOf(Module^)+'('+IntToStr(Row)+') '+ClassS
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

procedure TCompilerMessageListBox.SelectFirstError;
  function IsError(P : PCompilerMessage) : boolean;
    begin
      IsError:=(P^.TClass and (V_Fatal or V_Error))<>0;
    end;
  var
    P : PCompilerMessage;
begin
  P:=List^.FirstThat(@IsError);
  If Assigned(P) then
    Begin
      FocusItem(List^.IndexOf(P));
      DrawView;
    End;
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
  inherited Init(R,dialog_compilermessages,{SearchFreeWindowNo}wnNoNumber);
  HelpCtx:=hcCompilerMessagesWindow;

  AutoNumber:=true;

  HSB:=StandardScrollBar(sbHorizontal+sbHandleKeyboard);
  HSB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY;
  Insert(HSB);
  VSB:=StandardScrollBar(sbVertical+sbHandleKeyboard);
  VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
  Insert(VSB);

  GetExtent(R);
  R.Grow(-1,-1);
  New(MsgLB, Init(R, HSB, VSB));

  MsgLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
  Insert(MsgLB);
  CompilerMessageWindow:=@self;
end;


procedure TCompilerMessageWindow.AddMessage(AClass: longint;const Msg, Module: string; Line, Column: longint);
begin
  if AClass>=V_Info then
    Line:=0;
  MsgLB^.AddItem(New(PCompilerMessage,Init(AClass, Msg, MsgLB^.AddModuleName(Module), Line, Column)));
  if (@Self=CompilerMessageWindow) and ((AClass = V_fatal) or (AClass = V_Error)) then
    begin
      if not GetState(sfVisible) then
        Show;
      if Desktop^.First<>PView(CompilerMessageWindow) then
        MakeFirst;
    end;
end;


procedure TCompilerMessageWindow.ClearMessages;
begin
  MsgLB^.Clear;
  ReDraw;
end;


{procedure TCompilerMessageWindow.Updateinfo;
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
end;}


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


procedure TCompilerMessageWindow.SizeLimits(var Min, Max: TPoint);
begin
  inherited SizeLimits(Min,Max);
  Min.X:=20;
  Min.Y:=4;
end;


procedure TCompilerMessageWindow.Close;
begin
  Hide;
end;


function TCompilerMessageWindow.GetPalette: PPalette;
const
  S : string[length(CBrowserWindow)] = CBrowserWindow;
begin
  GetPalette:=@S;
end;


constructor TCompilerMessageWindow.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S,MsgLB);
end;


procedure TCompilerMessageWindow.Store(var S: TStream);
begin
  if MsgLB^.List=nil then
    MsgLB^.NewList(New(PCollection, Init(100,100)));
  inherited Store(S);
  PutSubViewPtr(S,MsgLB);
end;

procedure TCompilerMessageWindow.UpdateCommands;
var Active: boolean;
begin
  Active:=GetState(sfActive);
  SetCmdState(CompileCmds,Active);
  Message(Application,evBroadcast,cmCommandSetChanged,nil);
end;

procedure TCompilerMessageWindow.SetState(AState: Word; Enable: Boolean);
var OldState: word;
begin
  OldState:=State;
  inherited SetState(AState,Enable);
  if ((AState and sfActive)<>0) and (((OldState xor State) and sfActive)<>0) then
    UpdateCommands;
end;

destructor TCompilerMessageWindow.Done;
begin
  CompilerMessageWindow:=nil;
  inherited Done;
end;


{****************************************************************************
                          CompilerStatusDialog
****************************************************************************}

constructor TCompilerStatusDialog.Init;
var R: TRect;
begin
  R.Assign(0,0,50,11);
  ClearFormatParams; AddFormatParamStr(KillTilde(SwitchesModeName[SwitchesMode]));
  inherited Init(R, FormatStrF(dialog_compilingwithmode, FormatParams));
  GetExtent(R); R.B.Y:=11;
  R.Grow(-3,-2);
  New(ST, Init(R, ''));
  Insert(ST);
  GetExtent(R); R.B.Y:=11;
  R.Grow(-1,-1); R.A.Y:=R.B.Y-1;
  New(KeyST, Init(R, '', Blue*16+White+longint($80+Blue*16+White)*256,true));
  Insert(KeyST);
  { Reset Status infos see bug 1585 }
  Fillchar(Status,SizeOf(Status),#0);
end;

destructor TCompilerStatusDialog.Done;
begin
  if @Self=CompilerStatusDialog then
    CompilerStatusDialog:=nil;
  Inherited Done;
end;

procedure TCompilerStatusDialog.Update;
var
  StatusS,KeyS: string;
const
  MaxFileNameSize = 46;
begin
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
  case CompilationPhase of
    cpCompiling :
      begin
        ClearFormatParams;
        if Status.Compiling_current then
          begin
            AddFormatParamStr(ShrinkPath(SmartPath(Status.Currentsourcepath+Status.CurrentSource),
              MaxFileNameSize - Length(msg_compilingfile)));
            StatusS:=FormatStrF(msg_compilingfile,FormatParams);
          end
        else
          begin
            if Status.CurrentSource='' then
              StatusS:=''
            else
              begin
                StatusS:=ShrinkPath(SmartPath(DirAndNameOf(Status.Currentsourcepath+Status.CurrentSource)),
                  MaxFileNameSize-Length(msg_loadingunit));
                AddFormatParamStr(StatusS);
                StatusS:=FormatStrF(msg_loadingunit,FormatParams);
              end;
          end;
        KeyS:=msg_hint_pressesctocancel;
      end;
    cpLinking   :
      begin
        ClearFormatParams;
        AddFormatParamStr(ShrinkPath(ExeFile,
          MaxFileNameSize-Length(msg_linkingfile)));
        StatusS:=FormatStrF(msg_linkingfile,FormatParams);
        KeyS:=msg_hint_pleasewait;
      end;
    cpDone      :
      begin
        StatusS:=msg_compiledone;
        KeyS:=msg_hint_compilesuccessfulpressenter;
      end;
    cpFailed    :
      begin
        StatusS:=msg_failedtocompile;
        KeyS:=msg_hint_compilefailed;
      end;
    cpAborted    :
      begin
        StatusS:=msg_compilationaborted;
        KeyS:=msg_hint_compileaborted;
      end;
  end;
  ClearFormatParams;
  AddFormatParamStr(ShrinkPath(SmartPath(MainFile),
    MaxFileNameSize-Length('Main file: %s')));
  AddFormatParamStr(StatusS);
  AddFormatParamStr(KillTilde(TargetSwitches^.ItemName(TargetSwitches^.GetCurrSel)));
  AddFormatParamInt(Status.CurrentLine);
  AddFormatParamInt(MemAvail div 1024);
  AddFormatParamInt(Status.CompiledLines);
  AddFormatParamInt(Status.ErrorCount);
  ST^.SetText(
   FormatStrF(
    'Main file: %s'#13+
    '%s'+#13#13+
    'Target: %12s    '+     'Line number: %7d'+#13+
    'Free memory: %6dK    '+'Total lines: %7d'+#13+
    'Total errors: %5d',
   FormatParams)
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
  var
     event : tevent;
begin
  GetKeyEvent(Event);
  if (Event.What=evKeyDown) and (Event.KeyCode=kbEsc) then
    begin
       CompilationPhase:=cpAborted;
       { update info messages }
       if assigned(CompilerStatusDialog) then
        begin
{$ifdef redircompiler}
          RedirDisableAll;
{$endif}
          CompilerStatusDialog^.Update;
{$ifdef redircompiler}
          RedirEnableAll;
{$endif}
        end;
       CompilerStatus:=true;
       exit;
    end;
{ only display line info every 100 lines, ofcourse all other messages
  will be displayed directly }
  if (status.currentline mod 100=0) then
   begin
     { update info messages }
{$ifdef redircompiler}
          RedirDisableAll;
{$endif}
     if assigned(CompilerStatusDialog) then
      CompilerStatusDialog^.Update;
{$ifdef redircompiler}
          RedirEnableAll;
{$endif}
     { update memory usage }
     { HeapView^.Update; }
   end;
  CompilerStatus:=false;
end;

procedure CompilerStop; {$ifndef FPC}far;{$endif}
begin
{$ifndef GABOR}
  if StopJmpValid then
    Longjmp(StopJmp,1)
  else
    Halt(1);
{$endif}
end;

Function  CompilerGetNamedFileTime(const filename : string) : Longint; {$ifndef FPC}far;{$endif}
var t: longint;
    W: PSourceWindow;
begin
  W:=EditorWindowFile(FExpand(filename));
  if Assigned(W) and (W^.Editor^.GetModified) then
    t:=Now
  else
    t:=def_getnamedfiletime(filename);
  CompilerGetNamedFileTime:=t;
end;

{$ifdef COMPILER_1_0}
function CompilerOpenInputFile(const filename: string): pinputfile; {$ifndef FPC}far;{$endif}
var f: pinputfile;
    W: PSourceWindow;
begin
  W:=EditorWindowFile(FExpand(filename));
  if Assigned(W) and (W^.Editor^.GetModified) then
    f:=new(PFPInputFile, Init(W^.Editor))
  else
    f:={$ifndef GABOR}def_openinputfile(filename){$else}nil{$endif};
  if assigned(W) then
    W^.Editor^.CompileStamp:=CompileStamp;
  CompilerOpenInputFile:=f;
end;
{$else COMPILER_1_0}
function CompilerOpenInputFile(const filename: string): tinputfile; {$ifndef FPC}far;{$endif}
var f: tinputfile;
    W: PSourceWindow;
begin
  W:=EditorWindowFile(FExpand(filename));
  if Assigned(W) and (W^.Editor^.GetModified) then
    f:=TFPInputFile.Create(W^.Editor)
  else
    f:={$ifndef GABOR}def_openinputfile(filename){$else}nil{$endif};
  if assigned(W) then
    W^.Editor^.CompileStamp:=CompileStamp;
  CompilerOpenInputFile:=f;
end;
{$endif COMPILER_1_0}

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
{$ifdef redircompiler}
     RedirDisableAll;
{$endif}

     if not CompilerMessageWindow^.GetState(sfVisible) then
       CompilerMessageWindow^.Show;
     if Desktop^.First<>PView(CompilerMessageWindow) then
       CompilerMessageWindow^.MakeFirst;
     CompilerMessageWindow^.AddMessage(Level,S,status.currentsourcepath+status.currentsource,
       status.currentline,status.currentcolumn);
     { update info messages }
     if assigned(CompilerStatusDialog) then
      CompilerStatusDialog^.Update;
{$ifdef DEBUG}
 {$ifndef NODEBUG}
//     def_gdb_stop(level);
 {$endif}
{$endif DEBUG}
{$ifdef redircompiler}
      RedirEnableAll;
{$endif}
     { update memory usage }
     { HeapView^.Update; }
   end;
{$ifdef TEMPHEAP}
  switch_to_temp_heap;
{$endif TEMPHEAP}
end;


{****************************************************************************
                                 DoCompile
****************************************************************************}

{ This function must return '' if
  "Options|Directories|Exe and PPU directory" is empty }
function GetExePath: string;
var Path: string;
    I: Sw_integer;
begin
  Path:='';
  if DirectorySwitches<>nil then
    with DirectorySwitches^ do
    for I:=0 to ItemCount-1 do
      begin
        if ItemParam(I)='-FE' then
          begin
            Path:=GetStringItem(I);
            Break;
          end;
      end;
  if Path<>'' then
    GetExePath:=CompleteDir(FExpand(Path))
  else
    GetExePath:='';
end;

function GetMainFile(Mode: TCompileMode): string;
var FileName: string;
    P : PSourceWindow;
begin
  P:=Message(Desktop,evBroadcast,cmSearchWindow,nil);
  if (PrimaryFileMain='') and (P=nil) then
    FileName:='' { nothing to compile }
  else
    begin
      if (PrimaryFileMain<>'') and (Mode<>cCompile) then
        FileName:=PrimaryFileMain
      else if assigned(P) then
        begin
            FileName:=P^.Editor^.FileName;
            if FileName='' then
              P^.Editor^.SaveAsk(true);
            FileName:=P^.Editor^.FileName;
        end
      else
        FileName:='';
    end;
  If (FileName<>'') then
    FileName:=FixFileName(FExpand(FileName));
  GetMainFile:=FileName;
end;

procedure ResetErrorMessages;
  procedure ResetErrorLine(P: PView); {$ifndef FPC}far;{$endif}
  begin
    if assigned(P) and
       (TypeOf(P^)=TypeOf(TSourceWindow)) then
       PSourceWindow(P)^.Editor^.SetErrorMessage('');
  end;
begin
  Desktop^.ForEach(@ResetErrorLine);
end;


procedure DoCompile(Mode: TCompileMode);

  function IsExitEvent(E: TEvent): boolean;
  begin
    { following suggestion by Harsha Senanayake }
    IsExitEvent:=(E.What=evKeyDown);
  end;

var
  s,FileName: string;
  ErrFile : Text;
  MustRestartDebugger,
  StoreStopJumpValid : boolean;
  StoreStopJmp : Jmp_buf;
  JmpRet,Error,LinkErrorCount : longint;
  E : TEvent;
  DummyView: PView;
  PPasFile : string[64];
begin
  AskRecompileIfModifiedFlag:=true;
{ Get FileName }
  FileName:=GetMainFile(Mode);
  if FileName='' then
    begin
      ErrorBox(msg_nothingtocompile,nil);
      Exit;
    end else
  { THis is not longer necessary as unsaved files are loaded from a memorystream,
    and with the file as primaryfile set it is already incompatible with itself
   if FileName='*' then
    begin
      ErrorBox(msg_cantcompileunsavedfile,nil);
      Exit;
    end; }
  PushStatus('Beginning compilation...');
{ Show Compiler Messages Window }
{  if not CompilerMessageWindow^.GetState(sfVisible) then
   CompilerMessageWindow^.Show;
  CompilerMessageWindow^.MakeFirst;}
  CompilerMessageWindow^.ClearMessages;
  { Tell why we compile }
  NeedRecompile(Mode,true);

  MainFile:=FileName;
  SetStatus('Writing switches to file...');
  WriteSwitches(SwitchesPath);
  { leaving open browsers leads to crashes !! (PM) }
  SetStatus('Preparing symbol info...');
  CloseAllBrowsers;
  if ((DesktopFileFlags and dfSymbolInformation)<>0) then
    WriteSymbolsFile(BrowserName);
{  MainFile:=FixFileName(FExpand(FileName));}
  SetStatus('Preparing to compile...'+NameOf(MainFile));
  If GetEXEPath<>'' then
    EXEFile:=FixFileName(GetEXEPath+NameOf(MainFile)+ExeExt)
  else
    EXEFile:=DirOf(MainFile)+NameOf(MainFile)+ExeExt;
{ Reset }
  CtrlBreakHit:=false;
{ Create Compiler Status Dialog }
  CompilationPhase:=cpCompiling;
  New(CompilerStatusDialog, Init);
  CompilerStatusDialog^.SetState(sfModal,true);
  { disable window closing }
  CompilerStatusDialog^.Flags:=CompilerStatusDialog^.Flags and not wfclose;
  Application^.Insert(CompilerStatusDialog);
  CompilerStatusDialog^.Update;
{ hook compiler output }
{$ifdef TP}
  do_status:=CompilerStatus;
  do_stop:=CompilerStop;
  do_comment:=CompilerComment;
  {$ifndef GABOR}do_openinputfile:=CompilerOpenInputFile;{$endif}
  do_getnamedfiletime:=CompilerGetNamedFileTime;
{$else not TP}
  do_status:=@CompilerStatus;
  do_stop:=@CompilerStop;
  do_comment:=@CompilerComment;
  do_openinputfile:=@CompilerOpenInputFile;
  do_getnamedfiletime:=@CompilerGetNamedFileTime;
{$endif TP}
  do_initsymbolinfo:={$ifdef fpc}@{$endif}InitBrowserCol;
  do_donesymbolinfo:={$ifdef fpc}@{$endif}DoneBrowserCol;
  do_extractsymbolinfo:={$ifdef fpc}@{$endif}CreateBrowserCol;
{ Compile ! }
{$ifdef redircompiler}
  ChangeRedirOut(FPOutFileName,false);
  ChangeRedirError(FPErrFileName,false);
{$endif}
{$ifdef TEMPHEAP}
  split_heap;
  switch_to_temp_heap;
{$endif TEMPHEAP}
  { insert "" around name so that spaces are allowed }
  { only supported in compiler after 2000/01/14 PM   }
  if pos(' ',FileName)>0 then
    FileName:='"'+FileName+'"';
  if mode=cBuild then
    FileName:='-B '+FileName;
  { tokens are created and distroed by compiler.compile !! PM }
  DoneTokens;
{$ifdef COMPILER_1_0}
  PPasFile:='ppas'+source_os.scriptext;
{$else COMPILER_1_0}
  PPasFile:='ppas'+source_info.scriptext;
{$endif COMPILER_1_0}
  WUtils.DeleteFile(GetExePath+PpasFile);
  SetStatus('Compiling...');
{$ifndef GABOR}
  StoreStopJumpValid:=StopJmpValid;
  StoreStopJmp:=StopJmp;
  StopJmpValid:=true;
  JmpRet:=SetJmp(StopJmp);
  if JmpRet=0 then
    begin
      inc(CompileStamp);
      ResetErrorMessages;
{$ifndef NODEBUG}
      MustRestartDebugger:=false;
      if assigned(Debugger) then
        if Debugger^.HasExe then
          begin
            Debugger^.Reset;
            MustRestartDebugger:=true;
          end;
{$endif NODEBUG}
      LastCompileTime := cardinal(Now);
      FpIntF.Compile(FileName,SwitchesPath);
      SetStatus('Finished compiling...');
    end
  else
    begin
      Inc(status.errorCount);
{$ifdef HasSignal}
      Case JmpRet of
        SIGINT : s := 'Interrupted by Ctrl-C';
        SIGILL : s := 'Illegal instruction';
        SIGSEGV : s := 'Signal Segmentation violation';
        SIGFPE : s:='Floating point signal';
        else
          s:='Undetermined signal '+inttostr(JmpRet);
      end;
      CompilerMessageWindow^.AddMessage(V_error,s+' during compilation','',0,0);
{$endif HasSignal}
      CompilerMessageWindow^.AddMessage(V_error,'Long jumped out of compilation...','',0,0);
      SetStatus('Long jumped out of compilation...');
    end;
  StopJmpValid:=StoreStopJumpValid;
  StopJmp:=StoreStopJmp;
{$endif}
  { tokens are created and distroyed by compiler.compile !! PM }
  InitTokens;
  if LinkAfter and
     ExistsFile(GetExePath+PpasFile) and
     (CompilationPhase<>cpAborted) and
     (status.errorCount=0) then
    begin
       CompilationPhase:=cpLinking;
       CompilerStatusDialog^.Update;
       SetStatus('Assembling and/or linking...');
{$ifndef redircompiler}
       { At least here we want to catch output
        of batch file PM }
       ChangeRedirOut(FPOutFileName,false);
       ChangeRedirError(FPErrFileName,false);
{$endif}
{$ifdef Unix}
       Shell(GetExePath+PpasFile);
       Error:=LinuxError;
{$else}
       DosExecute(GetEnv('COMSPEC'),'/C '+GetExePath+PpasFile);
       Error:=DosError;
{$endif}
       SetStatus('Finished linking...');
       RestoreRedirOut;
       RestoreRedirError;
       if Error<>0 then
         Inc(status.errorCount);
       if Status.IsExe and not Status.IsLibrary and not ExistsFile(EXEFile) then
         begin
           Inc(status.errorCount);
           ClearFormatParams; AddFormatParamStr(ExeFile);
           CompilerMessageWindow^.AddMessage(V_error,FormatStrF(msg_couldnotcreatefile,FormatParams),'',0,0);
         {$I-}
           Assign(ErrFile,FPErrFileName);
           Reset(ErrFile);
           if EatIO<>0 then
             ErrorBox(FormatStrStr(msg_cantopenfile,FPErrFileName),nil)
           else
           begin
             LinkErrorCount:=0;
             While not eof(ErrFile) and (LinkErrorCount<25) do
               begin
                 readln(ErrFile,s);
                 CompilerMessageWindow^.AddMessage(V_error,s,'',0,0);
                 inc(LinkErrorCount);
               end;
             if not eof(ErrFile) then
             begin
               ClearFormatParams; AddFormatParamStr(FPErrFileName);
               CompilerMessageWindow^.AddMessage(V_error,
                 FormatStrF(msg_therearemoreerrorsinfile,FormatParams),'',0,0);
             end;

             Close(ErrFile);
           end;
           EatIO;
         {$I+}
         end
       else if error=0 then
         WUtils.DeleteFile(GetExePath+PpasFile);
    end;
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
{$ifdef redircompiler}
  RestoreRedirOut;
  RestoreRedirError;
{$endif}
  PopStatus;
{ Set end status }
  if not (CompilationPhase in [cpAborted,cpFailed]) then
    if (status.errorCount=0) then
      CompilationPhase:=cpDone
    else
      CompilationPhase:=cpFailed;
{ Show end status }
  { reenable window closing }
  CompilerStatusDialog^.Flags:=CompilerStatusDialog^.Flags or wfclose;
  CompilerStatusDialog^.Update;
  CompilerStatusDialog^.ReDraw;
  CompilerStatusDialog^.SetState(sfModal,false);
  if ((CompilationPhase in[cpAborted,cpDone,cpFailed]) or (ShowStatusOnError)) and (Mode<>cRun) then
   repeat
     CompilerStatusDialog^.GetEvent(E);
     if IsExitEvent(E)=false then
      CompilerStatusDialog^.HandleEvent(E);
   until IsExitEvent(E) or not assigned(CompilerStatusDialog);
  if assigned(CompilerStatusDialog) then
    begin
      Application^.Delete(CompilerStatusDialog);
      Dispose(CompilerStatusDialog, Done);
    end;
  CompilerStatusDialog:=nil;
{ end compilation returns true if the messagewindow should be removed }
  if CompilationPhase=cpDone then
   begin
     CompilerMessageWindow^.Hide;
     { This is the last compiled main file }
     PrevMainFile:=MainFile;
     MainHasDebugInfo:=DebugInfoSwitches^.GetCurrSelParam<>'-';
   end;
{ Update the app }
  Message(Application,evCommand,cmUpdate,nil);
{$ifdef TEMPHEAP}
  releasetempheap;
  unsplit_heap;
{$endif TEMPHEAP}
  DummyView:=Desktop^.First;
  while (DummyView<>nil) and (DummyView^.GetState(sfVisible)=false) do
  begin
    DummyView:=DummyView^.NextView;
  end;
  with DummyView^ do
   if GetState(sfVisible) then
    begin
      SetState(sfSelected,false);
      SetState(sfSelected,true);
    end;
  if Assigned(CompilerMessageWindow) then
    with CompilerMessageWindow^ do
      begin
        if GetState(sfVisible) then
          begin
            SetState(sfSelected,false);
            SetState(sfSelected,true);
          end;
        if (status.errorCount>0) then
          MsgLB^.SelectFirstError;
      end;
  { ^^^ we need this trick to reactivate the desktop }
  EditorModified:=false;
{$ifndef NODEBUG}
  if MustRestartDebugger then
    InitDebugger;
{$endif NODEBUG}
  { In case we have something that the compiler touched }
  AskToReloadAllModifiedFiles;
  { Try to read Browser info in again if compilation failure !! }
  if Not Assigned(Modules) and (CompilationPhase<>cpDone) and
     ((DesktopFileFlags and dfSymbolInformation)<>0) then
    ReadSymbolsFile(BrowserName);
end;

function NeedRecompile(Mode :TCompileMode; verbose : boolean): boolean;
var Need: boolean;
    I: sw_integer;
    SF: PSourceFile;
    SourceTime,PPUTime,ObjTime: longint;
    W: PSourceWindow;
begin
  if Assigned(SourceFiles)=false then
     Need:={(EditorModified=true)}true
  else
    begin
      Need:=(PrevMainFile<>GetMainFile(Mode)) and (PrevMainFile<>'');
      if Need then
        begin
          if verbose then
          begin
            ClearFormatParams; AddFormatParamStr(GetMainFile(Mode));
            CompilerMessageWindow^.AddMessage(V_info,
              FormatStrF(msg_firstcompilationof,FormatParams),
              '',0,0);
          end;
        end
      else
        for I:=0 to SourceFiles^.Count-1 do
          begin
            SF:=SourceFiles^.At(I);
            SourceTime:=GetFileTime(SF^.GetSourceFileName);
            PPUTime:=GetFileTime(SF^.GetPPUFileName);
            ObjTime:=GetFileTime(SF^.GetObjFileName);
{            writeln('S: ',SF^.GetSourceFileName,' - ',SourceTime);
            writeln('P: ',SF^.GetPPUFileName,' - ',PPUTime);
            writeln('O: ',SF^.GetObjFileName,' - ',ObjTime);
            writeln('------');}
            { some units don't generate object files }
            W:=EditorWindowFile(SF^.GetSourceFileName);
            if (SourceTime<>-1) then
              if ((SourceTime>PPUTime) or
                 ((SourceTime>ObjTime) and
                 (ObjTime<>-1))) or
                 (assigned(W) and (W^.Editor^.CompileStamp<0)) then
                begin
                  Need:=true;
                  if verbose then
                  begin
                    ClearFormatParams; AddFormatParamStr(SF^.GetSourceFileName);
                    CompilerMessageWindow^.AddMessage(V_info,
                      FormatStrF(msg_recompilingbecauseof,FormatParams),
                      SF^.GetSourceFileName,1,1);
                  end;
                  Break;
                end;
          end;
{      writeln('Need?', Need); system.readln;}
    end;

  NeedRecompile:=Need;
end;


{$ifdef COMPILER_1_0}
constructor TFPInputFile.Init(AEditor: PFileEditor);
begin
  if not Assigned(AEditor) then Fail;
  if inherited Init(AEditor^.FileName)=false then
    Fail;
  Editor:=AEditor;
end;
{$else COMPILER_1_0}
constructor TFPInputFile.Create(AEditor: PFileEditor);
begin
  if not Assigned(AEditor) then Fail;
  if inherited Create(AEditor^.FileName)=nil then
    Fail;
  Editor:=AEditor;
end;
{$endif COMPILER_1_0}

function TFPInputFile.fileopen(const filename: string): boolean;
var OK: boolean;
begin
  S:=New(PMemoryStream, Init(0,0));
  OK:=Assigned(S) and (S^.Status=stOK);
  if OK then OK:=Editor^.SaveToStream(S);
  if OK then
    S^.Seek(0)
  else
    begin
      if Assigned(S) then Dispose(S, Done);
      S:=nil;
    end;
  fileopen:=OK;
end;

function TFPInputFile.fileseek(pos: longint): boolean;
var OK: boolean;
begin
  OK:=assigned(S);
  if OK then
  begin
    S^.Reset;
    S^.Seek(pos);
    OK:=(S^.Status=stOK);
  end;
  fileseek:=OK;
end;

function TFPInputFile.fileread(var databuf; maxsize: longint): longint;
var
    size: longint;
begin
  if not assigned(S) then size:=0 else
  begin
    size:=min(maxsize,(S^.GetSize-S^.GetPos));
    S^.Read(databuf,size);
    if S^.Status<>stOK then size:=0;
  end;
  fileread:=size;
end;

function TFPInputFile.fileeof: boolean;
var EOF: boolean;
begin
  EOF:=not assigned(S);
  if not EOF then
    EOF:=(S^.Status<>stOK) or (S^.GetPos=S^.GetSize);
  fileeof:=EOF;
end;

function TFPInputFile.fileclose: boolean;
var OK: boolean;
begin
  OK:=assigned(S);
  if OK then
  begin
    S^.Reset;
    Dispose(S, Done);
    S:=nil;
    OK:=true;
  end;
  fileclose:=OK;
end;

procedure RegisterFPCompile;
begin
{$ifndef NOOBJREG}
  RegisterType(RCompilerMessageListBox);
  RegisterType(RCompilerMessageWindow);
{$endif}
end;


end.
{
  $Log$
  Revision 1.7  2002-03-20 14:48:27  pierre
   * moved StopJmp buffer to fpcatch unit

  Revision 1.6  2001/11/13 01:58:34  carl
  * Range check error fix

  Revision 1.5  2001/10/03 10:21:43  pierre
   fix for bug 1487

  Revision 1.4  2001/09/18 11:33:26  pierre
   * fix bug 1604

  Revision 1.3  2001/09/12 09:25:01  pierre
   * fix bug 1585

  Revision 1.2  2001/08/05 02:01:47  peter
    * FVISION define to compile with fvision units

  Revision 1.1  2001/08/04 11:30:22  peter
    * ide works now with both compiler versions

  Revision 1.1.2.24  2001/06/07 16:41:12  jonas
    *  updated for stricter checking of @ for procvars

  Revision 1.1.2.23  2001/05/09 15:42:08  pierre
   Reset debugger before recompilation

  Revision 1.1.2.22  2001/03/15 17:07:33  pierre
   * avoid scrolling in Compiler Dialog window

  Revision 1.1.2.21  2001/02/19 10:38:12  pierre
   * completely stop the debugger while compiling

  Revision 1.1.2.20  2001/02/13 16:04:01  pierre
   * fixes for bugs 1280

  Revision 1.1.2.19  2001/02/13 12:05:10  pierre
   * fix for bug 1379

  Revision 1.1.2.18  2000/12/30 22:52:27  peter
    * check modified while in debug mode. But placed it between a
      conditional again as it reports also if the file was already modified
      before the first compile.
    * remove unsaved file checks when compiling without primary file so it
      works the same as with a primary file set.

  Revision 1.1.2.17  2000/12/23 23:07:57  florian
    * better message for unsaved files

  Revision 1.1.2.16  2000/11/29 00:54:44  pierre
   + preserve window number and save special windows

  Revision 1.1.2.15  2000/11/27 11:44:05  pierre
   * remove the Can't open fp__.err problem

  Revision 1.1.2.14  2000/11/23 13:00:47  pierre
   + better infos while compiling

  Revision 1.1.2.13  2000/11/19 00:23:32  pierre
   Task 23: nicer error message when trying to run unit or library

  Revision 1.1.2.12  2000/11/16 23:06:30  pierre
  * correct handling of Compile/Make if primary file is set

  Revision 1.1.2.11  2000/11/14 17:40:02  pierre
   * fix the linking problem in another directory

  Revision 1.1.2.10  2000/11/14 09:23:55  marco
   * Second batch

  Revision 1.1.2.9  2000/11/06 16:55:48  pierre
   * fix failure to recompile when file changed

  Revision 1.1.2.8  2000/10/31 07:51:58  pierre
   * recover gracefully if compiler generates a signal

  Revision 1.1.2.7  2000/10/18 21:53:26  pierre
   * several Gabor fixes

  Revision 1.1.2.6  2000/10/09 16:28:24  pierre
   * several linux enhancements

  Revision 1.1.2.5  2000/10/03 16:15:57  pierre
   * Use LongJmp in CompilerStop

  Revision 1.1.2.4  2000/08/16 18:46:14  peter
   [*] double clicking on a droplistbox caused GPF (due to invalid recurson)
   [*] Make, Build now possible even in Compiler Messages Window
   [+] when started in a new dir the IDE now ask whether to create a local
       config, or to use the one located in the IDE dir

  Revision 1.1.2.3  2000/08/15 03:40:53  peter
   [*] no more fatal exits when the IDE can't find the error file (containing
       the redirected assembler/linker output) after compilation
   [*] hidden windows are now added always at the end of the Window List
   [*] TINIFile parsed entries encapsulated in string delimiters incorrectly
   [*] selection was incorrectly adjusted when typing in overwrite mode
   [*] the line wasn't expanded when it's end was reached in overw. mode
   [*] the IDE now tries to locate source files also in the user specified
       unit dirs (for ex. as a response to 'Open at cursor' (Ctrl+Enter) )
   [*] 'Open at cursor' is now aware of the extension (if specified)

  Revision 1.1.2.2  2000/08/10 07:10:37  michael
  * 'Auto save editor files' option did the opposite than expected, due
    to a typo in FPIDE.PAS
  + saving of source files before compilation is no longer neccessary.
    When a modified editor file is involved in the compilation, then the
    IDE saves it's contents to a memory stream and passes this to the
    compiler (instead of the file on the disk)

  Revision 1.1.2.1  2000/07/18 05:50:22  michael
  + Merged Gabors fixes

  Revision 1.1  2000/07/13 09:48:34  michael
  + Initial import

  Revision 1.60  2000/06/22 09:07:11  pierre
   * Gabor changes: see fixes.txt

  Revision 1.59  2000/06/16 08:50:40  pierre
   + new bunch of Gabor's changes

  Revision 1.58  2000/05/29 10:44:56  pierre
   + New bunch of Gabor's changes: see fixes.txt

  Revision 1.57  2000/05/02 08:42:27  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.56  2000/04/25 08:42:32  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.55  2000/04/18 11:42:36  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.54  2000/03/23 22:23:21  pierre
   + Use PushStatus in ParseUserScreen

  Revision 1.53  2000/03/21 23:33:18  pierre
   adapted to wcedit addition by Gabor

  Revision 1.52  2000/03/08 16:48:07  pierre
   + Read BackTrace from UseScreen

  Revision 1.51  2000/03/07 21:54:26  pierre
   + ParseUserScreen

  Revision 1.50  2000/02/06 23:41:42  pierre
   +  TCompilerMessageListBox.SelectFirstError

  Revision 1.49  2000/01/25 00:26:35  pierre
   + Browser info saving

  Revision 1.48  2000/01/14 15:38:28  pierre
    + support for long filenames with spaces for compilation
    * avoid too long linker error output

  Revision 1.47  2000/01/03 11:38:33  michael
  Changes from Gabor

  Revision 1.46  1999/12/01 17:08:19  pierre
   * GetFileTime moved to wutils unit

  Revision 1.45  1999/11/22 15:58:40  pierre
   * fix for web bug 633

  Revision 1.44  1999/11/21 01:44:34  pierre
   + Use def_gdb_stop for easy GDB debugging

  Revision 1.43  1999/11/18 13:49:56  pierre
   + use IsExe var to know if we need to call ppas

  Revision 1.42  1999/11/10 17:20:41  pierre
   * Use fpredir.dosexecute

  Revision 1.41  1999/10/25 16:34:19  pierre
    * some units have no object files
      led to wrong NeedRecompile result

  Revision 1.40  1999/09/20 15:36:38  pierre
   * adapted to new tokens unit

  Revision 1.39  1999/09/16 14:34:57  pierre
    + TBreakpoint and TWatch registering
    + WatchesCollection and BreakpointsCollection stored in desk file
    * Syntax highlighting was broken

  Revision 1.38  1999/09/13 16:24:43  peter
    + clock
    * backspace unident like tp7

  Revision 1.37  1999/09/09 14:19:16  pierre
   * status should not be present in TCompilerMessage.GetText

  Revision 1.36  1999/09/07 11:32:13  pierre
    * fix for Linux ./ prepended to ppas.sh
    * Build add '-B' option
    * if linkAfter is set, get errors from linker
      by redirecting files

  Revision 1.35  1999/08/22 22:27:30  pierre
   * not ppas call on compile failure

  Revision 1.34  1999/08/16 18:25:13  peter
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

  Revision 1.33  1999/08/03 20:22:26  peter
    + TTab acts now on Ctrl+Tab and Ctrl+Shift+Tab...
    + Desktop saving should work now
       - History saved
       - Clipboard content saved
       - Desktop saved
       - Symbol info saved
    * syntax-highlight bug fixed, which compared special keywords case sensitive
      (for ex. 'asm' caused asm-highlighting, while 'ASM' didn't)
    * with 'whole words only' set, the editor didn't found occourences of the
      searched text, if the text appeared previously in the same line, but didn't
      satisfied the 'whole-word' condition
    * ^QB jumped to (SelStart.X,SelEnd.X) instead of (SelStart.X,SelStart.Y)
      (ie. the beginning of the selection)
    * when started typing in a new line, but not at the start (X=0) of it,
      the editor inserted the text one character more to left as it should...
    * TCodeEditor.HideSelection (Ctrl-K+H) didn't update the screen
    * Shift shouldn't cause so much trouble in TCodeEditor now...
    * Syntax highlight had problems recognizing a special symbol if it was
      prefixed by another symbol character in the source text
    * Auto-save also occours at Dos shell, Tool execution, etc. now...

  Revision 1.32  1999/07/12 13:14:13  pierre
    * LineEnd bug corrected, now goes end of text even if selected
    + Until Return for debugger
    + Code for Quit inside GDB Window

  Revision 1.31  1999/06/28 19:32:17  peter
    * fixes from gabor

  Revision 1.30  1999/06/28 15:59:04  pierre
   * View Linking stage if external linking

  Revision 1.29  1999/06/28 12:39:14  pierre
   + close all browsers before compiling

  Revision 1.28  1999/06/21 23:42:16  pierre
   + LinkAfter and Esc to abort support added

  Revision 1.27  1999/05/22 13:44:29  peter
    * fixed couple of bugs

  Revision 1.26  1999/05/02 14:29:35  peter
    * fixed typo disableredir -> redirdisable

  Revision 1.25  1999/04/29 22:58:09  pierre
   + disabling of redirction in compiler dialogs

  Revision 1.24  1999/04/29 09:36:11  peter
    * fixed hotkeys with Compiler switches
    * fixed compiler status dialog
    * Run shows again the output

  Revision 1.23  1999/04/07 21:55:43  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.22  1999/04/01 10:27:07  pierre
   + file(line) in start of message added

  Revision 1.21  1999/04/01 10:15:17  pierre
    * CurrSt,InfoSt and LineSt were not disposed correctly in done
    * TComiplerMessage destructor first calls SetCompileShow(false)
      to get proper cleaning up

  Revision 1.20  1999/03/23 16:16:38  peter
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
