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
unit FPCompile;

interface

{ don't redir under linux, because all stdout (also from the ide!) will
  then be redired (PFV) }
{ this should work now correctly because
  RedirDisableAll and RedirEnableAll function are added in fpredir (PM) }

{ $define VERBOSETXT}

uses
  Objects,
  Drivers,Views,Dialogs,
  WViews,
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
    private
      {CompileShowed : boolean;}
      Mode   : TCompileMode;
      MsgLB  : PCompilerMessageListBox;
      {CurrST,
      InfoST : PColorStaticText;}
    end;

    PCompilerStatusDialog = ^TCompilerStatusDialog;
    TCompilerStatusDialog = object(TCenterDialog)
      ST    : PAdvancedStaticText;
      KeyST : PColorStaticText;
      constructor Init;
      procedure   Update;
    end;

const
    CompilerMessageWindow : PCompilerMessageWindow  = nil;
    CompilerStatusDialog  : PCompilerStatusDialog = nil;

procedure DoCompile(Mode: TCompileMode);
function  NeedRecompile(verbose : boolean): boolean;
procedure ParseUserScreen;

procedure RegisterFPCompile;


implementation

uses
{$ifdef linux}
  Linux,
{$endif}
  Dos,Video,
  App,Commands,tokens,
  CompHook, Compiler, systems, browcol,
  WUtils,WEditor,
  FPRedir,FPDesk,FPUsrScr,
  FPIde,FPConst,FPVars,FPUtils,FPIntf,FPSwitch;

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

    procedure SearchBackTrace;
      var AText,ModuleName,st : String;
          p2,row : longint;
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
                Val(st,row);
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
            Val(Copy(st,1,pos(',',st)-1),row);
            st:=Copy(st,Pos(',',st)+1,255);
            Val(Copy(st,1,pos(')',st)-1),col);
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
  inherited Init(R,'Compiler Messages',{SearchFreeWindowNo}wnNoNumber);
  HelpCtx:=hcMessagesWindow;

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
  inherited Init(R, 'Compiling  ('+KillTilde(SwitchesModeName[SwitchesMode])+' mode)');
  GetExtent(R); R.B.Y:=11;
  R.Grow(-3,-2);
  New(ST, Init(R, ''));
  Insert(ST);
  GetExtent(R); R.B.Y:=11;
  R.Grow(-1,-1); R.A.Y:=R.B.Y-1;
  New(KeyST, Init(R, '', Blue*16+White+longint($80+Blue*16+White)*256));
  Insert(KeyST);
end;


procedure TCompilerStatusDialog.Update;
const
  CtrlBS   = 'Press ESC to cancel';
  SuccessS = 'Compile successful: ~Press Enter~';
  FailS    = 'Compile failed';
  AbortS   = 'Compile aborted';
var
  StatusS,KeyS: string;
begin
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
  case CompilationPhase of
    cpCompiling :
      begin
        StatusS:='Compiling '+SmartPath(Status.CurrentSource);
        KeyS:=CtrlBS;
      end;
    cpLinking   :
      begin
        StatusS:='Linking '+ExeFile;
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
    cpAborted    :
      begin
        StatusS:='Compilation aborted...';
        KeyS:=AbortS;
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
{$ifdef redircompiler}
     RedirDisableAll;
{$endif}

     CompilerMessageWindow^.AddMessage(Level,S,status.currentsourcepath+status.currentsource,
       status.currentline,status.currentcolumn);
     { update info messages }
     if assigned(CompilerStatusDialog) then
      CompilerStatusDialog^.Update;
{$ifdef DEBUG}
 {$ifndef NODEBUG}
     def_gdb_stop(level);
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

function GetMainFile: string;
var FileName: string;
    P : PSourceWindow;
begin
  P:=Message(Desktop,evBroadcast,cmSearchWindow,nil);
  if (PrimaryFileMain='') and (P=nil) then
    FileName:='' { nothing to compile }
  else
    begin
      if PrimaryFileMain<>'' then
        FileName:=PrimaryFileMain
      else
        begin
          if P^.Editor^.Modified and (not P^.Editor^.Save) then
            FileName:='*' { file not saved }
          else
            FileName:=P^.Editor^.FileName;
        end;
    end;
  FileName:=FixFileName(FExpand(FileName));
  GetMainFile:=FileName;
end;

procedure DoCompile(Mode: TCompileMode);

  function IsExitEvent(E: TEvent): boolean;
  begin
    IsExitEvent:=(E.What=evKeyDown) and
                 ((E.KeyCode=kbEnter) or (E.KeyCode=kbEsc)) or
                 ((E.What=evCommand) and (E.command=cmClose));
  end;

var
  s,FileName: string;
  ErrFile : Text;
  Error,LinkErrorCount : longint;
  E : TEvent;
const
  PpasFile = 'ppas';

begin
{ Get FileName }
  FileName:=GetMainFile;
  if FileName='' then
    begin
      ErrorBox('Oooops, nothing to compile.',nil);
      Exit;
    end else
  if FileName='*' then
    begin
      ErrorBox('Can''t compile unsaved file.',nil);
      Exit;
    end;
{ Show Compiler Messages Window }
  if not CompilerMessageWindow^.GetState(sfVisible) then
   CompilerMessageWindow^.Show;
  CompilerMessageWindow^.MakeFirst;
  CompilerMessageWindow^.ClearMessages;
  { Tell why we compile }
  NeedRecompile(true);

  MainFile:=FileName;
  WriteSwitches(SwitchesPath);
  { leaving open browsers leads to crashes !! (PM) }
  CloseAllBrowsers;
  if ((DesktopFileFlags and dfSymbolInformation)<>0) then
    WriteSymbolsFile(BrowserName);
{  MainFile:=FixFileName(FExpand(FileName));}
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
  Application^.Insert(CompilerStatusDialog);
  CompilerStatusDialog^.Update;
{ hook compiler output }
{$ifdef TP}
  do_status:=CompilerStatus;
  do_stop:=CompilerStop;
  do_comment:=CompilerComment;
{$else not TP}
  do_status:=@CompilerStatus;
  do_stop:=@CompilerStop;
  do_comment:=@CompilerComment;
{$endif TP}
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
  FpIntF.Compile(FileName);
  { tokens are created and distroed by compiler.compile !! PM }
  InitTokens;
  if LinkAfter and IsExe and
     (CompilationPhase<>cpAborted) and
     (status.errorCount=0) then
    begin
       CompilationPhase:=cpLinking;
       CompilerStatusDialog^.Update;
{$ifndef redircompiler}
       { At least here we want to catch output
        of batch file PM }
       ChangeRedirOut(FPOutFileName,false);
       ChangeRedirError(FPErrFileName,false);
{$endif}
{$ifdef linux}
       Shell(GetExePath+PpasFile+source_os.scriptext);
       Error:=LinuxError;
{$else}
       DosExecute(GetEnv('COMSPEC'),'/C '+GetExePath+PpasFile+source_os.scriptext);
       Error:=DosError;
{$endif}
{$ifndef redircompiler}
       RestoreRedirOut;
       RestoreRedirError;
{$endif}
       if Error<>0 then
         Inc(status.errorCount);
       if not ExistsFile(EXEFile) then
         begin
           Inc(status.errorCount);
           CompilerMessageWindow^.AddMessage(V_error,'could not create '+ExeFile,'',0,0);
           Assign(ErrFile,FPErrFileName);
           Reset(ErrFile);
           LinkErrorCount:=0;
           While not eof(ErrFile) and (LinkErrorCount<25) do
             begin
               readln(ErrFile,s);
               CompilerMessageWindow^.AddMessage(V_error,s,'',0,0);
               inc(LinkErrorCount);
             end;
           if not eof(ErrFile) then
             CompilerMessageWindow^.AddMessage(V_error,
               'There are more errors in file '+FPErrFileName,'',0,0);

           Close(ErrFile);
         end;
    end;
{$ifdef TEMPHEAP}
  switch_to_base_heap;
{$endif TEMPHEAP}
{$ifdef redircompiler}
  RestoreRedirOut;
  RestoreRedirError;
{$endif}
{ Set end status }
  if CompilationPhase<>cpAborted then
    if (status.errorCount=0) then
      CompilationPhase:=cpDone
    else
      CompilationPhase:=cpFailed;
{ Show end status }
  CompilerStatusDialog^.Update;
  CompilerStatusDialog^.SetState(sfModal,false);
  if ((CompilationPhase in[cpAborted,cpDone,cpFailed]) or (ShowStatusOnError)) and (Mode<>cRun) then
   repeat
     CompilerStatusDialog^.GetEvent(E);
     if IsExitEvent(E)=false then
      CompilerStatusDialog^.HandleEvent(E);
   until IsExitEvent(E);
  Application^.Delete(CompilerStatusDialog);
  Dispose(CompilerStatusDialog, Done);
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
  { Try to read Browser info in again if compilation failure !! }
  if Not Assigned(Modules) and (CompilationPhase<>cpDone) and
     ((DesktopFileFlags and dfSymbolInformation)<>0) then
    ReadSymbolsFile(BrowserName);
end;

function NeedRecompile(verbose : boolean): boolean;
var Need: boolean;
    I: sw_integer;
    SF: PSourceFile;
    SourceTime,PPUTime,ObjTime: longint;
begin
  if Assigned(SourceFiles)=false then
     Need:={(EditorModified=true)}true
  else
    begin
      Need:=(PrevMainFile<>GetMainFile) and (PrevMainFile<>'');
      if Need then
        begin
          if verbose then
            CompilerMessageWindow^.AddMessage(V_info,
              'First compilation of '+GetMainFile,
              '',0,0);
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
            if (SourceTime<>-1) then
              if (SourceTime>PPUTime) or
                 ((SourceTime>ObjTime) and
                 (ObjTime<>-1)) then
                begin
                  Need:=true;
                  if verbose then
                    CompilerMessageWindow^.AddMessage(V_info,
                      'Recompiling because of '+SF^.GetSourceFileName,
                      SF^.GetSourceFileName,1,1);
                  Break;
                end;
          end;
{      writeln('Need?', Need); system.readln;}
    end;

  NeedRecompile:=Need;
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
  Revision 1.52  2000-03-08 16:48:07  pierre
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