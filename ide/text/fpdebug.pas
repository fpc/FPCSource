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
  Objects,Dialogs,Drivers,Views,
  GDBCon,GDBInt,Menus,
  WViews,
  FPViews;

type
  PDebugController=^TDebugController;
  TDebugController=object(TGDBController)
     InvalidSourceLine : boolean;
     LastFileName : string;
     LastSource   : PView; {PsourceWindow !! }
     HiddenStepsCount : longint;
     { no need to switch if using another terminal }
     NoSwitch : boolean;
    constructor Init(const exefn:string);
    destructor  Done;
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
{    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;}
    procedure DoEndSession(code:longint);virtual;
    procedure AnnotateError;
    procedure InsertBreakpoints;
    procedure RemoveBreakpoints;
    procedure ReadWatches;
    procedure ResetBreakpointsValues;
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
    procedure Reset;virtual;
    procedure ResetDebuggerRows;
    procedure Run;virtual;
    procedure Continue;virtual;
    procedure UntilReturn;virtual;
    procedure CommandBegin(const s:string);virtual;
    procedure CommandEnd(const s:string);virtual;
    function  AllowQuit : boolean;virtual;
  end;

  BreakpointType = (bt_function,bt_file_line,bt_watch,bt_awatch,bt_rwatch,bt_invalid);
  BreakpointState = (bs_enabled,bs_disabled,bs_deleted);

  PBreakpointCollection=^TBreakpointCollection;

  PBreakpoint=^TBreakpoint;
  TBreakpoint=object(TObject)
     typ  : BreakpointType;
     state : BreakpointState;
     owner : PBreakpointCollection;
     Name : PString;  { either function name or expr to watch }
     FileName : PString;
     OldValue,CurrentValue : Pstring;
     Line : Longint; { only used for bt_file_line type }
     Conditions : PString; { conditions relative to that breakpoint }
     IgnoreCount : Longint; { how many counts should be ignored }
     Commands : pchar; { commands that should be executed on breakpoint }
     GDBIndex : longint;
     GDBState : BreakpointState;
     constructor Init_function(Const AFunc : String);
     constructor Init_Empty;
     constructor Init_file_line(AFile : String; ALine : longint);
     constructor Init_type(atyp : BreakpointType;Const AnExpr : String);
     constructor Load(var S: TStream);
     procedure   Store(var S: TStream);
     procedure  Insert;
     procedure  Remove;
     procedure  Enable;
     procedure  Disable;
     procedure  ResetValues;
     destructor Done;virtual;
  end;

  TBreakpointCollection=object(TCollection)
      function  At(Index: Integer): PBreakpoint;
      function  GetGDB(index : longint) : PBreakpoint;
      function  GetType(typ : BreakpointType;Const s : String) : PBreakpoint;
      function  ToggleFileLine(Const FileName: String;LineNr : Longint) : boolean;
      procedure Update;
      procedure ShowBreakpoints(W : PSourceWindow);
    end;

    PBreakpointItem = ^TBreakpointItem;
    TBreakpointItem = object(TObject)
      Breakpoint : PBreakpoint;
      constructor Init(ABreakpoint : PBreakpoint);
      function    GetText(MaxLen: Sw_integer): string; virtual;
      procedure   Selected; virtual;
      function    GetModuleName: string; virtual;
    end;

    PBreakpointsListBox = ^TBreakpointsListBox;
    TBreakpointsListBox = object(THSListBox)
      Transparent : boolean;
      NoSelection : boolean;
      MaxWidth    : Sw_integer;
      (* ModuleNames : PStoreCollection; *)
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure   AddBreakpoint(P: PBreakpointItem); virtual;
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      function    GetLocalMenu: PMenu;virtual;
      procedure   Clear; virtual;
      procedure   TrackSource; virtual;
      procedure   EditNew; virtual;
      procedure   EditCurrent; virtual;
      procedure   DeleteCurrent; virtual;
      procedure   ToggleCurrent;
      procedure   Draw; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    end;

    PBreakpointsWindow = ^TBreakpointsWindow;
    TBreakpointsWindow = object(TDlgWindow)
      BreakLB : PBreakpointsListBox;
      constructor Init;
      procedure   AddBreakpoint(ABreakpoint : PBreakpoint);
      procedure   ClearBreakpoints;
      procedure   ReloadBreakpoints;
      procedure   Close; virtual;
      procedure   SizeLimits(var Min, Max: TPoint);virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Update; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    end;

    PBreakpointItemDialog = ^TBreakpointItemDialog;

    TBreakpointItemDialog = object(TCenterDialog)
      constructor Init(ABreakpoint: PBreakpoint);
      function    Execute: Word; virtual;
    private
      Breakpoint : PBreakpoint;
      TypeRB   : PRadioButtons;
      NameIL  : PInputLine;
      ConditionsIL: PInputLine;
      LineIL    : PInputLine;
      IgnoreIL  : PInputLine;
    end;

    PWatch = ^TWatch;
    TWatch =  Object(TObject)
      constructor Init(s : string);
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure rename(s : string);
      procedure Get_new_value;
      destructor done;virtual;
    private
      expr : pstring;
      last_value,current_value : pchar;
    end;

    PWatchesCollection = ^TWatchesCollection;
    TWatchesCollection = Object(TCollection)
      constructor Init;
      procedure Insert(Item: Pointer); virtual;
      function  At(Index: Integer): PWatch;
      procedure Update;
    private
      MaxW : integer;
    end;

    PWatchesListBox = ^TWatchesListBox;
    TWatchesListBox = object(THSListBox)
      Transparent : boolean;
      MaxWidth    : Sw_integer;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      (* procedure   AddWatch(P: PWatch); virtual; *)
      procedure   Update(AMaxWidth : integer);
      function    GetIndentedText(Item,Indent,MaxLen: Sw_Integer): String; virtual;
      function    GetLocalMenu: PMenu;virtual;
      (* procedure   Clear; virtual;
      procedure   TrackSource; virtual;*)
      procedure   EditNew; virtual;
      procedure   EditCurrent; virtual;
      procedure   DeleteCurrent; virtual;
      (*procedure   ToggleCurrent; *)
      procedure   Draw; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    end;

    PWatchItemDialog = ^TWatchItemDialog;

    TWatchItemDialog = object(TCenterDialog)
      constructor Init(AWatch: PWatch);
      function    Execute: Word; virtual;
    private
      Watch : PWatch;
      NameIL  : PInputLine;
      TextST : PAdvancedStaticText;
    end;

    PWatchesWindow = ^TWatchesWindow;
    TWatchesWindow = Object(TDlgWindow)
      WLB : PWatchesListBox;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

    PFramesListBox = ^TFramesListBox;
    TFramesListBox = object(TMessageListBox)
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure   Update;
      function    GetLocalMenu: PMenu;virtual;
      procedure   GotoSource; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    end;

    PStackWindow = ^TStackWindow;
    TStackWindow = Object(TDlgWindow)
      FLB : PFramesListBox;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

    PRegistersView = ^TRegistersView;
    TRegistersView = object(TView)
      constructor Init(var Bounds: TRect);
      procedure   Draw;virtual;
      destructor  Done; virtual;
    end;

    PRegistersWindow = ^TRegistersWindow;
    TRegistersWindow = Object(TDlgWindow)
      RV : PRegistersView;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

const
  StackWindow : PStackWindow = nil;
  RegistersWindow : PRegistersWindow = nil;

  procedure InitStackWindow;
  procedure DoneStackWindow;

  procedure InitRegistersWindow;
  procedure DoneRegistersWindow;


const
     BreakpointTypeStr : Array[BreakpointType] of String[9]
       = ( 'function','file-line','watch','awatch','rwatch','invalid' );
     BreakpointStateStr : Array[BreakpointState] of String[8]
       = ( 'enabled','disabled','invalid' );

     DebuggeeTTY : string = '';
var
  Debugger             : PDebugController;
  BreakpointsCollection : PBreakpointCollection;
  WatchesCollection    : PwatchesCollection;

procedure InitDebugger;
procedure DoneDebugger;
procedure InitGDBWindow;
procedure DoneGDBWindow;
procedure InitBreakpoints;
procedure DoneBreakpoints;
procedure InitWatches;
procedure DoneWatches;

procedure RegisterFPDebugViews;

procedure UpdateDebugViews;

implementation

uses
  Dos,Mouse,Video,
  App,Commands,Strings,
  FPVars,FPUtils,FPConst,
  FPIntf,FPCompile,FPIde,FPHelp,
  Validate,WEditor,WUtils;

const
  RBreakpointsWindow: TStreamRec = (
     ObjType: 1701;
     VmtLink: Ofs(TypeOf(TBreakpointsWindow)^);
     Load:    @TBreakpointsWindow.Load;
     Store:   @TBreakpointsWindow.Store
  );

  RBreakpointsListBox : TStreamRec = (
     ObjType: 1702;
     VmtLink: Ofs(TypeOf(TBreakpointsListBox)^);
     Load:    @TBreakpointsListBox.Load;
     Store:   @TBreakpointsListBox.Store
  );

  RWatchesWindow: TStreamRec = (
     ObjType: 1703;
     VmtLink: Ofs(TypeOf(TWatchesWindow)^);
     Load:    @TWatchesWindow.Load;
     Store:   @TWatchesWindow.Store
  );

  RWatchesListBox: TStreamRec = (
     ObjType: 1704;
     VmtLink: Ofs(TypeOf(TWatchesListBox)^);
     Load:    @TWatchesListBox.Load;
     Store:   @TWatchesListBox.Store
  );

  RStackWindow: TStreamRec = (
     ObjType: 1705;
     VmtLink: Ofs(TypeOf(TStackWindow)^);
     Load:    @TStackWindow.Load;
     Store:   @TStackWindow.Store
  );

  RFramesListBox: TStreamRec = (
     ObjType: 1706;
     VmtLink: Ofs(TypeOf(TFramesListBox)^);
     Load:    @TFramesListBox.Load;
     Store:   @TFramesListBox.Store
  );

  RBreakpoint: TStreamRec = (
     ObjType: 1707;
     VmtLink: Ofs(TypeOf(TBreakpoint)^);
     Load:    @TBreakpoint.Load;
     Store:   @TBreakpoint.Store
  );

  RWatch: TStreamRec = (
     ObjType: 1708;
     VmtLink: Ofs(TypeOf(TWatch)^);
     Load:    @TWatch.Load;
     Store:   @TWatch.Store
  );

  RBreakpointCollection: TStreamRec = (
     ObjType: 1709;
     VmtLink: Ofs(TypeOf(TBreakpointCollection)^);
     Load:    @TBreakpointCollection.Load;
     Store:   @TBreakpointCollection.Store
  );

  RWatchesCollection: TStreamRec = (
     ObjType: 1710;
     VmtLink: Ofs(TypeOf(TWatchesCollection)^);
     Load:    @TWatchesCollection.Load;
     Store:   @TWatchesCollection.Store
  );

  RRegistersWindow: TStreamRec = (
     ObjType: 1711;
     VmtLink: Ofs(TypeOf(TRegistersWindow)^);
     Load:    @TRegistersWindow.Load;
     Store:   @TRegistersWindow.Store
  );

  RRegistersView: TStreamRec = (
     ObjType: 1712;
     VmtLink: Ofs(TypeOf(TRegistersView)^);
     Load:    @TRegistersView.Load;
     Store:   @TRegistersView.Store
  );


{****************************************************************************
                            TDebugController
****************************************************************************}

procedure UpdateDebugViews;

  begin
     If assigned(StackWindow) then
       StackWindow^.Update;
     If assigned(RegistersWindow) then
       RegistersWindow^.Update;
  end;

constructor TDebugController.Init(const exefn:string);
  var f: string;
begin
  inherited Init;
  f := exefn;
  NoSwitch:=False;
  LoadFile(f);
  SetArgs(GetRunParameters);
  Debugger:=@self;
{$ifndef GABOR}
  switch_to_user:=true;
{$endif}
  InsertBreakpoints;
  ReadWatches;
end;

procedure TDebugController.InsertBreakpoints;
  procedure DoInsert(PB : PBreakpoint);
  begin
    PB^.Insert;
  end;

begin
  BreakpointsCollection^.ForEach(@DoInsert);
end;

procedure TDebugController.ReadWatches;
  procedure DoRead(PB : PWatch);
  begin
    PB^.Get_new_value;
  end;

begin
  WatchesCollection^.ForEach(@DoRead);
  If Assigned(WatchesWindow) then
    WatchesWindow^.Update;
end;


procedure TDebugController.RemoveBreakpoints;
  procedure DoDelete(PB : PBreakpoint);
    begin
      PB^.Remove;
    end;
begin
   BreakpointsCollection^.ForEach(@DoDelete);
end;

procedure TDebugController.ResetBreakpointsValues;
  procedure DoResetVal(PB : PBreakpoint);
    begin
      PB^.ResetValues;
    end;
begin
   BreakpointsCollection^.ForEach(@DoResetVal);
end;

destructor TDebugController.Done;
begin
  { kill the program if running }
  Reset;
  RemoveBreakpoints;
  inherited Done;
end;

procedure TDebugController.Run;
begin
  ResetBreakpointsValues;
{$ifdef win32}
  { Run the debugge in another console }
  if DebuggeeTTY<>'' then
    Command('set new-console on')
  else
    Command('set new-console off');
  NoSwitch:=DebuggeeTTY<>'';
{$endif win32}
{$ifdef linux}
  { Run the debugge in another tty }
  Command('set tty '+DebuggeeTTY);
  NoSwitch:=DebuggeeTTY<>'';
{$endif win32}
  { Switch to user screen to get correct handles }
  UserScreen;
  inherited Run;
  DebuggerScreen;
  IDEApp.SetCmdState([cmResetDebugger,cmUntilReturn],true);
  UpdateDebugViews;
end;

procedure TDebugController.Continue;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  if not debuggee_started then
    Run
  else
    inherited Continue;
  UpdateDebugViews;
{$endif NODEBUG}
end;

procedure TDebugController.UntilReturn;
begin
  Command('finish');
  UpdateDebugViews;
  { We could try to get the return value !
    Not done yet }
end;


procedure TDebugController.CommandBegin(const s:string);
begin
  if assigned(GDBWindow) and (in_command>1) then
    begin
      { We should do something special for errors !! }
      If StrLen(GetError)>0 then
        GDBWindow^.WriteErrorText(GetError);
      GDBWindow^.WriteOutputText(GetOutput);
    end;
  if assigned(GDBWindow) then
    GDBWindow^.WriteString(S);
end;

procedure TDebugController.CommandEnd(const s:string);
begin
  if assigned(GDBWindow) and (in_command=0) then
    begin
      { We should do something special for errors !! }
      If StrLen(GetError)>0 then
        GDBWindow^.WriteErrorText(GetError);
      GDBWindow^.WriteOutputText(GetOutput);
      GDBWindow^.Editor^.TextEnd;
    end;
end;

function  TDebugController.AllowQuit : boolean;
begin
  if ConfirmBox('Really quit editor ?',nil,true)=cmOK then
    begin
      Message(@IDEApp,evCommand,cmQuit,nil);
    end
  else
    AllowQuit:=false;
end;

procedure TDebugController.ResetDebuggerRows;
  procedure ResetDebuggerRow(P: PView); {$ifndef FPC}far;{$endif}
  begin
    if assigned(P) and
       (TypeOf(P^)=TypeOf(TSourceWindow)) then
       PSourceWindow(P)^.Editor^.SetDebuggerRow(-1);
  end;

begin
  Desktop^.ForEach(@ResetDebuggerRow);
end;

procedure TDebugController.Reset;
begin
  inherited Reset;
  NoSwitch:=false;
  IDEApp.SetCmdState([cmResetDebugger,cmUntilReturn],false);
  ResetDebuggerRows;
end;

procedure TDebugController.AnnotateError;
var errornb : longint;
begin
  if error then
    begin
       errornb:=error_num;
       ReadWatches;
       UpdateDebugViews;
       ErrorBox(#3'Error within GDB'#13#3'Error code = %d',@errornb);
    end;
end;


procedure TDebugController.DoSelectSourceLine(const fn:string;line:longint);
var
  W: PSourceWindow;
  Found : boolean;
  PB : PBreakpoint;
  S : String;
  BreakIndex : longint;
begin
  BreakIndex:=stop_breakpoint_number;
  Desktop^.Lock;
  { 0 based line count in Editor }
  if Line>0 then
    dec(Line);
  if (fn=LastFileName) then
    begin
      W:=PSourceWindow(LastSource);
      if assigned(W) then
        begin
          W^.Editor^.SetCurPtr(0,Line);
          W^.Editor^.TrackCursor(true);
          W^.Editor^.SetDebuggerRow(Line);
          ReadWatches;
          UpdateDebugViews;

          if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
            W^.Select;
          InvalidSourceLine:=false;
        end
      else
        InvalidSourceLine:=true;
    end
  else
    begin
      W:=TryToOpenFile(nil,fn,0,Line,false);
      if assigned(W) then
        begin
          W^.Editor^.SetDebuggerRow(Line);
          W^.Editor^.TrackCursor(true);
          UpdateDebugViews;
          ReadWatches;
          if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
            W^.Select;
          LastSource:=W;
          InvalidSourceLine:=false;
        end
        { only search a file once }
      else
       begin
         Desktop^.UnLock;
         Found:=IDEApp.OpenSearch(fn);
         Desktop^.Lock;
         if not Found then
           begin
             InvalidSourceLine:=true;
             LastSource:=Nil;
           end
         else
           begin
             { should now be open }
              W:=TryToOpenFile(nil,fn,0,Line,true);
              W^.Editor^.SetDebuggerRow(Line);
              W^.Editor^.TrackCursor(true);
              ReadWatches;
              UpdateDebugViews;
              if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
                W^.Select;
              LastSource:=W;
              InvalidSourceLine:=false;
           end;
       end;
    end;
  LastFileName:=fn;
  Desktop^.UnLock;
  if BreakIndex>0 then
    begin
      PB:=BreakpointsCollection^.GetGDB(BreakIndex);
      { For watch we should get old and new value !! }
      if (Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive)) and
         (PB^.typ<>bt_file_line) and (PB^.typ<>bt_function) then
        begin
           Command('p '+GetStr(PB^.Name));
           S:=GetPChar(GetOutput);
           got_error:=false;
           If Pos('=',S)>0 then
             S:=Copy(S,Pos('=',S)+1,255);
           If S[Length(S)]=#10 then
             Delete(S,Length(S),1);
           if Assigned(PB^.OldValue) then
             DisposeStr(PB^.OldValue);
           PB^.OldValue:=PB^.CurrentValue;
           PB^.CurrentValue:=NewStr(S);
           If PB^.typ=bt_function then
             WarningBox(#3'GDB stopped due to'#13+
               #3+BreakpointTypeStr[PB^.typ]+' '+GetStr(PB^.Name),nil)
           else if (GetStr(PB^.OldValue)<>S) then
             WarningBox(#3'GDB stopped due to'#13+
               #3+BreakpointTypeStr[PB^.typ]+' '+GetStr(PB^.Name)+#13+
               #3+'Old value = '+GetStr(PB^.OldValue)+#13+
               #3+'New value = '+GetStr(PB^.CurrentValue),nil)
           else
             WarningBox(#3'GDB stopped due to'#13+
               #3+BreakpointTypeStr[PB^.typ]+' '+GetStr(PB^.Name)+#13+
               #3+' value = '+GetStr(PB^.CurrentValue),nil);
        end;
    end;
end;

procedure TDebugController.DoEndSession(code:longint);
var P :Array[1..2] of longint;
    W : PSourceWindow;
begin
   IDEApp.SetCmdState([cmResetDebugger],false);
   ResetDebuggerRows;
   LastExitCode:=Code;
   If HiddenStepsCount=0 then
     InformationBox(#3'Program exited with '#13#3'exitcode = %d',@code)
   else
     begin
        P[1]:=code;
        P[2]:=HiddenStepsCount;
        WarningBox(#3'Program exited with '#13+
                   #3'exitcode = %d'#13+
                   #3'hidden steps = %d',@P);
     end;
end;


procedure TDebugController.DoDebuggerScreen;
begin
  if NoSwitch then
    PopStatus
  else
    IDEApp.ShowIDEScreen;
end;


procedure TDebugController.DoUserScreen;
begin
  if NoSwitch then
    PushStatus('Executable running in another window..')
  else
    IDEApp.ShowUserScreen;
end;

{****************************************************************************
                                 TBreakpoint
****************************************************************************}

constructor TBreakpoint.Init_function(Const AFunc : String);
begin
  typ:=bt_function;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=NewStr(AFunc);
  FileName:=nil;
  Line:=0;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Init_Empty;
begin
  typ:=bt_function;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=Nil;
  FileName:=nil;
  Line:=0;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Init_type(atyp : BreakpointType;Const AnExpr : String);
begin
  typ:=atyp;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=NewStr(AnExpr);
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Init_file_line(AFile : String; ALine : longint);
begin
  typ:=bt_file_line;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  { d:test.pas:12 does not work !! }
  { I do not know how to solve this if
  if (Length(AFile)>1) and (AFile[2]=':') then
    AFile:=Copy(AFile,3,255);
    Only use base name for now !! PM }
  FileName:=NewStr(AFile);
  Name:=nil;
  Line:=ALine;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Load(var S: TStream);
begin
  S.Read(typ,SizeOf(BreakpointType));
  S.Read(state,SizeOf(BreakpointState));
  GDBState:=bs_deleted;
  case typ of
    bt_file_line :
      begin
        FileName:=S.ReadStr;
        S.Read(Line,SizeOf(Line));
        Name:=nil;
      end;
  else
    begin
        Name:=S.ReadStr;
        Line:=0;
        FileName:=nil;
    end;
  end;
  S.Read(IgnoreCount,SizeOf(IgnoreCount));
  Commands:=S.StrRead;
  Conditions:=S.ReadStr;
  OldValue:=nil;
  CurrentValue:=nil;
end;

procedure TBreakpoint.Store(var S: TStream);
begin
  S.Write(typ,SizeOf(BreakpointType));
  S.Write(state,SizeOf(BreakpointState));
  case typ of
    bt_file_line :
      begin
        S.WriteStr(FileName);
        S.Write(Line,SizeOf(Line));
      end;
  else
    begin
        S.WriteStr(Name);
    end;
  end;
  S.Write(IgnoreCount,SizeOf(IgnoreCount));
  S.StrWrite(Commands);
  S.WriteStr(Conditions);
end;

procedure TBreakpoint.Insert;
begin
  If not assigned(Debugger) then Exit;
  Remove;
  Debugger^.last_breakpoint_number:=0;
  if (GDBState=bs_deleted) and (state=bs_enabled) then
    begin
      if (typ=bt_file_line) and assigned(FileName) then
        Debugger^.Command('break '+NameAndExtOf(FileName^)+':'+IntToStr(Line))
      else if (typ=bt_function) and assigned(name) then
        Debugger^.Command('break '+name^)
      else if (typ=bt_watch) and assigned(name) then
        Debugger^.Command('watch '+name^)
      else if (typ=bt_awatch) and assigned(name) then
        Debugger^.Command('awatch '+name^)
      else if (typ=bt_rwatch) and assigned(name) then
        Debugger^.Command('rwatch '+name^);
      if Debugger^.last_breakpoint_number<>0 then
        begin
          GDBIndex:=Debugger^.last_breakpoint_number;
          GDBState:=bs_enabled;
          Debugger^.Command('cond '+IntToStr(GDBIndex)+' '+GetStr(Conditions));
          If IgnoreCount>0 then
            Debugger^.Command('ignore '+IntToStr(GDBIndex)+' '+IntToStr(IgnoreCount));
          If Assigned(Commands) then
            begin
              {Commands are not handled yet }
            end;
        end
      else
      { Here there was a problem !! }
        begin
          GDBIndex:=0;
          ErrorBox(#3'Could not set Breakpoint'#13+
            #3+BreakpointTypeStr[typ]+' '+GetStr(Name),nil);
          state:=bs_disabled;
        end;
    end
  else if (GDBState=bs_disabled) and (state=bs_enabled) then
    Enable
  else if (GDBState=bs_enabled) and (state=bs_disabled) then
    Disable;
end;

procedure TBreakpoint.Remove;
begin
  If not assigned(Debugger) then Exit;
  if GDBIndex>0 then
    Debugger^.Command('delete '+IntToStr(GDBIndex));
  GDBIndex:=0;
  GDBState:=bs_deleted;
end;

procedure TBreakpoint.Enable;
begin
  If not assigned(Debugger) then Exit;
  if GDBIndex>0 then
    Debugger^.Command('enable '+IntToStr(GDBIndex))
  else
    Insert;
  GDBState:=bs_enabled;
end;

procedure TBreakpoint.Disable;
begin
  If not assigned(Debugger) then Exit;
  if GDBIndex>0 then
    Debugger^.Command('disable '+IntToStr(GDBIndex));
  GDBState:=bs_disabled;
end;

procedure TBreakpoint.ResetValues;
begin
  if assigned(OldValue) then
    DisposeStr(OldValue);
  OldValue:=nil;
  if assigned(CurrentValue) then
    DisposeStr(CurrentValue);
  CurrentValue:=nil;
end;

destructor TBreakpoint.Done;
begin
  Remove;
  ResetValues;
  if assigned(Name) then
    DisposeStr(Name);
  if assigned(FileName) then
    DisposeStr(FileName);
  if assigned(Conditions) then
    DisposeStr(Conditions);
  if assigned(Commands) then
    StrDispose(Commands);
  inherited Done;
end;

{****************************************************************************
                        TBreakpointCollection
****************************************************************************}

function TBreakpointCollection.At(Index: Integer): PBreakpoint;
begin
  At:=inherited At(Index);
end;

procedure TBreakpointCollection.Update;
begin
  if assigned(Debugger) then
    begin
      Debugger^.RemoveBreakpoints;
      Debugger^.InsertBreakpoints;
    end;
  if assigned(BreakpointsWindow) then
    BreakpointsWindow^.Update;
end;

function  TBreakpointCollection.GetGDB(index : longint) : PBreakpoint;

  function IsNum(P : PBreakpoint) : boolean;{$ifndef FPC}far;{$endif}
  begin
    IsNum:=P^.GDBIndex=index;
  end;

begin
  if index=0 then
    GetGDB:=nil
  else
    GetGDB:=FirstThat(@IsNum);
end;

procedure TBreakpointCollection.ShowBreakpoints(W : PSourceWindow);

  procedure SetInSource(P : PBreakpoint);{$ifndef FPC}far;{$endif}
  begin
    If assigned(P^.FileName) and (P^.FileName^=W^.Editor^.FileName) then
      W^.Editor^.SetLineBreakState(P^.Line,P^.state=bs_enabled);
  end;

begin
  ForEach(@SetInSource);
end;

function TBreakpointCollection.GetType(typ : BreakpointType;Const s : String) : PBreakpoint;

  function IsThis(P : PBreakpoint) : boolean;{$ifndef FPC}far;{$endif}
  begin
    IsThis:=(P^.typ=typ) and (GetStr(P^.Name)=S);
  end;

begin
  GetType:=FirstThat(@IsThis);
end;

function TBreakpointCollection.ToggleFileLine(Const FileName: String;LineNr : Longint) : boolean;

var PB : PBreakpoint;

  function IsThere(P : PBreakpoint) : boolean;{$ifndef FPC}far;{$endif}
  begin
    IsThere:=(P^.typ=bt_file_line) and (P^.FileName^=FileName) and (P^.Line=LineNr);
  end;
begin
    PB:=FirstThat(@IsThere);
    ToggleFileLine:=false;
    If Assigned(PB) then
      if PB^.state=bs_disabled then
        begin
          PB^.state:=bs_enabled;
          ToggleFileLine:=true;
        end
      else if PB^.state=bs_enabled then
        PB^.state:=bs_disabled;
    If not assigned(PB) then
      begin
        PB:= New(PBreakpoint,Init_file_line(FileName,LineNr));
        if assigned(PB) then
          Begin
            Insert(PB);
            ToggleFileLine:=true;
          End;
      end;
    Update;
end;



{****************************************************************************
                         TBreakpointItem
****************************************************************************}

constructor TBreakpointItem.Init(ABreakpoint : PBreakpoint);
begin
  inherited Init;
  Breakpoint:=ABreakpoint;
end;

function TBreakpointItem.GetText(MaxLen: Sw_integer): string;
var S: string;
begin
 with Breakpoint^ do
   begin
     S:=BreakpointTypeStr[typ];
     While Length(S)<10 do
       S:=S+' ';
     S:=S+'|';
     S:=S+BreakpointStateStr[state]+' ';
     While Length(S)<20 do
       S:=S+' ';
     S:=S+'|';
     if (typ=bt_file_line) then
       S:=S+NameAndExtOf(GetStr(FileName))+':'+IntToStr(Line)
         else
       S:=S+GetStr(name);
     While Length(S)<40 do
       S:=S+' ';
     S:=S+'|';
     if IgnoreCount>0 then
       S:=S+IntToStr(IgnoreCount);
     While Length(S)<49 do
       S:=S+' ';
     S:=S+'|';
     if assigned(Conditions) then
       S:=S+' '+GetStr(Conditions);
     if length(S)>MaxLen then S:=copy(S,1,MaxLen-2)+'..';
     GetText:=S;
   end;
end;

procedure TBreakpointItem.Selected;
begin
end;

function TBreakpointItem.GetModuleName: string;
begin
  if breakpoint^.typ=bt_file_line then
    GetModuleName:=GetStr(breakpoint^.FileName)
  else
    GetModuleName:='';
end;

{****************************************************************************
                         TBreakpointsListBox
****************************************************************************}

constructor TBreakpointsListBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AHScrollBar, AVScrollBar);
  GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
  NoSelection:=true;
end;

function TBreakpointsListBox.GetLocalMenu: PMenu;
var M: PMenu;
begin
  if (Owner<>nil) and (Owner^.GetState(sfModal)) then M:=nil else
  M:=NewMenu(
    NewItem('~G~oto source','',kbNoKey,cmMsgGotoSource,hcMsgGotoSource,
    NewItem('~E~dit breakpoint','',kbNoKey,cmEditBreakpoint,hcEditBreakpoint,
    NewItem('~N~ew breakpoint','',kbNoKey,cmNewBreakpoint,hcNewBreakpoint,
    NewItem('~D~elete breakpoint','',kbNoKey,cmDeleteBreakpoint,hcDeleteBreakpoint,
    NewItem('~T~oggle state','',kbNoKey,cmToggleBreakpoint,hcToggleBreakpoint,
    nil))))));
  GetLocalMenu:=M;
end;

procedure TBreakpointsListBox.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            Message(@Self,evCommand,cmMsgGotoSource,nil);
        else
          DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmListItemSelected :
          if Event.InfoPtr=@Self then
            Message(@Self,evCommand,cmEditBreakpoint,nil);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmMsgTrackSource :
            if Range>0 then
              TrackSource;
          cmEditBreakpoint :
              EditCurrent;
          cmToggleBreakpoint :
              ToggleCurrent;
          cmDeleteBreakpoint :
              DeleteCurrent;
          cmNewBreakpoint :
              EditNew;
          cmMsgClear :
            Clear;
          else
            DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TBreakpointsListBox.AddBreakpoint(P: PBreakpointItem);
var W : integer;
begin
  if List=nil then New(List, Init(20,20));
  W:=length(P^.GetText(255));
  if W>MaxWidth then
  begin
    MaxWidth:=W;
    if HScrollBar<>nil then
       HScrollBar^.SetRange(0,MaxWidth);
  end;
  List^.Insert(P);
  SetRange(List^.Count);
  if Focused=List^.Count-1-1 then
     FocusItem(List^.Count-1);
  DrawView;
end;

(* function TBreakpointsListBox.AddModuleName(const Name: string): PString;
var P: PString;
begin
  if ModuleNames<>nil then
    P:=ModuleNames^.Add(Name)
  else
    P:=nil;
  AddModuleName:=P;
end;  *)

function TBreakpointsListBox.GetText(Item,MaxLen: Sw_Integer): String;
var P: PBreakpointItem;
    S: string;
begin
  P:=List^.At(Item);
  S:=P^.GetText(MaxLen);
  GetText:=copy(S,1,MaxLen);
end;

procedure TBreakpointsListBox.Clear;
begin
  if assigned(List) then
    Dispose(List, Done);
  List:=nil;
  MaxWidth:=0;
  (* if assigned(ModuleNames) then
    ModuleNames^.FreeAll; *)
  SetRange(0); DrawView;
  Message(Application,evBroadcast,cmClearLineHighlights,@Self);
end;

procedure TBreakpointsListBox.TrackSource;
var W: PSourceWindow;
    P: PBreakpointItem;
    R: TRect;
    (* Row,Col: sw_integer; *)
begin
  (*Message(Application,evBroadcast,cmClearLineHighlights,@Self);
  if Range=0 then Exit;*)
  P:=List^.At(Focused);
  if P^.GetModuleName='' then Exit;
  Desktop^.Lock;
  GetNextEditorBounds(R);
  R.B.Y:=Owner^.Origin.Y;
  W:=EditorWindowFile(P^.GetModuleName);
  if assigned(W) then
    begin
      W^.GetExtent(R);
      R.B.Y:=Owner^.Origin.Y;
      W^.ChangeBounds(R);
      W^.Editor^.SetCurPtr(1,P^.Breakpoint^.Line);
    end
  else
    W:=TryToOpenFile(@R,P^.GetModuleName,1,P^.Breakpoint^.Line,true);
  if W<>nil then
    begin
      W^.Select;
      W^.Editor^.TrackCursor(true);
      W^.Editor^.SetHighlightRow(P^.Breakpoint^.Line);
    end;
  if Assigned(Owner) then
    Owner^.Select;
  Desktop^.UnLock;
end;

procedure TBreakpointsListBox.ToggleCurrent;
var W: PSourceWindow;
    P: PBreakpointItem;
    b : boolean;
    (* Row,Col: sw_integer; *)
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  if P^.Breakpoint^.state=bs_enabled then
    P^.Breakpoint^.state:=bs_disabled
  else if P^.Breakpoint^.state=bs_disabled then
    P^.Breakpoint^.state:=bs_enabled;
  BreakpointsCollection^.Update;
  if P^.Breakpoint^.typ=bt_file_line then
    begin
      W:=TryToOpenFile(nil,GetStr(P^.Breakpoint^.FileName),1,P^.Breakpoint^.Line,false);
      If assigned(W) then
        begin
          if P^.Breakpoint^.state=bs_enabled then
            b:=true
          else
            b:=false;
          W^.Editor^.SetLineBreakState(P^.Breakpoint^.Line,b);
        end;
    end;
end;

procedure TBreakpointsListBox.EditCurrent;
var
  P: PBreakpointItem;
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  Application^.ExecuteDialog(New(PBreakpointItemDialog,Init(P^.Breakpoint)),nil);
  BreakpointsCollection^.Update;
end;

procedure TBreakpointsListBox.DeleteCurrent;
var
  P: PBreakpointItem;
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  BreakpointsCollection^.free(P^.Breakpoint);
  List^.free(P);
  BreakpointsCollection^.Update;
end;

procedure TBreakpointsListBox.EditNew;
var
  P: PBreakpoint;
begin
  P:=New(PBreakpoint,Init_Empty);
  if Application^.ExecuteDialog(New(PBreakpointItemDialog,Init(P)),nil)<>cmCancel then
    begin
      BreakpointsCollection^.Insert(P);
      BreakpointsCollection^.Update;
    end
  else
    dispose(P,Done);
end;

procedure TBreakpointsListBox.Draw;
var
  I, J, Item: Sw_Integer;
  NormalColor, SelectedColor, FocusedColor, Color: Word;
  ColWidth, CurCol, Indent: Integer;
  B: TDrawBuffer;
  Text: String;
  SCOff: Byte;
  TC: byte;
procedure MT(var C: word); begin if TC<>0 then C:=(C and $ff0f) or (TC and $f0); end;
begin
  if (Owner<>nil) then TC:=ord(Owner^.GetColor(6)) else TC:=0;
  if State and (sfSelected + sfActive) = (sfSelected + sfActive) then
  begin
    NormalColor := GetColor(1);
    FocusedColor := GetColor(3);
    SelectedColor := GetColor(4);
  end else
  begin
    NormalColor := GetColor(2);
    SelectedColor := GetColor(4);
  end;
  if Transparent then
    begin MT(NormalColor); MT(SelectedColor); end;
  if NoSelection then
     SelectedColor:=NormalColor;
  if HScrollBar <> nil then Indent := HScrollBar^.Value
  else Indent := 0;
  ColWidth := Size.X div NumCols + 1;
  for I := 0 to Size.Y - 1 do
  begin
    for J := 0 to NumCols-1 do
    begin
      Item := J*Size.Y + I + TopItem;
      CurCol := J*ColWidth;
      if (State and (sfSelected + sfActive) = (sfSelected + sfActive)) and
        (Focused = Item) and (Range > 0) then
      begin
        Color := FocusedColor;
        SetCursor(CurCol+1,I);
        SCOff := 0;
      end
      else if (Item < Range) and IsSelected(Item) then
      begin
        Color := SelectedColor;
        SCOff := 2;
      end
      else
      begin
        Color := NormalColor;
        SCOff := 4;
      end;
      MoveChar(B[CurCol], ' ', Color, ColWidth);
      if Item < Range then
      begin
        Text := GetText(Item, ColWidth + Indent);
        Text := Copy(Text,Indent,ColWidth);
        MoveStr(B[CurCol+1], Text, Color);
        if ShowMarkers then
        begin
          WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
          WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
        end;
      end;
      MoveChar(B[CurCol+ColWidth-1], #179, GetColor(5), 1);
    end;
    WriteLine(0, I, Size.X, 1, B);
  end;
end;

constructor TBreakpointsListBox.Load(var S: TStream);
begin
  inherited Load(S);
end;

procedure TBreakpointsListBox.Store(var S: TStream);
var OL: PCollection;
    OldR : integer;
begin
  OL:=List;
  OldR:=Range;
  Range:=0;
  New(List, Init(1,1));

  inherited Store(S);

  Dispose(List, Done);
  Range:=OldR;
  List:=OL;
  { ^^^ nasty trick - has anyone a better idea how to avoid storing the
    collection? Pasting here a modified version of TListBox.Store+
    TAdvancedListBox.Store isn't a better solution, since by eventually
    changing the obj-hierarchy you'll always have to modify this, too - BG }
end;

destructor TBreakpointsListBox.Done;
begin
  inherited Done;
  if List<>nil then Dispose(List, Done);
  (* if ModuleNames<>nil then Dispose(ModuleNames, Done);*)
end;

{****************************************************************************
                         TBreakpointsWindow
****************************************************************************}

constructor TBreakpointsWindow.Init;
var R,R2: TRect;
    HSB,VSB: PScrollBar;
    ST: PStaticText;
    S: String;
    X,X1 : Sw_integer;
const White = 15;
begin
  Desktop^.GetExtent(R); R.A.Y:=R.B.Y-18;
  inherited Init(R, 'Breakpoint list', wnNoNumber);

  HelpCtx:=hcBreakpointListWindow;

  GetExtent(R); R.Grow(-1,-1); R.B.Y:=R.A.Y+1;
  S:=' Type      | State   | Position          | Ignore | Conditions ';
  New(ST, Init(R,S));
  ST^.GrowMode:=gfGrowHiX;
  Insert(ST);
  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,1); R.B.Y:=R.A.Y+1;
  New(ST, Init(R, CharStr('Ä', MaxViewWidth)));
  ST^.GrowMode:=gfGrowHiX;
  Insert(ST);
  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,2);Dec(R.B.Y,5);
  R2.Copy(R); Inc(R2.B.Y); R2.A.Y:=R2.B.Y-1;
  New(HSB, Init(R2)); HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX; Insert(HSB);
  R2.Copy(R); Inc(R2.B.X); R2.A.X:=R2.B.X-1;
  New(VSB, Init(R2)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  New(BreakLB, Init(R,HSB,VSB));
  BreakLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
  BreakLB^.Transparent:=true;
  Insert(BreakLB);
  GetExtent(R);R.Grow(-1,-1);
  Dec(R.B.Y);
  R.A.Y:=R.B.Y-2;
  X:=(R.B.X-R.A.X) div 4;
  X1:=R.A.X+(X div 2);
  R.A.X:=X1-3;R.B.X:=X1+7;
  Insert(New(PButton, Init(R, '~C~lose', cmClose, bfDefault)));
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  Insert(New(PButton, Init(R, '~N~ew', cmNewBreakpoint, bfNormal)));
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  Insert(New(PButton, Init(R, '~E~dit', cmEditBreakpoint, bfNormal)));
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  Insert(New(PButton, Init(R, '~D~elete', cmDeleteBreakpoint, bfNormal)));
  BreakLB^.Select;
  Update;
  BreakpointsWindow:=@self;
end;

constructor TBreakpointsWindow.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S,BreakLB);
end;

procedure TBreakpointsWindow.Store(var S: TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S,BreakLB);
end;

procedure TBreakpointsWindow.AddBreakpoint(ABreakpoint : PBreakpoint);
begin
  BreakLB^.AddBreakpoint(New(PBreakpointItem, Init(ABreakpoint)));
end;

procedure TBreakpointsWindow.ClearBreakpoints;
begin
  BreakLB^.Clear;
  ReDraw;
end;

procedure TBreakpointsWindow.ReloadBreakpoints;
  procedure InsertInBreakLB(P : PBreakpoint);
  begin
    BreakLB^.AddBreakpoint(New(PBreakpointItem, Init(P)));
  end;
begin
  If not assigned(BreakpointsCollection) then
    exit;
  BreakpointsCollection^.ForEach(@InsertInBreakLB);
  ReDraw;
end;

procedure TBreakpointsWindow.SizeLimits(var Min, Max: TPoint);
begin
  inherited SizeLimits(Min,Max);
  Min.X:=40; Min.Y:=18;
end;

procedure TBreakpointsWindow.Close;
begin
  Hide;
end;

procedure TBreakpointsWindow.HandleEvent(var Event: TEvent);
var DontClear : boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        if (Event.KeyCode=kbEnter) or (Event.KeyCode=kbEsc) then
          begin
            ClearEvent(Event);
            Hide;
          end;
      end;
    evCommand :
      begin
       DontClear:=False;
       case Event.Command of
         cmNewBreakpoint :
           BreakLB^.EditNew;
         cmEditBreakpoint :
           BreakLB^.EditCurrent;
         cmDeleteBreakpoint :
           BreakLB^.DeleteCurrent;
         cmClose :
           Hide;
          else
            DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmUpdate :
          Update;
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TBreakpointsWindow.Update;
begin
  ClearBreakpoints;
  ReloadBreakpoints;
end;

destructor TBreakpointsWindow.Done;
begin
  inherited Done;
  BreakpointsWindow:=nil;
end;

{****************************************************************************
                         TBreakpointItemDialog
****************************************************************************}

constructor TBreakpointItemDialog.Init(ABreakpoint: PBreakpoint);
var R,R2,R3: TRect;
    Items: PSItem;
    I : BreakpointType;
    KeyCount: sw_integer;
begin
  KeyCount:=longint(high(BreakpointType));

  R.Assign(0,0,60,Max(3+KeyCount,18));
  inherited Init(R,'Modify/New Breakpoint');
  Breakpoint:=ABreakpoint;

  GetExtent(R); R.Grow(-3,-2); R3.Copy(R);
  Inc(R.A.Y); R.B.Y:=R.A.Y+1; R.B.X:=R.A.X+36;
  New(NameIL, Init(R, 128)); Insert(NameIL);
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, '~N~ame', NameIL)));
  R.Move(0,3);
  New(LineIL, Init(R, 128)); Insert(LineIL);
  LineIL^.SetValidator(New(PRangeValidator, Init(0,MaxInt)));
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, '~L~ine', LineIL)));
  R.Move(0,3);
  New(ConditionsIL, Init(R, 128)); Insert(ConditionsIL);
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, 'Conditions', ConditionsIL)));
  R.Move(0,3);
  New(IgnoreIL, Init(R, 128)); Insert(IgnoreIL);
  IgnoreIL^.SetValidator(New(PRangeValidator, Init(0,MaxInt)));
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, '~I~gnore count', IgnoreIL)));

  R.Copy(R3); Inc(R.A.X,38); R.B.Y:=R.A.Y+KeyCount;
  Items:=nil;
  for I:=high(BreakpointType) downto low(BreakpointType) do
    Items:=NewSItem(BreakpointTypeStr[I], Items);
  New(TypeRB, Init(R, Items));
  Insert(TypeRB);

  InsertButtons(@Self);

  NameIL^.Select;
end;

function TBreakpointItemDialog.Execute: Word;
var R: word;
    S1: string;
    err: word;
    L: longint;
begin
  R:=longint(Breakpoint^.typ);
  TypeRB^.SetData(R);

  If Breakpoint^.typ=bt_file_line then
    S1:=GetStr(Breakpoint^.FileName)
  else
    S1:=GetStr(Breakpoint^.name);
  NameIL^.SetData(S1);

  If Breakpoint^.typ=bt_file_line then
    S1:=IntToStr(Breakpoint^.Line)
  else
    S1:='0';
  LineIL^.SetData(S1);

  S1:=IntToStr(Breakpoint^.IgnoreCount);
  IgnoreIL^.SetData(S1);
  S1:=GetStr(Breakpoint^.Conditions);
  ConditionsIL^.SetData(S1);

  R:=inherited Execute;
  if R=cmOK then
  begin
    TypeRB^.GetData(R);
    L:=R;
    Breakpoint^.typ:=BreakpointType(L);

    NameIL^.GetData(S1);
    If Breakpoint^.typ=bt_file_line then
      begin
        If assigned(Breakpoint^.FileName) then
          DisposeStr(Breakpoint^.FileName);
        Breakpoint^.FileName:=NewStr(S1);
      end
    else
      begin
        If assigned(Breakpoint^.Name) then
          DisposeStr(Breakpoint^.Name);
        Breakpoint^.name:=NewStr(S1);
      end;
    If Breakpoint^.typ=bt_file_line then
      begin
        LineIL^.GetData(S1);
        Val(S1,L,err);
        Breakpoint^.Line:=L;
      end;
    IgnoreIL^.GetData(S1);
    Val(S1,L,err);
    Breakpoint^.IgnoreCount:=L;

    ConditionsIL^.GetData(S1);
    If assigned(Breakpoint^.Conditions) then
      DisposeStr(Breakpoint^.Conditions);
    Breakpoint^.Conditions:=NewStr(S1);
  end;
  Execute:=R;
end;

{****************************************************************************
                         TWatch
****************************************************************************}

constructor TWatch.Init(s : string);
  begin
    expr:=NewStr(s);
    last_value:=nil;
    current_value:=nil;
    Get_new_value;
  end;

constructor TWatch.Load(var S: TStream);
  begin
    expr:=S.ReadStr;
    last_value:=nil;
    current_value:=nil;
    Get_new_value;
  end;

procedure TWatch.Store(var S: TStream);
  begin
    S.WriteStr(expr);
  end;

procedure TWatch.rename(s : string);
  begin
    if assigned(expr) then
      begin
        if GetStr(expr)=S then
          exit;
        DisposeStr(expr);
      end;
    expr:=NewStr(s);
    if assigned(last_value) then
      StrDispose(last_value);
    last_value:=nil;
    if assigned(current_value) then
      StrDispose(current_value);
    current_value:=nil;
    Get_new_value;
  end;

procedure TWatch.Get_new_value;
  var p,q : pchar;
      i : longint;
      last_removed : boolean;
  begin
    If not assigned(Debugger) then
      exit;
    if assigned(last_value) then
      strdispose(last_value);
    last_value:=current_value;
    Debugger^.Command('p '+GetStr(expr));
    if Debugger^.Error then
      p:=StrNew(Debugger^.GetError)
    else
      p:=StrNew(Debugger^.GetOutput);
    { do not open a messagebox for such errors }
    Debugger^.got_error:=false;
    q:=nil;
    if assigned(p) and (p[0]='$') then
      q:=StrPos(p,'=');
    if not assigned(q) then
      q:=p;
    if assigned(q) then
      i:=strlen(q)
    else
      i:=0;
    if (i>0) and (q[i-1]=#10) then
      begin
        q[i-1]:=#0;
        last_removed:=true;
      end
    else
      last_removed:=false;
    if assigned(q) then
      current_value:=strnew(q)
    else
      current_value:=strnew('');
    if last_removed then
      q[i-1]:=#10;
    strdispose(p);
  end;

destructor TWatch.Done;
  begin
    if assigned(expr) then
      disposestr(expr);
    if assigned(last_value) then
      strdispose(last_value);
    if assigned(current_value) then
      strdispose(current_value);
    inherited done;
  end;

{****************************************************************************
                         TWatchesCollection
****************************************************************************}

      constructor TWatchesCollection.Init;
        begin
          inherited Init(10,10);
        end;

      procedure TWatchesCollection.Insert(Item: Pointer);
       begin
         PWatch(Item)^.Get_new_value;
         Inherited Insert(Item);
         Update;
       end;

      procedure TWatchesCollection.Update;
        var
         W,W1 : integer;
         procedure GetMax(P : PWatch);
           begin
              if assigned(P^.Current_value) then
               begin
                 W1:=StrLen(P^.Current_value)+2+Length(GetStr(P^.expr));
                 if W1>W then
                  W:=W1;
               end;
           end;
        begin
          W:=0;
          ForEach(@GetMax);
          MaxW:=W;
          If assigned(WatchesWindow) then
            WatchesWindow^.WLB^.Update(MaxW);
        end;

      function  TWatchesCollection.At(Index: Integer): PWatch;
        begin
          At:=Inherited At(Index);
        end;

{****************************************************************************
                         TWatchesListBox
****************************************************************************}

constructor TWatchesListBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
  begin
    inherited Init(Bounds,1,AHScrollBar,AVScrollBar);
    If assigned(List) then
      dispose(list,done);
    List:=WatchesCollection;
  end;

procedure TWatchesListBox.Update(AMaxWidth : integer);
var R : TRect;
begin
  GetExtent(R);
  MaxWidth:=AMaxWidth;
  if HScrollBar<>nil then
    HScrollBar^.SetRange(0,MaxWidth);
  if R.B.X-R.A.X>MaxWidth then
    HScrollBar^.Hide
  else
    HScrollBar^.Show;
  SetRange(List^.Count);
  if R.B.Y-R.A.Y>Range then
    VScrollBar^.Hide
  else
    VScrollBar^.Show;

  if Focused=List^.Count-1-1 then
     FocusItem(List^.Count-1);
  DrawView;
end;

function    TWatchesListBox.GetIndentedText(Item,Indent,MaxLen: Sw_Integer): String;
var
  PW : PWatch;
  ValOffset : Sw_integer;
  S : String;
begin
  PW:=WatchesCollection^.At(Item);
  ValOffset:=Length(GetStr(PW^.Expr))+2;
  if Indent<ValOffset then
    begin
      if not assigned(PW^.current_value) then
        S:=' '+GetStr(PW^.Expr)+' <Unknown value>'
      else if not assigned(PW^.last_value) or
        (strcomp(PW^.Last_value,PW^.Current_value)=0) then
        S:=' '+GetStr(PW^.Expr)+' '+GetPChar(PW^.Current_value)
      else
        S:='!'+GetStr(PW^.Expr)+'!'+GetPchar(PW^.Current_value);
      GetIndentedText:=Copy(S,Indent,MaxLen);
    end
  else
   begin
      if not assigned(PW^.Current_value) or
         (StrLen(PW^.Current_value)<Indent-Valoffset) then
        S:=''
      else
        S:=GetStr(@(PW^.Current_Value[Indent-Valoffset]));
      GetIndentedText:=Copy(S,1,MaxLen);
   end;
end;

procedure TWatchesListBox.EditCurrent;
var
  P: PWatch;
begin
  if Range=0 then Exit;
  P:=WatchesCollection^.At(Focused);
  if P=nil then Exit;
  Application^.ExecuteDialog(New(PWatchItemDialog,Init(P)),nil);
  WatchesCollection^.Update;
end;

procedure TWatchesListBox.DeleteCurrent;
var
  P: PWatch;
begin
  if Range=0 then Exit;
  P:=WatchesCollection^.At(Focused);
  if P=nil then Exit;
  WatchesCollection^.free(P);
  WatchesCollection^.Update;
end;

procedure TWatchesListBox.EditNew;
var
  P: PWatch;
begin
  P:=New(PWatch,Init(''));
  if Application^.ExecuteDialog(New(PWatchItemDialog,Init(P)),nil)<>cmCancel then
    begin
      WatchesCollection^.Insert(P);
      WatchesCollection^.Update;
    end
  else
    dispose(P,Done);
end;

procedure   TWatchesListBox.Draw;
var
  I, J, Item: Sw_Integer;
  NormalColor, SelectedColor, FocusedColor, Color: Word;
  ColWidth, CurCol, Indent: Integer;
  B: TDrawBuffer;
  Text: String;
  SCOff: Byte;
  TC: byte;
procedure MT(var C: word); begin if TC<>0 then C:=(C and $ff0f) or (TC and $f0); end;
begin
  if (Owner<>nil) then TC:=ord(Owner^.GetColor(6)) else TC:=0;
  if State and (sfSelected + sfActive) = (sfSelected + sfActive) then
  begin
    NormalColor := GetColor(1);
    FocusedColor := GetColor(3);
    SelectedColor := GetColor(4);
  end else
  begin
    NormalColor := GetColor(2);
    SelectedColor := GetColor(4);
  end;
  if Transparent then
    begin MT(NormalColor); MT(SelectedColor); end;
  (* if NoSelection then
     SelectedColor:=NormalColor;*)
  if HScrollBar <> nil then Indent := HScrollBar^.Value
  else Indent := 0;
  ColWidth := Size.X div NumCols + 1;
  for I := 0 to Size.Y - 1 do
  begin
    for J := 0 to NumCols-1 do
    begin
      Item := J*Size.Y + I + TopItem;
      CurCol := J*ColWidth;
      if (State and (sfSelected + sfActive) = (sfSelected + sfActive)) and
        (Focused = Item) and (Range > 0) then
      begin
        Color := FocusedColor;
        SetCursor(CurCol+1,I);
        SCOff := 0;
      end
      else if (Item < Range) and IsSelected(Item) then
      begin
        Color := SelectedColor;
        SCOff := 2;
      end
      else
      begin
        Color := NormalColor;
        SCOff := 4;
      end;
      MoveChar(B[CurCol], ' ', Color, ColWidth);
      if Item < Range then
      begin
        (* Text := GetText(Item, ColWidth + Indent);
        Text := Copy(Text,Indent,ColWidth); *)
        Text:=GetIndentedText(Item,Indent,ColWidth);
        MoveStr(B[CurCol+1], Text, Color);
        if ShowMarkers then
        begin
          WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
          WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
        end;
      end;
      MoveChar(B[CurCol+ColWidth-1], #179, GetColor(5), 1);
    end;
    WriteLine(0, I, Size.X, 1, B);
  end;
end;

function TWatchesListBox.GetLocalMenu: PMenu;
var M: PMenu;
begin
  if (Owner<>nil) and (Owner^.GetState(sfModal)) then M:=nil else
  M:=NewMenu(
    NewItem('~E~dit watch','',kbNoKey,cmEdit,hcNoContext,
    NewItem('~N~ew watch','',kbNoKey,cmNew,hcNoContext,
    NewItem('~D~elete watch','',kbNoKey,cmDelete,hcNoContext,
    nil))));
  GetLocalMenu:=M;
end;

procedure   TWatchesListBox.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            Message(@Self,evCommand,cmEdit,nil);
          kbIns :
            Message(@Self,evCommand,cmNew,nil);
          kbDel :
            Message(@Self,evCommand,cmDelete,nil);
        else
          DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmListItemSelected :
          if Event.InfoPtr=@Self then
            Message(@Self,evCommand,cmEdit,nil);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmEdit :
              EditCurrent;
          cmDelete :
              DeleteCurrent;
          cmNew :
              EditNew;
          else
            DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

      constructor TWatchesListBox.Load(var S: TStream);
        begin
          inherited Load(S);
          If assigned(List) then
            dispose(list,done);
          List:=WatchesCollection;
          { we must set Range PM }
          SetRange(List^.count);
        end;

      procedure   TWatchesListBox.Store(var S: TStream);
        var OL: PCollection;
            OldRange : Sw_integer;
        begin
          OL:=List;
          OldRange:=Range;
          Range:=0;
          New(List, Init(1,1));
          inherited Store(S);
          Dispose(List, Done);
          List:=OL;
          { ^^^ nasty trick - has anyone a better idea how to avoid storing the
            collection? Pasting here a modified version of TListBox.Store+
            TAdvancedListBox.Store isn't a better solution, since by eventually
            changing the obj-hierarchy you'll always have to modify this, too - BG }
          SetRange(OldRange);
        end;

      destructor  TWatchesListBox.Done;
        begin
          List:=nil;
          inherited Done;
        end;

{****************************************************************************
                         TWatchesWindow
****************************************************************************}

  Constructor TWatchesWindow.Init;
    var
      HSB,VSB: PScrollBar;
      R,R2 : trect;
    begin
      Desktop^.GetExtent(R);
      R.A.Y:=R.B.Y-5;
      inherited Init(R, 'Watches', wnNoNumber);
      Palette:=wpCyanWindow;
      GetExtent(R);
      HelpCtx:=hcWatches;
      R.Grow(-1,-1);
      R2.Copy(R);
      Inc(R2.B.Y);
      R2.A.Y:=R2.B.Y-1;
      New(HSB, Init(R2));
      HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX;
      Insert(HSB);
      R2.Copy(R);
      Inc(R2.B.X);
      R2.A.X:=R2.B.X-1;
      New(VSB, Init(R2));
      VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
      Insert(VSB);
      New(WLB,Init(R,HSB,VSB));
      WLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
      WLB^.Transparent:=true;
      Insert(WLB);
      If assigned(WatchesWindow) then
        dispose(WatchesWindow,done);
      WatchesWindow:=@Self;
      Update;
    end;

  procedure TWatchesWindow.Update;
    begin
      WatchesCollection^.Update;
      Draw;
    end;

  constructor TWatchesWindow.Load(var S: TStream);
    begin
      inherited Load(S);
      GetSubViewPtr(S,WLB);
      If assigned(WatchesWindow) then
        dispose(WatchesWindow,done);
      WatchesWindow:=@Self;
    end;

  procedure TWatchesWindow.Store(var S: TStream);
    begin
      inherited Store(S);
      PutSubViewPtr(S,WLB);
    end;

  Destructor TWatchesWindow.Done;
    begin
      WatchesWindow:=nil;
      Dispose(WLB,done);
      inherited done;
    end;


{****************************************************************************
                         TWatchItemDialog
****************************************************************************}

constructor TWatchItemDialog.Init(AWatch: PWatch);
var R,R2: TRect;
begin
  R.Assign(0,0,50,10);
  inherited Init(R,'Edit Watch');
  Watch:=AWatch;

  GetExtent(R); R.Grow(-3,-2);
  Inc(R.A.Y); R.B.Y:=R.A.Y+1; R.B.X:=R.A.X+36;
  New(NameIL, Init(R, 255)); Insert(NameIL);
  R2.Copy(R); R2.Move(-1,-1);
  Insert(New(PLabel, Init(R2, '~E~xpression to watch', NameIL)));
  GetExtent(R);
  R.Grow(-1,-1);
  R.A.Y:=R.A.Y+3;
  R.B.X:=R.A.X+36;
  TextST:=New(PAdvancedStaticText, Init(R, 'Watch values'));
  Insert(TextST);

  InsertButtons(@Self);

  NameIL^.Select;
end;

function TWatchItemDialog.Execute: Word;
var R: word;
    S1,S2: string;
begin
  S1:=GetStr(Watch^.expr);
  NameIL^.SetData(S1);

  if assigned(Watch^.Current_value) then
    S1:=GetPChar(Watch^.Current_value)
  else
    S1:='';

  if assigned(Watch^.Last_value) then
    S2:=GetPChar(Watch^.Last_value)
  else
    S2:='';

  if assigned(Watch^.Last_value) and
     assigned(Watch^.Current_value) and
     (strcomp(Watch^.Last_value,Watch^.Current_value)=0) then
    S1:='Current value: '+#13+S1
  else
    S1:='Current value: '+#13+S1+#13+
        'Previous value: '+#13+S2;

  TextST^.SetText(S1);

  R:=inherited Execute;
  if R=cmOK then
  begin
    NameIL^.GetData(S1);
    Watch^.Rename(S1);
    If assigned(Debugger) then
       Debugger^.ReadWatches;
  end;
  Execute:=R;
end;

{****************************************************************************
                         TRegistersWindow
****************************************************************************}

  constructor TRegistersView.Init(var Bounds: TRect);

    begin
       inherited init(Bounds);
    end;

  procedure TRegistersView.Draw;

    var
       p : pchar;
       s : string;

    begin
       inherited draw;
       If not assigned(Debugger) then
         begin
            WriteStr(0,0,'<no values available>',7);
            exit;
         end;
{$ifndef NODEBUG}
       Debugger^.Command('info registers');
       if Debugger^.Error then
         WriteStr(0,0,'<Debugger error>',7)
       else
         begin
            p:=StrNew(Debugger^.GetOutput);
            if assigned(p) then
              begin
                 {!!!!!!!!! here we crash!! }
                 move(p^,s,strlen(p));
                 s[0]:=chr(strlen(p));
                 WriteStr(0,0,s,7);
              end
            else
              WriteStr(0,0,'<unknown values>',7);
         end;
       { do not open a messagebox for such errors }
       Debugger^.got_error:=false;
{$endif}
    end;

  destructor TRegistersView.Done;

    begin
       inherited done;
    end;

  constructor TRegistersWindow.Init;

    var
       R : TRect;

    begin
       Desktop^.GetExtent(R);
       R.A.X:=R.B.X-24;
       R.B.Y:=8;
       inherited Init(R,' Register View', wnNoNumber);
       Flags:=wfClose or wfMove;
       Palette:=wpCyanWindow;
       HelpCtx:=hcRegisters;
       R.Assign(1,1,22,6);
       RV:=new(PRegistersView,init(R));
       Insert(RV);
       If assigned(RegistersWindow) then
         dispose(RegistersWindow,done);
       RegistersWindow:=@Self;
       Update;
    end;

  constructor TRegistersWindow.Load(var S: TStream);

    begin
       inherited load(S);
       GetSubViewPtr(S,RV);
       If assigned(RegistersWindow) then
         dispose(RegistersWindow,done);
       RegistersWindow:=@Self;
    end;

  procedure TRegistersWindow.Store(var S: TStream);

    begin
       inherited Store(s);
       PutSubViewPtr(S,RV);
    end;

  procedure TRegistersWindow.Update;

    begin
       DrawView;
    end;

  destructor TRegistersWindow.Done;

    begin
       RegistersWindow:=nil;
       inherited done;
    end;

{****************************************************************************
                         TStackWindow
****************************************************************************}

  constructor TFramesListBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
    begin
      Inherited Init(Bounds,AHScrollBar,AVScrollBar);
    end;

  procedure TFramesListBox.Update;

    var i : longint;
        W : PSourceWindow;

    begin
      { call backtrace command }
      If not assigned(Debugger) then
        exit;
    {$ifndef NODEBUG}
      Clear;
      { forget all old frames }
      Debugger^.clear_frames;

      Debugger^.Command('backtrace');
      { generate list }
      { all is in tframeentry }
      for i:=Debugger^.frame_count-1 downto 0 do
        begin
          with Debugger^.frames[i]^ do
            begin
              AddItem(new(PMessageItem,init(0,GetPChar(function_name)+GetPChar(args),
                AddModuleName(GetPChar(file_name)),line_number,1)));
              W:=SearchOnDesktop(GetPChar(file_name),false);
              If assigned(W) then
                begin
                  W^.editor^.SetDebuggerRow(line_number);
                end;
            end;
        end;
      if List^.Count > 0 then
        FocusItem(0);
     {$endif}
    end;

  function TFramesListBox.GetLocalMenu: PMenu;
    begin
      GetLocalMenu:=Inherited GetLocalMenu;
    end;

  procedure TFramesListBox.GotoSource;
    begin
      { select frame for watches }
      If not assigned(Debugger) then
        exit;
    {$ifdef NODEBUG}
      Debugger^.Command('f '+IntToStr(Focused));
      { for local vars }
      Debugger^.ReadWatches;
   {$endif}
      { goto source }
      inherited GotoSource;
    end;

  procedure   TFramesListBox.HandleEvent(var Event: TEvent);
    begin
      inherited HandleEvent(Event);
    end;

  destructor  TFramesListBox.Done;
    begin
      Inherited Done;
    end;

  Constructor TStackWindow.Init;
    var
      HSB,VSB: PScrollBar;
      R,R2 : trect;
    begin
      Desktop^.GetExtent(R);
      R.A.Y:=R.B.Y-5;
      inherited Init(R, 'Call Stack', wnNoNumber);
      Palette:=wpCyanWindow;
      GetExtent(R);
      HelpCtx:=hcStack;
      R.Grow(-1,-1);
      R2.Copy(R);
      Inc(R2.B.Y);
      R2.A.Y:=R2.B.Y-1;
      New(HSB, Init(R2));
      HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX;
      Insert(HSB);
      R2.Copy(R);
      Inc(R2.B.X);
      R2.A.X:=R2.B.X-1;
      New(VSB, Init(R2));
      VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
      Insert(VSB);
      New(FLB,Init(R,HSB,VSB));
      FLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(FLB);
      If assigned(StackWindow) then
        dispose(StackWindow,done);
      StackWindow:=@Self;
      Update;
    end;

  procedure TStackWindow.Update;
    begin
      FLB^.Update;
      DrawView;
    end;

  constructor TStackWindow.Load(var S: TStream);
    begin
      inherited Load(S);
      GetSubViewPtr(S,FLB);
      If assigned(StackWindow) then
        dispose(StackWindow,done);
      StackWindow:=@Self;
    end;

  procedure TStackWindow.Store(var S: TStream);
    begin
      inherited Store(S);
      PutSubViewPtr(S,FLB);
    end;

  Destructor TStackWindow.Done;
    begin
      StackWindow:=nil;
      Dispose(FLB,done);
      inherited done;
    end;

{****************************************************************************
                         Init/Final
****************************************************************************}

procedure InitDebugger;
begin
{$ifdef DEBUG}
  Assign(gdb_file,GDBOutFileName);
  Rewrite(gdb_file);
  Use_gdb_file:=true;
{$endif}
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
{$ifdef GDBWINDOW}
  InitGDBWindow;
{$endif def GDBWINDOW}
end;


procedure DoneDebugger;
begin
  if assigned(Debugger) then
   dispose(Debugger,Done);
  Debugger:=nil;
{$ifdef DEBUG}
  If Use_gdb_file then
    Close(GDB_file);
  Use_gdb_file:=false;
{$endif}
  {DoneGDBWindow;}
end;

procedure InitGDBWindow;
var
  R : TRect;
begin
  if GDBWindow=nil then
    begin
      DeskTop^.GetExtent(R);
      new(GDBWindow,init(R));
      DeskTop^.Insert(GDBWindow);
    end;
end;

procedure DoneGDBWindow;
begin
  if assigned(GDBWindow) then
    begin
      DeskTop^.Delete(GDBWindow);
      GDBWindow:=nil;
    end;
end;

procedure InitStackWindow;
begin
  if StackWindow=nil then
    begin
      new(StackWindow,init);
      DeskTop^.Insert(StackWindow);
    end;
end;

procedure DoneStackWindow;
begin
  if assigned(StackWindow) then
    begin
      DeskTop^.Delete(StackWindow);
      StackWindow:=nil;
    end;
end;

procedure InitRegistersWindow;
begin
  if RegistersWindow=nil then
    begin
      new(RegistersWindow,init);
      DeskTop^.Insert(RegistersWindow);
    end;
end;

procedure DoneRegistersWindow;
begin
  if assigned(RegistersWindow) then
    begin
      DeskTop^.Delete(RegistersWindow);
      RegistersWindow:=nil;
    end;
end;

procedure InitBreakpoints;
begin
  New(BreakpointsCollection,init(10,10));
end;

procedure DoneBreakpoints;
begin
  Dispose(BreakpointsCollection,Done);
  BreakpointsCollection:=nil;
end;

procedure InitWatches;
begin
  New(WatchesCollection,init);
end;

procedure DoneWatches;
begin
  Dispose(WatchesCollection,Done);
  WatchesCollection:=nil;
end;

procedure RegisterFPDebugViews;
begin
  RegisterType(RWatchesWindow);
  RegisterType(RBreakpointsWindow);
  RegisterType(RWatchesListBox);
  RegisterType(RBreakpointsListBox);
  RegisterType(RStackWindow);
  RegisterType(RFramesListBox);
  RegisterType(RBreakpoint);
  RegisterType(RWatch);
  RegisterType(RBreakpointCollection);
  RegisterType(RWatchesCollection);
  RegisterType(RRegistersWindow);
  RegisterType(RRegistersView);
end;

end.

{
  $Log$
  Revision 1.38  2000-01-09 21:05:51  florian
    * some fixes for register view

  Revision 1.37  2000/01/08 18:26:20  florian
    + added a register window, doesn't work yet

  Revision 1.36  1999/12/20 14:23:16  pierre
    * MyApp renamed IDEApp
    * TDebugController.ResetDebuggerRows added to
      get resetting of debugger rows

  Revision 1.35  1999/11/24 14:03:16  pierre
   + Executing... in status line if in another window

  Revision 1.34  1999/11/10 17:19:58  pierre
   + Other window for Debuggee code

  Revision 1.33  1999/10/25 16:39:03  pierre
   + GetPChar to avoid nil pointer problems

  Revision 1.32  1999/09/16 14:34:57  pierre
    + TBreakpoint and TWatch registering
    + WatchesCollection and BreakpointsCollection stored in desk file
    * Syntax highlighting was broken

  Revision 1.31  1999/09/13 16:24:43  peter
    + clock
    * backspace unident like tp7

  Revision 1.30  1999/09/09 16:36:30  pierre
   * Breakpoint storage problem corrected

  Revision 1.29  1999/09/09 16:31:45  pierre
   * some breakpoint related fixes and Help contexts

  Revision 1.28  1999/09/09 14:20:05  pierre
   + Stack Window

  Revision 1.27  1999/08/24 22:04:33  pierre
    + TCodeEditor.SetDebuggerRow
      works like SetHighlightRow but is only disposed by a SetDebuggerRow(-1)
      so the current stop point in debugging is not lost if
      we move the cursor

  Revision 1.26  1999/08/22 22:26:48  pierre
   + Registration of Breakpoint/Watches windows

  Revision 1.25  1999/08/16 18:25:15  peter
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

  Revision 1.24  1999/08/03 20:22:28  peter
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

  Revision 1.23  1999/07/28 23:11:17  peter
    * fixes from gabor

  Revision 1.22  1999/07/12 13:14:15  pierre
    * LineEnd bug corrected, now goes end of text even if selected
    + Until Return for debugger
    + Code for Quit inside GDB Window

  Revision 1.21  1999/07/11 00:35:14  pierre
   * fix problems for wrong watches

  Revision 1.20  1999/07/10 01:24:14  pierre
   + First implementation of watches window

  Revision 1.19  1999/06/30 23:58:12  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.18  1999/03/16 00:44:42  peter
    * forgotten in last commit :(

  Revision 1.17  1999/03/02 13:48:28  peter
    * fixed far problem is fpdebug
    * tile/cascading with message window
    * grep fixes

  Revision 1.16  1999/03/01 15:41:52  peter
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

  Revision 1.15  1999/02/20 15:18:29  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.14  1999/02/16 12:47:36  pierre
   * GDBWindow does not popup on F7 or F8 anymore

  Revision 1.13  1999/02/16 10:43:54  peter
    * use -dGDB for the compiler
    * only use gdb_file when -dDEBUG is used
    * profiler switch is now a toggle instead of radiobutton

  Revision 1.12  1999/02/11 19:07:20  pierre
    * GDBWindow redesigned :
      normal editor apart from
      that any kbEnter will send the line (for begin to cursor)
      to GDB command !
      GDBWindow opened in Debugger Menu
       still buggy :
       -echo should not be present if at end of text
       -GDBWindow becomes First after each step (I don't know why !)

  Revision 1.11  1999/02/11 13:10:03  pierre
   + GDBWindow only with -dGDBWindow for now : still buggy !!

  Revision 1.10  1999/02/10 09:55:07  pierre
    + added OldValue and CurrentValue field for watchpoints
    + InitBreakpoints and DoneBreakpoints
    + MessageBox if GDB stops bacause of a watchpoint !

  Revision 1.9  1999/02/08 17:43:43  pierre
    * RestDebugger or multiple running of debugged program now works
    + added DoContToCursor(F4)
    * Breakpoints are now inserted correctly (was mainlyy a problem
      of directories)

  Revision 1.8  1999/02/05 17:21:52  pierre
    Invalid_line renamed InvalidSourceLine

  Revision 1.7  1999/02/05 13:08:41  pierre
   + new breakpoint types added

  Revision 1.6  1999/02/05 12:11:53  pierre
    + SourceDir that stores directories for sources that the
      compiler should not know about
      Automatically asked for addition when a new file that
      needed filedialog to be found is in an unknown directory
      Stored and retrieved from INIFile
    + Breakpoints conditions added to INIFile
    * Breakpoints insterted and removed at debin and end of debug session

  Revision 1.5  1999/02/04 17:54:22  pierre
   + several commands added

  Revision 1.4  1999/02/04 13:32:02  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.3  1999/02/02 16:41:38  peter
    + automatic .pas/.pp adding by opening of file
    * better debuggerscreen changes

  Revision 1.2  1999/01/22 18:14:09  pierre
   * adaptd to changes in gdbint and gdbcon for  to /

  Revision 1.1  1999/01/22 10:24:03  peter
    * first debugger things

}