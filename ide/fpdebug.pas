{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998-2000 by Pierre Muller

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
{$ifdef win32}
  Windows,
{$endif win32}
  Objects,Dialogs,Drivers,Views,
  GDBCon,GDBInt,Menus,
  WViews,
  FPViews;

type
  PDebugController=^TDebugController;
  TDebugController=object(TGDBController)
     InvalidSourceLine : boolean;

     { if true the current debugger raw will stay in middle of
       editor window when debugging PM }
     CenterDebuggerRow : boolean;
     LastFileName : string;
     LastSource   : PView; {PsourceWindow !! }
     HiddenStepsCount : longint;
     { no need to switch if using another terminal }
     NoSwitch : boolean;
     HasExe   : boolean;
     RunCount : longint;
     WindowWidth : longint;
     FPCBreakErrorNumber : longint;
    constructor Init;
    procedure SetExe(const exefn:string);
    procedure SetWidth(AWidth : longint);
    procedure SetDirectories;
    destructor  Done;
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
{    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;}
    procedure DoEndSession(code:longint);virtual;
    procedure DoUserSignal;virtual;
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
    function  IsRunning : boolean;
    function  AllowQuit : boolean;virtual;
    function  GetValue(Const expr : string) : pchar;
    function  GetFramePointer : CORE_ADDR;
    function  GetLongintAt(addr : CORE_ADDR) : longint;
    function  GetPointerAt(addr : CORE_ADDR) : CORE_ADDR;
  end;

  BreakpointType = (bt_function,bt_file_line,bt_watch,
                    bt_awatch,bt_rwatch,bt_address,bt_invalid);
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
     constructor Init_Address(Const AAddress : String);
     constructor Init_Empty;
     constructor Init_file_line(AFile : String; ALine : longint);
     constructor Init_type(atyp : BreakpointType;Const AnExpr : String);
     constructor Load(var S: TStream);
     procedure   Store(var S: TStream);
     procedure  Insert;
     procedure  Remove;
     procedure  Enable;
     procedure  Disable;
     procedure  UpdateSource;
     procedure  ResetValues;
     destructor Done;virtual;
  end;

  TBreakpointCollection=object(TCollection)
      function  At(Index: Integer): PBreakpoint;
      function  GetGDB(index : longint) : PBreakpoint;
      function  GetType(typ : BreakpointType;Const s : String) : PBreakpoint;
      function  ToggleFileLine(FileName: String;LineNr : Longint) : boolean;
      procedure Update;
      procedure ShowBreakpoints(W : PFPWindow);
      procedure ShowAllBreakpoints;
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
    TBreakpointsWindow = object(TFPDlgWindow)
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
      expr : pstring;
    private
      GDBRunCount : longint;
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
      function    GetText (Item: Sw_Integer; MaxLen: Sw_Integer): String; Virtual;
      function    GetIndentedText(Item,Indent,MaxLen: Sw_Integer;var Modified : boolean): String; virtual;
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
    TWatchesWindow = Object(TFPDlgWindow)
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
      procedure   GotoAssembly; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    end;

    PStackWindow = ^TStackWindow;
    TStackWindow = Object(TFPDlgWindow)
      FLB : PFramesListBox;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

    {$ifdef TP} dword = longint; {$endif}

    TIntRegs = record
{$ifdef I386}
       eax,ebx,ecx,edx,eip,esi,edi,esp,ebp : dword;
       cs,ds,es,ss,fs,gs : word;
       eflags : dword;
{$endif I386}
{$ifdef m68k}
       d0,d1,d2,d3,d4,d5,d6,d7 : dword;
       a0,a1,a2,a3,a4,a5,fp,sp : dword;
       ps,pc : dword;
{$endif m68k}
    end;

    PRegistersView = ^TRegistersView;
    TRegistersView = object(TView)
      OldReg : TIntRegs;
      constructor Init(var Bounds: TRect);
      procedure   Draw;virtual;
      destructor  Done; virtual;
    end;

    PRegistersWindow = ^TRegistersWindow;
    TRegistersWindow = Object(TFPDlgWindow)
      RV : PRegistersView;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

    TFPURegs = record
{$ifdef I386}
      st0,st1,st2,st3,st4,st5,st6,st7 :string;
      ftag,fop,fctrl,fstat,fiseg,foseg : word;
      fioff,fooff : cardinal;
{$endif I386}
{$ifdef m68k}
      fp0,fp1,fp2,fp3,fp4,fp5,fp6,fp7 : string;
      fpcontrol,fpstatus,fpiaddr : dword;
{$endif m68k}
    end;

    PFPUView = ^TFPUView;
    TFPUView = object(TView)
      OldReg : TFPURegs;
      constructor Init(var Bounds: TRect);
      procedure   Draw;virtual;
      destructor  Done; virtual;
    end;

    PFPUWindow = ^TFPUWindow;
    TFPUWindow = Object(TFPDlgWindow)
      RV : PFPUView;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

  procedure InitStackWindow;
  procedure DoneStackWindow;

  procedure InitRegistersWindow;
  procedure DoneRegistersWindow;
  procedure InitFPUWindow;
  procedure DoneFPUWindow;
  function  ActiveBreakpoints : boolean;
  function  GDBFileName(st : string) : string;
  function  OSFileName(st : string) : string;


const
     BreakpointTypeStr : Array[BreakpointType] of String[9]
       = ( 'function','file-line','watch','awatch','rwatch','address','invalid');
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
procedure InitDisassemblyWindow;
procedure DoneDisassemblyWindow;
procedure InitBreakpoints;
procedure DoneBreakpoints;
procedure InitWatches;
procedure DoneWatches;

procedure RegisterFPDebugViews;

procedure UpdateDebugViews;

implementation

uses
  Dos,Video,
  App,Strings,
{$ifdef FVISION}
  FVConsts,
{$else}
  Commands,HelpCtx,
{$endif}
{$ifdef win32}
  Windebug,
{$endif win32}
{$ifdef Unix}
  {$ifdef VER1_0}
    Linux,
  {$else}
    Unix,
  {$endif}
{$endif Unix}
  Systems,Globals,
  FPString,FPVars,FPUtils,FPConst,FPSwitch,
  FPIntf,FPCompil,FPIde,FPHelp,
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

  RFPUWindow: TStreamRec = (
     ObjType: 1713;
     VmtLink: Ofs(TypeOf(TFPUWindow)^);
     Load:    @TFPUWindow.Load;
     Store:   @TFPUWindow.Store
  );

  RFPUView: TStreamRec = (
     ObjType: 1714;
     VmtLink: Ofs(TypeOf(TFPUView)^);
     Load:    @TFPUView.Load;
     Store:   @TFPUView.Store
  );

{$ifdef I386}
const
  FrameName = '$ebp';
{$define FrameNameKnown}
{$endif i386}
{$ifdef m68k}
const
  FrameName = '$fp';
{$define FrameNameKnown}
{$endif m68k}

{$ifdef TP}
function HexStr(Value: longint; Len: byte): string;
begin
  HexStr:=IntToHex(Value,Len);
end;
{$endif}


function  GDBFileName(st : string) : string;
{$ifndef Unix}
var i : longint;
{$endif Unix}
begin
{$ifdef Unix}
  GDBFileName:=st;
{$else}
{ should we also use / chars ? }
  for i:=1 to Length(st) do
    if st[i]='\' then
{$ifdef win32}
  { Don't touch at '\ ' used to escapes spaces in windows file names PM }
     if (i=length(st)) or (st[i+1]<>' ') then
{$endif win32}
      st[i]:='/';
{$ifdef win32}
{ for win32 we should convert e:\ into //e/ PM }
  if (length(st)>2) and (st[2]=':') and (st[3]='/') then
    st:=CygDrivePrefix+'/'+st[1]+copy(st,3,length(st));
{ support spaces in the name by escaping them but without changing '\ ' into '\\ ' }
  for i:=Length(st) downto 1 do
    if (st[i]=' ') and ((i=1) or (st[i-1]<>'\')) then
      st:=copy(st,1,i-1)+'\'+copy(st,i,length(st));
{$endif win32}
{$ifdef go32v2}
{ for go32v2 we should convert //e/ back into e:/  PM }
  if (length(st)>3) and (st[1]='/') and (st[2]='/') and (st[4]='/') then
    st:=st[3]+':/'+copy(st,5,length(st));
{$endif go32v2}
  GDBFileName:=LowerCaseStr(st);
{$endif}
end;

function  OSFileName(st : string) : string;
{$ifndef Unix}
var i : longint;
{$endif Unix}
begin
{$ifdef Unix}
  OSFileName:=st;
{$else}
{$ifdef win32}
{ for win32 we should convert /cygdrive/e/ into e:\ PM }
  if pos(CygDrivePrefix+'/',st)=1 then
    st:=st[Length(CygdrivePrefix)+2]+':\'+copy(st,length(CygdrivePrefix)+4,length(st));
{$endif win32}
{ support spaces in the name by escaping them but without changing '\ ' into '\\ ' }
  for i:=Length(st) downto 2 do
    if (st[i]=' ') and (st[i-1]='\') then
      st:=copy(st,1,i-2)+copy(st,i,length(st));
{$ifdef go32v2}
{ for go32v2 we should convert //e/ back into e:/  PM }
  if (length(st)>3) and (st[1]='/') and (st[2]='/') and (st[4]='/') then
    st:=st[3]+':\'+copy(st,5,length(st));
{$endif go32v2}
{ should we also use / chars ? }
  for i:=1 to Length(st) do
    if st[i]='/' then
      st[i]:='\';
  OSFileName:=LowerCaseStr(st);
{$endif}
end;

{****************************************************************************
                            TDebugController
****************************************************************************}

procedure UpdateDebugViews;

  begin
     DeskTop^.Lock;
     If assigned(StackWindow) then
       StackWindow^.Update;
     If assigned(RegistersWindow) then
       RegistersWindow^.Update;
     If assigned(Debugger) then
       Debugger^.ReadWatches;
     If assigned(FPUWindow) then
       FPUWindow^.Update;
     DeskTop^.UnLock;
  end;

constructor TDebugController.Init;
begin
  inherited Init;
  CenterDebuggerRow:=IniCenterDebuggerRow;
  NoSwitch:=False;
  HasExe:=false;
  Debugger:=@self;
  WindowWidth:=-1;
{$ifndef GABOR}
  switch_to_user:=true;
{$endif}
  Command('set print object off');
end;

procedure TDebugController.SetExe(const exefn:string);
  var f : string;
begin
  f := GDBFileName(GetShortName(exefn));
  if (f<>'') and ExistsFile(exefn) then
    begin
      LoadFile(f);
      HasExe:=true;
      Command('b FPC_BREAK_ERROR');
      FPCBreakErrorNumber:=last_breakpoint_number;
{$ifdef FrameNameKnown}
      { this fails in GDB 5.1 because
        GDB replies that there is an attempt to dereference
        a generic pointer...
        test delayed in DoSourceLine... PM
      Command('cond '+IntToStr(FPCBreakErrorNumber)+
        ' (('+FrameName+' + 8)^ <> 0) or'+
        ' (('+FrameName+' + 12)^ <> 0)');  }
{$endif FrameNameKnown}
      SetArgs(GetRunParameters);
      SetDirectories;
      InsertBreakpoints;
      ReadWatches;
    end
  else
    begin
      HasExe:=false;
      Command('file');
    end;
end;

procedure TDebugController.SetWidth(AWidth : longint);
begin
  WindowWidth:=AWidth;
  Command('set width '+inttostr(WindowWidth));
end;

procedure TDebugController.SetDirectories;
  var f,s: string;
      i : longint;
      Dir : SearchRec;
begin
  f:=GetSourceDirectories;
  repeat
    i:=pos(';',f);
    if i=0 then
        s:=f
    else
      begin
        s:=copy(f,1,i-1);
        system.delete(f,1,i);
      end;
    DefaultReplacements(s);
    if (pos('*',s)=0) and ExistsDir(s) then
      Command('dir '+GDBFileName(GetShortName(s)))
    { we should also handle the /* cases of -Fu option }
    else if pos('*',s)>0 then
      begin
        Dos.FindFirst(s,Directory,Dir);
        { the '*' can only be in the last dir level }
        s:=DirOf(s);
        while Dos.DosError=0 do
          begin
            if ((Dir.attr and Directory) <> 0) and ExistsDir(s+Dir.Name) then
              Command('dir '+GDBFileName(GetShortName(s+Dir.Name)));
            Dos.FindNext(Dir);
          end;
{$ifdef FPC}
        Dos.FindClose(Dir);
{$endif def FPC}
      end;
  until i=0;
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

function  ActiveBreakpoints : boolean;
  var
    IsActive : boolean;

  procedure TestActive(PB : PBreakpoint);
    begin
        If PB^.state=bs_enabled then
          IsActive:=true;
    end;
begin
   IsActive:=false;
   If assigned(BreakpointsCollection) then
     BreakpointsCollection^.ForEach(@TestActive);
   ActiveBreakpoints:=IsActive;
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
{$ifdef Unix}
  { Run the debuggee in another tty }
  if DebuggeeTTY <> '' then
    begin
      Command('tty '+DebuggeeTTY);
      if DebuggeeTTY<>TTYName(stdout) then
        NoSwitch:= true
      else
        NoSwitch:=false;
    end
  else
    begin
      if TTYName(input)<>'' then
        Command('tty '+TTYName(input));
      NoSwitch := false;
    end;
{$endif Unix}
  { Switch to user screen to get correct handles }
  UserScreen;
  { Don't try to print GDB messages while in User Screen mode }
  If assigned(GDBWindow) then
    GDBWindow^.Editor^.Lock;
  inherited Run;
  DebuggerScreen;
  If assigned(GDBWindow) then
    GDBWindow^.Editor^.UnLock;
  IDEApp.SetCmdState([cmResetDebugger,cmUntilReturn],true);
  UpdateDebugViews;
end;


function TDebugController.IsRunning : boolean;
begin
  IsRunning:=debuggee_started;
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
  if IsRunning then
    begin
      if ConfirmBox('Really quit GDB window'#13+
         'and kill running program?',nil,true)=cmYes then
        begin
           Reset;
           DoneGDBWindow;
           {AllowQuit:=true;}
           AllowQuit:=false;
        end
      else
        AllowQuit:=false;
    end
  else if ConfirmBox('Really quit GDB window?',nil,true)=cmYes then
    begin
      DoneGDBWindow;
      {AllowQuit:=true;}
      AllowQuit:=false;
    end
  else
    AllowQuit:=false;
end;

procedure TDebugController.ResetDebuggerRows;
  procedure ResetDebuggerRow(P: PView); {$ifndef FPC}far;{$endif}
  begin
    if assigned(P) and
       (TypeOf(P^)=TypeOf(TSourceWindow)) then
       PSourceWindow(P)^.Editor^.SetLineFlagExclusive(lfDebuggerRow,-1);
  end;

begin
  Desktop^.ForEach(@ResetDebuggerRow);
end;

procedure TDebugController.Reset;
begin
  inherited Reset;
  { we need to free the executable
    if we want to recompile it }
  SetExe('');
  NoSwitch:=false;
  { In case we have something that the compiler touched }
  If IDEApp.IsRunning then
    begin
      IDEApp.SetCmdState([cmResetDebugger,cmUntilReturn],false);
      AskToReloadAllModifiedFiles;
      ResetDebuggerRows;
    end;
end;

procedure TDebugController.AnnotateError;
var errornb : longint;
begin
  if error then
    begin
       errornb:=error_num;
       UpdateDebugViews;
       ErrorBox(#3'Error within GDB'#13#3'Error code = %d',@errornb);
    end;
end;

function TDebugController.GetValue(Const expr : string) : pchar;
var
  p,p2,p3 : pchar;
begin
  if WindowWidth<>-1 then
    Command('set width 0xffffffff');
  Command('p '+expr);
  p:=GetOutput;
  p3:=nil;
  if assigned(p) and (p[strlen(p)-1]=#10) then
   begin
     p3:=p+strlen(p)-1;
     p3^:=#0;
   end;
  if assigned(p) then
    p2:=strpos(p,'=')
  else
    p2:=nil;
  if assigned(p2) then
    p:=p2+1;
  while p^ in [' ',TAB] do
    inc(p);
  { get rid of type }
  if p^ = '(' then
    p:=strpos(p,')')+1;
  while p^ in [' ',TAB] do
    inc(p);
  if assigned(p) then
    GetValue:=StrNew(p)
  else
    GetValue:=StrNew(GetError);
  if assigned(p3) then
    p3^:=#10;
  got_error:=false;
  if WindowWidth<>-1 then
    Command('set width '+IntToStr(WindowWidth));
end;

function TDebugController.GetFramePointer : CORE_ADDR;
var
  st : string;
  p : longint;
begin
{$ifdef FrameNameKnown}
  Command('p /d '+FrameName);
  st:=strpas(GetOutput);
  p:=pos('=',st);
  while (p<length(st)) and (st[p+1] in [' ',#9]) do
    inc(p);
  Delete(st,1,p);
  p:=1;
  while (st[p] in ['0'..'9']) do
    inc(p);
  Delete(st,p,High(st));
  GetFramePointer:=StrToCard(st);
{$else not FrameNameKnown}
  GetFramePointer:=0;
{$endif not FrameNameKnown}
end;

function TDebugController.GetLongintAt(addr : CORE_ADDR) : longint;
var
  st : string;
  p : longint;
begin
  Command('x /wd 0x'+hexstr(addr,8));
  st:=strpas(GetOutput);
  p:=pos(':',st);
  while (p<length(st)) and (st[p+1] in [' ',#9]) do
    inc(p);
  Delete(st,1,p);
  p:=1;
  while (st[p] in ['0'..'9']) do
    inc(p);
  Delete(st,p,High(st));
  GetLongintAt:=StrToInt(st);
end;

function TDebugController.GetPointerAt(addr : CORE_ADDR) : CORE_ADDR;
var
  val : CORE_ADDR;
  st : string;
  p : longint;
begin
  Command('x /wx 0x'+hexstr(addr,8));
  st:=strpas(GetOutput);
  p:=pos(':',st);
  while (p<length(st)) and (st[p+1] in [' ',#9]) do
    inc(p);
  if (p<length(st)) and (st[p+1]='$') then
    inc(p);
  Delete(st,1,p);
  p:=1;
  while (st[p] in ['0'..'9','A'..'F','a'..'f']) do
    inc(p);
  Delete(st,p,High(st));
  GetPointerAt:=HexToCard(st);
end;

procedure TDebugController.DoSelectSourceLine(const fn:string;line:longint);
var
  W: PSourceWindow;
  Found : boolean;
  PB : PBreakpoint;
  S : String;
  BreakIndex : longint;
  ebp,stop_addr : CORE_ADDR;
  i,ExitCode : longint;
  ExitAddr,ExitFrame : CORE_ADDR;
const
  FirstArgOffset = 2 * sizeof(CORE_ADDR);
  SecondArgOffset = 3 * sizeof(CORE_ADDR);
  ThirdArgOffset = 4 * sizeof(CORE_ADDR);

begin
  BreakIndex:=stop_breakpoint_number;
  Desktop^.Lock;
  { 0 based line count in Editor }
  if Line>0 then
    dec(Line);

  S:=fn;
  stop_addr:=current_pc;

  if (BreakIndex=FPCBreakErrorNumber) then
    begin
      { Procedure HandleErrorAddrFrame
         (Errno : longint;addr,frame : longint);
         [public,alias:'FPC_BREAK_ERROR']; }
{$ifdef FrameNameKnown}
      ExitCode:=GetLongintAt(GetFramePointer+FirstArgOffset);
      ExitAddr:=GetPointerAt(GetFramePointer+SecondArgOffset);
      ExitFrame:=GetPointerAt(GetFramePointer+ThirdArgOffset);
      if (ExitCode=0) and (ExitAddr=0) then
        begin
          Desktop^.Unlock;
          Command('continue');
          exit;
        end;
      { forget all old frames }
      clear_frames;
      { record new frames }
      Command('backtrace');
      for i:=0 to frame_count-1 do
        begin
          with frames[i]^ do
            begin
              if ExitAddr=address then
                begin
                  Command('f '+IntToStr(i));
                  if assigned(file_name) then
                    begin
                      s:=strpas(file_name);
                      line:=line_number;
                      stop_addr:=address;
                    end;
                  break;
                end;
            end;
        end;
{$endif FrameNameKnown}
    end;
  { Update Disassembly position }
  if Assigned(DisassemblyWindow) then
    DisassemblyWindow^.SetCurAddress(stop_addr);

  if (fn=LastFileName) then
    begin
      W:=PSourceWindow(LastSource);
      if assigned(W) then
        begin
          W^.Editor^.SetCurPtr(0,Line);
          W^.Editor^.TrackCursor(CenterDebuggerRow);
          W^.Editor^.SetLineFlagExclusive(lfDebuggerRow,Line);
          UpdateDebugViews;

          {if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
            handled by SelectInDebugSession}
          W^.SelectInDebugSession;
          InvalidSourceLine:=false;
        end
      else
        InvalidSourceLine:=true;
    end
  else
    begin
      if s='' then
        W:=nil
      else
        W:=TryToOpenFile(nil,s,0,Line,false);
      if assigned(W) then
        begin
          W^.Editor^.SetLineFlagExclusive(lfDebuggerRow,Line);
          W^.Editor^.TrackCursor(CenterDebuggerRow);
          UpdateDebugViews;
          {if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
            handled by SelectInDebugSession}
          W^.SelectInDebugSession;
          LastSource:=W;
          InvalidSourceLine:=false;
        end
        { only search a file once }
      else
       begin
         Desktop^.UnLock;
         if s='' then
           Found:=false
         else
         { it is easier to handle with a * at the end }
           Found:=IDEApp.OpenSearch(s+'*');
         Desktop^.Lock;
         if not Found then
           begin
             InvalidSourceLine:=true;
             LastSource:=Nil;
             { Show the stack in that case }
             InitStackWindow;
             UpdateDebugViews;
             StackWindow^.MakeFirst;
           end
         else
           begin
             { should now be open }
              W:=TryToOpenFile(nil,s,0,Line,true);
              W^.Editor^.SetLineFlagExclusive(lfDebuggerRow,Line);
              W^.Editor^.TrackCursor(CenterDebuggerRow);
              UpdateDebugViews;
              {if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
                handled by SelectInDebugSession}
              W^.SelectInDebugSession;
              LastSource:=W;
              InvalidSourceLine:=false;
           end;
       end;
    end;
  LastFileName:=s;
  Desktop^.UnLock;
  if BreakIndex>0 then
    begin
      PB:=BreakpointsCollection^.GetGDB(BreakIndex);
      if (BreakIndex=FPCBreakErrorNumber) then
       begin
          if (ExitCode<>0) or (ExitAddr<>0) then
            WarningBox(#3'Run Time Error '+IntToStr(ExitCode)+#13+
                     #3'Error address $'+IntToHex(ExitAddr,8),nil)
          else
            WarningBox(#3'Run Time Error',nil);
       end
      else if not assigned(PB) then
        begin
          WarningBox(#3'Stopped by breakpoint '+IntToStr(BreakIndex),nil);
        end
      { For watch we should get old and new value !! }
      else if (Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive)) and
         (PB^.typ<>bt_file_line) and (PB^.typ<>bt_function) and
         (PB^.typ<>bt_address) then
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

procedure TDebugController.DoUserSignal;
var P :Array[1..2] of pstring;
    S1, S2 : string;
begin
  S1:=strpas(signal_name);
  S2:=strpas(signal_string);
  P[1]:=@S1;
  P[2]:=@S2;
  WarningBox(msg_programsignal,@P);
end;

procedure TDebugController.DoEndSession(code:longint);
var P :Array[1..2] of longint;
begin
   IDEApp.SetCmdState([cmResetDebugger],false);
   ResetDebuggerRows;
   LastExitCode:=Code;
   If HiddenStepsCount=0 then
     InformationBox(msg_programexitedwithexitcode,@code)
   else
     begin
        P[1]:=code;
        P[2]:=HiddenStepsCount;
        WarningBox(msg_programexitedwithcodeandsteps,@P);
     end;
  { In case we have something that the compiler touched }
  AskToReloadAllModifiedFiles;
{$ifdef win32}
  main_pid_valid:=false;
{$endif win32}
end;


procedure TDebugController.DoDebuggerScreen;
{$ifdef win32}
  var
   IdeMode : DWord;
{$endif win32}
begin
  if NoSwitch then
    begin
      PopStatus;
    end
  else
    begin
      IDEApp.ShowIDEScreen;
      Message(Application,evBroadcast,cmDebuggerStopped,pointer(RunCount));
      PopStatus;
    end;
{$ifdef win32}
   if NoSwitch then
     begin
       { Ctrl-C as normal char }
       GetConsoleMode(GetStdHandle(Std_Input_Handle), @IdeMode);
       IdeMode:=(IdeMode or ENABLE_MOUSE_INPUT or ENABLE_WINDOW_INPUT) and not ENABLE_PROCESSED_INPUT;
       SetConsoleMode(GetStdHandle(Std_Input_Handle), IdeMode);
     end;
   ChangeDebuggeeWindowTitleTo(Stopped_State);
{$endif win32}
end;


procedure TDebugController.DoUserScreen;

{$ifdef win32}
  var
   IdeMode : DWord;
{$endif win32}
begin
  Inc(RunCount);
  if NoSwitch then
    begin
{$ifdef Unix}
      PushStatus(msg_runninginanotherwindow+DebuggeeTTY);
{$else not Unix}
      PushStatus(msg_runninginanotherwindow);
{$endif Unix}
    end
  else
    begin
      PushStatus(msg_runningprogram);
      IDEApp.ShowUserScreen;
    end;
{$ifdef win32}
   if NoSwitch then
     begin
       { Ctrl-C as interrupt }
       GetConsoleMode(GetStdHandle(Std_Input_Handle), @IdeMode);
       IdeMode:=(IdeMode or ENABLE_MOUSE_INPUT or ENABLE_PROCESSED_INPUT or ENABLE_WINDOW_INPUT);
       SetConsoleMode(GetStdHandle(Std_Input_Handle), IdeMode);
     end;
   ChangeDebuggeeWindowTitleTo(Running_State);
{$endif win32}
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

constructor TBreakpoint.Init_Address(Const AAddress : String);
begin
  typ:=bt_address;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=NewStr(AAddress);
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
var
  CurDir : String;
begin
  typ:=bt_file_line;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  { d:test.pas:12 does not work !! }
  { I do not know how to solve this if
  if (Length(AFile)>1) and (AFile[2]=':') then
    AFile:=Copy(AFile,3,255);        }
{$ifdef Unix}
  CurDir:=GetCurDir;
{$else}
  CurDir:=LowerCaseStr(GetCurDir);
{$endif Unix}
  if Pos(CurDir,OSFileName(FEXpand(AFile)))=1 then
    FileName:=NewStr(Copy(OSFileName(FExpand(AFile)),length(CurDir)+1,255))
  else
    FileName:=NewStr(OSFileName(FExpand(AFile)));
  Name:=nil;
  Line:=ALine;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Load(var S: TStream);
var
  FName : PString;
begin
  S.Read(typ,SizeOf(BreakpointType));
  S.Read(state,SizeOf(BreakpointState));
  GDBState:=bs_deleted;
  case typ of
    bt_file_line :
      begin
        { convert to current target }
        FName:=S.ReadStr;
        FileName:=NewStr(OSFileName(GetStr(FName)));
        If Assigned(FName) then
          DisposeStr(FName);
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
var
  St : String;
begin
  S.Write(typ,SizeOf(BreakpointType));
  S.Write(state,SizeOf(BreakpointState));
  case typ of
    bt_file_line :
      begin
        st:=OSFileName(GetStr(FileName));
        S.WriteStr(@St);
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
  var
    p,p2 : pchar;
    st : string;
begin
  If not assigned(Debugger) then Exit;
  Remove;
  Debugger^.last_breakpoint_number:=0;
  if (GDBState=bs_deleted) and (state=bs_enabled) then
    begin
      if (typ=bt_file_line) and assigned(FileName) then
        Debugger^.Command('break '+GDBFileName(NameAndExtOf(GetStr(FileName)))+':'+IntToStr(Line))
      else if (typ=bt_function) and assigned(name) then
        Debugger^.Command('break '+name^)
      else if (typ=bt_address) and assigned(name) then
        Debugger^.Command('break *0x'+name^)
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
              Debugger^.Command('command '+IntToStr(GDBIndex));
              p:=commands;
              while assigned(p) do
                begin
                  p2:=strscan(p,#10);
                  if assigned(p2) then
                      p2^:=#0;
                  st:=strpas(p);
                  Debugger^.command(st);
                  if assigned(p2) then
                      p2^:=#10;
                  p:=p2;
                  if assigned(p) then
                    inc(p);
                end;
              Debugger^.Command('end');
            end;
        end
      else
      { Here there was a problem !! }
        begin
          GDBIndex:=0;
          if (typ=bt_file_line) and assigned(FileName) then
            begin
              ClearFormatParams;
              AddFormatParamStr(NameAndExtOf(FileName^));
              AddFormatParamInt(Line);
              ErrorBox(msg_couldnotsetbreakpointat,@FormatParams);
            end
          else
            begin
              ClearFormatParams;
              AddFormatParamStr(BreakpointTypeStr[typ]);
              AddFormatParamStr(GetStr(Name));
              ErrorBox(msg_couldnotsetbreakpointtype,@FormatParams);
            end;
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

procedure  TBreakpoint.UpdateSource;
var W: PSourceWindow;
    b : boolean;
begin
  if typ=bt_file_line then
    begin
      W:=SearchOnDesktop(FExpand(OSFileName(GetStr(FileName))),false);
      If assigned(W) then
        begin
          if state=bs_enabled then
            b:=true
          else
            b:=false;
          W^.Editor^.SetLineFlagState(Line-1,lfBreakpoint,b);
        end;
    end;
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

procedure TBreakpointCollection.ShowBreakpoints(W : PFPWindow);

  procedure SetInSource(P : PBreakpoint);{$ifndef FPC}far;{$endif}
  begin
    If assigned(P^.FileName) and
      (OSFileName(FExpand(P^.FileName^))=OSFileName(FExpand(PSourceWindow(W)^.Editor^.FileName))) then
      PSourceWindow(W)^.Editor^.SetLineFlagState(P^.Line-1,lfBreakpoint,P^.state=bs_enabled);
  end;

  procedure SetInDisassembly(P : PBreakpoint);{$ifndef FPC}far;{$endif}
    var
      PDL : PDisasLine;
      S : string;
      ps,qs,i : longint;
  begin
    for i:=0 to PDisassemblyWindow(W)^.Editor^.GetLineCount-1 do
      begin
        PDL:=PDisasLine(PDisassemblyWindow(W)^.Editor^.GetLine(i));
        if PDL^.Address=0 then
          begin
            if (P^.typ=bt_file_line) then
              begin
                S:=PDisassemblyWindow(W)^.Editor^.GetDisplayText(i);
                ps:=pos(':',S);
                qs:=pos(' ',copy(S,ps+1,High(S)));
                if (GDBFileName(FExpand(P^.FileName^))=GDBFileName(FExpand(Copy(S,1,ps-1)))) and
                   (StrToInt(copy(S,ps+1,qs-1))=P^.line) then
                  PDisassemblyWindow(W)^.Editor^.SetLineFlagState(i,lfBreakpoint,P^.state=bs_enabled);
              end;
          end
        else
          begin
            If (P^.typ=bt_address) and (PDL^.Address=HexToCard(P^.Name^)) then
              PDisassemblyWindow(W)^.Editor^.SetLineFlagState(i,lfBreakpoint,P^.state=bs_enabled);
          end;
      end;
  end;

begin
  if W=PFPWindow(DisassemblyWindow) then
    ForEach(@SetInDisassembly)
  else
    ForEach(@SetInSource);
end;

procedure TBreakpointCollection.ShowAllBreakpoints;

  procedure SetInSource(P : PBreakpoint);{$ifndef FPC}far;{$endif}
    var
      W : PSourceWindow;
  begin
    If assigned(P^.FileName) then
      begin
        W:=SearchOnDesktop(P^.FileName^,false);
        if assigned(W) then
          W^.Editor^.SetLineFlagState(P^.Line-1,lfBreakpoint,P^.state=bs_enabled);
      end;
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

function TBreakpointCollection.ToggleFileLine(FileName: String;LineNr : Longint) : boolean;

var PB : PBreakpoint;

  function IsThere(P : PBreakpoint) : boolean;{$ifndef FPC}far;{$endif}
  begin
    IsThere:=(P^.typ=bt_file_line) and (OSFileName(FExpand(P^.FileName^))=FileName) and (P^.Line=LineNr);
  end;
begin
    FileName:=OSFileName(FileName);
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
    if assigned(PB) then
      PB^.UpdateSource;
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
    NewItem(menu_bplocal_gotosource,'',kbNoKey,cmMsgGotoSource,hcMsgGotoSource,
    NewItem(menu_bplocal_editbreakpoint,'',kbNoKey,cmEditBreakpoint,hcEditBreakpoint,
    NewItem(menu_bplocal_newbreakpoint,'',kbNoKey,cmNewBreakpoint,hcNewBreakpoint,
    NewItem(menu_bplocal_deletebreakpoint,'',kbNoKey,cmDeleteBreakpoint,hcDeleteBreakpoint,
    NewItem(menu_bplocal_togglestate,'',kbNoKey,cmToggleBreakpoint,hcToggleBreakpoint,
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
          kbIns :
            Message(@Self,evCommand,cmNewBreakpoint,nil);
          kbDel :
            Message(@Self,evCommand,cmDeleteBreakpoint,nil);
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
  P^.Breakpoint^.UpdateSource;
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
      W^.Editor^.SetLineFlagExclusive(lfHighlightRow,P^.Breakpoint^.Line);
    end;
  if Assigned(Owner) then
    Owner^.Select;
  Desktop^.UnLock;
end;

procedure TBreakpointsListBox.ToggleCurrent;
var
  P: PBreakpointItem;
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  if P^.Breakpoint^.state=bs_enabled then
    P^.Breakpoint^.state:=bs_disabled
  else if P^.Breakpoint^.state=bs_disabled then
    P^.Breakpoint^.state:=bs_enabled;
  P^.Breakpoint^.UpdateSource;
  BreakpointsCollection^.Update;
end;

procedure TBreakpointsListBox.EditCurrent;
var
  P: PBreakpointItem;
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  Application^.ExecuteDialog(New(PBreakpointItemDialog,Init(P^.Breakpoint)),nil);
  P^.Breakpoint^.UpdateSource;
  BreakpointsCollection^.Update;
end;

procedure TBreakpointsListBox.DeleteCurrent;
var
  P: PBreakpointItem;
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  { delete it form source window }
  P^.Breakpoint^.state:=bs_disabled;
  P^.Breakpoint^.UpdateSource;
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
      P^.UpdateSource;
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
    Btn: PButton;
begin
  Desktop^.GetExtent(R); R.A.Y:=R.B.Y-18;
  inherited Init(R, dialog_breakpointlist, wnNoNumber);

  HelpCtx:=hcBreakpointListWindow;

  GetExtent(R); R.Grow(-1,-1); R.B.Y:=R.A.Y+1;
  S:=label_breakpointpropheader;
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
  HSB^.SetStep(R.B.X-R.A.X-2,1);
  R2.Copy(R); Inc(R2.B.X); R2.A.X:=R2.B.X-1;
  New(VSB, Init(R2)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  VSB^.SetStep(R.B.Y-R.A.Y-2,1);
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
  New(Btn, Init(R, button_Close, cmClose, bfDefault));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  New(Btn, Init(R, button_New, cmNewBreakpoint, bfNormal));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  New(Btn, Init(R, button_Edit, cmEditBreakpoint, bfNormal));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  New(Btn, Init(R, button_Delete, cmDeleteBreakpoint, bfNormal));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
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

  R.Assign(0,0,60,Max(9+KeyCount,18));
  inherited Init(R,dialog_modifynewbreakpoint);
  Breakpoint:=ABreakpoint;

  GetExtent(R); R.Grow(-3,-2); R3.Copy(R);
  Inc(R.A.Y); R.B.Y:=R.A.Y+1;
  New(NameIL, Init(R, 255)); Insert(NameIL);
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, label_breakpoint_name, NameIL)));
  R.Move(0,3);
  New(ConditionsIL, Init(R, 255)); Insert(ConditionsIL);
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, label_breakpoint_conditions, ConditionsIL)));
  R.Move(0,3); R.B.X:=R.A.X+36;
  New(LineIL, Init(R, 128)); Insert(LineIL);
  LineIL^.SetValidator(New(PRangeValidator, Init(0,MaxInt)));
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, label_breakpoint_line, LineIL)));
  R.Move(0,3);
  New(IgnoreIL, Init(R, 128)); Insert(IgnoreIL);
  IgnoreIL^.SetValidator(New(PRangeValidator, Init(0,MaxInt)));
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, label_breakpoint_ignorecount, IgnoreIL)));

  R.Copy(R3); Inc(R.A.X,38); Inc(R.A.Y,7); R.B.Y:=R.A.Y+KeyCount;
  Items:=nil;
  { don't use invalid type }
  for I:=pred(high(BreakpointType)) downto low(BreakpointType) do
    Items:=NewSItem(BreakpointTypeStr[I], Items);
  New(TypeRB, Init(R, Items));

  R2.Copy(R); R2.Move(-1,-1); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, label_breakpoint_type, TypeRB)));

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
    GDBRunCount:=-1;
  end;

constructor TWatch.Load(var S: TStream);
  begin
    expr:=S.ReadStr;
    last_value:=nil;
    current_value:=nil;
    Get_new_value;
    GDBRunCount:=-1;
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
    GDBRunCount:=-1;
    Get_new_value;
  end;

procedure TWatch.Get_new_value;
  var p, q : pchar;
      i, j, curframe, startframe : longint;
      s,s2 : string;
      loop_higher, found : boolean;
      last_removed : char;

    function GetValue(var s : string) : boolean;
      begin
        Debugger^.command('p '+s);
        if not Debugger^.Error then
          begin
            s:=StrPas(Debugger^.GetOutput);
            GetValue:=true;
          end
        else
          begin
            s:=StrPas(Debugger^.GetError);
            GetValue:=false;
            { do not open a messagebox for such errors }
            Debugger^.got_error:=false;
          end;
      end;

  begin
    If not assigned(Debugger) or Not Debugger^.HasExe or
       (GDBRunCount=Debugger^.RunCount) then
      exit;
    GDBRunCount:=Debugger^.RunCount;
    if assigned(last_value) then
      strdispose(last_value);
    last_value:=current_value;
    s:=GetStr(expr);
    found:=GetValue(s);
    Debugger^.got_error:=false;
    loop_higher:=not found;
    if not found then
      begin
        curframe:=Debugger^.get_current_frame;
        startframe:=curframe;
      end
    else
      begin
        curframe:=0;
        startframe:=0;
      end;
    while loop_higher do
      begin
         s:='parent_ebp';
         if GetValue(s) then
           begin
             repeat
               inc(curframe);
               if not Debugger^.set_current_frame(curframe) then
                 loop_higher:=false;
               s2:='/x $ebp';
               getValue(s2);
               j:=pos('=',s2);
               if j>0 then
                 s2:=copy(s2,j+1,length(s2));
               while s2[1] in [' ',TAB] do
                 delete(s2,1,1);
               if pos(s2,s)>0 then
                 loop_higher :=false;
             until not loop_higher;
             { try again at that level }
             s:=GetStr(expr);
             found:=GetValue(s);
             loop_higher:=not found;
           end
         else
           loop_higher:=false;
      end;
    if found then
      p:=StrNew(Debugger^.GetOutput)
    else
      begin
        { get a reasonable output at least }
        s:=GetStr(expr);
        GetValue(s);
        p:=StrNew(Debugger^.GetError);
      end;
    Debugger^.got_error:=false;
    { We should try here to find the expr in parent
      procedure if there are
      I will implement this as I added a
      parent_ebp pseudo local var to local procedure
      in stabs debug info PM }
    { But there are some pitfalls like
      locals redefined in other sublocals that call the function }
    if curframe<>startframe then
      Debugger^.set_current_frame(startframe);

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
        while (i>1) and ((q[i-2]=' ') or (q[i-2]=#9)) do
          dec(i);
        last_removed:=q[i-1];
        q[i-1]:=#0;
      end
    else
      last_removed:=#0;
    if assigned(q) then
      current_value:=strnew(q)
    else
      current_value:=strnew('');
    if last_removed<>#0 then
      q[i-1]:=last_removed;
    strdispose(p);
    GDBRunCount:=Debugger^.RunCount;
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
                W1:=StrLen(P^.Current_value)+3+Length(GetStr(P^.expr))
              else
                W1:=2+Length(GetStr(P^.expr));
              if W1>W then
                W:=W1;
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
  if (HScrollBar<>nil) and (R.B.X-R.A.X<MaxWidth) then
    HScrollBar^.SetRange(0,MaxWidth-(R.B.X-R.A.X))
  else
    HScrollBar^.SetRange(0,0);
  if R.B.X-R.A.X>MaxWidth then
    HScrollBar^.Hide
  else
    HScrollBar^.Show;
  SetRange(List^.Count+1);
  if R.B.Y-R.A.Y>Range then
    VScrollBar^.Hide
  else
    VScrollBar^.Show;

  {if Focused=List^.Count-1-1 then
     FocusItem(List^.Count-1);
     What was that for ?? PM }
  DrawView;
end;

function    TWatchesListBox.GetIndentedText(Item,Indent,MaxLen: Sw_Integer;var Modified : boolean): String;
var
  PW : PWatch;
  ValOffset : Sw_integer;
  S : String;
begin
  Modified:=false;
  if Item>=WatchesCollection^.Count then
    begin
      GetIndentedText:='';
      exit;
    end;

  PW:=WatchesCollection^.At(Item);
  ValOffset:=Length(GetStr(PW^.Expr))+2;
  if not assigned(PW^.expr) then
    GetIndentedText:=''
  else if Indent<ValOffset then
    begin
      S:=GetStr(PW^.Expr);
      if Indent=0 then
        S:=' '+S
      else
        S:=Copy(S,Indent,High(S));
      if not assigned(PW^.current_value) then
        S:=S+' <Unknown value>'
      else
        S:=S+' '+GetPChar(PW^.Current_value);
      GetIndentedText:=Copy(S,1,MaxLen);
    end
  else
   begin
      if not assigned(PW^.Current_value) or
         (StrLen(PW^.Current_value)<Indent-Valoffset) then
        S:=''
      else
        S:=GetPchar(@(PW^.Current_Value[Indent-Valoffset]));
      GetIndentedText:=Copy(S,1,MaxLen);
   end;
   if assigned(PW^.current_value) and
      assigned(PW^.last_value) and
      (strcomp(PW^.Last_value,PW^.Current_value)<>0) then
     Modified:=true;
end;

procedure TWatchesListBox.EditCurrent;
var
  P: PWatch;
begin
  if Range=0 then Exit;
  if Focused<WatchesCollection^.Count then
    P:=WatchesCollection^.At(Focused)
  else
    P:=New(PWatch,Init(''));
  Application^.ExecuteDialog(New(PWatchItemDialog,Init(P)),nil);
  WatchesCollection^.Update;
end;

function    TWatchesListBox.GetText (Item: Sw_Integer; MaxLen: Sw_Integer): String;
var
  Dummy_Modified : boolean;
begin
  GetText:=GetIndentedText(Item, 0, MaxLen, Dummy_Modified);
end;

procedure TWatchesListBox.DeleteCurrent;
var
  P: PWatch;
begin
  if (Range=0) or
     (Focused>=WatchesCollection^.Count) then
    exit;
  P:=WatchesCollection^.At(Focused);
  WatchesCollection^.free(P);
  WatchesCollection^.Update;
end;

procedure TWatchesListBox.EditNew;
var
  P: PWatch;
  S : string;
begin
  if Focused<WatchesCollection^.Count then
    begin
      P:=WatchesCollection^.At(Focused);
      S:=GetStr(P^.expr);
    end
  else
    S:='';
  P:=New(PWatch,Init(S));
  if Application^.ExecuteDialog(New(PWatchItemDialog,Init(P)),nil)<>cmCancel then
    begin
      WatchesCollection^.AtInsert(Focused,P);
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
  Modified : boolean;
  Text: String;
  SCOff: Byte;
  TC: byte;
  procedure MT(var C: word);
    begin
      if TC<>0 then C:=(C and $ff0f) or (TC and $f0);
    end;
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
        Text:=GetIndentedText(Item,Indent,ColWidth,Modified);
        if modified then
          begin
            SCOff:=0;
            Color:=(Color and $fff0) or Red;
          end;
        MoveStr(B[CurCol], Text, Color);
        if {ShowMarkers or } Modified then
        begin
          WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
          WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
          WordRec(B[CurCol+ColWidth-2]).Hi := Color and $ff;
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
    NewItem(menu_watchlocal_edit,'',kbNoKey,cmEdit,hcNoContext,
    NewItem(menu_watchlocal_new,'',kbNoKey,cmNew,hcNoContext,
    NewItem(menu_watchlocal_delete,'',kbNoKey,cmDelete,hcNoContext,
    NewLine(
    NewItem(menu_msglocal_saveas,'',kbNoKey,cmSaveAs,hcSaveAs,
    nil))))));
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
          SetRange(List^.count+1);
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
      R.A.Y:=R.B.Y-7;
      inherited Init(R, dialog_watches,SearchFreeWindowNo);
      Palette:=wpCyanWindow;
      GetExtent(R);
      HelpCtx:=hcWatchesWindow;
      R.Grow(-1,-1);
      R2.Copy(R);
      Inc(R2.B.Y);
      R2.A.Y:=R2.B.Y-1;
      New(HSB, Init(R2));
      HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX;
      HSB^.SetStep(R.B.X-R.A.X,1);
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
  Insert(New(PLabel, Init(R2, label_watch_expressiontowatch, NameIL)));
  GetExtent(R);
  R.Grow(-1,-1);
  R.A.Y:=R.A.Y+3;
  R.B.X:=R.A.X+36;
  TextST:=New(PAdvancedStaticText, Init(R, label_watch_values));
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

  ClearFormatParams;
  AddFormatParamStr(S1);
  AddFormatParamStr(S2);
  if assigned(Watch^.Last_value) and
     assigned(Watch^.Current_value) and
     (strcomp(Watch^.Last_value,Watch^.Current_value)=0) then
    S1:=FormatStrF(msg_watch_currentvalue,FormatParams)
  else
    S1:=FormatStrF(msg_watch_currentandpreviousvalue,FormatParams);

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
                         TRegistersView
****************************************************************************}

  function GetIntRegs(var rs : TIntRegs) : boolean;

    var
       p,po : pchar;
       p1 : pchar;
       reg,value : string;
       buffer : array[0..255] of char;
       v : dword;
       code : word;

    begin
       GetIntRegs:=false;
{$ifndef NODEBUG}
       Debugger^.Command('info registers');
       if Debugger^.Error then
         exit
       else
         begin
            po:=StrNew(Debugger^.GetOutput);
            p:=po;
            if assigned(p) then
              begin
                 fillchar(rs,sizeof(rs),0);
                 p1:=strscan(p,' ');
                 while assigned(p1) do
                   begin
                      strlcopy(buffer,p,p1-p);
                      reg:=strpas(buffer);
                      p:=strscan(p,'$');
                      p1:=strscan(p,#9);
                      strlcopy(buffer,p,p1-p);
                      value:=strpas(buffer);
                      val(value,v,code);
{$ifdef i386}
                      if reg='eax' then
                        rs.eax:=v
                      else if reg='ebx' then
                        rs.ebx:=v
                      else if reg='ecx' then
                        rs.ecx:=v
                      else if reg='edx' then
                        rs.edx:=v
                      else if reg='eip' then
                        rs.eip:=v
                      else if reg='esi' then
                        rs.esi:=v
                      else if reg='edi' then
                        rs.edi:=v
                      else if reg='esp' then
                        rs.esp:=v
                      else if reg='ebp' then
                        rs.ebp:=v
                      { under win32 flags are on a register named ps !! PM }
                      else if (reg='eflags') or (reg='ps') then
                        rs.eflags:=v
                      else if reg='cs' then
                        rs.cs:=v
                      else if reg='ds' then
                        rs.ds:=v
                      else if reg='es' then
                        rs.es:=v
                      else if reg='fs' then
                        rs.fs:=v
                      else if reg='gs' then
                        rs.gs:=v
                      else if reg='ss' then
                        rs.ss:=v;
{$endif i386}
{$ifdef m68k}
                      if reg='d0' then
                        rs.d0:=v
                      else if reg='d1' then
                        rs.d1:=v
                      else if reg='d2' then
                        rs.d2:=v
                      else if reg='d3' then
                        rs.d3:=v
                      else if reg='d4' then
                        rs.d4:=v
                      else if reg='d5' then
                        rs.d5:=v
                      else if reg='d6' then
                        rs.d6:=v
                      else if reg='d7' then
                        rs.d7:=v
                      else if reg='a0' then
                        rs.a0:=v
                      else if reg='a1' then
                        rs.a1:=v
                      else if reg='a2' then
                        rs.a2:=v
                      else if reg='a3' then
                        rs.a3:=v
                      else if reg='a4' then
                        rs.a4:=v
                      else if reg='a5' then
                        rs.a5:=v
                      else if reg='fp' then
                        rs.fp:=v
                      else if reg='sp' then
                        rs.sp:=v
                      else if (reg='ps') then
                        rs.ps:=v
                      else if reg='pc' then
                        rs.pc:=v;
{$endif m68k}
                      p:=strscan(p1,#10);
                      if assigned(p) then
                        begin
                           p1:=strscan(p,' ');
                           inc(p);
                        end
                      else
                        break;
                   end;
                 { free allocated memory }
                 strdispose(po);
              end
            else
              exit;
         end;
       { do not open a messagebox for such errors }
       Debugger^.got_error:=false;
       GetIntRegs:=true;
{$endif}
    end;

  constructor TRegistersView.Init(var Bounds: TRect);

    begin
       inherited init(Bounds);
    end;

  procedure TRegistersView.Draw;

    var
       rs : tintregs;
       color :byte;

    procedure SetColor(x,y : longint);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    begin
       inherited draw;
       If not assigned(Debugger) then
         begin
            WriteStr(1,0,'<no values available>',7);
            exit;
         end;
       if GetIntRegs(rs) then
         begin
{$ifdef i386}
            SetColor(rs.eax,OldReg.eax);
            WriteStr(1,0,'EAX '+HexStr(rs.eax,8),color);
            SetColor(rs.ebx,OldReg.ebx);
            WriteStr(1,1,'EBX '+HexStr(rs.ebx,8),color);
            SetColor(rs.ecx,OldReg.ecx);
            WriteStr(1,2,'ECX '+HexStr(rs.ecx,8),color);
            SetColor(rs.edx,OldReg.edx);
            WriteStr(1,3,'EDX '+HexStr(rs.edx,8),color);
            SetColor(rs.eip,OldReg.eip);
            WriteStr(1,4,'EIP '+HexStr(rs.eip,8),color);
            SetColor(rs.esi,OldReg.esi);
            WriteStr(1,5,'ESI '+HexStr(rs.esi,8),color);
            SetColor(rs.edi,OldReg.edi);
            WriteStr(1,6,'EDI '+HexStr(rs.edi,8),color);
            SetColor(rs.esp,OldReg.esp);
            WriteStr(1,7,'ESP '+HexStr(rs.esp,8),color);
            SetColor(rs.ebp,OldReg.ebp);
            WriteStr(1,8,'EBP '+HexStr(rs.ebp,8),color);
            SetColor(rs.cs,OldReg.cs);
            WriteStr(14,0,'CS '+HexStr(rs.cs,4),color);
            SetColor(rs.ds,OldReg.ds);
            WriteStr(14,1,'DS '+HexStr(rs.ds,4),color);
            SetColor(rs.es,OldReg.es);
            WriteStr(14,2,'ES '+HexStr(rs.es,4),color);
            SetColor(rs.fs,OldReg.fs);
            WriteStr(14,3,'FS '+HexStr(rs.fs,4),color);
            SetColor(rs.gs,OldReg.gs);
            WriteStr(14,4,'GS '+HexStr(rs.gs,4),color);
            SetColor(rs.ss,OldReg.ss);
            WriteStr(14,5,'SS '+HexStr(rs.ss,4),color);
            SetColor(rs.eflags and $1,OldReg.eflags and $1);
            WriteStr(22,0,'c='+chr(byte((rs.eflags and $1)<>0)+48),color);
            SetColor(rs.eflags and $20,OldReg.eflags and $20);
            WriteStr(22,1,'z='+chr(byte((rs.eflags and $20)<>0)+48),color);
            SetColor(rs.eflags and $80,OldReg.eflags and $80);
            WriteStr(22,2,'s='+chr(byte((rs.eflags and $80)<>0)+48),color);
            SetColor(rs.eflags and $800,OldReg.eflags and $800);
            WriteStr(22,3,'o='+chr(byte((rs.eflags and $800)<>0)+48),color);
            SetColor(rs.eflags and $4,OldReg.eflags and $4);
            WriteStr(22,4,'p='+chr(byte((rs.eflags and $4)<>0)+48),color);
            SetColor(rs.eflags and $200,OldReg.eflags and $200);
            WriteStr(22,5,'i='+chr(byte((rs.eflags and $200)<>0)+48),color);
            SetColor(rs.eflags and $10,OldReg.eflags and $10);
            WriteStr(22,6,'a='+chr(byte((rs.eflags and $10)<>0)+48),color);
            SetColor(rs.eflags and $400,OldReg.eflags and $400);
            WriteStr(22,7,'d='+chr(byte((rs.eflags and $400)<>0)+48),color);
{$endif i386}
{$ifdef m68k}
            SetColor(rs.d0,OldReg.d0);
            WriteStr(1,0,'d0 '+HexStr(rs.d0,8),color);
            SetColor(rs.d1,OldReg.d1);
            WriteStr(1,1,'d1 '+HexStr(rs.d1,8),color);
            SetColor(rs.d2,OldReg.d2);
            WriteStr(1,2,'d2 '+HexStr(rs.d2,8),color);
            SetColor(rs.d3,OldReg.d3);
            WriteStr(1,3,'d3 '+HexStr(rs.d3,8),color);
            SetColor(rs.d4,OldReg.d4);
            WriteStr(1,4,'d4 '+HexStr(rs.d4,8),color);
            SetColor(rs.d5,OldReg.d5);
            WriteStr(1,5,'d5 '+HexStr(rs.d5,8),color);
            SetColor(rs.d6,OldReg.d6);
            WriteStr(1,6,'d6 '+HexStr(rs.d6,8),color);
            SetColor(rs.d7,OldReg.d7);
            WriteStr(1,7,'d7 '+HexStr(rs.d7,8),color);
            SetColor(rs.a0,OldReg.a0);
            WriteStr(14,0,'a0 '+HexStr(rs.a0,8),color);
            SetColor(rs.a1,OldReg.a1);
            WriteStr(14,1,'a1 '+HexStr(rs.a1,8),color);
            SetColor(rs.a2,OldReg.a2);
            WriteStr(14,2,'a2 '+HexStr(rs.a2,8),color);
            SetColor(rs.a3,OldReg.a3);
            WriteStr(14,3,'a3 '+HexStr(rs.a3,8),color);
            SetColor(rs.a4,OldReg.a4);
            WriteStr(14,4,'a4 '+HexStr(rs.a4,8),color);
            SetColor(rs.a5,OldReg.a5);
            WriteStr(14,5,'a5 '+HexStr(rs.a5,8),color);
            SetColor(rs.fp,OldReg.fp);
            WriteStr(14,6,'fp '+HexStr(rs.fp,8),color);
            SetColor(rs.sp,OldReg.sp);
            WriteStr(14,7,'sp '+HexStr(rs.sp,8),color);
            SetColor(rs.pc,OldReg.pc);
            WriteStr(1,8,'pc '+HexStr(rs.pc,8),color);
            SetColor(rs.ps and $1,OldReg.ps and $1);
            WriteStr(20,8,'c'+chr(byte((rs.ps and $1)<>0)+48),color);
            SetColor(rs.ps and $2,OldReg.ps and $2);
            WriteStr(18,8,'v'+chr(byte((rs.ps and $2)<>0)+48),color);
            SetColor(rs.ps and $4,OldReg.ps and $4);
            WriteStr(16,8,'z'+chr(byte((rs.ps and $4)<>0)+48),color);
            SetColor(rs.ps and $8,OldReg.ps and $8);
            WriteStr(14,8,'x'+chr(byte((rs.ps and $8)<>0)+48),color);
{$endif i386}
            OldReg:=rs;
         end
       else
         WriteStr(0,0,'<debugger error>',7);
    end;

  destructor TRegistersView.Done;

    begin
       inherited done;
    end;

{****************************************************************************
                         TRegistersWindow
****************************************************************************}

  constructor TRegistersWindow.Init;

    var
       R : TRect;

    begin
       Desktop^.GetExtent(R);
       R.A.X:=R.B.X-28;
       R.B.Y:=R.A.Y+11;
       inherited Init(R,dialog_registers, wnNoNumber);
       Flags:=wfClose or wfMove;
       Palette:=wpCyanWindow;
       HelpCtx:=hcRegistersWindow;
       R.Assign(1,1,26,10);
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
       ReDraw;
    end;

  destructor TRegistersWindow.Done;

    begin
       RegistersWindow:=nil;
       inherited done;
    end;

{****************************************************************************
                         TFPUView
****************************************************************************}

  function GetFPURegs(var rs : TFPURegs) : boolean;

    var
       p,po : pchar;
       p1 : pchar;
    {$ifndef NODEBUG}
       reg,value : string;
       buffer : array[0..255] of char;
       v : string;
       res : cardinal;
       i : longint;
       err : word;
    {$endif}

    begin
       GetFPURegs:=false;
{$ifndef NODEBUG}
       Debugger^.Command('info all');
       if Debugger^.Error then
         exit
       else
         begin
            po:=StrNew(Debugger^.GetOutput);
            p:=po;
            if assigned(p) then
              begin
                 fillchar(rs,sizeof(rs),0);
                 p1:=strscan(p,' ');
                 while assigned(p1) do
                   begin
                      strlcopy(buffer,p,p1-p);
                      reg:=strpas(buffer);
                      p:=p1;
                      while p^=' ' do
                        inc(p);
                      if p^='$' then
                        p1:=strscan(p,#9)
                      else
                        p1:=strscan(p,#10);
                      strlcopy(buffer,p,p1-p);
                      v:=strpas(buffer);
                      for i:=1 to length(v) do
                        if v[i]=#9 then
                          v[i]:=' ';
                      val(v,res,err);
{$ifdef i386}
                      if reg='st0' then
                        rs.st0:=v
                      else if reg='st1' then
                        rs.st1:=v
                      else if reg='st2' then
                        rs.st2:=v
                      else if reg='st3' then
                        rs.st3:=v
                      else if reg='st4' then
                        rs.st4:=v
                      else if reg='st5' then
                        rs.st5:=v
                      else if reg='st6' then
                        rs.st6:=v
                      else if reg='st7' then
                        rs.st7:=v
                      else if reg='ftag' then
                        rs.ftag:=res
                      else if reg='fctrl' then
                        rs.fctrl:=res
                      else if reg='fstat' then
                        rs.fstat:=res
                      else if reg='fiseg' then
                        rs.fiseg:=res
                      else if reg='fioff' then
                        rs.fioff:=res
                      else if reg='foseg' then
                        rs.foseg:=res
                      else if reg='fooff' then
                        rs.fooff:=res
                      else if reg='fop' then
                        rs.fop:=res;
{$endif i386}
{$ifdef m68k}
                      if reg='fp0' then
                        rs.fp0:=v
                      else if reg='fp1' then
                        rs.fp1:=v
                      else if reg='fp2' then
                        rs.fp2:=v
                      else if reg='fp3' then
                        rs.fp3:=v
                      else if reg='fp4' then
                        rs.fp4:=v
                      else if reg='fp5' then
                        rs.fp5:=v
                      else if reg='fp6' then
                        rs.fp6:=v
                      else if reg='fp7' then
                        rs.fp7:=v
                      else if reg='fpcontrol' then
                        rs.fpcontrol:=res
                      else if reg='fpstatus' then
                        rs.fpstatus:=res
                      else if reg='fpiaddr' then
                        rs.fpiaddr:=res;
{$endif m68k}
                      p:=strscan(p1,#10);
                      if assigned(p) then
                        begin
                           p1:=strscan(p,' ');
                           inc(p);
                        end
                      else
                        break;
                   end;
                 { free allocated memory }
                 strdispose(po);
              end
            else
              exit;
         end;
       { do not open a messagebox for such errors }
       Debugger^.got_error:=false;
       GetFPURegs:=true;
{$endif}
    end;

  constructor TFPUView.Init(var Bounds: TRect);

    begin
       inherited init(Bounds);
    end;

  procedure TFPUView.Draw;

    var
       rs : tfpuregs;
       top : byte;
       color :byte;
    const
      TypeStr : Array[0..3] of string[6] =
      ('Valid ','Zero  ','Spec  ','Empty ');

    procedure SetColor(Const x,y : string);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    procedure SetIColor(Const x,y : cardinal);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    begin
       inherited draw;
       If not assigned(Debugger) then
         begin
            WriteStr(1,0,'<no values available>',7);
            exit;
         end;
       if GetFPURegs(rs) then
         begin
{$ifdef i386}
            top:=(rs.fstat shr 11) and 7;
            SetColor(rs.st0,OldReg.st0);
            WriteStr(1,0,'ST0 '+TypeStr[(rs.ftag shr (2*((0+top) and 7))) and 3]+rs.st0,color);
            SetColor(rs.st1,OldReg.st1);
            WriteStr(1,1,'ST1 '+TypeStr[(rs.ftag shr (2*((1+top) and 7))) and 3]+rs.st1,color);
            SetColor(rs.st2,OldReg.st2);
            WriteStr(1,2,'ST2 '+TypeStr[(rs.ftag shr (2*((2+top) and 7))) and 3]+rs.st2,color);
            SetColor(rs.st3,OldReg.st3);
            WriteStr(1,3,'ST3 '+TypeStr[(rs.ftag shr (2*((3+top) and 7))) and 3]+rs.st3,color);
            SetColor(rs.st4,OldReg.st4);
            WriteStr(1,4,'ST4 '+TypeStr[(rs.ftag shr (2*((4+top) and 7))) and 3]+rs.st4,color);
            SetColor(rs.st5,OldReg.st5);
            WriteStr(1,5,'ST5 '+TypeStr[(rs.ftag shr (2*((5+top) and 7))) and 3]+rs.st5,color);
            SetColor(rs.st6,OldReg.st6);
            WriteStr(1,6,'ST6 '+TypeStr[(rs.ftag shr (2*((6+top) and 7))) and 3]+rs.st6,color);
            SetColor(rs.st7,OldReg.st7);
            WriteStr(1,7,'ST7 '+TypeStr[(rs.ftag shr (2*((7+top) and 7))) and 3]+rs.st7,color);
            SetIColor(rs.ftag,OldReg.ftag);
            WriteStr(1,8,'FTAG   '+hexstr(rs.ftag,4),color);
            SetIColor(rs.fctrl,OldReg.fctrl);
            WriteStr(13,8,'FCTRL  '+hexstr(rs.fctrl,4),color);
            SetIColor(rs.fstat,OldReg.fstat);
            WriteStr(1,9,'FSTAT  '+hexstr(rs.fstat,4),color);
            SetIColor(rs.fop,OldReg.fop);
            WriteStr(13,9,'FOP    '+hexstr(rs.fop,4),color);
            if (rs.fiseg<>OldReg.fiseg) or
               (rs.fioff<>OldReg.fioff) then
              color:=8
            else
              color:=7;
            WriteStr(1,10,'FI    '+hexstr(rs.fiseg,4)+':'+hexstr(rs.fioff,8),color);
            if (rs.foseg<>OldReg.foseg) or
               (rs.fooff<>OldReg.fooff) then
              color:=8
            else
              color:=7;
            WriteStr(1,11,'FO    '+hexstr(rs.foseg,4)+':'+hexstr(rs.fooff,8),color);
            OldReg:=rs;
{$endif i386}
{$ifdef m68k}
            SetColor(rs.fp0,OldReg.fp0);
            WriteStr(1,0,'fp0 '+rs.fp0,color);
            SetColor(rs.fp1,OldReg.fp1);
            WriteStr(1,1,'fp1 '+rs.fp1,color);
            SetColor(rs.fp2,OldReg.fp2);
            WriteStr(1,2,'fp2 '+rs.fp2,color);
            SetColor(rs.fp3,OldReg.fp3);
            WriteStr(1,3,'fp3 '+rs.fp3,color);
            SetColor(rs.fp4,OldReg.fp4);
            WriteStr(1,4,'fp4 '+rs.fp4,color);
            SetColor(rs.fp5,OldReg.fp5);
            WriteStr(1,5,'fp5 '+rs.fp5,color);
            SetColor(rs.fp6,OldReg.fp6);
            WriteStr(1,6,'fp6 '+rs.fp6,color);
            SetColor(rs.fp7,OldReg.fp7);
            WriteStr(1,7,'fp7 '+rs.fp7,color);
            SetIColor(rs.fpcontrol,OldReg.fpcontrol);
            WriteStr(1,8,'fpcontrol   '+hexstr(rs.fpcontrol,8),color);
            SetIColor(rs.fpstatus,OldReg.fpstatus);
            WriteStr(1,9,'fpstatus    '+hexstr(rs.fpstatus,8),color);
            SetIColor(rs.fpiaddr,OldReg.fpiaddr);
            WriteStr(1,10,'fpiaddr    '+hexstr(rs.fpiaddr,8),color);
            OldReg:=rs;
{$endif m68k}
         end
       else
         WriteStr(0,0,'<debugger error>',7);
    end;

  destructor TFPUView.Done;

    begin
       inherited done;
    end;

{****************************************************************************
                         TFPUWindow
****************************************************************************}

  constructor TFPUWindow.Init;

    var
       R : TRect;

    begin
       Desktop^.GetExtent(R);
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+14;
       inherited Init(R,dialog_fpu, wnNoNumber);
       Flags:=wfClose or wfMove;
       Palette:=wpCyanWindow;
       HelpCtx:=hcFPURegisters;
       R.Assign(1,1,42,13);
       RV:=new(PFPUView,init(R));
       Insert(RV);
       If assigned(FPUWindow) then
         dispose(FPUWindow,done);
       FPUWindow:=@Self;
       Update;
    end;

  constructor TFPUWindow.Load(var S: TStream);

    begin
       inherited load(S);
       GetSubViewPtr(S,RV);
       If assigned(FPUWindow) then
         dispose(FPUWindow,done);
       FPUWindow:=@Self;
    end;

  procedure TFPUWindow.Store(var S: TStream);

    begin
       inherited Store(s);
       PutSubViewPtr(S,RV);
    end;

  procedure TFPUWindow.Update;

    begin
       ReDraw;
    end;

  destructor TFPUWindow.Done;

    begin
       FPUWindow:=nil;
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
      DeskTop^.Lock;
      Clear;
      { forget all old frames }
      Debugger^.clear_frames;

      if Debugger^.WindowWidth<>-1 then
        Debugger^.Command('set width 0xffffffff');
      Debugger^.Command('backtrace');
      { generate list }
      { all is in tframeentry }
      for i:=0 to Debugger^.frame_count-1 do
        begin
          with Debugger^.frames[i]^ do
            begin
              if assigned(file_name) then
                AddItem(new(PMessageItem,init(0,GetPChar(function_name)+GetPChar(args),
                  AddModuleName(GetPChar(file_name)),line_number,1)))
              else
                AddItem(new(PMessageItem,init(0,HexStr(address,8)+' '+GetPChar(function_name)+GetPChar(args),
                  AddModuleName(''),line_number,1)));
              W:=SearchOnDesktop(GetPChar(file_name),false);
              { First reset all Debugger rows }
              If assigned(W) then
                begin
                  W^.Editor^.SetLineFlagExclusive(lfDebuggerRow,-1);
                  W^.Editor^.DebuggerRow:=-1;
                end;
            end;
        end;
      { Now set all Debugger rows }
      for i:=0 to Debugger^.frame_count-1 do
        begin
          with Debugger^.frames[i]^ do
            begin
              W:=SearchOnDesktop(GetPChar(file_name),false);
              If assigned(W) then
                begin
                  If W^.Editor^.DebuggerRow=-1 then
                    begin
                      W^.Editor^.SetLineFlagState(line_number-1,lfDebuggerRow,true);
                      W^.Editor^.DebuggerRow:=line_number-1;
                    end;
                end;
            end;
        end;
      if Assigned(list) and (List^.Count > 0) then
        FocusItem(0);
      if Debugger^.WindowWidth<>-1 then
        Debugger^.Command('set width '+IntToStr(Debugger^.WindowWidth));
      DeskTop^.Unlock;
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
    {$ifndef NODEBUG}
      Debugger^.Command('f '+IntToStr(Focused));
      { for local vars }
      Debugger^.ReadWatches;
   {$endif}
      { goto source }
      inherited GotoSource;
    end;

  procedure   TFramesListBox.GotoAssembly;
    begin
      { select frame for watches }
      If not assigned(Debugger) then
        exit;
    {$ifndef NODEBUG}
      Debugger^.Command('f '+IntToStr(Focused));
      { for local vars }
      Debugger^.ReadWatches;
   {$endif}
      { goto source/assembly mixture }
      InitDisassemblyWindow;
      DisassemblyWindow^.LoadFunction('');
      DisassemblyWindow^.SetCurAddress(Debugger^.frames[Focused]^.address);
      DisassemblyWindow^.SelectInDebugSession;
    end;


  procedure   TFramesListBox.HandleEvent(var Event: TEvent);
    begin
      if ((Event.What=EvKeyDown) and (Event.CharCode='i')) or
         ((Event.What=EvCommand) and (Event.Command=cmDisassemble)) then
        GotoAssembly;
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
      inherited Init(R, dialog_callstack, wnNoNumber);
      Palette:=wpCyanWindow;
      GetExtent(R);
      HelpCtx:=hcStackWindow;
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
{$ifdef DEBUG}
var s : string;
    i,p : longint;
{$endif DEBUG}
var
   NeedRecompileExe : boolean;
   cm : longint;
begin
{$ifdef DEBUG}
  if not use_gdb_file then
    begin
      Assign(gdb_file,GDBOutFileName);
      {$I-}
      Rewrite(gdb_file);
      if InOutRes<>0 then
        begin
          s:=GDBOutFileName;
          p:=pos('.',s);
          if p>1 then
           for i:=0 to 9 do
             begin
               s:=copy(s,1,p-2)+chr(i+ord('0'))+copy(s,p,length(s));
               InOutRes:=0;
               Assign(gdb_file,s);
               rewrite(gdb_file);
               if InOutRes=0 then
                 break;
             end;
        end;
      if IOResult=0 then
        Use_gdb_file:=true;
    end;
  {$I+}
{$endif}

  NeedRecompileExe:=false;
  if TargetSwitches^.GetCurrSelParam<>{$ifdef COMPILER_1_0}source_os{$else}source_info{$endif}.shortname then
    begin
     ClearFormatParams;
     AddFormatParamStr(TargetSwitches^.GetCurrSelParam);
     AddFormatParamStr({$ifdef COMPILER_1_0}source_os{$else}source_info{$endif}.shortname);
     cm:=ConfirmBox(msg_cantdebugchangetargetto,@FormatParams,true);
     if cm=cmCancel then
       Exit;
     if cm=cmYes then
       begin
         { force recompilation }
         PrevMainFile:='';
         NeedRecompileExe:=true;
         TargetSwitches^.SetCurrSelParam({$ifdef COMPILER_1_0}source_os{$else}source_info{$endif}.shortname);
         If DebugInfoSwitches^.GetCurrSelParam='-' then
           DebugInfoSwitches^.SetCurrSelParam('l');
         IDEApp.UpdateTarget;
       end;
    end;
  if not NeedRecompileExe then
    NeedRecompileExe:=(not ExistsFile(ExeFile)) or (CompilationPhase<>cpDone) or
     (PrevMainFile<>MainFile) or NeedRecompile(cRun,false);
  if Not NeedRecompileExe and Not MainHasDebugInfo then
    begin
     ClearFormatParams;
     cm:=ConfirmBox(msg_compiledwithoutdebuginforecompile,nil,true);
     if cm=cmCancel then
       Exit;
     if cm=cmYes then
       begin
         { force recompilation }
         PrevMainFile:='';
         NeedRecompileExe:=true;
         DebugInfoSwitches^.SetCurrSelParam('l');
       end;
    end;
  if NeedRecompileExe then
    DoCompile(cRun);
  if CompilationPhase<>cpDone then
    Exit;
  if (EXEFile='') then
   begin
     ErrorBox(msg_nothingtodebug,nil);
     Exit;
   end;
{ init debugcontroller }
  if not assigned(Debugger) then
    begin
      PushStatus(msg_startingdebugger);
      new(Debugger,Init);
      PopStatus;
    end;
  Debugger^.SetExe(ExeFile);
{$ifdef GDBWINDOW}
  InitGDBWindow;
{$endif def GDBWINDOW}
end;


procedure DoneDebugger;
begin
{$ifdef DEBUG}
  If IDEApp.IsRunning then
    PushStatus('Closing debugger');
{$endif}
  if assigned(Debugger) then
   dispose(Debugger,Done);
  Debugger:=nil;
{$ifdef DEBUG}
  If Use_gdb_file then
    begin
      Use_gdb_file:=false;
      Close(GDB_file);
    end;
  If IDEApp.IsRunning then
    PopStatus;
{$endif DEBUG}
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
  If IDEApp.IsRunning and
     assigned(GDBWindow) then
    begin
      DeskTop^.Delete(GDBWindow);
    end;
  GDBWindow:=nil;
end;

procedure InitDisassemblyWindow;
var
  R : TRect;
begin
  if DisassemblyWindow=nil then
    begin
      DeskTop^.GetExtent(R);
      new(DisassemblyWindow,init(R));
      DeskTop^.Insert(DisassemblyWindow);
    end;
end;

procedure DoneDisassemblyWindow;
begin
  if assigned(DisassemblyWindow) then
    begin
      DeskTop^.Delete(DisassemblyWindow);
      Dispose(DisassemblyWindow,Done);
      DisassemblyWindow:=nil;
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

procedure InitFPUWindow;
begin
  if FPUWindow=nil then
    begin
      new(FPUWindow,init);
      DeskTop^.Insert(FPUWindow);
    end;
end;

procedure DoneFPUWindow;
begin
  if assigned(FPUWindow) then
    begin
      DeskTop^.Delete(FPUWindow);
      FPUWindow:=nil;
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
  RegisterType(RFPUWindow);
  RegisterType(RFPUView);
end;

end.

{
  $Log$
  Revision 1.20  2002-06-06 14:11:25  pierre
   * handle win32 Ctrl-C change for graphic version

  Revision 1.19  2002/06/06 08:16:18  pierre
   * avoid crashes if quitting while debuggee is running

  Revision 1.18  2002/04/25 13:33:31  pierre
   * fix the problem with dirs containing asterisks

  Revision 1.17  2002/04/17 11:11:54  pierre
    * avoid problems for ClassVariable in Watches window

  Revision 1.16  2002/04/11 06:41:13  pierre
   * fix problem of TWatchesListBox with fvision

  Revision 1.15  2002/04/03 06:18:30  pierre
   * fix some win32 GDB filename problems

  Revision 1.14  2002/04/02 15:09:38  pierre
   * fixed wrong exit without unlock

  Revision 1.13  2002/04/02 13:23:54  pierre
   * Use StrToCard and HexToCard functions to avoid signed/unsigned overflows

  Revision 1.12  2002/04/02 12:20:58  pierre
   * fix problem with breakpoints in subdirs

  Revision 1.11  2002/04/02 11:10:29  pierre
   * fix FPC_BREAK_ERROR problem and avoid blinking J

  Revision 1.10  2002/03/27 11:24:09  pierre
   * fix several problems related to long file nmze support for win32 exes

  Revision 1.9  2002/02/06 14:45:00  pierre
   + handle signals

  Revision 1.8  2001/11/10 00:11:45  pierre
   * change target menu name if target changed to become debug-able

  Revision 1.7  2001/11/07 00:28:52  pierre
   + Disassembly window made public

  Revision 1.6  2001/10/14 14:16:06  peter
    * fixed typo for linux

  Revision 1.5  2001/10/11 11:39:35  pierre
   * better NoSwitch check for unix

  Revision 1.4  2001/09/12 09:48:38  pierre
   + SetDirectories method added to help for disassembly window

  Revision 1.3  2001/08/07 22:58:10  pierre
   * watches display enhanced and crashes removed

  Revision 1.2  2001/08/05 02:01:47  peter
    * FVISION define to compile with fvision units

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.35  2001/08/03 13:33:51  pierre
   * better looking m68k flags

  Revision 1.1.2.34  2001/07/31 21:40:42  pierre
   * fix typo erros in last commit

  Revision 1.1.2.33  2001/07/31 15:12:45  pierre
  + some m68k register support

  Revision 1.1.2.32  2001/07/29 22:12:23  peter
    * fixed private symbol that needs to be public

  Revision 1.1.2.31  2001/06/13 16:22:02  pierre
   * use CygdrivePrefix function for win32

  Revision 1.1.2.30  2001/04/10 11:50:09  pierre
    * only stop if erroraddress or exitcode non zero
    + reset the file in DoneDebugger to avoid problem
      if the executable file remains opened by GDB when recompiling

  Revision 1.1.2.29  2001/03/22 17:28:57  pierre
   * more stuff for stop at exit if error

  Revision 1.1.2.28  2001/03/22 01:14:08  pierre
   * work on Exit breakpoint if error

  Revision 1.1.2.27  2001/03/20 00:20:42  pierre
   * fix some memory leaks + several small enhancements

  Revision 1.1.2.26  2001/03/15 17:45:19  pierre
   * avoid to get the values of expressions twice

  Revision 1.1.2.25  2001/03/15 17:08:52  pierre
   * avoid extra info past watches values

  Revision 1.1.2.24  2001/03/13 00:36:44  pierre
   * small DisassemblyWindow fixes

  Revision 1.1.2.23  2001/03/12 17:34:54  pierre
   + Disassembly window started

  Revision 1.1.2.22  2001/03/09 15:08:12  pierre
    * Watches list reorganised so that the behavior
      is more near to BP one.
    + First version of FPU window for i386.

  Revision 1.1.2.21  2001/03/08 16:41:03  pierre
   * correct watch horizontal scrolling

  Revision 1.1.2.20  2001/03/06 22:42:22  pierre
   * check for modifed open files at stop of beguggee

  Revision 1.1.2.19  2001/03/06 21:44:13  pierre
   * avoid problems if recompiling in debug session

  Revision 1.1.2.18  2001/01/09 11:49:30  pierre
   * fix DebugRow highlighting problem if Call Stack Window is open

  Revision 1.1.2.17  2001/01/07 22:37:41  peter
    * quiting gdbwindow works now

  Revision 1.1.2.16  2000/12/13 16:58:11  pierre
   * AllowQuit changed, still does not work correctly :(

  Revision 1.1.2.15  2000/11/29 18:28:51  pierre
   + add save to file capability for list boxes

  Revision 1.1.2.14  2000/11/29 11:25:59  pierre
   + TFPDlgWindow that handles cmSearchWindow

  Revision 1.1.2.13  2000/11/29 00:54:44  pierre
   + preserve window number and save special windows

  Revision 1.1.2.12  2000/11/27 17:41:45  pierre
   * better GDB window opening if nothing compiled yet

  Revision 1.1.2.11  2000/11/16 23:06:30  pierre
  * correct handling of Compile/Make if primary file is set

  Revision 1.1.2.10  2000/11/14 17:40:42  pierre
   + External linking now optional

  Revision 1.1.2.9  2000/11/14 09:23:55  marco
   * Second batch

  Revision 1.1.2.8  2000/11/13 16:59:08  pierre
   * some function in double removed from fputils unit

  Revision 1.1.2.7  2000/10/31 07:47:54  pierre
   * start to support FPC_BREAK_ERROR

  Revision 1.1.2.6  2000/10/26 00:04:35  pierre
   + gdb prompt and FPC_BREAK_ERROR stop

  Revision 1.1.2.5  2000/10/09 19:48:15  pierre
   * wrong commit corrected

  Revision 1.1.2.4  2000/10/09 16:28:24  pierre
   * several linux enhancements

  Revision 1.1.2.3  2000/10/06 22:52:34  pierre
   * fixes for linux GDB tty command

  Revision 1.1.2.2  2000/09/22 12:02:34  jonas
    * corrected command for running user program in other tty under linux
      (doesn't work yet though)

  Revision 1.1.2.1  2000/07/18 05:50:22  michael
  + Merged Gabors fixes

  Revision 1.1  2000/07/13 09:48:34  michael
  + Initial import

  Revision 1.63  2000/06/22 09:07:11  pierre
   * Gabor changes: see fixes.txt

  Revision 1.62  2000/06/11 07:01:32  peter
    * give watches window also a number
    * leave watches window in the bottom when cascading windows

  Revision 1.61  2000/05/02 08:42:27  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.60  2000/04/18 21:45:35  pierre
   * Red line for breakpoint was off by one line

  Revision 1.59  2000/04/18 11:42:36  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.58  2000/03/21 23:32:38  pierre
   adapted to wcedit addition by Gabor

  Revision 1.57  2000/03/14 14:22:30  pierre
   + generate cmDebuggerStopped broadcast

  Revision 1.56  2000/03/08 16:57:01  pierre
    * Wrong highlighted line while debugging fixed
    + Check if exe has debugging info

  Revision 1.55  2000/03/07 21:52:54  pierre
   + TDebugController.GetValue

  Revision 1.54  2000/03/06 11:34:25  pierre
   + windebug unit for Window Title change when debugging

  Revision 1.53  2000/02/07 12:51:32  pierre
   * typo fix

  Revision 1.52  2000/02/07 11:50:30  pierre
   Gabor changes for TP

  Revision 1.51  2000/02/06 23:43:57  pierre
   * breakpoint path problems fixes

  Revision 1.50  2000/02/05 01:27:58  pierre
    * bug with Toggle Break fixed, hopefully
    + search for local vars in parent procs avoiding
      wrong results (see test.pas source)

  Revision 1.49  2000/02/04 23:18:05  pierre
   * no pushstatus in DoneDebugger because its called after App.done

  Revision 1.48  2000/02/04 14:34:46  pierre
  readme.txt

  Revision 1.47  2000/02/04 00:10:58  pierre
   * Breakpoint line in Source Window better handled

  Revision 1.46  2000/02/01 10:59:58  pierre
   * allow FP to debug itself

  Revision 1.45  2000/01/28 22:38:21  pierre
   * CrtlF9 starts debugger if there are active breakpoints

  Revision 1.44  2000/01/27 22:30:38  florian
    * start of FPU window
    * current executed line color has a higher priority then a breakpoint now

  Revision 1.43  2000/01/20 00:31:53  pierre
   * uses ShortName of exe to start GDB

  Revision 1.42  2000/01/10 17:49:40  pierre
   * Get RegisterView to Update correctly
   * Write in white changed regs (keeping a copy of previous values)

  Revision 1.41  2000/01/10 16:20:50  florian
    * working register window

  Revision 1.40  2000/01/10 13:20:57  pierre
   + debug only possible on source target

  Revision 1.39  2000/01/10 00:25:06  pierre
   * RegisterWindow problem fixed

  Revision 1.38  2000/01/09 21:05:51  florian
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
