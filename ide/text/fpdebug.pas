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
  Views,FPViews,
  Objects,GDBCon,GDBInt;

type
  PDebugController=^TDebugController;
  TDebugController=object(TGDBController)
     InvalidSourceLine : boolean;
     LastFileName : string;
     LastSource   : PView; {PsourceWindow !! }
     HiddenStepsCount : longint;
    constructor Init(const exefn:string);
    destructor  Done;
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
{    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;}
    procedure DoEndSession(code:longint);virtual;
    procedure AnnotateError;
    procedure InsertBreakpoints;
    procedure RemoveBreakpoints;
    procedure ResetBreakpointsValues;
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
    procedure Reset;virtual;
    procedure Run;virtual;
    procedure Continue;virtual;
    procedure CommandBegin(const s:string);virtual;
    procedure CommandEnd(const s:string);virtual;
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
     constructor Init_file_line(AFile : String; ALine : longint);
     constructor Init_type(atyp : BreakpointType;Const AnExpr : String);
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
      procedure FreeItem(Item: Pointer); virtual;
      procedure ShowBreakpoints(W : PSourceWindow);
    end;

const
     BreakpointTypeStr : Array[BreakpointType] of String[9]
       = ( 'function','file-line','watch','awatch','rwatch','invalid' );
     BreakpointStateStr : Array[BreakpointState] of String[8]
       = ( 'enabled','disabled','invalid' );

var
  Debugger : PDebugController;
  BreakpointCollection : PBreakpointCollection;
  
procedure InitDebugger;
procedure DoneDebugger;
procedure InitGDBWindow;
procedure DoneGDBWindow;
procedure InitBreakpoints;
procedure DoneBreakpoints;

implementation

uses
  Dos,Mouse,Video,
  App,Strings,
  FPVars,FPUtils,FPConst,
  FPIntf,FPCompile,FPIde;


{****************************************************************************
                            TDebugController
****************************************************************************}

constructor TDebugController.Init(const exefn:string);
  var f: string;
begin
  inherited Init;
  f := exefn;
  LoadFile(f);
  SetArgs(GetRunParameters);
  Debugger:=@self;
  InsertBreakpoints;
end;

procedure TDebugController.InsertBreakpoints;
  procedure DoInsert(PB : PBreakpoint);
  begin
    PB^.Insert;
  end;
    
begin
  BreakpointCollection^.ForEach(@DoInsert);
end;


procedure TDebugController.RemoveBreakpoints;
  procedure DoDelete(PB : PBreakpoint);
    begin
      PB^.Remove;
    end;
begin
   BreakpointCollection^.ForEach(@DoDelete);
end;

procedure TDebugController.ResetBreakpointsValues;
  procedure DoResetVal(PB : PBreakpoint);
    begin
      PB^.ResetValues;
    end;
begin
   BreakpointCollection^.ForEach(@DoResetVal);
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
  inherited Run;
  MyApp.SetCmdState([cmResetDebugger],true);
end;

procedure TDebugController.Continue;
begin
  if not debugger_started then
    Run;
  inherited Continue;
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
      { We should do somethnig special for errors !! }
      If StrLen(GetError)>0 then
        GDBWindow^.WriteErrorText(GetError);
      GDBWindow^.WriteOutputText(GetOutput);
      GDBWindow^.Editor^.TextEnd;
    end;
end;


procedure TDebugController.Reset;
var
  W : PSourceWindow;
begin
  inherited Reset;
  MyApp.SetCmdState([cmResetDebugger],false);
  W:=PSourceWindow(LastSource);
  if assigned(W) then
     W^.Editor^.SetHighlightRow(-1);
end;

procedure TDebugController.AnnotateError;
var errornb : longint;
begin
  if error then
    begin
       errornb:=error_num;
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
  if Line>0 then
    dec(Line);
  if (fn=LastFileName) then
    begin
      W:=PSourceWindow(LastSource);
      if assigned(W) then
        begin
          W^.Editor^.SetCurPtr(0,Line);
          W^.Editor^.TrackCursor(true);
          W^.Editor^.SetHighlightRow(Line);
          if Not assigned(GDBWindow) or not GDBWindow^.Focus then
            W^.Select;
          InvalidSourceLine:=false;
        end
      else
        InvalidSourceLine:=true;
    end
  else
    begin
      W:=TryToOpenFile(nil,fn,0,Line);
      if assigned(W) then
        begin
          W^.Editor^.SetHighlightRow(Line);
          W^.Editor^.TrackCursor(true);
          if Not assigned(GDBWindow) or not GDBWindow^.Focus then
            W^.Select;
          LastSource:=W;
          InvalidSourceLine:=false;
        end
        { only search a file once }
      else
       begin
         Desktop^.UnLock;
         Found:=MyApp.OpenSearch(fn);
         Desktop^.Lock;
         if not Found then
           begin
             InvalidSourceLine:=true;
             LastSource:=Nil;
           end
         else
           begin
             { should now be open }
              W:=TryToOpenFile(nil,fn,0,Line);
              W^.Editor^.SetHighlightRow(Line);
              W^.Editor^.TrackCursor(true);
              if Not assigned(GDBWindow) or not GDBWindow^.Focus then
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
      PB:=BreakpointCollection^.GetGDB(stop_breakpoint_number);
      { For watch we should get old and new value !! }
      if (Not assigned(GDBWindow) or not GDBWindow^.Focus) and
         (PB^.typ<>bt_file_line) then
        begin
           Command('p '+GetStr(PB^.Name));
           S:=StrPas(GetOutput);
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
   MyApp.SetCmdState([cmResetDebugger],false);
   W:=PSourceWindow(LastSource);
   if assigned(W) then
     W^.Editor^.SetHighlightRow(-1);
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
  MyApp.ShowIDEScreen;
end;


procedure TDebugController.DoUserScreen;
begin
  MyApp.ShowUserScreen;
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

procedure TBreakpoint.Insert;
begin
  If not assigned(Debugger) then Exit;
  Remove;
  Debugger^.last_breakpoint_number:=0;
  if (GDBState=bs_deleted) and (state=bs_enabled) then
    begin
      if (typ=bt_file_line) then
        Debugger^.Command('break '+NameAndExtOf(FileName^)+':'+IntToStr(Line))
      else if typ=bt_function then
        Debugger^.Command('break '+name^)
      else if typ=bt_watch then
        Debugger^.Command('watch '+name^)
      else if typ=bt_awatch then
        Debugger^.Command('awatch '+name^)
      else if typ=bt_rwatch then
        Debugger^.Command('rwatch '+name^);
      if Debugger^.last_breakpoint_number<>0 then
        begin
          GDBIndex:=Debugger^.last_breakpoint_number;
          GDBState:=bs_enabled;
          Debugger^.Command('cond '+IntToStr(GDBIndex)+' '+GetStr(Conditions));
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
            #3+BreakpointTypeStr[typ]+' '+Name^,nil);
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

procedure TBreakpointCollection.FreeItem(Item: Pointer);
begin
  if Item<>nil then
    Dispose(PBreakpoint(Item),Done);
end;

procedure TBreakpointCollection.Update;
begin
  if assigned(Debugger) then
    begin
      Debugger^.RemoveBreakpoints;
      Debugger^.InsertBreakpoints;
    end;
end;

function  TBreakpointCollection.GetGDB(index : longint) : PBreakpoint;

  function IsNum(P : PBreakpoint) : boolean;
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

  procedure SetInSource(P : PBreakpoint);
  begin
    If assigned(P^.FileName) and (P^.FileName^=W^.Editor^.FileName) then
      W^.Editor^.SetLineBreakState(P^.Line,P^.state=bs_enabled);
  end;
  
begin
  ForEach(@SetInSource);
end;

function TBreakpointCollection.GetType(typ : BreakpointType;Const s : String) : PBreakpoint;

  function IsThis(P : PBreakpoint) : boolean;
  begin
    IsThis:=(P^.typ=typ) and (P^.Name^=S);
  end;
  
begin
  GetType:=FirstThat(@IsThis);
end;

function TBreakpointCollection.ToggleFileLine(Const FileName: String;LineNr : Longint) : boolean;

var PB : PBreakpoint;

  function IsThere(P : PBreakpoint) : boolean;
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
                                 Initialize
****************************************************************************}

procedure InitDebugger;
begin
  Assign(gdb_file,'gdb$$$.out');
  Rewrite(gdb_file);
  Use_gdb_file:=true;
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
  If Use_gdb_file then
    Close(GDB_file);
  Use_gdb_file:=false;
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

procedure InitBreakpoints;
begin
  New(BreakpointCollection,init(10,10));
end;

procedure DoneBreakpoints;
begin
  Dispose(BreakpointCollection,Done);
  BreakpointCollection:=nil;
end;

end.

{
  $Log$
  Revision 1.12  1999-02-11 19:07:20  pierre
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
