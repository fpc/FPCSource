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
  Views,Objects,GDBCon,GDBInt;

type
  PDebugController=^TDebugController;
  TDebugController=object(TGDBController)
     Invalid_line : boolean;
     LastFileName : string;
     LastSource   : PView; {PsourceWindow !! }
    constructor Init(const exefn:string);
    destructor  Done;
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
{    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;}
    procedure DoEndSession(code:longint);virtual;
    procedure AnnotateError;
    procedure InsertBreakpoints;
    procedure RemoveBreakpoints;
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
  end;

  BreakpointType = (bt_function,bt_file_line,bt_watch,bt_awatch,bt_rwatch,bt_invalid);
  BreakpointState = (bs_enabled,bs_disabled,bs_deleted);

  PBreakpointCollection=^TBreakpointCollection;

  PBreakpoint=^TBreakpoint;
  TBreakpoint=object(TObject)
     typ  : BreakpointType;
     state : BreakpointState;
     owner : PBreakpointCollection;
     Name : PString;  { either function name or file name }
     Line : Longint; { only used for bt_file_line type }
     Conditions : PString; { conditions relative to that breakpoint }
     IgnoreCount : Longint; { how many counts should be ignored }
     Commands : pchar; { commands that should be executed on breakpoint }
     GDBIndex : longint;
     GDBState : BreakpointState;
     constructor Init_function(Const AFunc : String);
     constructor Init_file_line(Const AFile : String; ALine : longint);
     constructor Init_type(atyp : BreakpointType;Const AFunc : String);
     procedure  Insert;
     procedure  Remove;
     procedure  Enable;
     procedure  Disable;
     destructor Done;virtual;
  end;

  TBreakpointCollection=object(TCollection)
      function  At(Index: Integer): PBreakpoint;
      function  ToggleFileLine(Const FileName: String;LineNr : Longint) : boolean;
      procedure FreeItem(Item: Pointer); virtual;
    end;

var
  Debugger : PDebugController;
  BreakpointCollection : PBreakpointCollection;
  
procedure InitDebugger;
procedure DoneDebugger;

implementation

uses
  Dos,Mouse,Video,
  App,Strings,
  FPViews,FPVars,FPUtils,FPIntf,
  FPCompile,FPIde;


{****************************************************************************
                            TDebugController
****************************************************************************}

constructor TDebugController.Init(const exefn:string);
  var f: string;
begin
  inherited Init;
  f := exefn;
  LoadFile(f);
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

destructor TDebugController.Done;
begin
  { kill the program if running }
  Reset;
  RemoveBreakpoints;
  inherited Done;
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
  
begin
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
          W^.Select;
          Invalid_line:=false;
        end
      else
        Invalid_line:=true;
    end
  else
    begin
      W:=TryToOpenFile(nil,fn,0,Line);
      if assigned(W) then
        begin
          W^.Editor^.SetHighlightRow(Line);
          W^.Editor^.TrackCursor(true);
          W^.Select;
          LastSource:=W;
          Invalid_line:=false;
        end
        { only search a file once }
      else
       begin
         Desktop^.UnLock;
         Found:=MyApp.OpenSearch(fn);
         Desktop^.Lock;
         if not Found then
           begin
             Invalid_line:=true;
             LastSource:=Nil;
           end
         else
           begin
             { should now be open }
              W:=TryToOpenFile(nil,fn,0,Line);
              W^.Editor^.SetHighlightRow(Line);
              W^.Editor^.TrackCursor(true);
              W^.Select;
              LastSource:=W;
              Invalid_line:=false;
           end;
       end;
    end;
  LastFileName:=fn;
  Desktop^.UnLock;
end;

procedure TDebugController.DoEndSession(code:longint);
begin
   InformationBox(#3'Program exited with '#13#3'exitcode = %d',@code);
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
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
end;

constructor TBreakpoint.Init_type(atyp : BreakpointType;Const AFunc : String);
begin
  typ:=atyp;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=NewStr(AFunc);
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
end;

constructor TBreakpoint.Init_file_line(Const AFile : String; ALine : longint);
begin
  typ:=bt_file_line;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=NewStr(AFile);
  Line:=ALine;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
end;

procedure TBreakpoint.Insert;
begin
  If not assigned(Debugger) then Exit;
  
  Debugger^.last_breakpoint_number:=0;
  if (GDBState=bs_deleted) and (state=bs_enabled) then
    begin
      if (typ=bt_file_line) then
        Debugger^.Command('break '+name^+':'+IntToStr(Line))
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
    Debugger^.Command('enable '+IntToStr(GDBIndex));
  GDBState:=bs_enabled;
end;

procedure TBreakpoint.Disable;
begin
  If not assigned(Debugger) then Exit;
  if GDBIndex>0 then
    Debugger^.Command('disable '+IntToStr(GDBIndex));
  GDBState:=bs_disabled;
end;

destructor TBreakpoint.Done;
begin
  if assigned(Name) then
    DisposeStr(Name);
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
  if Item<>nil then Dispose(PBreakpoint(Item),Done);
end;

function TBreakpointCollection.ToggleFileLine(Const FileName: String;LineNr : Longint) : boolean;

var PB : PBreakpoint;

  function IsThere(P : PBreakpoint) : boolean;
  begin
    IsThere:=(P^.typ=bt_file_line) and (P^.Name^=FileName) and (P^.Line=LineNr);
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
end;


procedure DoneDebugger;
begin
  if assigned(Debugger) then
   dispose(Debugger,Done);
   If Use_gdb_file then
     Close(GDB_file);
   Use_gdb_file:=false;
end;

begin
  New(BreakpointCollection,init(10,10));
end.

{
  $Log$
  Revision 1.7  1999-02-05 13:08:41  pierre
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
