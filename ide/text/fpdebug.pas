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
  Objects,GDBCon;

type
  PDebugController=^TDebugController;
  TDebugController=object(TGDBController)
     Invalid_line : boolean;
     LastFileName : string;
    constructor Init(const exefn:string);
    destructor  Done;
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
{    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;}
    procedure DoEndSession(code:longint);virtual;
    procedure AnnotateError;
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
  end;

  BreakpointType = (bt_function,bt_file_line,bt_invalid);
  BreakpointState = (bs_enabled,bs_disabled,bs_invalid);

  PBreakpointCollection=^TBreakpointCollection;

  PBreakpoint=^TBreakpoint;
  TBreakpoint=object(TObject)
     typ  : BreakpointType;
     state : BreakpointState;
     owner : PBreakpointCollection;
     Name : PString;  { either function name or file name }
     Line : Longint; { only used for bt_file_line type }
     constructor Init_function(Const AFunc : String);
     constructor Init_file_line(Const AFile : String; ALine : longint);
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
  App,
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
end;


destructor TDebugController.Done;
begin
  { kill the program if running }
  Reset;
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
begin
  Desktop^.Lock;
  if Line>0 then
   dec(Line);
  W:=TryToOpenFile(nil,fn,0,Line);
  if assigned(W) then
   begin
     W^.Editor^.SetHighlightRow(Line);
     W^.Select;
     Invalid_line:=false;
   end
   { only search a file once }
  else if fn<>LastFileName then
   begin
     if not MyApp.OpenSearch(fn+'*') then
       Invalid_line:=true;
   end
  else
    Invalid_line:=true;
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
  GetMem(Name,Length(AFunc)+1);
  Name^:=AFunc;
end;

constructor TBreakpoint.Init_file_line(Const AFile : String; ALine : longint);
begin
  typ:=bt_file_line;
  state:=bs_enabled;
  GetMem(Name,Length(AFile)+1);
  Name^:=AFile;
  Line:=ALine;
end;


destructor TBreakpoint.Done;
begin
  if assigned(Name) then
    FreeMem(Name,Length(Name^)+1);
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
end;

begin
  New(BreakpointCollection,init(10,10));
end.

{
  $Log$
  Revision 1.4  1999-02-04 13:32:02  pierre
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
