{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit process;

interface

Uses Classes,
     pipes,
     SysUtils;

Type
  TProcessOption = (poRunSuspended,poWaitOnExit,
                    poUsePipes,poStderrToOutPut,
                    poNoConsole,poNewConsole,
                    poDefaultErrorMode,poNewProcessGroup,
                    poDebugProcess,poDebugOnlyThisProcess);

  TShowWindowOptions = (swoNone,swoHIDE,swoMaximize,swoMinimize,swoRestore,swoShow,
                        swoShowDefault,swoShowMaximized,swoShowMinimized,
                        swoshowMinNOActive,swoShowNA,swoShowNoActivate,swoShowNormal);

  TStartupOption = (suoUseShowWindow,suoUseSize,suoUsePosition,
                    suoUseCountChars,suoUseFillAttribute);

  TProcessPriority = (ppHigh,ppIdle,ppNormal,ppRealTime);

  TProcessOptions = Set of TPRocessOption;
  TstartUpoptions = set of TStartupOption;


Type
  TProcess = Class (TComponent)
  Private
    FProcessOptions : TProcessOptions;
    FStartupOptions : TStartupOptions;
    FProcessID : Integer;
    FThreadID : Integer;
    FProcessHandle : Thandle;
    FThreadHandle : Thandle;
    FFillAttribute : Cardinal;
    FApplicationName : string;
    FConsoleTitle : String;
    FCommandLine : String;
    FCurrentDirectory : String;
    FDeskTop : String;
    FEnvironment : Tstrings;
    FExitCode : Cardinal;
    FShowWindow : TShowWindowOptions;
    FInherithandles : Boolean;
    FInputSTream  : TOutputPipeStream;
    FOutputStream : TInPutPipeStream;
    FStdErrStream : TInputPipeStream;
    FRunning : Boolean;
    FPRocessPriority : TProcessPriority;
    dwXCountchars,
    dwXSize,
    dwYsize,
    dwx,
    dwYcountChars,
    dwy : Cardinal;
    Procedure FreeStreams;
    Function  GetExitStatus : Integer;
    Function  GetRunning : Boolean;
    Function  GetWindowRect : TRect;
    Procedure SetWindowRect (Value : TRect);
    Procedure SetShowWindow (Value : TShowWindowOptions);
    Procedure SetWindowColumns (Value : Cardinal);
    Procedure SetWindowHeight (Value : Cardinal);
    Procedure SetWindowLeft (Value : Cardinal);
    Procedure SetWindowRows (Value : Cardinal);
    Procedure SetWindowTop (Value : Cardinal);
    Procedure SetWindowWidth (Value : Cardinal);
    Procedure CreateStreams(InHandle,OutHandle,Errhandle : Longint);
    procedure SetApplicationname(const Value: String);
    procedure SetProcessOptions(const Value: TProcessOptions);
    procedure SetActive(const Value: Boolean);
    procedure SetEnvironment(const Value: TStrings);
    function  PeekExitStatus: Boolean;
    procedure CloseProcessHandles;
  Public
    Constructor Create (AOwner : TComponent);override;
    Destructor Destroy; override;
    Procedure Execute; virtual;
    Function Resume : Integer; virtual;
    Function Suspend : Integer; virtual;
    Function Terminate (AExitCode : Integer): Boolean; virtual;
    Function WaitOnExit : DWord;
    Property WindowRect : Trect Read GetWindowRect Write SetWindowRect;
    Property Handle : THandle Read FProcessHandle;
    Property ProcessHandle : THandle Read FProcessHandle;
    Property ThreadHandle : THandle Read FThreadHandle;
    Property Input  : TOutPutPipeStream Read FInPutStream;
    Property OutPut : TInputPipeStream  Read FOutPutStream;
    Property StdErr : TinputPipeStream  Read FStdErrStream;
    Property ExitStatus : Integer Read GetExitStatus;
    Property InheritHandles : Boolean Read FInheritHandles Write FInheritHandles;
  Published
    Property Active : Boolean Read Getrunning Write SetActive;
    Property ApplicationName : String Read FApplicationname Write SetApplicationname;
    Property CommandLine : String Read FCommandLine Write FCommandLine;
    Property ConsoleTitle : String Read FConsoleTitle Write FConsoleTitle;
    Property CurrentDirectory : String Read FCurrentDirectory Write FCurrentDirectory;
    Property DeskTop : String Read FDeskTop Write FDeskTop;
    Property Environment : TStrings Read FEnvironment Write SetEnvironment;
    Property Options : TProcessOptions Read FProcessOptions Write SetPRocessOptions;
    Property Priority : TProcessPriority Read FProcessPriority Write FProcessPriority;
    Property StartUpOptions : TStartUpOptions Read FStartUpOptions Write FStartupOptions;
    Property Running : Boolean Read GetRunning;
    Property ShowWindow : TShowWindowOptions Read FShowWindow Write SetShowWindow;
    Property WindowColumns : Cardinal Read dwXCountchars Write SetWindowColumns;
    Property WindowHeight : Cardinal Read dwYsize Write SetWindowHeight;
    Property WindowLeft : Cardinal Read dwx Write SetWindowLeft;
    Property WindowRows : Cardinal Read dwYcountChars Write SetWindowRows;
    Property WindowTop : Cardinal Read dwy Write SetWindowTop ;
    Property WindowWidth : Cardinal Read dwXsize Write SetWindowWidth;
    Property FillAttribute : Cardinal read FFillAttribute Write FFillAttribute;
  end;

implementation

{$i process.inc}

Constructor TProcess.Create (AOwner : TComponent);
begin
  Inherited;
  FProcessPriority:=ppNormal;
  FShowWindow:=swoNone;
  FInheritHandles:=True;
  FEnvironment:=TStringList.Create;
end;

Destructor TProcess.Destroy;

begin
  FEnvironment.Free;
  FreeStreams;
  CloseProcessHandles;
  Inherited Destroy;
end;

Procedure TProcess.FreeStreams;

  procedure FreeStream(var S: THandleStream);

  begin
    if (S<>Nil) then
      begin
      FileClose(S.Handle);
      FreeAndNil(S);
      end;
  end;

begin
  If FStdErrStream<>FOutputStream then
    FreeStream(FStdErrStream);
  FreeStream(FOutputStream);
  FreeStream(FInputStream);
end;


Function TProcess.GetExitStatus : Integer;

begin
  If FRunning then
    PeekExitStatus;
  Result:=FExitCode;
end;


Function TProcess.GetRunning : Boolean;

begin
  IF FRunning then
    FRunning:=Not PeekExitStatus;
  Result:=FRunning;
end;


Procedure TProcess.CreateStreams(InHandle,OutHandle,Errhandle : Longint);

begin
  FreeStreams;
  FInputStream:=TOutputPipeStream.Create (InHandle);
  FOutputStream:=TInputPipeStream.Create (OutHandle);
  if Not (poStdErrToOutPut in FProcessOptions) then
    FStdErrStream:=TInputPipeStream.Create(ErrHandle);
end;


Procedure TProcess.SetWindowColumns (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartUpOptions,suoUseCountChars);
  dwXCountChars:=Value;  
end;


Procedure TProcess.SetWindowHeight (Value : Cardinal);

begin
  if Value<>0 then
    include(FStartUpOptions,suoUsePosition);
  dwYSize:=Value;  
end;

Procedure TProcess.SetWindowLeft (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartUpOptions,suoUseSize);
  dwx:=Value;
end;

Procedure TProcess.SetWindowTop (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartUpOptions,suoUsePosition);
  dwy:=Value;  
end;

Procedure TProcess.SetWindowWidth (Value : Cardinal);
begin
  If (Value<>0) then
    Include(FStartUpOptions,suoUseSize);
  dwXSize:=Value;  
end;

Function TProcess.GetWindowRect : TRect;
begin
  With Result do
    begin
    Left:=dwx;
    Right:=dwx+dwxSize;
    Top:=dwy;
    Bottom:=dwy+dwysize;
    end;
end;

Procedure TProcess.SetWindowRect (Value : Trect);
begin
  Include(FStartupOptions,suouseSize);
  Include(FStartupOptions,suoUsePosition);
  With Value do
    begin
    dwx:=Left;
    dwxSize:=Right-Left;
    dwy:=Top;
    dwySize:=Bottom-top;
    end;
end;


Procedure TProcess.SetWindowRows (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartUpOptions,suoUseCountChars);
  dwYCountChars:=Value;
end;

procedure TProcess.SetApplicationname(const Value: String);
begin
  FApplicationname := Value;
  If (csdesigning in ComponentState) and
     (FCommandLine='') then
    FCommandLine:=Value;
end;

procedure TProcess.SetProcessOptions(const Value: TProcessOptions);
begin
  FProcessOptions := Value;
  If poNewConsole in FPRocessOptions then
    Exclude(FProcessoptions,poNoConsole);
  if poRunSuspended in FProcessOptions then
    Exclude(FPRocessoptions,poWaitOnExit);
end;

procedure TProcess.SetActive(const Value: Boolean);
begin
  if (Value<>GetRunning) then
    If Value then
      Execute
    else
      Terminate(0);
end;

procedure TProcess.SetEnvironment(const Value: TStrings);
begin
  FEnvironment.Assign(Value);
end;

end.
{
  $Log$
  Revision 1.23  2004-09-09 13:47:38  michael
  + Patch from Vincent Snijders to correctly handle PeekExitStatus

  Revision 1.22  2004/09/08 18:17:23  michael
  + Removed extra handle on process.

  Revision 1.21  2004/08/12 14:33:55  michael
  + New split of process.pp

  Revision 1.20  2004/07/30 12:55:42  michael
  Closing process handles in Windows. Patch from Vincent Snijders

  Revision 1.19  2004/02/03 08:12:22  michael
  + Patch from Vincent Snijders to fix passing environment vars in win32

  Revision 1.18  2003/10/30 20:34:47  florian
    * fixed inherited destroy; call of tprocess

  Revision 1.17  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.16  2003/08/12 13:49:42  michael
  + Freed streams were not closed correctly

  Revision 1.15  2003/05/08 20:04:16  armin
  * Dont close FStartupInfo.hStdError if options include poStdErrToOutPut

  Revision 1.14  2003/04/27 21:21:42  sg
  * Added typecast to prevent range check error in TProcess.WaitOnExit

  Revision 1.13  2002/09/07 15:15:25  peter
    * old logs removed and tabs fixed

}
