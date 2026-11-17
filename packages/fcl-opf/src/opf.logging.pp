{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    OPF Logger (can be hooked)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Logging;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  Opf.Types;

Type
  TLogKind = (lkWarning, lkError, lkInfo, lkTrace, lkSQL);
  TLogKinds = set of TLogKind;

  TOPLogger = Class;
  TOPLoggerClass = Class of TOPLogger;

  TOPLogEvent = procedure(Sender: TObject; aLevel: TLogKind; const aMsg: string) of object;

  { TOPLogger }

  TOPLogger = Class(TOPBaseComponent)
  Private
    Class var
      _Instance : TOPLogger;
      _InstanceClass : TOPLoggerClass;
  Private
    FLogKinds: TLogKinds;
    FLogToConsole: Boolean;
    FLogFileName: String;
    FLogFile: TextFile;
    FLogFileActive: Boolean;
    // Serialises the file output: several threads may log at the same time
    FFileLock: TRTLCriticalSection;
    FAddTimeStamp: Boolean;
    FOnLog: TOPLogEvent;
    procedure SetLogFileName(const AValue: String);
    procedure EnsureFileOpen;
    function FormatLogMessage(Sender: TObject; aLevel: TLogKind; const Msg: string): string;
  Public
    // Creates a logger that logs warnings, errors and info messages
    Constructor Create(aOwner: TComponent); override;
    // Closes the log file and frees the logger
    Destructor Destroy; override;
    // Frees the shared logger instance when the program ends
    Class Destructor Cleanup;
    // Sets the class used to create the shared logger instance
    Class Procedure SetInstanceClass(aClass: TOPLoggerClass);
    // Returns the shared logger instance, creating it when needed
    Class Function Instance: TOPLogger;
    // Frees the shared logger instance
    Class Procedure ClearInstance;
    // The exception class raised for logger errors
    class function GetExceptionClass: EOPFClass; override;
    // Logs a message of the given kind
    procedure Log(Sender: TObject; aLevel: TLogKind; const Msg: string);
    // Logs a message built from a format string and arguments
    procedure Log(Sender: TObject; aLevel: TLogKind; const Fmt: string; const Args: array of const);
    // Logs a message without a sender
    procedure Log(aLevel: TLogKind; const Msg: string);
    // Logs an info message
    procedure LogInfo(Sender: TObject; const Msg: string);
    // Logs an info message built from a format string and arguments
    procedure LogInfo(Sender: TObject; const Fmt: string; const Args: array of const);
    // Logs an error message
    procedure LogError(Sender: TObject; const Msg: string);
    // Logs an error message built from a format string and arguments
    procedure LogError(Sender: TObject; const Fmt: string; const Args: array of const);
    // Logs a warning message
    procedure LogWarning(Sender: TObject; const Msg: string);
    // Logs a warning message built from a format string and arguments
    procedure LogWarning(Sender: TObject; const Fmt: string; const Args: array of const);
    // Logs a trace message
    procedure LogTrace(Sender: TObject; const Msg: string);
    // Logs a trace message built from a format string and arguments
    procedure LogTrace(Sender: TObject; const Fmt: string; const Args: array of const);
    // True if the logger writes anywhere: console, file or event
    function IsActive: Boolean;
    // True if the logger is active and messages of the given kind are logged
    function IsKindActive(aKind: TLogKind): Boolean;
  Published
    // The kinds of messages that are logged
    Property LogKinds: TLogKinds Read FLogKinds Write FLogKinds;
    // Write log messages to the console
    Property LogToConsole: Boolean Read FLogToConsole Write FLogToConsole default False;
    // Name of the file to append log messages to; empty means no log file.
    // Writing to the file is thread-safe; console output and OnLog are not
    // serialised.
    Property LogFileName: String Read FLogFileName Write SetLogFileName;
    // Put the date and time in front of each log message
    Property AddTimeStamp: Boolean Read FAddTimeStamp Write FAddTimeStamp default False;
    // Called for every message that is logged
    Property OnLog: TOPLogEvent Read FOnLog Write FOnLog;
  end;

Function OPLogger: TOPLogger;

implementation

uses {$IFDEF FPC_DOTTEDUNITS}System.TypInfo{$ELSE}typinfo{$ENDIF};

Function OPLogger: TOPLogger;
begin
  Result:=TOPLogger.Instance;
end;

{ TOPLogger }

constructor TOPLogger.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLogKinds:=[lkWarning, lkError, lkInfo];
  FLogToConsole:=False;
  FLogFileActive:=False;
  FAddTimeStamp:=False;
  InitCriticalSection(FFileLock);
end;

destructor TOPLogger.Destroy;
begin
  if FLogFileActive then
  begin
    // A destructor must not raise, so a failure to close the log file is ignored.
    try
      CloseFile(FLogFile);
    except
    end;
    FLogFileActive:=False;
  end;
  DoneCriticalSection(FFileLock);
  inherited Destroy;
end;

class destructor TOPLogger.Cleanup;
begin
  ClearInstance;
end;

class procedure TOPLogger.SetInstanceClass(aClass: TOPLoggerClass);
begin
  if Assigned(_Instance) then
    _Instance.RaiseError('SetInstanceClass', 'An instance was already created');
  _InstanceClass:=aClass;
end;

class function TOPLogger.Instance: TOPLogger;
begin
  if _Instance=Nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Instance=Nil then
      begin
        if _InstanceClass=Nil then
          _InstanceClass:=TOPLogger;
        _Instance:=_InstanceClass.Create(Nil);
      end;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result:=_Instance;
end;

class procedure TOPLogger.ClearInstance;
begin
  FreeAndNil(_Instance);
  _InstanceClass:=Nil;
end;

class function TOPLogger.GetExceptionClass: EOPFClass;
begin
  Result:=EOPFLogging;
end;

procedure TOPLogger.SetLogFileName(const AValue: String);
begin
  EnterCriticalSection(FFileLock);
  try
    if FLogFileName=AValue then Exit;
    if FLogFileActive then
    begin
      FLogFileActive:=False;
      CloseFile(FLogFile);
    end;
    FLogFileName:=AValue;
  finally
    LeaveCriticalSection(FFileLock);
  end;
end;

procedure TOPLogger.EnsureFileOpen;
begin
  if FLogFileActive then Exit;
  if FLogFileName='' then Exit;
  AssignFile(FLogFile, FLogFileName);
  if FileExists(FLogFileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
  FLogFileActive:=True;
end;

function TOPLogger.FormatLogMessage(Sender: TObject; aLevel: TLogKind; const Msg: string): string;
var
  TS, SenderStr, LevelStr: string;
begin
  if FAddTimeStamp then
    TS:=FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now) + ' '
  else
    TS:='';
  LevelStr:=Copy(GetEnumName(TypeInfo(TLogKind), Ord(aLevel)), 3, MaxInt);
  if Sender=nil then
    SenderStr:='(nil)'
  else
    SenderStr:=Sender.ClassName + '(' + HexStr(Pointer(Sender)) + ')';
  Result:=TS + '[' + LevelStr + '] (' + SenderStr + ') ' + Msg;
end;

function TOPLogger.IsActive: Boolean;
begin
  Result:=FLogToConsole or (FLogFileName <> '') or Assigned(FOnLog);
end;

function TOPLogger.IsKindActive(aKind: TLogKind): Boolean;
begin
  Result:=IsActive and (aKind in FLogKinds);
end;

procedure TOPLogger.Log(Sender: TObject; aLevel: TLogKind; const Msg: string);
var
  FormattedMsg: string;
begin
  if not (aLevel in FLogKinds) then
    Exit;
  FormattedMsg:=FormatLogMessage(Sender, aLevel, Msg);
  if FLogToConsole then
    try
      Writeln(StdErr, FormattedMsg);
    except
      // Logging must not crash the application
    end;
  if FLogFileName <> '' then
  begin
    EnterCriticalSection(FFileLock);
    try
      try
        EnsureFileOpen;
        if FLogFileActive then
        begin
          Writeln(FLogFile, FormattedMsg);
          Flush(FLogFile);
        end;
      except
        // Logging must not crash the application
      end;
    finally
      LeaveCriticalSection(FFileLock);
    end;
  end;
  // A failing handler must not replace the error being logged
  if Assigned(FOnLog) then
    try
      FOnLog(Sender, aLevel, FormattedMsg);
    except
      on E: Exception do
        if FLogToConsole then
          try
            Writeln(StdErr, 'OnLog handler failed: ', E.Message);
          except
          end;
    end;
end;

procedure TOPLogger.Log(Sender: TObject; aLevel: TLogKind; const Fmt: string; const Args: array of const);
begin
  Log(Sender, aLevel, Format(Fmt, Args));
end;

procedure TOPLogger.Log(aLevel: TLogKind; const Msg: string);
begin
  Log(TObject(nil), aLevel, Msg);
end;

procedure TOPLogger.LogInfo(Sender: TObject; const Msg: string);
begin
  Log(Sender, lkInfo, Msg);
end;

procedure TOPLogger.LogInfo(Sender: TObject; const Fmt: string; const Args: array of const);
begin
  Log(Sender, lkInfo, Format(Fmt, Args));
end;

procedure TOPLogger.LogError(Sender: TObject; const Msg: string);
begin
  Log(Sender, lkError, Msg);
end;

procedure TOPLogger.LogError(Sender: TObject; const Fmt: string; const Args: array of const);
begin
  Log(Sender, lkError, Format(Fmt, Args));
end;

procedure TOPLogger.LogWarning(Sender: TObject; const Msg: string);
begin
  Log(Sender, lkWarning, Msg);
end;

procedure TOPLogger.LogWarning(Sender: TObject; const Fmt: string; const Args: array of const);
begin
  Log(Sender, lkWarning, Format(Fmt, Args));
end;

procedure TOPLogger.LogTrace(Sender: TObject; const Msg: string);
begin
  Log(Sender, lkTrace, Msg);
end;

procedure TOPLogger.LogTrace(Sender: TObject; const Fmt: string; const Args: array of const);
begin
  Log(Sender, lkTrace, Format(Fmt, Args));
end;

end.
