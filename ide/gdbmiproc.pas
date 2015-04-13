{
    Copyright (c) 2015 by Nikolay Nikolov

    This unit implements a class, which launches gdb in GDB/MI mode
    and allows sending textual commands to it and receiving the response

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit GDBMIProc;

{$MODE objfpc}{$H+}

{$I globdir.inc}

interface

uses
  SysUtils, Classes, Process;

type
  TGDBProcess = class
  private
    FProcess: TProcess;
    FDebugLog: TextFile;

    function IsAlive: Boolean;
    procedure GDBWrite(const S: string);
    procedure DebugLn(const S: string);
    procedure DebugErrorLn(const S: string);
  public
    constructor Create;
    destructor Destroy; override;
    function GDBReadLn: string;
    procedure GDBWriteLn(const S: string);
    property Alive: Boolean read IsAlive;
  end;

var
  GdbProgramName: string = 'gdb';

implementation

var
  DebugLogEnabled: Boolean = False;

function TGDBProcess.IsAlive: Boolean;
begin
  Result := Assigned(FProcess) and FProcess.Running;
end;

function TGDBProcess.GDBReadLn: string;
var
  C: Char;
begin
  Result := '';
  while FProcess.Running do
  begin
    FProcess.Output.Read(C, 1);
{$ifdef windows}
    { On windows we expect both #13#10 and #10 }
    if C = #13 then
    begin
      FProcess.Output.Read(C, 1);
      if C <> #10 then
        { #13 not followed by #10, what should we do? }
        Result := Result + #13;
    end;
{$endif windows}
    if C = #10 then
    begin
      DebugLn(Result);
      exit;
    end;
    Result := Result + C;
  end;
end;

constructor TGDBProcess.Create;
begin
  if DebugLogEnabled then
  begin
    AssignFile(FDebugLog, 'gdblog.txt');
    Rewrite(FDebugLog);
    CloseFile(FDebugLog);
  end;
  FProcess := TProcess.Create(nil);
  FProcess.Options := [poUsePipes, poStdErrToOutput];
  FProcess.Executable := GdbProgramName;
  FProcess.Parameters.Add('--interpreter=mi');
  try
    FProcess.Execute;
  except
    on e: Exception do
    begin
      DebugErrorLn('Could not start GDB: ' + e.Message);
      FreeAndNil(FProcess);
    end;
  end;
end;

destructor TGDBProcess.Destroy;
begin
  FProcess.Free;
  inherited Destroy;
end;

procedure TGDBProcess.DebugLn(const S: string);
begin
  if DebugLogEnabled then
  begin
    Append(FDebugLog);
    Writeln(FDebugLog, S);
    CloseFile(FDebugLog);
  end;
end;

procedure TGDBProcess.DebugErrorLn(const S: string);
begin
  DebugLn('ERROR: ' + S);
end;

procedure TGDBProcess.GDBWrite(const S: string);
begin
  FProcess.Input.Write(S[1], Length(S));
end;

procedure TGDBProcess.GDBWriteln(const S: string);
begin
  if not IsAlive then
  begin
    DebugErrorLn('Trying to send command to a dead GDB: ' + S);
    exit;
  end;
  DebugLn(S);
  GDBWrite(S + #10);
end;

begin
  if GetEnvironmentVariable('FPIDE_GDBLOG') = '1' then
    DebugLogEnabled := True;
  if GetEnvironmentVariable('FPIDE_GDBPROG') <> '' then
    GdbProgramName := GetEnvironmentVariable('FPIDE_GDBPROG');
end.
