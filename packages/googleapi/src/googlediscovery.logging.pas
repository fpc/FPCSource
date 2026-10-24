{
  GoogleDiscovery.Logging - Logging infrastructure

  Provides structured logging with configurable levels and output.
}
unit GoogleDiscovery.Logging;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  GoogleDiscovery.Types;

type
  { Log output handler type }
  TLogHandler = procedure(const ALevel: TLogLevel; const AMessage: string;
    const AContext: string) of object;

  { Logger class }
  TLogger = class
  private
    FMinLevel: TLogLevel;
    FEnabled: Boolean;
    FShowTimestamp: Boolean;
    FShowLevel: Boolean;
    FContext: string;
    FLogHandler: TLogHandler;
    procedure DefaultLogHandler(const ALevel: TLogLevel; const AMessage: string;
      const AContext: string);
  public
    constructor Create(const AContext: string = '');
    destructor Destroy; override;

    { Logging methods }
    procedure Debug(const AMessage: string); overload;
    procedure Debug(const AFormat: string; const AArgs: array of const); overload;
    procedure Info(const AMessage: string); overload;
    procedure Info(const AFormat: string; const AArgs: array of const); overload;
    procedure Warn(const AMessage: string); overload;
    procedure Warn(const AFormat: string; const AArgs: array of const); overload;
    procedure Error(const AMessage: string); overload;
    procedure Error(const AFormat: string; const AArgs: array of const); overload;

    { Generic log method }
    procedure Log(const ALevel: TLogLevel; const AMessage: string); overload;
    procedure Log(const ALevel: TLogLevel; const AFormat: string;
      const AArgs: array of const); overload;

    { Check if level would be logged }
    function IsLevelEnabled(const ALevel: TLogLevel): Boolean;

    { Properties }
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    property Enabled: Boolean read FEnabled write FEnabled;
    property ShowTimestamp: Boolean read FShowTimestamp write FShowTimestamp;
    property ShowLevel: Boolean read FShowLevel write FShowLevel;
    property Context: string read FContext write FContext;
    property LogHandler: TLogHandler read FLogHandler write FLogHandler;
  end;

{ Global logger instance }
function GetLogger: TLogger;
function GetLogger(const AContext: string): TLogger;

{ Convenience functions using global logger }
procedure LogDebug(const AMessage: string); overload;
procedure LogDebug(const AFormat: string; const AArgs: array of const); overload;
procedure LogInfo(const AMessage: string); overload;
procedure LogInfo(const AFormat: string; const AArgs: array of const); overload;
procedure LogWarn(const AMessage: string); overload;
procedure LogWarn(const AFormat: string; const AArgs: array of const); overload;
procedure LogError(const AMessage: string); overload;
procedure LogError(const AFormat: string; const AArgs: array of const); overload;

{ Logger configuration }
procedure SetLogLevel(const ALevel: TLogLevel);
procedure SetLogEnabled(const AEnabled: Boolean);
procedure SetLogTimestamp(const AShow: Boolean);

{ Utility functions }
function FormatLogMessage(const ALevel: TLogLevel; const AMessage: string;
  const AContext: string; AShowTimestamp, AShowLevel: Boolean): string;
function GetTimestamp: string;

implementation

var
  GlobalLogger: TLogger = nil;

{ TLogger }

constructor TLogger.Create(const AContext: string);
begin
  inherited Create;
  FContext := AContext;
  FMinLevel := llInfo;
  FEnabled := True;
  FShowTimestamp := True;
  FShowLevel := True;
  FLogHandler := @DefaultLogHandler;
end;

destructor TLogger.Destroy;
begin
  inherited Destroy;
end;

procedure TLogger.DefaultLogHandler(const ALevel: TLogLevel;
  const AMessage: string; const AContext: string);
var
  FormattedMsg: string;
begin
  FormattedMsg := FormatLogMessage(ALevel, AMessage, AContext,
    FShowTimestamp, FShowLevel);
  if ALevel >= llWarn then
    WriteLn(StdErr, FormattedMsg)
  else
    WriteLn(FormattedMsg);
end;

procedure TLogger.Debug(const AMessage: string);
begin
  Log(llDebug, AMessage);
end;

procedure TLogger.Debug(const AFormat: string; const AArgs: array of const);
begin
  Log(llDebug, AFormat, AArgs);
end;

procedure TLogger.Info(const AMessage: string);
begin
  Log(llInfo, AMessage);
end;

procedure TLogger.Info(const AFormat: string; const AArgs: array of const);
begin
  Log(llInfo, AFormat, AArgs);
end;

procedure TLogger.Warn(const AMessage: string);
begin
  Log(llWarn, AMessage);
end;

procedure TLogger.Warn(const AFormat: string; const AArgs: array of const);
begin
  Log(llWarn, AFormat, AArgs);
end;

procedure TLogger.Error(const AMessage: string);
begin
  Log(llError, AMessage);
end;

procedure TLogger.Error(const AFormat: string; const AArgs: array of const);
begin
  Log(llError, AFormat, AArgs);
end;

procedure TLogger.Log(const ALevel: TLogLevel; const AMessage: string);
begin
  if FEnabled and (ALevel >= FMinLevel) and Assigned(FLogHandler) then
    FLogHandler(ALevel, AMessage, FContext);
end;

procedure TLogger.Log(const ALevel: TLogLevel; const AFormat: string;
  const AArgs: array of const);
begin
  Log(ALevel, Format(AFormat, AArgs));
end;

function TLogger.IsLevelEnabled(const ALevel: TLogLevel): Boolean;
begin
  Result := FEnabled and (ALevel >= FMinLevel);
end;

{ Global logger functions }

function GetLogger: TLogger;
begin
  if GlobalLogger = nil then
    GlobalLogger := TLogger.Create('');
  Result := GlobalLogger;
end;

function GetLogger(const AContext: string): TLogger;
begin
  Result := TLogger.Create(AContext);
end;

procedure LogDebug(const AMessage: string);
begin
  GetLogger.Debug(AMessage);
end;

procedure LogDebug(const AFormat: string; const AArgs: array of const);
begin
  GetLogger.Debug(AFormat, AArgs);
end;

procedure LogInfo(const AMessage: string);
begin
  GetLogger.Info(AMessage);
end;

procedure LogInfo(const AFormat: string; const AArgs: array of const);
begin
  GetLogger.Info(AFormat, AArgs);
end;

procedure LogWarn(const AMessage: string);
begin
  GetLogger.Warn(AMessage);
end;

procedure LogWarn(const AFormat: string; const AArgs: array of const);
begin
  GetLogger.Warn(AFormat, AArgs);
end;

procedure LogError(const AMessage: string);
begin
  GetLogger.Error(AMessage);
end;

procedure LogError(const AFormat: string; const AArgs: array of const);
begin
  GetLogger.Error(AFormat, AArgs);
end;

procedure SetLogLevel(const ALevel: TLogLevel);
begin
  GetLogger.MinLevel := ALevel;
end;

procedure SetLogEnabled(const AEnabled: Boolean);
begin
  GetLogger.Enabled := AEnabled;
end;

procedure SetLogTimestamp(const AShow: Boolean);
begin
  GetLogger.ShowTimestamp := AShow;
end;

{ Utility functions }

function GetTimestamp: string;
begin
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Now);
end;

function FormatLogMessage(const ALevel: TLogLevel; const AMessage: string;
  const AContext: string; AShowTimestamp, AShowLevel: Boolean): string;
var
  Parts: TStringList;
  I: Integer;
begin
  Parts := TStringList.Create;
  try
    if AShowTimestamp then
      Parts.Add(GetTimestamp);

    if AShowLevel then
      Parts.Add('[' + ALevel.ToString + ']');

    if AContext <> '' then
      Parts.Add('[' + AContext + ']');

    Parts.Add(AMessage);

    Result := '';
    if Parts.Count > 0 then
    begin
      Result := Parts[0];
      for I := 1 to Parts.Count - 1 do
        Result := Result + ' ' + Parts[I];
    end;
  finally
    Parts.Free;
  end;
end;

initialization

finalization
  if GlobalLogger <> nil then
    GlobalLogger.Free;

end.
