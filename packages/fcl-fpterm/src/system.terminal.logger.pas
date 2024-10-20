{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  Copyright (C) 2022, 2024 Nikolay Nikolov <nickysn@users.sourceforge.net>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit System.Terminal.Logger;

{$mode objfpc}{$H+}

interface

type
  TVerbosityLevel = (
    vlSpam,
    vlDebug,
    vlVerbose,
    vlInfo,
    vlNotice,
    vlWarning,
    vlSuccess,
    vlError,
    vlCritical);

const
  DefaultLogLevel = vlWarning;

type

  { TLogger }

  TLogger = class
  public
    procedure LogMessage(AVerbosityLevel: TVerbosityLevel; const msg: string); virtual; abstract;
  end;

  { TSilentLogger }

  TSilentLogger = class(TLogger)
  public
    procedure LogMessage(AVerbosityLevel: TVerbosityLevel; const msg: string); override;
  end;

  { TConsoleLogger }

  TConsoleLogger = class(TLogger)
  private
    FVerbosityLevel: TVerbosityLevel;
  public
    constructor Create(AVerbosityLevel: TVerbosityLevel);
    procedure LogMessage(AVerbosityLevel: TVerbosityLevel; const msg: string); override;
  end;

  { TFileLogger }

  TFileLogger = class(TLogger)
  private
    FVerbosityLevel: TVerbosityLevel;
    FFileName: string;
  public
    constructor Create(const AFileName: string; AVerbosityLevel: TVerbosityLevel);
    procedure LogMessage(AVerbosityLevel: TVerbosityLevel; const msg: string); override;
  end;

function Str2VerbosityLevel(const S: string): TVerbosityLevel;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

function Str2VerbosityLevel(const S: string): TVerbosityLevel;
begin
  case UpCase(S) of
    'SPAM':
      Result := vlSpam;
    'DEBUG':
      Result := vlDebug;
    'VERBOSE':
      Result := vlVerbose;
    'INFO':
      Result := vlInfo;
    'NOTICE':
      Result := vlNotice;
    'WARNING':
      Result := vlWarning;
    'SUCCESS':
      Result := vlSuccess;
    'ERROR':
      Result := vlError;
    'CRITICAL':
      Result := vlCritical;
    else
      raise EArgumentException.Create('Invalid verbosity level');
  end;
end;

{ TSilentLogger }

procedure TSilentLogger.LogMessage(AVerbosityLevel: TVerbosityLevel; const msg: string);
begin
end;

{ TConsoleLogger }

constructor TConsoleLogger.Create(AVerbosityLevel: TVerbosityLevel);
begin
  FVerbosityLevel := AVerbosityLevel;
end;

procedure TConsoleLogger.LogMessage(AVerbosityLevel: TVerbosityLevel; const msg: string);
begin
  if AVerbosityLevel >= FVerbosityLevel then
    Writeln(msg);
end;

{ TFileLogger }

constructor TFileLogger.Create(const AFileName: string; AVerbosityLevel: TVerbosityLevel);
var
  F: TextFile;
begin
  FFileName := AFileName;
  FVerbosityLevel := AVerbosityLevel;
  AssignFile(F, FFileName);
  Rewrite(F);
  CloseFile(F);
end;

procedure TFileLogger.LogMessage(AVerbosityLevel: TVerbosityLevel; const msg: string);
var
  F: TextFile;
begin
  if AVerbosityLevel >= FVerbosityLevel then
  begin
    AssignFile(F, FFileName);
    {$I-}
    Append(F);
    {$I+}
    if IOResult <> 0 then
      Rewrite(F);
    try
      Writeln(F, msg);
    finally
      CloseFile(F);
    end;
  end;
end;

end.

