{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  This unit implements an Unix-like pseudoterminal as a connection for the
  terminal.

  Copyright (C) 2024 Nikolay Nikolov <nickysn@users.sourceforge.net>

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

unit FpTerm.InputOutputConnection.UnixPty;

{$mode objfpc}{$H+}

interface

uses
  FpTerm.InputOutputConnection, FpTerm.Pseudoterminal.Unix;

type

  { TTerminalInputOutputConnection_UnixPty }

  TTerminalInputOutputConnection_UnixPty = class(TInterfacedObject, ITerminalInputOutputConnection)
  private
    FUnixPseudoTerminal: TUnixPseudoTerminal;
  protected
    function IsDataAvailable: Boolean;
    function IsClosed: Boolean;
  public
    constructor Create(const progname: ansistring; argv, envp: PPansiChar; Width, Height: Integer);
    destructor Destroy; override;

    function Read(var Buffer; Bytes: SizeUInt): SizeInt;
    procedure Write(const Buffer; Bytes: SizeUInt);
    procedure Resize(NewWidth, NewHeight: Integer);
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, UnixApi.Base;
{$ELSE FPC_DOTTEDUNITS}
  SysUtils, BaseUnix;
{$ENDIF FPC_DOTTEDUNITS}

{ TTerminalInputOutputConnection_UnixPty }

function TTerminalInputOutputConnection_UnixPty.IsDataAvailable: Boolean;
var
  pfd: tpollfd;
begin
  pfd.fd := FUnixPseudoTerminal.FdMaster;
  pfd.events := POLLIN;
  Result := FpPoll(@pfd, 1, 0) > 0;
end;

function TTerminalInputOutputConnection_UnixPty.IsClosed: Boolean;
var
  wstatus: cInt;
  wpid: TPid;
begin
  Result := False;
  wpid := FpWaitPid(FUnixPseudoTerminal.ChildPid, wstatus, WNOHANG);
  if wpid = FUnixPseudoTerminal.ChildPid then
  begin
    if wifexited(wstatus) then
      Result := True;
  end;
end;

constructor TTerminalInputOutputConnection_UnixPty.Create(
  const progname: ansistring; argv, envp: PPAnsiChar; Width, Height: Integer);
begin
  FUnixPseudoTerminal := TUnixPseudoTerminal.Create(progname, argv, envp, Width, Height);
end;

destructor TTerminalInputOutputConnection_UnixPty.Destroy;
begin
  FreeAndNil(FUnixPseudoTerminal);
  inherited Destroy;
end;

function TTerminalInputOutputConnection_UnixPty.Read(var Buffer; Bytes: SizeUInt): SizeInt;
begin
  Result := FpRead(FUnixPseudoTerminal.FdMaster, Buffer, Bytes);
end;

procedure TTerminalInputOutputConnection_UnixPty.Write(const Buffer; Bytes: SizeUInt);
var
  W: TsSize;
  J: LongInt;
begin
  if Bytes <= 0 then
    exit;
  repeat
    W := FpWrite(FUnixPseudoTerminal.FdMaster, Buffer, Bytes);
    J := FpGetErrno;
  until (W <> -1) or ((J <> ESysEINTR) and (J <> ESysEAgain));
end;

procedure TTerminalInputOutputConnection_UnixPty.Resize(NewWidth, NewHeight: Integer);
begin
  FUnixPseudoTerminal.Resize(NewWidth, NewHeight);
end;

end.

