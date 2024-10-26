{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  Copyright (C) 2021, 2022, 2024 Nikolay Nikolov <nickysn@users.sourceforge.net>

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

unit FpTerm.PseudoTerminal.Unix;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CTypes, UnixApi.Base;
{$ELSE FPC_DOTTEDUNITS}
  ctypes, BaseUnix;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TUnixPseudoTerminal }

  TUnixPseudoTerminal = class
  private
    FChildPid: TPid;
    FFdSlave, FFdMaster: cint;
  public
    constructor Create(const progname: ansistring; argv, envp: PPAnsiChar; Width, Height: Integer);
    procedure Resize(NewWidth, NewHeight: Integer);

    property FdMaster: cint read FFdMaster;
    property ChildPid: TPid read FChildPid;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  UnixApi.Unix98.PTY, UnixApi.TermIO;
{$ELSE FPC_DOTTEDUNITS}
  unix98pty, termio;
{$ENDIF FPC_DOTTEDUNITS}

{ TUnixPseudoTerminal }

constructor TUnixPseudoTerminal.Create(const progname: ansistring; argv,
  envp: PPansiChar; Width, Height: Integer);
var
  ws: TWinSize;
begin
  FFdMaster:=fpposix_openpt(O_RDWR);
  if FFdMaster < 0 then
  begin
    Writeln(ErrOutput, 'Error creating a pseudo terminal');
    Halt(1);
  end;

  if fpgrantpt(FFdMaster) <> 0 then
  begin
    Writeln(ErrOutput, 'Error creating a pseudo terminal');
    Halt(1);
  end;

  if fpunlockpt(FFdMaster) <> 0 then
  begin
    Writeln(ErrOutput, 'Error creating a pseudo terminal');
    Halt(1);
  end;

  FFdSlave := FpOpen(fpptsname(FFdMaster), O_RDWR);
  if FFdSlave < 0 then
  begin
    Writeln(ErrOutput, 'Error creating a pseudo terminal');
    Halt(1);
  end;

  FillChar(ws, SizeOf(ws), 0);
  ws.ws_col := Width;
  ws.ws_row := Height;
  FpIOCtl(FFdMaster, TIOCSWINSZ, @ws);

  FChildPid:=FpFork;
  if FChildPid = 0 then
  begin
    { child }
    FpClose(FFdMaster);

    FpClose(0);
    FpClose(1);
    FpClose(2);

    FpDup(FFdSlave);
    FpDup(FFdSlave);
    FpDup(FFdSlave);

    FpSetsid;
    FpIOCtl(0, TIOCSCTTY, Pointer(0));

    FpExecve(progname, argv, envp);
    Halt(1);
  end;

  FpClose(FFdSlave);
end;

procedure TUnixPseudoTerminal.Resize(NewWidth, NewHeight: Integer);
var
  ws: TWinSize;
begin
  FillChar(ws, SizeOf(ws), 0);
  ws.ws_col := NewWidth;
  ws.ws_row := NewHeight;
  FpIOCtl(FFdMaster, TIOCSWINSZ, @ws);
end;

end.

