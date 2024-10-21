{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  This unit implements the display of the terminal, using the unit 'video'.

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

unit FpTerm.View.Video;

{$mode objfpc}{$H+}

{$if defined(UNIX)}
  {$DEFINE HAS_TERMIO}
{$endif}

interface

uses
  FpTerm.View.Video.Base
{$ifdef HAS_TERMIO}
  {$IFDEF FPC_DOTTEDUNITS}
    , UnixApi.TermIO
  {$ELSE FPC_DOTTEDUNITS}
    , termio
  {$ENDIF FPC_DOTTEDUNITS}
{$endif HAS_TERMIO};

type

  { TTerminalView_Video }

  TTerminalView_Video = class(TTerminalView_Video_Base)
  private
{$ifdef HAS_TERMIO}
    FLastWinSize: TWinSize;
{$endif HAS_TERMIO}
  public
{$ifdef HAS_TERMIO}
    constructor Create;
{$endif HAS_TERMIO}
    function CheckPendingResize(out NewWidth, NewHeight: Integer): Boolean; override;
  end;

implementation

{$ifdef HAS_TERMIO}
uses
  {$IFDEF FPC_DOTTEDUNITS}
    UnixApi.Base;
  {$ELSE FPC_DOTTEDUNITS}
    baseunix;
  {$ENDIF FPC_DOTTEDUNITS}
{$endif HAS_TERMIO}

{ TTerminalView_Video }

{$ifdef HAS_TERMIO}
constructor TTerminalView_Video.Create;
begin
  inherited Create;
  fpioctl(stdinputhandle,TIOCGWINSZ,@FLastWinSize);
end;
{$endif HAS_TERMIO}

{$ifdef HAS_TERMIO}
function TTerminalView_Video.CheckPendingResize(out NewWidth, NewHeight: Integer): Boolean;
var
  ws: TWinSize;
begin
  fpioctl(stdinputhandle,TIOCGWINSZ,@ws);
  if (ws.ws_col <> FLastWinSize.ws_col) or (ws.ws_row <> FLastWinSize.ws_row) then
  begin
    FLastWinSize := ws;
    NewWidth := ws.ws_col;
    NewHeight := ws.ws_row;
    Result := True;
  end
  else
  begin
    NewWidth := -1;
    NewHeight := -1;
    Result := False;
  end;
end;
{$else HAS_TERMIO}
function TTerminalView_Video.CheckPendingResize(out NewWidth, NewHeight: Integer): Boolean;
begin
  NewWidth := -1;
  NewHeight := -1;
  Result := False;
end;
{$endif HAS_TERMIO}

end.

