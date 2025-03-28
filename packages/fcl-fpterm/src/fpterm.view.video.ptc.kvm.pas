{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  This unit implements the display of the terminal, using ptckvm.

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

unit FpTerm.View.Video.PTC.KVM;

{$mode objfpc}{$H+}

interface

uses
  FpTerm.View.Video.Base,
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Video;
{$ELSE FPC_DOTTEDUNITS}
  video;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TTerminalView_Video_ptckvm }

  TTerminalView_Video_ptckvm = class(TTerminalView_Video_Base)
  public
    constructor Create;

    procedure StartBlinkingCursor; override;
    procedure StopBlinkingCursor; override;
    function CheckPendingResize(out NewWidth, NewHeight: Integer): Boolean; override;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  PTC.KVM;
{$ELSE FPC_DOTTEDUNITS}
  ptckvm;
{$ENDIF FPC_DOTTEDUNITS}

{ TTerminalView_Video_ptckvm }

constructor TTerminalView_Video_ptckvm.Create;
begin
  InitEnhancedVideo;
  ClearScreen;
end;

procedure TTerminalView_Video_ptckvm.StartBlinkingCursor;
begin
{$IFDEF FPC_DOTTEDUNITS}
  ptc.kvm.StartBlinkingCursor;
{$ELSE FPC_DOTTEDUNITS}
  ptckvm.StartBlinkingCursor;
{$ENDIF FPC_DOTTEDUNITS}
end;

procedure TTerminalView_Video_ptckvm.StopBlinkingCursor;
begin
{$IFDEF FPC_DOTTEDUNITS}
  ptc.kvm.StopBlinkingCursor;
{$ELSE FPC_DOTTEDUNITS}
  ptckvm.StopBlinkingCursor;
{$ENDIF FPC_DOTTEDUNITS}
end;

function TTerminalView_Video_ptckvm.CheckPendingResize(out NewWidth, NewHeight: Integer): Boolean;
var
  NewMode: TVideoMode;
begin
{$IFDEF FPC_DOTTEDUNITS}
  if ptc.kvm.CheckPendingResize(NewMode) then
{$ELSE FPC_DOTTEDUNITS}
  if ptckvm.CheckPendingResize(NewMode) then
{$ENDIF FPC_DOTTEDUNITS}
  begin
    NewWidth := NewMode.Col;
    NewHeight := NewMode.Row;
    Result := True;
  end
  else
  begin
    NewWidth := -1;
    NewHeight := -1;
    Result := False;
  end;
end;

end.

