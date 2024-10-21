{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  This unit implements the shared parts, between the 'video' and the 'ptckvm'
  terminal display.

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

unit FpTerm.View.Video.Base;

{$mode objfpc}{$H+}

interface

uses
  FpTerm.Base, FpTerm.View;

type

  { TTerminalView_Video_Base }

  TTerminalView_Video_Base = class(TTerminalView)
  protected
    function GetCursorX: Integer; override;
    function GetCursorY: Integer; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetCell(Y, X: Integer; AValue: TCell); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure UpdateScreen; override;
    procedure IdleLoop; override;
    procedure HideCursor; override;
    procedure ShowCursor; override;
    procedure StartBlinkingCursor; override;
    procedure StopBlinkingCursor; override;
    procedure SetCursorPos(NewCursorX, NewCursorY: Integer); override;
    function Resize(NewWidth, NewHeight: Integer): Boolean; override;
    function StringDisplayWidth(const S: UnicodeString): Integer; override;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Math, System.Console.Video;
{$ELSE FPC_DOTTEDUNITS}
  SysUtils, Math, Video;
{$ENDIF FPC_DOTTEDUNITS}

procedure Attr2Video(Attr: TAttribute; ReverseVideo: Boolean; var Cell: TEnhancedVideoCell);
var
  EVA: TEnhancedVideoAttributes;
begin
  EVA := [];
  if ReverseVideo then
  begin
    Cell.ForegroundColor := 0;
    Cell.BackgroundColor := 7;
  end
  else
  begin
    Cell.ForegroundColor := 7;
    Cell.BackgroundColor := 0;
  end;
  if (Attr.ForegroundColor >= cBlack) and (Attr.ForegroundColor <= cColor255) then
    Cell.ForegroundColor := Ord(Attr.ForegroundColor) - Ord(cBlack);
  if (Attr.BackgroundColor >= cBlack) and (Attr.BackgroundColor <= cColor255) then
    Cell.BackgroundColor := (Ord(Attr.BackgroundColor) - Ord(cBlack));
  if rfBold in Attr.RenditionFlags then
    Include(EVA, evaBold);
  if rfFaint in Attr.RenditionFlags then
    Include(EVA, evaFaint);
  if rfItalicized in Attr.RenditionFlags then
    Include(EVA, evaItalicized);
  if rfUnderlined in Attr.RenditionFlags then
    Include(EVA, evaUnderlined);
  if rfBlinkSlow in Attr.RenditionFlags then
    Include(EVA, evaBlinkSlow);
  if rfBlinkFast in Attr.RenditionFlags then
    Include(EVA, evaBlinkFast);
  if rfInverse in Attr.RenditionFlags then
    Include(EVA, evaInverse);
  if rfInvisible in Attr.RenditionFlags then
    Include(EVA, evaInvisible);
  if rfCrossedOut in Attr.RenditionFlags then
    Include(EVA, evaCrossedOut);
  if rfDoublyUnderlined in Attr.RenditionFlags then
    Include(EVA, evaDoublyUnderlined);

  Cell.EnhancedVideoAttributes := EVA;
end;

{ TTerminalView_Video_Base }

function TTerminalView_Video_Base.GetCursorX: Integer;
begin
{$IFDEF FPC_DOTTEDUNITS}
  Result := EnsureRange(System.Console.Video.CursorX, 0, ScreenWidth - 1);
{$ELSE FPC_DOTTEDUNITS}
  Result := EnsureRange(Video.CursorX, 0, ScreenWidth - 1);
{$ENDIF FPC_DOTTEDUNITS}
end;

function TTerminalView_Video_Base.GetCursorY: Integer;
begin
{$IFDEF FPC_DOTTEDUNITS}
  Result := EnsureRange(System.Console.Video.CursorY, 0, ScreenHeight - 1);
{$ELSE FPC_DOTTEDUNITS}
  Result := EnsureRange(Video.CursorY, 0, ScreenHeight - 1);
{$ENDIF FPC_DOTTEDUNITS}
end;

function TTerminalView_Video_Base.GetHeight: Integer;
begin
  Result := ScreenHeight;
end;

function TTerminalView_Video_Base.GetWidth: Integer;
begin
  Result := ScreenWidth;
end;

procedure TTerminalView_Video_Base.SetCell(Y, X: Integer; AValue: TCell);
begin
  if X < 0 then
    raise EArgumentOutOfRangeException.Create('X < 0');
  if X >= ScreenWidth then
    raise EArgumentOutOfRangeException.Create('X >= ScreenWidth');
  if Y < 0 then
    raise EArgumentOutOfRangeException.Create('Y < 0');
  if Y >= ScreenHeight then
    raise EArgumentOutOfRangeException.Create('Y >= ScreenHeight');
  Attr2Video(AValue.Attribute, ReverseVideo, EnhancedVideoBuf[Y * ScreenWidth + X]);
  with EnhancedVideoBuf[Y * ScreenWidth + X] do
  begin
    if AValue.Erased then
      ExtendedGraphemeCluster := ' '
    else
      ExtendedGraphemeCluster := AValue.ExtendedGraphemeCluster;
  end;
end;

constructor TTerminalView_Video_Base.Create;
begin
  inherited Create;
  InitEnhancedVideo;
  ClearScreen;
end;

destructor TTerminalView_Video_Base.Destroy;
begin
  DoneEnhancedVideo;
  inherited Destroy;
end;

procedure TTerminalView_Video_Base.UpdateScreen;
begin
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Video.UpdateScreen(False);
{$ELSE FPC_DOTTEDUNITS}
  Video.UpdateScreen(False);
{$ENDIF FPC_DOTTEDUNITS}
end;

procedure TTerminalView_Video_Base.IdleLoop;
begin
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Video.UpdateScreen(False);
{$ELSE FPC_DOTTEDUNITS}
  Video.UpdateScreen(False);
{$ENDIF FPC_DOTTEDUNITS}
end;

procedure TTerminalView_Video_Base.HideCursor;
begin
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Video.SetCursorType(crHidden);
{$ELSE FPC_DOTTEDUNITS}
  Video.SetCursorType(crHidden);
{$ENDIF FPC_DOTTEDUNITS}
end;

procedure TTerminalView_Video_Base.ShowCursor;
begin
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Video.SetCursorType(crUnderLine);
{$ELSE FPC_DOTTEDUNITS}
  Video.SetCursorType(crUnderLine);
{$ENDIF FPC_DOTTEDUNITS}
end;

procedure TTerminalView_Video_Base.StartBlinkingCursor;
begin
end;

procedure TTerminalView_Video_Base.StopBlinkingCursor;
begin
end;

procedure TTerminalView_Video_Base.SetCursorPos(NewCursorX, NewCursorY: Integer);
begin
  if NewCursorX < 0 then
    raise EArgumentOutOfRangeException.Create('NewCursorX < 0');
  if NewCursorY < 0 then
    raise EArgumentOutOfRangeException.Create('NewCursorY < 0');
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Video.SetCursorPos(NewCursorX, NewCursorY);
{$ELSE FPC_DOTTEDUNITS}
  Video.SetCursorPos(NewCursorX, NewCursorY);
{$ENDIF FPC_DOTTEDUNITS}
end;

function TTerminalView_Video_Base.Resize(NewWidth, NewHeight: Integer): Boolean;
var
{$IFDEF FPC_DOTTEDUNITS}
  Mode: System.Console.Video.TVideoMode;
{$ELSE FPC_DOTTEDUNITS}
  Mode: Video.TVideoMode;
{$ENDIF FPC_DOTTEDUNITS}
begin
  if (NewWidth = ScreenWidth) and (NewHeight = ScreenHeight) then
    exit(True);
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Video.GetVideoMode(Mode);
{$ELSE FPC_DOTTEDUNITS}
  Video.GetVideoMode(Mode);
{$ENDIF FPC_DOTTEDUNITS}
  Mode.Col := NewWidth;
  Mode.Row := NewHeight;
{$IFDEF FPC_DOTTEDUNITS}
  Result := System.Console.Video.SetVideoMode(Mode);
{$ELSE FPC_DOTTEDUNITS}
  Result := Video.SetVideoMode(Mode);
{$ENDIF FPC_DOTTEDUNITS}
end;

function TTerminalView_Video_Base.StringDisplayWidth(const S: UnicodeString): Integer;
begin
{$IFDEF FPC_DOTTEDUNITS}
  result := System.Console.Video.StringDisplayWidth(S);
{$ELSE FPC_DOTTEDUNITS}
  result := Video.StringDisplayWidth(S);
{$ENDIF FPC_DOTTEDUNITS}
end;

end.

