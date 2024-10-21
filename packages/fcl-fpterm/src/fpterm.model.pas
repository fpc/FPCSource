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

unit FpTerm.Model;

{$mode objfpc}{$H+}

interface

uses
  FpTerm.Base, FpTerm.View;

type

  { TTerminalModel }

  TTerminalModel = class
  private
    FView: TTerminalView;
    FCells: array [TScreenBuffer] of array of array of TCell;
    FCurrentVisibleScreenBuffer: TScreenBuffer;
    FCursorVisible: Boolean;
    FReverseVideo: Boolean;

    function GetCell(Y, X: Integer): TCell;
    function GetCursorX: Integer;
    function GetCursorY: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetCell(Y, X: Integer; AValue: TCell);
    procedure SetCurrentVisibleScreenBuffer(AValue: TScreenBuffer);
    procedure SetReverseVideo(AValue: Boolean);
  public
    constructor Create(AView: TTerminalView);

    procedure Reset;
    procedure UpdateScreen;
    procedure HideCursor;
    procedure ShowCursor;
    procedure StartBlinkingCursor;
    procedure StopBlinkingCursor;
    procedure SetCursorPos(NewCursorX, NewCursorY: Integer);
    function Resize(NewWidth, NewHeight: Integer): Boolean;
    function StringDisplayWidth(const S: UnicodeString): Integer;
    property CursorX: Integer read GetCursorX;
    property CursorY: Integer read GetCursorY;
    property CursorVisible: Boolean read FCursorVisible;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Cell [Y, X: Integer]: TCell read GetCell write SetCell;
    property CurrentVisibleScreenBuffer: TScreenBuffer read FCurrentVisibleScreenBuffer write SetCurrentVisibleScreenBuffer;
    property ReverseVideo: Boolean read FReverseVideo write SetReverseVideo;
  end;

implementation

{ TTerminalModel }

function TTerminalModel.GetCell(Y, X: Integer): TCell;
begin
  Result := FCells[CurrentVisibleScreenBuffer, Y, X];
end;

function TTerminalModel.GetCursorX: Integer;
begin
  Result := FView.CursorX;
end;

function TTerminalModel.GetCursorY: Integer;
begin
  Result := FView.CursorY;
end;

function TTerminalModel.GetHeight: Integer;
begin
  Result := FView.Height;
end;

function TTerminalModel.GetWidth: Integer;
begin
  Result := FView.Width;
end;

procedure TTerminalModel.SetCell(Y, X: Integer; AValue: TCell);
begin
  FCells[CurrentVisibleScreenBuffer, Y, X] := AValue;
  FView.Cell[Y, X] := AValue;
end;

procedure TTerminalModel.SetCurrentVisibleScreenBuffer(AValue: TScreenBuffer);
var
  Y, X: Integer;
begin
  if FCurrentVisibleScreenBuffer = AValue then
    exit;
  FCurrentVisibleScreenBuffer := AValue;
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      FView.Cell[Y, X] := FCells[FCurrentVisibleScreenBuffer, Y, X];
end;

procedure TTerminalModel.SetReverseVideo(AValue: Boolean);
var
  Y, X: Integer;
begin
  if FReverseVideo = AValue then
    exit;
  FReverseVideo := AValue;
  FView.ReverseVideo := AValue;
  { force screen refresh }
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      FView.Cell[Y, X] := FCells[FCurrentVisibleScreenBuffer, Y, X];
end;

constructor TTerminalModel.Create(AView: TTerminalView);
begin
  FView := AView;
  Reset;
end;

procedure TTerminalModel.Reset;
var
  sb: TScreenBuffer;
  Y, X: Integer;
begin
  FCurrentVisibleScreenBuffer := sbNormal;
  for sb in TScreenBuffer do
  begin
    SetLength(FCells[sb], Height);
    for Y := 0 to Height - 1 do
    begin
      SetLength(FCells[sb, Y], Width);
      for X := 0 to Width - 1 do
        with FCells[sb, Y, X] do
        begin
          ExtendedGraphemeCluster := '';
          Attribute.ForegroundColor := cDefaultForeground;
          Attribute.BackgroundColor := cDefaultBackground;
          Attribute.RenditionFlags := [];
        end;
    end;
  end;

  ShowCursor;
  SetCursorPos(0, 0);

  { force screen refresh }
  FCurrentVisibleScreenBuffer := sbAlternate;
  CurrentVisibleScreenBuffer := sbNormal;
end;

procedure TTerminalModel.UpdateScreen;
begin
  FView.UpdateScreen;
end;

procedure TTerminalModel.HideCursor;
begin
  FView.HideCursor;
  FCursorVisible := False;
end;

procedure TTerminalModel.ShowCursor;
begin
  FView.ShowCursor;
  FCursorVisible := True;
end;

procedure TTerminalModel.StartBlinkingCursor;
begin
  FView.StartBlinkingCursor;
end;

procedure TTerminalModel.StopBlinkingCursor;
begin
  FView.StopBlinkingCursor;
end;

procedure TTerminalModel.SetCursorPos(NewCursorX, NewCursorY: Integer);
begin
  FView.SetCursorPos(NewCursorX, NewCursorY);
end;

function TTerminalModel.Resize(NewWidth, NewHeight: Integer): Boolean;
var
  sb: TScreenBuffer;
  Y: Integer;
begin
  Result := FView.Resize(NewWidth, NewHeight);
  for sb in TScreenBuffer do
  begin
    SetLength(FCells[sb], Height);
    for Y := 0 to Height - 1 do
      SetLength(FCells[sb, Y], Width);
  end;
end;

function TTerminalModel.StringDisplayWidth(const S: UnicodeString): Integer;
begin
  Result := FView.StringDisplayWidth(S);
end;

end.

