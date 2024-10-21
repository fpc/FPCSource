{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  This unit defines the display of the terminal.

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

unit FpTerm.View;

{$mode objfpc}{$H+}

interface

uses
  FpTerm.Base;

type

  { TTerminalView }

  TTerminalView = class
  protected
    FReverseVideo: Boolean;

    function GetCursorX: Integer; virtual; abstract;
    function GetCursorY: Integer; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    procedure SetCell(Y, X: Integer; AValue: TCell); virtual; abstract;
  public
    procedure UpdateScreen; virtual; abstract;
    procedure IdleLoop; virtual; abstract;
    procedure HideCursor; virtual; abstract;
    procedure ShowCursor; virtual; abstract;
    procedure StartBlinkingCursor; virtual; abstract;
    procedure StopBlinkingCursor; virtual; abstract;
    procedure SetCursorPos(NewCursorX, NewCursorY: Integer); virtual; abstract;

    { Requests the view to resize itself. Can be triggered in response to a
      CheckPendingResize call returning true, but also by other events, such as
      by certain ESC sequences (i.e. resize requested by the app). }
    function Resize(NewWidth, NewHeight: Integer): Boolean; virtual; abstract;

    { Checks for a resize event, initiated by the view (for example, a window
      resize, initiated by the user, by resizing the console window with the
      mouse). }
    function CheckPendingResize(out NewWidth, NewHeight: Integer): Boolean; virtual; abstract;

    { Returns the number of character cells needed for the display of the given string }
    function StringDisplayWidth(const S: UnicodeString): Integer; virtual; abstract;

    property CursorX: Integer read GetCursorX;
    property CursorY: Integer read GetCursorY;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Cell [Y, X: Integer]: TCell {read GetCell} write SetCell;
    property ReverseVideo: Boolean read FReverseVideo write FReverseVideo;
  end;

implementation

end.

