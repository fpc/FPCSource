{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  This unit implements a pointing device for the terminal, using unit 'Mouse'.

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

unit System.Terminal.PointingDeviceInput.Mouse;

{$mode objfpc}{$H+}

interface

uses
  System.Terminal.Base, System.Terminal.PointingDeviceInput;

type

  { TTerminalPointingDeviceInput_Mouse }

  TTerminalPointingDeviceInput_Mouse = class(TTerminalPointingDeviceInput)
  protected
    function IsEventAvailable: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GetEvent(out Event: TPointingDeviceEvent); override;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Mouse;
{$ELSE FPC_DOTTEDUNITS}
  Mouse;
{$ENDIF FPC_DOTTEDUNITS}

procedure MouseEvent2TerminalPointingDeviceEvent(const me: TMouseEvent; var tpde: TPointingDeviceEvent);
var
  b: TPointingDeviceButton;
  bs: TPointingDeviceButtonState;
begin
  tpde.X := me.x;
  tpde.Y := me.y;
  bs := [];
  for b := Low(TPointingDeviceButton) to High(TPointingDeviceButton) do
    if (me.buttons and (Word(1) shl Ord(b))) <> 0 then
      Include(bs, b);
  tpde.ButtonState := bs;
end;

{ TTerminalPointingDeviceInput_Mouse }

function TTerminalPointingDeviceInput_Mouse.IsEventAvailable: Boolean;
var
  me: TMouseEvent;
begin
  Result := PollMouseEvent(me);
end;

constructor TTerminalPointingDeviceInput_Mouse.Create;
begin
  inherited Create;
  InitMouse;
end;

destructor TTerminalPointingDeviceInput_Mouse.Destroy;
begin
  DoneMouse;
  inherited Destroy;
end;

procedure TTerminalPointingDeviceInput_Mouse.GetEvent(out Event: TPointingDeviceEvent);
var
  me: TMouseEvent;
begin
  FillChar(Event, SizeOf(Event), 0);
  GetMouseEvent(me);
  MouseEvent2TerminalPointingDeviceEvent(me, Event);
end;

end.

