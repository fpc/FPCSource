{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  This unit implements a keyboard for the terminal, using unit 'keyboard'.

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

unit FpTerm.KeyboardInput.Keyboard;

{$mode objfpc}{$H+}

interface

uses
  FpTerm.Base, FpTerm.KeyboardInput;

type

  { TTerminalKeyboardInput_Keyboard }

  TTerminalKeyboardInput_Keyboard = class(TTerminalKeyboardInput)
  protected
    function IsEventAvailable: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetEvent(out Event: FpTerm.Base.TKeyEvent); override;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Keyboard;
{$ELSE FPC_DOTTEDUNITS}
  Keyboard;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF FPC_DOTTEDUNITS}
function ConvertShiftState(ess: System.Console.Keyboard.TEnhancedShiftState): FpTerm.Base.TShiftState;
{$ELSE FPC_DOTTEDUNITS}
function ConvertShiftState(ess: Keyboard.TEnhancedShiftState): FpTerm.Base.TShiftState;
{$ENDIF FPC_DOTTEDUNITS}
begin
  Result := [];
  if essShift in ess then
    Include(Result, ssShift);
  if essLeftShift in ess then
    Include(Result, ssLeftShift);
  if essRightShift in ess then
    Include(Result, ssRightShift);
  if essCtrl in ess then
    Include(Result, ssCtrl);
  if essLeftCtrl in ess then
    Include(Result, ssLeftCtrl);
  if essRightCtrl in ess then
    Include(Result, ssRightCtrl);
  if essAlt in ess then
    Include(Result, ssAlt);
  if essLeftAlt in ess then
    Include(Result, ssLeftAlt);
  if essRightAlt in ess then
    Include(Result, ssRightAlt);
  if essAltGr in ess then
    Include(Result, ssAltGr);
  if essCapsLockPressed in ess then
    Include(Result, ssCapsLockPressed);
  if essCapsLockOn in ess then
    Include(Result, ssCapsLockOn);
  if essNumLockPressed in ess then
    Include(Result, ssNumLockPressed);
  if essNumLockOn in ess then
    Include(Result, ssNumLockOn);
  if essScrollLockPressed in ess then
    Include(Result, ssScrollLockPressed);
  if essScrollLockOn in ess then
    Include(Result, ssScrollLockOn);
end;

{$IFDEF FPC_DOTTEDUNITS}
procedure EnhancedKeyEvent2TerminalKeyEvent(const k: System.Console.Keyboard.TEnhancedKeyEvent; var tk: FpTerm.Base.TKeyEvent);
{$ELSE FPC_DOTTEDUNITS}
procedure EnhancedKeyEvent2TerminalKeyEvent(const k: Keyboard.TEnhancedKeyEvent; var tk: FpTerm.Base.TKeyEvent);
{$ENDIF FPC_DOTTEDUNITS}
begin
  tk.VirtualKeyCode := k.VirtualKeyCode;
  tk.VirtualScanCode := k.VirtualScanCode;
  tk.UnicodeChar := k.UnicodeChar;
  tk.AsciiChar := k.AsciiChar;
  tk.ShiftState := ConvertShiftState(k.ShiftState);
  tk.Flags := k.Flags;
end;

{ TTerminalKeyboardInput_Keyboard }

function TTerminalKeyboardInput_Keyboard.IsEventAvailable: Boolean;
begin
  Result := PollEnhancedKeyEvent <> NilEnhancedKeyEvent;
end;

constructor TTerminalKeyboardInput_Keyboard.Create;
begin
  inherited Create;
  InitKeyboard;
end;

destructor TTerminalKeyboardInput_Keyboard.Destroy;
begin
  DoneKeyboard;
  inherited Destroy;
end;

procedure TTerminalKeyboardInput_Keyboard.GetEvent(out Event: FpTerm.Base.TKeyEvent);
var
{$IFDEF FPC_DOTTEDUNITS}
  k: System.Console.Keyboard.TEnhancedKeyEvent;
{$ELSE FPC_DOTTEDUNITS}
  k: Keyboard.TEnhancedKeyEvent;
{$ENDIF FPC_DOTTEDUNITS}
begin
  FillChar(Event, SizeOf(Event), 0);
  k := GetEnhancedKeyEvent;
  EnhancedKeyEvent2TerminalKeyEvent(k, Event);
end;

end.

