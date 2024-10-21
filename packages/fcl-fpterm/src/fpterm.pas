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

unit FpTerm;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
{$ELSE FPC_DOTTEDUNITS}
  SysUtils,
{$ENDIF FPC_DOTTEDUNITS}
  FpTerm.Base,
  FpTerm.View,
  FpTerm.Model,
  FpTerm.Controller,
  FpTerm.InputOutputConnection,
  FpTerm.PointingDeviceInput,
  FpTerm.KeyboardInput;

type

  { TTerminal }

  TTerminal = class
  private
    FView: TTerminalView;
    FModel: TTerminalModel;
    FController: TTerminalController;
    FInputOutputConnection: ITerminalInputOutputConnection;
    FKeyboardInput: TTerminalKeyboardInput;
    FPointingDeviceInput: TTerminalPointingDeviceInput;

    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure TransmitData(const buf; Bytes: SizeUInt);
    procedure ControllerOnResize(NewWidth, NewHeight: Integer);

    procedure HandlePointingDevice;
    procedure HandleKeyboard;
    procedure HandleInputOutput(out TimeToQuit: Boolean);
  public
    constructor Create(AView: TTerminalView; AKeyboardInput: TTerminalKeyboardInput;
                       APointingDeviceInput: TTerminalPointingDeviceInput);
    destructor Destroy; override;

    procedure Update(out TimeToQuit: Boolean);
    procedure Run;

    property InputOutputConnection: ITerminalInputOutputConnection read FInputOutputConnection write FInputOutputConnection;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

implementation

{ TTerminal }

procedure TTerminal.TransmitData(const buf; Bytes: SizeUInt);
begin
  if Assigned(FInputOutputConnection) and (Bytes > 0) then
    FInputOutputConnection.Write(buf, Bytes);
end;

function TTerminal.GetHeight: Integer;
begin
  Result := FModel.Height;
end;

function TTerminal.GetWidth: Integer;
begin
  Result := FModel.Width;
end;

procedure TTerminal.ControllerOnResize(NewWidth, NewHeight: Integer);
begin
  if Assigned(FInputOutputConnection) then
    FInputOutputConnection.Resize(NewWidth, NewHeight);
end;

procedure TTerminal.HandlePointingDevice;
var
  pde: TPointingDeviceEvent;
begin
  if FPointingDeviceInput.EventAvailable then
  begin
    FPointingDeviceInput.GetEvent(pde);
    FController.HandleMouseEvent(pde);
  end;
end;

procedure TTerminal.HandleKeyboard;
var
  k: TKeyEvent;
  ks: rawbytestring;
  UTF16_HighSurrogate: WideChar = #0;
begin
  if FKeyboardInput.EventAvailable then
  begin
    FKeyboardInput.GetEvent(k);
    if not (tmfKeyboardActionMode in FController.ModeFlags) then
    begin
      ks := '';
      if k.UnicodeChar <> WideChar(0) then
      begin
        if (k.UnicodeChar >= WideChar($D800)) and (k.UnicodeChar <= WideChar($DFFF)) then
        begin
          if (k.UnicodeChar >= WideChar($D800)) and (k.UnicodeChar <= WideChar($DBFF)) then
            { k.UnicodeChar is a high surrogate, save it }
            UTF16_HighSurrogate := k.UnicodeChar
          else
          begin
            { k.UnicodeChar is a low surrogate }
            { if the previous character was a valid high surrogate, combine them to obtain a non-BMP code point }
            if (UTF16_HighSurrogate >= WideChar($D800)) and (UTF16_HighSurrogate <= WideChar($DBFF)) then
              ks := UTF8Encode(UTF16_HighSurrogate + k.UnicodeChar);
            UTF16_HighSurrogate := #0;
          end;
        end
        else
        begin
          ks := UTF8Encode(k.UnicodeChar);
          if ks = #13 then
          begin
            if tmfAutoNewLine in FController.ModeFlags then
              ks := #13#10;
          end;
        end;
      end
      else
      begin
        UTF16_HighSurrogate := #0;
        case k.VirtualScanCode of
          $3B00:  { F1 }
            ks := FController.EncodeReturnC1(C1_SS3) + 'P';
          $3C00:  { F2 }
            ks := FController.EncodeReturnC1(C1_SS3) + 'Q';
          $3D00:  { F3 }
            ks := FController.EncodeReturnC1(C1_SS3) + 'R';
          $3E00:  { F4 }
            ks := FController.EncodeReturnC1(C1_SS3) + 'S';
          $3F00:  { F5 }
            ks := FController.EncodeReturnC1(C1_CSI) + '15~';
          $4000:  { F6 }
            ks := FController.EncodeReturnC1(C1_CSI) + '17~';
          $4100:  { F7 }
            ks := FController.EncodeReturnC1(C1_CSI) + '18~';
          $4200:  { F8 }
            ks := FController.EncodeReturnC1(C1_CSI) + '19~';
          $4300:  { F9 }
            ks := FController.EncodeReturnC1(C1_CSI) + '20~';
          $4400:  { F10 }
            ks := FController.EncodeReturnC1(C1_CSI) + '21~';
          $8500:  { F11 }
            ks := FController.EncodeReturnC1(C1_CSI) + '23~';
          $8600:  { F12 }
            ks := FController.EncodeReturnC1(C1_CSI) + '24~';
          $5400:  { Shift-F1 }
            ks := FController.EncodeReturnC1(C1_CSI) + '1;2P';
          $5500:  { Shift-F2 }
            ks := FController.EncodeReturnC1(C1_CSI) + '1;2Q';
          $5600:  { Shift-F3 }
            ks := FController.EncodeReturnC1(C1_CSI) + '1;2R';
          $5700:  { Shift-F4 }
            ks := FController.EncodeReturnC1(C1_CSI) + '1;2S';
          $5800:  { Shift-F5 }
            ks := FController.EncodeReturnC1(C1_CSI) + '15;2~';
          $5900:  { Shift-F6 }
            ks := FController.EncodeReturnC1(C1_CSI) + '17;2~';
          $5A00:  { Shift-F7 }
            ks := FController.EncodeReturnC1(C1_CSI) + '18;2~';
          $5B00:  { Shift-F8 }
            ks := FController.EncodeReturnC1(C1_CSI) + '19;2~';
          $5C00:  { Shift-F9 }
            ks := FController.EncodeReturnC1(C1_CSI) + '20;2~';
          $5D00:  { Shift-F10 }
            ks := FController.EncodeReturnC1(C1_CSI) + '21;2~';
          $8700:  { Shift-F11 }
            ks := FController.EncodeReturnC1(C1_CSI) + '23;2~';
          $8800:  { Shift-F12 }
            ks := FController.EncodeReturnC1(C1_CSI) + '24;2~';
          $5E00:  { Ctrl-F1 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6P'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5P';
          $5F00:  { Ctrl-F2 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6Q'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5Q';
          $6000:  { Ctrl-F3 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6R'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5R';
          $6100:  { Ctrl-F4 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6S'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5S';
          $6200:  { Ctrl-F5 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '15;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '15;5~';
          $6300:  { Ctrl-F6 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '17;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '17;5~';
          $6400:  { Ctrl-F7 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '18;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '18;5~';
          $6500:  { Ctrl-F8 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '19;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '19;5~';
          $6600:  { Ctrl-F9 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '20;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '20;5~';
          $6700:  { Ctrl-F10 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '21;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '21;5~';
          $8900:  { Ctrl-F11 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '23;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '23;5~';
          $8A00:  { Ctrl-F12 }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '24;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '24;5~';
          $6800:  { Alt-F1 }
            ks := FController.EncodeReturnC1(C1_CSI) + '1;3P';
          $6900:  { Alt-F2 }
            ks := FController.EncodeReturnC1(C1_CSI) + '1;3Q';
          $6A00:  { Alt-F3 }
            ks := FController.EncodeReturnC1(C1_CSI) + '1;3R';
          $6B00:  { Alt-F4 }
            ks := FController.EncodeReturnC1(C1_CSI) + '1;3S';
          $6C00:  { Alt-F5 }
            ks := FController.EncodeReturnC1(C1_CSI) + '15;3~';
          $6D00:  { Alt-F6 }
            ks := FController.EncodeReturnC1(C1_CSI) + '17;3~';
          $6E00:  { Alt-F7 }
            ks := FController.EncodeReturnC1(C1_CSI) + '18;3~';
          $6F00:  { Alt-F8 }
            ks := FController.EncodeReturnC1(C1_CSI) + '19;3~';
          $7000:  { Alt-F9 }
            ks := FController.EncodeReturnC1(C1_CSI) + '20;3~';
          $7100:  { Alt-F10 }
            ks := FController.EncodeReturnC1(C1_CSI) + '21;3~';
          $8B00:  { Alt-F11 }
            ks := FController.EncodeReturnC1(C1_CSI) + '23;3~';
          $8C00:  { Alt-F12 }
            ks := FController.EncodeReturnC1(C1_CSI) + '24;3~';
          $4800:  { Up Arrow }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;2A'
            else if tmfCursorKeysSendApplicationSequences in FController.ModeFlags then
              ks := FController.EncodeReturnC1(C1_SS3) + 'A'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + 'A';
          $5000:  { Down Arrow }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;2B'
            else if tmfCursorKeysSendApplicationSequences in FController.ModeFlags then
              ks := FController.EncodeReturnC1(C1_SS3) + 'B'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + 'B';
          $4D00:  { Right Arrow }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;2C'
            else if tmfCursorKeysSendApplicationSequences in FController.ModeFlags then
              ks := FController.EncodeReturnC1(C1_SS3) + 'C'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + 'C';
          $4B00:  { Left Arrow }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;2D'
            else if tmfCursorKeysSendApplicationSequences in FController.ModeFlags then
              ks := FController.EncodeReturnC1(C1_SS3) + 'D'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + 'D';
          $8D00:  { Ctrl-Up Arrow }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6A'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5A';
          $9100:  { Ctrl-Down Arrow }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6B'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5B';
          $7400:  { Ctrl-Right Arrow }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6C'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5C';
          $7300:  { Ctrl-Left Arrow }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6D'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5D';
          $4700:  { Home }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;2H'
            else if tmfCursorKeysSendApplicationSequences in FController.ModeFlags then
              ks := FController.EncodeReturnC1(C1_SS3) + 'H'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + 'H';
          $4F00:  { End }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;2F'
            else if tmfCursorKeysSendApplicationSequences in FController.ModeFlags then
              ks := FController.EncodeReturnC1(C1_SS3) + 'F'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + 'F';
          $5200:  { Insert }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '2;2~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '2~';
          $5300:  { Delete }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '3;2~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '3~';
          $4900:  { Page Up }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '5;2~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '5~';
          $5100:  { Page Down }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '6;2~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '6~';
          $7700:  { Ctrl-Home }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6H'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5H';
          $7500:  { Ctrl-End }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '1;6F'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '1;5F';
          $9200:  { Ctrl-Insert }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '2;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '2;5~';
          $9300:  { Ctrl-Delete }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '3;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '3;5~';
          $8400:  { Ctrl-Page Up }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '5;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '5;5~';
          $7600:  { Ctrl-Page Down }
            if ssShift in k.ShiftState then
              ks := FController.EncodeReturnC1(C1_CSI) + '6;6~'
            else
              ks := FController.EncodeReturnC1(C1_CSI) + '6;5~';
          $0F00:  { Shift-Tab }
            ks := FController.EncodeReturnC1(C1_CSI) + 'Z';
          $2900:  { Alt-` }
            ks := #27'`';
          $7800:  { Alt-1 }
            ks := #27'1';
          $7900:  { Alt-2 }
            ks := #27'2';
          $7A00:  { Alt-3 }
            ks := #27'3';
          $7B00:  { Alt-4 }
            ks := #27'4';
          $7C00:  { Alt-5 }
            ks := #27'5';
          $7D00:  { Alt-6 }
            ks := #27'6';
          $7E00:  { Alt-7 }
            ks := #27'7';
          $7F00:  { Alt-8 }
            ks := #27'8';
          $8000:  { Alt-9 }
            ks := #27'9';
          $8100:  { Alt-0 }
            ks := #27'0';
          $8200:  { Alt-- }
            ks := #27'-';
          $8300:  { Alt-= }
            ks := #27'=';
          $1000:  { Alt-Q }
            ks := #27'q';
          $1100:  { Alt-W }
            ks := #27'w';
          $1200:  { Alt-E }
            ks := #27'e';
          $1300:  { Alt-R }
            ks := #27'r';
          $1400:  { Alt-T }
            ks := #27't';
          $1500:  { Alt-Y }
            ks := #27'y';
          $1600:  { Alt-U }
            ks := #27'u';
          $1700:  { Alt-I }
            ks := #27'i';
          $1800:  { Alt-O }
            ks := #27'o';
          $1900:  { Alt-P }
            ks := #27'p';
          $1A00:  { Alt-[ }
            ks := #27'[';
          $1B00:  { Alt-] }
            ks := #27']';
          $2B00:  { Alt-\ }
            ks := #27'\';
          $1E00:  { Alt-A }
            ks := #27'a';
          $1F00:  { Alt-S }
            ks := #27's';
          $2000:  { Alt-D }
            ks := #27'd';
          $2100:  { Alt-F }
            ks := #27'f';
          $2200:  { Alt-G }
            ks := #27'g';
          $2300:  { Alt-H }
            ks := #27'h';
          $2400:  { Alt-J }
            ks := #27'j';
          $2500:  { Alt-K }
            ks := #27'k';
          $2600:  { Alt-L }
            ks := #27'l';
          $2700:  { Alt-; }
            ks := #27';';
          $2800:  { Alt-' }
            ks := #27'''';
          $2C00:  { Alt-Z }
            ks := #27'z';
          $2D00:  { Alt-X }
            ks := #27'x';
          $2E00:  { Alt-C }
            ks := #27'c';
          $2F00:  { Alt-V }
            ks := #27'v';
          $3000:  { Alt-B }
            ks := #27'b';
          $3100:  { Alt-N }
            ks := #27'n';
          $3200:  { Alt-M }
            ks := #27'm';
          $3300:  { Alt-, }
            ks := #27',';
          $3400:  { Alt-. }
            ks := #27'.';
          $3500:  { Alt-/ }
            ks := #27'/';
        end;
      end;
      if ks <> '' then
      begin
        FController.MaybeLocalEcho(ks);
        TransmitData(ks[1], Length(ks));
      end;
    end;
  end;
end;

procedure TTerminal.HandleInputOutput(out TimeToQuit: Boolean);
var
  buf: array [0..1023] of Char;
  bytes_read: SizeInt;
begin
  if Assigned(FInputOutputConnection) then
  begin
    TimeToQuit := FInputOutputConnection.Closed;
    if FInputOutputConnection.DataAvailable then
    begin
      bytes_read := FInputOutputConnection.Read(buf, SizeOf(buf));
      if bytes_read > 0 then
        FController.ReceiveData(buf, bytes_read);
    end;
  end
  else
    TimeToQuit := False;
end;

constructor TTerminal.Create(AView: TTerminalView;
  AKeyboardInput: TTerminalKeyboardInput;
  APointingDeviceInput: TTerminalPointingDeviceInput);
begin
  FView := AView;
  FKeyboardInput := AKeyboardInput;
  FPointingDeviceInput := APointingDeviceInput;
  FModel := TTerminalModel.Create(FView);
  FController := TTerminalController.Create(FModel, ttVT420);
  FController.OnTransmitData := @TransmitData;
  FController.OnResize := @ControllerOnResize;
end;

destructor TTerminal.Destroy;
begin
  FreeAndNil(FController);
  FreeAndNil(FModel);
  inherited Destroy;
end;

procedure TTerminal.Update(out TimeToQuit: Boolean);
var
  NewWidth, NewHeight: Integer;
begin
  FView.IdleLoop;
  if FView.CheckPendingResize(NewWidth, NewHeight) then
    FController.Resize(NewWidth, NewHeight);
  HandlePointingDevice;
  HandleKeyboard;
  HandleInputOutput(TimeToQuit);
end;

procedure TTerminal.Run;
var
  Done: Boolean;
begin
  Done := False;
  repeat
    Update(Done);
  until Done;
end;

end.

