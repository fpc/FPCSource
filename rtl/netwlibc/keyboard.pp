{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team.

    Keyboard unit for netware libc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Keyboard;

interface

{$i keybrdh.inc}

implementation

uses Libc;

{$i keyboard.inc}

procedure SysInitKeyboard;
begin
  PendingKeyEvent := 0;
end;


function SysGetKeyEvent: TKeyEvent;
var Ktype,Kvalue,Kstatus,Kscancode : byte;
begin
  if PendingKeyEvent<>0 then
  begin
    SysGetKeyEvent:=PendingKeyEvent;
    PendingKeyEvent:=0;
    exit;
  end;
  Libc.GetKey(Libc.GetScreenHandle,Ktype,Kvalue,Kstatus,Kscancode,0{ ??? linesToProtect:size_t});
  with TKeyRecord (SysGetKeyEvent) do
  begin
    Case Ktype of
      ENTER_KEY         : begin
                            KeyCode := $1c0d; Flags := 3;
                          end;
      ESCAPE_KEY        : begin
                            KeyCode := $011b; Flags := 3;
                          end;
      BACKSPACE_KEY     : begin
                            KeyCode := $0e08; Flags := 3;
                          end;
      NORMAL_KEY        : begin
                            if KStatus AND ALT_KEY_HELD > 0 then KValue := 0;
                            IF (KValue = 9) and ((KStatus and SHIFT_KEY_HELD) > 0) then KValue := 0;
                            KeyCode := (Kscancode shl 8) + KValue;
                            Flags := 3;
                          end;
      FUNCTION_KEY,
      DELETE_KEY,
      INSERT_KEY,
      CURSOR_DOWN_KEY,
      CURSOR_UP_KEY,
      CURSOR_RIGHT_KEY,
      CURSOR_LEFT_KEY,
      CURSOR_HOME_KEY,
      CURSOR_END_KEY,
      CURSOR_PUP_KEY,
      CURSOR_PDOWN_KEY  : begin
                            KeyCode := KScancode shl 8;
                            Flags := 3;
                          end;
    end;
    ShiftState := 0;
    if KStatus AND SHIFT_KEY_HELD     > 0 then ShiftState := ShiftState or kbShift;
    if KStatus AND CTRL_KEY_HELD      > 0 then ShiftState := ShiftState or kbCtrl;
    if KStatus AND ALT_KEY_HELD       > 0 then ShiftState := ShiftState or kbAlt;
  end;
end;


function SysPollKeyEvent: TKeyEvent;
begin
  if PendingKeyEvent<>0 then
    exit(PendingKeyEvent);
  if Libc.CheckKeyStatus (Libc.GetScreenHandle) <> 0 then
  begin
    PendingKeyEvent := SysGetKeyEvent;
    SysPollKeyEvent := PendingKeyEvent;
  end else
  begin
    SysPollKeyEvent := 0;
    //NXThreadYield;
    Delay(50);
  end;
end;


function SysPollShiftStateEvent: TKeyEvent;
begin
  SysPollShiftStateEvent:=0;
end;

function SysGetShiftState: Byte;
begin
  SysGetShiftState:=0;
end;

function SysTranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;
begin
  {if KeyEvent and $03000000 = $03000000 then
    KeyEvent := KeyEvent - $03000000;}
  SysTranslateKeyEvent := KeyEvent;
end;


Const
  SysKeyboardDriver : TKeyboardDriver = (
      InitDriver : Nil;
      DoneDriver : Nil;
      GetKeyevent : @SysGetKeyEvent;
      PollKeyEvent : @SysPollKeyEvent;
      GetShiftState : @SysGetShiftState;
      TranslateKeyEvent : nil;  //@SysTranslateKeyEvent;
      TranslateKeyEventUnicode : Nil;
    );

begin
  KeyboardInitialized := false;
  PendingKeyEvent := 0;
  SetKeyBoardDriver(SysKeyBoardDriver);
end.
