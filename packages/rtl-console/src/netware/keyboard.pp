{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team.

    Keyboard unit for netware

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ 2001/04/16 armin: first version for netware
  2002/03/03 armin: changes for fpc 1.1 }
unit Keyboard;
interface

{$i keybrdh.inc}


implementation

{$i keyboard.inc}
{$i nwsys.inc}

procedure SysInitKeyboard;
begin
  PendingKeyEvent := 0;
end;


function SysGetKeyEvent: TKeyEvent;
var T : TKeyEvent;
begin
  if PendingKeyEvent<>0 then
  begin
    SysGetKeyEvent:=PendingKeyEvent;
    PendingKeyEvent:=0;
    exit;
  end;
  T := byte(_getch);
  if T = 0 then
    T := word(_getch) shl 8;
  SysGetKeyEvent := $03000000 OR T;
end;


function SysPollKeyEvent: TKeyEvent;
begin
  if PendingKeyEvent<>0 then
   exit(PendingKeyEvent);
  if _kbhit <> 0 then
  begin
    PendingKeyEvent := byte(_getch);
    if PendingKeyEvent = 0 then
      PendingKeyEvent := word(_getch) shl 8;
    PendingKeyEvent := PendingKeyEvent OR $03000000;
    SysPollKeyEvent := PendingKeyEvent;
  end else
    SysPollKeyEvent := 0;
end;


function SysPollShiftStateEvent: TKeyEvent;
begin
  SysPollShiftStateEvent:=0;
end;

function SysGetShiftState: Byte;
begin
  SysGetShiftState:=0;
end;


Const
  SysKeyboardDriver : TKeyboardDriver = (
      InitDriver : Nil;
      DoneDriver : Nil;
      GetKeyevent : @SysGetKeyEvent;
      PollKeyEvent : @SysPollKeyEvent;
      GetShiftState : @SysGetShiftState;
      TranslateKeyEvent : Nil;
      TranslateKeyEventUnicode : Nil;
    );

begin
  KeyboardInitialized := false;
  PendingKeyEvent := 0;
  SetKeyBoardDriver(SysKeyBoardDriver);
end.
