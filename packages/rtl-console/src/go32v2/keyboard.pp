{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Keyboard unit for go32v2

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

uses
  go32;

{$i keyboard.inc}

var
  keyboard_type: byte;  { 0=83/84-key keyboard, $10=101/102+ keyboard }


procedure SysInitKeyboard;
const
  fCarry=1;
var
  regs: trealregs;
begin
  keyboard_type:=0;
  if (Mem[$40:$96] and $10)<>0 then
    begin
      regs.ax:=$1200;
      realintr($16,regs);
      if regs.ax<>$1200 then
        keyboard_type:=$10;
    end;
  regs.ax:=$6601;
  realintr($21,regs);
  if (regs.flags and fCarry) = 0 then
     CurrentLegacy2EnhancedKeyEventTranslationCodePage:=regs.bx;
end;


function SysGetShiftState: Byte;
begin
  SysGetShiftState:=(mem[$40:$17] and %1100) or
                   ((mem[$40:$17] and %0010) shr 1) or
                   ((mem[$40:$17] and %0001) shl 1);
end;


function SysGetKeyEvent: TKeyEvent;

var
  regs : trealregs;
begin
  regs.ah:=keyboard_type;
  realintr($16,regs);
  if (regs.al=$e0) and (regs.ah<>0) then
   regs.al:=0;
  SysGetKeyEvent:=(kbPhys shl 24) or regs.ax or (SysGetShiftState shl 16);
end;


function SysPollKeyEvent: TKeyEvent;
var
  regs : trealregs;
begin
  regs.ah:=keyboard_type+1;
  realintr($16,regs);
  if (regs.realflags and zeroflag<>0) then
   exit(0);
  if (regs.al=$e0) and (regs.ah<>0) then
   regs.al:=0;
  SysPollKeyEvent:=(kbPhys shl 24) or regs.ax or (SysGetShiftState shl 16);
end;


Const
  SysKeyboardDriver : TKeyboardDriver = (
    InitDriver : @SysInitKeyboard;
    DoneDriver : Nil;
    GetKeyevent : @SysGetKeyEvent;
    PollKeyEvent : @SysPollKeyEvent;
    GetShiftState : @SysGetShiftState;
    TranslateKeyEvent : Nil;
    TranslateKeyEventUnicode : Nil;
    GetEnhancedKeyEvent : Nil;
    PollEnhancedKeyEvent : Nil;
  );

begin
  SetKeyBoardDriver(SysKeyBoardDriver);
end.
