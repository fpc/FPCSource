{
    $Id$
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


function SysGetKeyEvent: TKeyEvent;

var
  regs : trealregs;
begin
  regs.ah:=$10;
  realintr($16,regs);
  if (regs.al=$e0) and (regs.ah<>0) then
   regs.al:=0;
  SysGetKeyEvent:=(kbPhys shl 24) or regs.ax or ((mem[$40:$17] and $f) shl 16);
end;


function SysPollKeyEvent: TKeyEvent;
var
  regs : trealregs;
begin
  regs.ah:=$11;
  realintr($16,regs);
  if (regs.realflags and zeroflag<>0) then
   exit(0);
  if (regs.al=$e0) and (regs.ah<>0) then
   regs.al:=0;
  SysPollKeyEvent:=(kbPhys shl 24) or regs.ax or ((mem[$40:$17] and $f) shl 16);
end;


function SysGetShiftState: Byte;
begin
  SysGetShiftState:=(mem[$40:$17] and $f);
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
  SetKeyBoardDriver(SysKeyBoardDriver);
end.

{
  $Log$
  Revision 1.3  2002-08-28 06:35:30  pierre
   * merge kbPhys patch from fixes branch

  Revision 1.1.2.3  2002/07/13 12:22:03  pierre
   * added kbPhys flag

  Revision 1.2  2001/09/21 21:33:35  michael
  + Merged driver support from fixbranch

  Revision 1.1.2.2  2001/09/21 21:20:43  michael
  + Added support for keyboard driver.
  + Added DefaultTranslateKeyEvent,DefaultTranslateKeyEventUnicode
  + PendingKeyEvent variable no longer public. Handling of this variable is
    now done entirely by global functions. System dependent code should not
    need it, it is set automatically.
  + InitVideo DoneVideo will check whether the keyboard is initialized or not.

  Revision 1.1.2.1  2001/01/30 21:52:01  peter
    * moved api utils to rtl

  Revision 1.1  2001/01/13 11:03:57  peter
    * API 2 RTL commit

}
