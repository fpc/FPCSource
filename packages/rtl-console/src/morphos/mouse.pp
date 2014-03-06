{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 Karoly Balogh
    member of the Free Pascal development team

    Mouse unit for Amiga/MorphOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Mouse;
interface

{$i mouseh.inc}

implementation

{$i mouse.inc}

function SysDetectMouse:byte;
var
  num : dword;
begin
  // Under Amiga/MorphOS, mouse is always there, and it's unable to easily
  // detect number of buttons. So lets report 3, which is common nowadays. (KB)
  SysDetectMouse:=3;
end;









const
  SysMouseDriver : TMouseDriver = (
    UseDefaultQueue : True;
    InitDriver      : Nil;
    DoneDriver      : Nil;
    DetectMouse     : @SysDetectMouse;
    ShowMouse       : Nil;
    HideMouse       : Nil;
    GetMouseX       : Nil;
    GetMouseY       : Nil;
    GetMouseButtons : Nil;
    SetMouseXY      : Nil;
    GetMouseEvent   : Nil;
    PollMouseEvent  : Nil;
    PutMouseEvent   : Nil;
  );

begin
  SetMouseDriver(SysMouseDriver);
end.
