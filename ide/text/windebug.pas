{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Pierre Muller

    Win32 specific debugger routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit windebug;

interface

  type
    DebuggeeState = (Running_State,Stopped_State);

  procedure  ChangeDebuggeeWindowTitleTo(State : DebuggeeState);

implementation

uses
  gdbint,
  strings,
  windows;

 function GetWindowHandle(H : HWND; state : LPARAM) : WINBOOL;stdcall;
   var pTitle, pEnd, pNewTitle : pchar;
       len : longint;
   begin
     GetWindowHandle:=true;
     GetMem(pTitle,256);
     { we want all windows !! }
     if GetWindowThreadProcessId(H,nil)=inferior_pid then
       begin
         len:=GetWindowText(H,pTitle,256);
         if DebuggeeState(State) = Stopped_State then
           begin
             GetMem(pNewTitle,len+50);
             pEnd:=strpos(pTitle,'... running under FP debugger');
             if assigned(pEnd) then
               pEnd^:=#0;
             strcopy(pNewTitle,pTitle);
             strcat(pNewTitle,'... stopped by FP debugger');
             SetWindowText(H,pNewTitle);
             FreeMem(pNewTitle,len+50);
           end
         else if DebuggeeState(State) = Running_State then
           begin
             GetMem(pNewTitle,len+50);
             pEnd:=strpos(pTitle,'... stopped by FP debugger');
             if assigned(pEnd) then
               pEnd^:=#0;
             strcopy(pNewTitle,pTitle);
             strcat(pNewTitle,'... running under FP debugger');
             SetWindowText(H,pNewTitle);
             FreeMem(pNewTitle,len+50);
           end;
       end;
     FreeMem(pTitle,256);
   end;

 procedure  ChangeDebuggeeWindowTitleTo(State : DebuggeeState);
   begin
     EnumWindows(EnumWindowsProc(@GetWindowHandle),longint(State));
   end;

end.