{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-2001 by Free Pascal team

    The most basic Presentation Mode example.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program BasicPM;

{$APPTYPE GUI}

uses
 Os2Def, PMWin;

function ClientWindowProc (Window, Msg: cardinal; MP1, MP2: pointer): pointer;
                                                                 cdecl; export;
var
 Li: longint;
 Ps: cardinal;
 R: TRectL;
 P: TPointL;
 Rgn: cardinal;
begin
 ClientWindowProc := nil;
 case Msg of
  wm_Paint: begin
             PS := WinBeginPaint (Window, 0, @R);
             WinFillRect (PS, @R, SYSCLR_WINDOW);
             WinEndPaint (PS);
            end;
  else ClientWindowProc := WinDefWindowProc (Window, Msg, MP1, MP2);
 end;
end;

const
 idClientWindow = 11000;
 WinFlags: cardinal = fcf_TitleBar + fcf_SysMenu + fcf_SizeBorder +
                                   fcf_MinMax + fcf_TaskList + fcf_NoByteAlign;
 ClassName = 'MYVIEW';

var
 Anchor, MsgQue: cardinal;
 Message: TQMsg;
 Frame, Client: cardinal;
begin
 Anchor := WinInitialize(0);
 { It might be beneficial to set the second parameter of the following }
 { call to something large, such as 1000.  The OS/2 documentation does }
 { not recommend this, however } MsgQue := WinCreateMsgQueue (Anchor, 0);
 if MsgQue = 0 then Halt (254);

 WinMessageBox (HWND_DESKTOP, HWND_DESKTOP, 'FPC test', 'Basic PM', 0,
                                                      MB_OK or MB_INFORMATION);

 WinRegisterClass (Anchor, ClassName, proc (@ClientWindowProc), cs_SizeRedraw,
                                                             SizeOf (pointer));
 Frame := WinCreateStdWindow (hwnd_Desktop, 0, WinFlags, ClassName,
                                     'BASIC PM', 0, 0, idClientWindow, Client);
 if (Frame <> 0) then
 begin
  WinSetWindowPos (Frame, 0, 0, WinQuerySysValue (hwnd_Desktop,
         sv_CyScreen) - 200, 200, 200, swp_Move + swp_Size + swp_Activate +
                                                                     swp_Show);
  while WinGetMsg (Anchor, Message, 0, 0, 0) do
                                              WinDispatchMsg (Anchor, Message);

  WinDestroyWindow (Frame);
 end;
 WinDestroyMsgQueue (MsgQue);
 WinTerminate (Anchor);
end.
