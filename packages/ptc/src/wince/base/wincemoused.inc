{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2006  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

Type
  TWinCEMouse = Class(TObject)
  Private
    FEventQueue : TEventQueue;

    FFullScreen : Boolean;

    { the actual image area, inside the window (top left and bottom right corner) }
    FWindowX1, FWindowY1, FWindowX2, FWindowY2 : Integer;

    { console resolution
      - mouse cursor position as seen by the user must always be in range:
        [0..FConsoleWidth-1, 0..FConsoleHeight-1] }
    FConsoleWidth, FConsoleHeight : Integer;

    FPreviousMouseButtonState : TPTCMouseButtonState;
    FPreviousMouseX, FPreviousMouseY : Integer; { for calculating the deltas }
    FPreviousMousePositionSaved : Boolean; { true, if FPreviousMouseX,
           FPreviousMouseY and FPreviousMouseButtonState contain valid values }

    { flag data }
    FEnabled : Boolean;
  Public
    { setup }
    Constructor Create(EventQueue : TEventQueue; FullScreen : Boolean; ConsoleWidth, ConsoleHeight : Integer);

    { window procedure }
    Function WndProc(hWnd : HWND; message : DWord; wParam : WPARAM; lParam : LPARAM) : LRESULT;

    Procedure SetWindowArea(WindowX1, WindowY1, WindowX2, WindowY2 : Integer);

    { control }
    Procedure enable;
    Procedure disable;
  End;
