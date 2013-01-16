{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2007, 2009, 2010  Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

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

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

type
  TWinCEMouse = Class(TObject)
  private
    FEventQueue: TEventQueue;

    FFullScreen: Boolean;

    { the actual image area, inside the window (top left and bottom right corner) }
    FWindowX1, FWindowY1, FWindowX2, FWindowY2: Integer;

    { console resolution
      - mouse cursor position as seen by the user must always be in range:
        [0..FConsoleWidth-1, 0..FConsoleHeight-1] }
    FConsoleWidth, FConsoleHeight: Integer;

    FPreviousMouseButtonState: TPTCMouseButtonState;
    FPreviousMouseX, FPreviousMouseY: Integer; { for calculating the deltas }
    FPreviousMousePositionSaved: Boolean; { true, if FPreviousMouseX,
           FPreviousMouseY and FPreviousMouseButtonState contain valid values }

    { flag data }
    FEnabled: Boolean;
  public
    { setup }
    constructor Create(EventQueue: TEventQueue; FullScreen: Boolean; ConsoleWidth, ConsoleHeight: Integer);

    { window procedure }
    function WndProc(hWnd: HWND; message: DWord; wParam: WPARAM; lParam: LPARAM): LRESULT;

    procedure SetWindowArea(WindowX1, WindowY1, WindowX2, WindowY2: Integer);

    { control }
    procedure enable;
    procedure disable;
  end;
