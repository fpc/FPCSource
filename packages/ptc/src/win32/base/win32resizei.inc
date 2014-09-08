{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2013 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

constructor TWin32Resize.Create(AWindow: HWND; AThread: DWord; AEventQueue: TEventQueue);
begin
  inherited Create(AWindow, AThread);
  FEventQueue := AEventQueue;
end;

function TWin32Resize.WndProc(hWnd: HWND; message: DWord; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  Width, Height: Integer;
begin
  Result := 0;
  
  if message = WM_SIZE then
  begin
    Width := lParam and $FFFF;
    Height := (lParam shr 16) and $FFFF;
    //Writeln(wParam, ' ', Width, ' ', Height);
    if wParam <> SIZE_MINIMIZED then
    begin
      FEventQueue.AddEvent(TPTCResizeEvent.Create(Width, Height));
    end;
  end;
end;
