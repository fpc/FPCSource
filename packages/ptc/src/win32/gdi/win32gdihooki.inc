{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2012 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

constructor TGDIHook.Create(AConsole: TGDIConsole; AWindow: HWND; AThread: DWord; ACursor, AManaged, AFullscreen: Boolean);
begin
  FConsole := AConsole;

  FCursor := ACursor;
  FManaged := AManaged;
  FFullscreen := AFullscreen;

  LOG('creating window hook');

  inherited Create(AWindow, AThread);
end;

destructor TGDIHook.Destroy;
begin
  LOG('destroying window hook');
  inherited Destroy;
end;

procedure TGDIHook.Cursor(AFlag: Boolean);
begin
  FCursor := AFlag;
end;

function TGDIHook.WndProc(hWnd: HWND; message: DWord; wParam: WPARAM; lParam: LPARAM): LRESULT;

  function WMSizeWParam2String(wParam: WPARAM): string;
  begin
    case wParam of
      SIZE_MAXHIDE:   Result := 'SIZE_MAXHIDE';
      SIZE_MAXIMIZED: Result := 'SIZE_MAXIMIZED';
      SIZE_MAXSHOW:   Result := 'SIZE_MAXSHOW';
      SIZE_MINIMIZED: Result := 'SIZE_MINIMIZED';
      SIZE_RESTORED:  Result := 'SIZE_RESTORED';
      else
        Result := 'Unknown (' + IntToStr(wParam) + ')';
    end;
  end;

var
  active: Boolean;
  thread: DWord;
  console: TGDIConsole;
begin
  case message of
    WM_PAINT: begin
      LOG('TGDIHook WM_PAINT');

      { paint console }
      //FConsole.Paint;
    end;
    WM_ACTIVATEAPP: begin
      LOG('TGDIHook WM_ACTIVATEAPP');

      { get window message data }
      active := wParam <> 0;
      thread := DWord(lParam);

      { check active flag }
      if active = False then
      begin
        if FConsole.FGrabMouse and (not FFullscreen) then
          FConsole.FWindow.ConfineCursor(False);

        if FFullscreen then
          ShowWindow(FWindow, SW_MINIMIZE);
      end
      else
      begin
        if FConsole.FGrabMouse and (not FFullscreen) then
          FConsole.FWindow.ConfineCursor(True);
      end;

    end;
    WM_SIZE:
      begin
        LOG('TGDIHook WM_SIZE, wParam = ' + WMSizeWParam2String(wParam));
        case wParam of
          SIZE_MINIMIZED:
            begin
              Deactivate;
            end;
          SIZE_RESTORED, SIZE_MAXIMIZED:
            begin
              Activate;
            end;
        end;

        { pass to the next handler (or DefWindowProc) }
        Result := 0;
        exit;
      end;
    WM_SETCURSOR: begin
      { check cursor }
      if not FCursor then
      begin
        if FFullscreen or (LOWORD(lParam) = HTCLIENT) then
        begin
          { hide cursor }
          SetCursor(0);

          { handled }
          Result := 1;
        end;
      end;
    end;
    WM_CLOSE: begin
      LOG('TGDIHook WM_CLOSE');

      if FManaged then
      begin
        if FConsole.InterceptClose then
        begin
          FConsole.FEventQueue.AddEvent(TPTCCloseEvent.Create);
          Result := 0;
          exit;
        end
        else
        begin
          console := FConsole;

          { close console }
          console.Close;

          { note: at this point the hook object has been destroyed by the console! }

          { internal console shutdown }
          //console.internal_shutdown;

          { halt }
          Halt(0);
        end;
      end;

      { handled }
      Result := 1;
      exit;
    end;
  end;

  { unhandled }
  Result := 0;
end;

procedure TGDIHook.Activate;
var
//  placement: WINDOWPLACEMENT;
  ModeSetter: TWin32ModeSetter;
begin
  LOG('activate');

  if FConsole.FOpen then
  begin
    if FFullscreen then
    begin
      ModeSetter := FConsole.FModeSetter;

      if not ModeSetter.InMode then
      begin
        ModeSetter.Save;
        ModeSetter.Enter;
      end;

      if not FCursor then
        FConsole.FWin32Cursor.Hide;
    end;
  end;
end;

procedure TGDIHook.Deactivate;
var
  ModeSetter: TWin32ModeSetter;
begin
  LOG('deactivate');

  if FConsole.FOpen then
  begin
    if FFullscreen then
    begin
      if not FCursor then
        FConsole.FWin32Cursor.Show;

      ModeSetter := FConsole.FModeSetter;

      if ModeSetter.InMode then
      begin
        ModeSetter.Leave;
        ModeSetter.Restore;
      end;
    end;
  end;
end;
