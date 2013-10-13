{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2009-2012  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

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

constructor TDirectXHook.Create(AConsole: TDirectXConsole; AWindow: HWND; AThread: DWord; ACursor, AManaged, AFullscreen: Boolean);
begin
  FConsole := AConsole;

  FCursor := ACursor;
  FManaged := AManaged;
  FFullscreen := AFullscreen;

  LOG('creating window hook');

  inherited Create(AWindow, AThread);
end;

destructor TDirectXHook.Destroy;
begin
  LOG('destroying window hook');
  inherited Destroy;
end;

procedure TDirectXHook.Cursor(AFlag: Boolean);
begin
  FCursor := AFlag;
end;

function TDirectXHook.WndProc(hWnd: HWND; message: DWord; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  active: Boolean;
  thread: DWord;
  placement: WINDOWPLACEMENT;
  console: TDirectXConsole;
begin
  case message of
    WM_PAINT: begin
      LOG('TDirectXHook WM_PAINT');

      { paint console }
      FConsole.Paint;
    end;
    WM_ACTIVATEAPP: begin
      LOG('TDirectXHook WM_ACTIVATEAPP');

      { get window message data }
      active := wParam <> 0;
      thread := DWord(lParam);

      { check active flag }
      if active = False then
      begin
        if FConsole.FGrabMouse and (not FFullscreen) then
	begin
	  FConsole.FWindow.ConfineCursor(False);
	end;

        { deactivate }
        Deactivate;

        { check cursor and fullscreen }
        if (not FCursor) and FFullscreen then
          { show cursor }
          FConsole.FWin32Cursor.Show;
      end
      else
      begin
        { get window placement for active app }
        if not GetWindowPlacement(hWnd, placement) then
          { on failure set to normal show }
          placement.showCmd := SW_SHOWNORMAL;

        { check cursor and fullscreen }
        if (not FCursor) and FFullscreen then
        begin
          { check show command is not minimize }
          if placement.showCmd <> SW_SHOWMINIMIZED then
	  begin
            {hide cursor}
            FConsole.FWin32Cursor.Hide;
	  end;
        end;

	if FConsole.FGrabMouse and (not FFullscreen) then
	begin
          if placement.showCmd <> SW_SHOWMINIMIZED then
	  begin
            FConsole.FWindow.ConfineCursor(True);
	  end;
	end;

        { activate }
        Activate;
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
    WM_PALETTECHANGED:
      begin
        LOG('TDirectXHook WM_PALETTECHANGED');
        if Windows.HWND(wParam) <> hWnd then
        begin
          LOG('not our window');
          if FConsole.FPrimary.Active then
          begin
            FConsole.FPrimary.ResetPalette;
          end;
        end;
      end;
    WM_QUERYNEWPALETTE:
      begin
        LOG('TDirectXHook WM_QUERYNEWPALETTE');
      end;
    WM_CLOSE: begin
      LOG('TDirectXHook WM_CLOSE');

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
          console.internal_shutdown;

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

procedure TDirectXHook.Activate;
var
  display: TDirectXDisplay;
  primary: TDirectXPrimary;
begin
  { check if open }
  if FConsole.FOpen then
  begin
    LOG('activate');

    { get console object references }
    display := FConsole.FDisplay;
    primary := FConsole.FPrimary;

    { check if primary is not active }
    if not primary.Active then
    begin
      { save display }
      display.Save;

      { activate primary }
      primary.Activate;
    end;
  end;
end;

procedure TDirectXHook.Deactivate;
var
  display: TDirectXDisplay;
  primary: TDirectXPrimary;
begin
  { check if open }
  if FConsole.FOpen then
  begin
    LOG('deactivate');

    { get console object references }
    display := FConsole.FDisplay;
    primary := FConsole.FPrimary;

    { check if primary is not active }
    if primary.Active then
    begin
      { save primary }
      primary.Save;

      { deactivate primary }
      primary.Deactivate;

      { restore display }
      display.Restore;
    end;
  end;
end;
