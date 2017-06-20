{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2012, 2017  Nikolay Nikolov (nickysn@users.sourceforge.net)
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

constructor TWin32Keyboard.Create(AWindow: HWND; AThread: DWord; AMultithreaded: Boolean; AEventQueue: TEventQueue);
begin
  FMonitor := nil;
  FEvent := nil;
  inherited Create(AWindow, AThread);
  FMonitor := TWin32Monitor.Create;
  FEvent := TWin32Event.Create;

  { setup data }
  FEventQueue := AEventQueue;
  FMultithreaded := AMultithreaded;

  { enable buffering }
  FEnabled := True;
end;

destructor TWin32Keyboard.Destroy;
begin
  FEvent.Free;
  FMonitor.Free;
  inherited Destroy;
end;

procedure TWin32Keyboard.Enable;
begin
  { enable buffering }
  FEnabled := True;
end;

procedure TWin32Keyboard.Disable;
begin
  { disable buffering }
  FEnabled := False;
end;

function TWin32Keyboard.WndProc(hWnd: HWND; message: DWord; wParam: WPARAM; lParam: LPARAM): LRESULT;
const
  {$warning move this to the windows unit! }
  MAPVK_VK_TO_CHAR = 2;
  MAPVK_VK_TO_VSC = 0;
  MAPVK_VSC_TO_VK = 1;
  MAPVK_VSC_TO_VK_EX = 3;
var
  i: Integer;
  scancode: UINT;
  KeyStateArray: array [0..255] of Byte;
  AsciiBuf: Word;
  press: Boolean;
  uni: Integer;
  TranslatedCharacters, TranslatedWideCharacters: Integer;
  WideStr: WideString;
  KeyCode: Integer;
  ScanCodeB: Byte;
  ExtendedKey, DeadKey: Boolean;
  ModifierKeys: TPTCModifierKeys;
  tmpUINT: UINT;
begin
  Result := 0;
  { check enabled flag }
  if not FEnabled then
    exit;

  { process key message }
  if (message = WM_KEYDOWN) or (message = WM_KEYUP) or (message = WM_SYSKEYDOWN) or (message = WM_SYSKEYUP) then
  begin
    press := (message = WM_KEYDOWN) or (message = WM_SYSKEYDOWN);
    ScanCodeB := Byte(lParam shr 16);
    ExtendedKey := (lParam and (1 shl 24)) <> 0;

    ModifierKeys := [];

    { update modifiers }
    { dead key? }
    DeadKey := (MapVirtualKey(wParam, MAPVK_VK_TO_CHAR) and $80000000) <> 0;
    if DeadKey then
      Include(ModifierKeys, pmkDeadKey);
    { alt }
    if (GetKeyState(VK_MENU) and $8000) <> 0 then
      Include(ModifierKeys, pmkAlt);
    { shift }
    if (GetKeyState(VK_SHIFT) and $8000) <> 0 then
      Include(ModifierKeys, pmkShift);
    { control }
    if (GetKeyState(VK_CONTROL) and $8000) <> 0 then
      Include(ModifierKeys, pmkControl);
    { left/right alt }
    if (GetKeyState(VK_LMENU) and $8000) <> 0 then
      Include(ModifierKeys, pmkLeftAlt);
    if (GetKeyState(VK_RMENU) and $8000) <> 0 then
      Include(ModifierKeys, pmkRightAlt);
    { left/right shift }
    if (GetKeyState(VK_LSHIFT) and $8000) <> 0 then
      Include(ModifierKeys, pmkLeftShift);
    if (GetKeyState(VK_RSHIFT) and $8000) <> 0 then
      Include(ModifierKeys, pmkRightShift);
    { left/right control }
    if (GetKeyState(VK_LCONTROL) and $8000) <> 0 then
      Include(ModifierKeys, pmkLeftControl);
    if (GetKeyState(VK_RCONTROL) and $8000) <> 0 then
      Include(ModifierKeys, pmkRightControl);
    { num lock }
    if (GetKeyState(VK_NUMLOCK) and $8000) <> 0 then
      Include(ModifierKeys, pmkNumLockPressed);
    if (GetKeyState(VK_NUMLOCK) and 1) <> 0 then
      Include(ModifierKeys, pmkNumLockActive);
    { caps lock }
    if (GetKeyState(VK_CAPITAL) and $8000) <> 0 then
      Include(ModifierKeys, pmkCapsLockPressed);
    if (GetKeyState(VK_CAPITAL) and 1) <> 0 then
      Include(ModifierKeys, pmkCapsLockActive);
    { scroll lock }
    if (GetKeyState(VK_SCROLL) and $8000) <> 0 then
      Include(ModifierKeys, pmkScrollLockPressed);
    if (GetKeyState(VK_SCROLL) and 1) <> 0 then
      Include(ModifierKeys, pmkScrollLockActive);

    { enter monitor if multithreaded }
    if FMultithreaded then
      FMonitor.Enter;

    uni := -1;

    if GetKeyboardState(@KeyStateArray) then
    begin
      scancode := (lParam shr 16) and $FF;
      if not press then
        scancode := scancode or $8000;
      if IsWindowUnicode(hWnd) then
      begin
        SetLength(WideStr, 16);
        TranslatedWideCharacters := ToUnicode(wParam, scancode, @KeyStateArray, @WideStr[1], Length(WideStr), 0);
        if TranslatedWideCharacters <> -1 then
          SetLength(WideStr, TranslatedWideCharacters)
        else
          WideStr := '';
      end
      else
      begin
        TranslatedCharacters := ToAscii(wParam, scancode, @KeyStateArray, @AsciiBuf, 0);
        if TranslatedCharacters > 0 then
        begin
          TranslatedWideCharacters := MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, @AsciiBuf, TranslatedCharacters, nil, 0);
          SetLength(WideStr, TranslatedWideCharacters);
          if TranslatedWideCharacters <> 0 then
            MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, @AsciiBuf, TranslatedCharacters, @WideStr[1], TranslatedWideCharacters);
        end
        else
          WideStr := '';
      end;
      if Length(WideStr) = 1 then
        uni := Ord(WideStr[1]);
    end;

    KeyCode := wParam;
    if wParam = VK_RETURN then
    begin
      KeyCode := PTCKEY_ENTER;
      if ExtendedKey then
        Include(ModifierKeys, pmkNumPadKey);
    end;
    if wParam = VK_SHIFT then
    begin
      tmpUINT := MapVirtualKey(ScanCodeB, MAPVK_VSC_TO_VK_EX);
      if tmpUINT <> 0 then
      begin
        { Windows NT 4.0/2000+ }
        if tmpUINT = VK_RSHIFT then
          Include(ModifierKeys, pmkRightKey);
      end
      else
      begin
        { Windows 98 }
        if ScanCodeB = 54 then
          Include(ModifierKeys, pmkRightKey);
      end;
    end;
    if ExtendedKey and (wParam in [VK_MENU,VK_CONTROL]) then
      Include(ModifierKeys, pmkRightKey);
    if not ExtendedKey and
      (wParam in [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN,VK_INSERT,VK_DELETE,
                  VK_HOME,VK_END,VK_PRIOR,VK_NEXT]) then
      Include(ModifierKeys, pmkNumPadKey);
    if wParam in [VK_CLEAR,VK_NUMPAD0..VK_NUMPAD9,VK_DECIMAL,VK_DIVIDE,
                  VK_MULTIPLY,VK_SUBTRACT,VK_ADD,VK_NUMLOCK] then
      Include(ModifierKeys, pmkNumPadKey);
    if wParam = VK_INSERT then
      KeyCode := PTCKEY_INSERT;
    if wParam = VK_DELETE then
      KeyCode := PTCKEY_DELETE;
    if wParam = VK_OEM_COMMA then
      KeyCode := PTCKEY_COMMA;
    if wParam = VK_OEM_PERIOD then
      KeyCode := PTCKEY_PERIOD;

    { handle key repeat count }
    for i := 1 to lParam and $FFFF do
      { create and insert key object }
      FEventQueue.AddEvent(TPTCKeyEvent.Create(KeyCode, uni, ModifierKeys, press));

    { check multithreaded flag }
    if FMultithreaded then
    begin
      { set event }
      FEvent.SetEvent;

      { leave monitor }
      FMonitor.Leave;
    end;
  end;
end;
