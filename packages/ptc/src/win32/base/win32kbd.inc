{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2012  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

constructor TWin32Keyboard.Create(AWindow: HWND; AThread: DWord; AMultithreaded: Boolean; AEventQueue: TEventQueue);
begin
  FMonitor := nil;
  FEvent := nil;
  inherited Create(AWindow, AThread);
  FMonitor := TWin32Monitor.Create;
  FEvent := TWin32Event.Create;

  { setup defaults }
  FAlt := False;
  FShift := False;
  FControl := False;

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
var
  i: Integer;
  scancode: Integer;
  KeyStateArray: array [0..255] of Byte;
  AsciiBuf: Word;
  press: Boolean;
  uni: Integer;
  TranslatedCharacters, TranslatedWideCharacters: Integer;
  WideStr: WideString;
  KeyCode: Integer;
begin
  Result := 0;
  { check enabled flag }
  if not FEnabled then
    exit;

  { process key message }
  if (message = WM_KEYDOWN) or (message = WM_KEYUP) or (message = WM_SYSKEYDOWN) or (message = WM_SYSKEYUP) then
  begin
    press := (message = WM_KEYDOWN) or (message = WM_SYSKEYDOWN);

    { update modifiers }
    if wParam = VK_MENU then
      { alt }
      FAlt := press
    else
      if wParam = VK_SHIFT then
        { shift }
        FShift := press
      else
        if wParam = VK_CONTROL then
          { control }
          FControl := press;

    { enter monitor if multithreaded }
    if FMultithreaded then
      FMonitor.Enter;

    uni := -1;

    if GetKeyboardState(@KeyStateArray) then
    begin
      scancode := (lParam shr 16) and $FF;
      {todo: ToUnicode (Windows NT)}
      TranslatedCharacters := ToAscii(wParam, scancode, @KeyStateArray, @AsciiBuf, 0);
      if (TranslatedCharacters = 1) or (TranslatedCharacters = 2) then
      begin
        TranslatedWideCharacters := MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, @AsciiBuf, TranslatedCharacters, nil, 0);
        if TranslatedWideCharacters <> 0 then
        begin
          SetLength(WideStr, TranslatedWideCharacters);
          MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, @AsciiBuf, TranslatedCharacters, @WideStr[1], TranslatedWideCharacters);

          if Length(WideStr) = 1 then
            uni := Ord(WideStr[1]);
        end;
      end;
    end;

    KeyCode := wParam;
    if wParam = VK_RETURN then
      KeyCode := PTCKEY_ENTER;
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
      FEventQueue.AddEvent(TPTCKeyEvent.Create(KeyCode, uni, FAlt, FShift, FControl, press));

    { check multithreaded flag }
    if FMultithreaded then
    begin
      { set event }
      FEvent.SetEvent;

      { leave monitor }
      FMonitor.Leave;
    end;
  end;
(*  else
    if message = WM_KEYUP then
      { update modifiers }
      if wParam = VK_MENU then
        { alt up }
        m_alt := False
      else
        if wParam = VK_SHIFT then
          { shift up }
          m_shift := False
        else
          if wParam = VK_CONTROL then
            { control up }
            m_control := False;*)
end;
