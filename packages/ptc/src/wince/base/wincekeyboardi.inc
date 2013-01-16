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

constructor TWinCEKeyboard.Create(EventQueue: TEventQueue);

begin
//  m_monitor := nil;
//  m_event := nil;
//  inherited Create(window, thread);
//  m_monitor := TWin32Monitor.Create;
//  m_event := TWin32Event.Create;

  { setup defaults }
  m_alt := False;
  m_shift := False;
  m_control := False;

  { setup data }
  FEventQueue := EventQueue;
//  m_multithreaded := multithreaded;

  { enable buffering }
  m_enabled := True;
end;

procedure TWinCEKeyboard.enable;

begin
  { enable buffering }
  m_enabled := True;
end;

procedure TWinCEKeyboard.disable;

begin
  { disable buffering }
  m_enabled := False;
end;

function TWinCEKeyboard.WndProc(hWnd: HWND; message: DWord; wParam: WPARAM; lParam: LPARAM): LRESULT;

var
  i: Integer;
  scancode: Integer;
  KeyStateArray: array [0..255] of Byte;
  AsciiBuf: Word;
  press: Boolean;
  uni: Integer;
  tmp: Integer;

begin
  WndProc := 0;
  { check enabled flag }
  if not m_enabled then
    exit;

  { process key message }
  if (message = WM_KEYDOWN) or (message = WM_KEYUP) {Or ((message = WM_SYSKEYDOWN) and ((lParam and (1 shl 29)) <> 0))} then
  begin
    if message = WM_KEYUP then
      press := False
    else
      press := True;

    { update modifiers }
    if wParam = VK_MENU then
      { alt }
      m_alt := press
    else
      if wParam = VK_SHIFT then
        { shift }
        m_shift := press
      else
        if wParam = VK_CONTROL then
          { control }
          m_control := press;

    { enter monitor if multithreaded }
(*    if m_multithreaded then
      m_monitor.enter;*)

    uni := -1;

(*    if GetKeyboardState(@KeyStateArray) then
    begin
      scancode := (lParam shr 16) and $FF;
      {todo: ToUnicode (Windows NT)}
      tmp := ToAscii(wParam, scancode, @KeyStateArray, @AsciiBuf, 0);
      if (tmp = 1) or (tmp = 2) then
      begin
        if tmp = 2 then
        begin
//          Writeln('[', AsciiBuf, ']'); {???? todo: dead keys ????}
        end
        else
        begin
//          Write(Chr(AsciiBuf));
          {todo: codepage -> unicode}
          if AsciiBuf <= 126 then
            uni := AsciiBuf;
        end;

      end;
    End;*)

    { handle key repeat count }
    for i := 1 to lParam and $FFFF do
      { create and insert key object }
      FEventQueue.AddEvent(TPTCKeyEvent.Create(wParam, uni, m_alt, m_shift, m_control, press));

    { check multithreaded flag }
(*    if m_multithreaded then
    begin
      { set event }
      m_event._set;

      { leave monitor }
      m_monitor.leave;
    End;*)
  end;
end;
