{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

Constructor TWin32Keyboard.Create(window : HWND; thread : DWord; multithreaded : Boolean; EventQueue : TEventQueue);

Begin
  m_monitor := Nil;
  m_event := Nil;
  Inherited Create(window, thread);
  m_monitor := TWin32Monitor.Create;
  m_event := TWin32Event.Create;

  { setup defaults }
  m_alt := False;
  m_shift := False;
  m_control := False;

  { setup data }
  FEventQueue := EventQueue;
  m_multithreaded := multithreaded;

  { enable buffering }
  m_enabled := True;
End;

Destructor TWin32Keyboard.Destroy;

Begin
  m_event.Free;
  m_monitor.Free;
  Inherited Destroy;
End;

(*Function TWin32Keyboard.internal_PeekKey(window : TWin32Window; k : TPTCKeyEvent) : Boolean;

Begin
  { check enabled flag }
  If Not m_enabled Then
  Begin
    Result := False;
    Exit;
  End;

  { enter monitor if multithreaded }
  If m_multithreaded Then
    m_monitor.enter;

  { update window }
  window.update;

  { is a key ready? }
  Result := ready;

  If Result = True Then
    k.Assign(m_buffer[m_tail]);

  { leave monitor if multithreaded }
  If m_multithreaded Then
    m_monitor.leave;
End;

Procedure TWin32Keyboard.internal_ReadKey(window : TWin32Window; k : TPTCKeyEvent);

Var
  read : TPTCKeyEvent;

Begin
  read := Nil;

  Try
    { check enabled flag }
    If Not m_enabled Then
    Begin
      read := TPTCKeyEvent.Create;
      Exit;
    End;

    { check if multithreaded }
    If m_multithreaded Then
    Begin
      { check if ready }
      If Not ready Then
      Begin
        { wait for key event }
        m_event.wait;

        { reset event }
        m_event.reset;
      End;

      { enter monitor }
      m_monitor.enter;

      { remove key }
      read := remove;

      { leave monitor }
      m_monitor.leave;
    End
    Else
    Begin
      { update until ready }
      While Not ready Do
        { update window }
        window.update;

      { remove key }
      read := remove;
    End;
  Finally
    If Assigned(read) Then
      k.Assign(read);
    read.Free;
  End;
End;*)

Procedure TWin32Keyboard.enable;

Begin
  { enable buffering }
  m_enabled := True;
End;

Procedure TWin32Keyboard.disable;

Begin
  { disable buffering }
  m_enabled := False;
End;

Function TWin32Keyboard.WndProc(hWnd : HWND; message : DWord; wParam : WPARAM; lParam : LPARAM) : LRESULT;

Var
  i : Integer;
  scancode : Integer;
  KeyStateArray : Array[0..255] Of Byte;
  AsciiBuf : Word;
  press : Boolean;
  uni : Integer;
  tmp : Integer;

Begin
  WndProc := 0;
  { check enabled flag }
  If Not m_enabled Then
    Exit;

  { process key message }
  If (message = WM_KEYDOWN) Or (message = WM_KEYUP) {Or ((message = WM_SYSKEYDOWN) And ((lParam And (1 Shl 29)) <> 0))} Then
  Begin
    If message = WM_KEYUP Then
      press := False
    Else
      press := True;

    { update modifiers }
    If wParam = VK_MENU Then
      { alt }
      m_alt := press
    Else
      If wParam = VK_SHIFT Then
        { shift }
        m_shift := press
      Else
        If wParam = VK_CONTROL Then
          { control }
          m_control := press;

    { enter monitor if multithreaded }
    If m_multithreaded Then
      m_monitor.enter;

    uni := -1;

    If GetKeyboardState(@KeyStateArray) Then
    Begin
      scancode := (lParam Shr 16) And $FF;
      {todo: ToUnicode (Windows NT)}
      tmp := ToAscii(wParam, scancode, @KeyStateArray, @AsciiBuf, 0);
      If (tmp = 1) Or (tmp = 2) Then
      Begin
        If tmp = 2 Then
        Begin
//          Writeln('[', AsciiBuf, ']'); {???? todo: dead keys ????}
        End
        Else
        Begin
//          Write(Chr(AsciiBuf));
          {todo: codepage -> unicode}
          If AsciiBuf <= 126 Then
            uni := AsciiBuf;
        End;

      End;
    End;

    { handle key repeat count }
    For i := 1 To lParam And $FFFF Do
      { create and insert key object }
      FEventQueue.AddEvent(TPTCKeyEvent.Create(wParam, uni, m_alt, m_shift, m_control, press));

    { check multithreaded flag }
    If m_multithreaded Then
    Begin
      { set event }
      m_event._set;

      { leave monitor }
      m_monitor.leave;
    End;
  End;
(*  Else
    If message = WM_KEYUP Then
      { update modifiers }
      If wParam = VK_MENU Then
        { alt up }
        m_alt := False
      Else
        If wParam = VK_SHIFT Then
          { shift up }
          m_shift := False
        Else
          If wParam = VK_CONTROL Then
            { control up }
            m_control := False;*)
End;

(*Procedure TWin32Keyboard.insert(_key : TPTCKeyEvent);

Begin
  { check for overflow }
  If (m_head <> (m_tail - 1)) And
    ((m_tail <> 0) Or (m_head <> High(m_buffer))) Then
  Begin
    { insert key at head }
    m_buffer[m_head] := _key;

    { increase head }
    Inc(m_head);

    { wrap head from end to start }
    If m_head > High(m_buffer) Then
      m_head := Low(m_buffer);
  End;
End;

Function TWin32Keyboard.remove : TPTCKeyEvent;

Begin
  { return key data from tail }
  remove := m_buffer[m_tail];

  { increase tail }
  Inc(m_tail);

  { wrap tail from end to start }
  If m_tail > High(m_buffer) Then
    m_tail := Low(m_buffer);
End;

Function TWin32Keyboard.ready : Boolean;

Begin
  ready := m_head <> m_tail;
End;
*)
