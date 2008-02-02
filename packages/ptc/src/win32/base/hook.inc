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

Type
  PWin32Hook_Lookup = ^TWin32Hook_Lookup;
  TWin32Hook_Lookup = Record
    window : HWND;
    wndproc : DWord;
    hook : Array[0..15] Of TWin32Hook;
    count : Integer;
  End;

Const
  TWin32Hook_m_count : Integer = 0;
  TWin32Hook_m_cached : PWin32Hook_Lookup = Nil;
  TWin32Hook_m_monitor : TWin32Monitor = Nil;

Var
{  TWin32Hook_m_hook : HHOOK;}
  TWin32Hook_m_registry : Array[0..15] Of TWin32Hook_Lookup;

Function TWin32Hook_hook(hwnd : HWND; msg : DWord; wParam : WPARAM; lParam : LPARAM) : LRESULT; StdCall;

Var
  lookup : PWin32Hook_Lookup;
  i : Integer;

Begin
  { enter monitor }
  TWin32Hook_m_monitor.enter;

  { lookup pointer }
  lookup := Nil;

  { check cached lookup if valid }
  If (TWin32Hook_m_cached <> Nil) And (TWin32Hook_m_cached^.window = hwnd) Then
    { cached lookup match }
    lookup := TWin32Hook_m_cached
  Else
  Begin
    { search for matching window }
    For i := 0 To TWin32Hook_m_count - 1 Do
      { check for lookup window match }
      If TWin32Hook_m_registry[i].window = hwnd Then
      Begin
        { setup cached lookup }
        TWin32Hook_m_cached := @TWin32Hook_m_registry[i];

        { setup lookup }
        lookup := TWin32Hook_m_cached;

        { break }
        Break;
      End;
{$IFDEF DEBUG}
    { check for search failure }
    If lookup = Nil Then
      Raise TPTCError.Create('TWin32Hook window lookup search failure!');
{$ENDIF}
  End;

  { result value }
  TWin32Hook_hook := 0;

  { iterate all hooks for this window }
  For i := lookup^.count - 1 DownTo 0 Do
  Begin
    { call hook window procedure }
    TWin32Hook_hook := lookup^.hook[i].WndProc(hwnd, msg, wParam, lParam);

    { check result value ? }
    {If result = True Then Break;}
  End;

  { check result }
  {If result <> True Then}

  { call original window procedure }
  result := CallWindowProc(WNDPROC(lookup^.wndproc), hwnd, msg, wParam, lParam);

  { leave monitor }
  TWin32Hook_m_monitor.leave;
End;

Constructor TWin32Hook.Create(window : HWND; thread : DWord);

Begin
  { setup data }
  m_window := window;
  m_thread := thread;

  { add to registry }
  add(m_window, m_thread);
End;

Destructor TWin32Hook.Destroy;

Begin
  { remove from registry }
  remove(m_window, m_thread);
  Inherited Destroy;
End;

Procedure TWin32Hook.Add(window : HWND; thread : DWord);

Var
  index, insert : Integer;

Begin
  { enter monitor }
  TWin32Hook_m_monitor.enter;

  { invalidate cache }
  TWin32Hook_m_cached := Nil;

  { registry index }
  index := 0;

  { iterate registry }
  While index < TWin32Hook_m_count Do
  Begin
    { search for existing window hook }
    If TWin32Hook_m_registry[index].window = window Then
    { match }
      Break;

    { next }
    Inc(index);
  End;

  { check results }
  If index <> TWin32Hook_m_count Then
  Begin
    { get insertion point for hook }
    insert := TWin32Hook_m_registry[index].count;

    { increase hook count }
    Inc(TWin32Hook_m_registry[index].count);

{$IFDEF DEBUG}
    { Check for maximum hook count }
    If TWin32Hook_m_registry[index].count > (High(TWin32Hook_m_registry[index].hook) + 1) Then
      Raise TPTCError.Create('TWin32Hook too many hooks created!');
{$ENDIF}

    { insert hook in registry }
    TWin32Hook_m_registry[index].hook[insert] := Self;
  End
  Else
  Begin
    { setup new lookup }
    TWin32Hook_m_registry[index].wndproc := GetWindowLong(window, GWL_WNDPROC);
    TWin32Hook_m_registry[index].window := window;
    TWin32Hook_m_registry[index].hook[0] := Self;
    TWin32Hook_m_registry[index].count := 1;

    { increase lookup count }
    Inc(TWin32Hook_m_count);

{$IFDEF DEBUG}
    { check for maximum count }
    If TWin32Hook_m_count > (High(TWin32Hook_m_registry) + 1) Then
      Raise TPTCError.Create('TWin32Hook too many lookups created!');
{$ENDIF}

    { set window procedure to hook procedure }
    SetWindowLong(window, GWL_WNDPROC, DWord(@TWin32Hook_hook));
  End;

  { leave monitor }
  TWin32Hook_m_monitor.leave;
End;

Procedure TWin32Hook.Remove(window : HWND; thread : DWord);

Var
  index, i, j : Integer;

Begin
  { enter monitor }
  TWin32Hook_m_monitor.enter;

  { invalidate cache }
  TWin32Hook_m_cached := Nil;

  { registry index }
  index := 0;

  { iterate registry }
  While index < TWin32Hook_m_count Do
  Begin
    { check for window match }
    If TWin32Hook_m_registry[index].window = window Then
    Begin
      { search for Self }
      For i := 0 To TWin32Hook_m_registry[index].count Do
        { check hook }
        If TWin32Hook_m_registry[index].hook[i] = Self Then
        Begin
          { remove this hook (quite inefficient for high count...) }
          For j := i To TWin32Hook_m_registry[index].count - 2 Do
            TWin32Hook_m_registry[index].hook[j] :=
              TWin32Hook_m_registry[index].hook[j + 1];

          { decrease hook count }
          Dec(TWin32Hook_m_registry[index].count);

          { break }
          Break;
        End;

      { check remaining hook count }
      If TWin32Hook_m_registry[index].count = 0 Then
      Begin
        { restore original window procedure }
        SetWindowLong(window, GWL_WNDPROC, TWin32Hook_m_registry[i].wndproc);

        { remove this lookup (quite inefficient for high count...) }
        For i := index To TWin32Hook_m_count - 2 Do
          TWin32Hook_m_registry[i] := TWin32Hook_m_registry[i + 1];

        { decrease count }
        Dec(TWin32Hook_m_count);
      End;

      { break }
      Break;
    End;

    { next }
    Inc(index);
  End;

  { leave monitor }
  TWin32Hook_m_monitor.leave;
End;
