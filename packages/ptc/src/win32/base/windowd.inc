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
  TWin32Window = Class(TObject)
  Private
    Procedure internal_create(wndclass, title : String; extra, style : DWord; show, x, y, width, height : Integer; center, _multithreaded : Boolean; data : Pointer);

    Procedure defaults;
    Procedure close;
    Class Procedure ThreadFunction(owner : TWin32Window);
{    Class Function WndProcSingleThreaded(hWnd : HWND; message : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT; StdCall;
    Class Function WndProcMultiThreaded(hWnd : HWND; message : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT; StdCall;}
    m_window : HWND;
    m_event : THANDLE;
    m_thread : THANDLE;
    m_id : DWord;
    m_name : AnsiString;
    m_title : AnsiString;
    m_extra : DWord;
    m_style : DWord;
    m_show : Integer;
    m_x, m_y : Integer;
    m_width, m_height : Integer;
    m_data : Pointer;
    m_managed : Boolean;
    m_multithreaded : Boolean;
  Public
    Constructor Create(window : HWND);
    Constructor Create(wndclass, title : String; extra, style : DWord; show, x, y, width, height : Integer; center, _multithreaded : Boolean; data : Pointer);
    Constructor Create(wndclass, title : String; extra, style : DWord; show, x, y, width, height : Integer; center, _multithreaded : Boolean);
    Destructor Destroy; Override;
    Procedure cursor(flag : Boolean);
    Procedure resize(width, height : Integer);
    Procedure update(force : Boolean);
    Procedure update; {force = False}
    Function handle : HWND;
    Function thread : DWord;
    Function managed : Boolean;
    Function multithreaded : Boolean;
  End;
