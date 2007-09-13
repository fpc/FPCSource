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
  TWin32Keyboard = Class(TWin32Hook)
  Private
    { window procedure }
    Function WndProc(hWnd : HWND; message : DWord; wParam : WPARAM; lParam : LPARAM) : LRESULT; Override;

    { internal key functions }
{    Procedure insert(_key : TPTCKeyEvent);
    Function remove : TPTCKeyEvent;
    Function ready : Boolean;}

    { data }
{    m_key : Boolean;}
    m_multithreaded : Boolean;
    m_event : TWin32Event;
    m_monitor : TWin32Monitor;
    FEventQueue : TEventQueue;

    { flag data }
    m_enabled : Boolean;

    { modifiers }
    m_alt : Boolean;
    m_shift : Boolean;
    m_control : Boolean;

    { key buffer }
{    m_head : Integer;
    m_tail : Integer;
    m_buffer : Array[0..1023] Of TPTCKeyEvent;}
  Public
    { setup }
    Constructor Create(window : HWND; thread : DWord; multithreaded : Boolean; EventQueue : TEventQueue);
    Destructor Destroy; Override;

    { input }
{    Function internal_PeekKey(window : TWin32Window; k : TPTCKeyEvent) : Boolean;
    Procedure internal_ReadKey(window : TWin32Window; k : TPTCKeyEvent);}

    { control }
    Procedure enable;
    Procedure disable;
  End;
