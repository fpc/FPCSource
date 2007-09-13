{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2006  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

{$MODE objfpc}
{$MACRO ON}
{$UNDEF ENABLE_C_API}

{$H+}

{$IFDEF UNIX}

  { X11 extensions we want to enable at compile time }
  {$INCLUDE x11/extensions.inc}

  {$IFDEF ENABLE_X11_EXTENSION_XF86DGA1}
    {$DEFINE ENABLE_X11_EXTENSION_XF86DGA}
  {$ENDIF ENABLE_X11_EXTENSION_XF86DGA1}
  {$IFDEF ENABLE_X11_EXTENSION_XF86DGA2}
    {$DEFINE ENABLE_X11_EXTENSION_XF86DGA}
  {$ENDIF ENABLE_X11_EXTENSION_XF86DGA2}

{$ENDIF UNIX}

Unit ptc;

Interface

{$IFNDEF FPDOC}
Uses
  Hermes;
{$ENDIF FPDOC}

Const
  PTCPAS_VERSION = 'PTCPas 0.99.7';

Type
  PUint8  = ^Uint8;
  PUint16 = ^Uint16;
  PUint32 = ^Uint32;
  PUint64 = ^Uint64;
  PSint8  = ^Sint8;
  PSint16 = ^Sint16;
  PSint32 = ^Sint32;
  PSint64 = ^Sint64;
  Uint8  = Byte;
  Uint16 = Word;
  Uint32 = DWord;
  Uint64 = QWord;
  Sint8  = ShortInt;
  Sint16 = SmallInt;
  Sint32 = LongInt;
  Sint64 = Int64;

{$INCLUDE coreinterface.inc}

{$IFNDEF FPDOC}

{$IFDEF ENABLE_C_API}
{$INCLUDE c_api/index.pp}
{$INCLUDE c_api/errord.pp}
{$INCLUDE c_api/exceptd.pp}
{$INCLUDE c_api/aread.pp}
{$INCLUDE c_api/colord.pp}
{$INCLUDE c_api/cleard.pp}
{$INCLUDE c_api/clipperd.pp}
{$INCLUDE c_api/copyd.pp}
{$INCLUDE c_api/keyd.pp}
{$INCLUDE c_api/formatd.pp}
{$INCLUDE c_api/paletted.pp}
{$INCLUDE c_api/surfaced.pp}
{$INCLUDE c_api/consoled.pp}
{$INCLUDE c_api/moded.pp}
{$INCLUDE c_api/timerd.pp}
{$ENDIF ENABLE_C_API}

{$ENDIF FPDOC}

Implementation

{$IFDEF GO32V2}
Uses
  textfx2, vesa, vga, cga, timeunit, crt, go32, mouse33h;
{$ENDIF GO32V2}

{$IFDEF Win32}
Uses
  Windows, p_ddraw;
{$ENDIF Win32}

{$IFDEF WinCE}
Uses
  Windows, p_gx;
{$ENDIF WinCE}

{$IFDEF UNIX}
Uses
  BaseUnix, Unix, ctypes, x, xlib, xutil, xatom, keysym
  {$IFDEF ENABLE_X11_EXTENSION_XRANDR}
  , xrandr
  {$ENDIF ENABLE_X11_EXTENSION_XRANDR}
  {$IFDEF ENABLE_X11_EXTENSION_XF86VIDMODE}
  , xf86vmode
  {$ENDIF ENABLE_X11_EXTENSION_XF86VIDMODE}
  {$IFDEF ENABLE_X11_EXTENSION_XF86DGA}
  , xf86dga
  {$ENDIF ENABLE_X11_EXTENSION_XF86DGA}
  {$IFDEF ENABLE_X11_EXTENSION_XSHM}
  , xshm, ipc
  {$ENDIF ENABLE_X11_EXTENSION_XSHM}
  ;
{$ENDIF UNIX}

{ this little procedure is not a good reason to include the whole sysutils
  unit :) }
Procedure FreeAndNil(Var q);

Var
  tmp : TObject;

Begin
  tmp := TObject(q);
  Pointer(q) := Nil;
  tmp.Free;
End;

Procedure FreeMemAndNil(Var q);

Var
  tmp : Pointer;

Begin
  tmp := Pointer(q);
  Pointer(q) := Nil;
  If tmp <> Nil Then
    FreeMem(tmp);
End;

Function IntToStr(Value : Integer) : String;

Begin
  System.Str(Value, Result);
End;

Function IntToStr(Value : Int64) : String;

Begin
  System.Str(Value, Result);
End;

Function IntToStr(Value : QWord) : String;
Begin
  System.Str(Value, Result);
End;

{$INCLUDE log.inc}

{$IFDEF WIN32}
{$INCLUDE win32/base/cursor.inc}
{$ENDIF WIN32}

{$INCLUDE coreimplementation.inc}

{$IFDEF GO32V2}
{$INCLUDE dos/includes.inc}
{$ENDIF GO32V2}

{$IFDEF Win32}
{$INCLUDE win32/base/monitord.inc}
{$INCLUDE win32/base/eventd.inc}
{$INCLUDE win32/base/windowd.inc}
{$INCLUDE win32/base/hookd.inc}
{$INCLUDE win32/base/kbdd.inc}
{$INCLUDE win32/base/moused.inc}
{$INCLUDE win32/directx/hookd.inc}
{$INCLUDE win32/directx/libraryd.inc}
{$INCLUDE win32/directx/displayd.inc}
{$INCLUDE win32/directx/primaryd.inc}
{$INCLUDE win32/directx/directxconsoled.inc}
{$INCLUDE win32/gdi/win32dibd.inc}
{$INCLUDE win32/gdi/gdiconsoled.inc}

{$INCLUDE win32/base/monitor.inc}
{$INCLUDE win32/base/event.inc}
{$INCLUDE win32/base/window.inc}
{$INCLUDE win32/base/hook.inc}
{$INCLUDE win32/base/kbd.inc}
{$INCLUDE win32/base/mousei.inc}
{$INCLUDE win32/directx/check.inc}
{$INCLUDE win32/directx/translate.inc}
{$INCLUDE win32/directx/hook.inc}
{$INCLUDE win32/directx/library.inc}
{$INCLUDE win32/directx/display.inc}
{$INCLUDE win32/directx/primary.inc}
{$INCLUDE win32/directx/directxconsolei.inc}
{$INCLUDE win32/gdi/win32dibi.inc}
{$INCLUDE win32/gdi/gdiconsolei.inc}
{$ENDIF Win32}

{$IFDEF WinCE}
{$INCLUDE wince/includes.inc}
{$ENDIF WinCE}

{$IFDEF UNIX}
{$INCLUDE x11/includes.inc}
{$ENDIF UNIX}

{$INCLUDE consolei.inc}

{$IFDEF ENABLE_C_API}
{$INCLUDE c_api/except.pp}
{$INCLUDE c_api/error.pp}
{$INCLUDE c_api/area.pp}
{$INCLUDE c_api/color.pp}
{$INCLUDE c_api/clear.pp}
{$INCLUDE c_api/clipper.pp}
{$INCLUDE c_api/copy.pp}
{$INCLUDE c_api/key.pp}
{$INCLUDE c_api/format.pp}
{$INCLUDE c_api/palette.pp}
{$INCLUDE c_api/surface.pp}
{$INCLUDE c_api/console.pp}
{$INCLUDE c_api/mode.pp}
{$INCLUDE c_api/timer.pp}
{$ENDIF ENABLE_C_API}

Initialization

Begin
  {$IFDEF ENABLE_C_API}
  ptc_error_handler_function := @ptc_error_handler_default;
  {$ENDIF ENABLE_C_API}
  {$IFDEF WIN32}
  TWin32Hook_m_monitor := TWin32Monitor.Create;
  {$ENDIF WIN32}
End;

Finalization

Begin
  {$IFDEF WIN32}
  FreeAndNil(TWin32Hook_m_monitor);
  {$ENDIF WIN32}
End;

End.
