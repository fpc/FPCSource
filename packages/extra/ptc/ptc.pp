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

{$MODE objfpc}
{$MACRO ON}
{$DEFINE PTC_LOGGING}
{$UNDEF ENABLE_C_API}

{$H+}

{$IFDEF UNIX}
{$DEFINE HAVE_X11_EXTENSIONS_XSHM}
{$ENDIF UNIX}

Unit ptc;

Interface

{$IFNDEF FPDOC}
Uses
  Hermes;
{$ENDIF FPDOC}

Const
  PTCPAS_VERSION = 'PTCPas 0.99.7';
{  PTC_WIN32_VERSION = 'OpenPTC Win32 1.0.18';}

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
  {to be deprecated}
{  Pint32 = ^int32;
  int32 = Uint32;
  Pshort16 = ^short16;
  short16 = Uint16;
  Pchar8 = ^char8;
  char8 = Uint8;}
  {/to be deprecated}
{$INCLUDE aread.inc}
{$INCLUDE colord.inc}
{$INCLUDE formatd.inc}
{$INCLUDE eventd.inc}
{$INCLUDE keyd.inc}
{$INCLUDE moused.inc}
{$INCLUDE moded.inc}
{$INCLUDE paletted.inc}
{$INCLUDE cleard.inc}
{$INCLUDE copyd.inc}
{$INCLUDE clipperd.inc}
{$INCLUDE basesurd.inc}
{$INCLUDE surfaced.inc}
{$INCLUDE basecond.inc}
{$INCLUDE consoled.inc}
{$INCLUDE errord.inc}
{$INCLUDE timerd.inc}

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

{$IFDEF WIN32}
Uses
  Windows, DirectDraw;
{$ENDIF WIN32}

{$IFDEF UNIX}
Uses
  BaseUnix, Unix, ctypes, x, xlib, xutil, xatom, keysym,
  xf86vmode, xf86dga,
  {$IFDEF HAVE_X11_EXTENSIONS_XSHM}
  xshm, ipc;
  {$ENDIF HAVE_X11_EXTENSIONS_XSHM}
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

{$INCLUDE log.inc}

{$IFDEF WIN32}
{$INCLUDE win32/base/cursor.inc}
{$ENDIF WIN32}

{$INCLUDE errori.inc}
{$INCLUDE areai.inc}
{$INCLUDE colori.inc}
{$INCLUDE formati.inc}
{$INCLUDE eventi.inc}
{$INCLUDE keyi.inc}
{$INCLUDE mousei.inc}
{$INCLUDE modei.inc}
{$INCLUDE palettei.inc}
{$INCLUDE cleari.inc}
{$INCLUDE copyi.inc}
{$INCLUDE clipperi.inc}
{$INCLUDE basesuri.inc}
{$INCLUDE baseconi.inc}
{$INCLUDE surfacei.inc}
{$INCLUDE timeri.inc}

{$IFDEF GO32V2}
{$INCLUDE dos/base/kbdd.inc}
{$INCLUDE dos/base/moused.inc}
{$INCLUDE dos/vesa/consoled.inc}
{$INCLUDE dos/fakemode/consoled.inc}
{$INCLUDE dos/textfx2/consoled.inc}
{$INCLUDE dos/cga/consoled.inc}

{$INCLUDE dos/base/kbd.inc}
{$INCLUDE dos/base/mousei.inc}
{$INCLUDE dos/vesa/console.inc}
{$INCLUDE dos/fakemode/console.inc}
{$INCLUDE dos/textfx2/console.inc}
{$INCLUDE dos/cga/console.inc}
{$ENDIF GO32V2}

{$IFDEF WIN32}
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
{$INCLUDE win32/directx/consoled.inc}

{$INCLUDE win32/base/monitor.inc}
{$INCLUDE win32/base/event.inc}
{$INCLUDE win32/base/window.inc}
{$INCLUDE win32/base/hook.inc}
{$INCLUDE win32/base/kbd.inc}
{$INCLUDE win32/base/mousei.inc}
{$INCLUDE win32/directx/check.inc}
{$INCLUDE win32/directx/translte.inc}
{$INCLUDE win32/directx/hook.inc}
{$INCLUDE win32/directx/library.inc}
{$INCLUDE win32/directx/display.inc}
{$INCLUDE win32/directx/primary.inc}
{$INCLUDE win32/directx/console.inc}
{$ENDIF WIN32}

{$IFDEF UNIX}
{$INCLUDE x11/modesd.inc}
{$INCLUDE x11/imaged.inc}
{$INCLUDE x11/displayd.inc}
{$INCLUDE x11/windowd.inc}
{$INCLUDE x11/dgadispd.inc}
{$INCLUDE x11/consoled.inc}

{$INCLUDE x11/check.inc}
{$INCLUDE x11/modesi.inc}
{$INCLUDE x11/imagei.inc}
{$INCLUDE x11/displayi.inc}
{$INCLUDE x11/windowi.inc}
{$INCLUDE x11/dgadispi.inc}
{$INCLUDE x11/consolei.inc}
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
