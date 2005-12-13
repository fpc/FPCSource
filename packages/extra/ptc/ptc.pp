{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$MODE objfpc}
{$MACRO ON}
{$DEFINE PTC_LOGGING}
{$UNDEF ENABLE_C_API}

{$H+}

{$IFDEF UNIX}
{$DEFINE HAVE_X11_EXTENSIONS_XSHM}
{$DEFINE XStringListToTextProperty_notyetimplemented_in_xutil_pp}
{$ENDIF UNIX}

Unit ptc;

Interface

Uses
{$IFDEF WIN32}
  Windows, DirectDraw,
{$ENDIF WIN32}

{$IFDEF UNIX}
  x, xlib, xutil, keysym,
  xf86vmode, xf86dga,
  {$IFDEF HAVE_X11_EXTENSIONS_XSHM}
  xshm, ipc,
  {$ENDIF HAVE_X11_EXTENSIONS_XSHM}
{$ENDIF UNIX}
  {SysUtils,} Hermes;

Const
  PTC_VERSION = 'OpenPTC 1.0';
  PTC_WIN32_VERSION = 'OpenPTC Win32 1.0.18';

Type
  Pchar8 = ^char8;
  char8 = Byte;
  Pshort16 = ^short16;
  short16 = Word;
  Pint32 = ^int32;
  int32 = DWord;
{$INCLUDE aread.inc}
{$INCLUDE colord.inc}
{$INCLUDE formatd.inc}
{$INCLUDE keyd.inc}
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

{$IFDEF ENABLE_C_API}
{$INCLUDE c_api/index.inc}
{$INCLUDE c_api/errord.inc}
{$INCLUDE c_api/exceptd.inc}
{$INCLUDE c_api/aread.inc}
{$INCLUDE c_api/colord.inc}
{$INCLUDE c_api/cleard.inc}
{$INCLUDE c_api/clipperd.inc}
{$INCLUDE c_api/copyd.inc}
{$INCLUDE c_api/keyd.inc}
{$INCLUDE c_api/formatd.inc}
{$INCLUDE c_api/paletted.inc}
{$INCLUDE c_api/surfaced.inc}
{$INCLUDE c_api/consoled.inc}
{$INCLUDE c_api/moded.inc}
{$INCLUDE c_api/timerd.inc}
{$ENDIF ENABLE_C_API}

{$IFDEF GO32V2}
{$INCLUDE dos/base/kbdd.inc}
{$INCLUDE dos/vesa/consoled.inc}
{$INCLUDE dos/fakemode/consoled.inc}
{$INCLUDE dos/textfx2/consoled.inc}
{$INCLUDE dos/cga/consoled.inc}
{$WARNING should be moved in the implementation part}
{$ENDIF GO32V2}

{$IFDEF WIN32}
{$INCLUDE win32/base/monitord.inc}
{$INCLUDE win32/base/eventd.inc}
{$INCLUDE win32/base/windowd.inc}
{$INCLUDE win32/base/hookd.inc}
{$INCLUDE win32/base/kbdd.inc}

{$INCLUDE win32/directx/hookd.inc}
{$INCLUDE win32/directx/libraryd.inc}
{$INCLUDE win32/directx/displayd.inc}
{$INCLUDE win32/directx/primaryd.inc}
{$INCLUDE win32/directx/consoled.inc}
{$WARNING should be moved in the implementation part}
{$ENDIF WIN32}

{$IFDEF UNIX}
{$INCLUDE x11/imaged.inc}
{$INCLUDE x11/displayd.inc}
{$INCLUDE x11/windowd.inc}
{$INCLUDE x11/dgadispd.inc}
{$INCLUDE x11/consoled.inc}
{$WARNING should be moved in the implementation part}
{$ENDIF UNIX}

Implementation

{$IFDEF GO32V2}
Uses
  textfx2, vesa, vga, cga, timeunit, crt, go32;
{$ENDIF GO32V2}

{$IFDEF WIN32}
{Uses
  Windows, DirectDraw;}
{$ENDIF WIN32}

{$IFDEF UNIX}
Uses
  BaseUnix, Unix;
{$ENDIF UNIX}

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
{$INCLUDE keyi.inc}
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
{$INCLUDE dos/base/kbd.inc}
{$INCLUDE dos/vesa/console.inc}
{$INCLUDE dos/fakemode/console.inc}
{$INCLUDE dos/textfx2/console.inc}
{$INCLUDE dos/cga/console.inc}
{$ENDIF GO32V2}

{$IFDEF WIN32}
{$INCLUDE win32/base/monitor.inc}
{$INCLUDE win32/base/event.inc}
{$INCLUDE win32/base/window.inc}
{$INCLUDE win32/base/hook.inc}
{$INCLUDE win32/base/kbd.inc}
{$INCLUDE win32/directx/check.inc}
{$INCLUDE win32/directx/translte.inc}
{$INCLUDE win32/directx/hook.inc}
{$INCLUDE win32/directx/library.inc}
{$INCLUDE win32/directx/display.inc}
{$INCLUDE win32/directx/primary.inc}
{$INCLUDE win32/directx/console.inc}
{$ENDIF WIN32}

{$IFDEF UNIX}
{$INCLUDE x11/check.inc}
{$INCLUDE x11/image.inc}
{$INCLUDE x11/display.inc}
{$INCLUDE x11/window.inc}
{$INCLUDE x11/dgadisp.inc}
{$INCLUDE x11/console.inc}
{$ENDIF UNIX}

{$INCLUDE consolei.inc}

{$IFDEF ENABLE_C_API}
{$INCLUDE c_api/except.inc}
{$INCLUDE c_api/error.inc}
{$INCLUDE c_api/area.inc}
{$INCLUDE c_api/color.inc}
{$INCLUDE c_api/clear.inc}
{$INCLUDE c_api/clipper.inc}
{$INCLUDE c_api/copy.inc}
{$INCLUDE c_api/key.inc}
{$INCLUDE c_api/format.inc}
{$INCLUDE c_api/palette.inc}
{$INCLUDE c_api/surface.inc}
{$INCLUDE c_api/console.inc}
{$INCLUDE c_api/mode.inc}
{$INCLUDE c_api/timer.inc}
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
