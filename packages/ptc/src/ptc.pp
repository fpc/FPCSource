{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2006  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

unit ptc;

interface

{$IFNDEF FPDOC}
uses
  Hermes;
{$ENDIF FPDOC}

const
  PTCPAS_VERSION = 'PTCPas 0.99.12';

type
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

{$INCLUDE core/coreinterface.inc}

{$IFNDEF FPDOC}

{$IFDEF ENABLE_C_API}
{$INCLUDE c_api/capi_index.inc}
{$INCLUDE c_api/capi_errord.inc}
{$INCLUDE c_api/capi_exceptd.inc}
{$INCLUDE c_api/capi_aread.inc}
{$INCLUDE c_api/capi_colord.inc}
{$INCLUDE c_api/capi_cleard.inc}
{$INCLUDE c_api/capi_clipperd.inc}
{$INCLUDE c_api/capi_copyd.inc}
{$INCLUDE c_api/capi_keyd.inc}
{$INCLUDE c_api/capi_formatd.inc}
{$INCLUDE c_api/capi_paletted.inc}
{$INCLUDE c_api/capi_surfaced.inc}
{$INCLUDE c_api/capi_consoled.inc}
{$INCLUDE c_api/capi_moded.inc}
{$INCLUDE c_api/capi_timerd.inc}
{$ENDIF ENABLE_C_API}

{$ENDIF FPDOC}

implementation

{$IFDEF GO32V2}
uses
  textfx2, vesa, vga, cga, timeunit, crt, go32fix, mouse33h;
{$ENDIF GO32V2}

{$IF defined(WIN32) OR defined(WIN64)}
uses
  Windows, p_ddraw;
{$ENDIF defined(WIN32) OR defined(WIN64)}

{$IFDEF WinCE}
uses
  Windows, p_gx;
{$ENDIF WinCE}

{$IFDEF UNIX}
uses
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
procedure FreeAndNil(var q);
var
  tmp: TObject;
begin
  tmp := TObject(q);
  Pointer(q) := nil;
  tmp.Free;
end;

procedure FreeMemAndNil(var q);
var
  tmp: Pointer;
begin
  tmp := Pointer(q);
  Pointer(q) := nil;
  if tmp <> nil then
    FreeMem(tmp);
end;

function IntToStr(Value: Integer): string;
begin
  System.Str(Value, Result);
end;

function IntToStr(Value: Int64): string;
begin
  System.Str(Value, Result);
end;

function IntToStr(Value: QWord): string;
begin
  System.Str(Value, Result);
end;

{$INCLUDE core/log.inc}

{$INCLUDE core/coreimplementation.inc}

{$IFDEF GO32V2}
{$INCLUDE dos/includes.inc}
{$ENDIF GO32V2}

{$IF defined(Win32) OR defined(Win64)}
{$INCLUDE win32/base/cursord.inc}
{$INCLUDE win32/base/cursormoded.inc}
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

{$INCLUDE win32/base/cursor.inc}
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
{$ENDIF defined(Win32) OR defined(Win64)}

{$IFDEF WinCE}
{$INCLUDE wince/includes.inc}
{$ENDIF WinCE}

{$IFDEF UNIX}
{$INCLUDE x11/includes.inc}
{$ENDIF UNIX}

{$INCLUDE core/consolei.inc}

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

initialization
  {$IFDEF ENABLE_C_API}
  ptc_error_handler_function := @ptc_error_handler_default;
  {$ENDIF ENABLE_C_API}
  {$IF defined(WIN32) OR defined(WIN64)}
  TWin32Hook_Monitor := TWin32Monitor.Create;
  {$ENDIF defined(WIN32) OR defined(WIN64)}

finalization
  {$IF defined(WIN32) OR defined(WIN64)}
  FreeAndNil(TWin32Hook_Monitor);
  {$ENDIF defined(WIN32) OR defined(WIN64)}

end.
