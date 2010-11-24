{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2010  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

{ setup }
function ptc_console_create: TPTC_CONSOLE;
procedure ptc_console_destroy(obj: TPTC_CONSOLE);

{ console configuration }
procedure ptc_console_configure(obj: TPTC_CONSOLE; _file: String);

{ console option string }
function ptc_console_option(obj: TPTC_CONSOLE; _option: String): Boolean;

{ console modes }
function ptc_console_mode(obj: TPTC_CONSOLE; index: Integer): TPTC_MODE;

{ console management }
procedure ptc_console_open(obj: TPTC_CONSOLE; title: string; pages: Integer);
procedure ptc_console_open_format(obj: TPTC_CONSOLE; title: string; format: TPTC_FORMAT; pages: Integer);
procedure ptc_console_open_resolution(obj: TPTC_CONSOLE; title: string; width, height: Integer; format: TPTC_FORMAT; pages: Integer);
procedure ptc_console_open_mode(obj: TPTC_CONSOLE; title: string; mode: TPTC_MODE; pages: Integer);
procedure ptc_console_close(obj: TPTC_CONSOLE);

{ synchronization }
procedure ptc_console_flush(obj: TPTC_CONSOLE);
procedure ptc_console_finish(obj: TPTC_CONSOLE);
procedure ptc_console_update(obj: TPTC_CONSOLE);
procedure ptc_console_update_area(obj: TPTC_CONSOLE; area: TPTC_AREA);

{ keyboard input }
function ptc_console_key(obj: TPTC_CONSOLE): Boolean;
procedure ptc_console_read(obj: TPTC_CONSOLE; key: TPTC_KEY);

{ copy to surface }
procedure ptc_console_copy(obj: TPTC_CONSOLE; surface: TPTC_SURFACE);
procedure ptc_console_copy_area(obj: TPTC_CONSOLE; surface: TPTC_SURFACE; source, destination: TPTC_AREA);

{ memory access }
function ptc_console_lock(obj: TPTC_CONSOLE): Pointer;
procedure ptc_console_unlock(obj: TPTC_CONSOLE);

{ load pixels to console }
procedure ptc_console_load(obj: TPTC_CONSOLE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE);
procedure ptc_console_load_area(obj: TPTC_CONSOLE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE; source, destination: TPTC_AREA);

{ save console pixels }
procedure ptc_console_save(obj: TPTC_CONSOLE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE);
procedure ptc_console_save_area(obj: TPTC_CONSOLE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE; source, destination: TPTC_AREA);

{ clear console }
procedure ptc_console_clear(obj: TPTC_CONSOLE);
procedure ptc_console_clear_color(obj: TPTC_CONSOLE; color: TPTC_COLOR);
procedure ptc_console_clear_color_area(obj: TPTC_CONSOLE; color: TPTC_COLOR; area: TPTC_AREA);

{ console palette }
procedure ptc_console_palette_set(obj: TPTC_CONSOLE; palette: TPTC_PALETTE);
function ptc_console_palette_get(obj: TPTC_CONSOLE): TPTC_PALETTE;

{ console clip area }
procedure ptc_console_clip_set(obj: TPTC_CONSOLE; area: TPTC_AREA);

{ data access }
function ptc_console_width(obj: TPTC_CONSOLE): Integer;
function ptc_console_height(obj: TPTC_CONSOLE): Integer;
function ptc_console_pages(obj: TPTC_CONSOLE): Integer;
function ptc_console_pitch(obj: TPTC_CONSOLE): Integer;
function ptc_console_area(obj: TPTC_CONSOLE): TPTC_AREA;
function ptc_console_clip(obj: TPTC_CONSOLE): TPTC_AREA;
function ptc_console_format(obj: TPTC_CONSOLE): TPTC_FORMAT;
function ptc_console_name(obj: TPTC_CONSOLE): string;
function ptc_console_title(obj: TPTC_CONSOLE): string;
function ptc_console_information(obj: TPTC_CONSOLE): string;

{ extension functions }
{#ifdef __PTC_WIN32_EXTENSIONS__
CAPI void PTCAPI ptc_console_open_window(PTC_CONSOLE object,HWND window,int pages);
CAPI void PTCAPI ptc_console_open_window_format(PTC_CONSOLE object,HWND window,PTC_FORMAT format,int pages);
CAPI void PTCAPI ptc_console_open_window_resolution(PTC_CONSOLE object,HWND window,int width,int height,PTC_FORMAT format,int pages);
CAPI void PTCAPI ptc_console_open_window_mode(PTC_CONSOLE object,HWND window,PTC_MODE mode,int pages);
CAPI HWND PTCAPI ptc_console_window(PTC_CONSOLE object);
CAPI LPDIRECTDRAW PTCAPI ptc_console_lpDD(PTC_CONSOLE object);
CAPI LPDIRECTDRAW2 PTCAPI ptc_console_lpDD2(PTC_CONSOLE object);
CAPI LPDIRECTDRAWSURFACE PTCAPI ptc_console_lpDDS(PTC_CONSOLE object);
CAPI LPDIRECTDRAWSURFACE PTCAPI ptc_console_lpDDS_primary(PTC_CONSOLE object);
CAPI LPDIRECTDRAWSURFACE PTCAPI ptc_console_lpDDS_secondary(PTC_CONSOLE object);
#endif}
