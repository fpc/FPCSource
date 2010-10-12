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
function ptc_surface_create(width, height: Integer; format: TPTC_FORMAT): TPTC_SURFACE;
procedure ptc_surface_destroy(obj: TPTC_SURFACE);

{ copy to surface }
procedure ptc_surface_copy(obj: TPTC_SURFACE; surface: TPTC_SURFACE);
procedure ptc_surface_copy_area(obj: TPTC_SURFACE; surface: TPTC_SURFACE; source, destination: TPTC_AREA);

{ memory access }
function ptc_surface_lock(obj: TPTC_SURFACE): Pointer;
procedure ptc_surface_unlock(obj: TPTC_SURFACE);

{ load pixels to surface }
procedure ptc_surface_load(obj: TPTC_SURFACE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE);
procedure ptc_surface_load_area(obj: TPTC_SURFACE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE; source, destination: TPTC_AREA);

{ save surface pixels }
procedure ptc_surface_save(obj: TPTC_SURFACE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE);
procedure ptc_surface_save_area(obj: TPTC_SURFACE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE; source, destination: TPTC_AREA);

{ clear surface }
procedure ptc_surface_clear(obj: TPTC_SURFACE);
procedure ptc_surface_clear_color(obj: TPTC_SURFACE; color: TPTC_COLOR);
procedure ptc_surface_clear_color_area(obj: TPTC_SURFACE; color: TPTC_COLOR; area: TPTC_AREA);

{ surface palette }
procedure ptc_surface_palette_set(obj: TPTC_SURFACE; palette: TPTC_PALETTE);
function ptc_surface_palette_get(obj: TPTC_SURFACE): TPTC_PALETTE;

{ surface clip area }
procedure ptc_surface_clip_set(obj: TPTC_SURFACE; area: TPTC_AREA);

{ data access }
function ptc_surface_width(obj: TPTC_SURFACE): Integer;
function ptc_surface_height(obj: TPTC_SURFACE): Integer;
function ptc_surface_pitch(obj: TPTC_SURFACE): Integer;
function ptc_surface_area(obj: TPTC_SURFACE): TPTC_AREA;
function ptc_surface_clip(obj: TPTC_SURFACE): TPTC_AREA;
function ptc_surface_format(obj: TPTC_SURFACE): TPTC_FORMAT;

{ surface option string }
function ptc_surface_option(obj: TPTC_SURFACE; _option: string): Boolean;
