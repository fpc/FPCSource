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
function ptc_palette_create: TPTC_PALETTE;
{function ptc_palette_create_data(data: Pint32): TPTC_PALETTE;}
procedure ptc_palette_destroy(obj: TPTC_PALETTE);

{ memory access }
function ptc_palette_lock(obj: TPTC_PALETTE): Pint32;
procedure ptc_palette_unlock(obj: TPTC_PALETTE);

{ load palette data }
procedure ptc_palette_load(obj: TPTC_PALETTE; data: Pint32);

{ save palette data }
procedure ptc_palette_save(obj: TPTC_PALETTE; data: Pint32);

{ get palette data }
function ptc_palette_data(obj: TPTC_PALETTE): Pint32;

{ operators }
procedure ptc_palette_assign(obj, palette: TPTC_PALETTE);
function ptc_palette_equals(obj, palette: TPTC_PALETTE): Boolean;
