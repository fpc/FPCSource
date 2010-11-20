{
    Free Pascal port of the Hermes C library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

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

{
   C converter main loops for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

procedure ConvertP(iface: PHermesConverterInterface); cdecl;
begin
  { Simply loop through all scanlines }
  repeat
    iface^.func(iface^.s_pixels, iface^.d_pixels, iface^.d_width, 1);

    Inc(iface^.s_pixels, iface^.s_pitch);
    Inc(iface^.d_pixels, iface^.d_pitch);
    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;

procedure ConvertPStretch(iface: PHermesConverterInterface); cdecl;
var
  dx, dy: DWord;
  y: DWord;
begin
  y := 0;
  dy := (iface^.s_height shl 16) div iface^.d_height;
  dx := (iface^.s_width shl 16) div iface^.d_width;

  { We have the increment of y and x on the source surface now let's start }
  repeat
    iface^.func(iface^.s_pixels, iface^.d_pixels, iface^.d_width, dx);

    Inc(iface^.d_pixels, iface^.d_pitch);

    Inc(y, dy);

    { Check how many lines we need to step forward }
    Inc(iface^.s_pixels, (y shr 16)*DWord(iface^.s_pitch));
    y := y and $ffff;
    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;
