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

{ Everything in here (C)1998 The Rasterman }

{ Rasterman's dither matrix }

const
  DitherMatrix_44: array [0..3, 0..3] of Uint8 = (
    (0, 4, 1, 5),
    (6, 2, 7, 3),
    (1, 5, 0, 4),
    (7, 3, 6, 2));

var
  DitherTab_r565_44: array [0..3, 0..3, 0..255] of Uint16;
  DitherTab_g565_44: array [0..3, 0..3, 0..255] of Uint16;
  DitherTab_b565_44: array [0..3, 0..3, 0..255] of Uint16;

  DitherTab_r332_44: array [0..3, 0..3, 0..255] of Uint8;
  DitherTab_g332_44: array [0..3, 0..3, 0..255] of Uint8;
  DitherTab_b332_44: array [0..3, 0..3, 0..255] of Uint8;

procedure Dither_SetupMatrices;
var
  i, x, y: LongInt;
begin
  for y := 0 to 3 do
    for x := 0 to 3 do
      for i := 0 to 255 do
      begin
        if (DitherMatrix_44[x, y] < (i and $7)) and (i < (256 - 8)) then
        begin
          DitherTab_r565_44[x, y, i] := ((i + 8) and $f8) shl 8;
          DitherTab_r332_44[x, y, i] := ((i + 8) and $e0);
        end
        else
        begin
          DitherTab_r565_44[x, y, i] := (i and $f8) shl 8;
          DitherTab_r332_44[x, y, i] := i and $e0;
        end;
        if (DitherMatrix_44[x, y] < ((i and $3) shl 1)) and (i < (256 - 4)) then
        begin
          DitherTab_g565_44[x, y, i] := (((i + 4) and $fc) shl 8) shr 5;
          DitherTab_g332_44[x, y, i] := ((i + 4) and $e0) shr 3;
        end
        else
        begin
          DitherTab_g565_44[x, y, i] := ((i and $fc) shl 8) shr 5;
          DitherTab_g332_44[x, y, i] := (i and $e0) shr 3;
        end;
        if (DitherMatrix_44[x, y] < (i and $7)) and (i < (256 - 8)) then
        begin
          DitherTab_b565_44[x, y, i] := (((i + 8) and $f8) shl 16) shr 19;
          DitherTab_b332_44[x, y, i] := ((i + 8) shr 6) and $3;
        end
        else
        begin
          DitherTab_b565_44[x, y, i] := ((i and $f8) shl 16) shr 19;
          DitherTab_b332_44[x, y, i] := (i shr 6) and $3;
        end;
      end;
end;
