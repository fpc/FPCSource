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

{procedure Hermes_Calculate_Generic_Info(s_r, s_g, s_b, s_a,
                                        d_r, d_g, d_b, d_a: Uint32;
                                        info: PHermesGenericInfo);
function Hermes_Topbit(mask: Uint32): Integer;}

procedure Hermes_Calculate_Generic_Info(s_r, s_g, s_b, s_a,
                                        d_r, d_g, d_b, d_a: Integer;
                                        info: PHermesGenericInfo);
var
  r_right, g_right, b_right, a_right: Integer;
begin
  {Calculate right shift amount for red. if it's <0, then set it to 0
   and calculate left shift amount}
  r_right := s_r - d_r;
  if r_right < 0 then
  begin
    info^.r_left := -r_right;
    info^.r_right := 0;
  end
  else
  begin
    info^.r_left := 0;
    info^.r_right := r_right;
  end;

  {Same for green}
  g_right := s_g - d_g;
  if g_right < 0 then
  begin
    info^.g_left := -g_right;
    info^.g_right := 0;
  end
  else
  begin
    info^.g_left := 0;
    info^.g_right := g_right;
  end;

  {Same for blue}
  b_right := s_b - d_b;
  if b_right < 0 then
  begin
    info^.b_left := -b_right;
    info^.b_right := 0;
  end
  else
  begin
    info^.b_left := 0;
    info^.b_right := b_right;
  end;

  {Alpha}
  a_right := s_a - d_a;
  if a_right < 0 then
  begin
    info^.a_left := -a_right;
    info^.a_right := 0;
  end
  else
  begin
    info^.a_left := 0;
    info^.a_right := a_right;
  end;
end;

function Hermes_Topbit(mask: Uint32): Integer;
var
  i: Integer;
begin
  if mask = 0 then
  begin
    Result := 0;
    exit;
  end;
  i := 0;
  while (mask and 1) = 0 do
  begin
    mask := mask shr 1;
    Inc(i);
  end;
  while (mask and 1) = 1 do
  begin
    mask := mask shr 1;
    Inc(i);
  end;
  Result := i;
end;
