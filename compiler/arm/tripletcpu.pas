{
    Copyright (c) 2020 by Jonas Maebe

    Construct the cpu part of the triplet

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit tripletcpu;

{$i fpcdefs.inc}

interface

uses
  globtype;

function tripletcpustr(tripletstyle: ttripletstyle): ansistring;

implementation

uses
  globals, cutils, systems, cpuinfo;

function tripletcpustr(tripletstyle: ttripletstyle): ansistring;
  begin
    result:=lower(cputypestr[current_settings.cputype]);
    { llvm replaces the cpu name with thumb for when generating thumb code}
    if (tripletstyle=triplet_llvm) and
       (current_settings.instructionset=is_thumb) then
      result:='thumb'+copy(result,4,255);
    if target_info.endian=endian_big then
      result:=result+'be';
  end;


end.

