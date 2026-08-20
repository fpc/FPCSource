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
  globtype, globals;

function tripletcpustr(globals: TReadOnlyCompilerGlobals; tripletstyle: ttripletstyle): ansistring;

implementation

uses
  systemstypes, systems, cutils, cpuinfo;

function tripletcpustr(globals: TReadOnlyCompilerGlobals; tripletstyle: ttripletstyle): ansistring;
  begin
    if tripletstyle=triplet_llvmrt then
      begin
        if (globals.target.info.abi=abi_eabihf) and
           not(globals.target.info.system in systems_windows) then
          result:='armhf'
        else
          result:='arm';
        exit;
      end;
    result:=lower(cputypestr[globals.current_settings.cputype]);
    { llvm replaces the cpu name with thumb for when generating thumb code}
    if (tripletstyle=triplet_llvm) and
       (globals.current_settings.instructionset=is_thumb) then
      result:='thumb'+copy(result,4,255);
    if globals.target.info.endian=endian_big then
      result:=result+'be';
  end;


end.

