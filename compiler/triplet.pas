{
    Copyright (c) 2007-2008, 2013, 2019-2020 by Jonas Maebe

    This unit handles constructing target triples

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
unit triplet;

{$i fpcdefs.inc}

interface

uses
  globtype;

function targettriplet(tripletstyle: ttripletstyle): ansistring;

implementation

uses
  globals,systems,
  cpuinfo,tripletcpu;

  function targettriplet(tripletstyle: ttripletstyle): ansistring;
    begin
      { architecture }
      result:=tripletcpustr(tripletstyle);
      { vendor and/or OS }
      if target_info.system in systems_darwin then
        begin
          result:=result+'-apple';
          if target_info.system in systems_macosx then
            result:=result+'-macosx'+MacOSXVersionMin
          else
            result:=result+'-ios'+iPhoneOSVersionMin;
        end
      else if target_info.system in (systems_linux+systems_android) then
        result:=result+'-unknown-linux'
      else if target_info.system in systems_all_windows then
        begin
          { WinCE isn't supported (yet) by llvm, but if/when added this is
            presumably how they will differentiate it }
          if target_info.system in systems_windows then
            result:=result+'-pc';
          result:=result+'-windows-msvc19'
        end
      else if target_info.system in systems_freebsd then
        result:=result+'-unknown-freebsd'
      else if target_info.system in systems_openbsd then
        result:=result+'-unknown-openbsd'
      else if target_info.system in systems_netbsd then
        result:=result+'-unknown-netbsd'
      else if target_info.system in systems_solaris then
        result:=result+'-sun-solaris2'
      else if target_info.system in systems_aix then
        result:=result+'-ibm-aix53'
      else if target_info.system in [system_i386_haiku] then
        result:=result+'-unknown-haiku'
      else if target_info.system in systems_embedded then
        result:=result+'-none'
      else
        result:=result+'-unknown';

      { environment/ABI }
      if target_info.system in systems_android then
        result:=result+'-android'
      else
{$ifdef arm}
      if target_info.abi=abi_eabihf then
        result:=result+'-gnueabihf'
      else if target_info.system in systems_embedded then
        result:=result+'-eabi'
      else if target_info.abi=abi_eabi then
        result:=result+'-gnueabi'
      else
{$endif}
      if target_info.system in systems_embedded then
        result:=result+'-elf'
      else if target_info.system in systems_linux then
        result:=result+'-gnu';
    end;



end.

