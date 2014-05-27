{
    Copyright (c) 2014 by Nikolay Nikolov

    i8086 version of some node tree helper routines

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
unit n8086util;

{$i fpcdefs.inc}

interface

  uses
    ngenutil;


  type
    ti8086nodeutils = class(tnodeutils)
      class procedure InsertMemorySizes; override;
    end;


implementation

  uses
    globals,cpuinfo,
    aasmbase,aasmdata,aasmtai;


  class procedure ti8086nodeutils.InsertMemorySizes;
    begin
      inherited;
      if current_settings.x86memorymodel in x86_far_data_models then
        begin
          new_section(current_asmdata.asmlists[al_globals],sec_stack,'__stack', 16);
          current_asmdata.asmlists[al_globals].concat(tai_datablock.Create('___stack', stacksize));
        end;
    end;


begin
  cnodeutils:=ti8086nodeutils;
end.
