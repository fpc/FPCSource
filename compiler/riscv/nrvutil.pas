{
    Copyright (c) 2024

    RISCV version of some node tree helper routines

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
unit nrvutil;

{$i fpcdefs.inc}

interface

  uses
    ngenutil;


  type
    trvnodeutils = class(tnodeutils)
      class procedure InsertObjectInfo; override;
    end;

implementation

  uses
    globtype,globals,
    systems,
    aasmdata,aasmtai;

  class procedure trvnodeutils.InsertObjectInfo;
    begin
      inherited InsertObjectInfo;
      if (target_info.system in systems_linux) and (cs_create_pic in current_settings.moduleswitches) then
        current_asmdata.asmlists[al_start].Concat(tai_directive.create(asd_option,'pic'));
    end;


begin
  cnodeutils:=trvnodeutils;
end.

