{
    Copyright (c) 2002-2006 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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

{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;

{$i fpcdefs.inc}

interface

    uses
       psub,procinfo;

    type
       tx86_64procinfo = class(tcgprocinfo)
         procedure set_first_temp_offset;override;
         procedure generate_parameter_info;override;
         function calc_stackframe_size:longint;override;
       end;


implementation

    uses
      systems,
      globals,
      cutils,
      symconst,
      tgobj;


    procedure tx86_64procinfo.set_first_temp_offset;
      begin
        if target_info.system=system_x86_64_win64 then
          begin
            if not(po_assembler in procdef.procoptions) and
              (tg.direction > 0) then
              tg.setfirsttemp(tg.direction*maxpushedparasize+4*8);
          end
        else
          inherited;
      end;


    procedure tx86_64procinfo.generate_parameter_info;
      begin
        inherited generate_parameter_info;
        if target_info.system=system_x86_64_win64 then
          para_stack_size:=0;
      end;


    function tx86_64procinfo.calc_stackframe_size:longint;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(current_settings.alignment.localalignmin,16));
        { RSP should be aligned on 16 bytes }
        result:=Align(tg.direction*tg.lasttemp+maxpushedparasize,16);
        if target_info.system=system_x86_64_win64 then
          inc(result,4*8);
      end;


begin
   cprocinfo:=tx86_64procinfo;
end.
