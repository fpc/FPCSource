{
    Copyright (c) 2002 by Florian Klaempfl

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
       psub,procinfo,aasmdata;

    type
       ti386procinfo = class(tcgprocinfo)
         constructor create(aparent:tprocinfo);override;
         procedure set_first_temp_offset;override;
         function calc_stackframe_size:longint;override;
         procedure generate_parameter_info;override;
         procedure allocate_got_register(list: tasmlist);override;
       end;


  implementation

    uses
      cutils,
      systems,globals,globtype,
      cgobj,tgobj,
      cpubase,
      cgutils,
      symconst;

    constructor ti386procinfo.create(aparent:tprocinfo);
      begin
        inherited create(aparent);
        got:=NR_EBX;
      end;


    procedure ti386procinfo.set_first_temp_offset;
      begin
        if use_fixed_stack then
          begin
            if not(po_assembler in procdef.procoptions) and
               (tg.direction > 0) then
              tg.setfirsttemp(tg.direction*maxpushedparasize);
          end;
      end;


    function ti386procinfo.calc_stackframe_size:longint;
      begin
        { align to 4 bytes at least
          otherwise all those subl $2,%esp are meaningless PM }
        if (target_info.system <> system_i386_darwin) then
          result:=Align(tg.direction*tg.lasttemp,min(current_settings.alignment.localalignmin,4))
        else
          result:=tg.direction*tg.lasttemp+maxpushedparasize;
      end;


    procedure ti386procinfo.generate_parameter_info;
      begin
        inherited generate_parameter_info;
        { Para_stack_size is only used to determine how many bytes to remove }
        { from the stack at the end of the procedure (in the "ret $xx").     }
        { If the stack is fixed, nothing has to be removed by the callee     }
        if use_fixed_stack then
          para_stack_size := 0;
      end;

    procedure ti386procinfo.allocate_got_register(list: tasmlist);
      begin
        if (target_info.system = system_i386_darwin) and
           (cs_create_pic in current_settings.moduleswitches) then
          begin
            got := cg.getaddressregister(list);
          end;
      end;

begin
   cprocinfo:=ti386procinfo;
end.
