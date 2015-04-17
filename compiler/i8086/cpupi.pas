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
       ti8086procinfo = class(tcgprocinfo)
       private
         procedure insert_8087_fwaits(list : TAsmList);
       public
         constructor create(aparent:tprocinfo);override;
         procedure set_first_temp_offset;override;
         function calc_stackframe_size:longint;override;
         procedure generate_parameter_info;override;
         procedure postprocess_code;override;
       end;


  implementation

    uses
      cutils,
      systems,globals,globtype,
      aasmtai,aasmcpu,
      cgobj,tgobj,paramgr,
      cpubase,cpuinfo,
      cgutils,
      symconst;

    constructor ti8086procinfo.create(aparent:tprocinfo);
      begin
        inherited create(aparent);
        got:=NR_EBX;
      end;


    procedure ti8086procinfo.set_first_temp_offset;
      begin
        if paramanager.use_fixed_stack then
          begin
            if not(po_assembler in procdef.procoptions) and
               (tg.direction > 0) then
              tg.setfirsttemp(tg.direction*maxpushedparasize);
            if (tg.direction < 0) and
               not(po_nostackframe in procdef.procoptions) then
              { compensate for the return address and the "pushl %ebp" }
              tg.setalignmentmismatch(sizeof(pint)*2);
          end;
      end;


    function ti8086procinfo.calc_stackframe_size:longint;
      begin
        { ???:
          align to 4 bytes at least
          otherwise all those subl $2,%esp are meaningless PM }
        if target_info.stackalign<=2 then
          result:=Align(tg.direction*tg.lasttemp,min(current_settings.alignment.localalignmax,2))
        else
          { aligned during stack frame allocation, because also depends number
            of saved registers }
          result:=tg.direction*tg.lasttemp+maxpushedparasize;
      end;


    procedure ti8086procinfo.generate_parameter_info;
      begin
        inherited generate_parameter_info;
        { Para_stack_size is only used to determine how many bytes to remove }
        { from the stack at the end of the procedure (in the "ret $xx").     }
        { If the stack is fixed, nothing has to be removed by the callee     }
        if paramanager.use_fixed_stack then
          para_stack_size := 0;
      end;


    procedure ti8086procinfo.insert_8087_fwaits(list : TAsmList);
      var
        curtai: tai;
      begin
        curtai:=tai(list.First);
        while assigned(curtai) do
          begin
            if (curtai.typ=ait_instruction)
               and requires_fwait_on_8087(taicpu(curtai).opcode) then
              list.InsertBefore(taicpu.op_none(A_FWAIT),curtai);

            curtai:=tai(curtai.Next);
          end;
      end;


    procedure ti8086procinfo.postprocess_code;
      begin
        { nickysn note: I don't know if the 187 requires FWAIT before
          every instruction like the 8087, so I'm including it just in case }
        if current_settings.cputype<=cpu_186 then
          insert_8087_fwaits(aktproccode);
      end;

begin
   cprocinfo:=ti8086procinfo;
end.
