{
    Copyright (c) 2024 by Kirill Kranz

    This unit contains the CPU specific part of inserting a NOP on a Read After Write dependency

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
unit cpuext;

{$i fpcdefs.inc}

interface
uses
    cutils,
    globtype, symdef,
    procinfo, cpuinfo, cpupara,
    psub, aasmdata, cgutils, aasmtai; 


    procedure resolveReadAfterWrite(list: TAsmList);


implementation
uses
    systems, globals, verbose, sysutils, cclasses,
    cpubase, cgbase, cgobj,
    tgobj, paramgr, symconst, symcpu, aasmcpu;


procedure resolveReadAfterWrite(list: TAsmList);
label skip;
var 
    p, pp : tai;
    l, x : TLinkedListItem;
    firstReg : tregister;    

begin

	l:= list.first;
    while assigned(l) do begin
			
		p:= tai(l);
        if p.typ = ait_instruction then begin

			if (taicpu(p).opcode = A_LB) or (taicpu(p).opcode = A_LBU) or
               (taicpu(p).opcode = A_LH) or (taicpu(p).opcode = A_LHU) or
               (taicpu(p).opcode = A_LW) or (taicpu(p).opcode = A_LWU) or
               (taicpu(p).opcode = A_LWL) or (taicpu(p).opcode = A_LWR) or
               (taicpu(p).opcode = A_MFC0) {MFC2} {LWC2} then begin

                firstReg:= taicpu(p).oper[0]^.reg;

				x:= l.next;
				pp:= tai(x);
				
				while pp.typ <> ait_instruction do begin
					x:= x.next;
					pp:= tai(x);
				end;

				if pp.typ = ait_instruction then begin

                   if (taicpu(p).opcode = A_LWL) and (taicpu(pp).opcode = A_LWR) then goto skip;
                   if (taicpu(p).opcode = A_LWR) and (taicpu(pp).opcode = A_LWL) then goto skip;

					if taicpu(pp).ops > 0 then begin

                        if taicpu(pp).ops = 1 then
                            if (taicpu(pp).oper[0]^.typ = top_reg) and (firstReg = taicpu(pp).oper[0]^.reg) then
                                list.insertAfter(taicpu.op_none(A_NOP), l);

						if taicpu(pp).ops = 2 then
                            if ((taicpu(pp).oper[0]^.typ = top_reg) and (firstReg = taicpu(pp).oper[0]^.reg)) 
                               or ((taicpu(pp).oper[1]^.typ = top_reg) and (firstReg = taicpu(pp).oper[1]^.reg))
                               or ((taicpu(pp).oper[1]^.typ = top_ref) and (firstReg = taicpu(pp).oper[1]^.ref^.base))
                               then
								    list.insertAfter(taicpu.op_none(A_NOP), l);

						if taicpu(pp).ops = 3 then
                            if ((taicpu(pp).oper[0]^.typ = top_reg) and (firstReg = taicpu(pp).oper[0]^.reg)) or 
                               ((taicpu(pp).oper[1]^.typ = top_reg) and (firstReg = taicpu(pp).oper[1]^.reg)) or
							   ((taicpu(pp).oper[2]^.typ = top_reg) and (firstReg = taicpu(pp).oper[2]^.reg))
                               then
								    list.insertAfter(taicpu.op_none(A_NOP), l);


					end;

				end;

			 end;

        end;
skip:
     	l:= l.next;

    end;

end;


begin
end.