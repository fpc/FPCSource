{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the i386 specific class for the register
    allocator

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

unit rgcpu;

{$i fpcdefs.inc}

  interface

    uses
      cpubase,
      cpuinfo,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      cclasses,globtype,cgbase,cgutils,rgobj,rgx86;

    type

       { trgcpu }

       trgcpu = class(trgx86)
          function  do_spill_replace(list:TAsmList;instr:taicpu;orgreg:tsuperregister;const spilltemp:treference):boolean;override;
          procedure add_constraints(reg:Tregister);override;
       end;

       trgintcpu = class(trgcpu)
         procedure add_cpu_interferences(p : tai);override;
       end;


implementation

    uses
      systems,
      verbose;

    const
       { This value is used in tsaved. If the array value is equal
         to this, then this means that this register is not used.}
       reg_not_saved = $7fffffff;

{************************************************************************
                                 trgcpu
*************************************************************************}

    function trgcpu.do_spill_replace(list: TAsmList; instr: taicpu; orgreg: tsuperregister; const spilltemp: treference): boolean;
      var
        spilltemp2: treference;
      begin
        spilltemp2:=spilltemp;
        if spilltemp2.segment=NR_SS then
          spilltemp2.segment:=NR_NO;
        Result:=inherited do_spill_replace(list, instr, orgreg, spilltemp2);
      end;


    procedure trgcpu.add_constraints(reg:Tregister);
      var
        supreg : tsuperregister;
      begin
        if getsubreg(reg) in [R_SUBL,R_SUBH] then
          begin
            { Some registers have no 8-bit subregister }
            supreg:=getsupreg(reg);
            add_edge(supreg,RS_SI);
            add_edge(supreg,RS_DI);
            add_edge(supreg,RS_BP);
          end;
      end;


    procedure trgintcpu.add_cpu_interferences(p : tai);
      var
        href : treference;
        i : integer;
      begin
        if p.typ=ait_instruction then
          begin
            for i:=0 to taicpu(p).ops-1 do
              begin
                if taicpu(p).oper[i]^.typ=top_ref then
                  begin
                    href:=taicpu(p).oper[i]^.ref^;
                    { in case there's exactly one register used, we can treat it
                      as either base or index and choose it from the larger set
                      of registers [BX, BP, SI, DI] }
                    if (href.base<>NR_NO) xor (href.index<>NR_NO) then
                      begin
                        if (href.base<>NR_NO) and (getsupreg(href.base)>=first_int_imreg) then
                          begin
                            add_edge(getsupreg(href.base),RS_AX);
                            add_edge(getsupreg(href.base),RS_CX);
                            add_edge(getsupreg(href.base),RS_DX);
                          end;
                        if (href.index<>NR_NO) and (getsupreg(href.index)>=first_int_imreg) then
                          begin
                            add_edge(getsupreg(href.index),RS_AX);
                            add_edge(getsupreg(href.index),RS_CX);
                            add_edge(getsupreg(href.index),RS_DX);
                          end;
                      end
                    else
                      begin
                        { base is chosen from the set [BX, BP] }
                        if (href.base<>NR_NO) and (getsupreg(href.base)>=first_int_imreg) then
                          begin
                            add_edge(getsupreg(href.base),RS_AX);
                            add_edge(getsupreg(href.base),RS_CX);
                            add_edge(getsupreg(href.base),RS_DX);
                            add_edge(getsupreg(href.base),RS_SI);
                            add_edge(getsupreg(href.base),RS_DI);
                          end;
                        { index is chosen from the set [SI, DI] }
                        if (href.index<>NR_NO) and (getsupreg(href.index)>=first_int_imreg) then
                          begin
                            add_edge(getsupreg(href.index),RS_AX);
                            add_edge(getsupreg(href.index),RS_BX);
                            add_edge(getsupreg(href.index),RS_CX);
                            add_edge(getsupreg(href.index),RS_DX);
                            add_edge(getsupreg(href.index),RS_BP);
                          end;
                      end;
                  end;
              end;
          end;
      end;

end.
