{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines to create a pass-through high-level code
    generator. This is used by most regular code generators.

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

unit hlcgx86;

interface

{$i fpcdefs.inc}

  uses
    globtype,
    aasmdata,
    cgbase,
    symtype,symdef,
    parabase,
    hlcgobj, hlcg2ll;

  type
    thlcgx86 = class(thlcg2ll)
     protected
      procedure gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara); override;
      procedure a_jmp_external_name(list: TAsmList; const externalname: TSymStr); override;
     public
      procedure a_load_undefined_cgpara(list: TAsmList; size: tdef; const cgpara: TCGPara); override;
      procedure a_bit_set_reg_reg(list: TAsmList; doset: boolean; bitnumbersize, destsize: tdef; bitnumber, dest: tregister); override;
    end;

implementation

  uses
    globals,systems,
    aasmbase,
    cgutils,
{$ifdef I8086}
    cpuinfo,
{$endif I8086}
    cpubase,aasmcpu;

{ thlcgx86 }

  procedure thlcgx86.gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara);
    begin
      { the caller will pop a value from the fpu stack }
      if assigned(resloc.location) and
         (resloc.location^.loc=LOC_FPUREGISTER) then
        list.concat(taicpu.op_none(A_FLDZ));
    end;


  procedure thlcgx86.a_jmp_external_name(list: TAsmList; const externalname: TSymStr);
    var
      ref : treference;
      sym : tasmsymbol;
    begin
     if (target_info.system = system_i386_darwin) then
       begin
         { a_jmp_name jumps to a stub which is always pic-safe on darwin }
         inherited;
         exit;
       end;

      sym:=current_asmdata.RefAsmSymbol(externalname,AT_FUNCTION);
      reference_reset_symbol(ref,sym,0,sizeof(pint),[]);

      { create pic'ed? }
      if (cs_create_pic in current_settings.moduleswitches) and
         { darwin/x86_64's assembler doesn't want @PLT after call symbols }
         not(target_info.system in [system_x86_64_darwin,system_i386_iphonesim,system_x86_64_iphonesim]) then
        ref.refaddr:=addr_pic
      else
        ref.refaddr:=addr_full;
      list.concat(taicpu.op_ref(A_JMP,S_NO,ref));
    end;


  procedure thlcgx86.a_load_undefined_cgpara(list: TAsmList; size: tdef; const cgpara: TCGPara);
    begin
      if not (cgpara.Location^.Loc in [LOC_REGISTER,LOC_CREGISTER]) and
        (cgpara.size=OS_ADDR) then
        a_load_reg_cgpara(list,size,NR_FRAME_POINTER_REG,cgpara)
      else
        inherited;
    end;

  procedure thlcgx86.a_bit_set_reg_reg(list: TAsmList; doset: boolean; bitnumbersize, destsize: tdef; bitnumber, dest: tregister);
    const
      bit_set_clr_instr: array[boolean] of tasmop = (A_BTR,A_BTS);
    begin
{$ifdef I8086}
      { BTR/BTS is only supportd by 80386 CPU or later }
      if not(CPUX86_HAS_BTX in cpu_capabilities[current_settings.optimizecputype]) then
	inherited
      else
{$endif I8086}
        list.concat(taicpu.op_reg_reg(bit_set_clr_instr[doset],S_NO,bitnumber,dest));
    end;


end.
