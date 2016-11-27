{
    Copyright (c) 2014 Jonas Maebe

    Generate LLVM IR for math nodes

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
unit nllvmmat;

{$i fpcdefs.inc}

interface

uses
  symtype,
  node, nmat, ncgmat, ncghlmat, cgbase;

type
  tllvmmoddivnode = class(tcgmoddivnode)
    procedure pass_generate_code; override;
  end;

  Tllvmunaryminusnode = class(tcgunaryminusnode)
    procedure emit_float_sign_change(r: tregister; _size : tdef);override;
  end;

  tllvmnotnode = class(tcghlnotnode)
  end;

implementation

uses
  globtype, systems, constexp,
  cutils, verbose, globals,
  symconst, symdef,
  aasmbase, aasmllvm, aasmtai, aasmdata,
  defutil,
  procinfo,
  hlcgobj, pass_2,
  ncon,
  llvmbase,
  ncgutil, cgutils;

{*****************************************************************************
                               tllvmmoddivnode
*****************************************************************************}

procedure tllvmmoddivnode.pass_generate_code;
  var
    op: tllvmop;
    hl: tasmlabel;
    tmpovreg1,
    tmpovreg2: tregister;
    ovloc: tlocation;
  begin
    secondpass(left);
    secondpass(right);
    if is_signed(left.resultdef) then
      if nodetype=divn then
        op:=la_sdiv
      else
        op:=la_srem
    else if nodetype=divn then
      op:=la_udiv
    else
      op:=la_urem;
    hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);
    if right.location.loc<>LOC_CONSTANT then
      begin
        hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,resultdef,true);
        { in llvm, div-by-zero is undefined on all platforms -> need explicit
          check }
        current_asmdata.getjumplabel(hl);
        hlcg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,resultdef,OC_NE,0,right.location,hl);
        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_divbyzero',[],nil).resetiftemp;
        hlcg.a_label(current_asmdata.CurrAsmList,hl);
      end;
    if (cs_check_overflow in current_settings.localswitches) and
       is_signed(left.resultdef) and
       ((right.nodetype<>ordconstn) or
        (tordconstnode(right).value=-1)) then
      begin
        current_asmdata.getjumplabel(hl);
        location_reset(ovloc,LOC_REGISTER,OS_8);
        ovloc.register:=hlcg.getintregister(current_asmdata.CurrAsmList,llvmbool1type);
        if right.nodetype=ordconstn then
          current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_const(la_icmp,ovloc.register,OC_EQ,resultdef,left.location.register,low(int64)))
        else
          begin
            tmpovreg1:=hlcg.getintregister(current_asmdata.CurrAsmList,llvmbool1type);
            tmpovreg2:=hlcg.getintregister(current_asmdata.CurrAsmList,llvmbool1type);
            current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_const(la_icmp,tmpovreg1,OC_EQ,resultdef,left.location.register,low(int64)));
            current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_const(la_icmp,tmpovreg2,OC_EQ,resultdef,right.location.register,-1));
            hlcg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_AND,llvmbool1type,tmpovreg1,tmpovreg2,ovloc.register);
          end;
        hlcg.g_overflowCheck_loc(current_asmdata.CurrAsmList,location,resultdef,ovloc);
      end;
    location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
    location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
    if right.location.loc=LOC_CONSTANT then
      current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_reg_const(op,location.register,resultdef,left.location.register,right.location.value))
    else
      current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_reg_reg(op,location.register,resultdef,left.location.register,right.location.register))
  end;

{*****************************************************************************
                               Tllvmunaryminusnode
*****************************************************************************}

procedure Tllvmunaryminusnode.emit_float_sign_change(r: tregister; _size : tdef);
var
  minusonereg: tregister;
begin
  { multiply with -1 instead of subtracting from 0, because otherwise we -x
    won't turn into -0.0 if x was 0.0 (0.0 - 0.0 = 0.0, but -1.0 * 0.0 = -0.0 }
  if _size.typ<>floatdef then
    internalerror(2014012212);
  minusonereg:=hlcg.getfpuregister(current_asmdata.CurrAsmList,_size);
  case tfloatdef(_size).floattype of
    s32real,s64real:
      current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_fpconst_size(la_bitcast,minusonereg,_size,-1.0,_size));
    { comp and currency are handled as int64 at the llvm level }
    s64comp,
    s64currency:
      begin
        { sc80floattype instead of _size, see comment in thlcgllvm.a_loadfpu_ref_reg }
        _size:=sc80floattype;
        current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_const_size(la_sitofp,minusonereg,s64inttype,-1,_size));
      end;
{$ifdef cpuextended}
    s80real,sc80real:
      current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_fpconst80_size(la_bitcast,minusonereg,_size,-1.0,_size));
{$endif cpuextended}
    else
      internalerror(2016112701);
  end;
  current_asmdata.CurrAsmList.Concat(taillvm.op_reg_size_reg_reg(la_fmul,r,_size,minusonereg,r));
end;


begin
  cmoddivnode := tllvmmoddivnode;
(*
  cshlshrnode := tllvmshlshrnode;
*)
  cnotnode    := tllvmnotnode;
  cunaryminusnode := Tllvmunaryminusnode;
end.
