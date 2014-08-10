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
  globtype, systems,
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
      hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,resultdef,true);
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
  zeroreg: tregister;
begin
  if _size.typ<>floatdef then
    internalerror(2014012212);
  zeroreg:=hlcg.getfpuregister(current_asmdata.CurrAsmList,_size);
  case tfloatdef(_size).floattype of
    s32real,s64real:
      current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_fpconst_size(la_bitcast,zeroreg,_size,0,_size));
    { comp and currency are handled as int64 at the llvm level }
    s64comp,
    s64currency:
      { sc80floattype instead of _size, see comment in thlcgllvm.a_loadfpu_ref_reg }
      current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_const_size(la_sitofp,zeroreg,s64inttype,0,sc80floattype));
{$ifdef cpuextended}
    s80real,sc80real:
      current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_fpconst80_size(la_bitcast,zeroreg,_size,0.0,_size));
{$endif cpuextended}
  end;
  current_asmdata.CurrAsmList.Concat(taillvm.op_reg_size_reg_reg(la_fsub,r,_size,zeroreg,r));
end;


begin
  cmoddivnode := tllvmmoddivnode;
(*
  cshlshrnode := tllvmshlshrnode;
*)
  cnotnode    := tllvmnotnode;
  cunaryminusnode := Tllvmunaryminusnode;
end.
