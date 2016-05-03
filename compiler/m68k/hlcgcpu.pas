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

unit hlcgcpu;

{$i fpcdefs.inc}

interface


  uses
    globtype,
    aasmbase, aasmdata,
    cgbase, cgutils,
    symconst,symtype,symdef,
    hlcg2ll;

  type
    thlcgcpu = class(thlcg2ll)
      procedure a_bit_set_reg_reg(list: TAsmList; doset: boolean; bitnumbersize, destsize: tdef; bitnumber, dest: tregister); override;
      procedure a_bit_set_const_reg(list: TAsmList; doset: boolean; destsize: tdef; bitnumber: tcgint; destreg: tregister); override;
      procedure a_bit_set_reg_ref(list: TAsmList; doset: boolean; fromsize, tosize: tdef; bitnumber: tregister; const ref: treference); override;
      procedure a_bit_set_const_ref(list: TAsmList; doset: boolean; destsize: tdef; bitnumber: tcgint; const ref: treference); override;
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    globals, verbose, systems, cutils,
    fmodule,
    aasmtai, aasmcpu,
    defutil,
    hlcgobj,
    cpuinfo, cgobj, cpubase, cgcpu;



  const
    bit_set_clr_instr: array[boolean] of tasmop = (A_BCLR,A_BSET);

  procedure thlcgcpu.a_bit_set_reg_reg(list: TAsmList; doset: boolean; bitnumbersize, destsize: tdef; bitnumber, dest: tregister);
    var
      tmpvalue: tregister;
    begin
      tmpvalue:=getintregister(list,ptruinttype);
      //list.concat(tai_comment.create(strpnew('a_bit_set_reg_reg: called!')));
      a_load_const_reg(list,ptruinttype,destsize.size*8-1,tmpvalue);
      a_op_reg_reg(list,OP_SUB,bitnumbersize,bitnumber,tmpvalue);
      list.concat(taicpu.op_reg_reg(bit_set_clr_instr[doset],S_NO,tmpvalue,dest));
    end;


  procedure thlcgcpu.a_bit_set_const_reg(list: TAsmList; doset: boolean; destsize: tdef; bitnumber: tcgint; destreg: tregister);
    begin
      //list.concat(tai_comment.create(strpnew('a_bit_set_const_reg: called!')));
      list.concat(taicpu.op_const_reg(bit_set_clr_instr[doset],S_NO,(destsize.size*8)-bitnumber-1,destreg));
    end;


  procedure thlcgcpu.a_bit_set_reg_ref(list: TAsmList; doset: boolean; fromsize, tosize: tdef; bitnumber: tregister; const ref: treference);
    var
      tmpvalue: tregister;
      sref: tsubsetreference;
    begin
      //list.concat(tai_comment.create(strpnew('a_bit_set_reg_ref: called!')));
      sref:=get_bit_reg_ref_sref(list,fromsize,tosize,bitnumber,ref);
      tcg68k(cg).fixref(list,sref.ref);

      tmpvalue:=getintregister(list,ptruinttype);
      a_load_const_reg(list,ptruinttype,7,tmpvalue);
      a_op_reg_reg(list,OP_SUB,fromsize,sref.bitindexreg,tmpvalue);

      { memory accesses of bset/bclr are always byte, so no alignment problem }
      list.concat(taicpu.op_reg_ref(bit_set_clr_instr[doset],S_NO,tmpvalue,sref.ref));
    end;


  procedure thlcgcpu.a_bit_set_const_ref(list: TAsmList; doset: boolean; destsize: tdef; bitnumber: tcgint; const ref: treference);
    var
      sref: tsubsetreference;
    begin
      //list.concat(tai_comment.create(strpnew('a_bit_set_const_ref: called!')));
      sref:=get_bit_const_ref_sref(bitnumber,destsize,ref);
      tcg68k(cg).fixref(list,sref.ref);

      { memory accesses of bset/bclr are always byte, so no alignment problem }
      list.concat(taicpu.op_const_ref(bit_set_clr_instr[doset],S_NO,8-sref.startbit-1,sref.ref));
    end;


  procedure thlcgcpu.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);

    procedure getselftoa0(offs:longint);
      var
        href : treference;
        selfoffsetfromsp : longint;
      begin
        { move.l offset(%sp),%a0 }

        { framepointer is pushed for nested procs }
        if procdef.parast.symtablelevel>normal_function_level then
          selfoffsetfromsp:=sizeof(aint)
        else
          selfoffsetfromsp:=0;
        reference_reset_base(href, voidstackpointertype, NR_SP,selfoffsetfromsp+offs,4);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_A0);
      end;

    procedure loadvmttoa0;
      var
        href : treference;
      begin
        { move.l  (%a0),%a0 ; load vmt}
        reference_reset_base(href, voidpointertype, NR_A0,0,4);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_A0);
      end;

    procedure op_ona0methodaddr;
      var
        href : treference;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(2013100701);
        reference_reset_base(href,voidpointertype,NR_A0,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),4);
        list.concat(taicpu.op_ref_reg(A_MOVE,S_L,href,NR_A0));
        reference_reset_base(href,voidpointertype,NR_A0,0,4);
        list.concat(taicpu.op_ref(A_JMP,S_NO,href));
      end;

    var
      make_global : boolean;
    begin
      if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
        Internalerror(200006137);
      if not assigned(procdef.struct) or
         (procdef.procoptions*[po_classmethod, po_staticmethod,
           po_methodpointer, po_interrupt, po_iocheck]<>[]) then
        Internalerror(200006138);
      if procdef.owner.symtabletype<>ObjectSymtable then
        Internalerror(200109191);

      make_global:=false;
      if (not current_module.is_unit) or
         create_smartlink or
         (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
        make_global:=true;

      if make_global then
        List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
      else
        List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

      { set param1 interface to self  }
      g_adjust_self_value(list,procdef,ioffset);

      { case 4 }
      if (po_virtualmethod in procdef.procoptions) and
          not is_objectpascal_helper(procdef.struct) then
        begin
          getselftoa0(4);
          loadvmttoa0;
          op_ona0methodaddr;
        end
      { case 0 }
      else
        list.concat(taicpu.op_sym(A_JMP,S_NO,current_asmdata.RefAsmSymbol(procdef.mangledname)));

      List.concat(Tai_symbol_end.Createname(labelname));
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;

begin
  chlcgobj:=thlcgcpu;
end.
