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

      procedure gen_load_loc_function_result(list: TAsmList; vardef: tdef; const l: tlocation);override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    globals, verbose, systems, cutils,
    fmodule,
    aasmtai, aasmcpu,
    defutil,
    hlcgobj,
    cpuinfo, cgobj, cpubase, cgcpu,
    parabase, procinfo;



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
      tcg68k(cg).fixref(list,sref.ref,false);

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
      tcg68k(cg).fixref(list,sref.ref,current_settings.cputype in cpu_coldfire);

      { memory accesses of bset/bclr are always byte, so no alignment problem }
      list.concat(taicpu.op_const_ref(bit_set_clr_instr[doset],S_NO,8-sref.startbit-1,sref.ref));
    end;


  procedure thlcgcpu.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);

    procedure putselfa0tostack(offs: longint);
      var
        href: treference;
      begin
        { move a0 which is self out of the way to the stack }
        reference_reset_base(href,voidpointertype,NR_STACK_POINTER_REG,offs,ctempposinvalid,4,[]);
        list.concat(taicpu.op_reg_ref(A_MOVE,S_L,NR_A0,href));
      end;

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
        reference_reset_base(href, voidstackpointertype, NR_SP,selfoffsetfromsp+offs,ctempposinvalid,4,[]);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_A0);
      end;

    procedure loadvmttoa0;
      var
        href : treference;
      begin
        { move.l  (%a0),%a0 ; load vmt}
        reference_reset_base(href, voidpointertype, NR_A0,0,ctempposinvalid,4,[]);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_A0);
      end;

    procedure op_onmethodaddrviastack(offs: longint);
      var
        href : treference;
        href2 : treference;
      begin
        if (procdef.extnumber=$ffff) then
          internalerror(2017061401);
        reference_reset_base(href,voidpointertype,NR_A0,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),ctempposinvalid,4,[]);

        { handle-too-large-for-68k offsets }
        { I'm not even sure this is handled elsewhere in the compiler for VMTs, but lets play safe... (KB) }
        if href.offset >= high(smallint) then
          begin
            list.concat(taicpu.op_const_reg(A_ADD,S_L,href.offset,NR_A0));
            href.offset:=0;
          end;

        { push the method address to the stack }
        reference_reset_base(href2,voidpointertype,NR_STACK_POINTER_REG,0,ctempposinvalid,4,[]);
        href2.direction:=dir_dec;
        list.concat(taicpu.op_ref_ref(A_MOVE,S_L,href,href2));

        { restore A0 from the stack }
        reference_reset_base(href2,voidpointertype,NR_STACK_POINTER_REG,offs+4,ctempposinvalid,4,[]); { offs+4, because we used dir_dec above }
        list.concat(taicpu.op_ref_reg(A_MOVE,S_L,href2,NR_A0));

        { pop the method address from the stack, and jump to it }
        list.concat(taicpu.op_none(A_RTS,S_NO));
      end;

    procedure op_ona0methodaddr;
      var
        href : treference;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(2013100701);
        reference_reset_base(href,voidpointertype,NR_A0,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),ctempposinvalid,4,[]);
        list.concat(taicpu.op_ref_reg(A_MOVE,S_L,href,NR_A0));
        reference_reset_base(href,voidpointertype,NR_A0,0,ctempposinvalid,4,[]);
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
        List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0,procdef))
      else
        List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0,procdef));

      { set param1 interface to self  }
      g_adjust_self_value(list,procdef,ioffset);

      { case 4 }
      if (po_virtualmethod in procdef.procoptions) and
          not is_objectpascal_helper(procdef.struct) then
        begin
          if (procdef.proccalloption in [pocall_register]) then
            begin
              putselfa0tostack(-8);
              loadvmttoa0;
              op_onmethodaddrviastack(-8);
            end
          else
            begin
              getselftoa0(4);
              loadvmttoa0;
              op_ona0methodaddr;
            end;
        end
      { case 0 }
      else
        list.concat(taicpu.op_sym(A_JMP,S_NO,current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION)));

      List.concat(Tai_symbol_end.Createname(labelname));
    end;


  procedure thlcgcpu.gen_load_loc_function_result(list: TAsmList; vardef: tdef; const l: tlocation);
    var
      cgpara: tcgpara;
    begin
      inherited;

      { Kludge:
        GCC (and SVR4 in general maybe?) requires a pointer
        result on the A0 register, as well as D0. So when we
        have a result in A0, also copy it to D0. See the decision
        making code in tcpuparamanager.get_funcretloc (KB) }
      cgpara:=current_procinfo.procdef.funcretloc[calleeside];
      if ((cgpara.location^.loc = LOC_REGISTER) and
          (isaddressregister(cgpara.location^.register))) then
        begin
          cg.a_load_reg_reg(list,OS_ADDR,OS_ADDR,NR_RETURN_ADDRESS_REG,NR_FUNCTION_RESULT_REG);
        end;
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;

begin
  chlcgobj:=thlcgcpu;
end.
