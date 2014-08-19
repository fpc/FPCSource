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
    aasmdata,
    symdef,
    hlcg2ll;

  type
    thlcgcpu = class(thlcg2ll)
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    globtype,verbose,
    fmodule,
    aasmbase,aasmtai,aasmcpu,
    symconst,
    hlcgobj,
    cgbase, cgutils, cgobj, cpubase, cgcpu;


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
