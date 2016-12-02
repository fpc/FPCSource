{
    Copyright (c) 20011 by Jonas Maebe

    LLVM version of some node tree helper routines

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
unit nllvmutil;

{$i fpcdefs.inc}

interface

  uses
    globtype,cclasses,
    aasmdata,ngenutil,
    symtype,symconst,symsym,symdef;


  type
    tllvmnodeutils = class(tnodeutils)
     strict protected
      class procedure insertbsssym(list: tasmlist; sym: tstaticvarsym; size: asizeint; varalign: shortint); override;
      class procedure InsertUsedList(var usedsyms: tfpobjectlist; const usedsymsname: TSymstr);
     public
      class procedure InsertObjectInfo; override;
    end;


implementation

    uses
      verbose,cutils,globals,fmodule,systems,
      aasmbase,aasmtai,cpubase,llvmbase,aasmllvm,
      aasmcnst,nllvmtcon,
      symbase,symtable,defutil,
      llvmtype;

  class procedure tllvmnodeutils.insertbsssym(list: tasmlist; sym: tstaticvarsym; size: asizeint; varalign: shortint);
    var
      asmsym: tasmsymbol;
      field1, field2: tsym;
      tcb: ttai_typedconstbuilder;
    begin
      if sym.globalasmsym then
        asmsym:=current_asmdata.DefineAsmSymbol(sym.mangledname,AB_GLOBAL,AT_DATA,sym.vardef)
      else
        asmsym:=current_asmdata.DefineAsmSymbol(sym.mangledname,AB_LOCAL,AT_DATA,sym.vardef);
      if not(vo_is_thread_var in sym.varoptions) then
        list.concat(taillvmdecl.createdef(asmsym,sym.vardef,nil,sec_data,varalign))
      else if tf_section_threadvars in target_info.flags then
        list.concat(taillvmdecl.createtls(asmsym,sym.vardef,varalign))
      else
        list.concat(taillvmdecl.createdef(asmsym,
          get_threadvar_record(sym.vardef,field1,field2),
          nil,sec_data,varalign));
    end;


  class procedure tllvmnodeutils.InsertUsedList(var usedsyms: tfpobjectlist; const usedsymsname: TSymstr);
    var
      useddef: tdef;
      tcb: ttai_typedconstbuilder;
      decl: taillvmdecl;
      i: longint;
    begin
      if usedsyms.count<>0 then
        begin
          tcb:=ctai_typedconstbuilder.create([tcalo_new_section]);
          tllvmtai_typedconstbuilder(tcb).appendingdef:=true;
          useddef:=carraydef.getreusable(voidpointertype,usedsyms.count);
          tcb.maybe_begin_aggregate(useddef);
          for i:=0 to usedsyms.count-1 do
            begin
              decl:=taillvmdecl(usedsyms[i]);
              tcb.queue_init(voidpointertype);
              tcb.queue_emit_asmsym(decl.namesym,decl.def);
            end;
          tcb.maybe_end_aggregate(useddef);
          current_asmdata.AsmLists[al_globals].concatlist(
            tcb.get_final_asmlist(
              current_asmdata.DefineAsmSymbol(
                usedsymsname,AB_GLOBAL,AT_DATA,useddef),useddef,sec_user,
                'llvm.metadata',0
            )
          );
          tcb.free;
        end;
      usedsyms.free;
      usedsyms:=nil;
    end;


  class procedure tllvmnodeutils.InsertObjectInfo;
    begin
      inherited;

      { add the llvm.compiler.used array }
      InsertUsedList(current_module.llvmcompilerusedsyms,'llvm.compiler.used');
      { add the llvm.used array }
      InsertUsedList(current_module.llvmusedsyms,'llvm.used');

      { add "type xx = .." statements for all used recorddefs }
      with TLLVMTypeInfo.Create do
        begin
          inserttypeinfo;
          free;
        end;
    end;


begin
  cnodeutils:=tllvmnodeutils;
end.

