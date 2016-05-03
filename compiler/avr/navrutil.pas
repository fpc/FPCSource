{
    Copyright (c) 2015 by Jeppe Johansen

    AVR version of some node tree helper routines

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
unit navrutil;

{$i fpcdefs.inc}

interface

  uses
    node,nbas,
    ngenutil,
    symtype,symconst,symsym,symdef;


  type
    tavrnodeutils = class(tnodeutils)
      class procedure InsertInitFinalTable; override;
    end;

implementation

    uses
      verbose,cutils,globtype,globals,constexp,fmodule,
      cclasses,
      aasmdata,aasmtai,aasmcpu,aasmcnst,aasmbase,
      cpubase,
      symbase,symcpu,symtable,defutil,
      ncnv,ncon,ninl,ncal,nld,nmem,
      systems,
      CPUInfo,
      ppu,
      pass_1;


  procedure AddToStructInits(p:TObject;arg:pointer);
    var
      StructList: TFPList absolute arg;
    begin
      if (tdef(p).typ in [objectdef,recorddef]) and
         not (df_generic in tdef(p).defoptions) then
        begin
          { first add the class... }
          if ([oo_has_class_constructor,oo_has_class_destructor] * tabstractrecorddef(p).objectoptions <> []) then
            StructList.Add(p);
          { ... and then also add all subclasses }
          tabstractrecorddef(p).symtable.deflist.foreachcall(@AddToStructInits,arg);
        end;
    end;


  class procedure tavrnodeutils.InsertInitFinalTable;
    var
      hp : tused_unit;
      op: TAsmOp;
      initCount, finalCount: longint;

      procedure write_struct_inits(InitList, FinalizeList: TAsmList; u: tmodule);
        var
          i: integer;
          structlist: TFPList;
          pd: tprocdef;
        begin
          structlist := TFPList.Create;
          if assigned(u.globalsymtable) then
            u.globalsymtable.DefList.ForEachCall(@AddToStructInits,structlist);
          u.localsymtable.DefList.ForEachCall(@AddToStructInits,structlist);
          { write structures }
          for i:=0 to structlist.Count-1 do
          begin
            pd:=tabstractrecorddef(structlist[i]).find_procdef_bytype(potype_class_constructor);
            if assigned(pd) then
              begin
                InitList.Concat(taicpu.op_sym(op,current_asmdata.RefAsmSymbol(pd.mangledname)));
                inc(initCount);
              end;

            pd := tabstractrecorddef(structlist[i]).find_procdef_bytype(potype_class_destructor);
            if assigned(pd) then
              begin
                FinalizeList.Concat(taicpu.op_sym(op,current_asmdata.RefAsmSymbol(pd.mangledname)));
                inc(finalCount);
              end;
          end;
          structlist.free;
        end;

    var
      initList, finalList, header: TAsmList;
    begin
      initList:=TAsmList.create;
      finalList:=TAsmList.create;

      initCount:=0;
      finalCount:=0;

      if CPUAVR_HAS_JMP_CALL in cpu_capabilities[current_settings.cputype] then
        op:=A_CALL
      else
        op:=A_RCALL;

      hp:=tused_unit(usedunits.first);
      while assigned(hp) do
        begin
          if (hp.u.flags and uf_classinits) <> 0 then
            write_struct_inits(initList, finalList, hp.u);

          if (hp.u.flags and (uf_init or uf_finalize))<>0 then
            begin
              if (hp.u.flags and uf_init)<>0 then
                begin
                  initList.Concat(taicpu.op_sym(op,current_asmdata.RefAsmSymbol(make_mangledname('INIT$',hp.u.globalsymtable,''))));
                  inc(initCount);
                end;

              if (hp.u.flags and uf_finalize)<>0 then
                begin
                  finalList.Concat(taicpu.op_sym(op,current_asmdata.RefAsmSymbol(make_mangledname('FINALIZE$',hp.u.globalsymtable,''))));
                  inc(finalCount);
                end;
            end;

          hp:=tused_unit(hp.next);
        end;

      { insert class constructors/destructor of the program }
      if (current_module.flags and uf_classinits) <> 0 then
        write_struct_inits(initList, finalList, current_module);

      { Insert initialization/finalization of the program }
      if (current_module.flags and (uf_init or uf_finalize))<>0 then
        begin
          if (current_module.flags and uf_init)<>0 then
            begin
              initList.Concat(taicpu.op_sym(op,current_asmdata.RefAsmSymbol(make_mangledname('INIT$',current_module.localsymtable,''))));
              inc(initCount);
            end;

          if (current_module.flags and uf_finalize)<>0 then
            begin
              finalList.Concat(taicpu.op_sym(op,current_asmdata.RefAsmSymbol(make_mangledname('FINALIZE$',current_module.localsymtable,''))));
              inc(finalCount);
            end;
        end;

      initList.Concat(taicpu.op_none(A_RET));
      finalList.Concat(taicpu.op_none(A_RET));

      begin
        header:=TAsmList.create;
        new_section(header, sec_code, 'FPC_INIT_FUNC_TABLE', 1);
        header.concat(tai_symbol.Createname_global('FPC_INIT_FUNC_TABLE',AT_FUNCTION,0));

        initList.insertList(header);
        header.free;

        current_asmdata.AsmLists[al_procedures].concatList(initList);
      end;

      begin
        header:=TAsmList.create;
        new_section(header, sec_code, 'FPC_FINALIZE_FUNC_TABLE', 1);
        header.concat(tai_symbol.Createname_global('FPC_FINALIZE_FUNC_TABLE',AT_FUNCTION,0));

        finalList.insertList(header);
        header.free;

        current_asmdata.AsmLists[al_procedures].concatList(finalList);
      end;

      initList.Free;
      finalList.Free;

      inherited InsertInitFinalTable;
    end;

begin
  cnodeutils:=tavrnodeutils;
end.

