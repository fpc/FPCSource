{
    Copyright (c) 1998-2020 by the Free Pascal team

    This unit implements the llvm-mc ("llvm machine code playground")
    assembler writer for WebAssembly

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

unit agllvmmc;

{$i fpcdefs.inc}

interface

  uses
    systems,cgutils,
    globtype,globals,
    symbase,symdef,symtype,symconst,symcpu,
    aasmbase,aasmtai,aasmdata,aasmcpu,
    assemble,aggas;

  type

    { TLLVMMachineCodePlaygroundAssembler }

    TLLVMMachineCodePlaygroundAssembler=class(TGNUassembler)
    protected
      procedure WriteProcDef(pd: tprocdef);
      procedure WriteSymtableProcdefs(st: TSymtable);
      procedure WriteImports;

      function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
    public
      constructor CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean); override;
      procedure WriteAsmList;override;
    end;

    { TWASM32InstrWriter }

    TWASM32InstrWriter = class(TCPUInstrWriter)
      procedure WriteInstruction(hp : tai);override;
    end;

implementation

  uses
    cutils,
    fmodule,finput,
    itcpugas,
    cpubase,
    hlcgobj,hlcgcpu,
    verbose;

  { TLLVMMachineCodePlaygroundAssembler }


  procedure TLLVMMachineCodePlaygroundAssembler.WriteProcDef(pd: tprocdef);
    begin
      if not assigned(tcpuprocdef(pd).exprasmlist) and
         not(po_abstractmethod in pd.procoptions) and
         (pd.proctypeoption in [potype_unitinit,potype_unitfinalize]) then
        exit;

      writer.AsmWriteLn(asminfo^.comment+'WriteProcDef('+pd.mangledname+')');
      WriteTree(tcpuprocdef(pd).exprasmlist);
      writer.AsmWriteLn(asminfo^.comment+'WriteProcDef('+pd.mangledname+') done');
    end;


  procedure TLLVMMachineCodePlaygroundAssembler.WriteSymtableProcdefs(st: TSymtable);
    var
      i   : longint;
      def : tdef;
    begin
      if not assigned(st) then
        exit;
      for i:=0 to st.DefList.Count-1 do
        begin
          def:=tdef(st.DefList[i]);
          case def.typ of
            procdef :
              begin
                { methods are also in the static/globalsymtable of the unit
                  -> make sure they are only written for the objectdefs that
                  own them }
                if (not(st.symtabletype in [staticsymtable,globalsymtable]) or
                    (def.owner=st)) and
                   not(df_generic in def.defoptions) then
                  begin
                    WriteProcDef(tprocdef(def));
                    if assigned(tprocdef(def).localst) then
                      WriteSymtableProcdefs(tprocdef(def).localst);
                  end;
              end;
            else
              ;
          end;
        end;
    end;


  procedure TLLVMMachineCodePlaygroundAssembler.WriteImports;
    var
      i    : integer;
      proc : tprocdef;
      list : TAsmList;
      cur_unit: tused_unit;
    begin
      for i:=0 to current_module.deflist.Count-1 do
        if tdef(current_module.deflist[i]).typ = procdef then
          begin
            proc := tprocdef(current_module.deflist[i]);
            if (po_external in proc.procoptions) and assigned(proc.import_dll) then
              begin
                //WriteProcDef(proc);
                list:=TAsmList.Create;
                thlcgwasm(hlcg).g_procdef(list,proc);
                WriteTree(list);
                list.free;
                writer.AsmWrite(#9'.import_module'#9);
                writer.AsmWrite(proc.mangledname);
                writer.AsmWrite(', ');
                writer.AsmWriteLn(proc.import_dll^);
                writer.AsmWrite(#9'.import_name'#9);
                writer.AsmWrite(proc.mangledname);
                writer.AsmWrite(', ');
                writer.AsmWriteLn(proc.import_name^);
              end;
          end;
      cur_unit:=tused_unit(usedunits.First);
      while assigned(cur_unit) do
        begin
          for i:=0 to cur_unit.u.deflist.Count-1 do
            if assigned(cur_unit.u.deflist[i]) and (tdef(cur_unit.u.deflist[i]).typ = procdef) then
              begin
                proc := tprocdef(cur_unit.u.deflist[i]);
                if (not proc.owner.iscurrentunit or (po_external in proc.procoptions)) and
                   ((proc.paras.Count=0) or (proc.has_paraloc_info in [callerside,callbothsides])) then
                  begin
                    list:=TAsmList.Create;
                    thlcgwasm(hlcg).g_procdef(list,proc);
                    WriteTree(list);
                    list.free;
                  end;
              end;
          cur_unit:=tused_unit(cur_unit.Next);
        end;
    end;


  function TLLVMMachineCodePlaygroundAssembler.sectionname(atype: TAsmSectiontype; const aname: string; aorder: TAsmSectionOrder): string;
    begin
      if atype=sec_fpc then
        atype:=sec_data;
      Result:=inherited sectionname(atype, aname, aorder)+',"",@';
    end;


  constructor TLLVMMachineCodePlaygroundAssembler.CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean);
    begin
      inherited;
      InstrWriter:=TWASM32InstrWriter.create(self);
    end;


  procedure TLLVMMachineCodePlaygroundAssembler.WriteAsmList;
    begin
      inherited;
      { print all global procedures/functions }
      WriteSymtableProcdefs(current_module.globalsymtable);
      WriteSymtableProcdefs(current_module.localsymtable);
      writer.AsmWriteLn(#9'.globaltype'#9+STACK_POINTER_SYM+', i32');
      WriteImports;
    end;


  { TWASM32InstrWriter }


  procedure TWASM32InstrWriter.WriteInstruction(hp: tai);

    function getreferencestring(var ref : treference) : ansistring;
      begin
        if assigned(ref.symbol) then
          begin
            // global symbol or field -> full type and name
            // ref.base can be <> NR_NO in case an instance field is loaded.
            // This register is not part of this instruction, it will have
            // been placed on the stack by the previous one.
            result:=ref.symbol.name;
            if ref.base<>NR_NO then
              result:=result+'+'+std_regname(ref.base);
            if ref.index<>NR_NO then
              result:=result+'+'+std_regname(ref.index);
            if ref.offset>0 then
              result:=result+'+'+tostr(ref.offset)
            else if ref.offset<0 then
              result:=result+tostr(ref.offset);
          end
        else
          begin
            // local symbol -> stack slot, stored in offset
            result:='';
            if (ref.base<>NR_STACK_POINTER_REG) and (ref.base<>NR_NO) then
              result:=std_regname(ref.base);
            if ref.index<>NR_NO then
              if result<>'' then
                result:=result+'+'+std_regname(ref.index)
              else
                result:=std_regname(ref.index);
            if ref.offset>0 then
              begin
                if result<>'' then
                  result:=result+'+'+tostr(ref.offset)
                else
                  result:=tostr(ref.offset);
              end
            else if ref.offset<0 then
              result:=result+tostr(ref.offset);
            if result='' then
              result:='0';
          end;
      end;

    function constsingle(s: single): ansistring;
      begin
        // wat2wasm is using strtof() internally
        str(s, result); //'0x'+hexstr(longint(t32bitarray(s)),8);
      end;

    function constdouble(d: double): ansistring;
       begin
         // force interpretation as double (since we write it out as an
         // integer, we never have to swap the endianess). We have to
         // include the sign separately because of the way Java parses
         // hex numbers (0x8000000000000000 is not a valid long)
         //result:=hexstr(abs(int64(t64bitarray(d))),16);
         //if int64(t64bitarray(d))<0 then
         //  result:='-'+result;
         //result:='0dx'+result;
         str(d, result);
       end;

    function getopstr(const o:toper) : ansistring;
      var
        d: double;
        s: single;
      begin
        case o.typ of
          top_reg:
            // should have been translated into a memory location by the
            // register allocator)
            if (cs_no_regalloc in current_settings.globalswitches) then
              getopstr:=std_regname(o.reg)
            else
              internalerror(2010122803);
          top_const:
            str(o.val,result);
          top_ref:
            getopstr:=getreferencestring(o.ref^);
          top_single:
            begin
              result:=constsingle(o.sval);
            end;
          top_double:
            begin
              result:=constdouble(o.dval);
            end;
          {top_string:
            begin
              result:=constastr(o.pcval,o.pcvallen);
            end;
          top_wstring:
            begin
              result:=constwstr(o.pwstrval^.data,getlengthwidestring(o.pwstrval));
            end}
          else
            internalerror(2010122802);
        end;
      end;

    var
      cpu : taicpu;
      i   : integer;
      writer: TExternalAssemblerOutputFile;
    begin
      writer:=owner.writer;
      cpu := taicpu(hp);
      writer.AsmWrite(#9#9);
      writer.AsmWrite(gas_op2str[cpu.opcode]);

      if cpu.ops<>0 then
        begin
          for i:=0 to cpu.ops-1 do
            begin
              writer.AsmWrite(#9);
              if cpu.oper[i]^.typ=top_functype then
                owner.WriteFuncType(cpu.oper[i]^.functype)
              else
                writer.AsmWrite(getopstr(cpu.oper[i]^));
            end;
        end;
      writer.AsmLn;
    end;


  const
    as_wasm32_llvm_mc_info : tasminfo =
       (
         id     : as_wasm32_llvm_mc;
         idtxt  : 'LLVM-MC';
         asmbin : 'llvm-mc';
         asmcmd : '--assemble --arch=wasm32 -mattr=+sign-ext --filetype=obj -o $OBJ $EXTRAOPT $ASM';
         supported_targets : [system_wasm32_wasm,system_wasm32_wasi];
         flags : [];
         labelprefix : '.L';
         labelmaxlen : -1;
         comment : '# ';
         dollarsign : '$';
       );

initialization
  RegisterAssembler(as_wasm32_llvm_mc_info,TLLVMMachineCodePlaygroundAssembler);
end.

