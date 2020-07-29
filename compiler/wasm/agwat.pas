{
    Copyright (c) 1998-2010 by the Free Pascal team

    This unit implements the WebAssembly text assembler
    The text is in S-expression format and should be consumable
    By either Binaryen or Wabt

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
unit agwat;

interface

  uses
    cclasses,systems, cgutils,
    globtype,globals,
    symconst,symbase,symdef,symsym, symtype,symcpu,
    aasmbase,aasmtai,aasmdata,aasmcpu,
    assemble
    ,cutils
    ,cpubase
    ,fmodule
    ,verbose, itcpuwasm;

  type
     TWatInstrWriter = class;

     {# This is a derived class which is used to write
        Binaryen-styled assembler.
     }

     { TWatAssembler }

     { TWabtTextAssembler }

     TWabtTextAssembler=class(texternalassembler)
     protected
       procedure WriteInstruction(hp: tai);
       procedure WriteProcDef(pd: tprocdef);
       procedure WriteProcParams(pd: tprocdef);
       procedure WriteProcResult(pd: tprocdef);
       procedure WriteSymtableProcdefs(st: TSymtable);
       procedure WriteSymtableVarSyms(st: TSymtable);
       procedure WriteTempAlloc(p:TAsmList);
       procedure WriteExports(p: TAsmList);
       procedure WriteImports(p: TAsmList);
     public
       constructor CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean); override;
       procedure WriteTree(p:TAsmList);override;
       procedure WriteAsmList;override;
     end;


     {# This is the base class for writing instructions.

        The WriteInstruction() method must be overridden
        to write a single instruction to the assembler
        file.
     }

     { TBinaryenInstrWriter }

     { TWatInstrWriter }

     TWatInstrWriter = class
       constructor create(_owner: TWabtTextAssembler);
       procedure WriteInstruction(hp : tai); virtual;
      protected
       owner: TWabtTextAssembler;
     end;

implementation

    type
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;


    const
      line_length = 70;

  {****************************************************************************}
  {                          Support routines                                  }
  {****************************************************************************}

     function fixline(s:string):string;
     {
       return s with all leading and ending spaces and tabs removed
     }
       var
         i,j,k : integer;
       begin
         i:=length(s);
         while (i>0) and (s[i] in [#9,' ']) do
          dec(i);
         j:=1;
         while (j<i) and (s[j] in [#9,' ']) do
          inc(j);
         for k:=j to i do
          if s[k] in [#0..#31,#127..#255] then
           s[k]:='.';
         fixline:=Copy(s,j,i-j+1);
       end;

     function GetWasmName(const st: TSymStr): ansistring;
     begin
       Result := '$'+st;
       Replace(Result, '(','');
       Replace(Result, ')','');
     end;

     function getreferencestring(var ref : treference) : ansistring;
       begin
         if (ref.index<>NR_NO) then
           internalerror(2010122809);
         if assigned(ref.symbol) then
           begin
             // global symbol or field -> full type and name
             // ref.base can be <> NR_NO in case an instance field is loaded.
             // This register is not part of this instruction, it will have
             // been placed on the stack by the previous one.
             result:=GetWasmName(ref.symbol.name);
           end
         else
           begin
             // local symbol -> stack slot, stored in offset
             if ref.base<>NR_STACK_POINTER_REG then
               internalerror(2010122810);
             result:=tostr(ref.offset);
           end;
       end;

     function constsingle(s: single): ansistring;
       begin
         result:='0x'+hexstr(longint(t32bitarray(s)),8);
       end;

     function constdouble(d: double): ansistring;
        begin
          // force interpretation as double (since we write it out as an
          // integer, we never have to swap the endianess). We have to
          // include the sign separately because of the way Java parses
          // hex numbers (0x8000000000000000 is not a valid long)
         result:=hexstr(abs(int64(t64bitarray(d))),16);
         if int64(t64bitarray(d))<0 then
           result:='-'+result;
         result:='0dx'+result;
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

  { TWabtTextAssembler }

    procedure TWabtTextAssembler.WriteInstruction(hp: tai);
    var
      cpu : taicpu;
      i   : integer;
    begin
      //writer.AsmWriteLn('instr');
      cpu := taicpu(hp);
      writer.AsmWrite(#9);
      writer.AsmWrite(wasm_op2str[cpu.opcode] );

      if (cpu.opcode = a_if)  then
        writer.AsmWrite(' (result i32)'); //todo: this is a hardcode, but shouldn't

      cpu := taicpu(hp);
      if cpu.ops<>0 then
        begin
          for i:=0 to cpu.ops-1 do
            begin
              writer.AsmWrite(#9);

              if (cpu.opcode in AsmOp_LoadStore) and (cpu.oper[i]^.typ = top_ref) then
                writer.AsmWrite('offset='+tostr( cpu.oper[i]^.ref^.offset))
              else
                writer.AsmWrite(getopstr(cpu.oper[i]^));

            end;
        end;

      writer.AsmLn;
    end;

    procedure TWabtTextAssembler.WriteProcDef(pd: tprocdef);
    var
      i : integer;
    begin
      if not assigned(tcpuprocdef(pd).exprasmlist) and
         not(po_abstractmethod in pd.procoptions) and
         (not is_javainterface(pd.struct) or
          (pd.proctypeoption in [potype_unitinit,potype_unitfinalize])) then
      begin
        exit;
      end;
      writer.AsmWrite('(func ');

      writer.AsmWrite( GetWasmName( pd.mangledname ));
      //procsym.RealName ));
      //writer.AsmWriteln(MethodDefinition(pd));
      {if jvmtypeneedssignature(pd) then
        begin
          writer.AsmWrite('.signature "');
          writer.AsmWrite(tcpuprocdef(pd).jvmmangledbasename(true));
          writer.AsmWriteln('"');
        end;}
      writer.AsmLn;
      WriteProcParams(tcpuprocdef(pd));
      WriteProcResult(tcpuprocdef(pd));
      WriteTempAlloc(tcpuprocdef(pd).exprasmlist);

      WriteTree(tcpuprocdef(pd).exprasmlist);
      writer.AsmWriteln(')');
      writer.AsmLn;
    end;

    procedure TWabtTextAssembler.WriteProcParams(pd: tprocdef);
      var
        i : integer;
        prm : tcpuparavarsym;
      begin
        if not Assigned(pd) or
           not Assigned(pd.paras) or
           (pd.paras.Count=0) then
          exit;

        for i:=0 to pd.paras.Count-1 do
          begin
            prm := tcpuparavarsym(pd.paras[i]);
            writer.AsmWrite(#9'(param'#9);
            case prm.getsize of
              1..4: writer.AsmWrite('i32');
              8: writer.AsmWrite('i64');
            end;
            writer.AsmWrite(')');
            writer.AsmLn;
          end;
      end;

    procedure TWabtTextAssembler.WriteProcResult(pd: tprocdef);
    begin
      if not assigned(pd) or
        not Assigned(pd.returndef) or
        (pd.returndef.size = 0)
        then exit;

      writer.AsmWrite(#9'(result'#9);
      case pd.returndef.size of
        1..4: writer.AsmWrite('i32');
        8: writer.AsmWrite('i64');
      end;
      writer.AsmWrite(')');
      writer.AsmLn;
    end;

    procedure TWabtTextAssembler.WriteTree(p: TAsmList);
      var
        ch       : char;
        hp       : tai;
        hp1      : tailineinfo;
        s        : ansistring;
        i,pos    : longint;
        InlineLevel : longint;
        do_line  : boolean;
      const
        WasmBasicTypeStr : array [TWasmBasicType] of string = ('i32','i64','f32','f64');

      begin
        if not assigned(p) then
         exit;

        InlineLevel:=0;
        { lineinfo is only needed for al_procedures (PFV) }
        do_line:=(cs_asm_source in current_settings.globalswitches);
        hp:=tai(p.first);
        while assigned(hp) do
         begin
           prefetch(pointer(hp.next)^);
           if not(hp.typ in SkipLineInfo) then
            begin
              hp1 := hp as tailineinfo;
              current_filepos:=hp1.fileinfo;
               { no line info for inlined code }
               if do_line and (inlinelevel=0) then
                begin
                  { load infile }
                  if lastfileinfo.fileindex<>hp1.fileinfo.fileindex then
                   begin
                     infile:=current_module.sourcefiles.get_file(hp1.fileinfo.fileindex);
                     if assigned(infile) then
                      begin
                        { open only if needed !! }
                        if (cs_asm_source in current_settings.globalswitches) then
                         infile.open;
                      end;
                     { avoid unnecessary reopens of the same file !! }
                     lastfileinfo.fileindex:=hp1.fileinfo.fileindex;
                     { be sure to change line !! }
                     lastfileinfo.line:=-1;
                   end;

                { write source }
                  if (cs_asm_source in current_settings.globalswitches) and
                     assigned(infile) then
                   begin
                     if (infile<>lastinfile) then
                       begin
                         writer.AsmWriteLn(asminfo^.comment+'['+infile.name+']');
                         if assigned(lastinfile) then
                           lastinfile.close;
                       end;
                     if (hp1.fileinfo.line<>lastfileinfo.line) and
                        ((hp1.fileinfo.line<infile.maxlinebuf) or (InlineLevel>0)) then
                       begin
                         if (hp1.fileinfo.line<>0) and
                            ((infile.linebuf^[hp1.fileinfo.line]>=0) or (InlineLevel>0)) then
                           writer.AsmWriteLn(asminfo^.comment+'['+tostr(hp1.fileinfo.line)+'] '+
                             fixline(infile.GetLineStr(hp1.fileinfo.line)));
                         { set it to a negative value !
                         to make that is has been read already !! PM }
                         if (infile.linebuf^[hp1.fileinfo.line]>=0) then
                           infile.linebuf^[hp1.fileinfo.line]:=-infile.linebuf^[hp1.fileinfo.line]-1;
                       end;
                   end;
                  lastfileinfo:=hp1.fileinfo;
                  lastinfile:=infile;
                end;
            end;

           case hp.typ of

             ait_comment :
               Begin
                 writer.AsmWrite(asminfo^.comment);
                 writer.AsmWritePChar(tai_comment(hp).str);
                 writer.AsmLn;
               End;

             ait_regalloc :
               begin
                 if (cs_asm_regalloc in current_settings.globalswitches) then
                   begin
                     writer.AsmWrite(#9+asminfo^.comment+'Register ');
                     repeat
                       writer.AsmWrite(std_regname(Tai_regalloc(hp).reg));
                       if (hp.next=nil) or
                          (tai(hp.next).typ<>ait_regalloc) or
                          (tai_regalloc(hp.next).ratype<>tai_regalloc(hp).ratype) then
                         break;
                       hp:=tai(hp.next);
                       writer.AsmWrite(',');
                     until false;
                     writer.AsmWrite(' ');
                     writer.AsmWriteLn(regallocstr[tai_regalloc(hp).ratype]);
                   end;
               end;

             ait_tempalloc :
               begin
                 if (cs_asm_tempalloc in current_settings.globalswitches) then
                   begin
  {$ifdef EXTDEBUG}
                     if assigned(tai_tempalloc(hp).problem) then
                       writer.AsmWriteLn(asminfo^.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                         tostr(tai_tempalloc(hp).tempsize)+' '+tai_tempalloc(hp).problem^)
                     else
  {$endif EXTDEBUG}
                   end;
               end;

             ait_align :
               begin

               end;

             ait_section :
               begin

               end;

             ait_datablock :
               begin
//                 internalerror(2010122701);
               end;

             ait_const:
               begin
                 writer.AsmWriteln('constant');
//                 internalerror(2010122702);
               end;

             ait_realconst :
               begin
                 internalerror(2010122703);
               end;

             ait_string :
               begin
                 pos:=0;
                  for i:=1 to tai_string(hp).len do
                   begin
                     if pos=0 then
                      begin
                        writer.AsmWrite(#9'strconst: '#9'"');
                        pos:=20;
                      end;
                     ch:=tai_string(hp).str[i-1];
                     case ch of
                        #0, {This can't be done by range, because a bug in FPC}
                   #1..#31,
                #128..#255 : s:='\'+tostr(ord(ch) shr 6)+tostr((ord(ch) and 63) shr 3)+tostr(ord(ch) and 7);
                       '"' : s:='\"';
                       '\' : s:='\\';
                     else
                      s:=ch;
                     end;
                     writer.AsmWrite(s);
                     inc(pos,length(s));
                     if (pos>line_length) or (i=tai_string(hp).len) then
                      begin
                        writer.AsmWriteLn('"');
                        pos:=0;
                      end;
                   end;
               end;

             ait_label :
               begin
                 // don't write any labels. Wasm don't support it
                 // labels are only allowed with the respective block structures
               end;

             ait_symbol :
               begin
                  if (tai_symbol(hp).sym.typ = AT_FUNCTION) then
                    begin
                    end
                  else
                   begin
                     writer.AsmWrite('data symbol: ');
                     writer.AsmWriteln(tai_symbol(hp).sym.name);
//                     internalerror(2010122706);
                   end;
               end;
             ait_symbol_end :
               begin
               end;

             ait_instruction :
               begin
                 WriteInstruction(hp);
               end;

             ait_force_line,
             ait_function_name : ;

             ait_cutobject :
               begin
               end;

             ait_marker :
               if tai_marker(hp).kind=mark_NoLineInfoStart then
                 inc(InlineLevel)
               else if tai_marker(hp).kind=mark_NoLineInfoEnd then
                 dec(InlineLevel);

             ait_directive :
               begin
                 { the CPU directive is probably not supported by the JVM assembler,
                   so it's commented out }

                 //todo:
                 writer.AsmWrite(asminfo^.comment);

                 if tai_directive(hp).directive=asd_cpu then
                   writer.AsmWrite(asminfo^.comment);
                 writer.AsmWrite('.'+directivestr[tai_directive(hp).directive]+' ');
                 if tai_directive(hp).name<>'' then
                   writer.AsmWrite(tai_directive(hp).name);
                 writer.AsmLn;
               end;

             ait_local :
               begin
                 writer.AsmWrite(#9'(local ');
                 writer.AsmWrite( WasmBasicTypeStr[ tai_local(hp).bastyp ] );
                 writer.AsmWrite(')');
                 writer.AsmLn;
               end;

             else
               internalerror(2010122707);
           end;
           hp:=tai(hp.next);
         end;
    end;

    procedure TWabtTextAssembler.WriteAsmList;
      var
        hal : tasmlisttype;
      begin
        if current_module.is_unit then begin
          writer.AsmWriteLn('(module)');
          exit;
        end;
        writer.MarkEmpty;
        writer.AsmWriteLn('(module ');
        writer.AsmWriteLn('(import "env" "memory" (memory 0)) ;;');
        WriteImports(current_asmdata.asmlists[al_imports]);

        { print all global variables }
        //current_asmdata.AsmSymbolDict
        WriteSymtableVarSyms(current_module.globalsymtable);
        WriteSymtableVarSyms(current_module.localsymtable);

        //writer.AsmLn;
        { print all global procedures/functions }
        WriteSymtableProcdefs(current_module.globalsymtable);
        WriteSymtableProcdefs(current_module.localsymtable);

        WriteExports(current_asmdata.asmlists[al_exports]);

        //WriteSymtableStructDefs(current_module.globalsymtable);
        //WriteSymtableStructDefs(current_module.localsymtable);
        //writer.decorator.LinePrefix := '';
        writer.AsmWriteLn(')');

        writer.AsmLn;
      end;

    constructor TWabtTextAssembler.CreateWithWriter(info: pasminfo;
      wr: TExternalAssemblerOutputFile; freewriter, smart: boolean);
      begin
        inherited CreateWithWriter(info, wr, freewriter, smart);
      end;

    procedure TWabtTextAssembler.WriteSymtableProcdefs(st: TSymtable);
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

    procedure TWabtTextAssembler.WriteSymtableVarSyms(st: TSymtable);
      var
        i : integer;
        sym : tsym;
        sz  : integer;
      begin
        if not assigned(st) then
          exit;

        sz := 0;
        for i:=0 to st.SymList.Count-1 do
         begin
           sym:=tsym(st.SymList[i]);
           case sym.typ of
             staticvarsym:
               begin
                 //WriteFieldSym(tabstractvarsym(sym));
                 //if (sym.typ=staticvarsym) and
                 //   assigned(tstaticvarsym(sym).defaultconstsym) then
                 //  WriteFieldSym(tabstractvarsym(tstaticvarsym(sym).defaultconstsym));
                 writer.AsmWrite(#9);
                 writer.AsmWrite('(global $');
                 writer.AsmWrite(tcpustaticvarsym(sym).mangledname);
                 writer.AsmWrite(' (mut i32) (i32.const ');
                 writer.AsmWrite( tostr(sz));
                 writer.AsmWrite(')');
                 writer.AsmWrite(') ');
                 writer.AsmWriteLn(';; static or field');
                 inc(sz, tcpustaticvarsym(sym).getsize);
               end;
             fieldvarsym:
               begin
                 writer.AsmWriteLn(';; field');
               end;
             constsym:
               begin
                 //if (sym.typ=staticvarsym) and
                 //   assigned(tstaticvarsym(sym).defaultconstsym) then
                 //  WriteFieldSym(tabstractvarsym(tstaticvarsym(sym).defaultconstsym));
                 //{ multiple procedures can have constants with the same name }
                 //if not assigned(sym.owner.defowner) or
                 //   (tdef(sym.owner.defowner).typ<>procdef) then
                 //  WriteConstSym(tconstsym(sym));
                 writer.AsmWriteLn(';; constant');
               end;
             {procsym:
               begin
                 for j:=0 to tprocsym(sym).procdeflist.count-1 do
                   if not(df_generic in tprocdef(tprocsym(sym).procdeflist[j]).defoptions) then
                     WriteSymtableVarSyms(tprocdef(tprocsym(sym).procdeflist[j]).localst);
               end;}
             else
               ;
           end;
         end;
      end;

    procedure TWabtTextAssembler.WriteTempAlloc(p: TAsmList);
      var
        hp: tai;
        tmp: array of tai_tempalloc;
        mx : integer;
        i  : integer;
      begin
        if not assigned(p) then
         exit;

        mx := -1;
        hp:=tai(p.first);
        while assigned(hp) do
         begin
           //prefetch(pointer(hp.next)^);
           if (hp.typ = ait_tempalloc) and
             tai_tempalloc(hp).allocation and
             (tai_tempalloc(hp).temppos >= mx) then
               mx := tai_tempalloc(hp).temppos+1;

           hp:=tai(hp.next);
         end;

        if (mx <= 0) then exit; // no temp allocations used

        SetLength(tmp, mx);

        hp:=tai(p.first);
        while assigned(hp) do
          begin
            //prefetch(pointer(hp.next)^);

            if (hp.typ = ait_tempalloc) and
              (tai_tempalloc(hp).allocation) and
              (tmp[ tai_tempalloc(hp).temppos ] = nil) then
              begin
                tmp[ tai_tempalloc(hp).temppos ] := tai_tempalloc(hp);
                dec(mx);
                if mx = 0 then break;
              end;
            hp:=tai(hp.next);
          end;

        for i:=0 to length(tmp)-1 do
          begin
            if tmp[i] = nil then continue;
            writer.AsmWrite(#9'(local'#9);
            if tmp[i].tempsize<=4 then writer.AsmWrite('i32')
            else if tmp[i].tempsize = 8 then writer.AsmWrite('i64');
            writer.AsmWrite(')');

            writer.AsmWrite(#9+asminfo^.comment+'Temp '+tostr(tmp[i].temppos)+','+
              tostr(tmp[i].tempsize)+' '+tempallocstr[tmp[i].allocation]);

            writer.AsmLn;
          end;

      end;

    procedure TWabtTextAssembler.WriteExports(p: TAsmList);
    var
      hp: tai;
      x: tai_impexp;
    begin
      if not Assigned(p) then Exit;
      hp:=tai(p.First);
      while Assigned(hp) do begin
        case hp.typ of
          ait_importexport:
          begin
            x:=tai_impexp(hp);
            writer.AsmWrite('(export "');
            writer.AsmWrite(x.extname);
            writer.AsmWrite('" (');
            case x.symstype of
              ie_Func: writer.AsmWrite('func');
            end;
            writer.AsmWrite(' ');
            writer.AsmWrite(GetWasmName(x.intname));
            writer.AsmWrite('))');
            writer.AsmLn;
          end;
        end;
        hp := tai_impexp(hp.Next);
      end;
    end;

    procedure TWabtTextAssembler.WriteImports(p: TAsmList);
    var
      hp: tai;
      x: tai_impexp;
    begin
      if not Assigned(p) then Exit;
      hp:=tai(p.First);
      while Assigned(hp) do begin
        case hp.typ of
          ait_importexport:
          begin
            x:=tai_impexp(hp);
            writer.AsmWrite('(import "');
            writer.AsmWrite(x.extmodule);
            writer.AsmWrite('" "');
            writer.AsmWrite(x.extname);
            writer.AsmWrite('" (');
            case x.symstype of
              ie_Func: writer.AsmWrite('func');
            end;
            writer.AsmWrite(' ');
            writer.AsmWrite(GetWasmName(x.intname));
            writer.AsmWrite('))');
            writer.AsmLn;
          end;
        end;
        hp := tai_impexp(hp.Next);
      end;
    end;


{ TWatInstrWriter }

  constructor TWatInstrWriter.create(_owner: TWabtTextAssembler);
    begin
      inherited create;
      owner := _owner;
    end;

  procedure TWatInstrWriter.WriteInstruction(hp: tai);
    begin

    end;


  const
    as_wasm_wabt_info : tasminfo =
       (
         id     : as_wasm_wabt;
         idtxt  : 'Wabt';
         asmbin : 'wat2wasm';
         asmcmd : '$EXTRAOPT $ASM';
         supported_targets : [system_wasm_wasm32];
         flags : [];
         labelprefix : 'L';
         comment : ';; ';
         dollarsign : '$';
       );

initialization
  RegisterAssembler(as_wasm_wabt_info, TWabtTextAssembler);

end.
