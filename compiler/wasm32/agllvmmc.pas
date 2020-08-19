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
    systems,
    globtype,globals,
    aasmbase,aasmtai,aasmdata,
    assemble;

  type

    { TLLVMMachineCodePlaygroundAssembler }

    TLLVMMachineCodePlaygroundAssembler=class(texternalassembler)
    public
      procedure WriteTree(p : TAsmList); override;
      procedure WriteAsmList;override;
    end;

implementation

  uses
    cutils,
    finput,
    cpubase;

  { TLLVMMachineCodePlaygroundAssembler }

  procedure TLLVMMachineCodePlaygroundAssembler.WriteTree(p: TAsmList);
    var
      lasthp,
      hp: tai;
      InlineLevel : longint;
      prevfileinfo : tfileposinfo;
      previnfile : tinputfile;
      counter,lines,i,j,l,tokens,pos,last_align: longint;
      quoted, do_line: Boolean;
      s, LastSecName: string;
      LastAlign: Integer;
      LastSecOrder: TAsmSectionOrder;
    begin
      if not assigned(p) then
        exit;
      InlineLevel:=0;
      last_align:=1;
      lasthp:=nil;
      { lineinfo is only needed for al_procedures (PFV) }
      do_line:=(cs_asm_source in current_settings.globalswitches) or
               ((cs_lineinfo in current_settings.moduleswitches)
                 and (p=current_asmdata.asmlists[al_procedures]));
      hp:=tai(p.first);
      while assigned(hp) do
        begin
          prefetch(pointer(hp.next)^);
          if not(hp.typ in SkipLineInfo) then
            begin
              previnfile:=lastinfile;
              prevfileinfo:=lastfileinfo;
              current_filepos:=tailineinfo(hp).fileinfo;

              { no line info for inlined code }
              if do_line and (inlinelevel=0) then
                WriteSourceLine(hp as tailineinfo);
              (*if (lastfileinfo.line<>prevfileinfo.line) or
                 (previnfile<>lastinfile) then
                begin
                  { +0 postfix means no line increment per assembler instruction }
                  writer.AsmWrite('%LINE '+tostr(current_filepos.line)+'+0');
                  if assigned(lastinfile) and ((previnfile<>lastinfile) or NewObject) then
                    writer.AsmWriteLn(' '+lastinfile.name)
                  else
                    writer.AsmLn;
                  NewObject:=false;
                end;*)
            end;
          case hp.typ of
            ait_comment :
              begin
                writer.AsmWrite(asminfo^.comment);
                writer.AsmWritePChar(tai_comment(hp).str);
                writer.AsmLn;
              end;
            ait_regalloc :
              begin
                if (cs_asm_regalloc in current_settings.globalswitches) then
                  writer.AsmWriteLn(#9#9+asminfo^.comment+'Register '+std_regname(tai_regalloc(hp).reg)+' '+
                    regallocstr[tai_regalloc(hp).ratype]);
              end;
            ait_tempalloc :
              begin
                if (cs_asm_tempalloc in current_settings.globalswitches) then
                  WriteTempalloc(tai_tempalloc(hp));
              end;
//            ait_section :
//              begin
//                if tai_section(hp).sectype<>sec_none then
//                  WriteSection(tai_section(hp).sectype,tai_section(hp).name^,tai_section(hp).secorder,
//                    tai_section(hp).secalign,tai_section(hp).secflags,tai_section(hp).secprogbits)
//                else
//                  begin
//{$ifdef EXTDEBUG}
//                    writer.AsmWrite(asminfo^.comment);
//                    writer.AsmWriteln(' sec_none');
//{$endif EXTDEBUG}
//                 end;
//              end;
//            ait_align :
//              begin
//                doalign(tai_align_abstract(hp).aligntype,tai_align_abstract(hp).use_op,tai_align_abstract(hp).fillop,tai_align_abstract(hp).maxbytes,last_align,lasthp);
//              end;
            ait_label :
              begin
                if tai_label(hp).labsym.is_used then
                  begin
                    writer.AsmWrite(ApplyAsmSymbolRestrictions(tai_label(hp).labsym.name));
                    if tai_label(hp).labsym.bind in [AB_GLOBAL,AB_PRIVATE_EXTERN] then
                      writer.AsmWriteLn('::')
                    else
                      writer.AsmWriteLn(':');
                  end;
              end;
            ait_symbol :
              begin
                if not(tai_symbol(hp).has_value) then
                  begin
                    //if tai_symbol(hp).is_global then
                    //  writer.AsmWriteLn(ApplyAsmSymbolRestrictions(tai_symbol(hp).sym.name) + '::')
                    //else
                      writer.AsmWriteLn(ApplyAsmSymbolRestrictions(tai_symbol(hp).sym.name) + ':');
                  end
                else
                  begin
                    if tai_symbol(hp).is_global then
                      writer.AsmWriteLn(ApplyAsmSymbolRestrictions(tai_symbol(hp).sym.name) + '==' + tostr(tai_symbol(hp).value))
                    else
                      writer.AsmWriteLn(ApplyAsmSymbolRestrictions(tai_symbol(hp).sym.name) + '=' + tostr(tai_symbol(hp).value));
                  end;
              end;
            ait_symbol_end :
              begin
              end;
            ait_datablock :
              begin
                if tai_datablock(hp).is_global or SmartAsm then
                  writer.AsmWrite(ApplyAsmSymbolRestrictions(tai_datablock(hp).sym.name) + '::')
                else
                  writer.AsmWrite(ApplyAsmSymbolRestrictions(tai_datablock(hp).sym.name) + ':');
                {if SmartAsm then
                  AddSymbol(tai_datablock(hp).sym.name,true);}
                writer.AsmWriteLn(#9'.rs'#9+tostr(tai_datablock(hp).size));
              end;
            ait_realconst:
              WriteRealConstAsBytes(tai_realconst(hp),#9'.db'#9,do_line);
//            ait_const:
//              begin
//                consttype:=tai_const(hp).consttype;
//                case consttype of
//                  aitconst_uleb128bit:
//                    WriteDecodedUleb128(qword(tai_const(hp).value));
//                  aitconst_sleb128bit:
//                    WriteDecodedSleb128(int64(tai_const(hp).value));
//                  aitconst_64bit,
//                  aitconst_64bit_unaligned,
//                  aitconst_32bit,
//                  aitconst_32bit_unaligned:
//                    begin
//                      writer.AsmWrite(#9'.dw'#9);
//                      l:=0;
// 		      tokens:=1;
//                      repeat
//                        if assigned(tai_const(hp).sym) then
//                          begin
//                            if assigned(tai_const(hp).endsym) then
//                              s:=ApplyAsmSymbolRestrictions(tai_const(hp).endsym.name)+'-'+ApplyAsmSymbolRestrictions(tai_const(hp).sym.name)
//                            else
//                              s:=ApplyAsmSymbolRestrictions(tai_const(hp).sym.name);
//                            if tai_const(hp).value<>0 then
//                              s:=s+tostr_with_plus(tai_const(hp).value);
//                            if consttype in [aitconst_64bit,aitconst_64bit_unaligned] then
//                              s:=s+',0,0,0'
//                            else
//                              s:=s+',0';
//                          end
//                        else
//                          if consttype in [aitconst_64bit,aitconst_64bit_unaligned] then
//                            s:=tostr(Word(tai_const(hp).value))       +','+tostr(Word(tai_const(hp).value shr 16))+','+
//                               tostr(Word(tai_const(hp).value shr 32))+','+tostr(Word(tai_const(hp).value shr 48))
//                          else
//                            s:=tostr(Word(tai_const(hp).value))+','+tostr(Word(tai_const(hp).value shr 16));
//                        writer.AsmWrite(s);
//                        inc(l,length(s));
// 		        inc(tokens);
//                        if (l>line_length) or
//                           (tokens>max_tokens) or
//                           (hp.next=nil) or
//                           (tai(hp.next).typ<>ait_const) or
//                           (tai_const(hp.next).consttype<>consttype) then
//                          break;
//                        hp:=tai(hp.next);
//                        writer.AsmWrite(',');
//                      until false;
//                      { Substract section start for secrel32 type }
//                      {if consttype=aitconst_secrel32_symbol then
//                        writer.AsmWrite(' - $$');}
//                      writer.AsmLn;
//                    end;
//                  {aitconst_128bit,}
//                  aitconst_16bit,
//                  aitconst_8bit,
//                  aitconst_16bit_unaligned{,
//                  aitconst_rva_symbol,
//                  aitconst_secrel32_symbol} :
//                    begin
//                      writer.AsmWrite(ait_const2str[consttype]);
//                      l:=0;
// 		      tokens:=1;
//                      repeat
//                        if assigned(tai_const(hp).sym) then
//                          begin
//                            if assigned(tai_const(hp).endsym) then
//                              s:=ApplyAsmSymbolRestrictions(tai_const(hp).endsym.name)+'-'+ApplyAsmSymbolRestrictions(tai_const(hp).sym.name)
//                            else
//                              s:=ApplyAsmSymbolRestrictions(tai_const(hp).sym.name);
//                            if tai_const(hp).value<>0 then
//                              s:=s+tostr_with_plus(tai_const(hp).value);
//                          end
//                        else
//                          s:=tostr(tai_const(hp).value);
//                        writer.AsmWrite(s);
//                        inc(l,length(s));
// 		        inc(tokens);
//                        if (l>line_length) or
//                           (tokens>max_tokens) or
//                           (hp.next=nil) or
//                           (tai(hp.next).typ<>ait_const) or
//                           (tai_const(hp.next).consttype<>consttype) then
//                          break;
//                        hp:=tai(hp.next);
//                        writer.AsmWrite(',');
//                      until false;
//                      { Substract section start for secrel32 type }
//                      if consttype=aitconst_secrel32_symbol then
//                        writer.AsmWrite(' - $$');
//                      writer.AsmLn;
//                    end;
//                  else
//                    begin
//                      writer.AsmWrite(asminfo^.comment);
//                      writer.AsmWrite('WARNING: not yet implemented in assembler output: ');
//                      Str(consttype,s);
//                      writer.AsmWriteLn(s);
//                    end;
//                end;
//              end;
//            ait_string :
//              begin
//                pos:=0;
//                for i:=1 to tai_string(hp).len do
//                  begin
//                    if pos=0 then
//                      begin
//                        writer.AsmWrite(#9'.ascii'#9'"');
//                        pos:=20;
//                      end;
//                    ch:=tai_string(hp).str[i-1];
//                    case ch of
//                              #0, {This can't be done by range, because a bug in FPC}
//                         #1..#31,
//                      #128..#255 : s:='\'+tostr(ord(ch) shr 6)+tostr((ord(ch) and 63) shr 3)+tostr(ord(ch) and 7);
//                             '"' : s:='\"';
//                             '\' : s:='\\';
//                    else
//                      s:=ch;
//                    end;
//                    writer.AsmWrite(s);
//                    inc(pos,length(s));
//                    if (pos>line_length) or (i=tai_string(hp).len) then
//                      begin
//                        writer.AsmWriteLn('"');
//                        pos:=0;
//                      end;
//                  end;
//              end;
//            ait_instruction :
//              begin
//                WriteInstruction(taicpu(hp));
//              end;
            ait_directive :
              begin
                case tai_directive(hp).directive of
                  asd_cpu :
                    writer.AsmWriteLn('; CPU '+tai_directive(hp).name);
                  else
                    begin
                      writer.AsmWrite(asminfo^.comment);
                      writer.AsmWrite('WARNING: not yet implemented in assembler output: ait_directive.');
                      Str(tai_directive(hp).directive,s);
                      writer.AsmWriteLn(s);
                    end;
                end;
              end;
//            ait_cutobject :
//              begin
//                if SmartAsm then
//                 begin
//                  { only reset buffer if nothing has changed }
//                  if not writer.ClearIfEmpty then
//                   begin
//                     {if SmartAsm then
//                       begin
//                         WriteSmartExternals;
//                         FreeExternChainList;
//                       end;
//                     WriteGroups;}
//                     writer.AsmClose;
//                     DoAssemble;
//                     writer.AsmCreate(tai_cutobject(hp).place);
//                     {ResetSectionsList;
//                     WriteHeader;}
//                   end;
//                { avoid empty files }
//                  LastSecType:=sec_none;
//                  LastSecName:='';
//                  LastSecOrder:=secorder_default;
//                  LastAlign:=1;
//                  while assigned(hp.next) and (tai(hp.next).typ in [ait_cutobject,ait_section,ait_comment]) do
//                   begin
//                     if tai(hp.next).typ=ait_section then
//                       begin
//                         LastSecType:=tai_section(hp.next).sectype;
//                         LastSecName:=tai_section(hp.next).name^;
//                         LastSecOrder:=tai_section(hp.next).secorder;
//                         LastAlign:=tai_section(hp.next).secalign;
//                       end;
//                     hp:=tai(hp.next);
//                   end;
//                  if LastSecType<>sec_none then
//                    WriteSection(LastSecType,LastSecName,LastSecOrder,LastAlign);
//                  writer.MarkEmpty;
//                  //NewObject:=true;
//                end;
//              end;
            ait_marker :
              if tai_marker(hp).kind=mark_NoLineInfoStart then
                inc(InlineLevel)
              else if tai_marker(hp).kind=mark_NoLineInfoEnd then
                dec(InlineLevel);
            ait_stab,
            ait_force_line,
            ait_function_name : ;
            else
              begin
                writer.AsmWrite(asminfo^.comment);
                writer.AsmWrite('WARNING: not yet implemented in assembler output: ');
                Str(hp.typ,s);
                writer.AsmWriteLn(s);
              end;
          end;
          lasthp:=hp;
          hp:=tai(hp.next);
        end;
    end;


  procedure TLLVMMachineCodePlaygroundAssembler.WriteAsmList;
    var
      hal: TAsmListType;
    begin
      for hal:=low(TasmlistType) to high(TasmlistType) do
        begin
          writer.AsmWriteLn(asminfo^.comment+'Begin asmlist '+AsmListTypeStr[hal]);
          writetree(current_asmdata.asmlists[hal]);
          writer.AsmWriteLn(asminfo^.comment+'End asmlist '+AsmListTypeStr[hal]);
        end;
    end;

  const
    as_wasm32_llvm_mc_info : tasminfo =
       (
         id     : as_wasm32_llvm_mc;
         idtxt  : 'LLVM-MC';
         asmbin : 'llvm-mc';
         asmcmd : '--assemble --arch=wasm32 --filetype=obj -o $OBJ $EXTRAOPT $ASM';
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

