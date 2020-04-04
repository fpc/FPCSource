{
    Copyright (c) 2003 by Florian Klaempfl

    This unit implements an asm for the Z80

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
{ This unit implements the assembler writer for the sdcc-sdasz80 assembler:
  http://sdcc.sourceforge.net/
}

unit agsdasz80;

{$i fpcdefs.inc}

  interface

    uses
       globtype,systems,
       aasmbase,aasmtai,aasmdata,aasmcpu,
       assemble,
       cpubase;

    type

      { TSdccSdasZ80Assembler }

      TSdccSdasZ80Assembler=class(TExternalAssembler)
      private
        procedure WriteDecodedSleb128(a: int64);
        procedure WriteDecodedUleb128(a: qword);
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
        procedure WriteSection(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder;secalign:longint;
          secflags:TSectionFlags=[];secprogbits:TSectionProgbits=SPB_None);
        procedure WriteInstruction(hp: taicpu);
        procedure WriteOper(const o:toper; opcode: tasmop;ops:longint;dest : boolean);
        procedure WriteOper_jmp(const o:toper; ai : taicpu);
      public
        procedure WriteTree(p : TAsmList); override;
        procedure WriteAsmList;override;
        function MakeCmdLine: TCmdStr; override;
      end;

  implementation

    uses
       cutils,globals,verbose,
       cpuinfo,
       cgbase,cgutils,
       finput;

    const
      line_length = 70;
      max_tokens : longint = 25;
      ait_const2str : array[aitconst_128bit..aitconst_64bit_unaligned] of string[20]=(
        #9''#9,#9'FIXMEDQ'#9,#9'FIXMEDD'#9,#9'.dw'#9,#9'.db'#9,
        #9'FIXMESLEB',#9'FIXEMEULEB',
        #9'FIXMEDD RVA'#9,#9'FIXMEDD SECREL32'#9,
        #9'FIXME',#9'FIXME',#9'FIXME',#9'FIXME',
        #9'.dw'#9,#9'FIXMEDD'#9,#9'FIXMEDQ'#9
      );

    procedure TSdccSdasZ80Assembler.WriteDecodedSleb128(a: int64);
      var
        i,len : longint;
        buf   : array[0..255] of byte;
      begin
        writer.AsmWrite(#9'.db'#9);
        len:=EncodeSleb128(a,buf,0);
        for i:=0 to len-1 do
          begin
            if (i > 0) then
              writer.AsmWrite(',');
            writer.AsmWrite(tostr(buf[i]));
          end;
        writer.AsmWriteLn(#9'; sleb '+tostr(a));
      end;

    procedure TSdccSdasZ80Assembler.WriteDecodedUleb128(a: qword);
      var
        i,len : longint;
        buf   : array[0..63] of byte;
      begin
        writer.AsmWrite(#9'.db'#9);
        len:=EncodeUleb128(a,buf,0);
        for i:=0 to len-1 do
          begin
            if (i > 0) then
              writer.AsmWrite(',');
            writer.AsmWrite(tostr(buf[i]));
          end;
        writer.AsmWriteLn(#9'; uleb '+tostr(a));
      end;

    function TSdccSdasZ80Assembler.sectionname(atype: TAsmSectiontype;
        const aname: string; aorder: TAsmSectionOrder): string;
      const
        secnames : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
          '_CODE',
          '_DATA',
          '_DATA',
          '.rodata',
          '.bss',
          '.threadvar',
          '.pdata',
          '', { stubs }
          '__DATA,__nl_symbol_ptr',
          '__DATA,__la_symbol_ptr',
          '__DATA,__mod_init_func',
          '__DATA,__mod_term_func',
          '.stab',
          '.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev','.debug_aranges','.debug_ranges',
          '.fpc',
          '.toc',
          '.init',
          '.fini',
          '.objc_class',
          '.objc_meta_class',
          '.objc_cat_cls_meth',
          '.objc_cat_inst_meth',
          '.objc_protocol',
          '.objc_string_object',
          '.objc_cls_meth',
          '.objc_inst_meth',
          '.objc_cls_refs',
          '.objc_message_refs',
          '.objc_symbols',
          '.objc_category',
          '.objc_class_vars',
          '.objc_instance_vars',
          '.objc_module_info',
          '.objc_class_names',
          '.objc_meth_var_types',
          '.objc_meth_var_names',
          '.objc_selector_strs',
          '.objc_protocol_ext',
          '.objc_class_ext',
          '.objc_property',
          '.objc_image_info',
          '.objc_cstring_object',
          '.objc_sel_fixup',
          '__DATA,__objc_data',
          '__DATA,__objc_const',
          '.objc_superrefs',
          '__DATA, __datacoal_nt,coalesced',
          '.objc_classlist',
          '.objc_nlclasslist',
          '.objc_catlist',
          '.obcj_nlcatlist',
          '.objc_protolist',
          '.stack',
          '.heap',
          '.gcc_except_table',
          '.ARM.attributes'
        );
      begin
        result:=secnames[atype];
      end;

    procedure TSdccSdasZ80Assembler.WriteSection(atype: TAsmSectiontype;
        const aname: string; aorder: TAsmSectionOrder; secalign: longint;
        secflags: TSectionFlags; secprogbits: TSectionProgbits);
      var
        s : string;
        secflag: TSectionFlag;
        sectionprogbits,
        sectionflags: boolean;
      begin
        writer.AsmLn;
        sectionflags:=false;
        sectionprogbits:=false;
        writer.AsmWrite(#9'.area ');
        { sectionname may rename those sections, so we do not write flags/progbits for them,
          the assembler will ignore them/spite out a warning anyways }
        if not(atype in [sec_data,sec_rodata,sec_rodata_norel]) then
          begin
            sectionflags:=true;
            sectionprogbits:=true;
          end;
        s:=sectionname(atype,aname,aorder);
        writer.AsmWrite(s);
        { flags explicitly defined? }
        (*if (sectionflags or sectionprogbits) and
           ((secflags<>[]) or
            (secprogbits<>SPB_None)) then
          begin
            if sectionflags then
              begin
                s:=',"';
                for secflag in secflags do
                  case secflag of
                    SF_A:
                      s:=s+'a';
                    SF_W:
                      s:=s+'w';
                    SF_X:
                      s:=s+'x';
                  end;
                writer.AsmWrite(s+'"');
              end;
            if sectionprogbits then
              begin
                case secprogbits of
                  SPB_PROGBITS:
                    writer.AsmWrite(',%progbits');
                  SPB_NOBITS:
                    writer.AsmWrite(',%nobits');
                  SPB_NOTE:
                    writer.AsmWrite(',%note');
                  SPB_None:
                    ;
                  else
                    InternalError(2019100801);
                end;
              end;
          end
        else
          case atype of
            sec_fpc :
              if aname = 'resptrs' then
                writer.AsmWrite(', "a", @progbits');
            sec_stub :
              begin
                case target_info.system of
                  { there are processor-independent shortcuts available    }
                  { for this, namely .symbol_stub and .picsymbol_stub, but }
                  { they don't work and gcc doesn't use them either...     }
                  system_powerpc_darwin,
                  system_powerpc64_darwin:
                    if (cs_create_pic in current_settings.moduleswitches) then
                      writer.AsmWriteln('__TEXT,__picsymbolstub1,symbol_stubs,pure_instructions,32')
                    else
                      writer.AsmWriteln('__TEXT,__symbol_stub1,symbol_stubs,pure_instructions,16');
                  system_i386_darwin,
                  system_i386_iphonesim:
                    writer.AsmWriteln('__IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5');
                  system_arm_darwin:
                    if (cs_create_pic in current_settings.moduleswitches) then
                      writer.AsmWriteln('__TEXT,__picsymbolstub4,symbol_stubs,none,16')
                    else
                      writer.AsmWriteln('__TEXT,__symbol_stub4,symbol_stubs,none,12')
                  { darwin/(x86-64/AArch64) uses PC-based GOT addressing, no
                    explicit symbol stubs }
                  else
                    internalerror(2006031101);
                end;
              end;
          else
            { GNU AS won't recognize '.text.n_something' section name as belonging
              to '.text' and assigns default attributes to it, which is not
              always correct. We have to fix it.

              TODO: This likely applies to all systems which smartlink without
              creating libraries }
            begin
              if is_smart_section(atype) and (aname<>'') then
                begin
                  s:=sectionattrs(atype);
                  if (s<>'') then
                    writer.AsmWrite(',"'+s+'"');
                end;
              if target_info.system in systems_aix then
                begin
                  s:=sectionalignment_aix(atype,secalign);
                  if s<>'' then
                    writer.AsmWrite(','+s);
                end;
            end;
          end;*)
        writer.AsmLn;
        LastSecType:=atype;
      end;

    procedure TSdccSdasZ80Assembler.WriteInstruction(hp: taicpu);
      var
        i: Integer;
      begin
        writer.AsmWrite(#9#9+std_op2str[hp.opcode]);
        if taicpu(hp).ops<>0 then
          begin
            for i:=0 to taicpu(hp).ops-1 do
              begin
                if i=0 then
                  begin
                    writer.AsmWrite(#9);
                    if hp.is_jmp and (hp.condition<>C_None) then
                      writer.AsmWrite(uppercond2str[hp.condition]+',');
                  end
                else
                  writer.AsmWrite(',');
                if is_calljmp(hp.opcode) then
                  WriteOper_jmp(taicpu(hp).oper[i]^,hp)
                else
                  WriteOper(taicpu(hp).oper[i]^,hp.opcode,taicpu(hp).ops,(i=2));
              end;
          end;
        writer.AsmLn;
      end;

    procedure TSdccSdasZ80Assembler.WriteOper(const o: toper; opcode: tasmop; ops: longint; dest: boolean);
      var
        need_plus: Boolean;
      begin
        case o.typ of
          top_reg :
            writer.AsmWrite(std_regname(o.reg));
          top_const :
            begin
{              if (ops=1) and (opcode<>A_RET) then
                writer.AsmWrite(sizestr(s,dest));}
              writer.AsmWrite('#'+tostr(longint(o.val)));
            end;
          top_ref:
            begin
              if not assigned(o.ref^.symbol) and
                 ((o.ref^.base<>NR_NO) or (o.ref^.index<>NR_NO)) and
                 (o.ref^.offset<>0) then
                begin
                  writer.AsmWrite(tostr(o.ref^.offset));
                  writer.AsmWrite(' (');
                  if o.ref^.base<>NR_NO then
                    begin
                      if o.ref^.index<>NR_NO then
                        internalerror(2020040201);
                      writer.AsmWrite(std_regname(o.ref^.base));
                    end
                  else if o.ref^.index<>NR_NO then
                    begin
                      if o.ref^.scalefactor>1 then
                        internalerror(2020040202);
                      writer.AsmWrite(std_regname(o.ref^.index));
                    end;
                  writer.AsmWrite(')');
                end
              else
                begin
                  writer.AsmWrite('(');
                  need_plus:=false;
                  if o.ref^.base<>NR_NO then
                    begin
                      if o.ref^.index<>NR_NO then
                        internalerror(2020040201);
                      writer.AsmWrite(std_regname(o.ref^.base));
                      need_plus:=true;
                    end
                  else if o.ref^.index<>NR_NO then
                    begin
                      if o.ref^.scalefactor>1 then
                        internalerror(2020040202);
                      writer.AsmWrite(std_regname(o.ref^.index));
                      need_plus:=true;
                    end;
                  if assigned(o.ref^.symbol) then
                    begin
                      {if SmartAsm then
                        AddSymbol(o.ref^.symbol.name,false);}
                      if need_plus then
                        writer.AsmWrite('+');
                      writer.AsmWrite(o.ref^.symbol.name);
                      need_plus:=true;
                    end;
                  if o.ref^.offset<>0 then
                    begin
                      if need_plus and (o.ref^.offset>0) then
                        writer.AsmWrite('+');
                      writer.AsmWrite(tostr(o.ref^.offset));
                      need_plus:=true;
                    end;
                  if not need_plus then
                    writer.AsmWrite('0');
                  writer.AsmWrite(')');
                end;
            end;
          else
            internalerror(10001);
        end;
      end;

    procedure TSdccSdasZ80Assembler.WriteOper_jmp(const o: toper; ai: taicpu);
      begin
        case o.typ of
          top_reg :
            writer.AsmWrite(std_regname(o.reg));
          top_const :
            begin
              writer.AsmWrite('#'+tostr(longint(o.val)));
            end;
          top_ref:
            begin
              if o.ref^.refaddr=addr_no then
                begin
                  writer.AsmWrite('TODO:indirect jump ref');
                  //WriteReference(o.ref^);
                end
              else
                begin
                  writer.AsmWrite(o.ref^.symbol.name);
                  //if SmartAsm then
                  //  AddSymbol(o.ref^.symbol.name,false);
                  if o.ref^.offset>0 then
                   writer.AsmWrite('+'+tostr(o.ref^.offset))
                  else
                   if o.ref^.offset<0 then
                    writer.AsmWrite(tostr(o.ref^.offset));
                end;
            end;
          else
            internalerror(10001);
        end;
      end;

    procedure TSdccSdasZ80Assembler.WriteTree(p: TAsmList);

      function getreferencestring(var ref : treference) : string;
        var
          s : string;
        begin
           s:='';
           with ref do
            begin
  {$ifdef extdebug}
              // if base=NR_NO then
              //   internalerror(200308292);

              // if ((index<>NR_NO) or (shiftmode<>SM_None)) and ((offset<>0) or (symbol<>nil)) then
              //   internalerror(200308293);
  {$endif extdebug}
              if index<>NR_NO then
                internalerror(2011021701)
              else if base<>NR_NO then
                begin
//                  if addressmode=AM_PREDRECEMENT then
//                    s:='-';

                  //case base of
                  //  NR_R26:
                  //    s:=s+'X';
                  //  NR_R28:
                  //    s:=s+'Y';
                  //  NR_R30:
                  //    s:=s+'Z';
                  //  else
                  //    s:=gas_regname(base);
                  //end;
                  //if addressmode=AM_POSTINCREMENT then
                  //  s:=s+'+';
                  //
                  //if offset>0 then
                  //  s:=s+'+'+tostr(offset)
                  //else if offset<0 then
                  //  s:=s+tostr(offset)
                end
              else if assigned(symbol) or (offset<>0) then
                begin
                  //if assigned(symbol) then
                  //  s:=ReplaceForbiddenAsmSymbolChars(symbol.name);
                  //
                  //if offset<0 then
                  //  s:=s+tostr(offset)
                  //else if offset>0 then
                  //  s:=s+'+'+tostr(offset);
                  //case refaddr of
                  //  addr_hi8:
                  //    s:='hi8('+s+')';
                  //  addr_hi8_gs:
                  //    s:='hi8(gs('+s+'))';
                  //  addr_lo8:
                  //    s:='lo8('+s+')';
                  //  addr_lo8_gs:
                  //    s:='lo8(gs('+s+'))';
                  //  else
                  //    s:='('+s+')';
                  //end;
                end;
            end;
          getreferencestring:=s;
        end;


      function getopstr(const o:toper) : string;
        var
          hs : string;
          first : boolean;
          r : tsuperregister;
        begin
          //case o.typ of
          //  top_reg:
          //    getopstr:=gas_regname(o.reg);
          //  top_const:
          //    getopstr:=tostr(longint(o.val));
          //  top_ref:
          //    if o.ref^.refaddr=addr_full then
          //      begin
          //        hs:=ReplaceForbiddenAsmSymbolChars(o.ref^.symbol.name);
          //        if o.ref^.offset>0 then
          //         hs:=hs+'+'+tostr(o.ref^.offset)
          //        else
          //         if o.ref^.offset<0 then
          //          hs:=hs+tostr(o.ref^.offset);
          //        getopstr:=hs;
          //      end
          //    else
          //      getopstr:=getreferencestring(o.ref^);
          //  else
          //    internalerror(2002070604);
          //end;
        end;


      procedure doalign(alignment: byte; use_op: boolean; fillop: byte; maxbytes: byte; out last_align: longint;lasthp:tai);
        var
          i: longint;
          alignment64 : int64;
        begin
          last_align:=alignment;
          if alignment>1 then
            writer.AsmWriteLn(#9'.bndry '+tostr(alignment));
        end;

    //var op: TAsmOp;
    //    s: string;
    //    i: byte;
    //    sep: string[3];
    var
      lasthp,
      hp: tai;
      s: string;
      counter,lines,i,j,l,tokens,pos,last_align: longint;
      quoted, do_line: Boolean;
      consttype: taiconst_type;
      ch: Char;
      InlineLevel : longint;
      prevfileinfo : tfileposinfo;
      previnfile : tinputfile;
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
            ait_section :
              begin
                if tai_section(hp).sectype<>sec_none then
                  WriteSection(tai_section(hp).sectype,tai_section(hp).name^,tai_section(hp).secorder,
                    tai_section(hp).secalign,tai_section(hp).secflags,tai_section(hp).secprogbits)
                else
                  begin
{$ifdef EXTDEBUG}
                    writer.AsmWrite(asminfo^.comment);
                    writer.AsmWriteln(' sec_none');
{$endif EXTDEBUG}
                 end;
              end;
            ait_align :
              begin
                doalign(tai_align_abstract(hp).aligntype,tai_align_abstract(hp).use_op,tai_align_abstract(hp).fillop,tai_align_abstract(hp).maxbytes,last_align,lasthp);
              end;
            ait_label :
              begin
                if tai_label(hp).labsym.is_used then
                  begin
                    writer.AsmWrite(tai_label(hp).labsym.name);
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
                    if tai_symbol(hp).is_global then
                      writer.AsmWriteLn(tai_symbol(hp).sym.name + '::')
                    else
                      writer.AsmWriteLn(tai_symbol(hp).sym.name + ':');
                  end
                else
                  begin
                    if tai_symbol(hp).is_global then
                      writer.AsmWriteLn(tai_symbol(hp).sym.name + '==' + tostr(tai_symbol(hp).value))
                    else
                      writer.AsmWriteLn(tai_symbol(hp).sym.name + '=' + tostr(tai_symbol(hp).value));
                  end;
              end;
            ait_symbol_end :
              begin
              end;
            ait_datablock :
              begin
                if tai_datablock(hp).is_global or SmartAsm then
                  writer.AsmWrite(tai_datablock(hp).sym.name + '::')
                else
                  writer.AsmWrite(tai_datablock(hp).sym.name + ':');
                {if SmartAsm then
                  AddSymbol(tai_datablock(hp).sym.name,true);}
                writer.AsmWriteLn(#9'.rs'#9+tostr(tai_datablock(hp).size));
              end;
            ait_const:
              begin
                consttype:=tai_const(hp).consttype;
                case consttype of
                  aitconst_uleb128bit:
                    WriteDecodedUleb128(qword(tai_const(hp).value));
                  aitconst_sleb128bit:
                    WriteDecodedSleb128(int64(tai_const(hp).value));
                  aitconst_64bit,
                  aitconst_64bit_unaligned,
                  aitconst_32bit,
                  aitconst_32bit_unaligned:
                    begin
                      writer.AsmWrite(#9'.dw'#9);
                      l:=0;
 		      tokens:=1;
                      repeat
                        if assigned(tai_const(hp).sym) then
                          begin
                            if assigned(tai_const(hp).endsym) then
                              s:=tai_const(hp).endsym.name+'-'+tai_const(hp).sym.name
                            else
                              s:=tai_const(hp).sym.name;
                            if tai_const(hp).value<>0 then
                              s:=s+tostr_with_plus(tai_const(hp).value);
                            if consttype in [aitconst_64bit,aitconst_64bit_unaligned] then
                              s:=s+',0,0,0'
                            else
                              s:=s+',0';
                          end
                        else
                          if consttype in [aitconst_64bit,aitconst_64bit_unaligned] then
                            s:=tostr(Word(tai_const(hp).value))       +','+tostr(Word(tai_const(hp).value shr 16))+','+
                               tostr(Word(tai_const(hp).value shr 32))+','+tostr(Word(tai_const(hp).value shr 48))
                          else
                            s:=tostr(Word(tai_const(hp).value))+','+tostr(Word(tai_const(hp).value shr 16));
                        writer.AsmWrite(s);
                        inc(l,length(s));
 		        inc(tokens);
                        if (l>line_length) or
                           (tokens>max_tokens) or
                           (hp.next=nil) or
                           (tai(hp.next).typ<>ait_const) or
                           (tai_const(hp.next).consttype<>consttype) then
                          break;
                        hp:=tai(hp.next);
                        writer.AsmWrite(',');
                      until false;
                      { Substract section start for secrel32 type }
                      {if consttype=aitconst_secrel32_symbol then
                        writer.AsmWrite(' - $$');}
                      writer.AsmLn;
                    end;
                  {aitconst_128bit,}
                  aitconst_16bit,
                  aitconst_8bit,
                  aitconst_16bit_unaligned{,
                  aitconst_rva_symbol,
                  aitconst_secrel32_symbol} :
                    begin
                      writer.AsmWrite(ait_const2str[consttype]);
                      l:=0;
 		      tokens:=1;
                      repeat
                        if assigned(tai_const(hp).sym) then
                          begin
                            if assigned(tai_const(hp).endsym) then
                              s:=tai_const(hp).endsym.name+'-'+tai_const(hp).sym.name
                            else
                              s:=tai_const(hp).sym.name;
                            if tai_const(hp).value<>0 then
                              s:=s+tostr_with_plus(tai_const(hp).value);
                          end
                        else
                          s:=tostr(tai_const(hp).value);
                        writer.AsmWrite(s);
                        inc(l,length(s));
 		        inc(tokens);
                        if (l>line_length) or
                           (tokens>max_tokens) or
                           (hp.next=nil) or
                           (tai(hp.next).typ<>ait_const) or
                           (tai_const(hp.next).consttype<>consttype) then
                          break;
                        hp:=tai(hp.next);
                        writer.AsmWrite(',');
                      until false;
                      { Substract section start for secrel32 type }
                      if consttype=aitconst_secrel32_symbol then
                        writer.AsmWrite(' - $$');
                      writer.AsmLn;
                    end;
                  else
                    begin
                      writer.AsmWrite(asminfo^.comment);
                      writer.AsmWrite('WARNING: not yet implemented in assembler output: ');
                      Str(consttype,s);
                      writer.AsmWriteLn(s);
                    end;
                end;
              end;
            ait_string :
              begin
                pos:=0;
                for i:=1 to tai_string(hp).len do
                  begin
                    if pos=0 then
                      begin
                        writer.AsmWrite(#9'.ascii'#9'"');
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
            ait_instruction :
              begin
                WriteInstruction(taicpu(hp));
              end;
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
      //op:=taicpu(hp).opcode;
      //s:=#9+gas_op2str[op]+cond2str[taicpu(hp).condition];
      //if taicpu(hp).ops<>0 then
      //  begin
      //    sep:=#9;
      //    for i:=0 to taicpu(hp).ops-1 do
      //      begin
      //        s:=s+sep+getopstr(taicpu(hp).oper[i]^);
      //        sep:=',';
      //      end;
      //  end;
      //owner.writer.AsmWriteLn(s);
    end;


    procedure TSdccSdasZ80Assembler.WriteAsmList;
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


    function TSdccSdasZ80Assembler.MakeCmdLine: TCmdStr;
      begin
        result := {'-mmcu='+lower(cputypestr[current_settings.cputype])+' '+}inherited MakeCmdLine;
      end;


    const
       as_sdcc_sdasZ80_asm_info : tasminfo =
          (
            id     : as_sdcc_sdasz80;

            idtxt  : 'SDCC-SDASZ80';
            asmbin : 'sdcc-sdasz80';
            asmcmd : '-o $OBJ $EXTRAOPT $ASM';
            supported_targets : [system_Z80_embedded];
            flags : [af_needar,af_smartlink_sections];
            labelprefix : '.L';
            comment : '; ';
            dollarsign: 's';
          );


begin
  RegisterAssembler(as_sdcc_sdasZ80_asm_info,TSdccSdasZ80Assembler);
end.
