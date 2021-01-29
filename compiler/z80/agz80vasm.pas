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
{ This unit implements the assembler writer for the vasm assembler:
  http://sun.hasenbraten.de/vasm/
}

unit agz80vasm;

{$i fpcdefs.inc}

  interface

    uses
       globtype,systems,
       aasmbase,aasmtai,aasmdata,aasmcpu,
       assemble,
       cpubase;

    type

      { TZ80vasm }

      TZ80Vasm=class(TExternalAssembler)
      private
        procedure WriteDecodedSleb128(a: int64);
        procedure WriteDecodedUleb128(a: qword);
        procedure WriteRealConstAsBytes(hp: tai_realconst; const dbdir: string; do_line: boolean);
        function sectionattrs(atype:TAsmSectiontype):string;
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
        procedure WriteSection(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder;secalign:longint;
          secflags:TSectionFlags=[];secprogbits:TSectionProgbits=SPB_None);
        procedure WriteInstruction(hp: taicpu);
        procedure WriteOper(const o:toper; opcode: tasmop;ops:longint;dest : boolean);
        procedure WriteOper_jmp(const o:toper; ai : taicpu);
        procedure WriteExternals;
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
{$ifdef FPC_SOFT_FPUX80}
       sfpux80,
{$endif FPC_SOFT_FPUX80}
       finput;

    const
      line_length = 70;
      max_tokens : longint = 25;
      ait_const2str : array[aitconst_128bit..aitconst_64bit_unaligned] of string[20]=(
        #9''#9,#9'FIXMEDQ'#9,#9'FIXMEDD'#9,#9'.word'#9,#9'.byte'#9,
        #9'FIXMESLEB',#9'FIXEMEULEB',
        #9'FIXMEDD RVA'#9,#9'FIXMEDD SECREL32'#9,
        #9'FIXME',#9'FIXME',#9'FIXME',#9'FIXME',
        #9'.uahalf'#9,#9'FIXMEDD'#9,#9'FIXMEDQ'#9
      );

    procedure TZ80vasm.WriteDecodedSleb128(a: int64);
      var
        i,len : longint;
        buf   : array[0..255] of byte;
      begin
        writer.AsmWrite(#9'.byte'#9);
        len:=EncodeSleb128(a,buf,0);
        for i:=0 to len-1 do
          begin
            if (i > 0) then
              writer.AsmWrite(',');
            writer.AsmWrite(tostr(buf[i]));
          end;
        writer.AsmWriteLn(#9'; sleb '+tostr(a));
      end;

    procedure TZ80vasm.WriteDecodedUleb128(a: qword);
      var
        i,len : longint;
        buf   : array[0..63] of byte;
      begin
        writer.AsmWrite(#9'.byte'#9);
        len:=EncodeUleb128(a,buf,0);
        for i:=0 to len-1 do
          begin
            if (i > 0) then
              writer.AsmWrite(',');
            writer.AsmWrite(tostr(buf[i]));
          end;
        writer.AsmWriteLn(#9'; uleb '+tostr(a));
      end;

    procedure TZ80vasm.WriteRealConstAsBytes(hp: tai_realconst; const dbdir: string; do_line: boolean);
      var
        pdata: pbyte;
        index, step, swapmask, count: longint;
        ssingle: single;
        ddouble: double;
        ccomp: comp;
{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
        eextended: extended;
{$else}
{$ifdef FPC_SOFT_FPUX80}
        eextended: floatx80;
{$endif}
{$endif cpuextended}
      begin
        if do_line then
          begin
            case tai_realconst(hp).realtyp of
              aitrealconst_s32bit:
                writer.AsmWriteLn(asminfo^.comment+'value: '+single2str(tai_realconst(hp).value.s32val));
              aitrealconst_s64bit:
                writer.AsmWriteLn(asminfo^.comment+'value: '+double2str(tai_realconst(hp).value.s64val));
{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
              { can't write full 80 bit floating point constants yet on non-x86 }
              aitrealconst_s80bit:
                writer.AsmWriteLn(asminfo^.comment+'value: '+extended2str(tai_realconst(hp).value.s80val));
{$else}
{$ifdef FPC_SOFT_FPUX80}
{$push}{$warn 6018 off} { Unreachable code due to compile time evaluation }
             aitrealconst_s80bit:
               begin
                 if sizeof(tai_realconst(hp).value.s80val) = sizeof(double) then
                   writer.AsmWriteLn(asminfo^.comment+'value: '+double2str(tai_realconst(hp).value.s80val))
                 else if sizeof(tai_realconst(hp).value.s80val) = sizeof(single) then
                   writer.AsmWriteLn(asminfo^.comment+'value: '+single2str(tai_realconst(hp).value.s80val))
                else
                  internalerror(2017091906);
              end;
{$pop}
{$endif}
{$endif cpuextended}
              aitrealconst_s64comp:
                writer.AsmWriteLn(asminfo^.comment+'value: '+extended2str(tai_realconst(hp).value.s64compval));
              else
                internalerror(2014050602);
            end;
          end;
        writer.AsmWrite(dbdir);
        { generic float writing code: get start address of value, then write
          byte by byte. Can't use fields directly, because e.g ts64comp is
          defined as extended on x86 }
        case tai_realconst(hp).realtyp of
          aitrealconst_s32bit:
            begin
              ssingle:=single(tai_realconst(hp).value.s32val);
              pdata:=@ssingle;
            end;
          aitrealconst_s64bit:
            begin
              ddouble:=double(tai_realconst(hp).value.s64val);
              pdata:=@ddouble;
            end;
{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
          { can't write full 80 bit floating point constants yet on non-x86 }
          aitrealconst_s80bit:
            begin
              eextended:=extended(tai_realconst(hp).value.s80val);
              pdata:=@eextended;
            end;
{$else}
{$ifdef FPC_SOFT_FPUX80}
{$push}{$warn 6018 off} { Unreachable code due to compile time evaluation }
          aitrealconst_s80bit:
            begin
              if sizeof(tai_realconst(hp).value.s80val) = sizeof(double) then
                eextended:=float64_to_floatx80(float64(double(tai_realconst(hp).value.s80val)))
              else if sizeof(tai_realconst(hp).value.s80val) = sizeof(single) then
                eextended:=float32_to_floatx80(float32(single(tai_realconst(hp).value.s80val)))
              else
                internalerror(2017091907);
              pdata:=@eextended;
            end;
{$pop}
{$endif}
{$endif cpuextended}
          aitrealconst_s64comp:
            begin
              ccomp:=comp(tai_realconst(hp).value.s64compval);
              pdata:=@ccomp;
            end;
          else
            internalerror(2014051003);
        end;
        count:=tai_realconst(hp).datasize;
        { write bytes in inverse order if source and target endianess don't
          match }
        if source_info.endian<>target_info.endian then
          begin
            { go from back to front }
            index:=count-1;
            step:=-1;
          end
        else
          begin
            index:=0;
            step:=1;
          end;
{$ifdef ARM}
        { ARM-specific: low and high dwords of a double may be swapped }
        if tai_realconst(hp).formatoptions=fo_hiloswapped then
          begin
            { only supported for double }
            if tai_realconst(hp).datasize<>8 then
              internalerror(2014050609);
            { switch bit of the index so that the words are written in
              the opposite order }
            swapmask:=4;
          end
        else
{$endif ARM}
          swapmask:=0;
        repeat
          writer.AsmWrite(tostr(pdata[index xor swapmask]));
          inc(index,step);
          dec(count);
          if count<>0 then
            writer.AsmWrite(',');
        until count=0;
        { padding }
        for count:=tai_realconst(hp).datasize+1 to tai_realconst(hp).savesize do
          writer.AsmWrite(',0');
        writer.AsmLn;
      end;

    function TZ80vasm.sectionattrs(atype:TAsmSectiontype):string;
      begin
        case atype of
          sec_code, sec_fpc, sec_init, sec_fini:
            result:='acrx';
          sec_data, sec_rodata, sec_rodata_norel, sec_bss, sec_threadvar:
            result:='adrw';
          sec_stab, sec_stabstr:
            result:='dr';
          else
            result:='';
        end;
      end;


    function TZ80vasm.sectionname(atype: TAsmSectiontype;
        const aname: string; aorder: TAsmSectionOrder): string;
      const
        secnames : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
          '.text',
          '.data',
          '.data',
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
      var
        sep: string[3];
      begin
        if atype=sec_user then
          result:=aname
        else
          result:=secnames[atype];

        if (aname<>'') then
          begin
            case aorder of
              secorder_begin :
                sep:='.b_';
              secorder_end :
                sep:='.z_';
              else
                sep:='.n_';
            end;
            result:=result+sep+aname;
          end;
{        else
          result:=secname;}
      end;

    procedure TZ80vasm.WriteSection(atype: TAsmSectiontype;
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
        writer.AsmWrite(#9'.section ');
        { sectionname may rename those sections, so we do not write flags/progbits for them,
          the assembler will ignore them/spite out a warning anyways }
        if not(atype in [sec_data,sec_rodata,sec_rodata_norel]) then
          begin
            sectionflags:=true;
            sectionprogbits:=true;
          end;
        s:=sectionname(atype,aname,aorder);
        writer.AsmWrite(s);
        s:=sectionattrs(atype);
        if (s<>'') then
          writer.AsmWrite(',"'+s+'"');
        writer.AsmLn;
        LastSecType:=atype;
      end;

    procedure TZ80vasm.WriteInstruction(hp: taicpu);
      var
        i: Integer;
      begin
        if hp.opcode=A_JRJP then
          writer.AsmWrite(#9#9'jp')
        else
          writer.AsmWrite(#9#9+std_op2str[hp.opcode]);
        if (taicpu(hp).ops<>0) or (hp.condition<>C_None) then
          begin
            writer.AsmWrite(#9);
            if hp.condition<>C_None then
              begin
                writer.AsmWrite(uppercond2str[hp.condition]);
                if taicpu(hp).ops<>0 then
                  writer.AsmWrite(',');
              end;
            for i:=0 to taicpu(hp).ops-1 do
              begin
                if i<>0 then
                  writer.AsmWrite(',');
                if is_calljmp(hp.opcode) then
                  WriteOper_jmp(taicpu(hp).oper[i]^,hp)
                else
                  WriteOper(taicpu(hp).oper[i]^,hp.opcode,taicpu(hp).ops,(i=2));
              end;
          end;
        writer.AsmLn;
      end;

    procedure TZ80vasm.WriteOper(const o: toper; opcode: tasmop; ops: longint; dest: boolean);
      var
        need_plus: Boolean;
      begin
        case o.typ of
          top_reg :
            writer.AsmWrite(std_regname(o.reg));
          top_const :
            begin
              writer.AsmWrite(tostr(longint(o.val)));
            end;
          top_ref:
            begin
              if assigned(o.ref^.symbol) and (o.ref^.refaddr in [addr_lo8,addr_hi8,addr_full]) then
                begin
                  {if SmartAsm then
                    AddSymbol(o.ref^.symbol.name,false);}
                  if (o.ref^.base<>NR_NO) or (o.ref^.index<>NR_NO) then
                    internalerror(2020041102);
                 // writer.AsmWrite('#');
                  case o.ref^.refaddr of
                    addr_lo8:
                      writer.AsmWrite('<');
                    addr_hi8:
                      writer.AsmWrite('>');
                    addr_full:
                      {nothing};
                    else
                      ;
                  end;
                  if o.ref^.offset<>0 then
                    writer.AsmWrite('('+ApplyAsmSymbolRestrictions(o.ref^.symbol.name)+'+'+tostr(o.ref^.offset)+')')
                  else
                    writer.AsmWrite(ApplyAsmSymbolRestrictions(o.ref^.symbol.name));
                end
              else if not assigned(o.ref^.symbol) and
                 ((o.ref^.base<>NR_NO) or (o.ref^.index<>NR_NO)) and
                 (o.ref^.offset<>0) then
                begin
                  //writer.AsmWrite(tostr(o.ref^.offset));
                  writer.AsmWrite('(');
                  if o.ref^.base<>NR_NO then
                    begin
                      if o.ref^.index<>NR_NO then
                        internalerror(2020040204);
                      writer.AsmWrite(std_regname(o.ref^.base));
                    end
                  else if o.ref^.index<>NR_NO then
                    begin
                      if o.ref^.scalefactor>1 then
                        internalerror(2020040207);
                      writer.AsmWrite(std_regname(o.ref^.index));
                    end;
                  if o.ref^.offset > 0 then
                    writer.AsmWrite('+'+tostr(o.ref^.offset))
                  else
                    if o.ref^.offset < 0 then
                      writer.AsmWrite(tostr(o.ref^.offset));
                  writer.AsmWrite(')');
                end
              else
                begin
                  writer.AsmWrite('(');
                  need_plus:=false;
                  if o.ref^.base<>NR_NO then
                    begin
                      if o.ref^.index<>NR_NO then
                        internalerror(2020040205);
                      writer.AsmWrite(std_regname(o.ref^.base));
                      need_plus:=true;
                    end
                  else if o.ref^.index<>NR_NO then
                    begin
                      if o.ref^.scalefactor>1 then
                        internalerror(2020040208);
                      writer.AsmWrite(std_regname(o.ref^.index));
                      need_plus:=true;
                    end;
                  if assigned(o.ref^.symbol) then
                    begin
                      {if SmartAsm then
                        AddSymbol(o.ref^.symbol.name,false);}
                      if need_plus then
                        writer.AsmWrite('+');
                      writer.AsmWrite(ApplyAsmSymbolRestrictions(o.ref^.symbol.name));
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
            internalerror(2020100805);
        end;
      end;

    procedure TZ80vasm.WriteOper_jmp(const o: toper; ai: taicpu);
      begin
        case o.typ of
          top_reg :
            writer.AsmWrite(std_regname(o.reg));
          top_const :
            begin
              writer.AsmWrite(tostr(longint(o.val)));
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
                  writer.AsmWrite(ApplyAsmSymbolRestrictions(o.ref^.symbol.name));
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
            internalerror(2020100806);
        end;
      end;

    procedure TZ80vasm.WriteExternals;
      var
        sym : TAsmSymbol;
        i   : longint;
      begin
        writer.AsmWriteln('; Begin externals');
        for i:=0 to current_asmdata.AsmSymbolDict.Count-1 do
          begin
            sym:=TAsmSymbol(current_asmdata.AsmSymbolDict[i]);
            if sym.bind in [AB_EXTERNAL,AB_EXTERNAL_INDIRECT] then
              writer.AsmWriteln(#9'.globl'#9+ApplyAsmSymbolRestrictions(sym.name));
          end;
        writer.AsmWriteln('; End externals');
      end;

    procedure TZ80vasm.WriteTree(p: TAsmList);

      procedure doalign(alignment: byte; use_op: boolean; fillop: byte; maxbytes: byte; out last_align: longint;lasthp:tai);
        var
          i: longint;
          alignment64 : int64;
        begin
          last_align:=alignment;
          if alignment>1 then
            writer.AsmWriteLn(#9'.balign '+tostr(alignment));
        end;

    var
      lasthp,
      hp: tai;
      s, LastSecName: string;
      counter,lines,i,j,l,tokens,pos,last_align: longint;
      quoted, do_line: Boolean;
      consttype: taiconst_type;
      ch: Char;
      InlineLevel : longint;
      prevfileinfo : tfileposinfo;
      previnfile : tinputfile;
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
                    writer.AsmWrite(ApplyAsmSymbolRestrictions(tai_label(hp).labsym.name));
                    if tai_label(hp).labsym.bind in [AB_GLOBAL,AB_PRIVATE_EXTERN] then
                      writer.AsmWriteLn(':')
                    else
                      writer.AsmWriteLn(':');
                  end;
              end;
            ait_symbol :
              begin
                if not(tai_symbol(hp).has_value) then
                  begin
                    if tai_symbol(hp).is_global then
                      writer.AsmWriteLn(#9'.globl '+ApplyAsmSymbolRestrictions(tai_symbol(hp).sym.name));
                    writer.AsmWriteLn(ApplyAsmSymbolRestrictions(tai_symbol(hp).sym.name) + ':');
                  end
                else
                  begin
                    if tai_symbol(hp).is_global then
                      writer.AsmWriteLn(#9'.globl '+ApplyAsmSymbolRestrictions(tai_symbol(hp).sym.name));
                    writer.AsmWriteLn(ApplyAsmSymbolRestrictions(tai_symbol(hp).sym.name) + '=' + tostr(tai_symbol(hp).value));
                  end;
              end;
            ait_symbol_end :
              begin
              end;
            ait_datablock :
              begin
                if tai_datablock(hp).is_global or SmartAsm then
                  writer.AsmWriteLn(#9'.globl '+ApplyAsmSymbolRestrictions(tai_datablock(hp).sym.name));
                writer.AsmWriteLn(ApplyAsmSymbolRestrictions(tai_datablock(hp).sym.name) + ':');
                {if SmartAsm then
                  AddSymbol(tai_datablock(hp).sym.name,true);}
                writer.AsmWriteLn(#9'.zero'#9+tostr(tai_datablock(hp).size));
              end;
            ait_realconst:
              WriteRealConstAsBytes(tai_realconst(hp),#9'.byte'#9,do_line);
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
                      writer.AsmWrite(#9'.uahalf'#9);
                      l:=0;
                      tokens:=1;
                      repeat
                        if assigned(tai_const(hp).sym) then
                          begin
                            if assigned(tai_const(hp).endsym) then
                              s:=ApplyAsmSymbolRestrictions(tai_const(hp).endsym.name)+'-'+ApplyAsmSymbolRestrictions(tai_const(hp).sym.name)
                            else
                              s:=ApplyAsmSymbolRestrictions(tai_const(hp).sym.name);
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
                              s:=ApplyAsmSymbolRestrictions(tai_const(hp).endsym.name)+'-'+ApplyAsmSymbolRestrictions(tai_const(hp).sym.name)
                            else
                              s:=ApplyAsmSymbolRestrictions(tai_const(hp).sym.name);
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
            ait_cutobject :
              begin
                if SmartAsm then
                 begin
                  { only reset buffer if nothing has changed }
                  if not writer.ClearIfEmpty then
                   begin
                     {if SmartAsm then
                       begin
                         WriteSmartExternals;
                         FreeExternChainList;
                       end;
                     WriteGroups;}
                     writer.AsmClose;
                     DoAssemble;
                     writer.AsmCreate(tai_cutobject(hp).place);
                     {ResetSectionsList;
                     WriteHeader;}
                   end;
                { avoid empty files }
                  LastSecType:=sec_none;
                  LastSecName:='';
                  LastSecOrder:=secorder_default;
                  LastAlign:=1;
                  while assigned(hp.next) and (tai(hp.next).typ in [ait_cutobject,ait_section,ait_comment]) do
                   begin
                     if tai(hp.next).typ=ait_section then
                       begin
                         LastSecType:=tai_section(hp.next).sectype;
                         LastSecName:=tai_section(hp.next).name^;
                         LastSecOrder:=tai_section(hp.next).secorder;
                         LastAlign:=tai_section(hp.next).secalign;
                       end;
                     hp:=tai(hp.next);
                   end;
                  if LastSecType<>sec_none then
                    WriteSection(LastSecType,LastSecName,LastSecOrder,LastAlign);
                  writer.MarkEmpty;
                  //NewObject:=true;
                end;
              end;
            ait_marker :
              if tai_marker(hp).kind=mark_NoLineInfoStart then
                inc(InlineLevel)
              else if tai_marker(hp).kind=mark_NoLineInfoEnd then
                dec(InlineLevel);
            ait_stab,
            ait_force_line,
            ait_function_name : ;
            else
              if not WriteComments(hp) then
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


    procedure TZ80vasm.WriteAsmList;
      var
        hal: TAsmListType;
      begin
        WriteExternals;

        for hal:=low(TasmlistType) to high(TasmlistType) do
          begin
            writer.AsmWriteLn(asminfo^.comment+'Begin asmlist '+AsmListTypeStr[hal]);
            writetree(current_asmdata.asmlists[hal]);
            writer.AsmWriteLn(asminfo^.comment+'End asmlist '+AsmListTypeStr[hal]);
          end;
      end;


    function TZ80vasm.MakeCmdLine: TCmdStr;
      begin
        result := {'-mmcu='+lower(cputypestr[current_settings.cputype])+' '+}inherited MakeCmdLine;
      end;


    const
       as_z80_vasm_info : tasminfo =
          (
            id     : as_z80_vasm;

            idtxt  : 'VASM';
            asmbin : 'vasmz80_std';
            asmcmd : '-quiet -Fvobj -o $OBJ $EXTRAOPT $ASM';
            supported_targets : [system_z80_embedded, system_z80_zxspectrum, system_z80_msxdos];
            flags : [af_needar,af_smartlink_sections];
            labelprefix : '.L';
            labelmaxlen : -1;
            comment : '; ';
            dollarsign: '$';
          );


begin
  RegisterAssembler(as_z80_vasm_info,TZ80vasm);
end.
