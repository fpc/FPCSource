{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an asmoutput class for the Nasm assembler with
    Intel syntax for the i386+

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
unit agx86nsm;

{$i fpcdefs.inc}

interface

    uses
      cclasses,cpubase,globtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,assemble,cgutils;

    type

      { TX86NasmAssembler }

      TX86NasmAssembler = class(texternalassembler)
      private
        FSectionsUsed: TFPHashList;
        FSectionsInDGROUP: TFPHashList;
        using_relative : boolean;
        function CodeSectionName(const aname:string): string;
        procedure WriteReference(var ref : treference);
        procedure WriteOper(const o:toper;s : topsize; opcode: tasmop;ops:longint;dest : boolean);
        procedure WriteOper_jmp(const o:toper; ai : taicpu);
        procedure WriteSection(atype:TAsmSectiontype;const aname:string;alignment : byte);
        procedure ResetSectionsList;
        procedure WriteGroups;
      protected
        function single2str(d: single): string; override;
        function double2str(d: double): string; override;
        function extended2str(e: extended): string; override;
      public
        destructor Destroy;override;
        procedure WriteTree(p:TAsmList);override;
        procedure WriteAsmList;override;
        procedure WriteExternals;
        procedure WriteSmartExternals;
        procedure WriteHeader;
        function  MakeCmdLine: TCmdStr;override;
      end;



  implementation

    uses
      cutils,globals,systems,
      fmodule,finput,verbose,cpuinfo,cgbase,omfbase
      ;

    const
      line_length = 64;

      nasm_regname_table : array[tregisterindex] of string[7] = (
        {r386nasm.inc contains the Nasm name of each register.}
{$if defined(x86_64)}
        {$i r8664nasm.inc}
{$elseif defined(i386)}
        {$i r386nasm.inc}
{$elseif defined(i8086)}
        {$i r8086nasm.inc}
{$endif}
      );
      nasm_cpu_name : array[tcputype] of string = (
{$if defined(x86_64)}
        '',            // cpu_none,
        'X64',         // cpu_athlon64,
        'X64',         // cpu_core_i,
        'X64',         // cpu_core_avx,
        'X64'          // cpu_core_avx2
{$elseif defined(i386)}
        '',         // cpu_none,
        '386',      // cpu_386,
        'PENTIUM',  // cpu_Pentium,
        'P2',       // cpu_Pentium2,
        'P3',       // cpu_Pentium3,
        'P4',       // cpu_Pentium4,
        'P4',       // cpu_PentiumM,
        'PRESCOTT', // cpu_core_i,
        'PRESCOTT', // cpu_core_avx,
        'PRESCOTT'  // cpu_core_avx2
{$elseif defined(i8086)}
        '',        // cpu_none
        '8086',    // cpu_8086
        '186',     // cpu_186
        '286',     // cpu_286
        '386',     // cpu_386
        'PENTIUM', // cpu_Pentium
        'P2',      // cpu_Pentium2
        'P3',      // cpu_Pentium3
        'P4',      // cpu_Pentium4
        'P4'       // cpu_PentiumM
{$endif}
      );

    function nasm_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=nasm_regname_table[p]
        else
          result:=generic_regname(r);
      end;


    function TX86NasmAssembler.single2str(d: single): string;
      var
         hs : string;
         p : longint;
      begin
         str(d,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         single2str:=lower(hs);
      end;


    function TX86NasmAssembler.double2str(d: double): string;
      var
         hs : string;
         p : longint;
      begin
         str(d,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         double2str:=lower(hs);
      end;


    function TX86NasmAssembler.extended2str(e: extended): string;
      var
         hs : string;
         p : longint;
      begin
         str(e,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         extended2str:=lower(hs);
      end;


    destructor TX86NasmAssembler.Destroy;
      begin
        FSectionsUsed.Free;
        FSectionsInDGROUP.Free;
        inherited Destroy;
      end;


    function sizestr(s:topsize;dest:boolean):string;
      begin
        case s of
           S_B : sizestr:='byte ';
           S_W : sizestr:='word ';
           S_L : sizestr:='dword ';
           S_Q : sizestr:='qword ';
           S_IS : sizestr:='word ';
           S_IL : sizestr:='dword ';
           S_IQ : sizestr:='qword ';
           S_FS : sizestr:='dword ';
           S_FL : sizestr:='qword ';
           S_FX : sizestr:='tword ';
           S_BW : if dest then
               sizestr:='word '
             else
               sizestr:='byte ';
           S_BL : if dest then
               sizestr:='dword '
             else
               sizestr:='byte ';
           S_WL : if dest then
               sizestr:='dword '
             else
               sizestr:='word ';
{$ifdef x86_64}
           S_BQ : if dest then
                   sizestr:='qword '
                  else
                   sizestr:='byte ';
           S_WQ : if dest then
                   sizestr:='qword '
                  else
                   sizestr:='word ';
           S_LQ : if dest then
                   sizestr:='qword '
                  else
                   sizestr:='dword ';
           { Nothing needed for XMM registers }
           S_XMM: sizestr:='';

{$endif x86_64}
          else { S_NO }
            sizestr:='';
        end;
      end;


    Function PadTabs(const p:string;addch:char):string;
      var
        s : string;
        i : longint;
      begin
        i:=length(p);
        if addch<>#0 then
         begin
           inc(i);
           s:=p+addch;
         end
        else
         s:=p;
        if i<8 then
         PadTabs:=s+#9#9
        else
         PadTabs:=s+#9;
      end;


    type
      PExternChain = ^TExternChain;

      TExternChain = Record
        psym : pshortstring;
        is_defined : boolean;
        next : PExternChain;
      end;

    const
      FEC : PExternChain = nil;

    procedure AddSymbol(symname : string; defined : boolean);
    var
       EC : PExternChain;
    begin
      EC:=FEC;
      while assigned(EC) do
        begin
          if EC^.psym^=symname then
            begin
              if defined then
                EC^.is_defined:=true;
              exit;
            end;
          EC:=EC^.next;
        end;
      New(EC);
      EC^.next:=FEC;
      FEC:=EC;
      FEC^.psym:=stringdup(symname);
      FEC^.is_defined := defined;
    end;

    procedure FreeExternChainList;
    var
       EC : PExternChain;
    begin
      EC:=FEC;
      while assigned(EC) do
        begin
          FEC:=EC^.next;
          stringdispose(EC^.psym);
          Dispose(EC);
          EC:=FEC;
        end;
    end;

{****************************************************************************
                               TX86NasmAssembler
 ****************************************************************************}


    function TX86NasmAssembler.CodeSectionName(const aname:string): string;
      begin
{$ifdef i8086}
        if current_settings.x86memorymodel in x86_far_code_models then
          begin
            if cs_huge_code in current_settings.moduleswitches then
              result:=aname + '_TEXT'
            else
              result:=current_module.modulename^ + '_TEXT';
          end
        else
          result:='_TEXT';
{$else i8086}
        result:='.text';
{$endif}
      end;


    procedure TX86NasmAssembler.WriteReference(var ref : treference);
      var
        first : boolean;
        base_done : boolean;
      begin
        with ref do
         begin
           writer.AsmWrite('[');
           first:=true;
           base_done:=false;
           if (segment<>NR_NO) then
             writer.AsmWrite(nasm_regname(segment)+':');
{$ifdef x86_64}
          if (base=NR_RIP) then
            begin
              { nasm RIP is implicit for pic }
              if not (ref.refaddr in [addr_pic,addr_pic_no_got]) and not using_relative then
                writer.AsmWrite('$ + ');
              base_done:=true;
            end;
{$endif x86_64}
           if assigned(symbol) then
            begin
              writer.AsmWrite(symbol.name);
              if SmartAsm then
                AddSymbol(symbol.name,false);
              first:=false;
            end;
           if (base<>NR_NO) and not base_done then
            begin
              if not(first) then
               writer.AsmWrite('+')
              else
               first:=false;
              writer.AsmWrite(nasm_regname(base))
            end;
           if (index<>NR_NO) then
             begin
               if not(first) then
                 writer.AsmWrite('+')
               else
                 first:=false;
               writer.AsmWrite(nasm_regname(index));
               if scalefactor<>0 then
                 writer.AsmWrite('*'+tostr(scalefactor));
             end;
           if offset<0 then
             begin
               writer.AsmWrite(tostr(offset));
               first:=false;
             end
           else if (offset>0) then
             begin
               writer.AsmWrite('+'+tostr(offset));
               first:=false;
             end;
           if first then
             writer.AsmWrite('0');
           writer.AsmWrite(']');
         end;
       end;


    procedure TX86NasmAssembler.WriteOper(const o:toper;s : topsize; opcode: tasmop;ops:longint;dest : boolean);
      begin
        case o.typ of
          top_reg :
            writer.AsmWrite(nasm_regname(o.reg));
          top_const :
            begin
              if (ops=1) and (opcode<>A_RET) then
               writer.AsmWrite(sizestr(s,dest));
              writer.AsmWrite(tostr(longint(o.val)));
            end;
          top_ref :
            begin
              if o.ref^.refaddr in [addr_no,addr_pic,addr_pic_no_got] then
                begin
                  if not ((opcode = A_LEA) or (opcode = A_LGS) or
                          (opcode = A_LSS) or (opcode = A_LFS) or
{$ifndef x86_64}
                          (opcode = A_LES) or (opcode = A_LDS) or
{$endif x86_64}
                         // (opcode = A_SHR) or (opcode = A_SHL) or
                         // (opcode = A_SAR) or (opcode = A_SAL) or
                          (opcode = A_OUT) or (opcode = A_IN)) then
                    writer.AsmWrite(sizestr(s,dest));
                  WriteReference(o.ref^);
                end
{$ifdef i8086}
              else if o.ref^.refaddr=addr_dgroup then
                begin
                  writer.AsmWrite('DGROUP');
                end
              else if o.ref^.refaddr=addr_fardataseg then
                begin
                  writer.AsmWrite(current_module.modulename^+'_DATA');
                end
{$endif i8086}
              else
                begin
{$ifdef x86_64}
                  if s=S_L then
                    writer.AsmWrite('dword ')
                  else
                    writer.AsmWrite('qword ');
{$endif}
{$ifdef i386}
                  writer.AsmWrite('dword ');
{$endif i386}
{$ifdef i8086}
                  if o.ref^.refaddr=addr_seg then
                    writer.AsmWrite('SEG ')
                  else
                    writer.AsmWrite('word ');
{$endif i8086}
                  if assigned(o.ref^.symbol) then
                   begin
                    if SmartAsm then
                      AddSymbol(o.ref^.symbol.name,false);
                    writer.AsmWrite(o.ref^.symbol.name);
                    if o.ref^.offset=0 then
                      exit;
                   end;
                  if o.ref^.offset>0 then
                   writer.AsmWrite('+');
                  writer.AsmWrite(tostr(o.ref^.offset));
                end;
            end;
          else
            internalerror(10001);
        end;
      end;


    procedure TX86NasmAssembler.WriteOper_jmp(const o:toper; ai : taicpu);
      begin
        case o.typ of
          top_reg :
            writer.AsmWrite(nasm_regname(o.reg));
          top_ref :
            if o.ref^.refaddr=addr_no then
              begin
                if ai.opsize=S_FAR then
                  writer.AsmWrite('far ');
                WriteReference(o.ref^);
              end
            else
              begin
                if ai.opsize=S_FAR then
                  writer.AsmWrite('far ');
                { else
                   writer.AsmWrite('near ') just disables short branches, increasing code size. 
                   Omitting it does not cause any bad effects, tested with nasm 2.11. }

                writer.AsmWrite(o.ref^.symbol.name);
                if SmartAsm then
                  AddSymbol(o.ref^.symbol.name,false);
                if o.ref^.offset>0 then
                 writer.AsmWrite('+'+tostr(o.ref^.offset))
                else
                 if o.ref^.offset<0 then
                  writer.AsmWrite(tostr(o.ref^.offset));
              end;
          top_const :
            writer.AsmWrite(tostr(aint(o.val)));
          else
            internalerror(10001);
        end;
      end;


    const
      ait_const2str : array[aitconst_128bit..aitconst_64bit_unaligned] of string[30]=(
        #9'FIXME_128BIT'#9,#9'DQ'#9,#9'DD'#9,#9'DW'#9,#9'DB'#9,
        #9'FIXME_SLEB128BIT'#9,#9'FIXME_ULEB128BIT'#9,
        #9'RVA'#9,#9'SECREL32'#9,#9'FIXME_darwin_dwarf_delta64'#9,
        #9'FIXME_darwin_dwarf_delta32'#9,#9'FIXME_half16bit'#9,#9'FIXME_gs'#9,
        #9'DW'#9,#9'DD'#9,#9'FIXME_64BIT_UNALIGNED'#9
      );

    procedure TX86NasmAssembler.WriteSection(atype : TAsmSectiontype;
      const aname : string; alignment : byte);
      const
        secnames : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
          '.text',
          '.data',
          '.data',
          '.rodata',
          '.bss',
          '.tbss',
          '.pdata',
          '.text','.data','.data','.data','.data',
          '.stab',
          '.stabstr',
          '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev',
          '.fpc',
          '',
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
          '.heap'
        );
      var
        secname: string;
      begin
        writer.AsmLn;
        writer.AsmWrite('SECTION ');
        { go32v2 stub only loads .text and .data sections, and allocates space for .bss.
          Thus, data which normally goes into .rodata and .rodata_norel sections must
          end up in .data section }
        if (atype in [sec_rodata,sec_rodata_norel]) and
          (target_info.system=system_i386_go32v2) then
          writer.AsmWrite('.data')
        else if (atype=sec_user) then
          writer.AsmWrite(aname)
        else if (atype=sec_threadvar) and
          (target_info.system in (systems_windows+systems_wince)) then
          writer.AsmWrite('.tls'#9'bss')
        else if target_info.system in [system_i8086_msdos,system_i8086_win16] then
          begin
            if secnames[atype]='.text' then
              secname:=CodeSectionName(aname)
            else if omf_segclass(atype)='FAR_DATA' then
              secname:=current_module.modulename^ + '_DATA'
            else
              secname:=omf_secnames[atype];
            writer.AsmWrite(secname);
            { first use of this section in the object file? }
            if FSectionsUsed.FindIndexOf(secname)=-1 then
              begin
                { yes -> write the section attributes as well }
                if atype=sec_stack then
                  writer.AsmWrite(' stack');
                if atype in [sec_debug_frame,sec_debug_info,sec_debug_line,sec_debug_abbrev] then
                  writer.AsmWrite(' use32')
                else
                  writer.AsmWrite(' use16');
                writer.AsmWrite(' class='+omf_segclass(atype)+
                  ' align='+tostr(omf_sectiontype2align(atype)));
                FSectionsUsed.Add(secname,Pointer(self));
                if section_belongs_to_dgroup(atype) then
                  FSectionsInDGROUP.Add(secname,Pointer(self));
              end;
          end
        else if secnames[atype]='.text' then
          writer.AsmWrite(CodeSectionName(aname))
        else
          writer.AsmWrite(secnames[atype]);
        if create_smartlink_sections and
           (atype<>sec_bss) and
           (aname<>'') then
          begin
            writer.AsmWrite('.');
            writer.AsmWrite(aname);
            if atype in [sec_init, sec_fini, sec_stub, sec_code] then
              writer.AsmWrite(' code align='+tostr(alignment))
            else if  atype in [sec_rodata, sec_rodata_norel] then
              writer.AsmWrite(' rdata align='+tostr(alignment))
            else
              writer.AsmWrite(' data align='+tostr(alignment))
          end;
        writer.AsmLn;
        LastSecType:=atype;
      end;

    procedure TX86NasmAssembler.ResetSectionsList;
      begin
        FSectionsUsed.Free;
        FSectionsUsed:=TFPHashList.Create;
        FSectionsInDGROUP.Free;
        FSectionsInDGROUP:=TFPHashList.Create;
      end;

    procedure TX86NasmAssembler.WriteGroups;
      {$ifdef i8086}
      var
        i: Integer;
      {$endif i8086}
      begin
{$ifdef i8086}
        if target_info.system in [system_i8086_msdos,system_i8086_win16] then
          begin
            if current_settings.x86memorymodel=mm_huge then
              WriteSection(sec_data,'',2);
            writer.AsmLn;
            writer.AsmWrite('GROUP DGROUP');
            for i:=0 to FSectionsInDGROUP.Count-1 do
              writer.AsmWrite(' '+FSectionsInDGROUP.NameOfIndex(i));
            writer.AsmLn;
          end;
{$endif i8086}
      end;

    procedure TX86NasmAssembler.WriteTree(p:TAsmList);
{$ifdef cpuextended}
    type
      t80bitarray = array[0..9] of byte;
{$endif cpuextended}
    var
      s : string;
      hp       : tai;
      counter,
      lines,
      i,j,l    : longint;
      InlineLevel : longint;
      consttype : taiconst_type;
      do_line, SkipNewLine,
      quoted   : boolean;
      fixed_opcode: TAsmOp;
      prefix, LastSecName  : string;
      LastAlign : Byte;
      cpu: tcputype;
    begin
      if not assigned(p) then
       exit;
      InlineLevel:=0;
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
            current_filepos:=tailineinfo(hp).fileinfo;
            { no line info for inlined code }
            if do_line and (inlinelevel=0) then
              WriteSourceLine(hp as tailineinfo);
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
                 writer.AsmWriteLn(#9#9+asminfo^.comment+'Register '+nasm_regname(tai_regalloc(hp).reg)+' '+
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
                 WriteSection(tai_section(hp).sectype,tai_section(hp).name^,tai_section(hp).secalign);
               LastSecType:=tai_section(hp).sectype;
             end;

           ait_align :
             begin
               if (tai_align(hp).aligntype>1) then
                 begin
                   if (LastSecType=sec_bss) or (
                      (LastSecType=sec_threadvar) and
                      (target_info.system in (systems_windows+systems_wince))
                     ) then
                      writer.AsmWriteLn(#9'ALIGNB '+tostr(tai_align(hp).aligntype))
                    else if tai_align_abstract(hp).use_op then
                      writer.AsmWriteLn(#9'ALIGN '+tostr(tai_align(hp).aligntype)+',DB '+tostr(tai_align_abstract(hp).fillop))
                    else if LastSecType in [sec_code,sec_stub,sec_init,sec_fini] then
                      writer.AsmWriteLn(#9'ALIGN '+tostr(tai_align(hp).aligntype))
                    else
                      writer.AsmWriteLn(#9'ALIGN '+tostr(tai_align(hp).aligntype)+',DB 0');
                 end;
             end;

           ait_datablock :
             begin
               if tai_datablock(hp).is_global or SmartAsm then
                begin
                  writer.AsmWrite(#9'GLOBAL ');
                  writer.AsmWriteLn(tai_datablock(hp).sym.name);
                end;
               writer.AsmWrite(PadTabs(tai_datablock(hp).sym.name,':'));
               if SmartAsm then
                 AddSymbol(tai_datablock(hp).sym.name,true);
               writer.AsmWriteLn('RESB'#9+tostr(tai_datablock(hp).size));
             end;

           ait_const:
             begin
               consttype:=tai_const(hp).consttype;
               case consttype of
                 aitconst_64bit,
                 aitconst_64bit_unaligned:
                    begin
                      if assigned(tai_const(hp).sym) then
                        internalerror(200404292);
                      writer.AsmWrite(ait_const2str[aitconst_32bit]);
                      writer.AsmWrite(tostr(longint(lo(tai_const(hp).value))));
                      writer.AsmWrite(',');
                      writer.AsmWrite(tostr(longint(hi(tai_const(hp).value))));
                      writer.AsmLn;
                    end;
                 aitconst_uleb128bit,
                 aitconst_sleb128bit,
                 aitconst_128bit:
                    begin
                      writer.AsmWriteLn(asminfo^.comment+'Unsupported const type '+
                        ait_const2str[consttype]);
                    end;
{$ifdef i8086}
                 aitconst_farptr:
                   begin
                     writer.AsmWrite(ait_const2str[aitconst_16bit]);
                     if assigned(tai_const(hp).sym) then
                       begin
                         if SmartAsm then
                           AddSymbol(tai_const(hp).sym.name,false);
                         writer.AsmWrite(tai_const(hp).sym.name);
                         if tai_const(hp).value<>0 then
                           writer.AsmWrite(tostr_with_plus(tai_const(hp).value));
                         writer.AsmLn;
                         writer.AsmWrite(ait_const2str[aitconst_16bit]);
                         writer.AsmWrite('SEG ');
                         writer.AsmWrite(tai_const(hp).sym.name);
                       end
                     else
                       writer.AsmWrite(tostr(lo(longint(tai_const(hp).value)))+','+
                                tostr(hi(longint(tai_const(hp).value))));
                     writer.AsmLn;
                   end;
                 aitconst_seg:
                   begin
                     writer.AsmWrite(ait_const2str[aitconst_16bit]);
                     if assigned(tai_const(hp).sym) then
                       begin
                         if SmartAsm then
                           AddSymbol(tai_const(hp).sym.name,false);
                         writer.AsmWrite('SEG ');
                         writer.AsmWrite(tai_const(hp).sym.name);
                       end
                     else
                       internalerror(2015110501);
                     writer.AsmLn;
                   end;
                 aitconst_dgroup:
                   writer.AsmWriteLn(#9'DW'#9'DGROUP');
                 aitconst_fardataseg:
                   writer.AsmWriteLn(#9'DW'#9+current_module.modulename^+'_DATA');
{$endif i8086}
                 aitconst_32bit,
                 aitconst_16bit,
                 aitconst_8bit,
                 aitconst_rva_symbol,
                 aitconst_secrel32_symbol,
                 aitconst_16bit_unaligned,
                 aitconst_32bit_unaligned:
                   begin
                     writer.AsmWrite(ait_const2str[tai_const(hp).consttype]);
                     l:=0;
                     repeat
                       if assigned(tai_const(hp).sym) then
                         begin
                           if SmartAsm then
                             begin
                               AddSymbol(tai_const(hp).sym.name,false);
                               if assigned(tai_const(hp).endsym) then
                                 AddSymbol(tai_const(hp).endsym.name,false);
                             end;
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
                       if (l>line_length) or
                          (hp.next=nil) or
                          (tai(hp.next).typ<>ait_const) or
                          (tai_const(hp.next).consttype<>consttype) then
                         break;
                       hp:=tai(hp.next);
                       writer.AsmWrite(',');
                     until false;
                     writer.AsmLn;
                   end;
                 else
                   internalerror(200704252);
               end;
             end;

           ait_realconst:
             begin
               WriteRealConstAsBytes(tai_realconst(hp),#9#9'DB'#9,do_line);
             end;

           ait_string :
             begin
               counter := 0;
               lines := tai_string(hp).len div line_length;
             { separate lines in different parts }
               if tai_string(hp).len > 0 then
                Begin
                  for j := 0 to lines-1 do
                   begin
                     writer.AsmWrite(#9#9'DB'#9);
                     quoted:=false;
                     for i:=counter to counter+line_length-1 do
                        begin
                          { it is an ascii character. }
                          if (ord(tai_string(hp).str[i])>31) and
                             (ord(tai_string(hp).str[i])<128) and
                             (tai_string(hp).str[i]<>'"') then
                              begin
                                if not(quoted) then
                                    begin
                                      if i>counter then
                                        writer.AsmWrite(',');
                                      writer.AsmWrite('"');
                                    end;
                                writer.AsmWrite(tai_string(hp).str[i]);
                                quoted:=true;
                              end { if > 31 and < 128 and ord('"') }
                          else
                              begin
                                  if quoted then
                                      writer.AsmWrite('"');
                                  if i>counter then
                                      writer.AsmWrite(',');
                                  quoted:=false;
                                  writer.AsmWrite(tostr(ord(tai_string(hp).str[i])));
                              end;
                       end; { end for i:=0 to... }
                     if quoted then writer.AsmWrite('"');
                       writer.AsmWrite(target_info.newline);
                     inc(counter,line_length);
                  end; { end for j:=0 ... }
                { do last line of lines }
                if counter<tai_string(hp).len then
                  writer.AsmWrite(#9#9'DB'#9);
                quoted:=false;
                for i:=counter to tai_string(hp).len-1 do
                  begin
                    { it is an ascii character. }
                    if (ord(tai_string(hp).str[i])>31) and
                       (ord(tai_string(hp).str[i])<128) and
                       (tai_string(hp).str[i]<>'"') then
                        begin
                          if not(quoted) then
                              begin
                                if i>counter then
                                  writer.AsmWrite(',');
                                writer.AsmWrite('"');
                              end;
                          writer.AsmWrite(tai_string(hp).str[i]);
                          quoted:=true;
                        end { if > 31 and < 128 and " }
                    else
                        begin
                          if quoted then
                            writer.AsmWrite('"');
                          if i>counter then
                              writer.AsmWrite(',');
                          quoted:=false;
                          writer.AsmWrite(tostr(ord(tai_string(hp).str[i])));
                        end;
                  end; { end for i:=0 to... }
                if quoted then
                  writer.AsmWrite('"');
                end;
               writer.AsmLn;
             end;

           ait_label :
             begin
               if tai_label(hp).labsym.is_used then
                 begin
                   if SmartAsm and (tai_label(hp).labsym.bind=AB_GLOBAL) then
                     begin
                       writer.AsmWrite(#9'GLOBAL ');
                       writer.AsmWriteLn(tai_label(hp).labsym.name);
                     end;
                   writer.AsmWriteLn(tai_label(hp).labsym.name+':');
                 end;
               if SmartAsm then
                 AddSymbol(tai_label(hp).labsym.name,true);
             end;

           ait_symbol :
             begin
               if tai_symbol(hp).has_value then
                 internalerror(2009090803);
               if tai_symbol(hp).is_global or SmartAsm then
                begin
                  writer.AsmWrite(#9'GLOBAL ');
                  writer.AsmWriteLn(tai_symbol(hp).sym.name);
                end;
               writer.AsmWrite(tai_symbol(hp).sym.name);
               if SmartAsm then
                 AddSymbol(tai_symbol(hp).sym.name,true);
               if assigned(hp.next) and not(tai(hp.next).typ in
                  [ait_const,ait_realconst,ait_string]) then
                writer.AsmWriteLn(':')
             end;

           ait_symbol_end : ;

           ait_instruction :
             begin
               fixed_opcode:=taicpu(hp).FixNonCommutativeOpcodes;
               { We need intel order, no At&t }
               taicpu(hp).SetOperandOrder(op_intel);
               { LOCK must be on same line as opcode }
               if (taicpu(hp).ops = 0) and
                   (fixed_opcode = A_LOCK) then
                 SkipNewLine:=true
               else
                 SkipNewLine:=false;

               s:='';
               if ((fixed_opcode=A_FADDP) or
                   (fixed_opcode=A_FMULP))
                  and (taicpu(hp).ops=0) then
                 begin
                   taicpu(hp).allocate_oper(2);
                   taicpu(hp).oper[0]^.typ:=top_reg;
                   taicpu(hp).oper[0]^.reg:=NR_ST1;
                   taicpu(hp).oper[1]^.typ:=top_reg;
                   taicpu(hp).oper[1]^.reg:=NR_ST;
                 end;
                 { NASM only accepts move for loading of
                   simple symbol address }
                  if ((taicpu(hp).opcode=A_LEA) and
                      (taicpu(hp).ops=2) and
                      (taicpu(hp).oper[0]^.typ=top_reg) and
                      (reg2opsize(taicpu(hp).oper[0]^.reg) in [S_NO,S_Q]) and
                      (taicpu(hp).oper[1]^.typ=top_ref) and
                      (taicpu(hp).oper[1]^.ref^.refaddr<>addr_no) and
                      assigned(taicpu(hp).oper[1]^.ref^.symbol) and
                      (taicpu(hp).oper[1]^.ref^.base=NR_NO)) then
                    begin
                      writer.AsmWrite(asminfo^.comment);
                      writer.AsmWriteln('Converting LEA to MOV instruction');
                      taicpu(hp).opcode:=A_MOV;
                    end;
               if fixed_opcode=A_FWAIT then
                writer.AsmWriteln(#9#9'DB'#9'09bh')
               else
                begin
                  prefix:='';
{$ifndef i8086}
                  { We need to explicitely set
                    word prefix to get selectors
                    to be pushed in 2 bytes  PM }
                  if (taicpu(hp).opsize=S_W) and
                     ((fixed_opcode=A_PUSH) or
                      (fixed_opcode=A_POP)) and
                      (taicpu(hp).oper[0]^.typ=top_reg) and
                      (is_segment_reg(taicpu(hp).oper[0]^.reg)) then
                    writer.AsmWriteln(#9#9'DB'#9'066h');
{$endif not i8086}
                  writer.AsmWrite(#9#9+prefix+std_op2str[fixed_opcode]+cond2str[taicpu(hp).condition]);
                  if taicpu(hp).ops<>0 then
                   begin
                     if is_calljmp(fixed_opcode) then
                      begin
                        writer.AsmWrite(#9);
                        WriteOper_jmp(taicpu(hp).oper[0]^,taicpu(hp));
                      end
                     else
                      begin
                        for i:=0 to taicpu(hp).ops-1 do
                         begin
                           if i=0 then
                            writer.AsmWrite(#9)
                           else
                            writer.AsmWrite(',');
                           WriteOper(taicpu(hp).oper[i]^,taicpu(hp).opsize,fixed_opcode,taicpu(hp).ops,(i=2));
                         end;
                      end;
                   end;
                  if not SkipNewLine then
                    writer.AsmLn;
                end;
             end;

           ait_stab,
           ait_force_line,
           ait_function_name : ;

           ait_cutobject :
             begin
               if SmartAsm then
                begin
                 { only reset buffer if nothing has changed }
                 if not writer.ClearIfEmpty then
                  begin
                    if SmartAsm then
                      begin
                        WriteSmartExternals;
                        FreeExternChainList;
                      end;
                    WriteGroups;
                    writer.AsmClose;
                    DoAssemble;
                    writer.AsmCreate(tai_cutobject(hp).place);
                    ResetSectionsList;
                    WriteHeader;
                  end;
               { avoid empty files }
                 LastSecType:=sec_none;
                 LastSecName:='';
                 LastAlign:=4;
                 while assigned(hp.next) and (tai(hp.next).typ in [ait_cutobject,ait_section,ait_comment]) do
                  begin
                    if tai(hp.next).typ=ait_section then
                      begin
                        LastSecType:=tai_section(hp.next).sectype;
                        LastSecName:=tai_section(hp.next).name^;
                        LastAlign:=tai_section(hp.next).secalign;
                      end;
                    hp:=tai(hp.next);
                  end;
                 if LastSecType<>sec_none then
                   WriteSection(LastSecType,LastSecName,LastAlign);
                 writer.MarkEmpty;
               end;
             end;

           ait_marker :
             if tai_marker(hp).kind=mark_NoLineInfoStart then
               inc(InlineLevel)
             else if tai_marker(hp).kind=mark_NoLineInfoEnd then
               dec(InlineLevel);

           ait_directive :
             begin
               case tai_directive(hp).directive of
                 asd_nasm_import,
                 asd_extern :
                   begin
                     case tai_directive(hp).directive of
                       asd_nasm_import :
                         writer.AsmWrite('import ');
                       asd_extern :
                         writer.AsmWrite('EXTERN ');
                       else
                         internalerror(200509191);
                     end;
                     if tai_directive(hp).name<>'' then
                       begin

                         if SmartAsm then
                           AddSymbol(tai_directive(hp).name,false);

                         writer.AsmWrite(tai_directive(hp).name);
                       end;
                   end;
                 asd_cpu :
                   begin
                     writer.AsmWrite('CPU ');
                     for cpu:=low(tcputype) to high(tcputype) do
                       begin
                         if tai_directive(hp).name=CPUTypeStr[CPU] then
                           begin
                             writer.AsmWriteLn(nasm_cpu_name[cpu]);
                             break;
                           end;
                       end;
                   end;
                 else
                   internalerror(200509191);
               end;
               writer.AsmLn;
             end;
           ait_seh_directive :
             { Ignore for now };
           ait_varloc:
             begin
               if tai_varloc(hp).newlocationhi<>NR_NO then
                 writer.AsmWriteLn(asminfo^.comment+'Var '+tai_varloc(hp).varsym.realname+' located in register '+
                   std_regname(tai_varloc(hp).newlocationhi)+':'+std_regname(tai_varloc(hp).newlocation))
               else
                 writer.AsmWriteLn(asminfo^.comment+'Var '+tai_varloc(hp).varsym.realname+' located in register '+
                   std_regname(tai_varloc(hp).newlocation));
             end;
           else
             internalerror(10000);
         end;
         hp:=tai(hp.next);
       end;
    end;


    procedure TX86NasmAssembler.WriteExternals;
      var
        sym : TAsmSymbol;
        i   : longint;
      begin
        for i:=0 to current_asmdata.AsmSymbolDict.Count-1 do
          begin
            sym:=TAsmSymbol(current_asmdata.AsmSymbolDict[i]);
            if sym.bind=AB_EXTERNAL then
              writer.AsmWriteln('EXTERN'#9+sym.name);
          end;
      end;

    procedure TX86NasmAssembler.WriteSmartExternals;
      var
        EC : PExternChain;
      begin
        EC:=FEC;
        while assigned(EC) do
          begin
            if not EC^.is_defined then
              writer.AsmWriteln('EXTERN'#9+EC^.psym^);
            EC:=EC^.next;
          end;
      end;

    procedure TX86NasmAssembler.WriteHeader;
      begin
{$if defined(i8086)}
      writer.AsmWriteLn('BITS 16');
      case current_settings.cputype of
        cpu_8086: writer.AsmWriteLn('CPU 8086');
        cpu_186: writer.AsmWriteLn('CPU 186');
        cpu_286: writer.AsmWriteLn('CPU 286');
        cpu_386: writer.AsmWriteLn('CPU 386');
        cpu_Pentium: writer.AsmWriteLn('CPU PENTIUM');
        cpu_Pentium2: writer.AsmWriteLn('CPU P2');
        cpu_Pentium3: writer.AsmWriteLn('CPU P3');
        cpu_Pentium4: writer.AsmWriteLn('CPU P4');
        cpu_PentiumM: writer.AsmWriteLn('CPU P4');
        else
          internalerror(2013050101);
      end;
{$elseif defined(i386)}
      writer.AsmWriteLn('BITS 32');
      using_relative:=false;
{$elseif defined(x86_64)}
      writer.AsmWriteLn('BITS 64');
      writer.AsmWriteLn('default rel');
      using_relative:=true;
{$endif}
      end;


    procedure TX86NasmAssembler.WriteAsmList;
    var
      hal : tasmlisttype;
    begin
{$ifdef EXTDEBUG}
      if current_module.mainsource<>'' then
       comment(v_info,'Start writing nasm-styled assembler output for '+current_module.mainsource);
{$endif}
      ResetSectionsList;
      WriteHeader;
      writer.AsmLn;

      WriteExternals;

      for hal:=low(TasmlistType) to high(TasmlistType) do
        begin
          if not (current_asmdata.asmlists[hal].empty) then
            begin
              writer.AsmWriteLn(asminfo^.comment+'Begin asmlist '+AsmListTypeStr[hal]);
              writetree(current_asmdata.asmlists[hal]);
              writer.AsmWriteLn(asminfo^.comment+'End asmlist '+AsmListTypeStr[hal]);
            end;
        end;

      writer.AsmLn;
      if SmartAsm then
        begin
          WriteSmartExternals;
          FreeExternChainList;
        end;
      WriteGroups;
{$ifdef EXTDEBUG}
      if current_module.mainsource<>'' then
       comment(v_info,'Done writing nasm-styled assembler output for '+current_module.mainsource);
{$endif EXTDEBUG}
   end;

    function TX86NasmAssembler.MakeCmdLine: TCmdStr;
      var
        FormatName : string;
      begin
        result:=Inherited MakeCmdLine;
{$ifdef i8086}
        case target_info.system of
          system_i8086_msdos,
          system_i8086_win16:
            FormatName:='obj';
          else
            internalerror(2014082060);
        end;
{$endif i8086}
{$ifdef i386}
        case target_info.system of
          system_i386_go32v2:
            FormatName:='coff';
          system_i386_wdosx,
          system_i386_win32:
            FormatName:='win32';
          system_i386_embedded:
            FormatName:='obj';
          system_i386_linux,
          system_i386_beos:
            FormatName:='elf';
          system_i386_darwin:
            FormatName:='macho32';
        else
          FormatName:='elf';
        end;
{$endif i386}
{$ifdef x86_64}
        case target_info.system of
          system_x86_64_win64:
            FormatName:='win64';
          system_x86_64_darwin:
            FormatName:='macho64';
          system_x86_64_embedded:
            FormatName:='obj';
          system_x86_64_linux:
            FormatName:='elf64';
        else
          FormatName:='elf64';
        end;
{$endif x86_64}
        Replace(result,'$FORMAT',FormatName);
      end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

{$ifdef i8086}
    const
        as_i8086_nasm_info : tasminfo =
          (
            id           : as_i8086_nasm;
            idtxt  : 'NASM';
            asmbin : 'nasm';
            asmcmd : '-f $FORMAT -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i8086_msdos,system_i8086_win16];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );
        as_i8086_nasmobj_info : tasminfo =
          (
            id           : as_i8086_nasmobj;
            idtxt  : 'NASMOBJ';
            asmbin : 'nasm';
            asmcmd : '-f obj -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i8086_msdos,system_i8086_win16];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );
{$endif i8086}
{$ifdef i386}
    const
        as_i386_nasmcoff_info : tasminfo =
          (
            id           : as_i386_nasmcoff;
            idtxt  : 'NASMCOFF';
            asmbin : 'nasm';
            asmcmd : '-f coff -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i386_go32v2];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );

       as_i386_nasmwin32_info : tasminfo =
          (
            id           : as_i386_nasmwin32;
            idtxt  : 'NASMWIN32';
            asmbin : 'nasm';
            asmcmd : '-f win32 -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i386_win32];
            flags : [af_needar,af_no_debug,af_smartlink_sections];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );

       as_i386_nasmobj_info : tasminfo =
          (
            id           : as_i386_nasmobj;
            idtxt  : 'NASMOBJ';
            asmbin : 'nasm';
            asmcmd : '-f obj -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i386_embedded, system_i8086_msdos];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );

       as_i386_nasmwdosx_info : tasminfo =
          (
            id           : as_i386_nasmwdosx;
            idtxt  : 'NASMWDOSX';
            asmbin : 'nasm';
            asmcmd : '-f win32 -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i386_wdosx];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );


       as_i386_nasmelf_info : tasminfo =
          (
            id           : as_i386_nasmelf;
            idtxt  : 'NASMELF';
            asmbin : 'nasm';
            asmcmd : '-f elf -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i386_linux];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );
{
       as_i386_nasmdarwin_info : tasminfo =
          (
            id           : as_i386_nasmdarwin;
            idtxt  : 'NASMDARWIN';
            asmbin : 'nasm';
            asmcmd : '-f macho32 -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i386_darwin];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );
}
       as_i386_nasmbeos_info : tasminfo =
          (
            id           : as_i386_nasmbeos;
            idtxt  : 'NASMELF';
            asmbin : 'nasm';
            asmcmd : '-f elf -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i386_beos];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );

       as_i386_nasmhaiku_info : tasminfo =
          (
            id           : as_i386_nasmhaiku;
            idtxt  : 'NASMELF';
            asmbin : 'nasm';
            asmcmd : '-f elf -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_i386_haiku];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );
       as_i386_nasm_info : tasminfo =
          (
            id           : as_i386_nasm;
            idtxt  : 'NASM';
            asmbin : 'nasm';
            asmcmd : '-f $FORMAT -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_any];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );

{$endif i386}
{$ifdef x86_64}
    const
       as_x86_64_nasm_info : tasminfo =
          (
            id           : as_x86_64_nasm;
            idtxt  : 'NASM';
            asmbin : 'nasm';
            asmcmd : '-f $FORMAT -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_any];
            flags : [af_needar{,af_no_debug}];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );

       as_x86_64_nasmwin64_info : tasminfo =
          (
            id           : as_x86_64_nasmwin64;
            idtxt  : 'NASMWIN64';
            asmbin : 'nasm';
            asmcmd : '-f win64 -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_x86_64_win64];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );

       as_x86_64_nasmelf_info : tasminfo =
          (
            id           : as_x86_64_nasmelf;
            idtxt  : 'NASMELF';
            asmbin : 'nasm';
            asmcmd : '-f elf64 -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_x86_64_linux];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );


       as_x86_64_nasmdarwin_info : tasminfo =
          (
            id           : as_x86_64_nasmdarwin;
            idtxt  : 'NASMDARWIN';
            asmbin : 'nasm';
            asmcmd : '-f macho64 -o $OBJ -w-orphan-labels $EXTRAOPT $ASM';
            supported_targets : [system_x86_64_darwin];
            flags : [af_needar,af_no_debug];
            labelprefix : '..@';
            comment : '; ';
            dollarsign: '$';
          );

{$endif x86_64}


initialization
{$ifdef i8086}
  RegisterAssembler(as_i8086_nasm_info,TX86NasmAssembler);
  RegisterAssembler(as_i8086_nasmobj_info,TX86NasmAssembler);
{$endif i8086}
{$ifdef i386}
  RegisterAssembler(as_i386_nasmcoff_info,TX86NasmAssembler);
  RegisterAssembler(as_i386_nasmwin32_info,TX86NasmAssembler);
  RegisterAssembler(as_i386_nasmwdosx_info,TX86NasmAssembler);
  RegisterAssembler(as_i386_nasmobj_info,TX86NasmAssembler);
  RegisterAssembler(as_i386_nasmbeos_info,TX86NasmAssembler);
  RegisterAssembler(as_i386_nasmhaiku_info,TX86NasmAssembler);
  RegisterAssembler(as_i386_nasmelf_info,TX86NasmAssembler);
  RegisterAssembler(as_i386_nasm_info,TX86NasmAssembler);
{$endif i386}
{$ifdef x86_64}
  RegisterAssembler(as_x86_64_nasm_info,TX86NasmAssembler);
  RegisterAssembler(as_x86_64_nasmwin64_info,TX86NasmAssembler);
  RegisterAssembler(as_x86_64_nasmelf_info,TX86NasmAssembler);
  RegisterAssembler(as_x86_64_nasmdarwin_info,TX86NasmAssembler);
{$endif x86_64}
end.
