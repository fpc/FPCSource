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
      cpubase,globtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,assemble,cgutils;

    type

      { T386NasmAssembler }

      TX86NasmAssembler = class(texternalassembler)
      private
        using_relative : boolean;
        function CodeSectionName: string;
        procedure WriteReference(var ref : treference);
        procedure WriteOper(const o:toper;s : topsize; opcode: tasmop;ops:longint;dest : boolean);
        procedure WriteOper_jmp(const o:toper; ai : taicpu);
        procedure WriteSection(atype:TAsmSectiontype;const aname:string);
      public
        procedure WriteTree(p:TAsmList);override;
        procedure WriteAsmList;override;
        procedure WriteExternals;
        procedure WriteSmartExternals;
        procedure WriteHeader;
        function  MakeCmdLine: TCmdStr;override;
      end;



  implementation

    uses
      cutils,globals,systems,cclasses,
      fmodule,finput,verbose,cpuinfo,cgbase
      ;

    type
{$ifdef cpuextended}
      t80bitarray = array[0..9] of byte;
{$endif cpuextended}
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;
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

    function single2str(d : single) : string;
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

    function double2str(d : double) : string;
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

    function extended2str(e : extended) : string;
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


  { convert floating point values }
  { to correct endian             }
  procedure swap64bitarray(var t: t64bitarray);
    var
     b: byte;
    begin
      b:= t[7];
      t[7] := t[0];
      t[0] := b;

      b := t[6];
      t[6] := t[1];
      t[1] := b;

      b:= t[5];
      t[5] := t[2];
      t[2] := b;

      b:= t[4];
      t[4] := t[3];
      t[3] := b;
   end;


   procedure swap32bitarray(var t: t32bitarray);
    var
     b: byte;
    begin
      b:= t[1];
      t[1]:= t[2];
      t[2]:= b;

      b:= t[0];
      t[0]:= t[3];
      t[3]:= b;
    end;


    function comp2str(d : bestreal) : string;
      type
        pdouble = ^double;
      var
        c  : comp;
        dd : pdouble;
      begin
         c:=comp(d);
         dd:=pdouble(@c); { this makes a bitwise copy of c into a double }
         comp2str:=double2str(dd^);
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


    function TX86NasmAssembler.CodeSectionName: string;
      begin
{$ifdef i8086}
        if current_settings.x86memorymodel in x86_far_code_models then
          result:=current_module.modulename^ + '_TEXT'
        else
{$endif}
          result:='.text';
      end;


    procedure TX86NasmAssembler.WriteReference(var ref : treference);
      var
        first : boolean;
        base_done : boolean;
      begin
        with ref do
         begin
           AsmWrite('[');
           first:=true;
           base_done:=false;
           if (segment<>NR_NO) then
             AsmWrite(nasm_regname(segment)+':');
{$ifdef x86_64}
          if (base=NR_RIP) then
            begin
              { nasm RIP is implicit for pic }
              if not (ref.refaddr in [addr_pic,addr_pic_no_got]) and not using_relative then
                AsmWrite('$ + ');
              base_done:=true;
            end;
{$endif x86_64}
           if assigned(symbol) then
            begin
              AsmWrite(symbol.name);
              if SmartAsm then
                AddSymbol(symbol.name,false);
              first:=false;
            end;
           if (base<>NR_NO) and not base_done then
            begin
              if not(first) then
               AsmWrite('+')
              else
               first:=false;
              AsmWrite(nasm_regname(base))
            end;
           if (index<>NR_NO) then
             begin
               if not(first) then
                 AsmWrite('+')
               else
                 first:=false;
               AsmWrite(nasm_regname(index));
               if scalefactor<>0 then
                 AsmWrite('*'+tostr(scalefactor));
             end;
           if offset<0 then
             begin
               AsmWrite(tostr(offset));
               first:=false;
             end
           else if (offset>0) then
             begin
               AsmWrite('+'+tostr(offset));
               first:=false;
             end;
           if first then
             AsmWrite('0');
           AsmWrite(']');
         end;
       end;


    procedure TX86NasmAssembler.WriteOper(const o:toper;s : topsize; opcode: tasmop;ops:longint;dest : boolean);
      begin
        case o.typ of
          top_reg :
            AsmWrite(nasm_regname(o.reg));
          top_const :
            begin
              if (ops=1) and (opcode<>A_RET) then
               AsmWrite(sizestr(s,dest));
              AsmWrite(tostr(longint(o.val)));
            end;
          top_ref :
            begin
              if o.ref^.refaddr in [addr_no,addr_pic,addr_pic_no_got] then
                begin
                  if not ((opcode = A_LEA) or (opcode = A_LGS) or
                          (opcode = A_LSS) or (opcode = A_LFS) or
                          (opcode = A_LES) or (opcode = A_LDS) or
                         // (opcode = A_SHR) or (opcode = A_SHL) or
                         // (opcode = A_SAR) or (opcode = A_SAL) or
                          (opcode = A_OUT) or (opcode = A_IN)) then
                    AsmWrite(sizestr(s,dest));
                  WriteReference(o.ref^);
                end
{$ifdef i8086}
              else if o.ref^.refaddr=addr_dgroup then
                begin
                  AsmWrite('dgroup');
                end
{$endif i8086}
              else
                begin
{$ifdef x86_64}
                  if s=S_L then
                    asmwrite('dword ')
                  else
                    asmwrite('qword ');
{$endif}
{$ifdef i386}
                  asmwrite('dword ');
{$endif i386}
{$ifdef i8086}
                  if o.ref^.refaddr=addr_seg then
                    asmwrite('SEG ')
                  else
                    asmwrite('word ');
{$endif i8086}
                  if assigned(o.ref^.symbol) then
                   begin
                    if SmartAsm then
                      AddSymbol(o.ref^.symbol.name,false);
                    asmwrite(o.ref^.symbol.name);
                    if o.ref^.offset=0 then
                      exit;
                   end;
                  if o.ref^.offset>0 then
                   asmwrite('+');
                  asmwrite(tostr(o.ref^.offset));
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
            AsmWrite(nasm_regname(o.reg));
          top_ref :
            if o.ref^.refaddr=addr_no then
              begin
                if ai.opsize=S_FAR then
                  AsmWrite('far ');
                WriteReference(o.ref^);
              end
            else
              begin
                if ai.opsize=S_FAR then
                  AsmWrite('far ')
                else
                  begin
{ NEAR forces NASM to emit near jumps, which are 386+ }
{$ifndef i8086}
                if not(
                       (ai.opcode=A_JCXZ) or (ai.opcode=A_JECXZ) or
    {$ifdef x86_64}
                       (ai.opcode=A_JRCXZ) or
    {$endif x86_64}
                       (ai.opcode=A_LOOP) or (ai.opcode=A_LOOPE) or
                       (ai.opcode=A_LOOPNE) or (ai.opcode=A_LOOPNZ) or
                       (ai.opcode=A_LOOPZ)
                      ) then
                  AsmWrite('NEAR ');
{$endif i8086}
                  end;

                AsmWrite(o.ref^.symbol.name);
                if SmartAsm then
                  AddSymbol(o.ref^.symbol.name,false);
                if o.ref^.offset>0 then
                 AsmWrite('+'+tostr(o.ref^.offset))
                else
                 if o.ref^.offset<0 then
                  AsmWrite(tostr(o.ref^.offset));
              end;
          top_const :
            AsmWrite(tostr(aint(o.val)));
          else
            internalerror(10001);
        end;
      end;


    const
      ait_const2str : array[aitconst_128bit..aitconst_64bit_unaligned] of string[30]=(
        #9'FIXME_128BIT'#9,#9'DQ'#9,#9'DD'#9,#9'DW'#9,#9'DB'#9,
        #9'FIXME_SLEB128BIT'#9,#9'FIXME_ULEB128BIT'#9,
        #9'RVA'#9,#9'SECREL32'#9,#9'FIXME_darwin_dwarf_delta64'#9,
        #9'FIXME_darwin_dwarf_delta32'#9,#9'FIXME_half16bit'#9,
        #9'DW'#9,#9'DD'#9,#9'FIXME_64BIT_UNALIGNED'#9
      );

    procedure TX86NasmAssembler.WriteSection(atype:TAsmSectiontype;const aname:string);
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
          '.objc_protolist'
        );
      begin
        AsmLn;
        AsmWrite('SECTION ');
        { go32v2 stub only loads .text and .data sections, and allocates space for .bss.
          Thus, data which normally goes into .rodata and .rodata_norel sections must
          end up in .data section }
        if (atype in [sec_rodata,sec_rodata_norel]) and
          (target_info.system=system_i386_go32v2) then
          AsmWrite('.data')
        else if (atype=sec_user) then
          AsmWrite(aname)
        else if (atype=sec_threadvar) and
          (target_info.system in (systems_windows+systems_wince)) then
          AsmWrite('.tls'#9'bss')
        else if secnames[atype]='.text' then
          AsmWrite(CodeSectionName)
        else
          AsmWrite(secnames[atype]);
        if create_smartlink_sections and
           (atype<>sec_bss) and
           (aname<>'') then
          begin
            AsmWrite('.');
            AsmWrite(aname);
          end;
        AsmLn;
        LasTSecType:=atype;
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
      do_line,
      quoted   : boolean;
      co       : comp;
      sin      : single;
      d        : double;
{$ifdef cpuextended}
      e        : extended;
{$endif cpuextended}
      fixed_opcode: TAsmOp;
      prefix   : string;
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
               AsmWrite(target_asm.comment);
               AsmWritePChar(tai_comment(hp).str);
               AsmLn;
             End;

           ait_regalloc :
             begin
               if (cs_asm_regalloc in current_settings.globalswitches) then
                 AsmWriteLn(#9#9+target_asm.comment+'Register '+nasm_regname(tai_regalloc(hp).reg)+' '+
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
                 WriteSection(tai_section(hp).sectype,tai_section(hp).name^);
               LasTSecType:=tai_section(hp).sectype;
             end;

           ait_align :
             begin
               if (tai_align(hp).aligntype>1) then
                 begin
                   if (lastsectype=sec_bss) or (
                      (lastsectype=sec_threadvar) and
                      (target_info.system in (systems_windows+systems_wince))
                     ) then
                      AsmWriteLn(#9'ALIGNB '+tostr(tai_align(hp).aligntype))
                    else
                      AsmWriteLn(#9'ALIGN '+tostr(tai_align(hp).aligntype));
                 end;
             end;

           ait_datablock :
             begin
               if tai_datablock(hp).is_global or SmartAsm then
                begin
                  AsmWrite(#9'GLOBAL ');
                  AsmWriteLn(tai_datablock(hp).sym.name);
                end;
               AsmWrite(PadTabs(tai_datablock(hp).sym.name,':'));
               if SmartAsm then
                 AddSymbol(tai_datablock(hp).sym.name,true);
               AsmWriteLn('RESB'#9+tostr(tai_datablock(hp).size));
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
                      AsmWrite(ait_const2str[aitconst_32bit]);
                      AsmWrite(tostr(longint(lo(tai_const(hp).value))));
                      AsmWrite(',');
                      AsmWrite(tostr(longint(hi(tai_const(hp).value))));
                      AsmLn;
                    end;
                 aitconst_uleb128bit,
                 aitconst_sleb128bit,
                 aitconst_128bit:
                    begin
                      AsmWriteLn(target_asm.comment+'Unsupported const type '+
                        ait_const2str[consttype]);
                    end;
{$ifdef i8086}
                 aitconst_farptr:
                   begin
                     AsmWrite(ait_const2str[aitconst_16bit]);
                     if assigned(tai_const(hp).sym) then
                       begin
                         if SmartAsm then
                           AddSymbol(tai_const(hp).sym.name,false);
                         AsmWrite(tai_const(hp).sym.name);
                         if tai_const(hp).value<>0 then
                           AsmWrite(tostr_with_plus(tai_const(hp).value));
                         AsmLn;
                         AsmWrite(ait_const2str[aitconst_16bit]);
                         AsmWrite('SEG ');
                         AsmWrite(tai_const(hp).sym.name);
                       end
                     else
                       AsmWrite(tostr(lo(longint(tai_const(hp).value)))+','+
                                tostr(hi(longint(tai_const(hp).value))));
                     AsmLn;
                   end;
{$endif i8086}
                 aitconst_32bit,
                 aitconst_16bit,
                 aitconst_8bit,
                 aitconst_rva_symbol,
                 aitconst_secrel32_symbol,
                 aitconst_16bit_unaligned,
                 aitconst_32bit_unaligned:
                   begin
                     AsmWrite(ait_const2str[tai_const(hp).consttype]);
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
                       AsmWrite(s);
                       inc(l,length(s));
                       if (l>line_length) or
                          (hp.next=nil) or
                          (tai(hp.next).typ<>ait_const) or
                          (tai_const(hp.next).consttype<>consttype) then
                         break;
                       hp:=tai(hp.next);
                       AsmWrite(',');
                     until false;
                     AsmLn;
                   end;
                 else
                   internalerror(200704252);
               end;
             end;

{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
           ait_real_80bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+'value: '+extended2str(tai_real_80bit(hp).value));
             { Make sure e is a extended type, bestreal could be
               a different type (bestreal) !! (PFV) }
               e:=tai_real_80bit(hp).value;
               AsmWrite(#9#9'DB'#9);
               for i:=0 to 9 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t80bitarray(e)[i]));
                end;
                for i:=11 to tai_real_80bit(hp).savesize do
                  AsmWrite(',0');
               AsmLn;
             end;
{$else cpuextended}
           ait_real_80bit :
             AsmWriteLn(#9#9'DT'#9+extended2str(tai_real_80bit(hp).value));
{$endif cpuextended}

           // ait_real_64bit :
           //   AsmWriteLn(#9#9'DQ'#9+double2str(tai_real_64bit(hp).value));
           ait_real_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+'value: '+double2str(tai_real_64bit(hp).value));
               d:=tai_real_64bit(hp).value;
               { swap the values to correct endian if required }
               if source_info.endian <> target_info.endian then
                 swap64bitarray(t64bitarray(d));
               AsmWrite(#9#9'DB'#9);
{$ifdef arm}
               if tai_real_64bit(hp).formatoptions=fo_hiloswapped then
                 begin
                   for i:=4 to 7 do
                     begin
                       if i<>4 then
                         AsmWrite(',');
                       AsmWrite(tostr(t64bitarray(d)[i]));
                     end;
                   for i:=0 to 3 do
                     begin
                       AsmWrite(',');
                       AsmWrite(tostr(t64bitarray(d)[i]));
                     end;
                 end
               else
{$endif arm}
                 begin
                   for i:=0 to 7 do
                     begin
                       if i<>0 then
                         AsmWrite(',');
                       AsmWrite(tostr(t64bitarray(d)[i]));
                     end;
                 end;
               AsmLn;
             end;
           // ait_real_32bit :
           //   AsmWriteLn(#9#9'DD'#9+single2str(tai_real_32bit(hp).value));
           ait_real_32bit :
             begin
               if do_line then
                 AsmWriteLn(target_asm.comment+'value: '+single2str(tai_real_32bit(hp).value));
               sin:=tai_real_32bit(hp).value;
               { swap the values to correct endian if required }
               if source_info.endian <> target_info.endian then
                 swap32bitarray(t32bitarray(sin));
               AsmWrite(#9#9'DB'#9);
               for i:=0 to 3 do
                begin
                  if i<>0 then
                    AsmWrite(',');
                  AsmWrite(tostr(t32bitarray(sin)[i]));
                end;
               AsmLn;
             end;
           // ait_comp_64bit :
           //   AsmWriteLn(#9#9'DQ'#9+comp2str(tai_real_80bit(hp).value));
           ait_comp_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+'value: '+extended2str(tai_comp_64bit(hp).value));
               AsmWrite(#9#9'DB'#9);
               co:=comp(tai_comp_64bit(hp).value);
               { swap the values to correct endian if required }
               if source_info.endian <> target_info.endian then
                 swap64bitarray(t64bitarray(co));
               for i:=0 to 7 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t64bitarray(co)[i]));
                end;
               AsmLn;
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
                     AsmWrite(#9#9'DB'#9);
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
                                        AsmWrite(',');
                                      AsmWrite('"');
                                    end;
                                AsmWrite(tai_string(hp).str[i]);
                                quoted:=true;
                              end { if > 31 and < 128 and ord('"') }
                          else
                              begin
                                  if quoted then
                                      AsmWrite('"');
                                  if i>counter then
                                      AsmWrite(',');
                                  quoted:=false;
                                  AsmWrite(tostr(ord(tai_string(hp).str[i])));
                              end;
                       end; { end for i:=0 to... }
                     if quoted then AsmWrite('"');
                       AsmWrite(target_info.newline);
                     inc(counter,line_length);
                  end; { end for j:=0 ... }
                { do last line of lines }
                if counter<tai_string(hp).len then
                  AsmWrite(#9#9'DB'#9);
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
                                  AsmWrite(',');
                                AsmWrite('"');
                              end;
                          AsmWrite(tai_string(hp).str[i]);
                          quoted:=true;
                        end { if > 31 and < 128 and " }
                    else
                        begin
                          if quoted then
                            AsmWrite('"');
                          if i>counter then
                              AsmWrite(',');
                          quoted:=false;
                          AsmWrite(tostr(ord(tai_string(hp).str[i])));
                        end;
                  end; { end for i:=0 to... }
                if quoted then
                  AsmWrite('"');
                end;
               AsmLn;
             end;

           ait_label :
             begin
               if tai_label(hp).labsym.is_used then
                 begin
                   if SmartAsm and (tai_label(hp).labsym.bind=AB_GLOBAL) then
                     begin
                       AsmWrite(#9'GLOBAL ');
                       AsmWriteLn(tai_label(hp).labsym.name);
                     end;
                   AsmWriteLn(tai_label(hp).labsym.name+':');
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
                  AsmWrite(#9'GLOBAL ');
                  AsmWriteLn(tai_symbol(hp).sym.name);
                end;
               AsmWrite(tai_symbol(hp).sym.name);
               if SmartAsm then
                 AddSymbol(tai_symbol(hp).sym.name,true);
               if assigned(hp.next) and not(tai(hp.next).typ in
                  [ait_const,
                   ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                AsmWriteLn(':')
             end;

           ait_symbol_end : ;

           ait_instruction :
             begin
               fixed_opcode:=taicpu(hp).FixNonCommutativeOpcodes;
               { We need intel order, no At&t }
               taicpu(hp).SetOperandOrder(op_intel);
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
                      AsmWrite(target_asm.comment);
                      AsmWriteln('Converting LEA to MOV instruction');
                      taicpu(hp).opcode:=A_MOV;
                    end;
               if fixed_opcode=A_FWAIT then
                AsmWriteln(#9#9'DB'#9'09bh')
               else
                begin
                  prefix:='';
{$ifdef i8086}
                  { nickysn note: I don't know if the 187 requires FWAIT before
                    every instruction like the 8087, so I'm including it just in case }
                  if (current_settings.cputype<=cpu_186) and
                      requires_fwait_on_8087(fixed_opcode) then
                    prefix:='wait '+prefix;
{$endif i8086}
{$ifndef i8086}
                  { We need to explicitely set
                    word prefix to get selectors
                    to be pushed in 2 bytes  PM }
                  if (taicpu(hp).opsize=S_W) and
                     ((fixed_opcode=A_PUSH) or
                      (fixed_opcode=A_POP)) and
                      (taicpu(hp).oper[0]^.typ=top_reg) and
                      (is_segment_reg(taicpu(hp).oper[0]^.reg)) then
                    AsmWriteln(#9#9'DB'#9'066h');
{$endif not i8086}
                  AsmWrite(#9#9+prefix+std_op2str[fixed_opcode]+cond2str[taicpu(hp).condition]);
                  if taicpu(hp).ops<>0 then
                   begin
                     if is_calljmp(fixed_opcode) then
                      begin
                        AsmWrite(#9);
                        WriteOper_jmp(taicpu(hp).oper[0]^,taicpu(hp));
                      end
                     else
                      begin
                        for i:=0 to taicpu(hp).ops-1 do
                         begin
                           if i=0 then
                            AsmWrite(#9)
                           else
                            AsmWrite(',');
                           WriteOper(taicpu(hp).oper[i]^,taicpu(hp).opsize,fixed_opcode,taicpu(hp).ops,(i=2));
                         end;
                      end;
                   end;
                  AsmLn;
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
                 if AsmSize=AsmStartSize then
                  AsmClear
                 else
                  begin
                    if SmartAsm then
                      begin
                        WriteSmartExternals;
                        FreeExternChainList;
                      end;
                    AsmClose;
                    DoAssemble;
                    AsmCreate(tai_cutobject(hp).place);
                    WriteHeader;
                  end;
               { avoid empty files }
                 while assigned(hp.next) and (tai(hp.next).typ in [ait_cutobject,ait_section,ait_comment]) do
                  begin
                    if tai(hp.next).typ=ait_section then
                      lasTSectype:=tai_section(hp.next).sectype;
                    hp:=tai(hp.next);
                  end;
                 if lasTSectype<>sec_none then
                   WriteSection(lasTSectype,'');
                 AsmStartSize:=AsmSize;
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
                 asd_nasm_import :
                   AsmWrite('import ');
                 asd_extern :
                   AsmWrite('EXTERN ');
                 else
                   internalerror(200509191);
               end;
               if tai_directive(hp).name<>'' then
                 begin

                   if SmartAsm then
                     AddSymbol(tai_directive(hp).name,false);

                   AsmWrite(tai_directive(hp).name);
                 end;
               AsmLn;
             end;
           ait_seh_directive :
             { Ignore for now };
           ait_varloc:
             begin
               if tai_varloc(hp).newlocationhi<>NR_NO then
                 AsmWriteLn(target_asm.comment+'Var '+tai_varloc(hp).varsym.realname+' located in register '+
                   std_regname(tai_varloc(hp).newlocationhi)+':'+std_regname(tai_varloc(hp).newlocation))
               else
                 AsmWriteLn(target_asm.comment+'Var '+tai_varloc(hp).varsym.realname+' located in register '+
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
              AsmWriteln('EXTERN'#9+sym.name);
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
              AsmWriteln('EXTERN'#9+EC^.psym^);
            EC:=EC^.next;
          end;
      end;

    procedure TX86NasmAssembler.WriteHeader;
      begin
{$ifdef i8086}
      AsmWriteLn('BITS 16');
      case current_settings.cputype of
        cpu_8086: AsmWriteLn('CPU 8086');
        cpu_186: AsmWriteLn('CPU 186');
        cpu_286: AsmWriteLn('CPU 286');
        cpu_386: AsmWriteLn('CPU 386');
        cpu_Pentium: AsmWriteLn('CPU PENTIUM');
        cpu_Pentium2: AsmWriteLn('CPU P2');
        cpu_Pentium3: AsmWriteLn('CPU P3');
        cpu_Pentium4: AsmWriteLn('CPU P4');
        cpu_PentiumM: AsmWriteLn('CPU P4');
        else
          internalerror(2013050101);
      end;

      AsmWriteLn('SECTION ' + CodeSectionName + ' use16 class=code');
      if current_settings.x86memorymodel in x86_near_data_models then
        begin
          { NASM complains if you put a missing section in the GROUP directive, so }
          { we add empty declarations to make sure they exist, even if empty }
          AsmWriteLn('SECTION .rodata');
          AsmWriteLn('SECTION .data');
          AsmWriteLn('SECTION .fpc');
          { WLINK requires class=bss in order to leave the BSS section out of the executable }
          AsmWriteLn('SECTION .bss class=bss');
          { group these sections in the same segment }
          if current_settings.x86memorymodel=mm_tiny then
            AsmWriteLn('GROUP dgroup text rodata data fpc bss')
          else
            AsmWriteLn('GROUP dgroup rodata data fpc bss');
        end;
      if paratargetdbg in [dbg_dwarf2,dbg_dwarf3,dbg_dwarf4] then
        begin
          AsmWriteLn('SECTION .debug_frame  use32 class=DWARF');
          AsmWriteLn('SECTION .debug_info   use32 class=DWARF');
          AsmWriteLn('SECTION .debug_line   use32 class=DWARF');
          AsmWriteLn('SECTION .debug_abbrev use32 class=DWARF');
        end;
      AsmWriteLn('SECTION ' + CodeSectionName);
{$else i8086}
{$ifdef i386}
      AsmWriteLn('BITS 32');
      using_relative:=false;
{$else not i386}
      AsmWriteLn('BITS 64');
      AsmWriteLn('default rel');
      using_relative:=true;
{$endif not i386}
{$endif i8086}
      end;


    procedure TX86NasmAssembler.WriteAsmList;
    var
      hal : tasmlisttype;
    begin
{$ifdef EXTDEBUG}
      if current_module.mainsource<>'' then
       comment(v_info,'Start writing nasm-styled assembler output for '+current_module.mainsource);
{$endif}
      WriteHeader;
      AsmLn;

      WriteExternals;

      for hal:=low(TasmlistType) to high(TasmlistType) do
        begin
          if not (current_asmdata.asmlists[hal].empty) then
            begin
              AsmWriteLn(target_asm.comment+'Begin asmlist '+AsmListTypeStr[hal]);
              writetree(current_asmdata.asmlists[hal]);
              AsmWriteLn(target_asm.comment+'End asmlist '+AsmListTypeStr[hal]);
            end;
        end;

      AsmLn;
      if SmartAsm then
        begin
          WriteSmartExternals;
          FreeExternChainList;
        end;
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
          system_i8086_msdos:
            FormatName:='obj';
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
            supported_targets : [system_i8086_msdos];
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
            supported_targets : [system_i8086_msdos];
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
            flags : [af_needar,af_no_debug];
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
