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
       aasmtai,aasmdata,
       assemble,
       cpubase;

    type

      { TSdccSdasZ80Assembler }

      TSdccSdasZ80Assembler=class(TExternalAssembler)
      private
        procedure WriteDecodedSleb128(a: int64);
        procedure WriteDecodedUleb128(a: qword);
      public
        procedure WriteTree(p : TAsmList); override;
        procedure WriteAsmList;override;
        function MakeCmdLine: TCmdStr; override;
      end;

  implementation

    uses
       cutils,globals,verbose,
       aasmbase,aasmcpu,
       cpuinfo,
       cgbase,cgutils;

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

    //var op: TAsmOp;
    //    s: string;
    //    i: byte;
    //    sep: string[3];
    var
      hp: tai;
      s: string;
      counter,lines,i,j,l,tokens,pos: longint;
      quoted: Boolean;
      consttype: taiconst_type;
      ch: Char;
    begin
      if not assigned(p) then
       exit;
      hp:=tai(p.first);
      while assigned(hp) do
        begin
          prefetch(pointer(hp.next)^);
          case hp.typ of
            ait_comment :
              begin
                writer.AsmWrite(asminfo^.comment);
                writer.AsmWritePChar(tai_comment(hp).str);
                writer.AsmLn;
              end;
            ait_align :
              begin
                if tai_align_abstract(hp).aligntype>1 then
                  writer.AsmWriteLn(asminfo^.comment+'Unsupported ALIGN '+tostr(tai_align_abstract(hp).aligntype));
              end;
            ait_label :
              begin
                if tai_label(hp).labsym.is_used then
                  begin
                    if tai_label(hp).labsym.bind in [AB_GLOBAL,AB_PRIVATE_EXTERN] then
                      begin
                        writer.AsmWrite('.globl'#9);
                        writer.AsmWriteLn(tai_label(hp).labsym.name);
                      end;
                    writer.AsmWrite(tai_label(hp).labsym.name);
                    writer.AsmWriteLn(':');
                  end;
              end;
            ait_symbol :
              begin
                if tai_symbol(hp).has_value then
                  internalerror(2009090802);
                if tai_symbol(hp).is_global then
                  begin
                    writer.AsmWrite('.globl'#9);
                    writer.AsmWriteln(tai_symbol(hp).sym.name);
                  end;
                writer.AsmWriteLn(tai_symbol(hp).sym.name + ':');
              end;
            ait_symbol_end :
              begin
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
            else
              begin
                writer.AsmWrite(asminfo^.comment);
                writer.AsmWrite('WARNING: not yet implemented in assembler output: ');
                Str(hp.typ,s);
                writer.AsmWriteLn(s);
              end;
          end;
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
