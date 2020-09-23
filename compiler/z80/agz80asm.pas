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
{ This unit implements the assembler writer for the z80asm assembler:
  http://savannah.nongnu.org/projects/z80asm
}

unit agz80asm;

{$i fpcdefs.inc}

  interface

    uses
       globtype,systems,
       aasmtai,aasmdata,
       assemble,
       cpubase;

    type

      { TZ80AsmAssembler }

      TZ80AsmAssembler=class(TExternalAssembler)
      private
        function EscapeLabel(s: ansistring): ansistring;
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
        #9''#9,#9'DQ'#9,#9'DD'#9,#9'DW'#9,#9'DB'#9,
        #9'FIXMESLEB',#9'FIXEMEULEB',
        #9'DD RVA'#9,#9'DD SECREL32'#9,
        #9'FIXME',#9'FIXME',#9'FIXME',#9'FIXME',
        #9'DW'#9,#9'DD'#9,#9'DQ'#9
      );

    function TZ80AsmAssembler.EscapeLabel(s: ansistring): ansistring;
      var
        i: Integer;
      begin
        result:='';
        for i:=1 to length(s) do
          if ((s[i]>='a') and (s[i]<='z')) or
             ((s[i]>='A') and (s[i]<='Z')) or
             ((s[i]>='0') and (s[i]<='9')) or
             (s[i]='.') then
            result:=result+s[i]
          else if s[i]='_' then
            result:=result+'__'
          else
            result:=result+('_'+HexStr(Ord(s[i]),2));
      end;

    procedure TZ80AsmAssembler.WriteTree(p: TAsmList);

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
                  //  s:=ApplyAsmSymbolRestrictions(symbol.name);
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
          result:='';
          //case o.typ of
          //  top_reg:
          //    getopstr:=gas_regname(o.reg);
          //  top_const:
          //    getopstr:=tostr(longint(o.val));
          //  top_ref:
          //    if o.ref^.refaddr=addr_full then
          //      begin
          //        hs:=ApplyAsmSymbolRestrictions(o.ref^.symbol.name);
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
      counter,lines,i,j,l,tokens: longint;
      quoted: Boolean;
      consttype: taiconst_type;
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
                   writer.AsmWrite(EscapeLabel(tai_label(hp).labsym.name));
                   writer.AsmWriteLn(':');
                 end;
              end;
            ait_symbol :
              begin
                if tai_symbol(hp).has_value then
                  internalerror(2009090802);
                { wasm is case insensitive, we nned to use only uppercase version
                  if both a lowercase and an uppercase version are provided }
                {if (asminfo^.id = as_i386_wasm) then
                  begin
                    nhp:=tai(hp.next);
                    while assigned(nhp) and (nhp.typ in [ait_function_name,ait_force_line]) do
                      nhp:=tai(nhp.next);
                    if assigned(nhp) and (tai(nhp).typ=ait_symbol) and
                       (lower(tai_symbol(nhp).sym.name)=tai_symbol(hp).sym.name) then
                      begin
                        writer.AsmWriteln(asminfo^.comment+' '+tai_symbol(hp).sym.name+' removed');
                        hp:=tai(nhp);
                      end;
                  end;}
                {if tai_symbol(hp).is_global then
                  writer.AsmWriteLn(#9'PUBLIC'#9+tai_symbol(hp).sym.name);}
                writer.AsmWrite(EscapeLabel(tai_symbol(hp).sym.name));
                {if assigned(hp.next) and not(tai(hp.next).typ in
                   [ait_const,ait_realconst,ait_string]) then}
                  writer.AsmWriteLn(':');
              end;
            ait_symbol_end :
              begin
              end;
            ait_const:
              begin
                consttype:=tai_const(hp).consttype;
                case consttype of
                  {aitconst_uleb128bit,
                  aitconst_sleb128bit,
                  aitconst_128bit,
                  aitconst_64bit,
                  aitconst_32bit,}
                  aitconst_16bit,
                  aitconst_8bit,
                  aitconst_16bit_unaligned{,
                  aitconst_32bit_unaligned,
                  aitconst_64bit_unaligned,
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
                              s:=EscapeLabel(tai_const(hp).endsym.name)+'-'+EscapeLabel(tai_const(hp).sym.name)
                            else
                              s:=EscapeLabel(tai_const(hp).sym.name);
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
                              (ord(tai_string(hp).str[i])<127) and
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
                               end { if > 31 and < 127 and ord('"') }
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
                      counter := counter+line_length;
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


    procedure TZ80AsmAssembler.WriteAsmList;
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


    function TZ80AsmAssembler.MakeCmdLine: TCmdStr;
      begin
        result := {'-mmcu='+lower(cputypestr[current_settings.cputype])+' '+}inherited MakeCmdLine;
      end;


    const
       as_Z80_asm_info : tasminfo =
          (
            id     : as_z80asm;

            idtxt  : 'Z80ASM';
            asmbin : 'z80asm';
            asmcmd : '-o $OBJ $EXTRAOPT $ASM';
            supported_targets : [system_Z80_embedded];
            flags : [af_needar{,af_smartlink_sections}];
            labelprefix : '.L';
            labelmaxlen : -1;
            comment : '; ';
            dollarsign: 's';
          );


begin
  RegisterAssembler(as_Z80_asm_info,TZ80AsmAssembler);
end.
