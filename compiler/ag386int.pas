{
    $Id$
    Copyright (c) 1996,97 by Florian Klaempfl

    This unit implements an asmoutput class for Intel syntax with Intel i386+

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
unit ag386int;

    interface

    uses aasm,assemble;

    type
      pi386intasmlist=^ti386intasmlist;
      ti386intasmlist = object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
      end;

  implementation

    uses
      dos,globals,systems,cobjects,i386,
      strings,files,verbose
{$ifdef GDB}
      ,gdb
{$endif GDB}
      ;

    const
      line_length = 70;

      extstr : array[EXT_NEAR..EXT_ABS] of String[8] =
             ('NEAR','FAR','PROC','BYTE','WORD','DWORD',
              'CODEPTR','DATAPTR','FWORD','PWORD','QWORD','TBYTE','ABS');

    function getreferencestring(const ref : treference) : string;
    var
      s     : string;
      first : boolean;
    begin
      if ref.isintvalue then
       s:= tostr(ref.offset)
      else
      with ref do
        begin
          first:=true;
          if ref.segment<>R_DEFAULT_SEG then
           begin
             if current_module^.output_format in [of_nasm,of_obj] then
              s:='['+int_reg2str[segment]+':'
             else
              s:=int_reg2str[segment]+':[';
           end
          else
           s:='[';

         if assigned(symbol) then
          begin
            s:=s+symbol^;
            first:=false;
          end;
         if (base<>R_NO) then
          begin
            if not(first) then
             s:=s+'+'
            else
             first:=false;
             s:=s+int_reg2str[base];
          end;
         if (index<>R_NO) then
           begin
             if not(first) then
               s:=s+'+'
             else
               first:=false;
             s:=s+int_reg2str[index];
             if scalefactor<>0 then
               s:=s+'*'+tostr(scalefactor);
           end;
         if offset<0 then
           s:=s+tostr(offset)
         else if (offset>0) then
           s:=s+'+'+tostr(offset);
         s:=s+']';
        end;
       getreferencestring:=s;
     end;

    function getopstr(t : byte;o : pointer;s : topsize; _operator: tasmop;dest : boolean) : string;

      var
    hs : string;

      begin
    case t of
       top_reg : { a floating point register can be only a register operand }
            if current_module^.output_format in [of_nasm,of_obj] then
               getopstr:=int_nasmreg2str[tregister(o)]
            else
               getopstr:=int_reg2str[tregister(o)];
       top_const,
       top_ref : begin
                  if t=top_const then
                    hs := tostr(longint(o))
                  else
                    hs:=getreferencestring(preference(o)^);
                  if current_module^.output_format in [of_nasm,of_obj] then
                    if (_operator = A_LEA) or (_operator = A_LGS)
                    or (_operator = A_LSS) or (_operator = A_LFS)
                    or (_operator = A_LES) or (_operator = A_LDS)
                    or (_operator = A_SHR) or (_operator = A_SHL)
                    or (_operator = A_SAR) or (_operator = A_SAL)
                    or (_operator = A_OUT) or (_operator = A_IN) then
                    begin
                    end
                    else
                      case s of
                         S_B : hs:='byte '+hs;
                         S_W : hs:='word '+hs;
                         S_L : hs:='dword '+hs;
                         S_IS : hs:='word '+hs;
                         S_IL : hs:='dword '+hs;
                         S_IQ : hs:='qword '+hs;
                         S_FS : hs:='dword '+hs;
                         S_FL : hs:='qword '+hs;
                         S_FX : hs:='tword '+hs;
                         S_BW : if dest then
                             hs:='word '+hs
                           else
                             hs:='byte '+hs;
                         S_BL : if dest then
                             hs:='dword '+hs
                           else
                             hs:='byte '+hs;
                         S_WL : if dest then
                             hs:='dword '+hs
                           else
                             hs:='word '+hs;
                      end
          else
          Begin
            { can possibly give a range check error under tp }
            { if using in...                                 }
            if ((_operator <> A_LGS) and (_operator <> A_LSS) and
               (_operator <> A_LFS) and (_operator <> A_LDS) and
               (_operator <> A_LES)) then
            Begin
            case s of
               S_B : hs:='byte ptr '+hs;
               S_W : hs:='word ptr '+hs;
               S_L : hs:='dword ptr '+hs;
               S_IS : hs:='word ptr '+hs;
               S_IL : hs:='dword ptr '+hs;
               S_IQ : hs:='qword ptr '+hs;
               S_FS : hs:='dword ptr '+hs;
               S_FL : hs:='qword ptr '+hs;
               S_FX : hs:='tbyte ptr '+hs;
               S_BW : if dest then
                   hs:='word ptr '+hs
                 else
                   hs:='byte ptr '+hs;
               S_BL : if dest then
                   hs:='dword ptr '+hs
                 else
                   hs:='byte ptr '+hs;
               S_WL : if dest then
                   hs:='dword ptr '+hs
                 else
                   hs:='word ptr '+hs;
            end;
            end;
          end;
              getopstr:=hs;
            end;
       top_symbol : begin
             hs[0]:=chr(strlen(pchar(pcsymbol(o)^.symbol)));
             move(pchar(pcsymbol(o)^.symbol)^,hs[1],byte(hs[0]));
             if current_module^.output_format=of_masm then
               hs:='offset '+hs
             else
               hs:='dword '+hs;

             if pcsymbol(o)^.offset>0 then
               hs:=hs+'+'+tostr(pcsymbol(o)^.offset)
             else if pcsymbol(o)^.offset<0 then
               hs:=hs+tostr(pcsymbol(o)^.offset);
             getopstr:=hs;
          end;
       else internalerror(10001);
    end;
      end;

    function getopstr_jmp(t : byte;o : pointer) : string;

      var
    hs : string;

      begin
    case t of
       top_reg : getopstr_jmp:=int_reg2str[tregister(o)];
       top_ref : getopstr_jmp:=getreferencestring(preference(o)^);
       top_const : getopstr_jmp:=tostr(longint(o));
       top_symbol : begin
             hs[0]:=chr(strlen(pchar(pcsymbol(o)^.symbol)));
             move(pchar(pcsymbol(o)^.symbol)^,hs[1],byte(hs[0]));
             if pcsymbol(o)^.offset>0 then
               hs:=hs+'+'+tostr(pcsymbol(o)^.offset)
             else if pcsymbol(o)^.offset<0 then
               hs:=hs+tostr(pcsymbol(o)^.offset);
             getopstr_jmp:=hs;
          end;
       else internalerror(10001);
    end;
      end;

{****************************************************************************
                               TI386INTASMLIST
 ****************************************************************************}

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'DD'#9,'',#9'DW'#9,#9'DB'#9);

    Function PadTabs(p:pchar;addch:char):string;
    var
      s : string;
      i : longint;
    begin
      i:=strlen(p);
      if addch<>#0 then
       begin
         inc(i);
         s:=StrPas(p)+addch;
       end
      else
       s:=StrPas(p);
      if i<8 then
       PadTabs:=s+#9#9
      else
       PadTabs:=s+#9;
    end;

    procedure ti386intasmlist.WriteTree(p:paasmoutput);
    type
      twowords=record
        word1,word2:word;
      end;
    var
      s,
      prefix,
      suffix   : string;
      hp       : pai;
      counter,
      lines,
      i,j,l    : longint;
      consttyp : tait;
      found,
      quoted   : boolean;
    begin
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         case hp^.typ of
       ait_comment : Begin
                       AsmWrite(target_asm.comment);
                       AsmWritePChar(pai_asm_comment(hp)^.str);
                       AsmLn;
                     End;
         ait_align : begin
                     { align not supported at all with nasm v095  }
                     { align with specific value not supported by }
                     { turbo assembler.                           }
                     { CAUSES PROBLEMS WITH THE SEGMENT DEFINITION   }
                     { SEGMENT DEFINITION SHOULD MATCH TYPE OF ALIGN }
                     { HERE UNDER TASM!                              }
                     { if current_module^.output_format<>of_nasm then }
                        AsmWriteLn(#9'ALIGN '+tostr(pai_align(hp)^.aligntype));
                     end;
      ait_external : begin
                       if current_module^.output_format in [of_nasm,of_obj] then
                        AsmWriteLn('EXTERN '+StrPas(pai_external(hp)^.name))
                       else
                        AsmWriteLn(#9#9'EXTRN'#9+StrPas(pai_external(hp)^.name)+
                                   ' :'+extstr[pai_external(hp)^.exttyp]);
                     end;
     ait_datablock : begin
                       if current_module^.output_format in [of_nasm,of_obj] then
                        begin
                          if pai_datablock(hp)^.is_global then
                           AsmWriteLn('GLOBAL '+StrPas(pai_datablock(hp)^.name));
                          AsmWriteLn(PadTabs(pai_datablock(hp)^.name,':')+'RESB'#9+tostr(pai_datablock(hp)^.size));
                        end
                       else
                        begin
                          if pai_datablock(hp)^.is_global then
                           AsmWriteLn(#9#9'PUBLIC'#9+StrPas(pai_datablock(hp)^.name));
                          AsmWriteLn(PadTabs(pai_datablock(hp)^.name,#0)+'DB'#9+tostr(pai_datablock(hp)^.size)+' DUP(?)');
                        end;
                     end;
   ait_const_32bit,
    ait_const_8bit,
   ait_const_16bit : begin
                       AsmWrite(ait_const2str[hp^.typ]+tostr(pai_const(hp)^.value));
                       consttyp:=hp^.typ;
                       l:=0;
                       repeat
                         found:=(not (Pai(hp^.next)=nil)) and (Pai(hp^.next)^.typ=consttyp);
                         if found then
                          begin
                            hp:=Pai(hp^.next);
                            s:=','+tostr(pai_const(hp)^.value);
                            AsmWrite(s);
                            inc(l,length(s));
                          end;
                       until (not found) or (l>line_length);
                       AsmLn;
                     end;
  ait_const_symbol : begin
                       if current_module^.output_format<>of_nasm then
                        AsmWrite(#9#9+'DD '#9'offset ')
                       else
                        AsmWrite(#9#9+'DD '#9);
                       AsmWriteLn(StrPas(pchar(pai_const(hp)^.value)));
                     end;
    ait_real_32bit : AsmWriteLn(#9#9'DD'#9+double2str(pai_single(hp)^.value));
    ait_real_64bit : AsmWriteLn(#9#9'DQ'#9+double2str(pai_double(hp)^.value));
 ait_real_extended : begin
                     { nasm v095 does not like DT with real constants }
                     { therefore write as double.                     }
                     { other possible solution: decode directly to hex}
                     { value.                                         }
                       if current_module^.output_format<>of_nasm then
                        AsmWriteLn(#9#9'DT'#9+double2str(pai_extended(hp)^.value))
                       else
                        begin
{$ifdef EXTDEBUG}
                          AsmLn;
                          AsmWriteLn('; NASM bug work around for extended real');
{$endif}
                          AsmWriteLn(#9#9'DD'#9+double2str(pai_extended(hp)^.value))
                        end;
                     end;
          ait_comp : AsmWriteLn(#9#9'DQ'#9+comp2str(pai_extended(hp)^.value));
        ait_string : begin
                       counter := 0;
                       lines := pai_string(hp)^.len div line_length;
                     { separate lines in different parts }
                       if pai_string(hp)^.len > 0 then
                        Begin
                          for j := 0 to lines-1 do
                           begin
                             AsmWrite(#9#9'DB'#9);
                             quoted:=false;
                             for i:=counter to counter+line_length do
                                begin
                                  { it is an ascii character. }
                                  if (ord(pai_string(hp)^.str[i])>31) and
                                     (ord(pai_string(hp)^.str[i])<128) and
                                     (pai_string(hp)^.str[i]<>'"') then
                                      begin
                                        if not(quoted) then
                                            begin
                                              if i>counter then
                                                AsmWrite(',');
                                              AsmWrite('"');
                                            end;
                                        AsmWrite(pai_string(hp)^.str[i]);
                                        quoted:=true;
                                      end { if > 31 and < 128 and ord('"') }
                                  else
                                      begin
                                          if quoted then
                                              AsmWrite('"');
                                          if i>counter then
                                              AsmWrite(',');
                                          quoted:=false;
                                          AsmWrite(tostr(ord(pai_string(hp)^.str[i])));
                                      end;
                               end; { end for i:=0 to... }
                             if quoted then AsmWrite('"');
                               AsmWrite(target_os.newline);
                             counter := counter+line_length;
                          end; { end for j:=0 ... }
                        { do last line of lines }
                        AsmWrite(#9#9'DB'#9);
                        quoted:=false;
                        for i:=counter to pai_string(hp)^.len-1 do
                          begin
                            { it is an ascii character. }
                            if (ord(pai_string(hp)^.str[i])>31) and
                               (ord(pai_string(hp)^.str[i])<128) and
                               (pai_string(hp)^.str[i]<>'"') then
                                begin
                                  if not(quoted) then
                                      begin
                                        if i>counter then
                                          AsmWrite(',');
                                        AsmWrite('"');
                                      end;
                                  AsmWrite(pai_string(hp)^.str[i]);
                                  quoted:=true;
                                end { if > 31 and < 128 and " }
                            else
                                begin
                                  if quoted then
                                    AsmWrite('"');
                                  if i>counter then
                                      AsmWrite(',');
                                  quoted:=false;
                                  AsmWrite(tostr(ord(pai_string(hp)^.str[i])));
                                end;
                          end; { end for i:=0 to... }
                        if quoted then
                          AsmWrite('"');
                        end;
                       AsmLn;
                     end;
         ait_label : begin
                       AsmWrite(lab2str(pai_label(hp)^.l));
                       if (current_module^.output_format in [of_obj,of_nasm]) or
                          (assigned(hp^.next) and not(pai(hp^.next)^.typ in
                           [ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_const_symbol,
                            ait_real_32bit,ait_real_64bit,ait_real_extended,ait_string])) then
                        AsmWriteLn(':');
                     end;
        ait_direct : begin
                       AsmWritePChar(pai_direct(hp)^.str);
                       AsmLn;
                     end;
ait_labeled_instruction :
                     begin
                       if (current_module^.output_format in [of_nasm,of_obj]) and
                          not (pai_labeled(hp)^._operator in [A_JMP,A_LOOP,A_LOOPZ,A_LOOPE,
                          A_LOOPNZ,A_LOOPNE,A_JCXZ,A_JECXZ]) then
                        AsmWriteLn(#9#9+int_op2str[pai_labeled(hp)^._operator]+#9+'near '+lab2str(pai_labeled(hp)^.lab))
                       else
                        AsmWriteLn(#9#9+int_op2str[pai_labeled(hp)^._operator]+#9+lab2str(pai_labeled(hp)^.lab));
                     end;
        ait_symbol : begin
                       if pai_symbol(hp)^.is_global then
                        begin
                          if current_module^.output_format in [of_nasm,of_obj] then
                           AsmWriteLn('GLOBAL '+StrPas(pai_symbol(hp)^.name))
                          else
                           AsmWriteLn(#9#9'PUBLIC'#9+StrPas(pai_symbol(hp)^.name));
                        end;
                       AsmWritePChar(pai_symbol(hp)^.name);
                       if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                        [ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_const_symbol,
                         ait_real_64bit,ait_string]) then
                        AsmWriteLn(':')
                     end;
   ait_instruction : begin
                       suffix:='';
                       prefix:= '';
                     { added prefix instructions, must be on same line as opcode }
                       if (pai386(hp)^.op1t = top_none) and
                          ((pai386(hp)^._operator = A_REP) or
                           (pai386(hp)^._operator = A_LOCK) or
                           (pai386(hp)^._operator =  A_REPE) or
                           (pai386(hp)^._operator = A_REPNE)) then
                        Begin
                          prefix:=int_op2str[pai386(hp)^._operator]+#9;
                          hp:=Pai(hp^.next);
                        { this is theorically impossible... }
                          if hp=nil then
                           begin
                             s:=#9#9+prefix;
                             AsmWriteLn(s);
                             break;
                           end;
                          { nasm prefers prefix on a line alone }
                          if (current_module^.output_format in [of_nasm,of_obj]) then
                            begin
                               AsmWriteln(#9#9+prefix);
                               prefix:='';
                            end;
                        end
                       else
                        prefix:= '';
                       { A_FNSTS need the w as suffix at least for nasm}
                       if (current_module^.output_format in [of_nasm,of_obj]) then
                         if (pai386(hp)^._operator = A_FNSTS) then
                           pai386(hp)^._operator:=A_FNSTSW
                         else if (pai386(hp)^._operator = A_FSTS) then
                           pai386(hp)^._operator:=A_FSTSW;
                       if pai386(hp)^.op1t<>top_none then
                        begin
                          if pai386(hp)^._operator in [A_CALL] then
                           begin
                             if output_format=of_nasm then
                              s:=getopstr_jmp(pai386(hp)^.op1t,pai386(hp)^.op1)
                              { with tasm call near ptr [edi+12] does not
                                work but call near [edi+12] works ?? (PM)}
                             else if pai386(hp)^.op1t=top_ref then
                                s:='near '+getopstr_jmp(pai386(hp)^.op1t,pai386(hp)^.op1)
                             else
                                s:='near ptr '+getopstr_jmp(pai386(hp)^.op1t,pai386(hp)^.op1);
                           end
                          else
                           begin
                             s:=getopstr(pai386(hp)^.op1t,pai386(hp)^.op1,pai386(hp)^.size,pai386(hp)^._operator,false);
                             if pai386(hp)^.op3t<>top_none then
                              begin
                                if pai386(hp)^.op2t<>top_none then
                                 s:=getopstr(pai386(hp)^.op2t,pointer(longint(twowords(pai386(hp)^.op2).word1)),
                                             pai386(hp)^.size,pai386(hp)^._operator,true)+','+s;
                                          s:=getopstr(pai386(hp)^.op3t,pointer(longint(twowords(pai386(hp)^.op2).word2)),
                                           pai386(hp)^.size,pai386(hp)^._operator,false)+','+s;
                              end
                             else
                              if pai386(hp)^.op2t<>top_none then
                               s:=getopstr(pai386(hp)^.op2t,pai386(hp)^.op2,pai386(hp)^.size,
                                           pai386(hp)^._operator,true)+','+s;
                           end;
                          s:=#9+s;
                        end
                       else
                        begin
                          { check if string instruction }
                          { long form, otherwise may give range check errors }
                          { in turbo pascal...                               }
                          if ((pai386(hp)^._operator = A_CMPS) or
                             (pai386(hp)^._operator = A_INS) or
                             (pai386(hp)^._operator = A_OUTS) or
                             (pai386(hp)^._operator = A_SCAS) or
                             (pai386(hp)^._operator = A_STOS) or
                             (pai386(hp)^._operator = A_MOVS) or
                             (pai386(hp)^._operator = A_LODS) or
                             (pai386(hp)^._operator = A_XLAT)) then
                           Begin
                             case pai386(hp)^.size of
                              S_B: suffix:='b';
                              S_W: suffix:='w';
                              S_L: suffix:='d';
                             else
                              Message(assem_f_invalid_suffix_intel);
                             end;
                           end;
                          s:='';
                        end;
                       AsmWriteLn(#9#9+prefix+int_op2str[pai386(hp)^._operator]+suffix+s);
                     end;
{$ifdef GDB}
             ait_stabn,
             ait_stabs,
ait_stab_function_name : ;
{$endif GDB}
         else
          internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;


    procedure ti386intasmlist.WriteAsmList;

    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Start writing intel-styled assembler output for '+current_module^.mainsource^);
{$endif}
      if current_module^.output_format in [of_nasm,of_obj] then
       begin
         WriteTree(externals);
         { INTEL ASM doesn't support stabs
         WriteTree(debuglist);}

         AsmWriteLn('BITS 32');
         AsmWriteLn('SECTION .text');
         {
         AsmWriteLn(#9#9'ASSUME'#9'CS:_TEXT,ES:DGROUP,DS:DGROUP,SS:DGROUP');
         }
         WriteTree(codesegment);

         AsmLn;
         AsmWriteLn('SECTION .data');
{$ifdef EXTDEBUG}
         AsmWriteLn(#9#9'DB'#9'"compiled by FPC '+version_string+'\0"');
         AsmWriteLn(#9#9'DB'#9'"target: '+target_info.target_name+'\0"');
{$endif EXTDEBUG}
         WriteTree(datasegment);
         WriteTree(consts);
         WriteTree(rttilist);

         AsmLn;
         AsmWriteLn('SECTION .bss');
         WriteTree(bsssegment);
       end
      else
       begin
         AsmWriteLn(#9'.386p');
         AsmWriteLn(#9'LOCALS '+target_asm.labelprefix);

         WriteTree(externals);
         { INTEL ASM doesn't support stabs
         WriteTree(debuglist);}

         AsmWriteLn('DGROUP'#9#9'GROUP'#9'_BSS,_DATA');
         AsmWriteLn('_TEXT'#9#9'SEGMENT'#9'PARA PUBLIC USE32 ''CODE''');
         AsmWriteLn(#9#9'ASSUME'#9'CS:_TEXT,ES:DGROUP,DS:DGROUP,SS:DGROUP');
         AsmLn;
         WriteTree(codesegment);
         AsmWriteLn('_TEXT'#9#9'ENDS');

         AsmLn;
         AsmWriteLn('_DATA'#9#9'SEGMENT'#9'PARA PUBLIC USE32 ''DATA''');
{$ifdef EXTDEBUG}
         AsmWriteLn(#9#9'DB'#9'"compiled by FPC '+version_string+'\0"');
         AsmWriteLn(#9#9'DB'#9'"target: '+target_info.target_name+'\0"');
{$endif EXTDEBUG}
         WriteTree(datasegment);
         WriteTree(consts);
         WriteTree(rttilist);
         AsmWriteLn('_DATA'#9#9'ENDS');

         AsmLn;
         AsmWriteLn('_BSS'#9#9'SEGMENT'#9'PARA PUBLIC USE32 ''BSS''');
         WriteTree(bsssegment);
         AsmWriteLn('_BSS'#9#9'ENDS');

         AsmLn;
         AsmWriteLn(#9#9'END');
      end;
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing intel-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
   end;

end.
{
  $Log$
  Revision 1.6  1998-05-04 17:54:24  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.5  1998/05/01 07:43:52  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.4  1998/04/29 10:33:41  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/08 16:58:01  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.2  1998/04/08 11:34:17  peter
    * nasm works (linux only tested)
}
