{
    $Id$
    Copyright (c) 1996,97 by Florian Klaempfl

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
{ R- Necessary for the in [] }
{$ifdef TP}
  {$N+,E+,R-}
{$endif}
unit ag386nsm;

    interface

    uses aasm,assemble;

    type
      pi386nasmasmlist=^ti386nasmasmlist;
      ti386nasmasmlist = object(tasmlist)
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

    function double2str(d : double) : string;
      var
         hs : string;
         p : byte;
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
         p : byte;
      begin
{$ifdef VER0_99_5}
         str(double(e),hs);
{$else}	 
         str(e,hs);
{$endif}	 
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         extended2str:=lower(hs);
      end;


    function comp2str(d : bestreal) : string;
      type
        pdouble = ^double;
      var
        c  : comp;
        dd : pdouble;
      begin
      {$ifdef TP}
         c:=d;
      {$else}
         c:=comp(d);
       {$endif}
         dd:=pdouble(@c); { this makes a bitwise copy of c into a double }
         comp2str:=double2str(dd^);
      end;


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
           s:='['+int_reg2str[segment]+':'
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
       top_reg : getopstr:=int_nasmreg2str[tregister(o)];
     top_const,
       top_ref : begin
                   if t=top_const then
                     hs := tostr(longint(o))
                   else
                     hs:=getreferencestring(preference(o)^);
                   if not ((_operator = A_LEA) or (_operator = A_LGS) or
                           (_operator = A_LSS) or (_operator = A_LFS) or
                           (_operator = A_LES) or (_operator = A_LDS) or
                           (_operator = A_SHR) or (_operator = A_SHL) or
                           (_operator = A_SAR) or (_operator = A_SAL) or
                           (_operator = A_OUT) or (_operator = A_IN)) then
                     begin
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
                     end;
                   getopstr:=hs;
                 end;
    top_symbol : begin
                   hs[0]:=chr(strlen(pchar(pcsymbol(o)^.symbol)));
                   move(pchar(pcsymbol(o)^.symbol)^,hs[1],byte(hs[0]));
                   hs:='dword '+hs;
                   if pcsymbol(o)^.offset>0 then
                     hs:=hs+'+'+tostr(pcsymbol(o)^.offset)
                   else
                     if pcsymbol(o)^.offset<0 then
                       hs:=hs+tostr(pcsymbol(o)^.offset);
                   getopstr:=hs;
                 end;
      else
        internalerror(10001);
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
                      else
                        if pcsymbol(o)^.offset<0 then
                          hs:=hs+tostr(pcsymbol(o)^.offset);
                      getopstr_jmp:=hs;
                    end;
      else
        internalerror(10001);
      end;
    end;

{****************************************************************************
                               Ti386nasmasmlist
 ****************************************************************************}

    var
      LastSec : tsection;

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'DD'#9,'',#9'DW'#9,#9'DB'#9);

      ait_section2nasmstr : array[tsection] of string[6]=
       ('','.text','.data','.bss','.idata');

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


    procedure ti386nasmasmlist.WriteTree(p:paasmoutput);
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
      if not assigned(p) then
       exit;
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         case hp^.typ of
       ait_comment : Begin
                       AsmWrite(target_asm.comment);
                       AsmWritePChar(pai_asm_comment(hp)^.str);
                       AsmLn;
                     End;
       ait_section : begin
                       if pai_section(hp)^.sec<>sec_none then
                        begin
                          AsmLn;
                          AsmWriteLn('SECTION '+ait_section2nasmstr[pai_section(hp)^.sec]);
                        end;
                       LastSec:=pai_section(hp)^.sec;
                     end;
         ait_align : AsmWriteLn(#9'ALIGN '+tostr(pai_align(hp)^.aligntype));
      ait_external : AsmWriteLn('EXTERN '+StrPas(pai_external(hp)^.name));
     ait_datablock : begin
                       if pai_datablock(hp)^.is_global then
                        AsmWriteLn(#9'GLOBAL '+StrPas(pai_datablock(hp)^.name));
                       AsmWriteLn(PadTabs(pai_datablock(hp)^.name,':')+'RESB'#9+tostr(pai_datablock(hp)^.size));
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
                       AsmWrite(#9#9+'DD '#9);
                       AsmWritePChar(pchar(pai_const(hp)^.value));
                       AsmLn;
                     end;
    ait_real_32bit : AsmWriteLn(#9#9'DD'#9+double2str(pai_single(hp)^.value));
    ait_real_64bit : AsmWriteLn(#9#9'DQ'#9+double2str(pai_double(hp)^.value));
 ait_real_extended : AsmWriteLn(#9#9'DT'#9+extended2str(pai_extended(hp)^.value));
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
                       if pai_label(hp)^.l^.is_used then
                        AsmWriteLn(lab2str(pai_label(hp)^.l)+':');
                     end;
        ait_direct : begin
                       AsmWritePChar(pai_direct(hp)^.str);
                       AsmLn;
                     end;
ait_labeled_instruction :
                     begin
                       if not (pai_labeled(hp)^._operator in [A_JMP,A_LOOP,A_LOOPZ,A_LOOPE,
                          A_LOOPNZ,A_LOOPNE,A_JCXZ,A_JECXZ]) then
                        AsmWriteLn(#9#9+int_op2str[pai_labeled(hp)^._operator]+#9+'near '+lab2str(pai_labeled(hp)^.lab))
                       else
                        AsmWriteLn(#9#9+int_op2str[pai_labeled(hp)^._operator]+#9+lab2str(pai_labeled(hp)^.lab));
                     end;
        ait_symbol : begin
                       if pai_symbol(hp)^.is_global then
                        AsmWriteLn(#9'GLOBAL '+StrPas(pai_symbol(hp)^.name));
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
                          AsmWriteln(#9#9+prefix);
                          prefix:='';
                        end
                       else
                        prefix:= '';
                       { A_FNSTS need the w as suffix at least for nasm}
                       if (pai386(hp)^._operator = A_FNSTS) then
                        pai386(hp)^._operator:=A_FNSTSW
                       else
                        if (pai386(hp)^._operator = A_FSTS) then
                         pai386(hp)^._operator:=A_FSTSW;
                       if pai386(hp)^.op1t<>top_none then
                        begin
                          if pai386(hp)^._operator in [A_CALL] then
                           s:=getopstr_jmp(pai386(hp)^.op1t,pai386(hp)^.op1)
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


    procedure ti386nasmasmlist.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Start writing nasm-styled assembler output for '+current_module^.mainsource^);
{$endif}
      LastSec:=sec_none;
      AsmWriteLn('BITS 32');
      AsmLn;

      WriteTree(externals);
    { Nasm doesn't support stabs
      WriteTree(debuglist);}

      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(bsssegment);

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing nasm-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
   end;

end.
{
  $Log$
  Revision 1.6  1998-08-10 15:49:39  peter
    * small fixes for 0.99.5

  Revision 1.5  1998/08/08 10:19:18  florian
    * small fixes to write the extended type correct

  Revision 1.4  1998/06/05 17:46:03  peter
    * tp doesn't like comp() typecast

  Revision 1.3  1998/05/28 17:24:27  peter
    - $R- for tp to solve range errors with in[]

  Revision 1.2  1998/05/25 17:11:37  pierre
    * firstpasscount bug fixed
      now all is already set correctly the first time
      under EXTDEBUG try -gp to skip all other firstpasses
      it works !!
    * small bug fixes
      - for smallsets with -dTESTSMALLSET
      - some warnings removed (by correcting code !)

  Revision 1.1  1998/05/23 01:20:56  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

}
