{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an asmoutput class for MOTOROLA syntax with
    Motorola 68000 (recognized by the Amiga Assembler and Charlie Gibbs's
    A68k)

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
unit ag68kmot;

    interface

    uses aasm,assemble;

    type
      pm68kmotasmlist=^tm68kmotasmlist;
      tm68kmotasmlist = object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
      end;

  implementation

    uses
      globtype,systems,
      dos,globals,cobjects,cpubase,
      strings,files,verbose
{$ifdef GDB}
      ,gdb
{$endif GDB}
      ;

    const
      line_length = 70;

    function double2str(d : double) : string;
      var
         hs : string;
      begin
         str(d,hs);
         double2str:=hs;
      end;


(* TO SUPPORT SOONER OR LATER!!!
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
      end; *)


    function getreferencestring(const ref : treference) : string;
      var
         s : string;
      begin
         s:='';
         if ref.isintvalue then
             s:='#'+tostr(ref.offset)
         else
           with ref do
             begin
                 if (index=R_NO) and (base=R_NO) and (direction=dir_none) then
                   begin
                     if assigned(symbol) then
                       begin
                         s:=s+symbol^;
                         if offset<0 then
                           s:=s+tostr(offset)
                         else
                         if (offset>0) then
                           s:=s+'+'+tostr(offset);
                       end
                     else
                       begin
                       { direct memory addressing }
                         s:=s+'('+tostr(offset)+').l';
                       end;
                   end
                 else
                   begin
                     if assigned(symbol) then
                       s:=s+symbol^;
                     if offset<0 then
                       s:=s+tostr(offset)
                     else
                     if (offset>0) then
                       begin
                         if (symbol=nil) then s:=tostr(offset)
                         else s:=s+'+'+tostr(offset);
                       end;
                     if (index<>R_NO) and (base=R_NO) and (direction=dir_none) then
                       begin
                         if (scalefactor = 1) or (scalefactor = 0) then
                           begin
                             if offset = 0 then
                               s:=s+'0(,'+mot_reg2str[index]+'.l)'
                             else
                               s:=s+'(,'+mot_reg2str[index]+'.l)';
                           end
                         else
                           begin
                             if offset = 0 then
                               s:=s+'0(,'+mot_reg2str[index]+'.l*'+tostr(scalefactor)+')'
                             else
                               s:=s+'(,'+mot_reg2str[index]+'.l*'+tostr(scalefactor)+')';
                           end
                       end
                     else
                     if (index=R_NO) and (base<>R_NO) and (direction=dir_inc) then
                       begin
                         if (scalefactor = 1) or (scalefactor = 0) then
                           s:=s+'('+mot_reg2str[base]+')+'
                         else
                           InternalError(10002);
                       end
                     else
                     if (index=R_NO) and (base<>R_NO) and (direction=dir_dec) then
                       begin
                         if (scalefactor = 1) or (scalefactor = 0) then
                           s:=s+'-('+mot_reg2str[base]+')'
                         else
                           InternalError(10003);
                       end
                     else
                     if (index=R_NO) and (base<>R_NO) and (direction=dir_none) then
                       begin
                         s:=s+'('+mot_reg2str[base]+')';
                       end
                     else
                     if (index<>R_NO) and (base<>R_NO) and (direction=dir_none) then
                       begin
                         if (scalefactor = 1) or (scalefactor = 0) then
                           begin
                             if offset = 0 then
                               s:=s+'0('+mot_reg2str[base]+','+mot_reg2str[index]+'.l)'
                             else
                               s:=s+'('+mot_reg2str[base]+','+mot_reg2str[index]+'.l)';
                           end
                         else
                          begin
                            if offset = 0 then
                              s:=s+'0('+mot_reg2str[base]+','+mot_reg2str[index]+'.l*'+tostr(scalefactor)+')'
                            else
                              s:=s+'('+mot_reg2str[base]+','+mot_reg2str[index]+'.l*'+tostr(scalefactor)+')';
                          end
                       end
      { if this is not a symbol, and is not in the above, then there is an error }
                     else
                     if NOT assigned(symbol) then
                       InternalError(10004);
                   end; { endif }
            end; { end with }
         getreferencestring:=s;
      end;


    function getopstr(t : byte;o : pointer) : string;
     var
      hs : string;
      i: tregister;
    begin
      case t of
       top_reg : getopstr:=mot_reg2str[tregister(o)];
         top_reglist: begin
                      hs:='';
                      for i:=R_NO to R_FPSR do
                      begin
                        if i in tregisterlist(o^) then
                         hs:=hs+mot_reg2str[i]+'/';
                      end;
                      delete(hs,length(hs),1);
                      getopstr := hs;
                    end;
       top_ref : getopstr:=getreferencestring(preference(o)^);
       top_const : getopstr:='#'+tostr(longint(o));
       top_symbol : begin
             { compare with i386 version, where this is a constant. }
             hs[0]:=chr(strlen(pchar(pcsymbol(o)^.symbol)));
                     move(pchar(pcsymbol(o)^.symbol)^,hs[1],byte(hs[0]));
{                     inc(byte(hs[0]));}
{                     hs[1]:='#';}
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
         top_reg : getopstr_jmp:=mot_reg2str[tregister(o)];
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
                              TM68KMOTASMLIST
 ****************************************************************************}

    var
      LastSec : tsection;

    const
      section2str : array[tsection] of string[6]=
       ('','CODE','DATA','BSS','','','','','','','','');

    procedure tm68kmotasmlist.WriteTree(p:paasmoutput);
    var
      hp        : pai;
      s         : string;
      counter,
      i,j,lines : longint;
      quoted    : boolean;
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
                          AsmWriteLn('SECTION _'+section2str[pai_section(hp)^.sec]+','+section2str[pai_section(hp)^.sec]);
                        end;
                       LastSec:=pai_section(hp)^.sec;
                     end;
{$ifdef DREGALLOC}
      ait_regalloc : AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+' allocated');
    ait_regdealloc : AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+' released');
{$endif DREGALLOC}
         ait_align : AsmWriteLn(#9'CNOP 0,'+tostr(pai_align(hp)^.aligntype));
      ait_external : AsmWriteLn(#9'XREF'#9+StrPas(pai_external(hp)^.name));
 ait_real_extended : Message(assem_e_extended_not_supported);
          ait_comp : Message(assem_e_comp_not_supported);
     ait_datablock : begin
                       { ------------------------------------------------------- }
                       { ----------- ALIGNMENT FOR ANY NON-BYTE VALUE ---------- }
                       { ------------- REQUIREMENT FOR 680x0 ------------------- }
                       { ------------------------------------------------------- }
                       if pai_datablock(hp)^.size <> 1 then
                        begin
                          if not(cs_littlesize in aktglobalswitches) then
                           AsmWriteLn(#9'CNOP 0,4')
                          else
                           AsmWriteLn(#9'CNOP 0,2');
                         end;
                       if pai_datablock(hp)^.is_global then
                        AsmWriteLn(#9'XDEF'#9+StrPas(pai_datablock(hp)^.name));
                       AsmWriteLn(StrPas(pai_datablock(hp)^.name)+#9#9'DS.B '+tostr(pai_datablock(hp)^.size));
                     end;
   ait_const_32bit : Begin
                       AsmWriteLn(#9#9'DC.L'#9+tostr(pai_const(hp)^.value));
                     end;
   ait_const_16bit : Begin
                       AsmWriteLn(#9#9'DC.W'#9+tostr(pai_const(hp)^.value));
                     end;
    ait_const_8bit : AsmWriteLn(#9#9'DC.B'#9+tostr(pai_const(hp)^.value));
  ait_const_symbol : Begin
                       AsmWriteLn(#9#9+'DC.L '#9+StrPas(pchar(pai_const(hp)^.value)));
                     end;
  ait_const_symbol_offset :
                     Begin
                       AsmWrite(#9#9+'DC.L '#9);
                       AsmWritePChar(pai_const_symbol_offset(hp)^.name);
                       if pai_const_symbol_offset(hp)^.offset>0 then
                         AsmWrite('+'+tostr(pai_const_symbol_offset(hp)^.offset))
                       else if pai_const_symbol_offset(hp)^.offset<0 then
                         AsmWrite(tostr(pai_const_symbol_offset(hp)^.offset));
                       AsmLn;
                     end;
    ait_real_64bit : Begin
                       AsmWriteLn(#9#9'DC.D'#9+double2str(pai_double(hp)^.value));
                     end;
    ait_real_32bit : Begin
                       AsmWriteLn(#9#9'DC.S'#9+double2str(pai_single(hp)^.value));
                     end;
{ TO SUPPORT SOONER OR LATER!!!
          ait_comp : AsmWriteLn(#9#9'DC.D'#9+comp2str(pai_extended(hp)^.value));}
        ait_string : begin
                       counter := 0;
                       lines := pai_string(hp)^.len div line_length;
                       { separate lines in different parts }
                       if pai_string(hp)^.len > 0 then
                       Begin
                         for j := 0 to lines-1 do
                           begin
                              AsmWrite(#9#9'DC.B'#9);
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
                                AsmLn;
                                counter := counter+line_length;
                               end; { end for j:=0 ... }
                               { do last line of lines }
                               AsmWrite(#9#9'DC.B'#9);
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
                             if quoted then AsmWrite('"');
                          end; { endif }
                        AsmLn;
                      end;
          ait_label : begin
                       if assigned(hp^.next) and (pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                            ait_const_symbol,ait_const_symbol_offset,
                           ait_real_64bit,ait_real_32bit,ait_string]) then
                        begin
                          if not(cs_littlesize in aktglobalswitches) then
                           AsmWriteLn(#9'CNOP 0,4')
                          else
                           AsmWriteLn(#9'CNOP 0,2');
                        end;
                        AsmWrite(lab2str(pai_label(hp)^.l));
                        if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                           [ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_const_symbol,
                            ait_real_64bit,ait_string]) then
                         AsmWriteLn(':');
                      end;
         ait_direct : begin
                        AsmWritePChar(pai_direct(hp)^.str);
                        AsmLn;
                      end;
ait_labeled_instruction :
                      Begin
                      { labeled operand }
                        if pai_labeled(hp)^._op1 = R_NO then
                         AsmWriteLn(#9+mot_op2str[pai_labeled(hp)^._operator]+#9+lab2str(pai_labeled(hp)^.lab))
                        else
                      { labeled operand with register }
                         AsmWriteLn(#9+mot_op2str[pai_labeled(hp)^._operator]+#9+
                                    mot_reg2str[pai_labeled(hp)^._op1]+','+lab2str(pai_labeled(hp)^.lab))
                     end;
        ait_symbol : begin
                       { ------------------------------------------------------- }
                       { ----------- ALIGNMENT FOR ANY NON-BYTE VALUE ---------- }
                       { ------------- REQUIREMENT FOR 680x0 ------------------- }
                       { ------------------------------------------------------- }
                       if assigned(hp^.next) and (pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,
                           ait_const_symbol,ait_const_symbol_offset,ait_const_8bit,
                           ait_real_64bit,ait_real_32bit,ait_string]) then
                        begin
                          if not(cs_littlesize in aktglobalswitches) then
                           AsmWriteLn(#9'CNOP 0,4')
                          else
                           AsmWriteLn(#9'CNOP 0,2');
                        end;
                       if pai_symbol(hp)^.is_global then
                        AsmWriteLn(#9'XDEF '+StrPas(pai_symbol(hp)^.name));
                       AsmWritePChar(pai_symbol(hp)^.name);
                       if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_const_symbol_offset,
                           ait_real_64bit,ait_string,ait_real_32bit]) then
                        AsmWriteLn(':');
                     end;
   ait_instruction : begin
                       s:=#9+mot_op2str[pai68k(hp)^._operator]+mot_opsize2str[pai68k(hp)^.size];
                       if pai68k(hp)^.op1t<>top_none then
                        begin
                        { call and jmp need an extra handling                          }
                        { this code is only called if jmp isn't a labeled instruction }
                          if pai68k(hp)^._operator in [A_JSR,A_JMP] then
                           s:=s+#9+getopstr_jmp(pai68k(hp)^.op1t,pai68k(hp)^.op1)
                          else
                           begin
                             if pai68k(hp)^.op1t = top_reglist then
                              s:=s+#9+getopstr(pai68k(hp)^.op1t,@(pai68k(hp)^.reglist))
                             else
                              s:=s+#9+getopstr(pai68k(hp)^.op1t,pai68k(hp)^.op1);
                             if pai68k(hp)^.op2t<>top_none then
                              begin
                                if pai68k(hp)^.op2t = top_reglist then
                                 s:=s+','+getopstr(pai68k(hp)^.op2t,@pai68k(hp)^.reglist)
                                else
                                 s:=s+','+getopstr(pai68k(hp)^.op2t,pai68k(hp)^.op2);
                             { three operands }
                                if pai68k(hp)^.op3t<>top_none then
                                 begin
                                   if (pai68k(hp)^._operator = A_DIVSL) or
                                      (pai68k(hp)^._operator = A_DIVUL) or
                                      (pai68k(hp)^._operator = A_MULU) or
                                      (pai68k(hp)^._operator = A_MULS) or
                                      (pai68k(hp)^._operator = A_DIVS) or
                                      (pai68k(hp)^._operator = A_DIVU) then
                                    s:=s+':'+getopstr(pai68k(hp)^.op3t,pai68k(hp)^.op3)
                                   else
                                    s:=s+','+getopstr(pai68k(hp)^.op3t,pai68k(hp)^.op3);
                                 end;
                              end;
                           end;
                        end;
                       AsmWriteLn(s);
                     end;
{$ifdef GDB}
              ait_stabn,
              ait_stabs,
         ait_force_line,
 ait_stab_function_name : ;
{$endif GDB}
        ait_marker : ;
         else
          internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;

    procedure tm68kmotasmlist.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Start writing motorola-styled assembler output for '+current_module^.mainsource^);
{$endif}

      countlabelref:=false;
      WriteTree(externals);
    { WriteTree(debuglist);}
      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(bsssegment);
      Writetree(importssection);
      Writetree(exportssection);
      Writetree(resourcesection);
      countlabelref:=true;

      AsmLn;
      AsmWriteLn(#9'END');
      AsmLn;

{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing motorola-styled assembler output for '+current_module^.mainsource^);
{$endif}
    end;

end.
{
  $Log$
  Revision 1.1  2000-07-13 06:29:43  michael
  + Initial import

  Revision 1.20  2000/02/09 13:22:44  peter
    * log truncated

  Revision 1.19  2000/01/07 01:14:18  peter
    * updated copyright to 2000

  Revision 1.18  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

}
