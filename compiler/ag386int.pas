{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
{$ifdef TP}
  {$N+,E+}
{$endif}
unit ag386int;

    interface

    uses aasm,assemble;

    type
      pi386intasmlist=^ti386intasmlist;
      ti386intasmlist = object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
        procedure WriteExternals;
      end;

  implementation

    uses
      strings,
      globtype,globals,systems,cobjects,
      files,verbose,cpubase,cpuasm
{$ifdef GDB}
      ,gdb
{$endif GDB}
      ;

    const
      line_length = 70;

{$ifdef EXTTYPE}
      extstr : array[EXT_NEAR..EXT_ABS] of String[8] =
             ('NEAR','FAR','PROC','BYTE','WORD','DWORD',
              'CODEPTR','DATAPTR','FWORD','PWORD','QWORD','TBYTE','ABS');
{$endif}

    function single2str(d : single) : string;
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
         single2str:=lower(hs);
      end;

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


    function comp2str(d : bestreal) : string;
      type
        pdouble = ^double;
      var
        c  : comp;
        dd : pdouble;
      begin
{$ifdef FPC}
         c:=comp(d);
{$else}
         c:=d;
{$endif}
         dd:=pdouble(@c); { this makes a bitwise copy of c into a double }
         comp2str:=double2str(dd^);
      end;

    function getreferencestring(var ref : treference) : string;
    var
      s     : string;
      first : boolean;
    begin
      if ref.is_immediate then
       begin
         getreferencestring:=tostr(ref.offset);
         exit;
       end
      else
      with ref do
        begin
          first:=true;
          inc(offset,offsetfixup);
          offsetfixup:=0;
          if ref.segment<>R_NO then
           s:=int_reg2str[segment]+':['
          else
           s:='[';
         if assigned(symbol) then
          begin
            s:=s+symbol^.name;
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


    function getopstr(const o:toper;s : topsize; opcode: tasmop;dest : boolean) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr:=int_reg2str[o.reg];
        top_const :
          getopstr:=tostr(o.val);
        top_symbol :
          begin
            if assigned(o.sym) then
              hs:='offset '+o.sym^.name
            else
              hs:='offset ';
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs)
            else
             if not(assigned(o.sym)) then
               hs:=hs+'0';
            getopstr:=hs;
          end;
        top_ref :
          begin
            hs:=getreferencestring(o.ref^);
            if ((opcode <> A_LGS) and (opcode <> A_LSS) and
                (opcode <> A_LFS) and (opcode <> A_LDS) and
                (opcode <> A_LES)) then
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
            getopstr:=hs;
          end;
        else
          internalerror(10001);
      end;
    end;

    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr_jmp:=int_reg2str[o.reg];
        top_const :
          getopstr_jmp:=tostr(o.val);
        top_symbol :
          begin
            hs:=o.sym^.name;
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs);
            getopstr_jmp:=hs;
          end;
        top_ref :
          getopstr_jmp:=getreferencestring(o.ref^);
        else
          internalerror(10001);
      end;
    end;


{****************************************************************************
                               TI386INTASMLIST
 ****************************************************************************}

    var
      LastSec : tsection;

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'DD'#9,#9'DW'#9,#9'DB'#9);

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

    procedure ti386intasmlist.WriteTree(p:paasmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
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
      sep      : char;
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
       ait_regalloc,
       ait_tempalloc : ;
       ait_section : begin
                       if LastSec<>sec_none then
                        AsmWriteLn('_'+target_asm.secnames[LastSec]+#9#9'ENDS');
                       if pai_section(hp)^.sec<>sec_none then
                        begin
                          AsmLn;
                          AsmWriteLn('_'+target_asm.secnames[pai_section(hp)^.sec]+#9#9+
                                     'SEGMENT'#9'PARA PUBLIC USE32 '''+
                                     target_asm.secnames[pai_section(hp)^.sec]+'''');
                        end;
                       LastSec:=pai_section(hp)^.sec;
                     end;
         ait_align : begin
                     { CAUSES PROBLEMS WITH THE SEGMENT DEFINITION   }
                     { SEGMENT DEFINITION SHOULD MATCH TYPE OF ALIGN }
                     { HERE UNDER TASM!                              }
                       AsmWriteLn(#9'ALIGN '+tostr(pai_align(hp)^.aligntype));
                     end;
     ait_datablock : begin
                       if pai_datablock(hp)^.is_global then
                         AsmWriteLn(#9'PUBLIC'#9+pai_datablock(hp)^.sym^.name);
                       AsmWriteLn(PadTabs(pai_datablock(hp)^.sym^.name,#0)+'DB'#9+tostr(pai_datablock(hp)^.size)+' DUP(?)');
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
                       AsmWriteLn(#9#9'DD'#9'offset '+pai_const_symbol(hp)^.sym^.name);
                       if pai_const_symbol(hp)^.offset>0 then
                         AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
                       else if pai_const_symbol(hp)^.offset<0 then
                         AsmWrite(tostr(pai_const_symbol(hp)^.offset));
                       AsmLn;
                     end;
     ait_const_rva : begin
                       AsmWriteLn(#9#9'RVA'#9+pai_const_symbol(hp)^.sym^.name);
                     end;
        ait_real_32bit : AsmWriteLn(#9#9'DD'#9+single2str(pai_real_32bit(hp)^.value));
        ait_real_64bit : AsmWriteLn(#9#9'DQ'#9+double2str(pai_real_64bit(hp)^.value));
      ait_real_80bit : AsmWriteLn(#9#9'DT'#9+extended2str(pai_real_80bit(hp)^.value));
          ait_comp_64bit : AsmWriteLn(#9#9'DQ'#9+comp2str(pai_real_80bit(hp)^.value));
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
                        begin
                          AsmWrite(pai_label(hp)^.l^.name);
                          if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                             [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                              ait_const_symbol,ait_const_rva,
                              ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                           AsmWriteLn(':');
                        end;
                     end;
        ait_direct : begin
                       AsmWritePChar(pai_direct(hp)^.str);
                       AsmLn;
                     end;
        ait_symbol : begin
                       if pai_symbol(hp)^.is_global then
                         AsmWriteLn(#9'PUBLIC'#9+pai_symbol(hp)^.sym^.name);
                       AsmWrite(pai_symbol(hp)^.sym^.name);
                       if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_const_rva,
                           ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                        AsmWriteLn(':')
                     end;
    ait_symbol_end : begin
                     end;
   ait_instruction : begin
                     { Must be done with args in ATT order }
                       paicpu(hp)^.CheckNonCommutativeOpcodes;
                     { We need intel order, no At&t }
                       paicpu(hp)^.SwapOperands;
                     { Reset }
                       suffix:='';
                       prefix:= '';
                       s:='';
                      { We need to explicitely set
                        word prefix to get selectors
                        to be pushed in 2 bytes  PM }
                      if (paicpu(hp)^.opsize=S_W) and
                         ((paicpu(hp)^.opcode=A_PUSH) or
                          (paicpu(hp)^.opcode=A_POP)) and
                          (paicpu(hp)^.oper[0].typ=top_reg) and
                          ((paicpu(hp)^.oper[0].reg>=firstsreg) and
                           (paicpu(hp)^.oper[0].reg<=lastsreg)) then
                        AsmWriteln(#9#9'DB'#9'066h');
                     { added prefix instructions, must be on same line as opcode }
                       if (paicpu(hp)^.ops = 0) and
                          ((paicpu(hp)^.opcode = A_REP) or
                           (paicpu(hp)^.opcode = A_LOCK) or
                           (paicpu(hp)^.opcode =  A_REPE) or
                           (paicpu(hp)^.opcode =  A_REPNZ) or
                           (paicpu(hp)^.opcode =  A_REPZ) or
                           (paicpu(hp)^.opcode = A_REPNE)) then
                        Begin
                          prefix:=int_op2str[paicpu(hp)^.opcode]+#9;
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
                       if paicpu(hp)^.ops<>0 then
                        begin
                          if is_calljmp(paicpu(hp)^.opcode) then
                           s:=#9+getopstr_jmp(paicpu(hp)^.oper[0])
                          else
                           begin
                             for i:=0to paicpu(hp)^.ops-1 do
                              begin
                                if i=0 then
                                 sep:=#9
                                else
                                 sep:=',';
                                s:=s+sep+getopstr(paicpu(hp)^.oper[i],paicpu(hp)^.opsize,paicpu(hp)^.opcode,(i=2));
                              end;
                           end;
                        end;
                       AsmWriteLn(#9#9+prefix+int_op2str[paicpu(hp)^.opcode]+cond2str[paicpu(hp)^.condition]+suffix+s);
                     end;
{$ifdef GDB}
             ait_stabn,
             ait_stabs,
        ait_force_line,
ait_stab_function_name : ;
{$endif GDB}
           ait_cut : begin
                     { only reset buffer if nothing has changed }
                       if AsmSize=AsmStartSize then
                        AsmClear
                       else
                        begin
                          if LastSec<>sec_none then
                           AsmWriteLn('_'+target_asm.secnames[LastSec]+#9#9'ENDS');
                          AsmLn;
                          AsmWriteLn(#9'END');
                          AsmClose;
                          DoAssemble;
                          AsmCreate(pai_cut(hp)^.place);
                        end;
                     { avoid empty files }
                       while assigned(hp^.next) and (pai(hp^.next)^.typ in [ait_cut,ait_section,ait_comment]) do
                        begin
                          if pai(hp^.next)^.typ=ait_section then
                           begin
                             lastsec:=pai_section(hp^.next)^.sec;
                           end;
                          hp:=pai(hp^.next);
                        end;
                       AsmWriteLn(#9'.386p');
                       { I was told that this isn't necesarry because }
                       { the labels generated by FPC are unique (FK)  }
                       { AsmWriteLn(#9'LOCALS '+target_asm.labelprefix); }
                       if lastsec<>sec_none then
                          AsmWriteLn('_'+target_asm.secnames[lastsec]+#9#9+
                                     'SEGMENT'#9'PARA PUBLIC USE32 '''+
                                     target_asm.secnames[lastsec]+'''');
                       AsmStartSize:=AsmSize;
                     end;
             ait_marker: ;
         else
          internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;

    var
      currentasmlist : PAsmList;

    procedure writeexternal(p:pnamedindexobject);{$ifndef FPC}far;{$endif}
      begin
        if pasmsymbol(p)^.typ=AS_EXTERNAL then
         currentasmlist^.AsmWriteln(#9'EXTRN'#9+p^.name);
      end;

    procedure ti386intasmlist.WriteExternals;
      begin
        currentasmlist:=@self;
        AsmSymbolList^.foreach({$ifndef VER70}@{$endif}writeexternal);
      end;


    procedure ti386intasmlist.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Start writing intel-styled assembler output for '+current_module^.mainsource^);
{$endif}
      LastSec:=sec_none;
      AsmWriteLn(#9'.386p');
      AsmWriteLn(#9'LOCALS '+target_asm.labelprefix);
      AsmWriteLn('DGROUP'#9'GROUP'#9'_BSS,_DATA');
      AsmWriteLn(#9'ASSUME'#9'CS:_CODE,ES:DGROUP,DS:DGROUP,SS:DGROUP');
      AsmLn;

      countlabelref:=false;

      WriteExternals;

    { INTEL ASM doesn't support stabs
      WriteTree(debuglist);}

      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(resourcestringlist);
      WriteTree(bsssegment);
      countlabelref:=true;

      AsmWriteLn(#9'END');
      AsmLn;

{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing intel-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
   end;

end.
{
  $Log$
  Revision 1.1  2000-07-13 06:29:43  michael
  + Initial import

  Revision 1.62  2000/05/12 21:26:22  pierre
    * fix the FDIV FDIVR FSUB FSUBR and popping equivalent
      simply by swapping from reverse to normal and vice-versa
      when passing from one syntax to the other !

  Revision 1.61  2000/05/09 21:44:27  pierre
    * add .byte 066h to force correct pushw %es
    * handle push es as a pushl %es

  Revision 1.60  2000/04/06 07:05:57  pierre
   * handle offsetfixup

  Revision 1.59  2000/02/09 13:22:43  peter
    * log truncated

  Revision 1.58  2000/01/07 01:14:18  peter
    * updated copyright to 2000

  Revision 1.57  1999/12/19 17:36:25  florian
    * generation of LOCALS @@ removed

  Revision 1.56  1999/11/06 14:34:16  peter
    * truncated log to 20 revs

  Revision 1.55  1999/11/02 15:06:56  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.54  1999/09/10 15:41:18  peter
    * added symbol_end

  Revision 1.53  1999/09/02 18:47:42  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.52  1999/08/25 11:59:36  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.51  1999/08/04 00:22:36  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.50  1999/07/22 09:37:31  florian
    + resourcestring implemented
    + start of longstring support

}