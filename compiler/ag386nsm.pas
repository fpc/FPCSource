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
{$ifdef TP}
  {$N+,E+}
{$endif}
unit ag386nsm;

    interface

    uses aasm,assemble;

    type
      pi386nasmasmlist=^ti386nasmasmlist;
      ti386nasmasmlist = object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
        procedure WriteExternals;
      end;

  implementation

    uses
      dos,strings,
      globtype,globals,systems,cobjects,
      files,verbose
      ,i386base,i386asm
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


    function getreferencestring(const ref : treference) : string;
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
          if ref.segment<>R_NO then
           s:='['+int_reg2str[segment]+':'
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

    function sizestr(s:topsize;dest:boolean):string;
      begin
        case s of
           S_B : sizestr:='byte ';
           S_W : sizestr:='word ';
           S_L : sizestr:='dword ';
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
        end;
      end;


    function getopstr(const o:toper;s : topsize; opcode: tasmop;ops:longint;dest : boolean) : string;
      var
        hs : string;
      begin
        case o.typ of
          top_reg :
            getopstr:=int_nasmreg2str[o.reg];
          top_const :
            begin
              if (ops=1) and (opcode<>A_RET) then
               getopstr:=sizestr(s,dest)+tostr(o.val)
              else
               getopstr:=tostr(o.val);
            end;
          top_symbol :
            begin
              if assigned(o.sym) then
               hs:='dword '+o.sym^.name
              else
               hs:='dword ';
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
              if not ((opcode = A_LEA) or (opcode = A_LGS) or
                      (opcode = A_LSS) or (opcode = A_LFS) or
                      (opcode = A_LES) or (opcode = A_LDS) or
                      (opcode = A_SHR) or (opcode = A_SHL) or
                      (opcode = A_SAR) or (opcode = A_SAL) or
                      (opcode = A_OUT) or (opcode = A_IN)) then
               begin
                 hs:=sizestr(s,dest)+hs;
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
            getopstr_jmp:=int_nasmreg2str[o.reg];
          top_ref :
            getopstr_jmp:=getreferencestring(o.ref^);
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
                       if pai_section(hp)^.sec<>sec_none then
                        begin
                          AsmLn;
                          AsmWriteLn('SECTION '+target_asm.secnames[pai_section(hp)^.sec]);
                        end;
                       LastSec:=pai_section(hp)^.sec;
                     end;
         ait_align : AsmWriteLn(#9'ALIGN '+tostr(pai_align(hp)^.aligntype));
     ait_datablock : begin
                       if pai_datablock(hp)^.is_global then
                        AsmWriteLn(#9'GLOBAL '+pai_datablock(hp)^.sym^.name);
                       AsmWriteLn(PadTabs(pai_datablock(hp)^.sym^.name,':')+'RESB'#9+tostr(pai_datablock(hp)^.size));
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
                       AsmWriteLn(#9#9'DD'#9+pai_const_symbol(hp)^.sym^.name);
                       if pai_const_symbol(hp)^.offset>0 then
                         AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
                       else if pai_const_symbol(hp)^.offset<0 then
                         AsmWrite(tostr(pai_const_symbol(hp)^.offset));
                       AsmLn;
                     end;
     ait_const_rva : begin
                       AsmWriteLn(#9#9'RVA'#9+pai_const_symbol(hp)^.sym^.name);
                     end;
    ait_real_32bit     : AsmWriteLn(#9#9'DD'#9+single2str(pai_real_32bit(hp)^.value));
    ait_real_64bit     : AsmWriteLn(#9#9'DQ'#9+double2str(pai_real_64bit(hp)^.value));
    ait_real_80bit   : AsmWriteLn(#9#9'DT'#9+extended2str(pai_real_80bit(hp)^.value));
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
                        AsmWriteLn(pai_label(hp)^.l^.name+':');
                     end;
        ait_direct : begin
                       AsmWritePChar(pai_direct(hp)^.str);
                       AsmLn;
                     end;
        ait_symbol : begin
                       if pai_symbol(hp)^.is_global then
                        AsmWriteLn(#9'GLOBAL '+pai_symbol(hp)^.sym^.name);
                       AsmWrite(pai_symbol(hp)^.sym^.name);
                       if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_const_rva,
                           ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                        AsmWriteLn(':')
                     end;
   ait_instruction : begin
                     { We need intel order, no At&t }
                       pai386(hp)^.SwapOperands;
                     { Reset }
                       suffix:='';
                       prefix:='';
                       s:='';
                       if pai386(hp)^.ops<>0 then
                        begin
                          if is_calljmp(pai386(hp)^.opcode) then
                           s:=#9+getopstr_jmp(pai386(hp)^.oper[0])
                          else
                           begin
                             for i:=0to pai386(hp)^.ops-1 do
                              begin
                                if i=0 then
                                 sep:=#9
                                else
                                 sep:=',';
                                s:=s+sep+getopstr(pai386(hp)^.oper[i],pai386(hp)^.opsize,pai386(hp)^.opcode,pai386(hp)^.ops,(i=2));
                              end;
                           end;
                        end;
                       if pai386(hp)^.opcode=A_FWAIT then
                        AsmWriteln(#9#9'DB'#9'09bh')
                       else
                        AsmWriteLn(#9#9+prefix+int_op2str[pai386(hp)^.opcode]+
                          cond2str[pai386(hp)^.condition]+suffix+s);
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
                          AsmClose;
                          DoAssemble;
                          if pai_cut(hp)^.EndName then
                           IsEndFile:=true;
                          AsmCreate;
                        end;
                     { avoid empty files }
                       while assigned(hp^.next) and (pai(hp^.next)^.typ in [ait_cut,ait_section,ait_comment]) do
                        begin
                          if pai(hp^.next)^.typ=ait_section then
                            lastsec:=pai_section(hp^.next)^.sec;
                          hp:=pai(hp^.next);
                        end;
                       if lastsec<>sec_none then
                         AsmWriteLn('SECTION '+target_asm.secnames[lastsec]);
                       AsmStartSize:=AsmSize;
                     end;
        ait_marker : ;
         else
          internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;


    var
      currentasmlist : PAsmList;

    procedure writeexternal(p:pasmsymbol);{$ifndef FPC}far;{$endif}
      begin
        if p^.typ=AS_EXTERNAL then
         currentasmlist^.AsmWriteln('EXTERN'#9+p^.name);
      end;

    procedure ti386nasmasmlist.WriteExternals;
      begin
        currentasmlist:=@self;
        AsmSymbolList^.foreach(writeexternal);
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

      countlabelref:=false;

      WriteExternals;

    { Nasm doesn't support stabs
      WriteTree(debuglist);}

      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(bsssegment);
      countlabelref:=true;

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing nasm-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
   end;

end.
{
  $Log$
  Revision 1.40  1999-05-27 19:44:02  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.39  1999/05/23 18:41:57  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.38  1999/05/21 13:54:43  peter
    * NEWLAB for label as symbol

  Revision 1.37  1999/05/12 00:19:39  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.36  1999/05/11 16:28:16  peter
    * long lines fixed

  Revision 1.35  1999/05/10 15:18:16  peter
    * fixed condition writing

  Revision 1.34  1999/05/08 19:52:34  peter
    + MessagePos() which is enhanced Message() function but also gets the
      position info
    * Removed comp warnings

  Revision 1.33  1999/05/07 00:08:48  pierre
   * AG386BIN cond -> OLDASM, only cosmetic

  Revision 1.32  1999/05/06 09:05:11  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.31  1999/05/04 21:44:32  florian
    * changes to compile it with Delphi 4.0

  Revision 1.30  1999/05/02 22:41:50  peter
    * moved section names to systems
    * fixed nasm,intel writer

  Revision 1.29  1999/05/01 13:23:59  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.28  1999/04/17 22:17:06  pierre
    * ifdef USE_OP3 released (changed into ifndef NO_OP3)
    * SHRD and SHLD first operand (ATT syntax) can only be CL reg or immediate const

  Revision 1.27  1999/04/16 11:49:40  peter
    + tempalloc
    + -at to show temp alloc info in .s file

  Revision 1.26  1999/04/16 10:00:56  pierre
    + ifdef USE_OP3 code :
      added all missing op_... constructors for tai386 needed
      for SHRD,SHLD and IMUL code in assembler readers
      (check in tests/tbs0123.pp)

  Revision 1.25  1999/03/29 16:05:44  peter
    * optimizer working for ag386bin

  Revision 1.24  1999/03/10 13:25:44  pierre
    section order changed to get closer output from coff writer

  Revision 1.23  1999/03/04 13:55:39  pierre
    * some m68k fixes (still not compilable !)
    * new(tobj) does not give warning if tobj has no VMT !

  Revision 1.22  1999/03/02 02:56:11  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.21  1999/03/01 15:46:17  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes

  Revision 1.20  1999/02/26 00:48:14  peter
    * assembler writers fixed for ag386bin

  Revision 1.19  1999/02/25 21:02:19  peter
    * ag386bin updates
    + coff writer

  Revision 1.18  1999/02/22 02:15:00  peter
    * updates for ag386bin

  Revision 1.17  1998/12/20 16:21:23  peter
    * smartlinking doesn't crash anymore

  Revision 1.16  1998/12/16 00:27:18  peter
    * removed some obsolete version checks

  Revision 1.15  1998/12/01 11:19:39  peter
    * fixed range problem with in [tasmop]

  Revision 1.14  1998/11/30 09:42:56  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.13  1998/11/17 00:26:10  peter
    * fixed for $H+

  Revision 1.12  1998/11/12 11:19:34  pierre
   * fix for first line of function break

  Revision 1.11  1998/10/12 12:20:42  pierre
    + added tai_const_symbol_offset
      for r : pointer = @var.field;
    * better message for different arg names on implementation
      of function

  Revision 1.10  1998/10/06 17:16:34  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.9  1998/10/01 20:19:07  jonas
    + ait_marker support

  Revision 1.8  1998/09/20 17:11:22  jonas
    * released REGALLOC

  Revision 1.7  1998/08/11 14:01:43  peter
    * fixed fwait bug using direct opcode

  Revision 1.6  1998/08/10 15:49:39  peter
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
