{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
      strings,
      globtype,globals,systems,cobjects,
      files,verbose,cpubase,cpuasm
{$ifdef GDB}
      ,gdb
{$endif GDB}
      ;

    const
      line_length = 64;

    var
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : pinputfile;
{$ifdef EXTTYPE}
      extstr : array[EXT_NEAR..EXT_ABS] of String[8] =
             ('NEAR','FAR','PROC','BYTE','WORD','DWORD',
              'CODEPTR','DATAPTR','FWORD','PWORD','QWORD','TBYTE','ABS');
{$endif}

   function fixline(s:string):string;
   {
     return s with all leading and ending spaces and tabs removed
   }
     var
       i,j,k : longint;
     begin
       i:=length(s);
       while (i>0) and (s[i] in [#9,' ']) do
        dec(i);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       for k:=j to i do
        if s[k] in [#0..#31,#127..#255] then
         s[k]:='.';
       fixline:=Copy(s,j,i-j+1);
     end;

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
          else { S_NO }
            sizestr:='';
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

    function getopstr_jmp(const o:toper; op : tasmop) : string;
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
              if (op=A_JCXZ) or (op=A_JECXZ) or
                 (op=A_LOOP) or (op=A_LOOPE) or
                 (op=A_LOOPNE) or (op=A_LOOPNZ) or
                 (op=A_LOOPZ) then
                getopstr_jmp:=hs
              else
                getopstr_jmp:='NEAR '+hs;
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
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
      nolinetai =[ait_label,
                  ait_regalloc,ait_tempalloc,
                  ait_stabn,ait_stabs,ait_section,
                  ait_cut,ait_marker,ait_align,ait_stab_function_name];
    var
      s : string;
      {prefix,
      suffix   : string; no need here }
      hp       : pai;
      counter,
      lines,
      i,j,l    : longint;
      InlineLevel : longint;
      consttyp : tait;
      found,
      do_line,
      quoted   : boolean;
      sep      : char;
    begin
      if not assigned(p) then
       exit;
      InlineLevel:=0;
      { lineinfo is only needed for codesegment (PFV) }
      do_line:=(cs_asm_source in aktglobalswitches) or
               ((cs_lineinfo in aktmoduleswitches)
                 and (p=codesegment));
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         aktfilepos:=hp^.fileinfo;

         if not(hp^.typ in nolinetai) then
           begin
             if do_line then
              begin
              { load infile }
                if lastfileinfo.fileindex<>hp^.fileinfo.fileindex then
                 begin
                   infile:=current_module^.sourcefiles^.get_file(hp^.fileinfo.fileindex);
                   if assigned(infile) then
                    begin
                      { open only if needed !! }
                      if (cs_asm_source in aktglobalswitches) then
                       infile^.open;
                    end;
                   { avoid unnecessary reopens of the same file !! }
                   lastfileinfo.fileindex:=hp^.fileinfo.fileindex;
                   { be sure to change line !! }
                   lastfileinfo.line:=-1;
                 end;
              { write source }
                if (cs_asm_source in aktglobalswitches) and
                   assigned(infile) then
                 begin
                   if (infile<>lastinfile) then
                     begin
                       AsmWriteLn(target_asm.comment+'['+infile^.name^+']');
                       if assigned(lastinfile) then
                         lastinfile^.close;
                     end;
                   if (hp^.fileinfo.line<>lastfileinfo.line) and
                      ((hp^.fileinfo.line<infile^.maxlinebuf) or (InlineLevel>0)) then
                     begin
                       if (hp^.fileinfo.line<>0) and
                          ((infile^.linebuf^[hp^.fileinfo.line]>=0) or (InlineLevel>0)) then
                         AsmWriteLn(target_asm.comment+'['+tostr(hp^.fileinfo.line)+'] '+
                           fixline(infile^.GetLineStr(hp^.fileinfo.line)));
                       { set it to a negative value !
                       to make that is has been read already !! PM }
                       if (infile^.linebuf^[hp^.fileinfo.line]>=0) then
                         infile^.linebuf^[hp^.fileinfo.line]:=-infile^.linebuf^[hp^.fileinfo.line]-1;
                     end;
                 end;
                lastfileinfo:=hp^.fileinfo;
                lastinfile:=infile;
              end;
           end;
         case hp^.typ of
           ait_comment :
             Begin
               AsmWrite(target_asm.comment);
               AsmWritePChar(pai_asm_comment(hp)^.str);
               AsmLn;
             End;

           ait_regalloc :
             begin
               if (cs_asm_regalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+
                   allocstr[pairegalloc(hp)^.allocation]);
             end;

           ait_tempalloc :
             begin
               if (cs_asm_tempalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Temp '+tostr(paitempalloc(hp)^.temppos)+','+
                   tostr(paitempalloc(hp)^.tempsize)+allocstr[paitempalloc(hp)^.allocation]);
             end;

           ait_section :
             begin
               if pai_section(hp)^.sec<>sec_none then
                begin
                  AsmLn;
                  AsmWriteLn('SECTION '+target_asm.secnames[pai_section(hp)^.sec]);
                end;
               LastSec:=pai_section(hp)^.sec;
             end;

           ait_align :
             AsmWriteLn(#9'ALIGN '+tostr(pai_align(hp)^.aligntype));

           ait_datablock :
             begin
               if pai_datablock(hp)^.is_global then
                begin
                  AsmWrite(#9'GLOBAL ');
                  AsmWriteLn(pai_datablock(hp)^.sym^.name);
                end;
               AsmWrite(PadTabs(pai_datablock(hp)^.sym^.name,':'));
               AsmWriteLn('RESB'#9+tostr(pai_datablock(hp)^.size));
             end;

           ait_const_32bit,
           ait_const_16bit,
           ait_const_8bit :
             begin
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

           ait_const_symbol :
             begin
               AsmWrite(#9#9'DD'#9);
               AsmWrite(pai_const_symbol(hp)^.sym^.name);
               if pai_const_symbol(hp)^.offset>0 then
                 AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
               else if pai_const_symbol(hp)^.offset<0 then
                 AsmWrite(tostr(pai_const_symbol(hp)^.offset));
               AsmLn;
             end;

           ait_const_rva :
             begin
               AsmWrite(#9#9'RVA'#9);
               AsmWriteLn(pai_const_symbol(hp)^.sym^.name);
             end;

           ait_real_32bit :
             AsmWriteLn(#9#9'DD'#9+single2str(pai_real_32bit(hp)^.value));

           ait_real_64bit :
             AsmWriteLn(#9#9'DQ'#9+double2str(pai_real_64bit(hp)^.value));

           ait_real_80bit :
             AsmWriteLn(#9#9'DT'#9+extended2str(pai_real_80bit(hp)^.value));

           ait_comp_64bit :
             AsmWriteLn(#9#9'DQ'#9+comp2str(pai_real_80bit(hp)^.value));

           ait_string :
             begin
               counter := 0;
               lines := pai_string(hp)^.len div line_length;
             { separate lines in different parts }
               if pai_string(hp)^.len > 0 then
                Begin
                  for j := 0 to lines-1 do
                   begin
                     AsmWrite(#9#9'DB'#9);
                     quoted:=false;
                     for i:=counter to counter+line_length-1 do
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
                     inc(counter,line_length);
                  end; { end for j:=0 ... }
                { do last line of lines }
                if counter<pai_string(hp)^.len then
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

           ait_label :
             begin
               if pai_label(hp)^.l^.is_used then
                AsmWriteLn(pai_label(hp)^.l^.name+':');
             end;

           ait_direct :
             begin
               AsmWritePChar(pai_direct(hp)^.str);
               AsmLn;
             end;

           ait_symbol :
             begin
               if pai_symbol(hp)^.is_global then
                begin
                  AsmWrite(#9'GLOBAL ');
                  AsmWriteLn(pai_symbol(hp)^.sym^.name);
                end;
               AsmWrite(pai_symbol(hp)^.sym^.name);
               if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                  [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                   ait_const_symbol,ait_const_rva,
                   ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                AsmWriteLn(':')
             end;

           ait_symbol_end :
             begin
             end;

           ait_instruction :
             begin
             { Must be done with args in ATT order }
               paicpu(hp)^.CheckNonCommutativeOpcodes;
             { We need intel order, no At&t }
               paicpu(hp)^.SwapOperands;
             { Reset
               suffix:='';
               prefix:='';}
               s:='';
               if (paicpu(hp)^.opcode=A_FADDP) and (paicpu(hp)^.ops=0) then
                 begin
                   paicpu(hp)^.ops:=2;
                   paicpu(hp)^.oper[0].typ:=top_reg;
                   paicpu(hp)^.oper[0].reg:=R_ST1;
                   paicpu(hp)^.oper[1].typ:=top_reg;
                   paicpu(hp)^.oper[1].reg:=R_ST;
                 end;
               if paicpu(hp)^.ops<>0 then
                begin
                  if is_calljmp(paicpu(hp)^.opcode) then
                   s:=#9+getopstr_jmp(paicpu(hp)^.oper[0],paicpu(hp)^.opcode)
                  else
                   begin
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
                     for i:=0 to paicpu(hp)^.ops-1 do
                      begin
                        if i=0 then
                         sep:=#9
                        else
                         sep:=',';
                        s:=s+sep+getopstr(paicpu(hp)^.oper[i],paicpu(hp)^.opsize,paicpu(hp)^.opcode,
                          paicpu(hp)^.ops,(i=2));
                      end;
                   end;
                end;
               if paicpu(hp)^.opcode=A_FWAIT then
                AsmWriteln(#9#9'DB'#9'09bh')
               else
                AsmWriteLn(#9#9+{prefix+}int_op2str[paicpu(hp)^.opcode]+
                  cond2str[paicpu(hp)^.condition]+{suffix+}s);
             end;
{$ifdef GDB}
           ait_stabn,
           ait_stabs,
           ait_force_line,
           ait_stab_function_name : ;
{$endif GDB}

           ait_cut :
             begin
             { only reset buffer if nothing has changed }
               if AsmSize=AsmStartSize then
                AsmClear
               else
                begin
                  AsmClose;
                  DoAssemble;
                  AsmCreate(pai_cut(hp)^.place);
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

           ait_marker :
             if pai_marker(hp)^.kind=InlineStart then
               inc(InlineLevel)
             else if pai_marker(hp)^.kind=InlineEnd then
               dec(InlineLevel);

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
         currentasmlist^.AsmWriteln('EXTERN'#9+p^.name);
      end;

    procedure ti386nasmasmlist.WriteExternals;
      begin
        currentasmlist:=@self;
        AsmSymbolList^.foreach({$ifndef TP}@{$endif}writeexternal);
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
      lastfileinfo.line:=-1;
      lastfileinfo.fileindex:=0;
      lastinfile:=nil;

      WriteExternals;

    { Nasm doesn't support stabs
      WriteTree(debuglist);}

      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(resourcestringlist);
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
  Revision 1.60  2000-05-15 14:11:45  pierre
   * add implicit args for FADDP

  Revision 1.59  2000/05/12 21:26:22  pierre
    * fix the FDIV FDIVR FSUB FSUBR and popping equivalent
      simply by swapping from reverse to normal and vice-versa
      when passing from one syntax to the other !

  Revision 1.58  2000/05/09 21:44:27  pierre
    * add .byte 066h to force correct pushw %es
    * handle push es as a pushl %es

  Revision 1.57  2000/04/06 07:09:15  pierre
    * handle offset fixup
    + add source lines
    * no NEAR for opcodes that only support short jumps

  Revision 1.56  2000/02/09 13:22:43  peter
    * log truncated

  Revision 1.55  2000/01/07 01:14:18  peter
    * updated copyright to 2000

  Revision 1.54  1999/11/06 14:34:16  peter
    * truncated log to 20 revs

  Revision 1.53  1999/11/02 15:06:56  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.52  1999/09/13 16:27:24  peter
    * fix for jmps to be always near
    * string writing fixed

  Revision 1.51  1999/09/10 15:41:18  peter
    * added symbol_end

  Revision 1.50  1999/09/02 18:47:43  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.49  1999/08/25 11:59:38  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.48  1999/08/04 00:22:37  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.47  1999/08/01 18:28:10  florian
    * modifications for the new code generator

  Revision 1.46  1999/07/22 09:37:33  florian
    + resourcestring implemented
    + start of longstring support

}