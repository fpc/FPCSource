{
    $Id$
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
unit ag386nsm;

{$i fpcdefs.inc}

interface

    uses
      cpubase,
      aasmbase,aasmtai,aasmcpu,assemble;

    type
      T386NasmAssembler = class(texternalassembler)
      private
        procedure WriteReference(var ref : treference);
        procedure WriteOper(const o:toper;s : topsize; opcode: tasmop;ops:longint;dest : boolean);
        procedure WriteOper_jmp(const o:toper; op : tasmop);
      public
        procedure WriteTree(p:taasmoutput);override;
        procedure WriteAsmList;override;
        procedure WriteExternals;
      end;



  implementation

    uses
{$ifdef delphi}
      sysutils,
{$endif}
      cutils,globtype,globals,systems,cclasses,
      fmodule,finput,verbose,cpuinfo,ag386int
      ;

    const
      line_length = 64;

    int_nasmreg2str : reg2strtable = ('',
     'eax','ecx','edx','ebx','esp','ebp','esi','edi',
     'ax','cx','dx','bx','sp','bp','si','di',
     'al','cl','dl','bl','ah','ch','bh','dh',
     'cs','ds','es','ss','fs','gs',
     'st0','st0','st1','st2','st3','st4','st5','st6','st7',
     'dr0','dr1','dr2','dr3','dr6','dr7',
     'cr0','cr2','cr3','cr4',
     'tr3','tr4','tr5','tr6','tr7',
     'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7',
     'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7'
   );

    var
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : tinputfile;

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


{****************************************************************************
                               T386NasmAssembler
 ****************************************************************************}

    procedure T386NasmAssembler.WriteReference(var ref : treference);
      var
        first,no_s,no_b,no_i : boolean;
      begin
        with ref do
         begin
           no_s:=(segment.enum=R_NO) or ((segment.enum=R_INTREGISTER) and (segment.number=NR_NO));
           no_b:=(base.enum=R_NO) or ((base.enum=R_INTREGISTER) and (base.number=NR_NO));
           no_i:=(index.enum=R_NO) or ((index.enum=R_INTREGISTER) and (index.number=NR_NO));
           AsmWrite('[');
           first:=true;
           inc(offset,offsetfixup);
           offsetfixup:=0;
           if not no_s then
            if segment.enum=R_INTREGISTER then
              asmwrite(intel_regname(segment.number))
            else
              asmwrite(std_reg2str[segment.enum]+':');
           if assigned(symbol) then
            begin
              AsmWrite(symbol.name);
              first:=false;
            end;
           if not no_b then
            begin
              if not(first) then
               AsmWrite('+')
              else
               first:=false;
            if base.enum=R_INTREGISTER then
              asmwrite(intel_regname(base.number))
            else
              asmwrite(int_nasmreg2str[base.enum]);
            end;
           if not no_i then
             begin
               if not(first) then
                 AsmWrite('+')
               else
                 first:=false;
              if index.enum=R_INTREGISTER then
                asmwrite(intel_regname(index.number))
              else
               AsmWrite(int_nasmreg2str[index.enum]);
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


    procedure T386NasmAssembler.WriteOper(const o:toper;s : topsize; opcode: tasmop;ops:longint;dest : boolean);
      begin
        case o.typ of
          top_reg :
            begin
              if o.reg.enum=R_INTREGISTER then
                asmwrite(intel_regname(o.reg.number))
              else
                asmwrite(int_nasmreg2str[o.reg.enum]);
            end;
          top_const :
            begin
              if (ops=1) and (opcode<>A_RET) then
               AsmWrite(sizestr(s,dest));
              AsmWrite(tostr(longint(o.val)));
            end;
          top_symbol :
            begin
              AsmWrite('dword ');
              if assigned(o.sym) then
               AsmWrite(o.sym.name);
              if o.symofs>0 then
               AsmWrite('+'+tostr(o.symofs))
              else
               if o.symofs<0 then
                AsmWrite(tostr(o.symofs))
               else
                if not(assigned(o.sym)) then
                 AsmWrite('0');
            end;
          top_ref :
            begin
              if not ((opcode = A_LEA) or (opcode = A_LGS) or
                      (opcode = A_LSS) or (opcode = A_LFS) or
                      (opcode = A_LES) or (opcode = A_LDS) or
                      (opcode = A_SHR) or (opcode = A_SHL) or
                      (opcode = A_SAR) or (opcode = A_SAL) or
                      (opcode = A_OUT) or (opcode = A_IN)) then
                AsmWrite(sizestr(s,dest));
              WriteReference(o.ref^);
            end;
          else
            internalerror(10001);
        end;
      end;


    procedure T386NasmAssembler.WriteOper_jmp(const o:toper; op : tasmop);
      begin
        case o.typ of
          top_reg :
            begin
              if o.reg.enum=R_INTREGISTER then
                asmwrite(intel_regname(o.reg.number))
              else
                asmwrite(int_nasmreg2str[o.reg.enum]);
            end;
          top_ref :
            WriteReference(o.ref^);
          top_const :
            AsmWrite(tostr(longint(o.val)));
          top_symbol :
            begin
              if not(
                     (op=A_JCXZ) or (op=A_JECXZ) or
                     (op=A_LOOP) or (op=A_LOOPE) or
                     (op=A_LOOPNE) or (op=A_LOOPNZ) or
                     (op=A_LOOPZ)
                    ) then
                AsmWrite('NEAR ');
              AsmWrite(o.sym.name);
              if o.symofs>0 then
               AsmWrite('+'+tostr(o.symofs))
              else
               if o.symofs<0 then
                AsmWrite(tostr(o.symofs));
            end;
          else
            internalerror(10001);
        end;
      end;


    var
      LasTSec : TSection;

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'DD'#9,#9'DW'#9,#9'DB'#9);

    procedure T386NasmAssembler.WriteTree(p:taasmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
    var
      s : string;
      hp       : tai;
      hp1      : tailineinfo;
      counter,
      lines,
      i,j,l    : longint;
      InlineLevel : longint;
      consttyp : taitype;
      found,
      do_line,
      quoted   : boolean;
      regstr:string[5];
    begin
      if not assigned(p) then
       exit;
      InlineLevel:=0;
      { lineinfo is only needed for codesegment (PFV) }
      do_line:=(cs_asm_source in aktglobalswitches) or
               ((cs_lineinfo in aktmoduleswitches)
                 and (p=codesegment));
      hp:=tai(p.first);
      while assigned(hp) do
       begin

         if not(hp.typ in SkipLineInfo) then
           begin
             hp1:=hp as tailineinfo;
             aktfilepos:=hp1.fileinfo;
             if do_line then
              begin
              { load infile }
                if lastfileinfo.fileindex<>hp1.fileinfo.fileindex then
                 begin
                   infile:=current_module.sourcefiles.get_file(hp1.fileinfo.fileindex);
                   if assigned(infile) then
                    begin
                      { open only if needed !! }
                      if (cs_asm_source in aktglobalswitches) then
                       infile.open;
                    end;
                   { avoid unnecessary reopens of the same file !! }
                   lastfileinfo.fileindex:=hp1.fileinfo.fileindex;
                   { be sure to change line !! }
                   lastfileinfo.line:=-1;
                 end;
              { write source }
                if (cs_asm_source in aktglobalswitches) and
                   assigned(infile) then
                 begin
                   if (infile<>lastinfile) then
                     begin
                       AsmWriteLn(target_asm.comment+'['+infile.name^+']');
                       if assigned(lastinfile) then
                         lastinfile.close;
                     end;
                   if (hp1.fileinfo.line<>lastfileinfo.line) and
                      ((hp1.fileinfo.line<infile.maxlinebuf) or (InlineLevel>0)) then
                     begin
                       if (hp1.fileinfo.line<>0) and
                          ((infile.linebuf^[hp1.fileinfo.line]>=0) or (InlineLevel>0)) then
                         AsmWriteLn(target_asm.comment+'['+tostr(hp1.fileinfo.line)+'] '+
                           fixline(infile.GetLineStr(hp1.fileinfo.line)));
                       { set it to a negative value !
                       to make that is has been read already !! PM }
                       if (infile.linebuf^[hp1.fileinfo.line]>=0) then
                         infile.linebuf^[hp1.fileinfo.line]:=-infile.linebuf^[hp1.fileinfo.line]-1;
                     end;
                 end;
                lastfileinfo:=hp1.fileinfo;
                lastinfile:=infile;
              end;
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
               if tai_regalloc(hp).reg.enum=R_INTREGISTER then
                regstr:=intel_regname(tai_regalloc(hp).reg.number)
               else
                regstr:=std_reg2str[tai_regalloc(hp).reg.enum];
               if (cs_asm_regalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Register '+regstr+
                   allocstr[tai_regalloc(hp).allocation]);
             end;

           ait_tempalloc :
             begin
               if (cs_asm_tempalloc in aktglobalswitches) then
                 begin
{$ifdef EXTDEBUG}
                   if assigned(tai_tempalloc(hp).problem) then
                     AsmWriteLn(target_asm.comment+tai_tempalloc(hp).problem^+' ('+tostr(tai_tempalloc(hp).temppos)+','+
                       tostr(tai_tempalloc(hp).tempsize)+')')
                   else
{$endif EXTDEBUG}
                     AsmWriteLn(target_asm.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                       tostr(tai_tempalloc(hp).tempsize)+allocstr[tai_tempalloc(hp).allocation]);
                 end;
             end;

           ait_section :
             begin
               if tai_section(hp).sec<>sec_none then
                begin
                  AsmLn;
                  AsmWriteLn('SECTION '+target_asm.secnames[tai_section(hp).sec]);
                end;
               LasTSec:=tai_section(hp).sec;
             end;

           ait_align :
             AsmWriteLn(#9'ALIGN '+tostr(tai_align(hp).aligntype));

           ait_datablock :
             begin
               if tai_datablock(hp).is_global then
                begin
                  AsmWrite(#9'GLOBAL ');
                  AsmWriteLn(tai_datablock(hp).sym.name);
                end;
               AsmWrite(PadTabs(tai_datablock(hp).sym.name,':'));
               AsmWriteLn('RESB'#9+tostr(tai_datablock(hp).size));
             end;

           ait_const_32bit,
           ait_const_16bit,
           ait_const_8bit :
             begin
               AsmWrite(ait_const2str[hp.typ]+tostr(tai_const(hp).value));
               consttyp:=hp.typ;
               l:=0;
               repeat
                 found:=(not (tai(hp.next)=nil)) and (tai(hp.next).typ=consttyp);
                 if found then
                  begin
                    hp:=tai(hp.next);
                    s:=','+tostr(tai_const(hp).value);
                    AsmWrite(s);
                    inc(l,length(s));
                  end;
               until (not found) or (l>line_length);
               AsmLn;
             end;

           ait_const_symbol :
             begin
               AsmWrite(#9#9'DD'#9);
               AsmWrite(tai_const_symbol(hp).sym.name);
               if tai_const_symbol(hp).offset>0 then
                 AsmWrite('+'+tostr(tai_const_symbol(hp).offset))
               else if tai_const_symbol(hp).offset<0 then
                 AsmWrite(tostr(tai_const_symbol(hp).offset));
               AsmLn;
             end;

           ait_const_rva :
             begin
               AsmWrite(#9#9'RVA'#9);
               AsmWriteLn(tai_const_symbol(hp).sym.name);
             end;

           ait_real_32bit :
             AsmWriteLn(#9#9'DD'#9+single2str(tai_real_32bit(hp).value));

           ait_real_64bit :
             AsmWriteLn(#9#9'DQ'#9+double2str(tai_real_64bit(hp).value));

           ait_real_80bit :
             AsmWriteLn(#9#9'DT'#9+extended2str(tai_real_80bit(hp).value));

           ait_comp_64bit :
             AsmWriteLn(#9#9'DQ'#9+comp2str(tai_real_80bit(hp).value));

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
               if tai_label(hp).l.is_used then
                AsmWriteLn(tai_label(hp).l.name+':');
             end;

           ait_direct :
             begin
               AsmWritePChar(tai_direct(hp).str);
               AsmLn;
             end;

           ait_symbol :
             begin
               if tai_symbol(hp).is_global then
                begin
                  AsmWrite(#9'GLOBAL ');
                  AsmWriteLn(tai_symbol(hp).sym.name);
                end;
               AsmWrite(tai_symbol(hp).sym.name);
               if assigned(hp.next) and not(tai(hp.next).typ in
                  [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                   ait_const_symbol,ait_const_rva,
                   ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                AsmWriteLn(':')
             end;

           ait_symbol_end : ;

           ait_instruction :
             begin
               taicpu(hp).CheckNonCommutativeOpcodes;
               { We need intel order, no At&t }
               taicpu(hp).SetOperandOrder(op_intel);
               s:='';
               if ((taicpu(hp).opcode=A_FADDP) or
                   (taicpu(hp).opcode=A_FMULP))
                  and (taicpu(hp).ops=0) then
                 begin
                   taicpu(hp).ops:=2;
                   taicpu(hp).oper[0].typ:=top_reg;
                   taicpu(hp).oper[0].reg.enum:=R_ST1;
                   taicpu(hp).oper[1].typ:=top_reg;
                   taicpu(hp).oper[1].reg.enum:=R_ST;
                 end;
               if taicpu(hp).opcode=A_FWAIT then
                AsmWriteln(#9#9'DB'#9'09bh')
               else
                begin
                  { We need to explicitely set
                    word prefix to get selectors
                    to be pushed in 2 bytes  PM }
                  if (taicpu(hp).opsize=S_W) and
                     ((taicpu(hp).opcode=A_PUSH) or
                      (taicpu(hp).opcode=A_POP)) and
                      (taicpu(hp).oper[0].typ=top_reg) and
                      ((taicpu(hp).oper[0].reg.enum in [firstsreg..lastsreg])) then
                    AsmWriteln(#9#9'DB'#9'066h');
                  AsmWrite(#9#9+std_op2str[taicpu(hp).opcode]+cond2str[taicpu(hp).condition]);
                  if taicpu(hp).ops<>0 then
                   begin
                     if is_calljmp(taicpu(hp).opcode) then
                      begin
                        AsmWrite(#9);
                        WriteOper_jmp(taicpu(hp).oper[0],taicpu(hp).opcode);
                      end
                     else
                      begin
                        for i:=0 to taicpu(hp).ops-1 do
                         begin
                           if i=0 then
                            AsmWrite(#9)
                           else
                            AsmWrite(',');
                           WriteOper(taicpu(hp).oper[i],taicpu(hp).opsize,taicpu(hp).opcode,taicpu(hp).ops,(i=2));
                         end;
                      end;
                   end;
                  AsmLn;
                end;
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
                  AsmCreate(tai_cut(hp).place);
                end;
             { avoid empty files }
               while assigned(hp.next) and (tai(hp.next).typ in [ait_cut,ait_section,ait_comment]) do
                begin
                  if tai(hp.next).typ=ait_section then
                    lasTSec:=tai_section(hp.next).sec;
                  hp:=tai(hp.next);
                end;
               if lasTSec<>sec_none then
                 AsmWriteLn('SECTION '+target_asm.secnames[lasTSec]);
               AsmStartSize:=AsmSize;
             end;

           ait_marker :
             if tai_marker(hp).kind=InlineStart then
               inc(InlineLevel)
             else if tai_marker(hp).kind=InlineEnd then
               dec(InlineLevel);

           else
             internalerror(10000);
         end;
         hp:=tai(hp.next);
       end;
    end;


    var
      currentasmlist : TExternalAssembler;

    procedure writeexternal(p:tnamedindexitem;arg:pointer);
      begin
        if tasmsymbol(p).defbind=AB_EXTERNAL then
         currentasmlist.AsmWriteln('EXTERN'#9+p.name);
      end;

    procedure T386NasmAssembler.WriteExternals;
      begin
        currentasmlist:=self;
        objectlibrary.symbolsearch.foreach_static({$ifdef fpcprocvar}@{$endif}writeexternal,nil);
      end;


    procedure T386NasmAssembler.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Start writing nasm-styled assembler output for '+current_module.mainsource^);
{$endif}
      LasTSec:=sec_none;
      AsmWriteLn('BITS 32');
      AsmLn;

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
      Writetree(importssection);
      { exports are written by DLLTOOL
        if we use it so don't insert it twice (PM) }
      if not UseDeffileForExport and assigned(exportssection) then
        Writetree(exportssection);
      Writetree(resourcesection);

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Done writing nasm-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
   end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_i386_nasmcoff_info : tasminfo =
          (
            id           : as_i386_nasmcoff;
            idtxt  : 'NASMCOFF';
            asmbin : 'nasm';
            asmcmd : '-f coff -o $OBJ $ASM';
            supported_target : system_i386_go32v2;
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr','')
          );

       as_i386_nasmwin32_info : tasminfo =
          (
            id           : as_i386_nasmwin32;
            idtxt  : 'NASMWIN32';
            asmbin : 'nasm';
            asmcmd : '-f win32 -o $OBJ $ASM';
            supported_target : system_i386_win32;
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr','')
          );

       as_i386_nasmobj_info : tasminfo =
          (
            id           : as_i386_nasmobj;
            idtxt  : 'NASMOBJ';
            asmbin : 'nasm';
            asmcmd : '-f obj -o $OBJ $ASM';
            supported_target : system_any; { what should I write here ?? }
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr','')
          );

       as_i386_nasmwdosx_info : tasminfo =
          (
            id           : as_i386_nasmwdosx;
            idtxt  : 'NASMWDOSX';
            asmbin : 'nasm';
            asmcmd : '-f win32 -o $OBJ $ASM';
            supported_target : system_i386_wdosx;
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr','')
          );


       as_i386_nasmelf_info : tasminfo =
          (
            id           : as_i386_nasmelf;
            idtxt  : 'NASMELF';
            asmbin : 'nasm';
            asmcmd : '-f elf -o $OBJ $ASM';
            supported_target : system_i386_linux;
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr','')
          );


initialization
  RegisterAssembler(as_i386_nasmcoff_info,T386NasmAssembler);
  RegisterAssembler(as_i386_nasmwin32_info,T386NasmAssembler);
  RegisterAssembler(as_i386_nasmwdosx_info,T386NasmAssembler);
  RegisterAssembler(as_i386_nasmobj_info,T386NasmAssembler);
  RegisterAssembler(as_i386_nasmelf_info,T386NasmAssembler);
end.
{
  $Log$
  Revision 1.32  2003-03-08 13:59:17  daniel
    * Work to handle new register notation in ag386nsm
    + Added newra version of Ti386moddivnode

  Revision 1.31  2003/03/08 08:59:07  daniel
    + $define newra will enable new register allocator
    + getregisterint will return imaginary registers with $newra
    + -sr switch added, will skip register allocation so you can see
      the direct output of the code generator before register allocation

  Revision 1.30  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.29  2002/12/24 18:10:34  peter
    * Long symbol names support

  Revision 1.28  2002/11/17 16:31:59  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.27  2002/11/15 01:58:56  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.26  2002/08/18 20:06:28  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.25  2002/08/12 15:08:41  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.24  2002/08/11 14:32:29  peter
    * renamed current_library to objectlibrary

  Revision 1.23  2002/08/11 13:24:16  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.22  2002/07/26 21:15:43  florian
    * rewrote the system handling

  Revision 1.21  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.20  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.19  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.17  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.16  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.15  2002/04/14 16:58:41  carl
  + att_reg2str -> gas_reg2str

  Revision 1.14  2002/04/04 18:27:37  carl
  + added wdosx support (patch from Pavel)

  Revision 1.13  2002/04/02 17:11:33  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

}
