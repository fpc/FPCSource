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
unit ag386nsm;

{$i defines.inc}

interface

    uses aasm,assemble;

    type
      T386NasmAssembler = class(texternalassembler)
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
      fmodule,finput,verbose,cpubase,cpuasm,tainst
      ;

    const
      line_length = 64;

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


    function getreferencestring(var ref : treference) : string;
    var
      s     : string;
      first : boolean;
    begin
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
            s:=s+symbol.name;
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
         if s[length(s)]='[' then
           s:=s+'0';
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
               getopstr:=sizestr(s,dest)+tostr(longint(o.val))
              else
               getopstr:=tostr(longint(o.val));
            end;
          top_symbol :
            begin
              if assigned(o.sym) then
               hs:='dword '+o.sym.name
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
            getopstr_jmp:=tostr(longint(o.val));
          top_symbol :
            begin
              hs:=o.sym.name;
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
                               T386NasmAssembler
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


    procedure T386NasmAssembler.WriteTree(p:taasmoutput);
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
      hp       : tai;
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
      hp:=tai(p.first);
      while assigned(hp) do
       begin
         aktfilepos:=hp.fileinfo;

         if not(hp.typ in nolinetai) then
           begin
             if do_line then
              begin
              { load infile }
                if lastfileinfo.fileindex<>hp.fileinfo.fileindex then
                 begin
                   infile:=current_module.sourcefiles.get_file(hp.fileinfo.fileindex);
                   if assigned(infile) then
                    begin
                      { open only if needed !! }
                      if (cs_asm_source in aktglobalswitches) then
                       infile.open;
                    end;
                   { avoid unnecessary reopens of the same file !! }
                   lastfileinfo.fileindex:=hp.fileinfo.fileindex;
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
                   if (hp.fileinfo.line<>lastfileinfo.line) and
                      ((hp.fileinfo.line<infile.maxlinebuf) or (InlineLevel>0)) then
                     begin
                       if (hp.fileinfo.line<>0) and
                          ((infile.linebuf^[hp.fileinfo.line]>=0) or (InlineLevel>0)) then
                         AsmWriteLn(target_asm.comment+'['+tostr(hp.fileinfo.line)+'] '+
                           fixline(infile.GetLineStr(hp.fileinfo.line)));
                       { set it to a negative value !
                       to make that is has been read already !! PM }
                       if (infile.linebuf^[hp.fileinfo.line]>=0) then
                         infile.linebuf^[hp.fileinfo.line]:=-infile.linebuf^[hp.fileinfo.line]-1;
                     end;
                 end;
                lastfileinfo:=hp.fileinfo;
                lastinfile:=infile;
              end;
           end;
         case hp.typ of
           ait_comment :
             Begin
               AsmWrite(target_asm.comment);
               AsmWritePChar(tai_asm_comment(hp).str);
               AsmLn;
             End;

           ait_regalloc :
             begin
               if (cs_asm_regalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Register '+att_reg2str[tairegalloc(hp).reg]+
                   allocstr[tairegalloc(hp).allocation]);
             end;

           ait_tempalloc :
             begin
               if (cs_asm_tempalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Temp '+tostr(taitempalloc(hp).temppos)+','+
                   tostr(taitempalloc(hp).tempsize)+allocstr[taitempalloc(hp).allocation]);
             end;

           ait_section :
             begin
               if tai_section(hp).sec<>sec_none then
                begin
                  AsmLn;
                  AsmWriteLn('SECTION '+target_asm.secnames[tai_section(hp).sec]);
                end;
               LastSec:=tai_section(hp).sec;
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
             { Must be done with args in ATT order }
               taicpu(hp).SetOperandOrder(op_att);
               taicpu(hp).CheckNonCommutativeOpcodes;
             { We need intel order, no At&t }
               taicpu(hp).SetOperandOrder(op_intel);
             { Reset
               suffix:='';
               prefix:='';}
               s:='';
               if ((taicpu(hp).opcode=A_FADDP) or
                   (taicpu(hp).opcode=A_FMULP))
                  and (taicpu(hp).ops=0) then
                 begin
                   taicpu(hp).ops:=2;
                   taicpu(hp).oper[0].typ:=top_reg;
                   taicpu(hp).oper[0].reg:=R_ST1;
                   taicpu(hp).oper[1].typ:=top_reg;
                   taicpu(hp).oper[1].reg:=R_ST;
                 end;
               if taicpu(hp).ops<>0 then
                begin
                  if is_calljmp(taicpu(hp).opcode) then
                   s:=#9+getopstr_jmp(taicpu(hp).oper[0],taicpu(hp).opcode)
                  else
                   begin
                      { We need to explicitely set
                        word prefix to get selectors
                        to be pushed in 2 bytes  PM }
                      if (taicpu(hp).opsize=S_W) and
                         ((taicpu(hp).opcode=A_PUSH) or
                          (taicpu(hp).opcode=A_POP)) and
                          (taicpu(hp).oper[0].typ=top_reg) and
                          ((taicpu(hp).oper[0].reg>=firstsreg) and
                           (taicpu(hp).oper[0].reg<=lastsreg)) then
                        AsmWriteln(#9#9'DB'#9'066h');
                     for i:=0 to taicpu(hp).ops-1 do
                      begin
                        if i=0 then
                         sep:=#9
                        else
                         sep:=',';
                        s:=s+sep+getopstr(taicpu(hp).oper[i],taicpu(hp).opsize,taicpu(hp).opcode,
                          taicpu(hp).ops,(i=2));
                      end;
                   end;
                end;
               if taicpu(hp).opcode=A_FWAIT then
                AsmWriteln(#9#9'DB'#9'09bh')
               else
                AsmWriteLn(#9#9+{prefix+}int_op2str[taicpu(hp).opcode]+
                  cond2str[taicpu(hp).condition]+{suffix+}s);
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
                    lastsec:=tai_section(hp.next).sec;
                  hp:=tai(hp.next);
                end;
               if lastsec<>sec_none then
                 AsmWriteLn('SECTION '+target_asm.secnames[lastsec]);
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

    procedure writeexternal(p:tnamedindexitem);
      begin
        if tasmsymbol(p).defbind=AB_EXTERNAL then
         currentasmlist.AsmWriteln('EXTERN'#9+p.name);
      end;

    procedure T386NasmAssembler.WriteExternals;
      begin
        currentasmlist:=self;
        AsmSymbolList.foreach_static({$ifdef fpcprocvar}@{$endif}writeexternal);
      end;


    procedure T386NasmAssembler.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Start writing nasm-styled assembler output for '+current_module.mainsource^);
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
      Writetree(importssection);
      { exports are written by DLLTOOL
        if we use it so don't insert it twice (PM) }
      if not UseDeffileForExport and assigned(exportssection) then
        Writetree(exportssection);
      Writetree(resourcesection);
      countlabelref:=true;

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
            supported_target : target_i386_go32v2;
            outputbinary: false;
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
          );

       as_i386_nasmwin32_info : tasminfo =
          (
            id           : as_i386_nasmwin32;
            idtxt  : 'NASMWIN32';
            asmbin : 'nasm';
            asmcmd : '-f win32 -o $OBJ $ASM';
            supported_target : target_i386_win32;
            outputbinary: false;
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
          );

       as_i386_nasmobj_info : tasminfo =
          (
            id           : as_i386_nasmobj;
            idtxt  : 'NASMOBJ';
            asmbin : 'nasm';
            asmcmd : '-f obj -o $OBJ $ASM';
            supported_target : target_any; { what should I write here ?? }
            outputbinary: false;
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
          );
          
       as_i386_nasmwdosx_info : tasminfo =
          (
            id           : as_i386_nasmwdosx;
            idtxt  : 'NASMWDOSX';
            asmbin : 'nasm';
            asmcmd : '-f win32 -o $OBJ $ASM';
            supported_target : target_i386_wdosx;
            outputbinary: false;
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
          );
          

       as_i386_nasmelf_info : tasminfo =
          (
            id           : as_i386_nasmelf;
            idtxt  : 'NASMELF';
            asmbin : 'nasm';
            asmcmd : '-f elf -o $OBJ $ASM';
            supported_target : target_i386_linux;
            outputbinary: false;
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure: false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
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
  Revision 1.14  2002-04-04 18:27:37  carl
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

  Revision 1.12  2001/12/29 15:29:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.11  2001/05/06 17:13:23  jonas
    * completed incomplete typed constant records

  Revision 1.10  2001/04/21 15:33:03  peter
    * stupid bug, finalization to initialization renaming

  Revision 1.9  2001/04/21 12:09:00  peter
    * fixed bug 1472 (merged)

  Revision 1.8  2001/04/18 22:02:00  peter
    * registration of targets and assemblers

  Revision 1.7  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.6  2001/03/05 21:39:11  peter
    * changed to class with common TAssembler also for internal assembler

  Revision 1.5  2001/02/20 21:36:39  peter
    * tasm/masm fixes merged

  Revision 1.4  2001/01/13 20:24:24  peter
    * fixed operand order that got mixed up for external writers after
      my previous assembler block valid instruction check

  Revision 1.3  2000/12/25 00:07:31  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.2  2000/11/29 00:30:43  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.1  2000/10/15 09:47:42  peter
    * moved to i386/

  Revision 1.6  2000/09/24 15:06:11  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/20 17:38:21  peter
    * smartlinking fixed for linux (merged)

  Revision 1.3  2000/07/13 12:08:24  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:30  michael
  + removed logs

}
