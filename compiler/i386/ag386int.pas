{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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
{
  This unit implements an asmoutput class for Intel syntax with Intel i386+
}
unit ag386int;

{$i fpcdefs.inc}

interface

    uses
      cpubase,
      aasmbase,aasmtai,aasmcpu,assemble;

    type
      T386IntelAssembler = class(TExternalAssembler)
      private
        procedure WriteReference(var ref : treference);
        procedure WriteOper(const o:toper;s : topsize; opcode: tasmop;dest : boolean);
        procedure WriteOper_jmp(const o:toper;s : topsize);
      public
        procedure WriteTree(p:TAAsmoutput);override;
        procedure WriteAsmList;override;
        Function  DoAssemble:boolean;override;
        procedure WriteExternals;
      end;


implementation

    uses
{$ifdef delphi}
      sysutils,
{$endif}
      cutils,globtype,globals,systems,cclasses,
      verbose,finput,fmodule,script,cpuinfo,
      itx86int
      ;

    const
      line_length = 70;

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


{****************************************************************************
                               T386IntelAssembler
 ****************************************************************************}

    procedure T386IntelAssembler.WriteReference(var ref : treference);
      var
        first : boolean;
      begin
        with ref do
         begin
           first:=true;
           if segment<>NR_NO then
            AsmWrite(masm_regname(segment)+':[')
           else
            AsmWrite('[');
           if assigned(symbol) then
            begin
              if (aktoutputformat = as_i386_tasm) then
                AsmWrite('dword ptr ');
              AsmWrite(symbol.name);
              first:=false;
            end;
           if (base<>NR_NO) then
            begin
              if not(first) then
               AsmWrite('+')
              else
               first:=false;
               AsmWrite(masm_regname(base));
            end;
           if (index<>NR_NO) then
            begin
              if not(first) then
               AsmWrite('+')
              else
               first:=false;
              AsmWrite(masm_regname(index));
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


    procedure T386IntelAssembler.WriteOper(const o:toper;s : topsize; opcode: tasmop;dest : boolean);
      begin
        case o.typ of
          top_reg :
            AsmWrite(masm_regname(o.reg));
          top_const :
            AsmWrite(tostr(longint(o.val)));
          top_symbol :
            begin
              AsmWrite('offset ');
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
              if ((opcode <> A_LGS) and (opcode <> A_LSS) and
                  (opcode <> A_LFS) and (opcode <> A_LDS) and
                  (opcode <> A_LES)) then
               Begin
                 case s of
                  S_B : AsmWrite('byte ptr ');
                  S_W : AsmWrite('word ptr ');
                  S_L : AsmWrite('dword ptr ');
                 S_IS : AsmWrite('word ptr ');
                 S_IL : AsmWrite('dword ptr ');
                 S_IQ : AsmWrite('qword ptr ');
                 S_FS : AsmWrite('dword ptr ');
                 S_FL : AsmWrite('qword ptr ');
                 S_FX : AsmWrite('tbyte ptr ');
                 S_BW : if dest then
                         AsmWrite('word ptr ')
                        else
                         AsmWrite('byte ptr ');
                 S_BL : if dest then
                         AsmWrite('dword ptr ')
                        else
                         AsmWrite('byte ptr ');
                 S_WL : if dest then
                         AsmWrite('dword ptr ')
                        else
                         AsmWrite('word ptr ');
                 end;
               end;
              WriteReference(o.ref^);
            end;
          else
            internalerror(10001);
        end;
      end;


    procedure T386IntelAssembler.WriteOper_jmp(const o:toper;s : topsize);
    begin
      case o.typ of
        top_reg :
          AsmWrite(masm_regname(o.reg));
        top_const :
          AsmWrite(tostr(longint(o.val)));
        top_symbol :
          begin
            AsmWrite(o.sym.name);
            if o.symofs>0 then
             AsmWrite('+'+tostr(o.symofs))
            else
             if o.symofs<0 then
              AsmWrite(tostr(o.symofs));
          end;
        top_ref :
          { what about lcall or ljmp ??? }
          begin
            if (aktoutputformat <> as_i386_tasm) then
              begin
                if s=S_FAR then
                  AsmWrite('far ptr ')
                else
                  AsmWrite('dword ptr ');
              end;
            WriteReference(o.ref^);
          end;
        else
          internalerror(10001);
      end;
    end;


    var
      LasTSec : TSection;
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : tinputfile;

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

    procedure T386IntelAssembler.WriteTree(p:TAAsmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
    var
      s,
      prefix,
      suffix   : string;
      hp       : tai;
      hp1      : tailineinfo;
      counter,
      lines,
      InlineLevel : longint;
      i,j,l    : longint;
      consttyp : taitype;
      found,
      do_line,DoNotSplitLine,
      quoted   : boolean;
    begin
      if not assigned(p) then
       exit;
      { lineinfo is only needed for codesegment (PFV) }
      do_line:=((cs_asm_source in aktglobalswitches) or
                (cs_lineinfo in aktmoduleswitches))
                 and (p=codesegment);
      InlineLevel:=0;
      DoNotSplitLine:=false;
      hp:=tai(p.first);
      while assigned(hp) do
       begin
         if do_line and not(hp.typ in SkipLineInfo) and
            not DoNotSplitLine then
           begin
              hp1:=hp as tailineinfo;
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
         DoNotSplitLine:=false;
         case hp.typ of
       ait_comment : Begin
                       AsmWrite(target_asm.comment);
                       AsmWritePChar(tai_comment(hp).str);
                       AsmLn;
                     End;

           ait_regalloc :
             begin
               if (cs_asm_regalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Register '+masm_regname(tai_regalloc(hp).reg)+
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

       ait_section : begin
                       if LasTSec<>sec_none then
                        AsmWriteLn('_'+target_asm.secnames[LasTSec]+#9#9'ENDS');
                       if tai_section(hp).sec<>sec_none then
                        begin
                          AsmLn;
                          AsmWriteLn('_'+target_asm.secnames[tai_section(hp).sec]+#9#9+
                                     'SEGMENT'#9'PARA PUBLIC USE32 '''+
                                     target_asm.secnames[tai_section(hp).sec]+'''');
                        end;
                       LasTSec:=tai_section(hp).sec;
                     end;
         ait_align : begin
                     { CAUSES PROBLEMS WITH THE SEGMENT DEFINITION   }
                     { SEGMENT DEFINITION SHOULD MATCH TYPE OF ALIGN }
                     { HERE UNDER TASM!                              }
                       AsmWriteLn(#9'ALIGN '+tostr(tai_align(hp).aligntype));
                     end;
     ait_datablock : begin
                       if tai_datablock(hp).is_global then
                         AsmWriteLn(#9'PUBLIC'#9+tai_datablock(hp).sym.name);
                       AsmWriteLn(PadTabs(tai_datablock(hp).sym.name,#0)+'DB'#9+tostr(tai_datablock(hp).size)+' DUP(?)');
                     end;
   ait_const_32bit,
    ait_const_8bit,
   ait_const_16bit : begin
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
  ait_const_symbol : begin
                       AsmWriteLn(#9#9'DD'#9'offset '+tai_const_symbol(hp).sym.name);
                       if tai_const_symbol(hp).offset>0 then
                         AsmWrite('+'+tostr(tai_const_symbol(hp).offset))
                       else if tai_const_symbol(hp).offset<0 then
                         AsmWrite(tostr(tai_const_symbol(hp).offset));
                       AsmLn;
                     end;
     ait_const_rva : begin
                       AsmWriteLn(#9#9'RVA'#9+tai_const_symbol(hp).sym.name);
                     end;
        ait_real_32bit : AsmWriteLn(#9#9'DD'#9+single2str(tai_real_32bit(hp).value));
        ait_real_64bit : AsmWriteLn(#9#9'DQ'#9+double2str(tai_real_64bit(hp).value));
      ait_real_80bit : AsmWriteLn(#9#9'DT'#9+extended2str(tai_real_80bit(hp).value));
          ait_comp_64bit : AsmWriteLn(#9#9'DQ'#9+comp2str(tai_real_80bit(hp).value));
        ait_string : begin
                       counter := 0;
                       lines := tai_string(hp).len div line_length;
                     { separate lines in different parts }
                       if tai_string(hp).len > 0 then
                        Begin
                          for j := 0 to lines-1 do
                           begin
                             AsmWrite(#9#9'DB'#9);
                             quoted:=false;
                             for i:=counter to counter+line_length do
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
                             counter := counter+line_length;
                          end; { end for j:=0 ... }
                        { do last line of lines }
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
         ait_label : begin
                       if tai_label(hp).l.is_used then
                        begin
                          AsmWrite(tai_label(hp).l.name);
                          if assigned(hp.next) and not(tai(hp.next).typ in
                             [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                              ait_const_symbol,ait_const_rva,
                              ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                           AsmWriteLn(':')
                          else
                           DoNotSplitLine:=true;
                        end;
                     end;
        ait_direct : begin
                       AsmWritePChar(tai_direct(hp).str);
                       AsmLn;
                     end;
        ait_symbol : begin
                       if tai_symbol(hp).is_global then
                         AsmWriteLn(#9'PUBLIC'#9+tai_symbol(hp).sym.name);
                       AsmWrite(tai_symbol(hp).sym.name);
                       if assigned(hp.next) and not(tai(hp.next).typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_const_rva,
                           ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                        AsmWriteLn(':')
                     end;
    ait_symbol_end : begin
                     end;
   ait_instruction : begin
                       taicpu(hp).CheckNonCommutativeOpcodes;
                       taicpu(hp).SetOperandOrder(op_intel);
                       { Reset }
                       suffix:='';
                       prefix:= '';
                       { We need to explicitely set
                         word prefix to get selectors
                         to be pushed in 2 bytes  PM }
                       if (taicpu(hp).opsize=S_W) and
                           (
                            (
                             (taicpu(hp).opcode=A_PUSH) or
                             (taicpu(hp).opcode=A_POP)
                            ) and
                            (taicpu(hp).oper[0].typ=top_reg) and
                            is_segment_reg(taicpu(hp).oper[0].reg)
                           ) then
                         AsmWriteln(#9#9'DB'#9'066h');

                       { added prefix instructions, must be on same line as opcode }
                       if (taicpu(hp).ops = 0) and
                          ((taicpu(hp).opcode = A_REP) or
                           (taicpu(hp).opcode = A_LOCK) or
                           (taicpu(hp).opcode =  A_REPE) or
                           (taicpu(hp).opcode =  A_REPNZ) or
                           (taicpu(hp).opcode =  A_REPZ) or
                           (taicpu(hp).opcode = A_REPNE)) then
                        Begin
                          prefix:=std_op2str[taicpu(hp).opcode]+#9;
                          hp:=tai(hp.next);
                        { this is theorically impossible... }
                          if hp=nil then
                           begin
                             AsmWriteLn(#9#9+prefix);
                             break;
                           end;
                          { nasm prefers prefix on a line alone
                          AsmWriteln(#9#9+prefix); but not masm PM
                          prefix:=''; }
                          if (aktoutputformat = as_i386_masm) then
                             begin
                               AsmWriteln(s);
                               prefix:='';
                             end;
                        end
                       else
                        prefix:= '';
                       if (aktoutputformat = as_i386_wasm) and
                        (taicpu(hp).opsize=S_W) and
                        (taicpu(hp).opcode=A_PUSH) and
                        (taicpu(hp).oper[0].typ=top_const) then
                        begin
                          AsmWriteln(#9#9'DB 66h,68h ; pushw imm16');
                          AsmWrite(#9#9'DW');
                        end
                       else  
                         AsmWrite(#9#9+prefix+std_op2str[taicpu(hp).opcode]+cond2str[taicpu(hp).condition]+suffix);
                       if taicpu(hp).ops<>0 then
                        begin
                          if is_calljmp(taicpu(hp).opcode) then
                           begin
                             AsmWrite(#9);
                             WriteOper_jmp(taicpu(hp).oper[0],taicpu(hp).opsize);
                           end
                          else
                           begin
                             for i:=0to taicpu(hp).ops-1 do
                              begin
                                if i=0 then
                                 AsmWrite(#9)
                                else
                                 AsmWrite(',');
                                WriteOper(taicpu(hp).oper[i],taicpu(hp).opsize,taicpu(hp).opcode,(i=2));
                              end;
                           end;
                        end;
                       AsmLn;
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
                          if LasTSec<>sec_none then
                           AsmWriteLn('_'+target_asm.secnames[LasTSec]+#9#9'ENDS');
                          AsmLn;
                          AsmWriteLn(#9'END');
                          AsmClose;
                          DoAssemble;
                          AsmCreate(tai_cut(hp).place);
                        end;
                     { avoid empty files }
                       while assigned(hp.next) and (tai(hp.next).typ in [ait_cut,ait_section,ait_comment]) do
                        begin
                          if tai(hp.next).typ=ait_section then
                           begin
                             lasTSec:=tai_section(hp.next).sec;
                           end;
                          hp:=tai(hp.next);
                        end;
                       AsmWriteLn(#9'.386p');
                       AsmWriteLn('DGROUP'#9'GROUP'#9'_BSS,_DATA');
                       AsmWriteLn(#9'ASSUME'#9'CS:_CODE,ES:DGROUP,DS:DGROUP,SS:DGROUP');
                       { I was told that this isn't necesarry because }
                       { the labels generated by FPC are unique (FK)  }
                       { AsmWriteLn(#9'LOCALS '+target_asm.labelprefix); }
                       if lasTSec<>sec_none then
                          AsmWriteLn('_'+target_asm.secnames[lasTSec]+#9#9+
                                     'SEGMENT'#9'PARA PUBLIC USE32 '''+
                                     target_asm.secnames[lasTSec]+'''');
                       AsmStartSize:=AsmSize;
                     end;
           ait_marker :
             begin
               if tai_marker(hp).kind=InlineStart then
                 inc(InlineLevel)
               else if tai_marker(hp).kind=InlineEnd then
                 dec(InlineLevel);
             end;
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
          begin
            if (aktoutputformat in [as_i386_masm,as_i386_wasm]) then
              currentasmlist.AsmWriteln(#9'EXTRN'#9+p.name
                +': NEAR')
            else
              currentasmlist.AsmWriteln(#9'EXTRN'#9+p.name);
          end;
      end;

    procedure T386IntelAssembler.WriteExternals;
      begin
        currentasmlist:=self;
        objectlibrary.symbolsearch.foreach_static({$ifdef fpcprocvar}@{$endif}writeexternal,nil);
      end;


    function t386intelassembler.DoAssemble : boolean;
    var f : file;
    begin
      DoAssemble:=Inherited DoAssemble;
      { masm does not seem to recognize specific extensions and uses .obj allways PM }
      if (aktoutputformat in [as_i386_masm,as_i386_wasm]) then
        begin
          if not(cs_asm_extern in aktglobalswitches) then
            begin
              if Not FileExists(objfile) and
                 FileExists(ForceExtension(objfile,'.obj')) then
                begin
                  Assign(F,ForceExtension(objfile,'.obj'));
                  Rename(F,objfile);
                end;
            end
          else
            AsmRes.AddAsmCommand('mv',ForceExtension(objfile,'.obj')+' '+objfile,objfile);
        end;
    end;


    procedure T386IntelAssembler.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Start writing intel-styled assembler output for '+current_module.mainsource^);
{$endif}
      LasTSec:=sec_none;
      AsmWriteLn(#9'.386p');
      { masm 6.11 does not seem to like LOCALS PM }
      if (aktoutputformat = as_i386_tasm) then
        begin
          AsmWriteLn(#9'LOCALS '+target_asm.labelprefix);
        end;
      AsmWriteLn('DGROUP'#9'GROUP'#9'_BSS,_DATA');
      AsmWriteLn(#9'ASSUME'#9'CS:_CODE,ES:DGROUP,DS:DGROUP,SS:DGROUP');
      AsmLn;

      WriteExternals;

    { INTEL ASM doesn't support stabs
      WriteTree(debuglist);}

      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(resourcestringlist);
      WriteTree(bsssegment);

      AsmWriteLn(#9'END');
      AsmLn;

{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Done writing intel-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
   end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_i386_tasm_info : tasminfo =
          (
            id           : as_i386_tasm;
            idtxt  : 'TASM';
            asmbin : 'tasm';
            asmcmd : '/m2 /ml $ASM $OBJ';
            supported_target : system_any; { what should I write here ?? }
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure : true;
            labelprefix : '@@';
            comment : '; ';
            secnames : ('',
              'CODE','DATA','BSS',
              '','','','','','',
              '','','')
          );

       as_i386_masm_info : tasminfo =
          (
            id           : as_i386_masm;
            idtxt  : 'MASM';
            asmbin : 'masm';
            asmcmd : '/c /Cp $ASM /Fo$OBJ';
            supported_target : system_any; { what should I write here ?? }
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '@@';
            comment : '; ';
            secnames : ('',
              'CODE','DATA','BSS',
              '','','','','','',
              '','','')
          );

       as_i386_wasm_info : tasminfo =
          (
            id     : as_i386_wasm;
            idtxt  : 'WASM';
            asmbin : 'wasm';
            asmcmd : '$ASM -5s -fpc -fp3 -ms -zq -Fo=$OBJ';
            supported_target : system_any; { what should I write here ?? }
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '@@';
            comment : '; ';
            secnames : ('',
              'CODE','DATA','BSS',
              '','','','','','',
              '','','')
          );

initialization
  RegisterAssembler(as_i386_tasm_info,T386IntelAssembler);
  RegisterAssembler(as_i386_masm_info,T386IntelAssembler);
  RegisterAssembler(as_i386_wasm_info,T386IntelAssembler);
end.
{
  $Log$
  Revision 1.40  2003-09-30 08:39:50  michael
  + Patch from Wiktor Sywula for watcom support

  Revision 1.39  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.38  2003/09/05 17:41:13  florian
    * merged Wiktor's Watcom patches in 1.1

  Revision 1.37  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.36.2.3  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.36.2.2  2003/08/27 21:07:03  peter
    * more updates

  Revision 1.36.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.36  2003/08/18 11:52:57  marco
   * fix for 2592, pushw imm

  Revision 1.35  2003/08/09 18:56:54  daniel
    * cs_regalloc renamed to cs_regvars to avoid confusion with register
      allocator
    * Some preventive changes to i386 spillinh code

  Revision 1.34  2003/03/08 08:59:07  daniel
    + $define newra will enable new register allocator
    + getregisterint will return imaginary registers with $newra
    + -sr switch added, will skip register allocation so you can see
      the direct output of the code generator before register allocation

  Revision 1.33  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.32  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.31  2002/12/24 18:10:34  peter
    * Long symbol names support

  Revision 1.30  2002/11/17 16:31:58  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.29  2002/11/15 01:58:56  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.28  2002/08/20 21:40:44  florian
    + target macos for ppc added
    + frame work for mpw assembler output

  Revision 1.27  2002/08/18 20:06:28  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.26  2002/08/12 15:08:41  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.25  2002/08/11 14:32:29  peter
    * renamed current_library to objectlibrary

  Revision 1.24  2002/08/11 13:24:16  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.23  2002/07/26 21:15:43  florian
    * rewrote the system handling

  Revision 1.22  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.21  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.20  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.18  2002/05/12 16:53:16  peter
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

  Revision 1.17  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.16  2002/04/04 19:06:07  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.15  2002/04/02 17:11:33  peter
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
