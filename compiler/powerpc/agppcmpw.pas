{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit implements an asmoutput class for PowerPC with MPW syntax

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
  This unit implements an asmoutput class for PowerPC with MPW syntax
}
unit agppcmpw;

{$i fpcdefs.inc}

interface

    uses
       aasmbase,aasmtai,aasmcpu,assemble,
       cpubase;

    const
      mpw_reg2str : treg2strtable = ('',
        'r0','r1','r2','r3','r4','r5','r6','r7','r8','r9','r10','r11','r12','r13','r14','r15','r16',
        'r17','r18','r19','r20','r21','r22','r23','r24','r25','r26','r27','r28','r29','r30','r31',
        'f0','f1','f2','f3','f4','f5','f6','f7', 'f8','f9','f10','f11','f12',
        'f13','f14','f15','f16','f17', 'f18','f19','f20','f21','f22', 'f23','f24',
        'f25','f26','f27','f28','f29','f30','f31',
        'v0','v1','v2','v3','v4','v5','v6','v7','v8','v9','v10','v11','v12',
        'v13','v14','v15','v16','v17','v18','v19','v20','v21','v22', 'v23','v24',
        'v25','v26','v27','v28','v29','v30','v31',
        'cR','cr0','cr1','cr2','cr3','cr4','cr5','cr6','cr7',
        'xer','lr','ctr','fpscr'
      );
    type
      TPPCMPWAssembler = class(TExternalAssembler)
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
      verbose,finput,fmodule,script,cpuinfo
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

    function getreferencestring(var ref : treference) : string;
    var
      s     : string;
      first : boolean;
    begin
       getreferencestring:=s;
     end;


    function getopstr(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg:
          getopstr:=mpw_reg2str[o.reg];
        { no top_ref jumping for powerpc }
        top_const:
          getopstr:=tostr(longint(o.val));
        top_ref:
          getopstr:=getreferencestring(o.ref^);
        top_symbol:
          begin
            hs:=o.sym.name;
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs);
            getopstr:=hs;
          end;
        else
{$ifndef testing}
          internalerror(2002070604);
{$else testing}
          begin
            writeln('internalerror 10001');
            halt(1);
          end;
{$endif testing}
      end;
    end;


    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr_jmp:=mpw_reg2str[o.reg];
        { no top_ref jumping for powerpc }
        top_const :
          getopstr_jmp:=tostr(o.val);
        top_symbol :
          begin
            hs:=o.sym.name;
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs);
            getopstr_jmp:=hs;
          end;
        else
{$ifndef testing}
          internalerror(2002070603);
{$else testing}
          begin
            writeln('internalerror 10001');
            halt(1);
          end;
{$endif testing}
      end;
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

    procedure TPPCMPWAssembler.WriteTree(p:TAAsmoutput);
    const
      nolinetai =[ait_label,
                  ait_regalloc,ait_tempalloc,
{$ifdef GDB}
                  ait_stabn,ait_stabs,ait_stab_function_name,
{$endif GDB}
                  ait_cut,ait_marker,ait_align,ait_section];
    var
      s,
      prefix,
      suffix   : string;
      hp       : tai;
      counter,
      lines,
      InlineLevel : longint;
      i,j,l    : longint;
      consttyp : taitype;
      found,
      do_line,DoNotSplitLine,
      quoted   : boolean;
      sep      : char;
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
         if do_line and not(hp.typ in nolinetai) and
            not DoNotSplitLine then
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
         DoNotSplitLine:=false;
         case hp.typ of
            ait_comment:
              begin
                 AsmWrite(target_asm.comment);
                 AsmWritePChar(tai_comment(hp).str);
                 AsmLn;
              end;
            ait_regalloc,
            ait_tempalloc:
              ;
            ait_section:
              begin
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
             ait_align:
               begin
                  AsmWriteLn(#9'ALIGN '+tostr(tai_align(hp).aligntype));
               end;
             ait_datablock:
               begin
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
{!!!!
                     { Must be done with args in ATT order }
                       taicpu(hp).SetOperandOrder(op_att);
                       taicpu(hp).CheckNonCommutativeOpcodes;
                     { We need intel order, no At&t }
                       taicpu(hp).SetOperandOrder(op_intel);
                     { Reset }
                       suffix:='';
                       prefix:= '';
                       s:='';
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
                             s:=#9#9+prefix;
                             AsmWriteLn(s);
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
                       if taicpu(hp).ops<>0 then
                        begin
                          if is_calljmp(taicpu(hp).opcode) then
                           s:=#9+getopstr_jmp(taicpu(hp).oper[0],taicpu(hp).opsize)
                          else
                           begin
                             for i:=0to taicpu(hp).ops-1 do
                              begin
                                if i=0 then
                                 sep:=#9
                                else
                                 sep:=',';
                                s:=s+sep+getopstr(taicpu(hp).oper[i],taicpu(hp).opsize,taicpu(hp).opcode,(i=2));
                              end;
                           end;
                        end;
                       AsmWriteLn(#9#9+prefix+std_op2str[taicpu(hp).opcode]+cond2str[taicpu(hp).condition]+suffix+s);
}
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
           currentasmlist.AsmWriteln(#9'EXTRN'#9+p.name);
      end;

    procedure TPPCMPWAssembler.WriteExternals;
      begin
        currentasmlist:=self;
        objectlibrary.symbolsearch.foreach_static({$ifdef fpcprocvar}@{$endif}writeexternal,nil);
      end;


    function TPPCMPWAssembler.DoAssemble : boolean;
    var f : file;
    begin
      DoAssemble:=Inherited DoAssemble;
      { masm does not seem to recognize specific extensions and uses .obj allways PM }
      if (aktoutputformat = as_i386_masm) then
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


    procedure TPPCMPWAssembler.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Start writingMPW-styled assembler output for '+current_module.mainsource^);
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
       comment(v_info,'Done writing MPW-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
   end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_powerpc_mpw_info : tasminfo =
          (
            id           : as_powerpc_mpw;
            idtxt  : 'MPW';
            asmbin : 'ppcasm';
            asmcmd : '';
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

initialization
  RegisterAssembler(as_powerpc_mpw_info,TPPCMPWAssembler);
end.
{
  $Log$
  Revision 1.2  2002-08-31 12:43:31  florian
    * ppc compilation fixed

  Revision 1.1  2002/08/20 21:40:44  florian
    + target macos for ppc added
    + frame work for mpw assembler output
}
