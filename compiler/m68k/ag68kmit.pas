{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an asmoutput class for MIT syntax with
    Motorola 68000 (for MIT syntax TEST WITH GAS v1.34)

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

  What's to do:
    o Verify if this actually work as indirect mode with name of variables
    o write lines numbers and file names to output file
    o generate debugging informations
}

unit ag68kmit;

    interface

    uses aasm,assemble;

    type
      pm68kmitasmlist=^tm68kmitasmlist;
      tm68kmitasmlist = object(tasmlist)
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

    var
{$ifdef GDB}
      n_line       : byte;     { different types of source lines }
      linecount,
      includecount : longint;
      funcname     : pchar;
      stabslastfileinfo : tfileposinfo;
{$endif}
      lastsec    : tsection; { last section type written }
      lastsecidx,
      lastfileindex,
      lastline   : longint;


    function double2str(d : double) : string;
      var
         hs : string;
      begin
         str(d,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
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
                  { symbol and offset }
                  if (assigned(symbol)) and (offset<>0) then
                    Begin
                      s:=s+'('+tostr(offset)+symbol^;
                    end
                  else
                  { symbol only }
                  if (assigned(symbol)) and (offset=0) then
                    Begin
                      s:=s+'('+symbol^;
                    end
                  else
                  { offset only }
                  if (symbol=nil) and (offset<>0) then
                    Begin
                      s:=s+'('+tostr(offset);
                    end
                  else
                  { NOTHING - put zero as offset }
                  if (symbol=nil) and (offset=0) then
                    Begin
                      s:=s+'('+'0';
                    end
                  else
                   InternalError(10004);
                  if (index<>R_NO) and (base=R_NO) and (direction=dir_none) then
                   InternalError(10004)
                else if (index=R_NO) and (base<>R_NO) and (direction=dir_inc) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                    Begin
                      if offset<>0 then
                        s:=mit_reg2str[base]+'@+'+s+')'
                      else
                        s:=mit_reg2str[base]+'@+';
                    end
                  else
                   InternalError(10002);
                end
                else if (index=R_NO) and (base<>R_NO) and (direction=dir_dec) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                    Begin
                      if offset<>0 then
                         s:=mit_reg2str[base]+'@-'+s+')'
                      else
                         s:=mit_reg2str[base]+'@-';
                    end
                  else
                   InternalError(10003);
                end
              else if (index=R_NO) and (base<>R_NO) and (direction=dir_none) then
                begin
                  if (offset=0) and (symbol=nil) then
                     s:=mit_reg2str[base]+'@'
                  else
                     s:=mit_reg2str[base]+'@'+s+')';
                end
              else if (index<>R_NO) and (base<>R_NO) and (direction=dir_none) then
                begin
                  s:=mit_reg2str[base]+'@'+s+','+mit_reg2str[index]+':L';
                  if (scalefactor = 1) or (scalefactor = 0) then
                      s:=s+')'
                  else
                     s:=s+':'+tostr(scalefactor)+')';
                end
                else
                if assigned(symbol) then
                Begin
                   s:=symbol^;
                   if offset<>0 then
                     s:=s+'+'+tostr(offset);
                end
                { this must be a physical address }
                else
                  s:=s+')';
{                else if NOT assigned(symbol) then
                  InternalError(10004);}
            end; { end with }
         getreferencestring:=s;
      end;


    function getopstr(t : byte;o : pointer) : string;
      var
         hs : string;
         i: tregister;
      begin
         case t of
            top_reg : getopstr:=mit_reg2str[tregister(o)];
               top_ref : getopstr:=getreferencestring(preference(o)^);
         top_reglist: begin
                      hs:='';
                      for i:=R_NO to R_FPSR do
                      begin
                        if i in tregisterlist(o^) then
                         hs:=hs+mit_reg2str[i]+'/';
                      end;
                      delete(hs,length(hs),1);
                      getopstr := hs;
                    end;
             top_const : getopstr:='#'+tostr(longint(o));
            top_symbol :
                    { compare with i386, where a symbol is considered }
                    { a constant.                                     }
                    begin
                     hs[0]:=chr(strlen(pchar(pcsymbol(o)^.symbol)));
                            move(pchar(pcsymbol(o)^.symbol)^,hs[1],byte(hs[0]));
{                           inc(byte(hs[0]));}
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
            top_reg : getopstr_jmp:=mit_reg2str[tregister(o)];
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
                             T68kGASASMOUTPUT
 ****************************************************************************}

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'.long'#9,#9'.short'#9,#9'.byte'#9);

      ait_section2str : array[tsection] of string[8]=
       ('','.text','.data','.bss',
        '.stab','.stabstr',
        '.idata2','.idata4','.idata5','.idata6','.idata7',
        '.edata','');

    procedure tm68kmitasmlist.WriteTree(p:paasmoutput);
    var
      hp        : pai;
      ch        : char;
      consttyp  : tait;
      s         : string;
      pos,l,i   : longint;
      found     : boolean;
{$ifdef GDB}
      curr_n    : byte;
      infile    : pinputfile;
      funcname  : pchar;
      linecount : longint;
{$endif GDB}
    begin
      if not assigned(p) then
       exit;
{$ifdef GDB}
      funcname:=nil;
      linecount:=1;
{$endif GDB}
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
       { write debugger informations }
{$ifdef GDB}
         if ((cs_debuginfo in aktmoduleswitches) or
            (cs_gdb_lineinfo in aktglobalswitches)) then
          begin
            if not (hp^.typ in  [ait_external,ait_regalloc, ait_regdealloc,ait_stabn,ait_stabs,
                    ait_label,ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
             begin
             { file changed ? (must be before line info) }
               if lastfileindex<>hp^.fileinfo.fileindex then
                begin
                  infile:=current_module^.sourcefiles^.get_file(hp^.fileinfo.fileindex);
                  if includecount=0 then
                   curr_n:=n_sourcefile
                  else
                   curr_n:=n_includefile;
                  if (infile^.path^<>'') then
                   begin
                     AsmWriteLn(#9'.stabs "'+lower(BsToSlash(FixPath(infile^.path^,false)))+'",'+
                       tostr(curr_n)+',0,0,'+'Ltext'+ToStr(IncludeCount));
                   end;
                  AsmWriteLn(#9'.stabs "'+lower(FixFileName(infile^.name^))+'",'+
                    tostr(curr_n)+',0,0,'+'Ltext'+ToStr(IncludeCount));
                  AsmWriteLn('Ltext'+ToStr(IncludeCount)+':');
                  inc(includecount);
                  lastfileindex:=hp^.fileinfo.fileindex;
                end;
             { line changed ? }
               if (hp^.fileinfo.line<>lastline) and (hp^.fileinfo.line<>0) then
                begin
                  if (n_line=n_textline) and assigned(funcname) and
                     (target_os.use_function_relative_addresses) then
                   begin
                     AsmWriteLn(target_asm.labelprefix+'l'+tostr(linecount)+':');
                     AsmWrite(#9'.stabn '+tostr(n_line)+',0,'+tostr(hp^.fileinfo.line)+','+
                                target_asm.labelprefix+'l'+tostr(linecount)+' - ');
                     AsmWritePChar(FuncName);
                     AsmLn;
                     inc(linecount);
                   end
                  else
                   AsmWriteLn(#9'.stabd'#9+tostr(n_line)+',0,'+tostr(hp^.fileinfo.line));
                  lastline:=hp^.fileinfo.line;
                end;
             end;
          end;
{$endif GDB}
         case hp^.typ of
      ait_external : ; { external is ignored }
       ait_comment : Begin
                       AsmWrite(target_asm.comment);
                       AsmWritePChar(pai_asm_comment(hp)^.str);
                       AsmLn;
                     End;
{$ifdef DREGALLOC}
      ait_regalloc : AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+' allocated');
    ait_regdealloc : AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+' released');
{$endif DREGALLOC}
         ait_align : AsmWriteLn(#9'.align '+tostr(pai_align(hp)^.aligntype));
       ait_section : begin
                       if pai_section(hp)^.sec<>sec_none then
                        begin
                          AsmLn;
                          AsmWrite(ait_section2str[pai_section(hp)^.sec]);
                          if pai_section(hp)^.idataidx>0 then
                           AsmWrite('$'+tostr(pai_section(hp)^.idataidx));
                          AsmLn;
{$ifdef GDB}
                          case pai_section(hp)^.sec of
                           sec_code : n_line:=n_textline;
                           sec_data : n_line:=n_dataline;
                            sec_bss : n_line:=n_bssline;
                          end;
{$endif GDB}
                        end;
                       LastSec:=pai_section(hp)^.sec;
                     end;
     ait_datablock : begin
                       { ------------------------------------------------------- }
                       { ----------- ALIGNMENT FOR ANY NON-BYTE VALUE ---------- }
                       { ------------- REQUIREMENT FOR 680x0 ------------------- }
                       { ------------------------------------------------------- }
                       if pai_datablock(hp)^.size <> 1 then
                        begin
                          if not(cs_littlesize in aktglobalswitches) then
                           AsmWriteLn(#9#9'.align 4')
                          else
                           AsmWriteLn(#9#9'.align 2');
                        end;
                       if pai_datablock(hp)^.is_global then
                        AsmWrite(#9'.comm'#9)
                       else
                        AsmWrite(#9'.lcomm'#9);
                       AsmWriteLn(StrPas(pai_datablock(hp)^.name)+','+tostr(pai_datablock(hp)^.size));
                     end;
   ait_const_32bit, { alignment is required for 16/32 bit data! }
   ait_const_16bit:  begin
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
    ait_const_8bit : begin
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
  ait_const_symbol : Begin
                       AsmWriteLn(#9'.long'#9+StrPas(pchar(pai_const(hp)^.value)));
                     end;
  ait_const_symbol_offset :
                     Begin
                       AsmWrite(#9'.long'#9);
                       AsmWritePChar(pai_const_symbol_offset(hp)^.name);
                       if pai_const_symbol_offset(hp)^.offset>0 then
                         AsmWrite('+'+tostr(pai_const_symbol_offset(hp)^.offset))
                       else if pai_const_symbol_offset(hp)^.offset<0 then
                         AsmWrite(tostr(pai_const_symbol_offset(hp)^.offset));
                       AsmLn;
                     end;
    ait_real_64bit : Begin
                       AsmWriteLn(#9'.double'#9+double2str(pai_double(hp)^.value));
                     end;
    ait_real_32bit : Begin
                       AsmWriteLn(#9'.single'#9+double2str(pai_single(hp)^.value));
                     end;
 ait_real_extended : Begin
                       AsmWriteLn(#9'.extend'#9+double2str(pai_extended(hp)^.value));
                     { comp type is difficult to write so use double }
                     end;
{ TO SUPPORT SOONER OR LATER!!!
          ait_comp : Begin
                       AsmWriteLn(#9'.double'#9+comp2str(pai_extended(hp)^.value));
                     end; }
        ait_direct : begin
                       AsmWritePChar(pai_direct(hp)^.str);
                       AsmLn;
{$IfDef GDB}
                       if strpos(pai_direct(hp)^.str,'.data')<>nil then
                         n_line:=n_dataline
                       else if strpos(pai_direct(hp)^.str,'.text')<>nil then
                         n_line:=n_textline
                       else if strpos(pai_direct(hp)^.str,'.bss')<>nil then
                         n_line:=n_bssline;
{$endif GDB}
                     end;
        ait_string : begin
                       pos:=0;
                       for i:=1 to pai_string(hp)^.len do
                        begin
                          if pos=0 then
                           begin
                             AsmWrite(#9'.ascii'#9'"');
                             pos:=20;
                           end;
                          ch:=pai_string(hp)^.str[i-1];
                          case ch of
                             #0, {This can't be done by range, because a bug in FPC}
                        #1..#31,
                     #128..#255 : s:='\'+tostr(ord(ch) shr 6)+tostr((ord(ch) and 63) shr 3)+tostr(ord(ch) and 7);
                            '"' : s:='\"';
                            '\' : s:='\\';
                          else
                           s:=ch;
                          end;
                          AsmWrite(s);
                          inc(pos,length(s));
                          if (pos>line_length) or (i=pai_string(hp)^.len) then
                           begin
                             AsmWriteLn('"');
                             pos:=0;
                           end;
                        end;
                     end;
         ait_label : begin
                       if assigned(hp^.next) and (pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_const_symbol_offset,
                           ait_real_64bit,ait_real_32bit,ait_string]) then
                        begin
                          if not(cs_littlesize in aktglobalswitches) then
                           AsmWriteLn(#9#9'.align 4')
                          else
                           AsmWriteLn(#9#9'.align 2');
                        end;
                       if (pai_label(hp)^.l^.is_used) then
                        AsmWriteLn(lab2str(pai_label(hp)^.l)+':');
                     end;
ait_labeled_instruction : begin
                     { labeled operand }
                       if pai_labeled(hp)^._op1 = R_NO then
                        AsmWriteLn(#9+mot_op2str[pai_labeled(hp)^._operator]+#9+lab2str(pai_labeled(hp)^.lab))
                       else
                     { labeled operand with register }
                        AsmWriteLn(#9+mot_op2str[pai_labeled(hp)^._operator]+#9+
                                 mit_reg2str[pai_labeled(hp)^._op1]+','+lab2str(pai_labeled(hp)^.lab))
                     end;
        ait_symbol : begin
                       { ------------------------------------------------------- }
                       { ----------- ALIGNMENT FOR ANY NON-BYTE VALUE ---------- }
                       { ------------- REQUIREMENT FOR 680x0 ------------------- }
                       { ------------------------------------------------------- }
                       if assigned(hp^.next) and (pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_const_symbol_offset,
                           ait_real_64bit,ait_real_32bit,ait_string]) then
                        begin
                          if not(cs_littlesize in aktglobalswitches) then
                           AsmWriteLn(#9#9'.align 4')
                          else
                           AsmWriteLn(#9#9'.align 2');
                        end;
                       if pai_symbol(hp)^.is_global then
                        AsmWriteLn('.globl '+StrPas(pai_symbol(hp)^.name));
                       AsmWriteLn(StrPas(pai_symbol(hp)^.name)+':');
                     end;
   ait_instruction : begin
                       { old versions of GAS don't like PEA.L and LEA.L }
                       if (pai68k(hp)^._operator in [
                            A_LEA,A_PEA,A_ABCD,A_BCHG,A_BCLR,A_BSET,A_BTST,
                            A_EXG,A_NBCD,A_SBCD,A_SWAP,A_TAS,A_SCC,A_SCS,
                            A_SEQ,A_SGE,A_SGT,A_SHI,A_SLE,A_SLS,A_SLT,A_SMI,
                            A_SNE,A_SPL,A_ST,A_SVC,A_SVS,A_SF]) then
                        s:=#9+mot_op2str[pai68k(hp)^._operator]
                       else
                        s:=#9+mot_op2str[pai68k(hp)^._operator]+mit_opsize2str[pai68k(hp)^.size];
                       if pai68k(hp)^.op1t<>top_none then
                        begin
                        { call and jmp need an extra handling                          }
                        { this code is only callded if jmp isn't a labeled instruction }
                          if pai68k(hp)^._operator in [A_JSR,A_JMP] then
                           s:=s+#9+getopstr_jmp(pai68k(hp)^.op1t,pai68k(hp)^.op1)
                          else
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
                       AsmWriteLn(s);
                     end;
{$ifdef GDB}
         ait_stabs : begin
                       AsmWrite(#9'.stabs ');
                       AsmWritePChar(pai_stabs(hp)^.str);
                       AsmLn;
                     end;
         ait_stabn : begin
                       AsmWrite(#9'.stabn ');
                       AsmWritePChar(pai_stabn(hp)^.str);
                       AsmLn;
                     end;
    ait_force_line : begin
                        stabslastfileinfo.line:=0;
                     end;
ait_stab_function_name : funcname:=pai_stab_function_name(hp)^.str;
{$endif GDB}
           ait_cut : begin
                     { create only a new file when the last is not empty }
                       if AsmSize>0 then
                        begin
                          AsmClose;
                          DoAssemble;
                          AsmCreate;
                        end;
                     { avoid empty files }
                       while assigned(hp^.next) and (pai(hp^.next)^.typ in [ait_cut,ait_section,ait_comment]) do
                        begin
                          if pai(hp^.next)^.typ=ait_section then
                           begin
                             lastsec:=pai_section(hp^.next)^.sec;
                             lastsecidx:=pai_section(hp^.next)^.idataidx;
                           end;
                          hp:=pai(hp^.next);
                        end;
                       if lastsec<>sec_none then
                         AsmWriteLn(ait_section2str[lastsec,lastsecidx]);
                     end;
        ait_marker : ;
         else
          internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;

    procedure tm68kmitasmlist.WriteAsmList;
    var
      p:dirstr;
      n:namestr;
      e:extstr;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Start writing gas-styled assembler output for '+current_module^.mainsource^);
{$endif}

      lastline:=0;
      lastfileindex:=0;
      LastSec:=sec_none;
{$ifdef GDB}
      includecount:=0;
      n_line:=n_bssline;
{$endif GDB}

      if assigned(current_module^.mainsource) then
       fsplit(current_module^.mainsource^,p,n,e)
      else
       begin
         p:=inputdir;
         n:=inputfile;
         e:=inputextension;
       end;
    { to get symify to work }
      AsmWriteLn(#9'.file "'+FixFileName(n+e)+'"');

      countlabelref:=false;
      { there should be nothing but externals so we don't need to process
      WriteTree(externals); }

      If (cs_debuginfo in aktmoduleswitches) then
        WriteTree(debuglist);
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
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing gas-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
    end;

end.
{
  $Log$
  Revision 1.1  2000-11-30 20:30:34  peter
    * moved into m68k subdir

  Revision 1.2  2000/07/13 11:32:31  michael
  + removed logs

}
