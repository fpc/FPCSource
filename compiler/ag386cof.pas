{
    $Id$
    Copyright (c) 1996-98 by the FPC development team

    This unit implements an asmoutput class for i386 coff

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
unit ag386cof;

    interface

    uses cobjects,aasm,assemble;

    type
      pi386coffasmlist=^ti386coffasmlist;
      ti386coffasmlist=object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
{$ifdef GDB}
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
{$endif}
      end;

  implementation

    uses
      dos,globtype,globals,systems,i386,
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
      lastsecidx : longint;
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : pinputfile;


    function double2str(d : double) : string;
      var
         hs : string;
      begin
         str(d,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         double2str:='0d'+hs
      end;

    function extended2str(e : extended) : string;
      var
         hs : string;
      begin
         str(e,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         extended2str:='0d'+hs
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
      s : string;
    begin
      if ref.isintvalue then
       s:='$'+tostr(ref.offset)
      else
       begin
         with ref do
          begin
          { have we a segment prefix ? }
          { These are probably not correctly handled under GAS }
          { should be replaced by coding the segment override  }
          { directly! - DJGPP FAQ                              }
            if segment<>R_DEFAULT_SEG then
             s:=att_reg2str[segment]+':'
            else
             s:='';
            if assigned(symbol) then
             s:=s+symbol^;
            if offset<0 then
             s:=s+tostr(offset)
            else
             if (offset>0) then
              begin
                if assigned(symbol) then
                 s:=s+'+'+tostr(offset)
                else
                 s:=s+tostr(offset);
              end;
            if (index<>R_NO) and (base=R_NO) then
             Begin
               s:=s+'(,'+att_reg2str[index];
               if scalefactor<>0 then
                s:=s+','+tostr(scalefactor)+')'
               else
                s:=s+')';
             end
            else
             if (index=R_NO) and (base<>R_NO) then
              s:=s+'('+att_reg2str[base]+')'
             else
              if (index<>R_NO) and (base<>R_NO) then
               Begin
                 s:=s+'('+att_reg2str[base]+','+att_reg2str[index];
                 if scalefactor<>0 then
                  s:=s+','+tostr(scalefactor)+')'
                 else
                  s := s+')';
               end;
          end;
       end;
      getreferencestring:=s;
    end;

    function getopstr(t : byte;o : pointer) : string;
    var
      hs : string;
    begin
      case t of
        top_reg : getopstr:=att_reg2str[tregister(o)];
        top_ref : getopstr:=getreferencestring(preference(o)^);
      top_const : getopstr:='$'+tostr(longint(o));
     top_symbol : begin
                    hs:='$'+strpas(pchar(pcsymbol(o)^.symbol));
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
       top_reg : getopstr_jmp:=att_reg2str[tregister(o)];
       top_ref : getopstr_jmp:='*'+getreferencestring(preference(o)^);
     top_const : getopstr_jmp:=tostr(longint(o));
    top_symbol : begin
                    hs:=strpas(pchar(pcsymbol(o)^.symbol));
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
                            TI386ATTASMOUTPUT
 ****************************************************************************}

    const
      ait_const2str : array[ait_const_32bit..ait_const_8bit] of string[8]=
       (#9'.long'#9,#9'.short'#9,#9'.byte'#9);

    function ait_section2str(s:tsection;idx:longint):string;
    begin
      case s of
        sec_code : ait_section2str:='.text';
        sec_data : ait_section2str:='.data';
         sec_bss : if target_info.target=target_i386_Win32 then
                    ait_section2str:='.section .bss'
                   else
                    ait_section2str:='.bss';
       sec_idata : ait_section2str:='.section .idata$'+tostr(idx);
       sec_edata : ait_section2str:='.section .edata';
      else
       ait_section2str:='';
      end;
{$ifdef GDB}
      { this is needed for line info in data }
      funcname:=nil;
      case s of
       sec_code : n_line:=n_textline;
       sec_data : n_line:=n_dataline;
        sec_bss : n_line:=n_bssline;
      else
       n_line:=n_dataline;
      end;
{$endif GDB}
      LastSec:=s;
      LastSecIdx:=idx;
    end;


{$ifdef GDB}
      procedure ti386attasmlist.WriteFileLineInfo(var fileinfo : tfileposinfo);
        var
          curr_n : byte;
        begin
          if not (cs_debuginfo in aktmoduleswitches) then
           exit;
        { file changed ? (must be before line info) }
          if (fileinfo.fileindex<>0) and
             (stabslastfileinfo.fileindex<>fileinfo.fileindex) then
           begin
             infile:=current_module^.sourcefiles^.get_file(fileinfo.fileindex);
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
           end;
        { line changed ? }
          if (stabslastfileinfo.line<>fileinfo.line) and (fileinfo.line<>0) then
           begin
             if (n_line=n_textline) and assigned(funcname) and
                (target_os.use_function_relative_addresses) then
              begin
                AsmWriteLn(target_asm.labelprefix+'l'+tostr(linecount)+':');
                AsmWrite(#9'.stabn '+tostr(n_line)+',0,'+tostr(fileinfo.line)+','+
                           target_asm.labelprefix+'l'+tostr(linecount)+' - ');
                AsmWritePChar(FuncName);
                AsmLn;
                inc(linecount);
              end
             else
              AsmWriteLn(#9'.stabd'#9+tostr(n_line)+',0,'+tostr(fileinfo.line));
           end;
          stabslastfileinfo:=fileinfo;
        end;
{$endif GDB}

    function getops(p:pai386;var optyp1,optyp2,optyp3:longint):longint;

      function doop(ot:longint;op:pointer):longint;
      begin
        case ot of
          top_reg :
            begin
              doop:=reg_2_type[tregister(op)];
            end;
        else
          internalerror(191918);
        end;
      end;

    var
      ops,opx : longint;
    begin
      ops:=0;
      optyp1:=0;
      optyp2:=0;
      optyp3:=0;
      with p^ do
       begin
         if opxt=0 then
          exit;
         optyp1:=doop(opx and $f,op1);
         optyp2:=doop((opx shr 4) and $f,op2);
       end;
      getops:=ops;
    end;


    procedure ti386coffasmlist.WriteTree(p:paasmoutput);
    type
      twowords=record
        word1,word2:word;
      end;
      textendedarray = array[0..9] of byte; { last longint will be and $ffff }
    var
      ch       : char;
      hp       : pai;
      consttyp : tait;
      s        : string;
      found    : boolean;
      i,pos,l  : longint;
      e        : extended;
      calljmp,
      do_line  : boolean;

      instruc : tasmop;
      insops  : longint;
      fits    : boolean;
      optyp1,
      optyp2,
      optyp3  : longint;

    begin
      if not assigned(p) then
       exit;
      do_line:=(cs_debuginfo in aktmoduleswitches) or (cs_asm_source in aktglobalswitches);
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         if do_line then
          begin
          { I think it is better to write stabs before source line PM }
{$ifdef GDB}
          { write stabs }
            if cs_debuginfo in aktmoduleswitches then
             begin
               if not (hp^.typ in  [ait_external,ait_regalloc, ait_regdealloc,ait_stabn,ait_stabs,
                      ait_label,ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
                 begin
                    WriteFileLineInfo(hp^.fileinfo);
                 end;
             end;
{$endif GDB}
          { load infile }
            if lastfileinfo.fileindex<>hp^.fileinfo.fileindex then
             begin
               infile:=current_module^.sourcefiles^.get_file(hp^.fileinfo.fileindex);
               { open only if needed !! }
               if (cs_asm_source in aktglobalswitches) then
                 infile^.open;
               { avoid unnecessary reopens of the same file !! }
               lastfileinfo.fileindex:=hp^.fileinfo.fileindex;
               { be sure to change line !! }
               lastfileinfo.line:=-1;
             end;
          { write source }
            if (cs_asm_source in aktglobalswitches) and
                not (hp^.typ in  [ait_external,ait_stabn,ait_stabs,ait_section,
                      ait_label,ait_cut,ait_align,ait_stab_function_name]) then
             begin
               if (infile<>lastinfile) and assigned(lastinfile) then
                 begin
                   AsmWriteLn(target_asm.comment+'['+infile^.name^+']');
                   lastinfile^.close;
                 end;
               if (hp^.fileinfo.line<>lastfileinfo.line) and
                  (hp^.fileinfo.line<infile^.maxlinebuf) then
                 begin
                   if infile^.linebuf^[hp^.fileinfo.line]>=0 then
                     AsmWriteLn(target_asm.comment+'['+tostr(hp^.fileinfo.line)+'] '+
                       trimspace(infile^.GetLineStr(hp^.fileinfo.line)));
                   { set it to a negative value !
                   to make that is has been read already !! PM }
                   infile^.linebuf^[hp^.fileinfo.line]:=-infile^.linebuf^[hp^.fileinfo.line]-1;
                end;
               lastfileinfo:=hp^.fileinfo;
               lastinfile:=infile;
             end;
          end;

         case hp^.typ of
      ait_external : ; { external is ignored }
       ait_comment : Begin
                       AsmWrite(target_asm.comment);
                       AsmWritePChar(pai_asm_comment(hp)^.str);
                       AsmLn;
                     End;
{$ifdef DRegAlloc}
    ait_regalloc : AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+' allocated');
    ait_regdealloc : AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+' released');
{$Else DRegAlloc}
    ait_regalloc, ait_regdealloc:;
{$endif DRegAlloc}
         ait_align : begin
                        { Fix Align bytes for Go32 which uses empty bits }
                        l:=pai_align(hp)^.aligntype;
                        if (target_info.target in [target_i386_GO32V1,target_i386_GO32V2]) then
                         begin
                           i:=0;
                           while l>1 do
                            begin
                              l:=l shr 1;
                              inc(i);
                            end;
                           l:=i;
                         end;
                        { use correct align opcode }
                        AsmWrite(#9'.align '+tostr(l));
                        if pai_align(hp)^.use_op then
                         AsmWrite(','+tostr(pai_align(hp)^.op));
                        AsmLn;
                     end;
       ait_section : begin
                       if pai_section(hp)^.sec<>sec_none then
                        begin
                          AsmLn;
                          AsmWriteLn(ait_section2str(pai_section(hp)^.sec,pai_section(hp)^.idataidx));
                        end;
                     end;
     ait_datablock : begin
                       if pai_datablock(hp)^.is_global then
                        AsmWrite(#9'.comm'#9)
                       else
                        AsmWrite(#9'.lcomm'#9);
                       AsmWritePChar(pai_datablock(hp)^.name);
                       AsmWriteLn(','+tostr(pai_datablock(hp)^.size));
                     end;
   ait_const_32bit,
   ait_const_16bit,
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
  ait_const_symbol : begin
                       AsmWrite(#9'.long'#9);
                       AsmWritePChar(pchar(pai_const(hp)^.value));
                       AsmLn;
                     end;
  ait_const_symbol_offset : begin
                       AsmWrite(#9'.long'#9);
                       AsmWritePChar(pai_const_symbol_offset(hp)^.name);
                       if pai_const_symbol_offset(hp)^.offset>0 then
                         AsmWrite('+'+tostr(pai_const_symbol_offset(hp)^.offset))
                       else if pai_const_symbol_offset(hp)^.offset<0 then
                         AsmWrite(tostr(pai_const_symbol_offset(hp)^.offset));
                       AsmLn;
                     end;
     ait_const_rva : begin
                       AsmWrite(#9'.rva'#9);
                       AsmWritePChar(pchar(pai_const(hp)^.value));
                       AsmLn;
                     end;
    ait_real_64bit : AsmWriteLn(#9'.double'#9+double2str(pai_double(hp)^.value));
    ait_real_32bit : AsmWriteLn(#9'.single'#9+double2str(pai_single(hp)^.value));
 ait_real_extended : begin
{$ifdef EXTDEBUG}
                       AsmWriteLn('# workaround for Extended '+extended2str(pai_extended(hp)^.value));
{$endif}
                     { Make sure e is a extended type, bestreal could be
                       a different type (bestreal) !! (PFV) }
                       e:=pai_extended(hp)^.value;
                       AsmWrite(#9'.byte'#9);
                       for i:=0 to 9 do
                        begin
                          if i<>0 then
                           AsmWrite(',');
                          AsmWrite(tostr(textendedarray(e)[i]));
                        end;
                       AsmLn;
                     end;
          ait_comp : begin
                     { comp type is difficult to write so use double }
                       AsmWriteLn(#9'.double'#9+comp2str(pai_comp(hp)^.value));
                     end;
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
                       if (pai_label(hp)^.l^.is_used) then
                        begin
                          if pai_label(hp)^.l^.is_data and (cs_smartlink in aktmoduleswitches) then
                            AsmWriteLn('.globl'#9+lab2str(pai_label(hp)^.l));
                          AsmWriteLn(lab2str(pai_label(hp)^.l)+':');
                        end;
                     end;
 ait_labeled_instruction : begin
                       AsmWriteLn(#9+att_op2str[pai_labeled(hp)^._operator]+#9+lab2str(pai_labeled(hp)^.lab));
                     end;
        ait_symbol : begin
                       if pai_symbol(hp)^.is_global then
                        begin
                          AsmWrite('.globl'#9);
                          AsmWritePChar(pai_symbol(hp)^.name);
                          AsmLn;
                        end;
                       if target_info.target=target_i386_linux then
                        begin
                           AsmWrite(#9'.type'#9);
                           AsmWritePChar(pai_symbol(hp)^.name);
                           if assigned(pai(hp^.next)) and
                              (pai(hp^.next)^.typ in [ait_const_symbol,ait_const_symbol_offset,
                                 ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_datablock,
                                 ait_real_64bit,ait_real_32bit,ait_real_extended,ait_comp]) then
                            AsmWriteLn(',@object')
                           else
                            AsmWriteLn(',@function');
                        end;
                       AsmWritePChar(pai_symbol(hp)^.name);
                       AsmWriteLn(':');
                     end;
   ait_instruction :
     begin { writes an instruction, highly table driven }
       { get local info }
       instruc:=pai386(hp)^._operator;
       insops:=getops(pai386(hp),optyp1,optyp2,optyp3);
       { get the correct instruction from the it table }
       if itcache^[instruc]<>-1 then
         i:=itcache^[instruc]
       else
         i:=0;
       fits:=false;
       while (not fits) do
        begin
          if (it[i].i=instruc) and (itcache^[instruc]=-1) then
           itcache^[instruc]:=i;
          if (it[i].i=instruc) and (it[i].ops=insops) then
           begin
             { first fit }
             case insops of
              0 : begin
                    fits:=true;
                    break;
                  end;
              1 : Begin
                    if (optyp1 and it[i].o1)<>0 then
                     Begin
                       fits:=true;
                       break;
                     end;
                   { I consider sign-extended 8bit value to }
                   { be equal to immediate 8bit therefore   }
                   { convert...                             }
                   if (optyp1 = ao_imm8) then
                   Begin
                     { check if this is a simple sign extend. }
                     if (it[i].o1<>ao_imm8s) then
                     Begin
                       fits:=true;
                       break;
                     end;
                   end;
                 end;
             2 : if ((optyp1 and it[i].o1)<>0) and
                  ((optyp2 and it[i].o2)<>0) then
                  Begin
                        fits:=true;
                        break;
                  end
                  { if the operands can be swaped }
                  { then swap them                }
                  else if ((it[i].m and af_d)<>0) and
                  ((optyp1 and it[i].o2)<>0) and
                  ((optyp2 and it[i].o1)<>0) then
                  begin
                    fits:=true;
                    break;
                  end;
             3 : if ((optyp1 and it[i].o1)<>0) and
                  ((optyp2 and it[i].o2)<>0) and
                  ((optyp3 and it[i].o3)<>0) then
                  Begin
                    fits:=true;
                    break;
                  end;
             end; { end case }

           end;
          if it[i].i=A_NONE then
           InternalError(191919);
        end;

      { Old Writer code }
                       if (pai386(hp)^._operator=A_PUSH) and
                          (pai386(hp)^.size=S_W) and
                          (pai386(hp)^.op1t=top_const) then
                        begin
{$ifdef EXTDEBUG}
                          AsmWriteLn('# workaround for pushw'#9+tostr(longint(pai386(hp)^.op1)));
{$endif}
                          AsmWriteLn(#9'.byte 0x66,0x68');
                          AsmWriteLn(#9'.word '+tostr(longint(pai386(hp)^.op1)));
                        end
                       else
                        begin
                          calljmp:=(pai386(hp)^._operator=A_CALL) or (pai386(hp)^._operator=A_JMP);
                        { call maybe not translated to calll }
                          if calljmp then
                           s:=#9+att_op2str[pai386(hp)^._operator]
                          else
                           s:=#9+att_op2str[pai386(hp)^._operator]+att_opsize2str[pai386(hp)^.size];
                        { process operands }
                          if pai386(hp)^.op1t<>top_none then
                           begin
                           { call and jmp need an extra handling                          }
                           { this code is only called if jmp isn't a labeled instruction }
                             if calljmp then
                              s:=s+#9+getopstr_jmp(pai386(hp)^.op1t,pai386(hp)^.op1)
                             else
                              begin
                                s:=s+#9+getopstr(pai386(hp)^.op1t,pai386(hp)^.op1);
                                if pai386(hp)^.op3t<>top_none then
                                 begin
                                   if pai386(hp)^.op2t<>top_none then
                                    s:=s+','+getopstr(pai386(hp)^.op2t,
                                      pointer(longint(twowords(pai386(hp)^.op2).word1)));
                                    s:=s+','+getopstr(pai386(hp)^.op3t,
                                    pointer(longint(twowords(pai386(hp)^.op2).word2)));
                                 end
                                else
                                 if pai386(hp)^.op2t<>top_none then
                                  s:=s+','+getopstr(pai386(hp)^.op2t,pai386(hp)^.op2);
                              end;
                           end;
                          AsmWriteLn(s);
                        end;
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
ait_stab_function_name:
                     funcname:=pai_stab_function_name(hp)^.str;
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
                           begin
                             lastsec:=pai_section(hp^.next)^.sec;
                             lastsecidx:=pai_section(hp^.next)^.idataidx;
                           end;
                          hp:=pai(hp^.next);
                        end;
{$ifdef GDB}
                       { force write of filename }
                       FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
                       includecount:=0;
                       funcname:=nil;
                       WriteFileLineInfo(hp^.fileinfo);
{$endif GDB}
                       if lastsec<>sec_none then
                         AsmWriteLn(ait_section2str(lastsec,lastsecidx));
                       AsmStartSize:=AsmSize;
                     end;
        ait_marker : ;
           else
             internalerror(10000);
           end;
           hp:=pai(hp^.next);
        end;
      end;


    procedure ti386coffasmlist.WriteAsmList;
    var
      p:dirstr;
      n:namestr;
      e:extstr;
{$ifdef GDB}
      fileinfo : tfileposinfo;
{$endif GDB}

    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       Comment(v_info,'Start writing att-styled assembler output for '+current_module^.mainsource^);
{$endif}

      LastSec:=sec_none;
{$ifdef GDB}
      FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
{$endif GDB}
      FillChar(lastfileinfo,sizeof(lastfileinfo),0);
      LastInfile:=nil;

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

{$ifdef GDB}
      n_line:=n_bssline;
      funcname:=nil;
      linecount:=1;
      includecount:=0;
      fileinfo.fileindex:=1;
      fileinfo.line:=1;
      { Write main file }
      WriteFileLineInfo(fileinfo);
{$endif GDB}
      AsmStartSize:=AsmSize;

      countlabelref:=false;
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
       comment(v_info,'Done writing att-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
    end;


end.
{
  $Log$
  Revision 1.1  1999-01-10 15:37:51  peter
    * moved some tables from ra386*.pas -> i386.pas
    + start of coff writer
    * renamed asmutils unit to rautils

}
