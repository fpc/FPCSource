{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an asmoutput class for i386 AT&T syntax

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
unit ag386att;

{$i defines.inc}

interface

    uses
      cclasses,
      globals,
      aasm,assemble;

    type
      T386ATTAssembler=class(texternalassembler)
        procedure WriteTree(p:TAAsmoutput);override;
        procedure WriteAsmList;override;
{$ifdef GDB}
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
        procedure WriteFileEndInfo;
{$endif}
      end;

  implementation

    uses
{$ifdef Delphi}
      sysutils,
      dmisc,
{$else Delphi}
      strings,
      dos,
{$endif Delphi}
      cutils,globtype,systems,
      fmodule,finput,verbose,cpubase,cpuasm,tainst
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
      lastsec      : tsection; { last section type written }
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : tinputfile;
      symendcount  : longint;

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
      begin
         str(d,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         single2str:='0d'+hs
      end;

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


    function getreferencestring(var ref : treference) : string;
    var
      s : string;
    begin
      with ref do
       begin
         inc(offset,offsetfixup);
         offsetfixup:=0;
       { have we a segment prefix ? }
       { These are probably not correctly handled under GAS }
       { should be replaced by coding the segment override  }
       { directly! - DJGPP FAQ                              }
         if segment<>R_NO then
          s:=att_reg2str[segment]+':'
         else
          s:='';
         if assigned(symbol) then
          s:=s+symbol.name;
         if offset<0 then
          s:=s+tostr(offset)
         else
          if (offset>0) then
           begin
             if assigned(symbol) then
              s:=s+'+'+tostr(offset)
             else
              s:=s+tostr(offset);
           end
         else if (index=R_NO) and (base=R_NO) and not assigned(symbol) then
           s:=s+'0';
         if (index<>R_NO) and (base=R_NO) then
          begin
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
            begin
              s:=s+'('+att_reg2str[base]+','+att_reg2str[index];
              if scalefactor<>0 then
               s:=s+','+tostr(scalefactor)+')'
              else
               s := s+')';
            end;
       end;
      getreferencestring:=s;
    end;

    function getopstr(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr:=att_reg2str[o.reg];
        top_ref :
          getopstr:=getreferencestring(o.ref^);
        top_const :
          getopstr:='$'+tostr(longint(o.val));
        top_symbol :
          begin
            if assigned(o.sym) then
              hs:='$'+o.sym.name
            else
              hs:='$';
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
          getopstr_jmp:='*'+att_reg2str[o.reg];
        top_ref :
          getopstr_jmp:='*'+getreferencestring(o.ref^);
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


    function ait_section2str(s:tsection):string;
    begin
       ait_section2str:=target_asm.secnames[s];
{$ifdef GDB}
       { this is needed for line info in data }
       funcname:=nil;
       case s of
         sec_code : n_line:=n_textline;
         sec_data : n_line:=n_dataline;
         sec_bss  : n_line:=n_bssline;
         else       n_line:=n_dataline;
      end;
{$endif GDB}
      LastSec:=s;
    end;


{$ifdef GDB}
      procedure T386ATTAssembler.WriteFileLineInfo(var fileinfo : tfileposinfo);
        var
          curr_n : byte;
        begin
          if not ((cs_debuginfo in aktmoduleswitches) or
             (cs_gdb_lineinfo in aktglobalswitches)) then
           exit;
        { file changed ? (must be before line info) }
          if (fileinfo.fileindex<>0) and
             (stabslastfileinfo.fileindex<>fileinfo.fileindex) then
           begin
             infile:=current_module.sourcefiles.get_file(fileinfo.fileindex);
             if assigned(infile) then
              begin
                if includecount=0 then
                 curr_n:=n_sourcefile
                else
                 curr_n:=n_includefile;
                if (infile.path^<>'') then
                 begin
                   AsmWriteLn(#9'.stabs "'+lower(BsToSlash(FixPath(infile.path^,false)))+'",'+
                     tostr(curr_n)+',0,0,'+'Ltext'+ToStr(IncludeCount));
                 end;
                AsmWriteLn(#9'.stabs "'+lower(FixFileName(infile.name^))+'",'+
                  tostr(curr_n)+',0,0,'+'Ltext'+ToStr(IncludeCount));
                AsmWriteLn('Ltext'+ToStr(IncludeCount)+':');
                inc(includecount);
              end;
           end;
        { line changed ? }
          if (stabslastfileinfo.line<>fileinfo.line) and (fileinfo.line<>0) then
           begin
             if (n_line=n_textline) and assigned(funcname) and
                (target_info.use_function_relative_addresses) then
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

      procedure T386ATTAssembler.WriteFileEndInfo;

        begin
          if not ((cs_debuginfo in aktmoduleswitches) or
             (cs_gdb_lineinfo in aktglobalswitches)) then
           exit;
          AsmLn;
          AsmWriteLn(ait_section2str(sec_code));
          AsmWriteLn(#9'.stabs "",'+tostr(n_sourcefile)+',0,0,Letext');
          AsmWriteLn('Letext:');
        end;

{$endif GDB}


    procedure T386ATTAssembler.WriteTree(p:TAAsmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
      nolinetai =[ait_label,
                  ait_regalloc,ait_tempalloc,
                  ait_stabn,ait_stabs,ait_section,
                  ait_cut,ait_marker,ait_align,ait_stab_function_name];
    type
      t80bitarray = array[0..9] of byte;
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;
    var
      ch       : char;
      hp       : tai;
      consttyp : tait;
      s        : string;
      found    : boolean;
      i,pos,l  : longint;
      InlineLevel : longint;
      co       : comp;
      sin      : single;
      d        : double;
      e        : extended;
      op       : tasmop;
      calljmp,
      do_line  : boolean;
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
{$ifdef GDB}
             { write stabs }
             if (cs_debuginfo in aktmoduleswitches) or
                (cs_gdb_lineinfo in aktglobalswitches) then
               WriteFileLineInfo(hp.fileinfo);
{$endif GDB}

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

           ait_align :
             begin
               AsmWrite(#9'.balign '+tostr(tai_align(hp).aligntype));
               if tai_align(hp).use_op then
                AsmWrite(','+tostr(tai_align(hp).fillop));
               AsmLn;
             end;

           ait_section :
             begin
               if tai_section(hp).sec<>sec_none then
                begin
                  AsmLn;
                  AsmWriteLn(ait_section2str(tai_section(hp).sec));
{$ifdef GDB}
                  lastfileinfo.line:=-1;
{$endif GDB}
                end;
             end;

           ait_datablock :
             begin
               if tai_datablock(hp).is_global then
                AsmWrite(#9'.comm'#9)
               else
                AsmWrite(#9'.lcomm'#9);
               AsmWrite(tai_datablock(hp).sym.name);
               AsmWriteLn(','+tostr(tai_datablock(hp).size));
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
               AsmWrite(#9'.long'#9+tai_const_symbol(hp).sym.name);
               if tai_const_symbol(hp).offset>0 then
                 AsmWrite('+'+tostr(tai_const_symbol(hp).offset))
               else if tai_const_symbol(hp).offset<0 then
                 AsmWrite(tostr(tai_const_symbol(hp).offset));
               AsmLn;
             end;

           ait_const_rva :
             AsmWriteLn(#9'.rva'#9+tai_const_symbol(hp).sym.name);

           ait_real_80bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+extended2str(tai_real_80bit(hp).value));
             { Make sure e is a extended type, bestreal could be
               a different type (bestreal) !! (PFV) }
               e:=tai_real_80bit(hp).value;
               AsmWrite(#9'.byte'#9);
               for i:=0 to 9 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t80bitarray(e)[i]));
                end;
               AsmLn;
             end;

           ait_real_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+double2str(tai_real_64bit(hp).value));
               d:=tai_real_64bit(hp).value;
               AsmWrite(#9'.byte'#9);
               for i:=0 to 7 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t64bitarray(d)[i]));
                end;
               AsmLn;
             end;

           ait_real_32bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+single2str(tai_real_32bit(hp).value));
               sin:=tai_real_32bit(hp).value;
               AsmWrite(#9'.byte'#9);
               for i:=0 to 3 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t32bitarray(sin)[i]));
                end;
               AsmLn;
             end;

           ait_comp_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+extended2str(tai_comp_64bit(hp).value));
               AsmWrite(#9'.byte'#9);
{$ifdef FPC}
               co:=comp(tai_comp_64bit(hp).value);
{$else}
               co:=tai_comp_64bit(hp).value;
{$endif}
               for i:=0 to 7 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t64bitarray(co)[i]));
                end;
               AsmLn;
             end;

           ait_direct :
             begin
               AsmWritePChar(tai_direct(hp).str);
               AsmLn;
{$IfDef GDB}
               if strpos(tai_direct(hp).str,'.data')<>nil then
                 n_line:=n_dataline
               else if strpos(tai_direct(hp).str,'.text')<>nil then
                 n_line:=n_textline
               else if strpos(tai_direct(hp).str,'.bss')<>nil then
                 n_line:=n_bssline;
{$endif GDB}
             end;

           ait_string :
             begin
               pos:=0;
               for i:=1 to tai_string(hp).len do
                begin
                  if pos=0 then
                   begin
                     AsmWrite(#9'.ascii'#9'"');
                     pos:=20;
                   end;
                  ch:=tai_string(hp).str[i-1];
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
                  if (pos>line_length) or (i=tai_string(hp).len) then
                   begin
                     AsmWriteLn('"');
                     pos:=0;
                   end;
                end;
             end;

           ait_label :
             begin
               if (tai_label(hp).l.is_used) then
                begin
                  if tai_label(hp).l.defbind=AB_GLOBAL then
                   begin
                     AsmWrite('.globl'#9);
                     AsmWriteLn(tai_label(hp).l.name);
                   end;
                  AsmWrite(tai_label(hp).l.name);
                  AsmWriteLn(':');
                end;
             end;

           ait_symbol :
             begin
               if tai_symbol(hp).is_global then
                begin
                  AsmWrite('.globl'#9);
                  AsmWriteLn(tai_symbol(hp).sym.name);
                end;
               if target_info.target in [target_i386_linux,target_i386_beos] then
                begin
                   AsmWrite(#9'.type'#9);
                   AsmWrite(tai_symbol(hp).sym.name);
                   if assigned(tai(hp.next)) and
                      (tai(hp.next).typ in [ait_const_symbol,ait_const_rva,
                         ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_datablock,
                         ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit]) then
                    AsmWriteLn(',@object')
                   else
                    AsmWriteLn(',@function');
                   if tai_symbol(hp).sym.size>0 then
                    begin
                      AsmWrite(#9'.size'#9);
                      AsmWrite(tai_symbol(hp).sym.name);
                      AsmWrite(', ');
                      AsmWriteLn(tostr(tai_symbol(hp).sym.size));
                    end;
                end;
               AsmWrite(tai_symbol(hp).sym.name);
               AsmWriteLn(':');
             end;

           ait_symbol_end :
             begin
               if target_info.target in [target_i386_linux,target_i386_beos] then
                begin
                  s:=target_asm.labelprefix+'e'+tostr(symendcount);
                  inc(symendcount);
                  AsmWriteLn(s+':');
                  AsmWrite(#9'.size'#9);
                  AsmWrite(tai_symbol(hp).sym.name);
                  AsmWrite(', '+s+' - ');
                  AsmWriteLn(tai_symbol(hp).sym.name);
                end;
             end;

           ait_instruction :
             begin
               taicpu(hp).SetOperandOrder(op_att);
               op:=taicpu(hp).opcode;
               calljmp:=is_calljmp(op);
             { call maybe not translated to call }
               s:=#9+att_op2str[op]+cond2str[taicpu(hp).condition];
               { suffix needed ?  fnstsw,fldcw don't support suffixes
                 with binutils 2.9.5 under linux }
               if (not calljmp) and
                  (att_needsuffix[op]<>AttSufNONE) and
                  (op<>A_FNSTSW) and (op<>A_FSTSW) and
                  (op<>A_FNSTCW) and (op<>A_FSTCW) and
                  (op<>A_FLDCW) and
                  not(
                   (taicpu(hp).oper[0].typ=top_reg) and
                   (taicpu(hp).oper[0].reg in [R_ST..R_ST7])
                  ) then
                  s:=s+att_opsize2str[taicpu(hp).opsize];
             { process operands }
               if taicpu(hp).ops<>0 then
                begin
                { call and jmp need an extra handling                          }
                { this code is only called if jmp isn't a labeled instruction  }
                { quick hack to overcome a problem with manglednames=255 chars }
                  if calljmp then
                    begin
                       AsmWrite(s+#9);
                       s:=getopstr_jmp(taicpu(hp).oper[0]);
                    end
                  else
                   begin
                     for i:=0 to taicpu(hp).ops-1 do
                      begin
                        if i=0 then
                         sep:=#9
                        else
                         sep:=',';
                        s:=s+sep+getopstr(taicpu(hp).oper[i])
                      end;
                   end;
                end;
               AsmWriteLn(s);
             end;

{$ifdef GDB}
           ait_stabs :
             begin
               AsmWrite(#9'.stabs ');
               AsmWritePChar(tai_stabs(hp).str);
               AsmLn;
             end;

           ait_stabn :
             begin
               AsmWrite(#9'.stabn ');
               AsmWritePChar(tai_stabn(hp).str);
               AsmLn;
             end;

           ait_force_line :
             stabslastfileinfo.line:=0;

           ait_stab_function_name:
             funcname:=tai_stab_function_name(hp).str;
{$endif GDB}

           ait_cut :
             begin
               if SmartAsm then
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
{$ifdef GDB}
                  { force write of filename }
                  FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
                  includecount:=0;
                  funcname:=nil;
                  WriteFileLineInfo(hp.fileinfo);
{$endif GDB}
                  if lastsec<>sec_none then
                    AsmWriteLn(ait_section2str(lastsec));
                  AsmStartSize:=AsmSize;
                end;
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


    procedure T386ATTAssembler.WriteAsmList;
    var
      p:dirstr;
      n:namestr;
      e:extstr;
{$ifdef GDB}
      fileinfo : tfileposinfo;
{$endif GDB}

    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       Comment(v_info,'Start writing att-styled assembler output for '+current_module.mainsource^);
{$endif}

      LastSec:=sec_none;
{$ifdef GDB}
      FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
{$endif GDB}
      FillChar(lastfileinfo,sizeof(lastfileinfo),0);
      LastInfile:=nil;

      if assigned(current_module.mainsource) then
       fsplit(current_module.mainsource^,p,n,e)
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
      symendcount:=0;

      countlabelref:=false;
      If (cs_debuginfo in aktmoduleswitches) then
        WriteTree(debuglist);
      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      Writetree(resourcestringlist);
      WriteTree(bsssegment);
      Writetree(importssection);
      { exports are written by DLLTOOL
        if we use it so don't insert it twice (PM) }
      if not UseDeffileForExport and assigned(exportssection) then
        Writetree(exportssection);
      Writetree(resourcesection);
      {$ifdef GDB}
      WriteFileEndInfo;
      {$ENDIF}
      countlabelref:=true;

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Done writing att-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
    end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_i386_as_info : tasminfo =
          (
            id           : as_i386_as;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            outputbinary: false;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          );

       as_i386_as_aout_info : tasminfo =
          (
            id           : as_i386_as_aout;
            idtxt  : 'AS_AOUT';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_i386_os2;
            outputbinary: false;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : 'L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          );

       as_i386_asw_info : tasminfo =
          (
            id           : as_i386_asw;
            idtxt  : 'ASW';
            asmbin : 'asw';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_i386_win32;
            outputbinary: false;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.section .bss',
              '.section .idata$2','.section .idata$4','.section .idata$5',
                '.section .idata$6','.section .idata$7','.section .edata',
              '.stab','.stabstr')
          );


initialization
  RegisterAssembler(as_i386_as_info,T386ATTAssembler);
  RegisterAssembler(as_i386_as_aout_info,T386ATTAssembler);
  RegisterAssembler(as_i386_asw_info,T386ATTAssembler);
end.
{
  $Log$
  Revision 1.13  2002-04-02 17:11:33  peter
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

  Revision 1.11  2001/09/17 21:29:13  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.10  2001/08/30 20:57:10  peter
    * asbsd merged

  Revision 1.9  2001/05/06 17:13:23  jonas
    * completed incomplete typed constant records

  Revision 1.8  2001/04/21 15:33:03  peter
    * stupid bug, finalization to initialization renaming

  Revision 1.7  2001/04/21 12:09:00  peter
    * fixed bug 1472 (merged)

  Revision 1.6  2001/04/18 22:02:00  peter
    * registration of targets and assemblers

  Revision 1.5  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.4  2001/03/05 21:39:11  peter
    * changed to class with common TAssembler also for internal assembler

  Revision 1.3  2001/01/13 20:24:24  peter
    * fixed operand order that got mixed up for external writers after
      my previous assembler block valid instruction check

  Revision 1.2  2000/12/25 00:07:31  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.1  2000/11/30 22:18:48  florian
    * moved to i386

  Revision 1.6  2000/09/24 15:06:10  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/20 17:38:21  peter
    * smartlinking fixed for linux (merged)

  Revision 1.3  2000/07/13 12:08:24  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:28  michael
  + removed logs

}
