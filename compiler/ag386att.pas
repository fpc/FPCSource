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
{$ifdef TP}
  {$N+,E+}
{$endif}
unit ag386att;

    interface

    uses cobjects,aasm,assemble;

    type
      pi386attasmlist=^ti386attasmlist;
      ti386attasmlist=object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
{$ifdef GDB}
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
{$endif}
      end;

  implementation

    uses
{$ifdef Delphi}
      dmisc,
{$else Delphi}
      dos,
{$endif Delphi}
      strings,
      globtype,globals,systems,
      files,verbose,cpubase,cpuasm
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
      lastinfile   : pinputfile;
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

    type
      pdouble = ^double;
    function comp2str(d : bestreal) : string;
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
      s : string;
    begin
      if ref.is_immediate then
       begin
         internalerror(1000101);
         exit;
       end
      else
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
             s:=s+symbol^.name;
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
          getopstr:='$'+tostr(o.val);
        top_symbol :
          begin
            if assigned(o.sym) then
              hs:='$'+o.sym^.name
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
             if assigned(infile) then
              begin
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


    procedure ti386attasmlist.WriteTree(p:paasmoutput);
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
      hp       : pai;
      consttyp : tait;
      s        : string;
      found    : boolean;
      i,pos,l  : longint;
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
      { lineinfo is only needed for codesegment (PFV) }
      do_line:=(cs_asm_source in aktglobalswitches) or
               ((cs_lineinfo in aktmoduleswitches) and (p=codesegment));
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         aktfilepos:=hp^.fileinfo;

         if not(hp^.typ in nolinetai) then
          begin
{$ifdef GDB}
             { write stabs }
             if cs_debuginfo in aktmoduleswitches then
               WriteFileLineInfo(hp^.fileinfo);
{$endif GDB}

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
                      (hp^.fileinfo.line<infile^.maxlinebuf) then
                     begin
                       if (hp^.fileinfo.line<>0) and
                          (infile^.linebuf^[hp^.fileinfo.line]>=0) then
                         AsmWriteLn(target_asm.comment+'['+tostr(hp^.fileinfo.line)+'] '+
                           fixline(infile^.GetLineStr(hp^.fileinfo.line)));
                       { set it to a negative value !
                       to make that is has been read already !! PM }
                       infile^.linebuf^[hp^.fileinfo.line]:=-infile^.linebuf^[hp^.fileinfo.line]-1;
                     end;
                 end;
{$ifdef LINEINFO}
              { lineinfo }
                if (cs_lineinfo in aktmoduleswitches) then
                 begin
                   if (infile<>lastinfile) then
                    begin
                      lineinfolist^.concat(new(pai_const(init_8bit
                    end
                   else
                    begin
                    end;
                 end;
{$endif LINEINFO}
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

           ait_align :
             begin
               AsmWrite(#9'.balign '+tostr(pai_align(hp)^.aligntype));
               if pai_align(hp)^.use_op then
                AsmWrite(','+tostr(pai_align(hp)^.fillop));
               AsmLn;
             end;

           ait_section :
             begin
               if pai_section(hp)^.sec<>sec_none then
                begin
                  AsmLn;
                  AsmWriteLn(ait_section2str(pai_section(hp)^.sec));
{$ifdef GDB}
                  lastfileinfo.line:=-1;
{$endif GDB}
                end;
             end;

           ait_datablock :
             begin
               if pai_datablock(hp)^.is_global then
                AsmWrite(#9'.comm'#9)
               else
                AsmWrite(#9'.lcomm'#9);
               AsmWrite(pai_datablock(hp)^.sym^.name);
               AsmWriteLn(','+tostr(pai_datablock(hp)^.size));
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
               AsmWrite(#9'.long'#9+pai_const_symbol(hp)^.sym^.name);
               if pai_const_symbol(hp)^.offset>0 then
                 AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
               else if pai_const_symbol(hp)^.offset<0 then
                 AsmWrite(tostr(pai_const_symbol(hp)^.offset));
               AsmLn;
             end;

           ait_const_rva :
             AsmWriteLn(#9'.rva'#9+pai_const_symbol(hp)^.sym^.name);

           ait_real_80bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+extended2str(pai_real_80bit(hp)^.value));
             { Make sure e is a extended type, bestreal could be
               a different type (bestreal) !! (PFV) }
               e:=pai_real_80bit(hp)^.value;
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
                AsmWriteLn(target_asm.comment+double2str(pai_real_64bit(hp)^.value));
               d:=pai_real_64bit(hp)^.value;
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
                AsmWriteLn(target_asm.comment+single2str(pai_real_32bit(hp)^.value));
               sin:=pai_real_32bit(hp)^.value;
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
                AsmWriteLn(target_asm.comment+comp2str(pai_comp_64bit(hp)^.value));
               AsmWrite(#9'.byte'#9);
{$ifdef FPC}
               co:=comp(pai_comp_64bit(hp)^.value);
{$else}
               co:=pai_comp_64bit(hp)^.value;
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

           ait_string :
             begin
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

           ait_label :
             begin
               if (pai_label(hp)^.l^.is_used) then
                begin
                  if pai_label(hp)^.l^.typ=AS_GLOBAL then
                   begin
                     AsmWrite('.globl'#9);
                     AsmWriteLn(pai_label(hp)^.l^.name);
                   end;
                  AsmWrite(pai_label(hp)^.l^.name);
                  AsmWriteLn(':');
                end;
             end;

           ait_symbol :
             begin
               if pai_symbol(hp)^.is_global then
                begin
                  AsmWrite('.globl'#9);
                  AsmWriteLn(pai_symbol(hp)^.sym^.name);
                end;
               if target_info.target=target_i386_linux then
                begin
                   AsmWrite(#9'.type'#9);
                   AsmWrite(pai_symbol(hp)^.sym^.name);
                   if assigned(pai(hp^.next)) and
                      (pai(hp^.next)^.typ in [ait_const_symbol,ait_const_rva,
                         ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_datablock,
                         ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit]) then
                    AsmWriteLn(',@object')
                   else
                    AsmWriteLn(',@function');
                   if pai_symbol(hp)^.sym^.size>0 then
                    begin
                      AsmWrite(#9'.size'#9);
                      AsmWrite(pai_symbol(hp)^.sym^.name);
                      AsmWrite(', ');
                      AsmWriteLn(tostr(pai_symbol(hp)^.sym^.size));
                    end;
                end;
               AsmWrite(pai_symbol(hp)^.sym^.name);
               AsmWriteLn(':');
             end;

           ait_symbol_end :
             begin
               if target_info.target=target_i386_linux then
                begin
                  s:=target_asm.labelprefix+'e'+tostr(symendcount);
                  inc(symendcount);
                  AsmWriteLn(s+':');
                  AsmWrite(#9'.size'#9);
                  AsmWrite(pai_symbol(hp)^.sym^.name);
                  AsmWrite(', '+s+' - ');
                  AsmWriteLn(pai_symbol(hp)^.sym^.name);
                end;
             end;

           ait_instruction :
             begin
               op:=paicpu(hp)^.opcode;
               calljmp:=is_calljmp(op);
             { call maybe not translated to call }
               s:=#9+att_op2str[op]+cond2str[paicpu(hp)^.condition];
               if (not calljmp) and
                  (att_needsuffix[op]<>AttSufNONE) and
                  (op<>A_FNSTSW) and
                  not(
                   (paicpu(hp)^.oper[0].typ=top_reg) and
                   (paicpu(hp)^.oper[0].reg in [R_ST..R_ST7])
                  ) then
                  s:=s+att_opsize2str[paicpu(hp)^.opsize];
             { process operands }
               if paicpu(hp)^.ops<>0 then
                begin
                { call and jmp need an extra handling                          }
                { this code is only called if jmp isn't a labeled instruction  }
                { quick hack to overcome a problem with manglednames=255 chars }
                  if calljmp then
                    begin
                       AsmWrite(s+#9);
                       s:=+getopstr_jmp(paicpu(hp)^.oper[0]);
                    end
                  else
                   begin
                     for i:=0 to paicpu(hp)^.ops-1 do
                      begin
                        if i=0 then
                         sep:=#9
                        else
                         sep:=',';
                        s:=s+sep+getopstr(paicpu(hp)^.oper[i])
                      end;
                   end;
                end;
               AsmWriteLn(s);
             end;

{$ifdef GDB}
           ait_stabs :
             begin
               AsmWrite(#9'.stabs ');
               AsmWritePChar(pai_stabs(hp)^.str);
               AsmLn;
             end;

           ait_stabn :
             begin
               AsmWrite(#9'.stabn ');
               AsmWritePChar(pai_stabn(hp)^.str);
               AsmLn;
             end;

           ait_force_line :
             stabslastfileinfo.line:=0;

           ait_stab_function_name:
             funcname:=pai_stab_function_name(hp)^.str;
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
                     AsmCreate(pai_cut(hp)^.place);
                   end;
                { avoid empty files }
                  while assigned(hp^.next) and (pai(hp^.next)^.typ in [ait_cut,ait_section,ait_comment]) do
                   begin
                     if pai(hp^.next)^.typ=ait_section then
                       lastsec:=pai_section(hp^.next)^.sec;
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
                    AsmWriteLn(ait_section2str(lastsec));
                  AsmStartSize:=AsmSize;
                end;
             end;

           ait_marker :
             ;

           else
             internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;


    procedure ti386attasmlist.WriteAsmList;
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
  Revision 1.25  2000-02-07 17:51:20  peter
    * quick hack for fnstsww which is not supported under linux as

  Revision 1.24  2000/01/28 09:41:39  peter
    * fixed fpu suffix parsing for att reader

  Revision 1.23  2000/01/07 01:14:18  peter
    * updated copyright to 2000

  Revision 1.22  1999/12/18 20:00:33  florian
    * Bug reported by Marco fixed: Intel assembler reader: fld qword ptr x
      was read as fldq x but it must be fldl x

  Revision 1.21  1999/12/08 10:39:59  pierre
    + allow use of unit var in exports of DLL for win32
      by using direct export writing by default instead of use of DEFFILE
      that does not allow assembler labels that do not
      start with an underscore.
      Use -WD to force use of Deffile for Win32 DLL

  Revision 1.20  1999/11/06 14:34:16  peter
    * truncated log to 20 revs

  Revision 1.19  1999/11/02 15:06:56  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.18  1999/10/27 16:11:28  peter
    * insns.dat is used to generate all i386*.inc files

  Revision 1.17  1999/09/27 23:36:33  peter
    * fixed -al with macro's

  Revision 1.16  1999/09/21 20:53:21  florian
    * fixed 1/s problem from mailing list

  Revision 1.15  1999/09/19 20:55:11  florian
    * fixed calls to procedures with manglednames=255 chars
      (taking the address of such a procedure would still cause a problem!)

  Revision 1.14  1999/09/10 18:48:00  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.13  1999/09/02 17:07:38  florian
    * problems with -Or fixed: tdef.isfpuregable was wrong!

  Revision 1.12  1999/08/25 16:03:46  peter
    * symbol name is now written using separate asmwrite() calls to overcome
      > 255 char strings

  Revision 1.11  1999/08/25 11:59:32  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.10  1999/08/13 15:44:57  peter
    * first things to include lineinfo in the executable

  Revision 1.9  1999/08/10 12:26:20  pierre
   * avoid double .edata section if using DLLTOOL

  Revision 1.8  1999/08/04 00:22:34  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.7  1999/07/30 12:26:07  peter
    * write .size only for linux

  Revision 1.6  1999/07/29 20:53:56  peter
    * write .size also

  Revision 1.5  1999/07/22 09:37:29  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.4  1999/07/18 10:19:38  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.3  1999/07/03 00:27:04  peter
    * better smartlinking support

  Revision 1.2  1999/06/22 15:25:14  peter
    * merged

  Revision 1.1.2.1  1999/06/22 15:23:08  peter
    * reinserted

  Revision 1.100  1999/06/22 14:41:20  peter
    * merged

}
