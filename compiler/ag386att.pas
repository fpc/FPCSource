{
    $Id$
    Copyright (c) 1996-98 by the FPC development team

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
{$endif Delphi}
      dos,strings,
      globtype,globals,systems,
      files,verbose
      ,i386base,i386asm
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


    procedure ti386attasmlist.WriteTree(p:paasmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
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
      do_line:=(cs_debuginfo in aktmoduleswitches) or (cs_asm_source in aktglobalswitches);
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         aktfilepos:=hp^.fileinfo;
         if do_line then
          begin
          { I think it is better to write stabs before source line PM }
{$ifdef GDB}
          { write stabs }
            if cs_debuginfo in aktmoduleswitches then
             begin
               if not (hp^.typ in  [
                      ait_label,
                      ait_regalloc,ait_tempalloc,
                      ait_stabn,ait_stabs,ait_section,
                      ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
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
                not (hp^.typ in  [
                      ait_label,
                      ait_stabn,ait_stabs,ait_section,
                      ait_cut,ait_align,ait_stab_function_name]) then
             begin
               if (infile<>lastinfile) and assigned(lastinfile) then
                 begin
                   AsmWriteLn(target_asm.comment+'['+infile^.name^+']');
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
                    AsmWriteLn('.globl'#9+pai_label(hp)^.l^.name);
                  AsmWriteLn(pai_label(hp)^.l^.name+':');
                end;
             end;

           ait_symbol :
             begin
               if pai_symbol(hp)^.is_global then
                AsmWriteLn('.globl'#9+pai_symbol(hp)^.sym^.name);
               if target_info.target=target_i386_linux then
                begin
                   AsmWrite(#9'.type'#9+pai_symbol(hp)^.sym^.name);
                   if assigned(pai(hp^.next)) and
                      (pai(hp^.next)^.typ in [ait_const_symbol,ait_const_rva,
                         ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_datablock,
                         ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit]) then
                    AsmWriteLn(',@object')
                   else
                    AsmWriteLn(',@function');
                end;
               AsmWriteLn(pai_symbol(hp)^.sym^.name+':');
             end;

           ait_instruction :
             begin
               op:=pai386(hp)^.opcode;
               calljmp:=is_calljmp(op);
             { call maybe not translated to calll }
               s:=#9+att_op2str[op]+cond2str[pai386(hp)^.condition];
               if (not calljmp) and
                  (not att_nosuffix[op]) and
                  not(
                   (pai386(hp)^.oper[0].typ=top_reg) and
                   (pai386(hp)^.oper[0].reg in [R_ST..R_ST7])
                  ) then
                s:=s+att_opsize2str[pai386(hp)^.opsize];
             { process operands }
               if pai386(hp)^.ops<>0 then
                begin
                { call and jmp need an extra handling                          }
                { this code is only called if jmp isn't a labeled instruction }
                  if calljmp then
                   s:=s+#9+getopstr_jmp(pai386(hp)^.oper[0])
                  else
                   begin
                     for i:=0to pai386(hp)^.ops-1 do
                      begin
                        if i=0 then
                         sep:=#9
                        else
                         sep:=',';
                        s:=s+sep+getopstr(pai386(hp)^.oper[i])
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
  Revision 1.3  1999-07-03 00:27:04  peter
    * better smartlinking support

  Revision 1.2  1999/06/22 15:25:14  peter
    * merged

  Revision 1.1.2.1  1999/06/22 15:23:08  peter
    * reinserted

  Revision 1.100  1999/06/22 14:41:20  peter
    * merged

  Revision 1.99  1999/06/14 17:47:44  peter
    * merged

  Revision 1.97.2.3  1999/06/22 14:40:27  peter
    * small change to fpureg check

  Revision 1.97.2.2  1999/06/22 14:20:19  peter
    * fixed parsing and writing of fpureg

  Revision 1.97.2.1  1999/06/14 17:30:41  peter
    * align fixes from pierre

  Revision 1.98  1999/06/11 22:54:10  pierre
    * .align problem treated :
      .align is considered as .p2align on go32v1 and go32v2
      and as .balign on other targets
    + ra386att supports also .balign and .p2align
    * ag386att uses .balign allways

  Revision 1.97  1999/06/09 23:00:06  peter
    * small ansistring fixes
    * val_ansistr_sint destsize changed to longint
    * don't write low/hi ascii with -al

  Revision 1.96  1999/06/06 15:53:13  peter
    * suffix adding can be turned of for some tasmops in att_nosuffix array

  Revision 1.95  1999/05/27 19:43:56  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.94  1999/05/23 18:41:54  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.93  1999/05/21 13:54:39  peter
    * NEWLAB for label as symbol

  Revision 1.92  1999/05/16 17:03:05  peter
    * better file position info

  Revision 1.91  1999/05/12 00:19:36  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.90  1999/05/08 19:52:31  peter
    + MessagePos() which is enhanced Message() function but also gets the
      position info
    * Removed comp warnings

  Revision 1.89  1999/05/07 00:38:22  pierre
   * comp fixes 2

  Revision 1.88  1999/05/07 00:09:35  pierre
   * better comp output

  Revision 1.87  1999/05/06 09:05:06  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.86  1999/05/04 21:44:29  florian
    * changes to compile it with Delphi 4.0

  Revision 1.85  1999/05/02 23:29:57  peter
    * readded condition, becuase it's needed for set<cond> and cmov<cond> !

  Revision 1.84  1999/05/02 22:41:47  peter
    * moved section names to systems
    * fixed nasm,intel writer

  Revision 1.83  1999/05/02 21:33:51  florian
    * several bugs regarding -Or fixed

  Revision 1.82  1999/05/01 13:47:51  peter
    * fix hack for fsub

  Revision 1.81  1999/05/01 13:23:56  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.80  1999/04/17 22:17:04  pierre
    * ifdef USE_OP3 released (changed into ifndef NO_OP3)
    * SHRD and SHLD first operand (ATT syntax) can only be CL reg or immediate const

  Revision 1.79  1999/04/16 11:49:37  peter
    + tempalloc
    + -at to show temp alloc info in .s file

  Revision 1.78  1999/04/16 10:00:54  pierre
    + ifdef USE_OP3 code :
      added all missing op_... constructors for tai386 needed
      for SHRD,SHLD and IMUL code in assembler readers
      (check in tests/tbs0123.pp)

  Revision 1.77  1999/04/14 12:44:46  daniel
  * Proper fix for the .bss conflict

  Revision 1.76  1999/04/14 11:43:25  michael
  + reverted back to .section .bss

  Revision 1.75  1999/04/13 08:45:33  daniel
  * EMX assembler prefers .bss instead of .section .bss

  Revision 1.74  1999/04/10 16:14:59  peter
    * fixed browcol
    + -ar to show regalloc info in .s file

  Revision 1.73  1999/04/09 08:33:45  peter
    * write * before register with call for the stricter as versions

  Revision 1.72  1999/03/31 13:55:02  peter
    * assembler inlining working for ag386bin

  Revision 1.71  1999/03/29 16:05:42  peter
    * optimizer working for ag386bin

  Revision 1.70  1999/03/10 21:48:21  florian
    * bug0218 fixed, ag386att writes now all real types as byte
      sequences to minimize rouding error, in -al mode the
      value is written as comment

  Revision 1.69  1999/03/10 13:25:43  pierre
    section order changed to get closer output from coff writer

  Revision 1.68  1999/03/02 02:56:09  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.67  1999/03/01 15:46:15  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes

  Revision 1.66  1999/02/26 00:48:12  peter
    * assembler writers fixed for ag386bin

  Revision 1.65  1999/02/25 21:02:17  peter
    * ag386bin updates
    + coff writer

  Revision 1.64  1999/02/22 02:14:57  peter
    * updates for ag386bin

  Revision 1.63  1999/02/17 10:16:25  peter
    * small fixes for the binary writer

  Revision 1.62  1999/01/12 14:21:26  peter
    * fixed pushw warning

  Revision 1.61  1998/12/29 18:50:04  jonas
    * don't write debug info if not (cs_debuginfo in aktmoduleswitches)

  Revision 1.60  1998/12/23 22:53:43  peter
    * don't count ait_marker for lineinfo

  Revision 1.58  1998/12/11 00:02:38  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.57  1998/12/01 23:36:32  pierre
   * zero padded alignment was buggy

  Revision 1.56  1998/12/01 11:19:37  peter
    * fixed range problem with in [tasmop]

  Revision 1.55  1998/11/30 09:42:53  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.54  1998/11/17 10:04:13  pierre
   * zero indexed file not searched

  Revision 1.53  1998/11/17 00:26:08  peter
    * fixed for $H+

  Revision 1.52  1998/11/12 11:19:32  pierre
   * fix for first line of function break

  Revision 1.51  1998/11/09 09:21:18  pierre
   * fix for stabs line infos

  Revision 1.50  1998/11/06 09:49:25  pierre
   * n_line stuff cleaned

  Revision 1.49  1998/10/26 23:07:02  peter
    * fixpath fix

  Revision 1.48  1998/10/15 15:08:39  pierre
    * removed lots of unnecessary inputfile system.open calls
      (made a big speed decrease on go32v2 !)

  Revision 1.47  1998/10/13 14:01:05  peter
    * fixed -al

  Revision 1.46  1998/10/13 13:10:07  peter
    * new style for m68k/i386 infos and enums

  Revision 1.45  1998/10/12 12:20:39  pierre
    + added tai_const_symbol_offset
      for r : pointer = @var.field;
    * better message for different arg names on implementation
      of function

  Revision 1.44  1998/10/06 17:16:32  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.43  1998/10/01 20:19:12  jonas
    + ait_marker support

  Revision 1.42  1998/09/28 16:57:08  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.41  1998/09/20 17:11:19  jonas
    * released REGALLOC

  Revision 1.40  1998/09/16 17:58:34  jonas
    * fixed -dRegAlloc and -dDRegalloc problems

  Revision 1.39  1998/09/11 11:30:41  pierre
  -al -g option bug corrected

  Revision 1.38.2.1  1998/09/11 10:49:09  pierre
    * bug with -g -al option removed

  Revision 1.38  1998/09/07 22:23:35  peter
    * fixed for no gdb compiler

  Revision 1.37  1998/09/07 18:33:34  peter
    + smartlinking for win95 imports

  Revision 1.36  1998/09/04 17:34:19  pierre
    * bug with datalabel corrected
    + assembler errors better commented
    * one nested record crash removed

  Revision 1.35  1998/09/03 17:08:38  pierre
    * better lines for stabs
      (no scroll back to if before else part
      no return to case line at jump outside case)
    + source lines also if not in order

  Revision 1.34  1998/09/03 11:22:41  peter
    + support for cs_asm_source

  Revision 1.33  1998/08/26 10:06:33  peter
    * reduce amount of asmfiles generated
    * no stabs are written in writefilelineinfo when debuginfo is off

  Revision 1.32  1998/08/20 09:26:35  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.31  1998/08/11 14:01:16  peter
    * @object type also for extended and comp

  Revision 1.30  1998/08/10 23:56:02  peter
    * fixed extended writing

  Revision 1.29  1998/08/10 14:49:35  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.27  1998/08/08 12:30:07  florian
    * extended writing improved

  Revision 1.26  1998/08/08 10:19:16  florian
    * small fixes to write the extended type correct

  Revision 1.28  1998/08/10 10:01:33  peter
    * Fixed with GDB undefined

  Revision 1.25  1998/08/06 16:53:25  pierre
    * debugging info corrected

  Revision 1.24  1998/07/14 14:46:37  peter
    * released NEWINPUT

  Revision 1.23  1998/07/07 11:19:51  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.22  1998/06/08 22:59:42  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.21  1998/06/05 17:46:01  peter
    * tp doesn't like comp() typecast

  Revision 1.20  1998/06/04 23:51:27  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.19  1998/05/31 14:13:29  peter
    * fixed call bugs with assembler readers
    + OPR_SYMBOL to hold a symbol in the asm parser
    * fixed staticsymtable vars which were acessed through %ebp instead of
      name

  Revision 1.18  1998/05/28 17:24:25  peter
    - $R- for tp to solve range errors with in[]

  Revision 1.17  1998/05/25 17:11:34  pierre
    * firstpasscount bug fixed
      now all is already set correctly the first time
      under EXTDEBUG try -gp to skip all other firstpasses
      it works !!
    * small bug fixes
      - for smallsets with -dTESTSMALLSET
      - some warnings removed (by correcting code !)

  Revision 1.16  1998/05/23 01:20:54  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.15  1998/05/11 13:07:53  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.14  1998/05/06 18:36:53  peter
    * tai_section extended with code,data,bss sections and enumerated type
    * ident 'compiled by FPC' moved to pmodules
    * small fix for smartlink

  Revision 1.13  1998/05/06 08:38:32  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.12  1998/05/04 17:54:24  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.11  1998/05/01 07:43:52  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.10  1998/04/30 15:59:39  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.9  1998/04/29 10:33:41  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.8  1998/04/28 08:23:58  pierre
    * bug in stabn generation fixed

  Revision 1.7  1998/04/27 23:10:27  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.6  1998/04/21 11:30:13  peter
    * fixed $ifdef regalloc

  Revision 1.5  1998/04/16 16:53:24  jonas
    * changed $ifdef regalloc to $ifdef dregalloc (= debugging info)

  Revision 1.4  1998/04/09 15:46:38  florian
    + register allocation tracing stuff added

  Revision 1.3  1998/04/08 16:58:00  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

}
