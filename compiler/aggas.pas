{
    $Id$
    Copyright (c) 1998-2002 by the Free Pascal team

    This unit implements generic GNU assembler (v2.8 or later)

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
{ Base unit for writing GNU assembler output.
}
unit aggas;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      globals,
      aasmbase,aasmtai,aasmcpu,
      assemble;



    type

      {# This is a derived class which is used to write
         GAS styled assembler.

         The WriteInstruction() method must be overriden
         to write a single instruction to the assembler
         file.
      }
      TGNUAssembler=class(texternalassembler)
      public
        procedure WriteTree(p:TAAsmoutput);override;
        procedure WriteAsmList;override;
{$ifdef GDB}
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
        procedure WriteFileEndInfo;
{$endif}
        procedure WriteInstruction(hp: tai);  virtual; abstract;
      end;

implementation

    uses
{$ifdef Delphi}
      dmisc,
{$else Delphi}
      dos,
{$endif Delphi}
      cutils,globtype,systems,
      fmodule,finput,verbose,cpubase
{$ifdef GDB}
  {$ifdef delphi}
      ,sysutils
  {$else}
      ,strings
  {$endif}
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
      lasTSec      : TSection; { last section type written }
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : tinputfile;
      symendcount  : longint;

    type
      t80bitarray = array[0..9] of byte;
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;

{****************************************************************************}
{                          Support routines                                  }
{****************************************************************************}

   function fixline(s:string):string;
   {
     return s with all leading and ending spaces and tabs removed
   }
     var
       i,j,k : integer;
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


  { convert floating point values }
  { to correct endian             }
  procedure swap64bitarray(var t: t64bitarray);
    var
     b: byte;
    begin
      b:= t[7];
      t[7] := t[0];
      t[0] := b;

      b := t[6];
      t[6] := t[1];
      t[1] := b;

      b:= t[5];
      t[5] := t[2];
      t[2] := b;

      b:= t[4];
      t[4] := t[3];
      t[3] := b;
   end;


   procedure swap32bitarray(var t: t32bitarray);
    var
     b: byte;
    begin
      b:= t[1];
      t[1]:= t[2];
      t[2]:= b;

      b:= t[0];
      t[0]:= t[3];
      t[3]:= b;
    end;

   procedure swap80bitarray(var t: t80bitarray);
    begin
     {!!!!!!!!!!!!}
    end;



    const
      ait_const2str : array[ait_const_32bit..ait_const_8bit] of string[8]=
       (#9'.long'#9,#9'.short'#9,#9'.byte'#9);


    function ait_section2str(s:TSection):string;
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
      LasTSec:=s;
    end;

{****************************************************************************}
{                          GNU Assembler writer                              }
{****************************************************************************}

{$ifdef GDB}
      procedure TGNUAssembler.WriteFileLineInfo(var fileinfo : tfileposinfo);
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

      procedure TGNUAssembler.WriteFileEndInfo;

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


    procedure TGNUAssembler.WriteTree(p:TAAsmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
      nolinetai =[ait_label,
                  ait_regalloc,ait_tempalloc,
{$ifdef GDB}
                  ait_stabn,ait_stabs,ait_stab_function_name,
{$endif GDB}
                  ait_cut,ait_marker,ait_align,ait_section];
    type
      t80bitarray = array[0..9] of byte;
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;
    var
      ch       : char;
      hp       : tai;
      consttyp : taitype;
      s        : string;
      found    : boolean;
      i,pos,l  : longint;
      InlineLevel : longint;
      co       : comp;
      sin      : single;
      d        : double;
      e        : extended;
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
               AsmWritePChar(tai_comment(hp).str);
               AsmLn;
             End;

           ait_regalloc :
             begin
               if (cs_asm_regalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Register '+std_reg2str[tai_regalloc(hp).reg]+
                   allocstr[tai_regalloc(hp).allocation]);
             end;

           ait_tempalloc :
             begin
               if (cs_asm_tempalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                   tostr(tai_tempalloc(hp).tempsize)+allocstr[tai_tempalloc(hp).allocation]);
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
               { swap the values to correct endian if required }
               if source_info.endian <> target_info.endian then
                 swap64bitarray(t64bitarray(d));
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
               { swap the values to correct endian if required }
               if source_info.endian <> target_info.endian then
                 swap32bitarray(t32bitarray(sin));
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
               { swap the values to correct endian if required }
               if source_info.endian <> target_info.endian then
                 swap64bitarray(t64bitarray(co));
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
               if target_info.system in [system_i386_linux,system_i386_beos] then
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
               if target_info.system in [system_i386_linux,system_i386_beos] then
                begin
                  s:=target_asm.labelprefix+'e'+tostr(symendcount);
                  inc(symendcount);
                  AsmWriteLn(s+':');
                  AsmWrite(#9'.size'#9);
                  AsmWrite(tai_symbol_end(hp).sym.name);
                  AsmWrite(', '+s+' - ');
                  AsmWriteLn(tai_symbol_end(hp).sym.name);
                end;
             end;

           ait_instruction :
             begin
               WriteInstruction(hp);
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
                       lasTSec:=tai_section(hp.next).sec;
                     hp:=tai(hp.next);
                   end;
{$ifdef GDB}
                  { force write of filename }
                  FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
                  includecount:=0;
                  funcname:=nil;
                  WriteFileLineInfo(hp.fileinfo);
{$endif GDB}
                  if lasTSec<>sec_none then
                    AsmWriteLn(ait_section2str(lasTSec));
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




    procedure TGNUAssembler.WriteAsmList;
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
       Comment(v_info,'Start writing gas-styled assembler output for '+current_module.mainsource^);
{$endif}

      LasTSec:=sec_none;
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

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Done writing gas-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
    end;

end.
{
  $Log$
  Revision 1.9  2002-08-18 20:06:23  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.8  2002/07/26 21:15:37  florian
    * rewrote the system handling

  Revision 1.7  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.6  2002/07/01 18:46:20  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.5  2002/05/18 13:34:05  peter
    * readded missing revisions

  Revision 1.4  2002/05/16 19:46:34  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.2  2002/04/15 18:53:48  carl
  + comments in register allocator uses std_Reg2str

  Revision 1.1  2002/04/14 16:51:54  carl
  + basic GNU assembler writer class

}

