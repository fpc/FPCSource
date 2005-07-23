{
    Copyright (c) 1998-2004 by the Free Pascal team

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
{$IFDEF USE_SYSUTILS}
      SysUtils,
{$ELSE USE_SYSUTILS}
      dos,
{$ENDIF USE_SYSUTILS}
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
      protected
        function sectionname(atype:tasmsectiontype;const aname:string):string;virtual;
        procedure WriteSection(atype:tasmsectiontype;const aname:string);
        procedure WriteExtraHeader;virtual;
{$ifdef GDB}
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
        procedure WriteFileEndInfo;
{$endif}
        procedure WriteInstruction(hp: tai);  virtual; abstract;
      public
        procedure WriteTree(p:TAAsmoutput);override;
        procedure WriteAsmList;override;
      end;

    const
      regname_count=45;
      regname_count_bsstart=32;   { Largest power of 2 out of regname_count. }


implementation

    uses
      cutils,globtype,systems,
      fmodule,finput,verbose,
      itcpugas
{$ifdef GDB}
{$IFDEF USE_SYSUTILS}
{$ELSE USE_SYSUTILS}
      ,strings
{$ENDIF USE_SYSUTILS}
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
      lasTSecType  : TAsmSectionType; { last section type written }
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : tinputfile;
      symendcount  : longint;

    type
{$ifdef cpuextended}
      t80bitarray = array[0..9] of byte;
{$endif cpuextended}
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


    const
      ait_const2str : array[ait_const_128bit..ait_const_indirect_symbol] of string[20]=(
        #9'.fixme128'#9,#9'.quad'#9,#9'.long'#9,#9'.short'#9,#9'.byte'#9,
        #9'.sleb128'#9,#9'.uleb128'#9,
        #9'.rva'#9,#9'.indirect_symbol'#9
      );

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
                   AsmWriteLn(#9'.stabs "'+BsToSlash(FixPath(infile.path^,false))+'",'+
                     tostr(curr_n)+',0,0,'+target_asm.labelprefix+'text'+ToStr(IncludeCount));
                 end;
                AsmWriteLn(#9'.stabs "'+FixFileName(infile.name^)+'",'+
                  tostr(curr_n)+',0,0,'+target_asm.labelprefix+'text'+ToStr(IncludeCount));
                AsmWriteLn(target_asm.labelprefix+'text'+ToStr(IncludeCount)+':');
                inc(includecount);
                { force new line info }
                stabslastfileinfo.line:=-1;
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
          WriteSection(sec_code,'');
          AsmWriteLn(#9'.stabs "",'+tostr(n_sourcefile)+',0,0,'+target_asm.labelprefix+'etext');
          AsmWriteLn(target_asm.labelprefix+'etext:');
        end;

{$endif GDB}


    function TGNUAssembler.sectionname(atype:tasmsectiontype;const aname:string):string;
      const
        secnames : array[tasmsectiontype] of string[12] = ('',
{$warning TODO .rodata not yet working}
          '.text','.data','.data','.bss',
          'common',
          '.note',
          '.stab','.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame'
        );
      begin
        if use_smartlink_section and
           (atype<>sec_bss) and
           (aname<>'') then
          result:='.gnu.linkonce'+copy(secnames[atype],1,2)+'.'+aname
        else
          result:=secnames[atype];
      end;


    procedure TGNUAssembler.WriteSection(atype:tasmsectiontype;const aname:string);
      var
        s : string;
      begin
        AsmLn;
        case target_info.system of
         system_powerpc_darwin, system_i386_OS2, system_i386_EMX: ;
         else
          AsmWrite('.section ');
        end;
        s:=sectionname(atype,aname);
        AsmWrite(s);
        if copy(s,1,4)='.gnu' then
          begin
            case atype of
              sec_rodata,
              sec_data :
                AsmWrite(',""');
              sec_code :
                AsmWrite(',"x"');
            end;
          end;
        AsmLn;
{$ifdef GDB}
        { this is needed for line info in data }
        funcname:=nil;
        case atype of
          sec_code :
            n_line:=n_textline;
          sec_rodata,
          sec_data :
            n_line:=n_dataline;
          sec_bss  :
            n_line:=n_bssline;
          else
            n_line:=n_dataline;
        end;
{$endif GDB}
        LasTSecType:=atype;
      end;


    procedure TGNUAssembler.WriteTree(p:TAAsmoutput);
    const
      regallocstr : array[tregalloctype] of string[10]=(' allocated',' released',' sync',' resized');
      tempallocstr : array[boolean] of string[10]=(' released',' allocated');
    var
      ch       : char;
      hp       : tai;
      hp1      : tailineinfo;
      consttyp : taitype;
      s        : string;
      i,pos,l  : longint;
      InlineLevel : longint;
      last_align : longint;
      co       : comp;
      sin      : single;
      d        : double;
{$ifdef cpuextended}
      e        : extended;
{$endif cpuextended}
      do_line  : boolean;
    begin
      if not assigned(p) then
       exit;
      last_align := 2;
      InlineLevel:=0;
      { lineinfo is only needed for codesegment (PFV) }
      do_line:=(cs_asm_source in aktglobalswitches) or
               ((cs_lineinfo in aktmoduleswitches)
                 and (p=asmlist[codesegment]));
      hp:=tai(p.first);
      while assigned(hp) do
       begin
         if not(hp.typ in SkipLineInfo) then
          begin
            hp1 := hp as tailineinfo;
            aktfilepos:=hp1.fileinfo;
{$ifdef GDB}
             { write stabs }
             if (cs_debuginfo in aktmoduleswitches) or
                (cs_gdb_lineinfo in aktglobalswitches) then
               WriteFileLineInfo(hp1.fileinfo);
{$endif GDB}
             { no line info for inlined code }
             if do_line and (inlinelevel=0) then
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
               if (cs_asm_regalloc in aktglobalswitches) then
                 begin
                   AsmWrite(#9+target_asm.comment+'Register ');
                   repeat
                     AsmWrite(gas_regname(Tai_regalloc(hp).reg));
                     if (hp.next=nil) or
                        (tai(hp.next).typ<>ait_regalloc) or
                        (tai_regalloc(hp.next).ratype<>tai_regalloc(hp).ratype) then
                       break;
                     hp:=tai(hp.next);
                     AsmWrite(',');
                   until false;
                   AsmWriteLn(regallocstr[tai_regalloc(hp).ratype]);
                 end;
             end;

           ait_tempalloc :
             begin
               if (cs_asm_tempalloc in aktglobalswitches) then
                 begin
{$ifdef EXTDEBUG}
                   if assigned(tai_tempalloc(hp).problem) then
                     AsmWriteLn(target_asm.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                       tostr(tai_tempalloc(hp).tempsize)+' '+tai_tempalloc(hp).problem^)
                   else
{$endif EXTDEBUG}
                     AsmWriteLn(target_asm.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                       tostr(tai_tempalloc(hp).tempsize)+tempallocstr[tai_tempalloc(hp).allocation]);
                 end;
             end;

           ait_align :
             begin
               if tai_align(hp).aligntype>1 then
                 begin
                   if target_info.system <> system_powerpc_darwin then
                     begin
                       AsmWrite(#9'.balign '+tostr(tai_align(hp).aligntype));
                       if tai_align(hp).use_op then
                        AsmWrite(','+tostr(tai_align(hp).fillop))
                     end
                   else
                     begin
                       { darwin as only supports .align }
                       if not ispowerof2(tai_align(hp).aligntype,i) then
                         internalerror(2003010305);
                       AsmWrite(#9'.align '+tostr(i));
                       last_align := i;
                     end;
                   AsmLn;
                 end;
             end;

           ait_section :
             begin
               if tai_section(hp).sectype<>sec_none then
                begin
                  WriteSection(tai_section(hp).sectype,tai_section(hp).name^);
{$ifdef GDB}
                  lastfileinfo.line:=-1;
{$endif GDB}
                end
               else
                begin
{$ifdef EXTDEBUG}
                  AsmWrite(target_asm.comment);
                  AsmWriteln(' sec_none');
{$endif EXTDEBUG}
                end;
             end;

           ait_datablock :
             begin
               if (target_info.system <> system_powerpc_darwin) or
                  not tai_datablock(hp).is_global then
                 begin
                   if tai_datablock(hp).is_global then
                    AsmWrite(#9'.comm'#9)
                   else
                    AsmWrite(#9'.lcomm'#9);
                   AsmWrite(tai_datablock(hp).sym.name);
                   AsmWrite(','+tostr(tai_datablock(hp).size));
                   if (target_info.system = system_powerpc_darwin) { and
                      not(tai_datablock(hp).is_global)} then
                     AsmWrite(','+tostr(last_align));
                   AsmWriteln('');
                 end
               else
                 begin
                   AsmWrite('.globl ');
                   AsmWriteln(tai_datablock(hp).sym.name);
                   AsmWriteln('.data');
                   AsmWrite('.zerofill __DATA, __common, ');
                   AsmWrite(tai_datablock(hp).sym.name);
                   AsmWriteln(', '+tostr(tai_datablock(hp).size)+','+tostr(last_align));
                   if not(lasTSectype in [sec_data,sec_none]) then
                     WriteSection(lasTSectype,'');
                 end;
             end;

{$ifndef cpu64bit}
           ait_const_128bit :
              begin
                internalerror(200404291);
              end;

           ait_const_64bit :
              begin
                if assigned(tai_const(hp).sym) then
                  internalerror(200404292);
                AsmWrite(ait_const2str[ait_const_32bit]);
                if target_info.endian = endian_little then
                  begin
                    AsmWrite(tostr(longint(lo(tai_const(hp).value))));
                    AsmWrite(',');
                    AsmWrite(tostr(longint(hi(tai_const(hp).value))));
                  end
                else
                  begin
                    AsmWrite(tostr(longint(hi(tai_const(hp).value))));
                    AsmWrite(',');
                    AsmWrite(tostr(longint(lo(tai_const(hp).value))));
                  end;
                AsmLn;
              end;
{$endif cpu64bit}

           ait_const_uleb128bit,
           ait_const_sleb128bit,
{$ifdef cpu64bit}
           ait_const_128bit,
           ait_const_64bit,
{$endif cpu64bit}
           ait_const_32bit,
           ait_const_16bit,
           ait_const_8bit,
           ait_const_rva_symbol,
           ait_const_indirect_symbol :
             begin
               AsmWrite(ait_const2str[hp.typ]);
               consttyp:=hp.typ;
               l:=0;
               repeat
                 if assigned(tai_const(hp).sym) then
                   begin
                     if assigned(tai_const(hp).endsym) then
                       s:=tai_const(hp).endsym.name+'-'+tai_const(hp).sym.name
                     else
                       s:=tai_const(hp).sym.name;
                     if tai_const(hp).value<>0 then
                       s:=s+tostr_with_plus(tai_const(hp).value);
                   end
                 else
                   s:=tostr(tai_const(hp).value);
                 AsmWrite(s);
                 inc(l,length(s));
                 { Values with symbols are written on a single line to improve
                   reading of the .s file (PFV) }
                 if assigned(tai_const(hp).sym) or
                    not(LasTSecType in [sec_data,sec_rodata]) or
                    (l>line_length) or
                    (hp.next=nil) or
                    (tai(hp.next).typ<>consttyp) or
                    assigned(tai_const(hp.next).sym) then
                   break;
                 hp:=tai(hp.next);
                 AsmWrite(',');
               until false;
               AsmLn;
             end;

{$ifdef cpuextended}
           ait_real_80bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+'value: '+extended2str(tai_real_80bit(hp).value));
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
{$endif cpuextended}

           ait_real_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+'value: '+double2str(tai_real_64bit(hp).value));
               d:=tai_real_64bit(hp).value;
               { swap the values to correct endian if required }
               if source_info.endian <> target_info.endian then
                 swap64bitarray(t64bitarray(d));
               AsmWrite(#9'.byte'#9);
{$ifdef arm}
{ on a real arm cpu, it's already hi/lo swapped }
{$ifndef cpuarm}
               if tai_real_64bit(hp).formatoptions=fo_hiloswapped then
                 begin
                   for i:=4 to 7 do
                     begin
                       if i<>4 then
                         AsmWrite(',');
                       AsmWrite(tostr(t64bitarray(d)[i]));
                     end;
                   for i:=0 to 3 do
                     begin
                       AsmWrite(',');
                       AsmWrite(tostr(t64bitarray(d)[i]));
                     end;
                 end
               else
{$endif cpuarm}
{$endif arm}
                 begin
                   for i:=0 to 7 do
                     begin
                       if i<>0 then
                         AsmWrite(',');
                       AsmWrite(tostr(t64bitarray(d)[i]));
                     end;
                 end;
               AsmLn;
             end;

           ait_real_32bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+'value: '+single2str(tai_real_32bit(hp).value));
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
                AsmWriteLn(target_asm.comment+'value: '+extended2str(tai_comp_64bit(hp).value));
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
               if target_info.system in [system_i386_linux,system_i386_beos,
                                         system_powerpc_linux,system_m68k_linux,
                                         system_sparc_linux,system_alpha_linux,
                                         system_x86_64_linux,system_arm_linux] then
                begin
                   AsmWrite(#9'.type'#9);
                   AsmWrite(tai_symbol(hp).sym.name);
                   if assigned(tai(hp.next)) and
                      (tai(hp.next).typ in [ait_const_rva_symbol,
                         ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_datablock,
                         ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit]) then
                     begin
                       if target_info.system = system_arm_linux then
                         AsmWriteLn(',#object')
                       else
                         AsmWriteLn(',@object')
                     end
                   else
                     begin
                       if target_info.system = system_arm_linux then
                         AsmWriteLn(',#function')
                       else
                         AsmWriteLn(',@function');
                     end;
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
               if tf_needs_symbol_size in target_info.flags then
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
               if assigned(tai_stabs(hp).str) then
                 begin
                   AsmWrite(#9'.stabs ');
                   AsmWritePChar(tai_stabs(hp).str);
                   AsmLn;
                 end;
             end;

           ait_stabn :
             begin
               if assigned(tai_stabn(hp).str) then
                 begin
                   AsmWrite(#9'.stabn ');
                   AsmWritePChar(tai_stabn(hp).str);
                   AsmLn;
                 end;
             end;

           ait_force_line :
             stabslastfileinfo.line:=0;

           ait_stab_function_name:
             funcname:=tai_stab_function_name(hp).str;
{$endif GDB}

           ait_cutobject :
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
                     AsmCreate(tai_cutobject(hp).place);
                   end;
                { avoid empty files }
                  while assigned(hp.next) and (tai(hp.next).typ in [ait_cutobject,ait_section,ait_comment]) do
                   begin
                     if tai(hp.next).typ=ait_section then
                       lasTSectype:=tai_section(hp.next).sectype;
                     hp:=tai(hp.next);
                   end;
{$ifdef GDB}
                  { force write of filename }
                  FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
                  includecount:=0;
                  funcname:=nil;
                  WriteFileLineInfo(aktfilepos);
{$endif GDB}
                  if lasTSectype<>sec_none then
                    WriteSection(lasTSectype,'');
                  AsmStartSize:=AsmSize;
                end;
             end;

           ait_marker :
             if tai_marker(hp).kind=InlineStart then
               inc(InlineLevel)
             else if tai_marker(hp).kind=InlineEnd then
               dec(InlineLevel);

           ait_non_lazy_symbol_pointer:
             AsmWriteLn('.non_lazy_symbol_pointer');

           else
             internalerror(10000);
         end;
         hp:=tai(hp.next);
       end;
    end;


    procedure TGNUAssembler.WriteExtraHeader;

      begin
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
       Comment(V_Debug,'Start writing gas-styled assembler output for '+current_module.mainsource^);
{$endif}

      LasTSectype:=sec_none;
{$ifdef GDB}
      FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
{$endif GDB}
      FillChar(lastfileinfo,sizeof(lastfileinfo),0);
      LastInfile:=nil;

      if assigned(current_module.mainsource) then
{$IFDEF USE_SYSUTILS}
      begin
       p := SplitPath(current_module.mainsource^);
       n := SplitName(current_module.mainsource^);
       e := SplitExtension(current_module.mainsource^);
      end
{$ELSE USE_SYSUTILS}
       fsplit(current_module.mainsource^,p,n,e)
{$ENDIF USE_SYSUTILS}
      else
       begin
         p:=inputdir;
         n:=inputfile;
         e:=inputextension;
       end;
    { to get symify to work }
      AsmWriteLn(#9'.file "'+FixFileName(n+e)+'"');
      WriteExtraHeader;
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
        WriteTree(asmlist[debuglist]);
      WriteTree(asmlist[codesegment]);
      WriteTree(asmlist[datasegment]);
      WriteTree(asmlist[consts]);
      WriteTree(asmlist[rttilist]);
      WriteTree(asmlist[picdata]);
      Writetree(asmlist[resourcestrings]);
      WriteTree(asmlist[bsssegment]);
      Writetree(asmlist[importsection]);
      { exports are written by DLLTOOL
        if we use it so don't insert it twice (PM) }
      if not UseDeffileForExports and assigned(asmlist[exportsection]) then
        Writetree(asmlist[exportsection]);
      Writetree(asmlist[resourcesection]);
      Writetree(asmlist[dwarflist]);
      {$ifdef GDB}
      WriteFileEndInfo;
      {$ENDIF}

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       Comment(V_Debug,'Done writing gas-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
    end;

end.
