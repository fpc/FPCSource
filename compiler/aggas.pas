{
    Copyright (c) 1998-2006 by the Free Pascal team

    This unit implements the generic part of the GNU assembler
    (v2.8 or later) writer

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
      globtype,globals,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      assemble;


    type
      TCPUInstrWriter = class;
      {# This is a derived class which is used to write
         GAS styled assembler.
      }
      TGNUAssembler=class(texternalassembler)
      protected
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;virtual;
        procedure WriteSection(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder);
        procedure WriteExtraHeader;virtual;
        procedure WriteInstruction(hp: tai);
       public
        function MakeCmdLine: TCmdStr; override;
        procedure WriteTree(p:TAsmList);override;
        procedure WriteAsmList;override;
        destructor destroy; override;
       private
        setcount: longint;
        procedure WriteDecodedSleb128(a: int64);
        procedure WriteDecodedUleb128(a: qword);
        function NextSetLabel: string;
       protected
        InstrWriter: TCPUInstrWriter;
      end;


      {# This is the base class for writing instructions.

         The WriteInstruction() method must be overriden
         to write a single instruction to the assembler
         file.
      }
      TCPUInstrWriter = class
        constructor create(_owner: TGNUAssembler);
        procedure WriteInstruction(hp : tai); virtual; abstract;
       protected
        owner: TGNUAssembler;
      end;


      TAppleGNUAssembler=class(TGNUAssembler)
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
       private
        debugframecount: aint;
       end;


implementation

    uses
      SysUtils,
      cutils,cfileutils,systems,
      fmodule,finput,verbose,
      itcpugas,cpubase
      ;

    const
      line_length = 70;

    var
      CurrSecType  : TAsmSectiontype; { last section type written }
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
      ait_const2str : array[aitconst_128bit..aitconst_indirect_symbol] of string[20]=(
        #9'.fixme128'#9,#9'.quad'#9,#9'.long'#9,#9'.short'#9,#9'.byte'#9,
        #9'.sleb128'#9,#9'.uleb128'#9,
        #9'.rva'#9,#9'.indirect_symbol'#9
      );

{****************************************************************************}
{                          GNU Assembler writer                              }
{****************************************************************************}

    destructor TGNUAssembler.Destroy;
      begin
        InstrWriter.free;
        inherited destroy;
      end;


    function TGNUAssembler.MakeCmdLine: TCmdStr;
      begin
        result := inherited MakeCmdLine;
        // MWE: disabled again. It generates dwarf info for the generated .s
        //      files as well. This conflicts with the info we generate
        // if target_dbg.id = dbg_dwarf then
        //  result := result + ' --gdwarf-2';
      end;


    function TGNUAssembler.NextSetLabel: string;
      begin
        inc(setcount);
        result := target_asm.labelprefix+'$set$'+tostr(setcount);
      end;

    function TGNUAssembler.sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
      const
        secnames : array[TAsmSectiontype] of string[17] = ('',
          '.text',
          '.data',
{ why doesn't .rodata work? (FK) }
{$warning TODO .rodata not yet working}
{$if defined(arm) or defined(powerpc)}
          '.rodata',
{$else arm}
          '.data',
{$endif arm}
          '.bss',
          '.threadvar',
          '.pdata',
          '', { stubs }
          '.stab',
          '.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev',
          '.fpc',
          '.toc',
          '.init'
        );
        secnames_pic : array[TAsmSectiontype] of string[17] = ('',
          '.text',
          '.data.rel',
          '.data.rel',
          '.bss',
          '.threadvar',
          '.pdata',
          '', { stubs }
          '.stab',
          '.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev',
          '.fpc',
          '.toc',
          '.init'
        );
      var
        sep     : string[3];
        secname : string;
      begin
        if (cs_create_pic in current_settings.moduleswitches) and
           not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
          secname:=secnames_pic[atype]
        else
          secname:=secnames[atype];
{$ifdef m68k}
        { old Amiga GNU AS doesn't support .section .fpc }
        if (atype=sec_fpc) and (target_info.system = system_m68k_amiga) then
            secname:=secnames[sec_data];
{$endif}
        if (atype=sec_fpc) and (Copy(aname,1,3)='res') then
          begin
            result:=secname+'.'+aname;
            exit;
          end;

        if (atype=sec_threadvar) and
          (target_info.system=system_i386_win32) then
          secname:='.tls';

        { For bss we need to set some flags that are target dependent,
          it is easier to disable it for smartlinking. It doesn't take up
          filespace }
        if not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) and
           use_smartlink_section and
           (aname<>'') and
           (atype <> sec_toc) and
           (atype<>sec_bss) then
          begin
            case aorder of
              secorder_begin :
                sep:='.b_';
              secorder_end :
                sep:='.z_';
              else
                sep:='.n_';
            end;
            result:=secname+sep+aname
          end
        else
          result:=secname;
      end;


    procedure TGNUAssembler.WriteSection(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder);
      var
        s : string;
      begin
        AsmLn;
        case target_info.system of
         system_i386_OS2,
         system_i386_EMX,
         system_m68k_amiga,  { amiga has old GNU AS (2.14), which blews up from .section (KB) }
         system_m68k_linux: ;
         system_powerpc_darwin,
         system_i386_darwin:
           begin
             if (atype = sec_stub) then
               AsmWrite('.section ');
           end
         else
          AsmWrite('.section ');
        end;
        s:=sectionname(atype,aname,aorder);
        AsmWrite(s);
        case atype of
          sec_fpc :
            if aname = 'resptrs' then
              AsmWrite(', "a", @progbits');
          sec_stub :
            begin
              case target_info.system of
                { there are processor-independent shortcuts available    }
                { for this, namely .symbol_stub and .picsymbol_stub, but }
                { they don't work and gcc doesn't use them either...     }
                system_powerpc_darwin:
                  AsmWriteln('__TEXT,__symbol_stub1,symbol_stubs,pure_instructions,16');
                system_i386_darwin:
                  AsmWriteln('__IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5');
                else
                  internalerror(2006031101);
              end;
            end;
        end;
        AsmLn;
        CurrSecType:=atype;
      end;


    procedure TGNUAssembler.WriteDecodedUleb128(a: qword);
      var
        i,len : longint;
        buf   : array[0..63] of byte;
      begin
        len:=EncodeUleb128(a,buf);
        for i:=0 to len-1 do
          begin
            if (i > 0) then
              AsmWrite(',');
            AsmWrite(tostr(buf[i]));
          end;
      end;


    procedure TGNUAssembler.WriteDecodedSleb128(a: int64);
      var
        i,len : longint;
        buf   : array[0..255] of byte;
      begin
        len:=EncodeSleb128(a,buf);
        for i:=0 to len-1 do
          begin
            if (i > 0) then
              AsmWrite(',');
            AsmWrite(tostr(buf[i]));
          end;
      end;


    procedure TGNUAssembler.WriteTree(p:TAsmList);

    function needsObject(hp : tai_symbol) : boolean;
      begin
        needsObject :=
            (
              assigned(hp.next) and
               (tai(hp.next).typ in [ait_const,ait_datablock,
                ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit])
            ) or
            (hp.sym.typ=AT_DATA);

      end;

    var
      ch       : char;
      hp       : tai;
      hp1      : tailineinfo;
      constdef : taiconst_type;
      s,t      : string;
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

      sepChar : char;
      nextdwarffileidx : longint;
    begin
      if not assigned(p) then
       exit;

       nextdwarffileidx:=1;

      last_align := 2;
      InlineLevel:=0;
      { lineinfo is only needed for al_procedures (PFV) }
      do_line:=(cs_asm_source in current_settings.globalswitches) or
               ((cs_lineinfo in current_settings.moduleswitches)
                 and (p=current_asmdata.asmlists[al_procedures]));
      hp:=tai(p.first);
      while assigned(hp) do
       begin
         if not(hp.typ in SkipLineInfo) then
          begin
            hp1 := hp as tailineinfo;
            current_filepos:=hp1.fileinfo;
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
                      if (cs_asm_source in current_settings.globalswitches) then
                       infile.open;
                    end;
                   { avoid unnecessary reopens of the same file !! }
                   lastfileinfo.fileindex:=hp1.fileinfo.fileindex;
                   { be sure to change line !! }
                   lastfileinfo.line:=-1;
                 end;
              { write source }
                if (cs_asm_source in current_settings.globalswitches) and
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
               if (cs_asm_regalloc in current_settings.globalswitches) then
                 begin
                   AsmWrite(#9+target_asm.comment+'Register ');
                   repeat
                     AsmWrite(std_regname(Tai_regalloc(hp).reg));
                     if (hp.next=nil) or
                        (tai(hp.next).typ<>ait_regalloc) or
                        (tai_regalloc(hp.next).ratype<>tai_regalloc(hp).ratype) then
                       break;
                     hp:=tai(hp.next);
                     AsmWrite(',');
                   until false;
                   AsmWrite(' ');
                   AsmWriteLn(regallocstr[tai_regalloc(hp).ratype]);
                 end;
             end;

           ait_tempalloc :
             begin
               if (cs_asm_tempalloc in current_settings.globalswitches) then
                 begin
{$ifdef EXTDEBUG}
                   if assigned(tai_tempalloc(hp).problem) then
                     AsmWriteLn(target_asm.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                       tostr(tai_tempalloc(hp).tempsize)+' '+tai_tempalloc(hp).problem^)
                   else
{$endif EXTDEBUG}
                     AsmWriteLn(target_asm.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                       tostr(tai_tempalloc(hp).tempsize)+' '+tempallocstr[tai_tempalloc(hp).allocation]);
                 end;
             end;

           ait_align :
             begin
               if tai_align_abstract(hp).aligntype>1 then
                 begin
                   if not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
                     begin
                       AsmWrite(#9'.balign '+tostr(tai_align_abstract(hp).aligntype));
                       if tai_align_abstract(hp).use_op then
                         AsmWrite(','+tostr(tai_align_abstract(hp).fillop))
{$ifdef x86}
                       { force NOP as alignment op code }
                       else if CurrSecType=sec_code then
                         AsmWrite(',0x90');
{$endif x86}
                     end
                   else
                     begin
                       { darwin as only supports .align }
                       if not ispowerof2(tai_align_abstract(hp).aligntype,i) then
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
                 WriteSection(tai_section(hp).sectype,tai_section(hp).name^,tai_section(hp).secorder)
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
               if target_info.system in [system_powerpc_darwin,system_i386_darwin] then
                 begin
                   {On Mac OS X you can't have common symbols in a shared
                    library, since those are in the TEXT section and the text section is
                    read-only in shared libraries (so it can be shared among different
                    processes). The alternate code creates some kind of common symbols in
                    the data segment. The generic code no longer uses common symbols, but
                    this doesn't work on Mac OS X as well.}
                   if tai_datablock(hp).is_global then
                     begin
                       asmwrite('.globl ');
                       asmwriteln(tai_datablock(hp).sym.name);
                       asmwriteln('.data');
                       asmwrite('.zerofill __DATA, __common, ');
                       asmwrite(tai_datablock(hp).sym.name);
                       asmwriteln(', '+tostr(tai_datablock(hp).size)+','+tostr(last_align));
                       if not(CurrSecType in [sec_data,sec_none]) then
                         writesection(CurrSecType,'',secorder_default);
                     end
                   else
                     begin
                       asmwrite(#9'.lcomm'#9);
                       asmwrite(tai_datablock(hp).sym.name);
                       asmwrite(','+tostr(tai_datablock(hp).size));
                       asmwrite(','+tostr(last_align));
                       asmwriteln('');
                     end
                 end
               else
                 begin
                   if Tai_datablock(hp).is_global then
                     begin
                       asmwrite(#9'.globl ');
                       asmwriteln(Tai_datablock(hp).sym.name);
                     end;
                   if (target_info.system <> system_arm_linux) then
                     sepChar := '@'
                   else
                     sepChar := '%';
                   if (tf_needs_symbol_type in target_info.flags) then
                     asmwriteln(#9'.type '+Tai_datablock(hp).sym.name+','+sepChar+'object');
                   if (tf_needs_symbol_size in target_info.flags) and (tai_datablock(hp).size > 0) then
                     asmwriteln(#9'.size '+Tai_datablock(hp).sym.name+','+tostr(Tai_datablock(hp).size));
                   asmwrite(Tai_datablock(hp).sym.name);
                   asmwriteln(':');
                   asmwriteln(#9'.zero '+tostr(Tai_datablock(hp).size));
                 end;
             end;

           ait_const:
             begin
               constdef:=tai_const(hp).consttype;
               case constdef of
{$ifndef cpu64bit}
                 aitconst_128bit :
                    begin
                      internalerror(200404291);
                    end;

                 aitconst_64bit :
                    begin
                      if assigned(tai_const(hp).sym) then
                        internalerror(200404292);
                      AsmWrite(ait_const2str[aitconst_32bit]);
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
                 aitconst_uleb128bit,
                 aitconst_sleb128bit,
{$ifdef cpu64bit}
                 aitconst_128bit,
                 aitconst_64bit,
{$endif cpu64bit}
                 aitconst_32bit,
                 aitconst_16bit,
                 aitconst_8bit,
                 aitconst_rva_symbol,
                 aitconst_indirect_symbol :
                   begin
                     if (target_info.system in [system_powerpc_darwin,system_i386_darwin]) and
                        (tai_const(hp).consttype in [aitconst_uleb128bit,aitconst_sleb128bit]) then
                       begin
                         AsmWrite(ait_const2str[aitconst_8bit]);
                         case tai_const(hp).consttype of
                           aitconst_uleb128bit:
                             WriteDecodedUleb128(qword(tai_const(hp).value));
                           aitconst_sleb128bit:
                             WriteDecodedSleb128(int64(tai_const(hp).value));
                         end
                       end
                     else
                       begin
                         AsmWrite(ait_const2str[tai_const(hp).consttype]);
                         l:=0;
                         t := '';
                         repeat
                           if assigned(tai_const(hp).sym) then
                             begin
                               if assigned(tai_const(hp).endsym) then
                                 begin
                                   if (target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
                                     begin
                                       s := NextSetLabel;
                                       t := #9'.set '+s+','+tai_const(hp).endsym.name+'-'+tai_const(hp).sym.name;
                                     end
                                   else
                                     s:=tai_const(hp).endsym.name+'-'+tai_const(hp).sym.name
                                  end
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
                              not(CurrSecType in [sec_data,sec_rodata]) or
                              (l>line_length) or
                              (hp.next=nil) or
                              (tai(hp.next).typ<>ait_const) or
                              (tai_const(hp.next).consttype<>constdef) or
                              assigned(tai_const(hp.next).sym) then
                             break;
                           hp:=tai(hp.next);
                           AsmWrite(',');
                         until false;
                         if (t <> '') then
                           begin
                             AsmLn;
                             AsmWrite(t);
                           end;
                       end;
                      AsmLn;
                   end;
                 end;
             end;

           { the "and defined(FPC_HAS_TYPE_EXTENDED)" isn't optimal but currently the only solution
             it prevents proper cross compilation to i386 though
           }
{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
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
               co:=comp(tai_comp_64bit(hp).value);
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
               if (tai_label(hp).labsym.is_used) then
                begin
                  if tai_label(hp).labsym.bind=AB_GLOBAL then
                   begin
                     AsmWrite('.globl'#9);
                     AsmWriteLn(tai_label(hp).labsym.name);
                   end;
                  AsmWrite(tai_label(hp).labsym.name);
                  AsmWriteLn(':');
                end;
             end;

           ait_symbol :
             begin
               if (target_info.system = system_powerpc64_linux) and
                 (tai_symbol(hp).sym.typ = AT_FUNCTION) and (cs_profile in current_settings.moduleswitches) then
                 begin
                 AsmWriteLn('.globl _mcount');
               end;
               if tai_symbol(hp).is_global then
                begin
                  AsmWrite('.globl'#9);
                  AsmWriteLn(tai_symbol(hp).sym.name);
                end;
               if (target_info.system = system_powerpc64_linux) and
                 (tai_symbol(hp).sym.typ = AT_FUNCTION) then
                 begin
                   AsmWriteLn('.section "opd", "aw"');
                   AsmWriteLn('.align 3');
                   AsmWriteLn(tai_symbol(hp).sym.name + ':');
                   AsmWriteLn('.quad .' + tai_symbol(hp).sym.name + ', .TOC.@tocbase, 0');
                   AsmWriteLn('.previous');
                   AsmWriteLn('.size ' + tai_symbol(hp).sym.name + ', 24');
                   if (tai_symbol(hp).is_global) then
                     AsmWriteLn('.globl .' + tai_symbol(hp).sym.name);
                   AsmWriteLn('.type .' + tai_symbol(hp).sym.name + ', @function');
                   { the dotted name is the name of the actual function entry }
                   AsmWrite('.');
                 end
               else
                 begin
                   if (target_info.system <> system_arm_linux) then
                     sepChar := '@'
                   else
                     sepChar := '#';
                   if (tf_needs_symbol_type in target_info.flags) then
                     begin
                       AsmWrite(#9'.type'#9 + tai_symbol(hp).sym.name);
                       if (needsObject(tai_symbol(hp))) then
                         AsmWriteLn(',' + sepChar + 'object')
                       else
                         AsmWriteLn(',' + sepChar + 'function');
                     end;
                 end;
               AsmWriteLn(tai_symbol(hp).sym.name + ':');
             end;

           ait_symbol_end :
             begin
               if tf_needs_symbol_size in target_info.flags then
                begin
                  s:=target_asm.labelprefix+'e'+tostr(symendcount);
                  inc(symendcount);
                  AsmWriteLn(s+':');
                  AsmWrite(#9'.size'#9);
                  if (target_info.system = system_powerpc64_linux) and (tai_symbol_end(hp).sym.typ = AT_FUNCTION) then
                    AsmWrite('.');
                  AsmWrite(tai_symbol_end(hp).sym.name);
                  AsmWrite(', '+s+' - ');
                  if (target_info.system = system_powerpc64_linux) and (tai_symbol_end(hp).sym.typ = AT_FUNCTION) then
                     AsmWrite('.');
                  AsmWriteLn(tai_symbol_end(hp).sym.name);
                end;
             end;

           ait_instruction :
             begin
               WriteInstruction(hp);
             end;

           ait_stab :
             begin
               if assigned(tai_stab(hp).str) then
                 begin
                   AsmWrite(#9'.'+stabtypestr[tai_stab(hp).stabtype]+' ');
                   AsmWritePChar(tai_stab(hp).str);
                   AsmLn;
                 end;
             end;

           ait_file :
             begin
               tai_file(hp).idx:=nextdwarffileidx;
               inc(nextdwarffileidx);
               AsmWrite(#9'.file '+tostr(tai_file(hp).idx)+' "');

               AsmWritePChar(tai_file(hp).str);
               AsmWrite('"');
               AsmLn;
             end;

           ait_loc :
             begin
               AsmWrite(#9'.loc '+tostr(tai_loc(hp).fileentry.idx)+' '+tostr(tai_loc(hp).line)+' '+tostr(tai_loc(hp).column));
               AsmLn;
             end;

           ait_force_line,
           ait_function_name : ;

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
                       CurrSecType:=tai_section(hp.next).sectype;
                     hp:=tai(hp.next);
                   end;
                  if CurrSecType<>sec_none then
                    WriteSection(CurrSecType,'',secorder_default);
                  AsmStartSize:=AsmSize;

                  { reset dwarf file index }
                  nextdwarffileidx:=1;
                end;
             end;

           ait_marker :
             if tai_marker(hp).kind=mark_InlineStart then
               inc(InlineLevel)
             else if tai_marker(hp).kind=mark_InlineEnd then
               dec(InlineLevel);

           ait_directive :
             begin
               AsmWrite('.'+directivestr[tai_directive(hp).directive]+' ');
               if assigned(tai_directive(hp).name) then
                 AsmWrite(tai_directive(hp).name^);
               AsmLn;
             end;

           else
             internalerror(2006012201);
         end;
         hp:=tai(hp.next);
       end;
    end;


    procedure TGNUAssembler.WriteExtraHeader;
      begin
      end;


    procedure TGNUAssembler.WriteInstruction(hp: tai);
      begin
        InstrWriter.WriteInstruction(hp);
      end;


    procedure TGNUAssembler.WriteAsmList;
    var
      n : string;
      hal : tasmlisttype;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       Comment(V_Debug,'Start writing gas-styled assembler output for '+current_module.mainsource^);
{$endif}

      CurrSecType:=sec_none;
      FillChar(lastfileinfo,sizeof(lastfileinfo),0);
      LastInfile:=nil;

      if assigned(current_module.mainsource) then
        n:=ExtractFileName(current_module.mainsource^)
      else
        n:=InputFileName;
      AsmWriteLn(#9'.file "'+FixFileName(n)+'"');
      WriteExtraHeader;
      AsmStartSize:=AsmSize;
      symendcount:=0;

      for hal:=low(TasmlistType) to high(TasmlistType) do
        begin
          AsmWriteLn(target_asm.comment+'Begin asmlist '+AsmlistTypeStr[hal]);
          writetree(current_asmdata.asmlists[hal]);
          AsmWriteLn(target_asm.comment+'End asmlist '+AsmlistTypeStr[hal]);
        end;

{
      Result doesn't work properly yet due to a bug in Apple's linker

      if (cs_create_smart in current_settings.moduleswitches) and
         (target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
        AsmWriteLn(#9'.subsections_via_symbols');
}

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       Comment(V_Debug,'Done writing gas-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
    end;


{****************************************************************************}
{                        Apple/GNU Assembler writer                          }
{****************************************************************************}

    function TAppleGNUAssembler.sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
      begin
        if (target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
          case atype of
            sec_bss:
              { all bss (lcomm) symbols are automatically put in the right }
              { place by using the lcomm assembler directive               }
              atype := sec_none;
            sec_debug_frame,
            sec_eh_frame:
              begin
                result := '.section __DWARFA,__debug_frame,coalesced,no_toc+strip_static_syms'#10'EH_frame'+tostr(debugframecount)+':';
                inc(debugframecount);
                exit;
              end;
            sec_rodata:
              begin
                result := '.const';
                exit;
              end;
            sec_fpc:
              begin
                result := '.section __TEXT, .fpc, regular, no_dead_strip';
                exit;
              end;
          end;
        result := inherited sectionname(atype,aname,aorder);
      end;


{****************************************************************************}
{                        Abstract Instruction Writer                         }
{****************************************************************************}

     constructor TCPUInstrWriter.create(_owner: TGNUAssembler);
       begin
         inherited create;
         owner := _owner;
       end;

end.
