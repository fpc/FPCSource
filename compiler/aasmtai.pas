{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an abstract asmoutput class for all processor types

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
{ @abstract(This unit implements an abstract asm output class for all processor types)
  This unit implements an abstract assembler output class for all processors, these
  are then overriden for each assembler writer to actually write the data in these
  classes to an assembler file.
}

unit aasmtai;

{$i fpcdefs.inc}

interface

    uses
       cutils,cclasses,
       globtype,globals,systems,
       cginfo,cpuinfo,cpubase,
       symppu,symtype,
       aasmbase;

    type
       taitype = (
          ait_none,
          ait_align,
          ait_section,
          ait_comment,
          ait_direct,
          ait_string,
          ait_instruction,
          ait_datablock,
          ait_symbol,
          ait_symbol_end, { needed to calc the size of a symbol }
          ait_label,
          ait_const_32bit,
          ait_const_16bit,
          ait_const_8bit,
          ait_const_symbol,
          { the following is only used by the win32 version of the compiler }
          { and only the GNU AS Win32 is able to write it                   }
          ait_const_rva,
          ait_real_32bit,
          ait_real_64bit,
          ait_real_80bit,
          ait_comp_64bit,
          ait_real_128bit,
{$ifdef GDB}
          ait_stabn,
          ait_stabs,
          ait_force_line,
          ait_stab_function_name,
{$endif GDB}
{$ifdef alpha}
          { the follow is for the DEC Alpha }
          ait_frame,
          ait_ent,
{$endif alpha}
{$ifdef ia64}
          ait_bundle,
          ait_stop,
{$endif ia64}
{$ifdef m68k}
          ait_labeled_instruction,
{$endif m68k}
{$ifdef SPARC}
          ait_labeled_instruction,
{$endif SPARC}
          ait_cut, { used to split into tiny assembler files }
          ait_regalloc,
          ait_tempalloc,
          ait_marker { used to mark assembler blocks and inlined functions }
          );

    const
       taitypestr : array[taitype] of string[14] = (
          '<none>',
          'align',
          'section',
          'comment',
          'direct',
          'string',
          'instruction',
          'datablock',
          'symbol',
          'symbol_end',
          'label',
          'const_32bit',
          'const_16bit',
          'const_8bit',
          'const_symbol',
          'const_rva',
          'real_32bit',
          'real_64bit',
          'real_80bit',
          'comp_64bit',
          'real_128bit',
{$ifdef GDB}
          'stabn',
          'stabs',
          'force_line',
          'stab_funcname',
{$endif GDB}
{$ifdef alpha}
          { the follow is for the DEC Alpha }
          'frame',
          'ent',
{$endif alpha}
{$ifdef ia64}
          'bundle',
          'stop',
{$endif ia64}
{$ifdef m68k}
          'labeled_instr',
{$endif m68k}
{$ifdef SPARC}
          'labeled_instr',
{$endif SPARC}
          'cut',
          'regalloc',
          'tempalloc',
          'marker'
          );

    type
      { Types of operand }
      toptype=(top_none,top_reg,top_ref,top_const,top_symbol,top_local);

      toper=record
        ot : longint;
        case typ : toptype of
         top_none   : ();
         top_reg    : (reg:tregister);
         top_ref    : (ref:preference);
         top_const  : (val:aword);
         top_symbol : (sym:tasmsymbol;symofs:longint);
         { local varsym that will be inserted in pass_2 }
         top_local  : (localsym:pointer;localsymderef:tderef;localsymofs:longint);
      end;


{ ait_* types which don't result in executable code or which don't influence   }
{ the way the program runs/behaves, but which may be encountered by the        }
{ optimizer (= if it's sometimes added to the exprasm list). Update if you add }
{ a new ait type!                                                              }
    const
      SkipInstr = [ait_comment, ait_symbol,ait_section
{$ifdef GDB}
                   ,ait_stabs, ait_stabn, ait_stab_function_name, ait_force_line
{$endif GDB}
                   ,ait_regalloc, ait_tempalloc, ait_symbol_end];

{ ait_* types which do not have line information (and hence which are of type
  tai, otherwise, they are of type tailineinfo }
{ ait_* types which do not have line information (and hence which are of type
  tai, otherwise, they are of type tailineinfo }
      SkipLineInfo =[ait_label,
                     ait_regalloc,ait_tempalloc,
{$ifdef GDB}
                  ait_stabn,ait_stabs,ait_stab_function_name,
{$endif GDB}
                  ait_cut,ait_marker,ait_align,ait_section,ait_comment,
                  ait_const_8bit,ait_const_16bit,ait_const_32bit,
                  ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit
                  ];


    type
       { cut type, required for alphanumeric ordering of the assembler filenames }
       TCutPlace=(cut_normal,cut_begin,cut_end);

       TMarker = (NoPropInfoStart,NoPropInfoEnd,
                  AsmBlockStart,AsmBlockEnd,
                  InlineStart,InlineEnd,marker_blockstart);

       { Buffer type used for alignment }
       tfillbuffer = array[0..63] of char;

       Tspill_temp_list=array[0..255] of Treference;

       { abstract assembler item }
       tai = class(TLinkedListItem)
{$ifndef NOOPT}
          { pointer to record with optimizer info about this tai object }
          optinfo  : pointer;
{$endif NOOPT}
          typ      : taitype;
          constructor Create;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);virtual;
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;
          procedure derefimpl;virtual;
       end;

       { abstract assembler item with line information }
       tailineinfo = class(tai)
        fileinfo : tfileposinfo;
        constructor Create;
        constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
        procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;


       taiclass = class of tai;

       taiclassarray = array[taitype] of taiclass;

       { Generates an assembler string }
       tai_string = class(tailineinfo)
          str : pchar;
          { extra len so the string can contain an \0 }
          len : longint;
          constructor Create(const _str : string);
          constructor Create_pchar(_str : pchar);
          constructor Create_length_pchar(_str : pchar;length : longint);
          destructor Destroy;override;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function getcopy:tlinkedlistitem;override;
       end;

       { Generates a common label }
       tai_symbol = class(tailineinfo)
          is_global : boolean;
          sym       : tasmsymbol;
          size      : longint;
          constructor Create(_sym:tasmsymbol;siz:longint);
          constructor Create_Global(_sym:tasmsymbol;siz:longint);
          constructor Createname(const _name : string;siz:longint);
          constructor Createname_global(const _name : string;siz:longint);
          constructor Createdataname(const _name : string;siz:longint);
          constructor Createdataname_global(const _name : string;siz:longint);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
       end;

       tai_symbol_end = class(tailineinfo)
          sym : tasmsymbol;
          constructor Create(_sym:tasmsymbol);
          constructor Createname(const _name : string);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
       end;

       { Generates an assembler label }
       tai_label = class(tai)
          is_global : boolean;
          l         : tasmlabel;
          constructor Create(_l : tasmlabel);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
       end;

       { Directly output data to final assembler file }
       tai_direct = class(tailineinfo)
          str : pchar;
          constructor Create(_str : pchar);
          destructor Destroy; override;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function getcopy:tlinkedlistitem;override;
       end;

       { Generates an assembler comment }
       tai_comment = class(tai)
          str : pchar;
          constructor Create(_str : pchar);
          destructor Destroy; override;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function getcopy:tlinkedlistitem;override;
       end;


       { Generates a section / segment directive }
       tai_section = class(tai)
          sec : TSection;
          constructor Create(s : TSection);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;


       { Generates an uninitializised data block }
       tai_datablock = class(tailineinfo)
          is_global : boolean;
          sym       : tasmsymbol;
          size      : longint;
          constructor Create(const _name : string;_size : longint);
          constructor Create_global(const _name : string;_size : longint);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
       end;


       { Generates a long integer (32 bit) }
       tai_const = class(tai)
          value : longint;
          constructor Create_32bit(_value : longint);
          constructor Create_16bit(_value : word);
          constructor Create_8bit(_value : byte);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tai_const_symbol = class(tailineinfo)
          sym    : tasmsymbol;
          offset : longint;
          constructor Create(_sym:tasmsymbol);
          constructor Create_offset(_sym:tasmsymbol;ofs:longint);
          constructor Create_rva(_sym:tasmsymbol);
          constructor Createname(const name:string);
          constructor Createname_offset(const name:string;ofs:longint);
          constructor Createname_rva(const name:string);
          constructor Createdataname(const name:string);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy:tlinkedlistitem;override;
       end;

       { Generates a single float (32 bit real) }
       tai_real_32bit = class(tai)
          value : ts32real;
          constructor Create(_value : ts32real);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       { Generates a double float (64 bit real) }
       tai_real_64bit = class(tai)
          value : ts64real;
          constructor Create(_value : ts64real);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       { Generates an extended float (80 bit real) }
       tai_real_80bit = class(tai)
          value : ts80real;
          constructor Create(_value : ts80real);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       { Generates a comp int (integer over 64 bits)

          This is Intel 80x86 specific, and is not
          really supported on other processors.
       }
       tai_comp_64bit = class(tai)
          value : ts64comp;
          constructor Create(_value : ts64comp);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       { Insert a cut to split assembler into several smaller files }
       tai_cut = class(tai)
          place : tcutplace;
          constructor Create;
          constructor Create_begin;
          constructor Create_end;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       { Insert a marker for assembler and inline blocks }
       tai_marker = class(tai)
          Kind: TMarker;
          Constructor Create(_Kind: TMarker);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tai_tempalloc = class(tai)
          allocation : boolean;
{$ifdef EXTDEBUG}
          problem : pstring;
{$endif EXTDEBUG}
          temppos,
          tempsize   : longint;
          constructor alloc(pos,size:longint);
          constructor dealloc(pos,size:longint);
{$ifdef EXTDEBUG}
          constructor allocinfo(pos,size:longint;const st:string);
{$endif EXTDEBUG}
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tai_regalloc = class(tai)
          allocation : boolean;
          reg        : tregister;
          constructor alloc(r : tregister);
          constructor dealloc(r : tregister);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

      Taasmoutput=class;

      Trggetproc=procedure(list:Taasmoutput;position:Tai;subreg:Tsubregister;var result:Tregister) of object;
      Trgungetproc=procedure(list:Taasmoutput;position:Tai;r:Tregister) of object;

       { Class template for assembler instructions
       }
       taicpu_abstract = class(tailineinfo)
       protected
          procedure ppuloadoper(ppufile:tcompilerppufile;var o:toper);virtual;abstract;
          procedure ppuwriteoper(ppufile:tcompilerppufile;const o:toper);virtual;abstract;
          procedure ppuderefoper(var o:toper);virtual;abstract;
       public
          { Condition flags for instruction }
          condition : TAsmCond;
          { Number of operands to instruction }
          ops       : byte;
          { Operands of instruction }
          oper      : array[0..max_operands-1] of toper;
          { Actual opcode of instruction }
          opcode    : tasmop;
{$ifdef x86}
          segprefix : tregister;
{$endif x86}
          { true if instruction is a jmp }
          is_jmp    : boolean; { is this instruction a jump? (needed for optimizer) }
          Constructor Create(op : tasmop);
          Destructor Destroy;override;
          function getcopy:TLinkedListItem;override;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          procedure SetCondition(const c:TAsmCond);
          procedure loadconst(opidx:longint;l:aword);
          procedure loadsymbol(opidx:longint;s:tasmsymbol;sofs:longint);
          procedure loadlocal(opidx:longint;s:pointer;sofs:longint);
          procedure loadref(opidx:longint;const r:treference);
          procedure loadreg(opidx:longint;r:tregister);
          procedure loadoper(opidx:longint;o:toper);
          procedure clearop(opidx:longint);
          function is_nop:boolean;virtual;abstract;
          function is_move:boolean;virtual;abstract;
          { register allocator }
          function get_insert_pos(p:Tai;huntfor1,huntfor2,huntfor3:Tsuperregister;var unusedregsint:Tsuperregisterset):Tai;
          procedure forward_allocation(p:Tai;var unusedregsint:Tsuperregisterset);
          function spill_registers(list:Taasmoutput;
                                   rgget:Trggetproc;
                                   rgunget:Trgungetproc;
                                   r:tsuperregisterset;
                                   var unusedregsint:Tsuperregisterset;
                                   const spilltemplist:Tspill_temp_list):boolean;virtual;
          function spilling_decode_loadstore(op: tasmop; var counterpart: tasmop; var wasload: boolean): boolean;virtual;abstract;
          function spilling_create_loadstore(op: tasmop; r:tregister; const ref:treference): tai;virtual;abstract;
          function spilling_create_load(const ref:treference;r:tregister): tai;virtual;abstract;
          function spilling_create_store(r:tregister; const ref:treference): tai;virtual;abstract;
       end;

       { alignment for operator }
       tai_align_abstract = class(tai)
          aligntype : byte;   { 1 = no align, 2 = word align, 4 = dword align }
          fillsize  : byte;   { real size to fill }
          fillop    : byte;   { value to fill with - optional }
          use_op    : boolean;
          constructor Create(b:byte);
          constructor Create_op(b: byte; _op: byte);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function calculatefillbuf(var buf : tfillbuffer):pchar;virtual;
       end;

       Ttranstable=array[Tsuperregister] of Tsuperregister;

       taasmoutput = class(tlinkedlist)
          constructor create;
          function getlasttaifilepos : pfileposinfo;
          procedure translate_registers(const table:Ttranstable);
       end;


    var
      { array with all class types for tais }
      aiclass : taiclassarray;
      { temporary lists }
      exprasmlist,
      { default lists }
      datasegment,codesegment,bsssegment,
      debuglist,withdebuglist,consts,
      importssection,exportssection,
      resourcesection,rttilist,
      resourcestringlist         : taasmoutput;

    function ppuloadai(ppufile:tcompilerppufile):tai;
    procedure ppuwriteai(ppufile:tcompilerppufile;n:tai);


implementation

    uses
{$ifdef delphi}
      sysutils,
{$else}
      strings,
{$endif}
      verbose;

    const
      pputaimarker = 254;


{****************************************************************************
                                 Helpers
 ****************************************************************************}

    function ppuloadai(ppufile:tcompilerppufile):tai;
      var
        b : byte;
        t : taitype;
      begin
        { marker }
        b:=ppufile.getbyte;
        if b<>pputaimarker then
          internalerror(200208181);
        { load nodetype }
        t:=taitype(ppufile.getbyte);
        if t<>ait_none then
         begin
           if t>high(taitype) then
             internalerror(200208182);
           if not assigned(aiclass[t]) then
             internalerror(200208183);
           {writeln('taiload: ',taitypestr[t]);}
           { generate tai of the correct class }
           ppuloadai:=aiclass[t].ppuload(t,ppufile);
         end
        else
         ppuloadai:=nil;
      end;


    procedure ppuwriteai(ppufile:tcompilerppufile;n:tai);
      begin
        { marker, read by ppuloadnode }
        ppufile.putbyte(pputaimarker);
        if assigned(n) then
         begin
           { type, read by ppuloadnode }
           ppufile.putbyte(byte(n.typ));
           {writeln('taiwrite: ',taitypestr[n.typ]);}
           n.ppuwrite(ppufile);
         end
        else
         ppufile.putbyte(byte(ait_none));
      end;


{****************************************************************************
                             TAI
 ****************************************************************************}

    constructor tai.Create;
      begin
{$ifndef NOOPT}
        optinfo:=nil;
{$endif NOOPT}
      end;


    constructor tai.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        typ:=t;
{$ifndef NOOPT}
        optinfo:=nil;
{$endif}
      end;


    procedure tai.ppuwrite(ppufile:tcompilerppufile);
      begin
      end;


    procedure tai.derefimpl;
      begin
      end;


{****************************************************************************
                              TAILINEINFO
 ****************************************************************************}

    constructor tailineinfo.create;
     begin
       inherited create;
       fileinfo:=aktfilepos;
     end;


    constructor tailineinfo.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getposinfo(fileinfo);
      end;


    procedure tailineinfo.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putposinfo(fileinfo);
      end;


{****************************************************************************
                             TAI_SECTION
 ****************************************************************************}

    constructor tai_section.Create(s : TSection);
      begin
         inherited Create;
         typ:=ait_section;
         sec:=s;
      end;


    constructor tai_section.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        sec:=tsection(ppufile.getbyte);
      end;


    procedure tai_section.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(sec));
      end;


{****************************************************************************
                             TAI_DATABLOCK
 ****************************************************************************}

    constructor tai_datablock.Create(const _name : string;_size : longint);

      begin
         inherited Create;
         typ:=ait_datablock;
         sym:=objectlibrary.newasmsymboltype(_name,AB_LOCAL,AT_DATA);
         { keep things aligned }
         if _size<=0 then
           _size:=4;
         size:=_size;
         is_global:=false;
      end;


    constructor tai_datablock.Create_global(const _name : string;_size : longint);
      begin
         inherited Create;
         typ:=ait_datablock;
         sym:=objectlibrary.newasmsymboltype(_name,AB_GLOBAL,AT_DATA);
         { keep things aligned }
         if _size<=0 then
           _size:=4;
         size:=_size;
         is_global:=true;
      end;


    constructor tai_datablock.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited Create;
        sym:=ppufile.getasmsymbol;
        size:=ppufile.getlongint;
        is_global:=boolean(ppufile.getbyte);
      end;


    procedure tai_datablock.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putasmsymbol(sym);
        ppufile.putlongint(size);
        ppufile.putbyte(byte(is_global));
      end;


    procedure tai_datablock.derefimpl;
      begin
        objectlibrary.DerefAsmsymbol(sym);
      end;


{****************************************************************************
                               TAI_SYMBOL
 ****************************************************************************}

    constructor tai_symbol.Create(_sym:tasmsymbol;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=_sym;
         size:=siz;
         sym.defbind:=AB_LOCAL;
         is_global:=false;
      end;

    constructor tai_symbol.Create_global(_sym:tasmsymbol;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=_sym;
         size:=siz;
         sym.defbind:=AB_GLOBAL;
         is_global:=true;
      end;

    constructor tai_symbol.Createname(const _name : string;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=objectlibrary.newasmsymboltype(_name,AB_LOCAL,AT_FUNCTION);
         size:=siz;
         is_global:=false;
      end;

    constructor tai_symbol.Createname_global(const _name : string;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=objectlibrary.newasmsymboltype(_name,AB_GLOBAL,AT_FUNCTION);
         size:=siz;
         is_global:=true;
      end;

    constructor tai_symbol.Createdataname(const _name : string;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=objectlibrary.newasmsymboltype(_name,AB_LOCAL,AT_DATA);
         size:=siz;
         is_global:=false;
      end;

    constructor tai_symbol.Createdataname_global(const _name : string;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=objectlibrary.newasmsymboltype(_name,AB_GLOBAL,AT_DATA);
         size:=siz;
         is_global:=true;
      end;


    constructor tai_symbol.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        sym:=ppufile.getasmsymbol;
        size:=ppufile.getlongint;
        is_global:=boolean(ppufile.getbyte);
      end;


    procedure tai_symbol.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putasmsymbol(sym);
        ppufile.putlongint(size);
        ppufile.putbyte(byte(is_global));
      end;


    procedure tai_symbol.derefimpl;
      begin
        objectlibrary.DerefAsmsymbol(sym);
      end;


{****************************************************************************
                               TAI_SYMBOL
 ****************************************************************************}

    constructor tai_symbol_end.Create(_sym:tasmsymbol);
      begin
         inherited Create;
         typ:=ait_symbol_end;
         sym:=_sym;
      end;

    constructor tai_symbol_end.Createname(const _name : string);
      begin
         inherited Create;
         typ:=ait_symbol_end;
         sym:=objectlibrary.newasmsymboltype(_name,AB_GLOBAL,AT_NONE);
      end;

    constructor tai_symbol_end.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        sym:=ppufile.getasmsymbol;
      end;


    procedure tai_symbol_end.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putasmsymbol(sym);
      end;


    procedure tai_symbol_end.derefimpl;
      begin
        objectlibrary.DerefAsmsymbol(sym);
      end;


{****************************************************************************
                               TAI_CONST
 ****************************************************************************}

    constructor tai_const.Create_32bit(_value : longint);

      begin
         inherited Create;
         typ:=ait_const_32bit;
         value:=_value;
      end;

    constructor tai_const.Create_16bit(_value : word);

      begin
         inherited Create;
         typ:=ait_const_16bit;
         value:=_value;
      end;

    constructor tai_const.Create_8bit(_value : byte);

      begin
         inherited Create;
         typ:=ait_const_8bit;
         value:=_value;
      end;


    constructor tai_const.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        value:=ppufile.getlongint;
      end;


    procedure tai_const.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putlongint(value);
      end;


{****************************************************************************
                               TAI_CONST_SYMBOL
 ****************************************************************************}

    constructor tai_const_symbol.Create(_sym:tasmsymbol);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=_sym;
         offset:=0;
         { update sym info }
         sym.increfs;
      end;

    constructor tai_const_symbol.Create_offset(_sym:tasmsymbol;ofs:longint);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=_sym;
         offset:=ofs;
         { update sym info }
         sym.increfs;
      end;

    constructor tai_const_symbol.Create_rva(_sym:tasmsymbol);
      begin
         inherited Create;
         typ:=ait_const_rva;
         sym:=_sym;
         offset:=0;
         { update sym info }
         sym.increfs;
      end;

    constructor tai_const_symbol.Createname(const name:string);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=objectlibrary.newasmsymbol(name);
         offset:=0;
         { update sym info }
         sym.increfs;
      end;

    constructor tai_const_symbol.Createname_offset(const name:string;ofs:longint);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=objectlibrary.newasmsymbol(name);
         offset:=ofs;
         { update sym info }
         sym.increfs;
      end;

    constructor tai_const_symbol.Createname_rva(const name:string);
      begin
         inherited Create;
         typ:=ait_const_rva;
         sym:=objectlibrary.newasmsymbol(name);
         offset:=0;
         { update sym info }
         sym.increfs;
      end;


    constructor tai_const_symbol.Createdataname(const name:string);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=objectlibrary.newasmsymboltype(name,AB_EXTERNAL,AT_DATA);
         offset:=0;
         { update sym info }
         sym.increfs;
      end;


    constructor tai_const_symbol.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        sym:=ppufile.getasmsymbol;
        offset:=ppufile.getlongint;
      end;


    procedure tai_const_symbol.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putasmsymbol(sym);
        ppufile.putlongint(offset);
      end;


    procedure tai_const_symbol.derefimpl;
      begin
        objectlibrary.DerefAsmsymbol(sym);
      end;


    function tai_const_symbol.getcopy:tlinkedlistitem;
      begin
        getcopy:=inherited getcopy;
        { we need to increase the reference number }
        sym.increfs;
      end;


{****************************************************************************
                               TAI_real_32bit
 ****************************************************************************}

    constructor tai_real_32bit.Create(_value : ts32real);

      begin
         inherited Create;
         typ:=ait_real_32bit;
         value:=_value;
      end;

    constructor tai_real_32bit.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        value:=ppufile.getreal;
      end;


    procedure tai_real_32bit.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putreal(value);
      end;


{****************************************************************************
                               TAI_real_64bit
 ****************************************************************************}

    constructor tai_real_64bit.Create(_value : ts64real);

      begin
         inherited Create;
         typ:=ait_real_64bit;
         value:=_value;
      end;


    constructor tai_real_64bit.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        value:=ppufile.getreal;
      end;


    procedure tai_real_64bit.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putreal(value);
      end;


{****************************************************************************
                               TAI_real_80bit
 ****************************************************************************}

    constructor tai_real_80bit.Create(_value : ts80real);

      begin
         inherited Create;
         typ:=ait_real_80bit;
         value:=_value;
      end;


    constructor tai_real_80bit.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        value:=ppufile.getreal;
      end;


    procedure tai_real_80bit.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putreal(value);
      end;


{****************************************************************************
                               Tai_comp_64bit
 ****************************************************************************}

    constructor tai_comp_64bit.Create(_value : ts64comp);

      begin
         inherited Create;
         typ:=ait_comp_64bit;
         value:=_value;
      end;


    constructor tai_comp_64bit.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.putdata(value,sizeof(value));
      end;


    procedure tai_comp_64bit.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.getdata(value,sizeof(value));
      end;


{****************************************************************************
                               TAI_STRING
 ****************************************************************************}

     constructor tai_string.Create(const _str : string);

       begin
          inherited Create;
          typ:=ait_string;
          len:=length(_str);
          getmem(str,len+1);
          strpcopy(str,_str);
       end;

     constructor tai_string.Create_pchar(_str : pchar);

       begin
          inherited Create;
          typ:=ait_string;
          str:=_str;
          len:=strlen(_str);
       end;

    constructor tai_string.Create_length_pchar(_str : pchar;length : longint);

       begin
          inherited Create;
          typ:=ait_string;
          str:=_str;
          len:=length;
       end;

    destructor tai_string.destroy;

      begin
         { you can have #0 inside the strings so }
         if str<>nil then
           freemem(str,len+1);
         inherited Destroy;
      end;


    constructor tai_string.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        len:=ppufile.getlongint;
        getmem(str,len+1);
        ppufile.getdata(str^,len);
        str[len]:=#0;
      end;


    procedure tai_string.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putlongint(len);
        ppufile.putdata(str^,len);
      end;


    function tai_string.getcopy : tlinkedlistitem;
      var
        p : tlinkedlistitem;
      begin
        p:=inherited getcopy;
        getmem(tai_string(p).str,len+1);
        move(str^,tai_string(p).str^,len+1);
        getcopy:=p;
      end;


{****************************************************************************
                               TAI_LABEL
 ****************************************************************************}

    constructor tai_label.create(_l : tasmlabel);
      begin
        inherited Create;
        typ:=ait_label;
        l:=_l;
        l.is_set:=true;
        is_global:=(l.defbind=AB_GLOBAL);
      end;


    constructor tai_label.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        l:=tasmlabel(ppufile.getasmsymbol);
        is_global:=boolean(ppufile.getbyte);
      end;


    procedure tai_label.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putasmsymbol(l);
        ppufile.putbyte(byte(is_global));
      end;


    procedure tai_label.derefimpl;
      begin
        objectlibrary.DerefAsmsymbol(tasmsymbol(l));
        l.is_set:=true;
      end;


{****************************************************************************
                              TAI_DIRECT
 ****************************************************************************}

     constructor tai_direct.Create(_str : pchar);

       begin
          inherited Create;
          typ:=ait_direct;
          str:=_str;
       end;

    destructor tai_direct.destroy;

      begin
         strdispose(str);
         inherited Destroy;
      end;

    constructor tai_direct.ppuload(t:taitype;ppufile:tcompilerppufile);
      var
        len : longint;
      begin
        inherited ppuload(t,ppufile);
        len:=ppufile.getlongint;
        getmem(str,len+1);
        ppufile.getdata(str^,len);
        str[len]:=#0;
      end;


    procedure tai_direct.ppuwrite(ppufile:tcompilerppufile);
      var
        len : longint;
      begin
        inherited ppuwrite(ppufile);
        len:=strlen(str);
        ppufile.putlongint(len);
        ppufile.putdata(str^,len);
      end;


    function tai_direct.getcopy : tlinkedlistitem;
      var
        p : tlinkedlistitem;
      begin
        p:=inherited getcopy;
        getmem(tai_direct(p).str,strlen(str)+1);
        move(str^,tai_direct(p).str^,strlen(str)+1);
        getcopy:=p;
      end;


{****************************************************************************
          tai_comment  comment to be inserted in the assembler file
 ****************************************************************************}

     constructor tai_comment.Create(_str : pchar);

       begin
          inherited Create;
          typ:=ait_comment;
          str:=_str;
       end;

    destructor tai_comment.destroy;

      begin
         strdispose(str);
         inherited Destroy;
      end;

    constructor tai_comment.ppuload(t:taitype;ppufile:tcompilerppufile);
      var
        len : longint;
      begin
        inherited ppuload(t,ppufile);
        len:=ppufile.getlongint;
        getmem(str,len+1);
        ppufile.getdata(str^,len);
        str[len]:=#0;
      end;


    procedure tai_comment.ppuwrite(ppufile:tcompilerppufile);
      var
        len : longint;
      begin
        inherited ppuwrite(ppufile);
        len:=strlen(str);
        ppufile.putlongint(len);
        ppufile.putdata(str^,len);
      end;


    function tai_comment.getcopy : tlinkedlistitem;
      var
        p : tlinkedlistitem;
      begin
        p:=inherited getcopy;
        getmem(tai_comment(p).str,strlen(str)+1);
        move(str^,tai_comment(p).str^,strlen(str)+1);
        getcopy:=p;
      end;


{****************************************************************************
                              TAI_CUT
 ****************************************************************************}

     constructor tai_cut.Create;
       begin
          inherited Create;
          typ:=ait_cut;
          place:=cut_normal;
       end;


     constructor tai_cut.Create_begin;
       begin
          inherited Create;
          typ:=ait_cut;
          place:=cut_begin;
       end;


     constructor tai_cut.Create_end;
       begin
          inherited Create;
          typ:=ait_cut;
          place:=cut_end;
       end;


    constructor tai_cut.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        place:=TCutPlace(ppufile.getbyte);
      end;


    procedure tai_cut.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(place));
      end;


{****************************************************************************
                             Tai_Marker
 ****************************************************************************}

    constructor Tai_Marker.Create(_Kind: TMarker);
      begin
        Inherited Create;
        typ := ait_marker;
        Kind := _Kind;
      end;


    constructor Tai_Marker.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        kind:=TMarker(ppufile.getbyte);
      end;


    procedure Tai_Marker.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(kind));
      end;


{*****************************************************************************
                                tai_tempalloc
*****************************************************************************}

    constructor tai_tempalloc.alloc(pos,size:longint);
      begin
        inherited Create;
        typ:=ait_tempalloc;
        allocation:=true;
        temppos:=pos;
        tempsize:=size;
{$ifdef EXTDEBUG}
        problem:=nil;
{$endif EXTDEBUG}
      end;


    destructor tai_tempalloc.destroy;
      begin
{$ifdef EXTDEBUG}
        stringdispose(problem);
{$endif EXTDEBUG}
        inherited destroy;
      end;


    constructor tai_tempalloc.dealloc(pos,size:longint);
      begin
        inherited Create;
        typ:=ait_tempalloc;
        allocation:=false;
        temppos:=pos;
        tempsize:=size;
{$ifdef EXTDEBUG}
        problem:=nil;
{$endif EXTDEBUG}
      end;


{$ifdef EXTDEBUG}
    constructor tai_tempalloc.allocinfo(pos,size:longint;const st:string);
      begin
        inherited Create;
        typ:=ait_tempalloc;
        allocation:=false;
        temppos:=pos;
        tempsize:=size;
        problem:=stringdup(st);
      end;
{$endif EXTDEBUG}


    constructor tai_tempalloc.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        temppos:=ppufile.getlongint;
        tempsize:=ppufile.getlongint;
        allocation:=boolean(ppufile.getbyte);
{$ifdef EXTDEBUG}
        problem:=nil;
{$endif EXTDEBUG}
      end;


    procedure tai_tempalloc.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putlongint(temppos);
        ppufile.putlongint(tempsize);
        ppufile.putbyte(byte(allocation));
      end;


{*****************************************************************************
                                 tai_regalloc
*****************************************************************************}

    constructor tai_regalloc.alloc(r : tregister);
      begin
        inherited create;
        typ:=ait_regalloc;
        allocation:=true;
        reg:=r;
      end;


    constructor tai_regalloc.dealloc(r : tregister);
      begin
        inherited create;
        typ:=ait_regalloc;
        allocation:=false;
        reg:=r;
      end;


    constructor tai_regalloc.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getdata(reg,sizeof(Tregister));
        allocation:=boolean(ppufile.getbyte);
      end;


    procedure tai_regalloc.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putdata(reg,sizeof(Tregister));
        ppufile.putbyte(byte(allocation));
      end;


{*****************************************************************************
                               TaiInstruction
*****************************************************************************}

    constructor taicpu_abstract.Create(op : tasmop);

      begin
         inherited create;
         typ:=ait_instruction;
         is_jmp:=false;
         opcode:=op;
         ops:=0;
         fillchar(condition,sizeof(condition),0);
         fillchar(oper,sizeof(oper),0);
      end;


    destructor taicpu_abstract.Destroy;

      var
        i : longint;
      begin
        for i:=0 to ops-1 do
        case oper[i].typ of
          top_ref:
            dispose(oper[i].ref);
{$ifdef ARM}
          top_shifterop:
            dispose(oper[i].shifterop);
{$endif ARM}
        end;
        inherited destroy;
      end;


{ ---------------------------------------------------------------------
    Loading of operands.
  ---------------------------------------------------------------------}

    procedure taicpu_abstract.loadconst(opidx:longint;l:aword);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ<>top_const then
             clearop(opidx);
           val:=l;
           typ:=top_const;
         end;
      end;


    procedure taicpu_abstract.loadsymbol(opidx:longint;s:tasmsymbol;sofs:longint);
      begin
        if not assigned(s) then
         internalerror(200204251);
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ<>top_symbol then
             clearop(opidx);
           sym:=s;
           symofs:=sofs;
           typ:=top_symbol;
         end;
        s.increfs;
      end;


    procedure taicpu_abstract.loadlocal(opidx:longint;s:pointer;sofs:longint);
      begin
        if not assigned(s) then
         internalerror(200204251);
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ<>top_local then
             clearop(opidx);
           localsym:=s;
           localsymofs:=sofs;
           typ:=top_local;
         end;
      end;


    procedure taicpu_abstract.loadref(opidx:longint;const r:treference);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
          begin
            if typ<>top_ref then
              begin
                clearop(opidx);
                new(ref);
              end;

            ref^:=r;
{$ifdef i386}
            { We allow this exception for i386, since overloading this would be
              too much of a a speed penalty}
            if (ref^.segment<>NR_NO) and (ref^.segment<>NR_DS) then
              segprefix:=ref^.segment;
{$endif}
            typ:=top_ref;
            { mark symbol as used }
            if assigned(ref^.symbol) then
              ref^.symbol.increfs;
          end;
      end;


    procedure taicpu_abstract.loadreg(opidx:longint;r:tregister);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ<>top_reg then
             clearop(opidx);
           reg:=r;
           typ:=top_reg;
         end;
      end;


    procedure taicpu_abstract.loadoper(opidx:longint;o:toper);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        clearop(opidx);
        oper[opidx]:=o;
        { copy also the reference }
        case oper[opidx].typ of
          top_ref:
            begin
              new(oper[opidx].ref);
              oper[opidx].ref^:=o.ref^;
            end;
{$ifdef ARM}
          top_shifterop:
            begin
              new(oper[opidx].shifterop);
              oper[opidx].shifterop^:=o.shifterop^;
            end;
{$endif ARM}
         end;
      end;


    procedure taicpu_abstract.clearop(opidx:longint);
      begin
        with oper[opidx] do
          case typ of
            top_ref:
              dispose(ref);
{$ifdef ARM}
            top_shifterop:
              dispose(shifterop);
{$endif ARM}
          end;
      end;


{ ---------------------------------------------------------------------
    Register allocator methods.
  ---------------------------------------------------------------------}

    function taicpu_abstract.get_insert_pos(p:Tai;huntfor1,huntfor2,huntfor3:Tsuperregister;var unusedregsint:Tsuperregisterset):Tai;
      var
        back   : Tsuperregisterset;
        supreg : tsuperregister;
      begin
        back:=unusedregsint;
        get_insert_pos:=p;
        while (p<>nil) and (p.typ=ait_regalloc) do
          begin
            supreg:=getsupreg(Tai_regalloc(p).reg);
            {Rewind the register allocation.}
            if Tai_regalloc(p).allocation then
              include(unusedregsint,supreg)
            else
              begin
                exclude(unusedregsint,supreg);
                if supreg=huntfor1 then
                  begin
                    get_insert_pos:=Tai(p.previous);
                    back:=unusedregsint;
                  end;
                if supreg=huntfor2 then
                  begin
                    get_insert_pos:=Tai(p.previous);
                    back:=unusedregsint;
                  end;
                if supreg=huntfor3 then
                  begin
                    get_insert_pos:=Tai(p.previous);
                    back:=unusedregsint;
                  end;
              end;
            p:=Tai(p.previous);
          end;
        unusedregsint:=back;
      end;


    procedure taicpu_abstract.forward_allocation(p:Tai;var unusedregsint:Tsuperregisterset);
      begin
        {Forward the register allocation again.}
        while (p<>self) do
          begin
            if p.typ<>ait_regalloc then
              internalerror(200305311);
            if Tai_regalloc(p).allocation then
              exclude(unusedregsint,getsupreg(Tai_regalloc(p).reg))
            else
              include(unusedregsint,getsupreg(Tai_regalloc(p).reg));
            p:=Tai(p.next);
          end;
      end;


    function taicpu_abstract.spill_registers(list:Taasmoutput;
                             rgget:Trggetproc;
                             rgunget:Trgungetproc;
                             r:Tsuperregisterset;
                             var unusedregsint:Tsuperregisterset;
                              const spilltemplist:Tspill_temp_list): boolean;
      var
        i:byte;
        supreg, reg1, reg2, reg3: Tsuperregister;
        helpreg:Tregister;
        helpins:Tai;
        op:Tasmop;
        pos:Tai;
        wasload: boolean;

      begin
        spill_registers:=false;
        if (ops = 2) and
           (oper[1].typ=top_ref) and
           { oper[1] can also be ref in case of "lis r3,symbol@ha" or so }
           spilling_decode_loadstore(opcode,op,wasload) then
          begin
            { the register that's being stored/loaded }
            supreg:=getsupreg(oper[0].reg);
            if supreg in r then
              begin
                // Example:
                //   l?? r20d, 8(r1)   ; r20d must be spilled into -60(r1)
                //
                //   Change into:
                //
                //   l?? r21d, 8(r1)
                //   st? r21d, -60(r1)
                //
                // And:
                //
                //   st? r20d, 8(r1)   ; r20d must be spilled into -60(r1)
                //
                //   Change into:
                //
                //   l?? r21d, -60(r1)
                //   st? r21d, 8(r1)

                pos := get_insert_pos(Tai(previous),supreg,
                                      getsupreg(oper[1].ref^.base),
                                      getsupreg(oper[1].ref^.index),
                                      unusedregsint);
                rgget(list,pos,R_SUBWHOLE,helpreg);
                spill_registers := true;
                if wasload then
                  begin
                    helpins:=spilling_create_loadstore(opcode,helpreg,oper[1].ref^);
                    loadref(1,spilltemplist[supreg]);
                    opcode := op;
                  end
                else
                  helpins:=spilling_create_loadstore(op,helpreg,spilltemplist[supreg]);
                if pos=nil then
                  list.insertafter(helpins,list.first)
                else
                  list.insertafter(helpins,pos.next);
                loadreg(0,helpreg);
                rgunget(list,helpins,helpreg);
                forward_allocation(tai(helpins.next),unusedregsint);
{
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
              end;

            { now the registers used in the reference }
            { a) base                                 }
            supreg := getsupreg(oper[1].ref^.base);
            if supreg in r then
              begin
                if wasload then
                  pos:=get_insert_pos(Tai(previous),getsupreg(oper[1].ref^.index),getsupreg(oper[0].reg),0,unusedregsint)
                else
                  pos:=get_insert_pos(Tai(previous),getsupreg(oper[1].ref^.index),0,0,unusedregsint);
                rgget(list,pos,R_SUBWHOLE,helpreg);
                spill_registers:=true;
                helpins:=spilling_create_load(spilltemplist[supreg],helpreg);
                if pos=nil then
                  list.insertafter(helpins,list.first)
                else
                  list.insertafter(helpins,pos.next);
                oper[1].ref^.base:=helpreg;
                rgunget(list,helpins,helpreg);
                forward_allocation(Tai(helpins.next),unusedregsint);
{
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
              end;

            { b) index }
            supreg := getsupreg(oper[1].ref^.index);
            if supreg in r then
              begin
                if wasload then
                  pos:=get_insert_pos(Tai(previous),getsupreg(oper[1].ref^.base),getsupreg(oper[0].reg),0,unusedregsint)
                else
                  pos:=get_insert_pos(Tai(previous),getsupreg(oper[1].ref^.base),0,0,unusedregsint);
                rgget(list,pos,R_SUBWHOLE,helpreg);
                spill_registers:=true;
                helpins:=spilling_create_load(spilltemplist[supreg],helpreg);
                if pos=nil then
                  list.insertafter(helpins,list.first)
                else
                  list.insertafter(helpins,pos.next);
                oper[1].ref^.index:=helpreg;
                rgunget(list,helpins,helpreg);
                forward_allocation(Tai(helpins.next),unusedregsint);
{
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
              end;
            { load/store is done }
            exit;
          end;

        { all other instructions the compiler generates are the same (I hope):   }
        { operand 0 is a register and is the destination, the others are sources }
        { and can be either registers or constants                               }
        { exception: branches (is_jmp isn't always set for them)                 }
        if oper[0].typ <> top_reg then
          exit;
        reg1 := getsupreg(oper[0].reg);
        if oper[1].typ = top_reg then
          reg2 := getsupreg(oper[1].reg)
        else
          reg2 := 0;
        if (ops >= 3) and
           (oper[2].typ = top_reg) then
          reg3 := getsupreg(oper[2].reg)
        else
          reg3 := 0;

        supreg:=reg1;
        if supreg in r then
          begin
            // Example:
            //   add r20d, r21d, r22d   ; r20d must be spilled into -60(r1)
            //
            //   Change into:
            //
            //   lwz r23d, -60(r1)
            //   add r23d, r21d, r22d
            //   stw r23d, -60(r1)

            pos := get_insert_pos(Tai(previous),reg1,reg2,reg3,unusedregsint);
            rgget(list,pos,R_SUBWHOLE,helpreg);
            spill_registers := true;
            helpins:=spilling_create_store(helpreg,spilltemplist[supreg]);
            list.insertafter(helpins,self);
            helpins:=spilling_create_load(spilltemplist[supreg],helpreg);
            if pos=nil then
              list.insertafter(helpins,list.first)
            else
              list.insertafter(helpins,pos.next);
            loadreg(0,helpreg);
            rgunget(list,helpins,helpreg);
            forward_allocation(tai(helpins.next),unusedregsint);
{
            writeln('spilling!');
            list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
          end;

        for i := 1 to 2 do
          if (oper[i].typ = top_reg) then
            begin
              supreg:=getsupreg(oper[i].reg);
              if supreg in r then
                begin
                  // Example:
                  //   add r20d, r21d, r22d   ; r20d must be spilled into -60(r1)
                  //
                  //   Change into:
                  //
                  //   lwz r23d, -60(r1)
                  //   add r23d, r21d, r22d
                  //   stw r23d, -60(r1)

                  pos := get_insert_pos(Tai(previous),reg1,reg2,reg3,unusedregsint);
                  rgget(list,pos,R_SUBWHOLE,helpreg);
                  spill_registers := true;
                  helpins:=spilling_create_load(spilltemplist[supreg],helpreg);
                  if pos=nil then
                    list.insertafter(helpins,list.first)
                  else
                    list.insertafter(helpins,pos.next);
                  loadreg(i,helpreg);
                  rgunget(list,helpins,helpreg);
                  forward_allocation(tai(helpins.next),unusedregsint);
{
                  writeln('spilling!');
                  list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
                end;
            end;
      end;


{ ---------------------------------------------------------------------
    Miscellaneous methods.
  ---------------------------------------------------------------------}

    procedure taicpu_abstract.SetCondition(const c:TAsmCond);
      begin
         condition:=c;
      end;


    Function taicpu_abstract.getcopy:TLinkedListItem;
      var
        i : longint;
        p : TLinkedListItem;
      begin
        p:=inherited getcopy;
        { make a copy of the references }
        for i:=1 to ops do
         if (taicpu_abstract(p).oper[i-1].typ=top_ref) then
          begin
            new(taicpu_abstract(p).oper[i-1].ref);
            taicpu_abstract(p).oper[i-1].ref^:=oper[i-1].ref^;
          end;
        getcopy:=p;
      end;


    constructor taicpu_abstract.ppuload(t:taitype;ppufile:tcompilerppufile);
      var
        i : integer;
      begin
        inherited ppuload(t,ppufile);
        { hopefully, we don't get problems with big/litte endian here when cross compiling :/ }
        ppufile.getdata(condition,sizeof(tasmcond));
        ops:=ppufile.getbyte;
        for i:=1 to ops do
          ppuloadoper(ppufile,oper[i-1]);
        opcode:=tasmop(ppufile.getword);
{$ifdef i386}
        ppufile.getdata(segprefix,sizeof(Tregister));
{$endif i386}
        is_jmp:=boolean(ppufile.getbyte);
      end;


    procedure taicpu_abstract.ppuwrite(ppufile:tcompilerppufile);
      var
        i : integer;
      begin
        inherited ppuwrite(ppufile);
        ppufile.putdata(condition,sizeof(tasmcond));
        ppufile.putbyte(ops);
        for i:=1 to ops do
          ppuwriteoper(ppufile,oper[i-1]);
        ppufile.putword(word(opcode));
{$ifdef i386}
        ppufile.putdata(segprefix,sizeof(Tregister));
{$endif i386}
        ppufile.putbyte(byte(is_jmp));
      end;


    procedure taicpu_abstract.derefimpl;

    var i:byte;

    begin
      for i:=1 to ops do
        ppuderefoper(oper[i-1]);
    end;

{****************************************************************************
                              tai_align_abstract
 ****************************************************************************}

     constructor tai_align_abstract.Create(b: byte);
       begin
          inherited Create;
          typ:=ait_align;
          if b in [1,2,4,8,16,32] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=0;
          use_op:=false;
       end;


     constructor tai_align_abstract.Create_op(b: byte; _op: byte);
       begin
          inherited Create;
          typ:=ait_align;
          if b in [1,2,4,8,16,32] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=_op;
          use_op:=true;
       end;


     function tai_align_abstract.calculatefillbuf(var buf : tfillbuffer):pchar;
       begin
         fillchar(buf,high(buf),fillop);
         calculatefillbuf:=pchar(@buf);
       end;


    constructor tai_align_abstract.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        aligntype:=ppufile.getbyte;
        fillsize:=0;
        fillop:=ppufile.getbyte;
        use_op:=boolean(ppufile.getbyte);
      end;


    procedure tai_align_abstract.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(aligntype);
        ppufile.putbyte(fillop);
        ppufile.putbyte(byte(use_op));
      end;


{*****************************************************************************
                                 TAAsmOutput
*****************************************************************************}

    constructor taasmoutput.create;
      begin
        inherited create;
        { make sure the optimizer won't remove the first tai of this list}
        insert(tai_marker.create(marker_blockstart));
      end;

    function taasmoutput.getlasttaifilepos : pfileposinfo;
      var
       hp : tlinkedlistitem;
      begin
         getlasttaifilepos := nil;
         if assigned(last) then
           begin
              { find the last file information record }
              if not (tai(last).typ in SkipLineInfo) then
                getlasttaifilepos:=@tailineinfo(last).fileinfo
              else
               { go through list backwards to find the first entry
                 with line information
               }
               begin
                 hp:=tai(last);
                 while assigned(hp) and (tai(hp).typ in SkipLineInfo) do
                    hp:=hp.Previous;
                 { found entry }
                 if assigned(hp) then
                   getlasttaifilepos:=@tailineinfo(hp).fileinfo
               end;
           end;
      end;

    procedure Taasmoutput.translate_registers(const table:Ttranstable);

    var p,q:Tai;
        i:shortint;
        r:Preference;
{$ifdef arm}
        so:pshifterop;
{$endif arm}


    begin
      p:=Tai(first);
      while assigned(p) do
        begin
          case p.typ of
            ait_regalloc:
              if (getregtype(Tai_regalloc(p).reg)=R_INTREGISTER) then
                setsupreg(Tai_regalloc(p).reg,table[getsupreg(Tai_regalloc(p).reg)]);
            ait_instruction:
              begin
                for i:=0 to Taicpu_abstract(p).ops-1 do
                  case Taicpu_abstract(p).oper[i].typ of
                    Top_reg:
                       if (getregtype(Taicpu_abstract(p).oper[i].reg)=R_INTREGISTER) then
                         setsupreg(Taicpu_abstract(p).oper[i].reg,table[getsupreg(Taicpu_abstract(p).oper[i].reg)]);
                    Top_ref:
                      begin
                        r:=Taicpu_abstract(p).oper[i].ref;
                        if r^.base<>NR_NO then
                          setsupreg(r^.base,table[getsupreg(r^.base)]);
                        if r^.index<>NR_NO then
                          setsupreg(r^.index,table[getsupreg(r^.index)]);
                      end;
{$ifdef arm}
                    Top_shifterop:
                      begin
                        so:=Taicpu_abstract(p).oper[i].shifterop;
                        if so^.rs<>NR_NO then
                          setsupreg(so^.rs,table[getsupreg(so^.rs)]);
                      end;
{$endif arm}
                  end;

                { Maybe the operation can be removed when
                  it is a move and both arguments are the same }
                if Taicpu_abstract(p).is_nop then
                  begin
                    q:=p;
                    p:=Tai(p.next);
                    remove(q);
                    continue;
                  end;
              end;
          end;
          p:=Tai(p.next);
        end;
    end;

end.
{
  $Log$
  Revision 1.40  2003-09-23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.39  2003/09/07 22:09:34  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.38  2003/09/04 00:15:28  florian
    * first bunch of adaptions of arm compiler for new register type

  Revision 1.37  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.36  2003/09/03 11:18:36  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.35.2.5  2003/08/31 21:08:16  peter
    * first batch of sparc fixes

  Revision 1.35.2.4  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.35.2.3  2003/08/28 18:35:07  peter
    * tregister changed to cardinal

  Revision 1.35.2.2  2003/08/27 20:23:55  peter
    * remove old ra code

  Revision 1.35.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.35  2003/08/21 14:47:41  peter
    * remove convert_registers

  Revision 1.34  2003/08/20 20:29:06  daniel
    * Some more R_NO changes
    * Preventive code to loadref added

  Revision 1.33  2003/08/17 20:47:47  daniel
    * Notranslation changed into -sr functionality

  Revision 1.32  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.31  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.30  2003/07/02 16:43:48  jonas
    * always add dummy marker object at the start of an assembler list, so
      the optimizer can't remove the first object

  Revision 1.29  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.28  2003/05/12 18:13:57  peter
    * create rtti label using newasmsymboldata and update binding
      only when calling tai_symbol.create
    * tai_symbol.create_global added

  Revision 1.27  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.26  2002/04/25 16:12:09  florian
    * fixed more problems with cpubase and x86-64

  Revision 1.25  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.24  2003/04/24 13:03:01  florian
    * comp is now written with its bit pattern to the ppu instead as an extended

  Revision 1.23  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.22  2003/04/22 10:09:34  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.21  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.20  2003/01/30 21:46:20  peter
    * tai_const_symbol.createdataname added

  Revision 1.19  2003/01/21 08:48:08  daniel
    * Another 200301081 fixed

  Revision 1.18  2003/01/09 20:40:59  daniel
    * Converted some code in cgx86.pas to new register numbering

  Revision 1.17  2003/01/09 15:49:56  daniel
    * Added register conversion

  Revision 1.16  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.15  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.14  2002/12/06 17:50:21  peter
    * symbol count fix merged

  Revision 1.13  2002/11/17 16:31:55  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.12  2002/11/15 16:29:30  peter
    * made tasmsymbol.refs private (merged)

  Revision 1.11  2002/11/15 01:58:45  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.10  2002/11/09 15:38:03  carl
    + NOOPT removed the optinfo field

  Revision 1.9  2002/10/05 12:43:23  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.8  2002/08/19 19:36:42  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.7  2002/08/18 20:06:23  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.6  2002/08/16 05:21:09  florian
    * powerpc compilation fix

  Revision 1.5  2002/08/15 19:10:35  peter
    * first things tai,tnode storing in ppu

  Revision 1.4  2002/08/11 14:32:25  peter
    * renamed current_library to objectlibrary

  Revision 1.3  2002/08/11 13:24:10  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.2  2002/08/05 18:27:48  carl
    + more more more documentation
    + first version include/exclude (can't test though, not enough scratch for i386 :()...

  Revision 1.1  2002/07/01 18:46:20  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.27  2002/05/18 13:34:04  peter
    * readded missing revisions

  Revision 1.25  2002/05/14 19:34:38  peter
    * removed old logs and updated copyright year

  Revision 1.24  2002/05/14 17:28:08  peter
    * synchronized cpubase between powerpc and i386
    * moved more tables from cpubase to cpuasm
    * tai_align_abstract moved to tainst, cpuasm must define
      the tai_align class now, which may be empty

  Revision 1.23  2002/04/15 18:54:34  carl
  - removed tcpuflags

  Revision 1.22  2002/04/07 13:18:19  carl
  + more documentation

  Revision 1.21  2002/04/07 10:17:40  carl
  - remove packenumfixed (requires version 1.0.2 or later to compile now!)
  + changing some comments so its commented automatically

  Revision 1.20  2002/03/24 19:04:31  carl
  + patch for SPARC from Mazen NEIFER

}
