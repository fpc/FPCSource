{
    Copyright (c) 1998-2006 by Florian Klaempfl

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
       globtype,systems,
       cpuinfo,cpubase,
       cgbase,cgutils,
       symtype,
       aasmbase,aasmdata,ogbase;

    type
       { keep the number of elements in this enumeration less or equal than 32 as long
         as FPC knows only 4 byte and 32 byte sets (FK) }
       taitype = (
          ait_none,
          ait_align,
          ait_section,
          ait_comment,
          ait_string,
          ait_instruction,
          ait_datablock,
          ait_symbol,
          { needed to calc the size of a symbol }
          ait_symbol_end,
          ait_directive,
          ait_label,
          ait_const,
          ait_real_32bit,
          ait_real_64bit,
          ait_real_80bit,
          ait_comp_64bit,
          ait_real_128bit,
          ait_stab,
          ait_force_line,
          ait_function_name,
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
{$ifdef arm}
          ait_thumb_func,
{$endif arm}
          { used to split into tiny assembler files }
          ait_cutobject,
          ait_regalloc,
          ait_tempalloc,
          { used to mark assembler blocks and inlined functions }
          ait_marker
          );

        taiconst_type = (
          aitconst_128bit,
          aitconst_64bit,
          aitconst_32bit,
          aitconst_16bit,
          aitconst_8bit,
          aitconst_sleb128bit,
          aitconst_uleb128bit,
          { win32 only }
          aitconst_rva_symbol,
          aitconst_secrel32_symbol,
          { darwin only }
          aitconst_indirect_symbol,
          { From gcc/config/darwin.c (darwin_asm_output_dwarf_delta):
            ***
            Output a difference of two labels that will be an assembly time
            constant if the two labels are local.  (.long lab1-lab2 will be
            very different if lab1 is at the boundary between two sections; it
            will be relocated according to the second section, not the first,
            so one ends up with a difference between labels in different
            sections, which is bad in the dwarf2 eh context for instance.)
            ***
            We cannot use this everywhere, because older versions of the
            darwin assembler don't support the construct used for these
            relsyms (nor do they support dwarf, for that matter)
          }
          aitconst_darwin_dwarf_delta64,
          aitconst_darwin_dwarf_delta32
        );

    const
{$ifdef cpu64bitaddr}
       aitconst_ptr = aitconst_64bit;
{$else cpu64bitaddr}
       aitconst_ptr = aitconst_32bit;
{$endif cpu64bitaddr}

{$ifdef cpu64bitalu}
       aitconst_aint = aitconst_64bit;
{$else cpu64bitaddr}
       aitconst_aint = aitconst_32bit;
{$endif cpu64bitaddr}

       taitypestr : array[taitype] of string[24] = (
          '<none>',
          'align',
          'section',
          'comment',
          'string',
          'instruction',
          'datablock',
          'symbol',
          'symbol_end',
          'symbol_directive',
          'label',
          'const',
          'real_32bit',
          'real_64bit',
          'real_80bit',
          'comp_64bit',
          'real_128bit',
          'stab',
          'force_line',
          'function_name',
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
{$ifdef arm}
          'thumb_func',
{$endif arm}
          'cut',
          'regalloc',
          'tempalloc',
          'marker'
          );

    type
      { Types of operand }
      toptype=(top_none,top_reg,top_ref,top_const,top_bool,top_local
{$ifdef arm}
       { ARM only }
       ,top_regset
       ,top_shifterop
       ,top_conditioncode
{$endif arm}
{$ifdef m68k}
       { m68k only }
       ,top_regset
{$endif m68k}
       { i386 only});

      { kinds of operations that an instruction can perform on an operand }
      topertype = (operand_read,operand_write,operand_readwrite);

      tlocaloper = record
        localsym : pointer;
        localsymderef : tderef;
        localsymofs : longint;
        localindexreg : tregister;
        localscale : byte;
        localgetoffset,
        localforceref : boolean
      end;
      plocaloper = ^tlocaloper;

      { please keep the size of this record <=12 bytes and keep it properly aligned }
      toper = record
        ot : longint;
        case typ : toptype of
          top_none   : ();
          top_reg    : (reg:tregister);
          top_ref    : (ref:preference);
          top_const  : (val:aint);
          top_bool   : (b:boolean);
          { local varsym that will be inserted in pass_generate_code }
          top_local  : (localoper:plocaloper);
      {$ifdef arm}
          top_regset : (regset:^tcpuregisterset);
          top_shifterop : (shifterop : pshifterop);
          top_conditioncode: (cc: TAsmCond);
      {$endif arm}
      {$ifdef m68k}
          top_regset : (regset:^tcpuregisterset);
      {$endif m68k}
      end;
      poper=^toper;

    const
      { ait_* types which don't result in executable code or which don't influence
        the way the program runs/behaves, but which may be encountered by the
        optimizer (= if it's sometimes added to the exprasm list). Update if you add
        a new ait type!                                                              }
      SkipInstr = [ait_comment, ait_symbol,ait_section
                   ,ait_stab, ait_function_name, ait_force_line
                   ,ait_regalloc, ait_tempalloc, ait_symbol_end, ait_directive];

      { ait_* types which do not have line information (and hence which are of type
        tai, otherwise, they are of type tailineinfo }
      SkipLineInfo =[ait_label,
                     ait_regalloc,ait_tempalloc,
                     ait_stab,ait_function_name,
                     ait_cutobject,ait_marker,ait_align,ait_section,ait_comment,
                     ait_const,
{$ifdef arm}
                     ait_thumb_func,
{$endif arm}
                     ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_real_128bit,
                     ait_symbol
                    ];


    type
      { cut type, required for alphanumeric ordering of the assembler filenames }
      TCutPlace=(cut_normal,cut_begin,cut_end);

      TAsmMarker = (
        mark_NoPropInfoStart,mark_NoPropInfoEnd,
        mark_AsmBlockStart,mark_AsmBlockEnd,
        mark_InlineStart,mark_InlineEnd,mark_BlockStart,
        mark_Position
      );

      TRegAllocType = (ra_alloc,ra_dealloc,ra_sync,ra_resize);

      TStabType = (stab_stabs,stab_stabn,stab_stabd);

      TAsmDirective=(
        asd_non_lazy_symbol_pointer,asd_indirect_symbol,asd_lazy_symbol_pointer,
        asd_extern,asd_nasm_import, asd_toc_entry, asd_mod_init_func, asd_mod_term_func,
        asd_reference,asd_no_dead_strip,asd_weak_reference,asd_lazy_reference,
        asd_weak_definition
      );

    const
      regallocstr : array[tregalloctype] of string[10]=('allocated','released','sync','resized');
      tempallocstr : array[boolean] of string[10]=('released','allocated');
      stabtypestr : array[TStabType] of string[5]=('stabs','stabn','stabd');
      directivestr : array[TAsmDirective] of string[23]=(
        'non_lazy_symbol_pointer','indirect_symbol','lazy_symbol_pointer',
        'extern','nasm_import', 'tc', 'mod_init_func', 'mod_term_func', 'reference',
        'no_dead_strip','weak_reference','lazy_reference','weak_definition'
      );

    type
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
          procedure buildderefimpl;virtual;
          procedure derefimpl;virtual;
       end;

       { abstract assembler item with line information }
       tailineinfo = class(tai)
        fileinfo : tfileposinfo;
        constructor Create;
        constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
        procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tai_simple = class(tai)
         constructor create(_typ : taitype);
       end;

       taiclass = class of tai;

       taiclassarray = array[taitype] of taiclass;

       { Generates an assembler string }
       tai_string = class(tailineinfo)
          str : pchar;
          { extra len so the string can contain an \0 }
          len : longint;
          constructor Create(const _str : string);
          constructor Create_pchar(_str : pchar;length : longint);
          destructor Destroy;override;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function getcopy:tlinkedlistitem;override;
       end;

       { Generates a common label }
       tai_symbol = class(tai)
          sym       : tasmsymbol;
          value     : puint;
          size      : longint;
          is_global,
          has_value : boolean;
          constructor Create(_sym:tasmsymbol;siz:longint);
          constructor Create_Global(_sym:tasmsymbol;siz:longint);
          constructor Createname(const _name : string;_symtyp:Tasmsymtype;siz:longint);
          constructor Createname_global(const _name : string;_symtyp:Tasmsymtype;siz:longint);
          constructor Createname_global_value(const _name : string;_symtyp:Tasmsymtype;siz:longint;val:ptruint);
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

       tai_directive = class(tailineinfo)
          name : pshortstring;
          directive : TAsmDirective;
          constructor Create(_directive:TAsmDirective;const _name:string);
          destructor Destroy;override;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       { Generates an assembler label }
       tai_label = class(tai)
          is_global : boolean;
          labsym    : tasmlabel;
          constructor Create(_labsym : tasmlabel);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
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
          sectype  : TAsmSectiontype;
          secorder : TasmSectionorder;
          secalign : byte;
          name     : pshortstring;
          sec      : TObjSection; { used in binary writer }
          constructor Create(Asectype:TAsmSectiontype;Aname:string;Aalign:byte;Asecorder:TasmSectionorder=secorder_default);
          destructor Destroy;override;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;


       { Generates an uninitializised data block }
       tai_datablock = class(tailineinfo)
          is_global : boolean;
          sym       : tasmsymbol;
          size      : aint;
          constructor Create(const _name : string;_size : aint);
          constructor Create_global(const _name : string;_size : aint);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
       end;


       { Generates an integer const }
       tai_const = class(tai)
          sym,
          endsym  : tasmsymbol;
          { if symbols and offset are provided the symofs is used,
            the value is calculated during assembling }
          symofs,
          value   : int64;
          consttype : taiconst_type;
          { we use for the 128bit int64/qword for now because I can't imagine a
            case where we need 128 bit now (FK) }
          constructor Create(_typ:taiconst_type;_value : int64);
          constructor Create_128bit(_value : int64);
          constructor Create_64bit(_value : int64);
          constructor Create_32bit(_value : longint);
          constructor Create_16bit(_value : word);
          constructor Create_8bit(_value : byte);
          constructor Create_sleb128bit(_value : int64);
          constructor Create_uleb128bit(_value : qword);
          constructor Create_aint(_value : aint);
          constructor Create_pint(_value : pint);
          constructor Create_sym(_sym:tasmsymbol);
          constructor Create_type_sym(_typ:taiconst_type;_sym:tasmsymbol);
          constructor Create_sym_offset(_sym:tasmsymbol;ofs:aint);
          constructor Create_rel_sym(_typ:taiconst_type;_sym,_endsym:tasmsymbol);
          constructor Create_rva_sym(_sym:tasmsymbol);
          constructor Create_indirect_sym(_sym:tasmsymbol);
          constructor Createname(const name:string;ofs:aint);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy:tlinkedlistitem;override;
          function size:longint;
       end;

       { Generates a single float (32 bit real) }
       tai_real_32bit = class(tai)
          value : ts32real;
          constructor Create(_value : ts32real);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tformatoptions = (fo_none,fo_hiloswapped);

       { Generates a double float (64 bit real) }
       tai_real_64bit = class(tai)
          value : ts64real;
{$ifdef ARM}
          formatoptions : tformatoptions;
          constructor Create_hiloswapped(_value : ts64real);
{$endif ARM}
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


       { Generates an float128 (128 bit real) }
       tai_real_128bit = class(tai)
          value : ts128real;
          constructor Create(_value : ts128real);
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

       tai_stab = class(tai)
          str : pchar;
          stabtype : TStabType;
          constructor Create(_stabtype:TStabType;_str : pchar);
          constructor Create_str(_stabtype:TStabType;const s:string);
          destructor Destroy;override;
       end;

       tai_force_line = class(tailineinfo)
          constructor Create;
       end;

       tai_function_name = class(tai)
          funcname : pshortstring;
          constructor create(const s:string);
          destructor destroy;override;
       end;

       { Insert a cut to split assembler into several smaller files }
       tai_cutobject = class(tai)
          place : tcutplace;
          constructor Create;
          constructor Create_begin;
          constructor Create_end;
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       { Insert a marker for assembler and inline blocks }
       tai_marker = class(tai)
          Kind: TAsmMarker;
          Constructor Create(_Kind: TAsmMarker);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tai_tempalloc = class(tai)
          allocation : boolean;
{$ifdef EXTDEBUG}
          problem : pshortstring;
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
          reg     : tregister;
          ratype  : TRegAllocType;
          { reg(de)alloc belongs to this instruction, this
            is only used for automatic inserted (de)alloc for
            imaginary register and required for spilling code }
          instr   : tai;
          constructor alloc(r : tregister;ainstr:tai);
          constructor dealloc(r : tregister;ainstr:tai);
          constructor sync(r : tregister);
          constructor resize(r : tregister);
          constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tadd_reg_instruction_proc=procedure(instr:Tai;r:tregister) of object;

        { Class template for assembler instructions
        }
        tai_cpu_abstract = class(tailineinfo)
        protected
           procedure ppuloadoper(ppufile:tcompilerppufile;var o:toper);virtual;
           procedure ppuwriteoper(ppufile:tcompilerppufile;const o:toper);virtual;
           procedure ppubuildderefimploper(var o:toper);virtual;abstract;
           procedure ppuderefoper(var o:toper);virtual;abstract;
        public
           { Condition flags for instruction }
           condition : TAsmCond;
           { Number of operands to instruction }
           ops       : byte;
           { Number of allocate oper structures }
           opercnt   : byte;
           { Operands of instruction }
           oper      : array[0..max_operands-1] of poper;
           { Actual opcode of instruction }
           opcode    : tasmop;
{$ifdef x86}
           segprefix : tregister;
{$endif x86}
           { true if instruction is a jmp }
           is_jmp    : boolean; { is this instruction a jump? (needed for optimizer) }
           Constructor Create(op : tasmop);virtual;
           Destructor Destroy;override;
           function getcopy:TLinkedListItem;override;
           constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
           procedure ppuwrite(ppufile:tcompilerppufile);override;
           procedure buildderefimpl;override;
           procedure derefimpl;override;
           procedure SetCondition(const c:TAsmCond);
           procedure allocate_oper(opers:longint);
           procedure loadconst(opidx:longint;l:aint);
           procedure loadsymbol(opidx:longint;s:tasmsymbol;sofs:longint);
           procedure loadlocal(opidx:longint;s:pointer;sofs:longint;indexreg:tregister;scale:byte;getoffset,forceref:boolean);
           procedure loadref(opidx:longint;const r:treference);
           procedure loadreg(opidx:longint;r:tregister);
           procedure loadoper(opidx:longint;o:toper);
           procedure clearop(opidx:longint);
           procedure freeop(opidx:longint);
           { register allocator }
           function is_same_reg_move(regtype: Tregistertype):boolean;virtual;
           function spilling_get_operation_type(opnr: longint): topertype;virtual;
           function spilling_get_operation_type_ref(opnr: longint; reg: tregister): topertype;virtual;

           function  Pass1(objdata:TObjData):longint;virtual;
           procedure Pass2(objdata:TObjData);virtual;

           procedure resetpass1; virtual;
           procedure resetpass2; virtual;
        end;
        tai_cpu_class = class of tai_cpu_abstract;

        { Buffer type used for alignment }
        tfillbuffer = array[0..63] of char;

        { alignment for operator }
        tai_align_abstract = class(tai)
           aligntype : byte;   { 1 = no align, 2 = word align, 4 = dword align }
           fillsize  : byte;   { real size to fill }
           fillop    : byte;   { value to fill with - optional }
           use_op    : boolean;
           constructor Create(b:byte);virtual;
           constructor Create_op(b: byte; _op: byte);virtual;
           constructor Create_zeros(b:byte);
           constructor ppuload(t:taitype;ppufile:tcompilerppufile);override;
           procedure ppuwrite(ppufile:tcompilerppufile);override;
           function calculatefillbuf(var buf : tfillbuffer):pchar;virtual;
        end;
        tai_align_class = class of tai_align_abstract;

    var
      { array with all class types for tais }
      aiclass : taiclassarray;

      { target specific tais }
      cai_align : tai_align_class;
      cai_cpu   : tai_cpu_class;

      { hook to notify uses of registers }
      add_reg_instruction_hook : tadd_reg_instruction_proc;

    procedure maybe_new_object_file(list:TAsmList);
    procedure new_section(list:TAsmList;Asectype:TAsmSectiontype;Aname:string;Aalign:byte;Asecorder:TasmSectionorder=secorder_default);
    procedure section_symbol_start(list:TAsmList;const Aname:string;Asymtyp:Tasmsymtype;
                                   Aglobal:boolean;Asectype:TAsmSectiontype;Aalign:byte);
    procedure section_symbol_end(list:TAsmList;const Aname:string);

    function ppuloadai(ppufile:tcompilerppufile):tai;
    procedure ppuwriteai(ppufile:tcompilerppufile;n:tai);


implementation

    uses
      SysUtils,
      verbose,
      globals,
      fmodule;

    const
      pputaimarker = 254;


{****************************************************************************
                                 Helpers
 ****************************************************************************}

    procedure maybe_new_object_file(list:TAsmList);
      begin
        if create_smartlink_library then
          list.concat(tai_cutobject.create);
      end;


    procedure new_section(list:TAsmList;Asectype:TAsmSectiontype;Aname:string;Aalign:byte;Asecorder:TasmSectionorder=secorder_default);
      begin
        list.concat(tai_section.create(Asectype,Aname,Aalign,Asecorder));
        list.concat(cai_align.create(Aalign));
      end;


    procedure section_symbol_start(list:TAsmList;const Aname:string;Asymtyp:Tasmsymtype;
                                   Aglobal:boolean;Asectype:TAsmSectiontype;Aalign:byte);
      begin
        maybe_new_object_file(list);
        list.concat(tai_section.create(Asectype,Aname,Aalign));
        list.concat(cai_align.create(Aalign));
        if Aglobal or
           create_smartlink then
          list.concat(tai_symbol.createname_global(Aname,Asymtyp,0))
        else
          list.concat(tai_symbol.createname(Aname,Asymtyp,0));
      end;


    procedure section_symbol_end(list:TAsmList;const Aname:string);
      begin
        list.concat(tai_symbol_end.createname(Aname));
      end;


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


    procedure tai.buildderefimpl;
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
       fileinfo:=current_filepos;
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
                              TAI_SIMPLE
 ****************************************************************************}

    constructor tai_simple.create(_typ : taitype);
      begin
        inherited create;
        typ:=_typ;
      end;


{****************************************************************************
                             TAI_SECTION
 ****************************************************************************}

    constructor tai_section.Create(Asectype:TAsmSectiontype;Aname:string;Aalign:byte;Asecorder:TasmSectionorder=secorder_default);
      begin
        inherited Create;
        typ:=ait_section;
        sectype:=asectype;
        secalign:=Aalign;
        secorder:=Asecorder;
        name:=stringdup(Aname);
        sec:=nil;
      end;


    constructor tai_section.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        sectype:=TAsmSectiontype(ppufile.getbyte);
        secalign:=ppufile.getbyte;
        name:=stringdup(ppufile.getstring);
        sec:=nil;
      end;


    destructor tai_section.Destroy;
      begin
        stringdispose(name);
      end;


    procedure tai_section.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(sectype));
        ppufile.putbyte(secalign);
        ppufile.putstring(name^);
      end;


{****************************************************************************
                             TAI_DATABLOCK
 ****************************************************************************}

    constructor tai_datablock.Create(const _name : string;_size : aint);

      begin
         inherited Create;
         typ:=ait_datablock;
         sym:=current_asmdata.DefineAsmSymbol(_name,AB_LOCAL,AT_DATA);
         { keep things aligned }
         if _size<=0 then
           _size:=sizeof(aint);
         size:=_size;
         is_global:=false;
      end;


    constructor tai_datablock.Create_global(const _name : string;_size : aint);
      begin
         inherited Create;
         typ:=ait_datablock;
         sym:=current_asmdata.DefineAsmSymbol(_name,AB_GLOBAL,AT_DATA);
         { keep things aligned }
         if _size<=0 then
           _size:=sizeof(aint);
         size:=_size;
         is_global:=true;
      end;


    constructor tai_datablock.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited Create;
        sym:=ppufile.getasmsymbol;
        size:=ppufile.getaint;
        is_global:=boolean(ppufile.getbyte);
      end;


    procedure tai_datablock.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putasmsymbol(sym);
        ppufile.putaint(size);
        ppufile.putbyte(byte(is_global));
      end;


    procedure tai_datablock.derefimpl;
      begin
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
         sym.bind:=AB_LOCAL;
         is_global:=false;
      end;


    constructor tai_symbol.Create_global(_sym:tasmsymbol;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=_sym;
         size:=siz;
         { don't override PRIVATE_EXTERN with GLOBAL }
         if not(sym.bind in [AB_GLOBAL,AB_PRIVATE_EXTERN]) then
           sym.bind:=AB_GLOBAL;
         is_global:=true;
      end;


    constructor tai_symbol.Createname(const _name : string;_symtyp:Tasmsymtype;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=current_asmdata.DefineAsmSymbol(_name,AB_LOCAL,_symtyp);
         size:=siz;
         is_global:=false;
      end;


    constructor tai_symbol.Createname_global(const _name : string;_symtyp:Tasmsymtype;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=current_asmdata.DefineAsmSymbol(_name,AB_GLOBAL,_symtyp);
         size:=siz;
         is_global:=true;
      end;


    constructor tai_symbol.createname_global_value(const _name: string;_symtyp: tasmsymtype; siz: longint; val: ptruint);
      begin
        Createname_global(_name,_symtyp,siz);
        value:=val;
        has_value:=true;
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
      end;


{****************************************************************************
                               TAI_SYMBOL_END
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
         sym:=current_asmdata.RefAsmSymbol(_name);
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
      end;


{****************************************************************************
                               TAI_SYMBOL_END
 ****************************************************************************}

    constructor tai_directive.Create(_directive:TAsmDirective;const _name:string);
      begin
         inherited Create;
         typ:=ait_directive;
         name:=stringdup(_name);
         directive:=_directive;
      end;


    destructor tai_directive.Destroy;
      begin
        stringdispose(name);
      end;


    constructor tai_directive.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        name:=stringdup(ppufile.getstring);
        directive:=TAsmDirective(ppufile.getbyte);
      end;


    procedure tai_directive.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putstring(name^);
        ppufile.putbyte(byte(directive));
      end;


{****************************************************************************
                               TAI_CONST
 ****************************************************************************}

    constructor tai_const.Create(_typ:taiconst_type;_value : int64);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=_typ;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_128bit(_value : int64);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_128bit;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_64bit(_value : int64);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_64bit;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_32bit(_value : longint);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_32bit;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_16bit(_value : word);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_16bit;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_8bit(_value : byte);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_8bit;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_sleb128bit(_value : int64);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_sleb128bit;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_uleb128bit(_value : qword);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_uleb128bit;
         value:=int64(_value);
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_aint(_value : aint);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_aint;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_pint(_value : pint);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_ptr;
         value:=_value;
         sym:=nil;
         endsym:=nil;
      end;


    constructor tai_const.Create_type_sym(_typ:taiconst_type;_sym:tasmsymbol);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=_typ;
         sym:=_sym;
         endsym:=nil;
         value:=0;
         { update sym info }
         if assigned(sym) then
           sym.increfs;
      end;


    constructor tai_const.Create_sym(_sym:tasmsymbol);
      begin
         self.create_sym_offset(_sym,0);
      end;


    constructor tai_const.Create_sym_offset(_sym:tasmsymbol;ofs:aint);
      begin
         inherited Create;
         typ:=ait_const;
         consttype:=aitconst_ptr;
         { sym is allowed to be nil, this is used to write nil pointers }
         sym:=_sym;
         endsym:=nil;
         { store the original offset in symofs so that we can recalculate the
           value field in the assembler }
         symofs:=ofs;
         value:=ofs;
         { update sym info }
         if assigned(sym) then
           sym.increfs;
      end;


    constructor tai_const.Create_rel_sym(_typ:taiconst_type;_sym,_endsym:tasmsymbol);
      begin
         self.create_sym_offset(_sym,0);
         consttype:=_typ;
         endsym:=_endsym;
         endsym.increfs;
      end;


    constructor tai_const.Create_rva_sym(_sym:tasmsymbol);
      begin
         self.create_sym_offset(_sym,0);
         consttype:=aitconst_rva_symbol;
      end;


    constructor tai_const.Create_indirect_sym(_sym:tasmsymbol);
      begin
         self.create_sym_offset(_sym,0);
         consttype:=aitconst_indirect_symbol;
      end;


    constructor tai_const.Createname(const name:string;ofs:aint);
      begin
         self.create_sym_offset(current_asmdata.RefAsmSymbol(name),ofs);
      end;


    constructor tai_const.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        consttype:=taiconst_type(ppufile.getbyte);
        sym:=ppufile.getasmsymbol;
        endsym:=ppufile.getasmsymbol;
        value:=ppufile.getint64;
      end;


    procedure tai_const.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(consttype));
        ppufile.putasmsymbol(sym);
        ppufile.putasmsymbol(endsym);
        ppufile.putint64(value);
      end;


    procedure tai_const.derefimpl;
      begin
      end;


    function tai_const.getcopy:tlinkedlistitem;
      begin
        getcopy:=inherited getcopy;
        { we need to increase the reference number }
        sym.increfs;
        if assigned(endsym) then
          endsym.increfs;
      end;


    function tai_const.size:longint;
      begin
        case consttype of
          aitconst_8bit :
            result:=1;
          aitconst_16bit :
            result:=2;
          aitconst_32bit :
            result:=4;
          aitconst_64bit :
            result:=8;
          aitconst_indirect_symbol,
          aitconst_secrel32_symbol,
          aitconst_rva_symbol :
            if target_info.system=system_x86_64_win64 then
              result:=sizeof(longint)
            else
              result:=sizeof(pint);
          aitconst_uleb128bit :
            result:=LengthUleb128(qword(value));
          aitconst_sleb128bit :
            result:=LengthSleb128(value);
          else
            internalerror(200603253);
        end;
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


{$ifdef ARM}
    constructor tai_real_64bit.Create_hiloswapped(_value : ts64real);

      begin
         inherited Create;
         typ:=ait_real_64bit;
         value:=_value;
         formatoptions:=fo_hiloswapped;
      end;
{$endif ARM}

    constructor tai_real_64bit.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        value:=ppufile.getreal;
{$ifdef ARM}
        formatoptions:=tformatoptions(ppufile.getbyte);
{$endif ARM}
      end;


    procedure tai_real_64bit.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putreal(value);
{$ifdef ARM}
        ppufile.putbyte(byte(formatoptions));
{$endif ARM}
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
                               TAI_real_80bit
 ****************************************************************************}

    constructor tai_real_128bit.Create(_value : ts128real);

      begin
         inherited Create;
         typ:=ait_real_128bit;
         value:=_value;
      end;


    constructor tai_real_128bit.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        value:=ppufile.getreal;
      end;


    procedure tai_real_128bit.ppuwrite(ppufile:tcompilerppufile);
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


    constructor tai_string.Create_pchar(_str : pchar;length : longint);
       begin
          inherited Create;
          typ:=ait_string;
          str:=_str;
          len:=length;
       end;


    destructor tai_string.destroy;
      begin
         if str<>nil then
           freemem(str);
         inherited Destroy;
      end;


    constructor tai_string.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        len:=ppufile.getlongint;
        getmem(str,len);
        ppufile.getdata(str^,len);
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
        getmem(tai_string(p).str,len);
        move(str^,tai_string(p).str^,len);
        getcopy:=p;
      end;


{****************************************************************************
                               TAI_LABEL
 ****************************************************************************}

    constructor tai_label.create(_labsym : tasmlabel);
      begin
        inherited Create;
        typ:=ait_label;
        labsym:=_labsym;
        labsym.is_set:=true;
        is_global:=(labsym.bind in [AB_GLOBAL,AB_PRIVATE_EXTERN]);
      end;


    constructor tai_label.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        labsym:=tasmlabel(ppufile.getasmsymbol);
        is_global:=boolean(ppufile.getbyte);
      end;


    procedure tai_label.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putasmsymbol(labsym);
        ppufile.putbyte(byte(is_global));
      end;


    procedure tai_label.derefimpl;
      begin
        labsym.is_set:=true;
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
         freemem(str);
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
                              TAI_STABS
 ****************************************************************************}

    constructor tai_stab.create(_stabtype:TStabType;_str : pchar);
      begin
         inherited create;
         typ:=ait_stab;
         str:=_str;
         stabtype:=_stabtype;
      end;

    constructor tai_stab.create_str(_stabtype:TStabType;const s:string);
      begin
         self.create(_stabtype,strpnew(s));
      end;

    destructor tai_stab.destroy;
      begin
         freemem(str);
         inherited destroy;
      end;


{****************************************************************************
                            TAI_FORCE_LINE
 ****************************************************************************}

    constructor tai_force_line.create;
      begin
         inherited create;
         typ:=ait_force_line;
      end;


{****************************************************************************
                              TAI_FUNCTION_NAME
 ****************************************************************************}

    constructor tai_function_name.create(const s:string);
      begin
         inherited create;
         typ:=ait_function_name;
         funcname:=stringdup(s);
      end;

    destructor tai_function_name.destroy;
      begin
         stringdispose(funcname);
         inherited destroy;
      end;


{****************************************************************************
                              TAI_CUTOBJECT
 ****************************************************************************}

     constructor tai_cutobject.Create;
       begin
          inherited Create;
          typ:=ait_cutobject;
          place:=cut_normal;
       end;


     constructor tai_cutobject.Create_begin;
       begin
          inherited Create;
          typ:=ait_cutobject;
          place:=cut_begin;
       end;


     constructor tai_cutobject.Create_end;
       begin
          inherited Create;
          typ:=ait_cutobject;
          place:=cut_end;
       end;


    constructor tai_cutobject.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        place:=TCutPlace(ppufile.getbyte);
      end;


    procedure tai_cutobject.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(place));
      end;


{****************************************************************************
                             Tai_Marker
 ****************************************************************************}

    constructor Tai_Marker.Create(_Kind: TAsmMarker);
      begin
        Inherited Create;
        typ := ait_marker;
        Kind := _Kind;
      end;


    constructor Tai_Marker.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        kind:=TAsmMarker(ppufile.getbyte);
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

    constructor tai_regalloc.alloc(r : tregister;ainstr:tai);
      begin
        inherited create;
        typ:=ait_regalloc;
        ratype:=ra_alloc;
        reg:=r;
        { ainstr must be an instruction }
        if assigned(ainstr) and
           (ainstr.typ<>ait_instruction) then
          internalerror(200411011);
        instr:=ainstr;
      end;


    constructor tai_regalloc.dealloc(r : tregister;ainstr:tai);
      begin
        inherited create;
        typ:=ait_regalloc;
        ratype:=ra_dealloc;
        reg:=r;
        { ainstr must be an instruction }
        if assigned(ainstr) and
           (ainstr.typ<>ait_instruction) then
          internalerror(200411012);
        instr:=ainstr;
      end;


    constructor tai_regalloc.sync(r : tregister);
      begin
        inherited create;
        typ:=ait_regalloc;
        ratype:=ra_sync;
        reg:=r;
      end;


    constructor tai_regalloc.resize(r : tregister);
      begin
        inherited create;
        typ:=ait_regalloc;
        ratype:=ra_resize;
        reg:=r;
      end;


    constructor tai_regalloc.ppuload(t:taitype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getdata(reg,sizeof(Tregister));
        ratype:=tregalloctype(ppufile.getbyte);
      end;


    procedure tai_regalloc.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putdata(reg,sizeof(Tregister));
        ppufile.putbyte(byte(ratype));
      end;


{*****************************************************************************
                               TaiInstruction
*****************************************************************************}

    constructor tai_cpu_abstract.Create(op : tasmop);

      begin
         inherited create;
         typ:=ait_instruction;
         is_jmp:=false;
         opcode:=op;
         ops:=0;
         fillchar(condition,sizeof(condition),0);
         fillchar(oper,sizeof(oper),0);
      end;


    destructor tai_cpu_abstract.Destroy;
      var
        i : integer;
      begin
        for i:=0 to opercnt-1 do
          freeop(i);
        inherited destroy;
      end;


{ ---------------------------------------------------------------------
    Loading of operands.
  ---------------------------------------------------------------------}

    procedure tai_cpu_abstract.allocate_oper(opers:longint);
      begin
        while (opers>opercnt) do
          begin
            new(oper[opercnt]);
            fillchar(oper[opercnt]^,sizeof(toper),0);
            inc(opercnt);
          end;
      end;


    procedure tai_cpu_abstract.loadconst(opidx:longint;l:aint);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_const then
             clearop(opidx);
           val:=l;
           typ:=top_const;
         end;
      end;


    procedure tai_cpu_abstract.loadsymbol(opidx:longint;s:tasmsymbol;sofs:longint);
      var
        r : treference;
      begin
        reference_reset_symbol(r,s,sofs,1);
        r.refaddr:=addr_full;
        loadref(opidx,r);
      end;


    procedure tai_cpu_abstract.loadlocal(opidx:longint;s:pointer;sofs:longint;indexreg:tregister;scale:byte;getoffset,forceref:boolean);
      begin
        if not assigned(s) then
         internalerror(200204251);
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_local then
             begin
               clearop(opidx);
               new(localoper);
             end;
           with oper[opidx]^.localoper^ do
             begin
               localsym:=s;
               localsymofs:=sofs;
               localindexreg:=indexreg;
               localscale:=scale;
               localgetoffset:=getoffset;
               localforceref:=forceref;
             end;
           typ:=top_local;
         end;
      end;


    procedure tai_cpu_abstract.loadref(opidx:longint;const r:treference);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            if typ<>top_ref then
              begin
                clearop(opidx);
                new(ref);
              end;

            ref^:=r;
{$ifdef x86}
            { We allow this exception for x86, since overloading this would be
              too much of a a speed penalty}
            if (ref^.segment<>NR_NO) and (ref^.segment<>NR_DS) then
              segprefix:=ref^.segment;
{$endif}
            if (cs_create_pic in current_settings.moduleswitches) and
              assigned(r.symbol) and
              not assigned(r.relsymbol) and
              (r.refaddr=addr_no) then
              internalerror(200502052);
            typ:=top_ref;
            if assigned(add_reg_instruction_hook) then
              begin
                add_reg_instruction_hook(self,ref^.base);
                add_reg_instruction_hook(self,ref^.index);
              end;
            { mark symbol as used }
            if assigned(ref^.symbol) then
              ref^.symbol.increfs;
            if assigned(ref^.relsymbol) then
              ref^.relsymbol.increfs;
          end;
      end;


    procedure tai_cpu_abstract.loadreg(opidx:longint;r:tregister);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_reg then
             clearop(opidx);
           reg:=r;
           typ:=top_reg;
         end;
        if assigned(add_reg_instruction_hook) then
          add_reg_instruction_hook(self,r);
{$ifdef ARM}
        { R15 is the PC on the ARM thus moves to R15 are jumps.
          Due to speed considerations we don't use a virtual overridden method here.
          Because the pc/r15 isn't handled by the reg. allocator this should never cause
          problems with iregs getting r15.
        }
        is_jmp:=(opcode=A_MOV) and (opidx=0) and (r=NR_R15);
{$endif ARM}
      end;


    procedure tai_cpu_abstract.loadoper(opidx:longint;o:toper);
      begin
        allocate_oper(opidx+1);
        clearop(opidx);
        oper[opidx]^:=o;
        { copy also the reference }
        with oper[opidx]^ do
          begin
            case typ of
              top_reg:
                begin
                  if assigned(add_reg_instruction_hook) then
                    add_reg_instruction_hook(self,reg);
                end;
              top_ref:
                begin
                  new(ref);
                  ref^:=o.ref^;
                  if assigned(add_reg_instruction_hook) then
                    begin
                      add_reg_instruction_hook(self,ref^.base);
                      add_reg_instruction_hook(self,ref^.index);
                    end;
                end;
{$ifdef ARM}
              top_shifterop:
                begin
                  new(shifterop);
                  shifterop^:=o.shifterop^;
                  if assigned(add_reg_instruction_hook) then
                    add_reg_instruction_hook(self,shifterop^.rs);
                end;
{$endif ARM}
             end;
          end;
      end;

    procedure tai_cpu_abstract.clearop(opidx:longint);
      begin
        with oper[opidx]^ do
          begin
            case typ of
              top_ref:
                dispose(ref);
              top_local:
                dispose(localoper);
{$ifdef ARM}
              top_shifterop:
                dispose(shifterop);
              top_regset:
                dispose(regset);
{$endif ARM}
            end;
            typ:=top_none;
          end;
      end;


    procedure tai_cpu_abstract.freeop(opidx:longint);
      begin
        clearop(opidx);
        dispose(oper[opidx]);
      end;


{ ---------------------------------------------------------------------
    Miscellaneous methods.
  ---------------------------------------------------------------------}

    procedure tai_cpu_abstract.SetCondition(const c:TAsmCond);
      begin
         condition:=c;
      end;


    Function tai_cpu_abstract.getcopy:TLinkedListItem;
      var
        i : longint;
        p : tai_cpu_abstract;
      begin
        p:=tai_cpu_abstract(inherited getcopy);
        { make a copy of the references }
        p.opercnt:=0;
        p.allocate_oper(ops);
        for i:=0 to ops-1 do
          begin
            p.oper[i]^:=oper[i]^;
            case oper[i]^.typ of
              top_local :
                begin
                  new(p.oper[i]^.localoper);
                  p.oper[i]^.localoper^:=oper[i]^.localoper^;
                end;
              top_ref :
                begin
                  new(p.oper[i]^.ref);
                  p.oper[i]^.ref^:=oper[i]^.ref^;
                end;
{$ifdef ARM}
              top_shifterop:
                begin
                  new(p.oper[i]^.shifterop);
                  p.oper[i]^.shifterop^:=oper[i]^.shifterop^;
                end;
{$endif ARM}
            end;
          end;
        getcopy:=p;
      end;


    function tai_cpu_abstract.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        { When the generic RA is used this needs to be overriden, we don't use
          virtual;abstract; to prevent a lot of warnings of unimplemented abstract methods
          when tai_cpu is created (PFV) }
        internalerror(200404091);
        result:=false;
      end;


    function tai_cpu_abstract.spilling_get_operation_type(opnr: longint): topertype;
      begin
        internalerror(200404091);
        result:=operand_readwrite;
      end;


    function tai_cpu_abstract.spilling_get_operation_type_ref(opnr: longint; reg: tregister): topertype;
      begin
        result := operand_read;
      end;


    constructor tai_cpu_abstract.ppuload(t:taitype;ppufile:tcompilerppufile);
      var
        i : integer;
      begin
        inherited ppuload(t,ppufile);
        { hopefully, we don't get problems with big/litte endian here when cross compiling :/ }
        ppufile.getdata(condition,sizeof(tasmcond));
        allocate_oper(ppufile.getbyte);
        for i:=0 to ops-1 do
          ppuloadoper(ppufile,oper[i]^);
        opcode:=tasmop(ppufile.getword);
{$ifdef x86}
        ppufile.getdata(segprefix,sizeof(Tregister));
{$endif x86}
        is_jmp:=boolean(ppufile.getbyte);
      end;


    procedure tai_cpu_abstract.ppuwrite(ppufile:tcompilerppufile);
      var
        i : integer;
      begin
        inherited ppuwrite(ppufile);
        ppufile.putdata(condition,sizeof(tasmcond));
        ppufile.putbyte(ops);
        for i:=0 to ops-1 do
          ppuwriteoper(ppufile,oper[i]^);
        ppufile.putword(word(opcode));
{$ifdef x86}
        ppufile.putdata(segprefix,sizeof(Tregister));
{$endif x86}
        ppufile.putbyte(byte(is_jmp));
      end;


    procedure tai_cpu_abstract.buildderefimpl;
      var
        i : integer;
      begin
        for i:=0 to ops-1 do
          ppubuildderefimploper(oper[i]^);
      end;


    procedure tai_cpu_abstract.derefimpl;
      var
        i : integer;
      begin
        for i:=0 to ops-1 do
          ppuderefoper(oper[i]^);
      end;


    procedure tai_cpu_abstract.resetpass1;
      begin
      end;


    procedure tai_cpu_abstract.resetpass2;
      begin
      end;


   function tai_cpu_abstract.Pass1(objdata:TObjData):longint;
      begin
        result:=0;
      end;


    procedure tai_cpu_abstract.Pass2(objdata:TObjData);
      begin
      end;


    procedure tai_cpu_abstract.ppuloadoper(ppufile:tcompilerppufile;var o:toper);
      begin
        o.typ:=toptype(ppufile.getbyte);
        o.ot:=ppufile.getlongint;
        case o.typ of
          top_reg :
            ppufile.getdata(o.reg,sizeof(Tregister));
          top_ref :
            begin
              new(o.ref);
{$ifdef x86}
              ppufile.getdata(o.ref^.segment,sizeof(Tregister));
{$endif x86}
              ppufile.getdata(o.ref^.base,sizeof(Tregister));
              ppufile.getdata(o.ref^.index,sizeof(Tregister));
              ppufile.getdata(o.ref^.refaddr,sizeof(o.ref^.refaddr));
              o.ref^.scalefactor:=ppufile.getbyte;
              o.ref^.offset:=ppufile.getaint;
              o.ref^.symbol:=ppufile.getasmsymbol;
              o.ref^.relsymbol:=ppufile.getasmsymbol;
            end;
          top_const :
            o.val:=ppufile.getaint;
          top_local :
            begin
              new(o.localoper);
              with o.localoper^ do
                begin
                  ppufile.getderef(localsymderef);
                  localsymofs:=ppufile.getaint;
                  localindexreg:=tregister(ppufile.getlongint);
                  localscale:=ppufile.getbyte;
                  localgetoffset:=(ppufile.getbyte<>0);
                end;
            end;
          else
            internalerror(2007010210);
        end;
      end;


    procedure tai_cpu_abstract.ppuwriteoper(ppufile:tcompilerppufile;const o:toper);
      begin
        ppufile.putbyte(byte(o.typ));
        ppufile.putlongint(o.ot);
        case o.typ of
          top_reg :
            ppufile.putdata(o.reg,sizeof(Tregister));
          top_ref :
            begin
{$ifdef x86}
              ppufile.putdata(o.ref^.segment,sizeof(Tregister));
{$endif x86}
              ppufile.putdata(o.ref^.base,sizeof(Tregister));
              ppufile.putdata(o.ref^.index,sizeof(Tregister));
              ppufile.putdata(o.ref^.refaddr,sizeof(o.ref^.refaddr));
              ppufile.putbyte(o.ref^.scalefactor);
              ppufile.putaint(o.ref^.offset);
              ppufile.putasmsymbol(o.ref^.symbol);
              ppufile.putasmsymbol(o.ref^.relsymbol);
            end;
          top_const :
            ppufile.putaint(o.val);
          top_local :
            begin
              with o.localoper^ do
                begin
                  ppufile.putderef(localsymderef);
                  ppufile.putaint(localsymofs);
                  ppufile.putlongint(longint(localindexreg));
                  ppufile.putbyte(localscale);
                  ppufile.putbyte(byte(localgetoffset));
                end;
            end;
          else
            internalerror(2007010211);
        end;
      end;

{****************************************************************************
                              tai_align_abstract
 ****************************************************************************}

     constructor tai_align_abstract.Create(b: byte);
       begin
          inherited Create;
          typ:=ait_align;
{$ifdef EXTDEBUG}
          if upper(classname)='TAI_ALIGN_ABSTRACT' then
            internalerror(200709191);
{$endif EXTDEBUG}
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


     constructor tai_align_abstract.Create_zeros(b: byte);
       begin
          inherited Create;
          typ:=ait_align;
          if b in [1,2,4,8,16,32] then
            aligntype := b
          else
            aligntype := 1;
         use_op:=true;
         fillsize:=0;
         fillop:=0;
       end;


     function tai_align_abstract.calculatefillbuf(var buf : tfillbuffer):pchar;
       begin
         if fillsize>sizeof(buf) then
           internalerror(200404293);
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


begin
  cai_cpu:=tai_cpu_abstract;
  { aasmcpu is earlier in the unit order and can
    already set the cai_align }
  if not assigned(cai_align) then
    cai_align:=tai_align_abstract;
end.
