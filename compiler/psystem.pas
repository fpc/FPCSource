{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Load the system unit, create required defs for systemunit

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
unit psystem;

{$i fpcdefs.inc}

interface

    uses
      symbase;

    procedure create_intern_symbols;
    procedure create_intern_types;

    procedure load_intern_types;

    procedure registernodes;
    procedure registertais;


implementation

    uses
      globals,globtype,verbose,
      systems,
      symconst,symtype,symsym,symdef,symtable,
      aasmtai,aasmdata,aasmcpu,ncgutil,fmodule,
      node,nbas,nflw,nset,ncon,ncnv,nld,nmem,ncal,nmat,nadd,ninl,nopt
      ;


    procedure create_intern_symbols;
      {
        all intern procedures for the system unit
      }
      begin
        systemunit.insert(tsyssym.create('Concat',in_concat_x));
        systemunit.insert(tsyssym.create('Write',in_write_x));
        systemunit.insert(tsyssym.create('WriteLn',in_writeln_x));
        systemunit.insert(tsyssym.create('Assigned',in_assigned_x));
        systemunit.insert(tsyssym.create('Read',in_read_x));
        systemunit.insert(tsyssym.create('ReadLn',in_readln_x));
        systemunit.insert(tsyssym.create('Ofs',in_ofs_x));
        systemunit.insert(tsyssym.create('SizeOf',in_sizeof_x));
        systemunit.insert(tsyssym.create('TypeOf',in_typeof_x));
        systemunit.insert(tsyssym.create('Low',in_low_x));
        systemunit.insert(tsyssym.create('High',in_high_x));
        systemunit.insert(tsyssym.create('Slice',in_slice_x));
        systemunit.insert(tsyssym.create('Seg',in_seg_x));
        systemunit.insert(tsyssym.create('Ord',in_ord_x));
        systemunit.insert(tsyssym.create('Pred',in_pred_x));
        systemunit.insert(tsyssym.create('Succ',in_succ_x));
        systemunit.insert(tsyssym.create('Exclude',in_exclude_x_y));
        systemunit.insert(tsyssym.create('Include',in_include_x_y));
        systemunit.insert(tsyssym.create('Break',in_break));
        systemunit.insert(tsyssym.create('Exit',in_exit));
        systemunit.insert(tsyssym.create('Continue',in_continue));
        systemunit.insert(tsyssym.create('Leave',in_leave)); {macpas only}
        systemunit.insert(tsyssym.create('Cycle',in_cycle)); {macpas only}
        systemunit.insert(tsyssym.create('Dec',in_dec_x));
        systemunit.insert(tsyssym.create('Inc',in_inc_x));
        systemunit.insert(tsyssym.create('Str',in_str_x_string));
        systemunit.insert(tsyssym.create('Assert',in_assert_x_y));
        systemunit.insert(tsyssym.create('Val',in_val_x));
        systemunit.insert(tsyssym.create('Addr',in_addr_x));
        systemunit.insert(tsyssym.create('TypeInfo',in_typeinfo_x));
        systemunit.insert(tsyssym.create('SetLength',in_setlength_x));
        systemunit.insert(tsyssym.create('Copy',in_copy_x));
        systemunit.insert(tsyssym.create('Initialize',in_initialize_x));
        systemunit.insert(tsyssym.create('Finalize',in_finalize_x));
        systemunit.insert(tsyssym.create('Length',in_length_x));
        systemunit.insert(tsyssym.create('New',in_new_x));
        systemunit.insert(tsyssym.create('Dispose',in_dispose_x));
      end;


    procedure create_intern_types;
      {
        all the types inserted into the system unit
      }

        function addtype(const s:string;const t:ttype):ttypesym;
        begin
          result:=ttypesym.create(s,t);
          systemunit.insert(result);
          { add init/final table if required }
          if t.def.needs_inittable then
           generate_inittable(result);
        end;

        procedure adddef(const s:string;def:tdef);
        var
          t : ttype;
        begin
          t.setdef(def);
          systemunit.insert(ttypesym.create(s,t));
        end;

      var
        hrecst : trecordsymtable;
      begin
        symtablestack.push(systemunit);
        cundefinedtype.setdef(tundefineddef.create);
        cformaltype.setdef(tformaldef.create);
        voidtype.setdef(torddef.create(uvoid,0,0));
        u8inttype.setdef(torddef.create(u8bit,0,255));
        s8inttype.setdef(torddef.create(s8bit,-128,127));
        u16inttype.setdef(torddef.create(u16bit,0,65535));
        s16inttype.setdef(torddef.create(s16bit,-32768,32767));
        u32inttype.setdef(torddef.create(u32bit,0,high(longword)));
        s32inttype.setdef(torddef.create(s32bit,low(longint),high(longint)));
        u64inttype.setdef(torddef.create(u64bit,low(qword),TConstExprInt(high(qword))));
        s64inttype.setdef(torddef.create(s64bit,low(int64),high(int64)));
        booltype.setdef(torddef.create(bool8bit,0,1));
        cchartype.setdef(torddef.create(uchar,0,255));
        cwidechartype.setdef(torddef.create(uwidechar,0,65535));
        cshortstringtype.setdef(tstringdef.createshort(255));
        { should we give a length to the default long and ansi string definition ?? }
        clongstringtype.setdef(tstringdef.createlong(-1));
        cansistringtype.setdef(tstringdef.createansi(-1));
        cwidestringtype.setdef(tstringdef.createwide(-1));
        { length=0 for shortstring is open string (needed for readln(string) }
        openshortstringtype.setdef(tstringdef.createshort(0));
        openchararraytype.setdef(tarraydef.create(0,-1,s32inttype));
        tarraydef(openchararraytype.def).setelementtype(cchartype);
{$ifdef x86}
        s32floattype.setdef(tfloatdef.create(s32real));
        s64floattype.setdef(tfloatdef.create(s64real));
        s80floattype.setdef(tfloatdef.create(s80real));
        if target_info.system<>system_x86_64_win64 then
          s64currencytype.setdef(tfloatdef.create(s64currency))
        else
          s64currencytype.setdef(torddef.create(scurrency,low(int64),high(int64)));
{$endif x86}
{$ifdef powerpc}
        s32floattype.setdef(tfloatdef.create(s32real));
        s64floattype.setdef(tfloatdef.create(s64real));
        s80floattype.setdef(tfloatdef.create(s80real));
        s64currencytype.setdef(torddef.create(scurrency,low(int64),high(int64)));
{$endif powerpc}
{$ifdef POWERPC64}
        s32floattype.setdef(tfloatdef.create(s32real));
        s64floattype.setdef(tfloatdef.create(s64real));
        s80floattype.setdef(tfloatdef.create(s80real));
        s64currencytype.setdef(torddef.create(scurrency,low(int64),high(int64)));
{$endif POWERPC64}
{$ifdef sparc}
        s32floattype.setdef(tfloatdef.create(s32real));
        s64floattype.setdef(tfloatdef.create(s64real));
        s80floattype.setdef(tfloatdef.create(s80real));
        s64currencytype.setdef(torddef.create(scurrency,low(int64),high(int64)));
{$endif sparc}
{$ifdef m68k}
        s32floattype.setdef(tfloatdef.create(s32real));
        s64floattype.setdef(tfloatdef.create(s64real));
        s80floattype.setdef(tfloatdef.create(s80real));
        s64currencytype.setdef(torddef.create(scurrency,low(int64),high(int64)));
{$endif}
{$ifdef arm}
        s32floattype.setdef(tfloatdef.create(s32real));
        s64floattype.setdef(tfloatdef.create(s64real));
        s80floattype.setdef(tfloatdef.create(s80real));
        s64currencytype.setdef(torddef.create(scurrency,low(int64),high(int64)));
{$endif arm}
{$ifdef cpu64bit}
        uinttype:=u64inttype;
        sinttype:=s64inttype;
        ptrinttype:=u64inttype;
{$else cpu64bit}
        uinttype:=u32inttype;
        sinttype:=s32inttype;
        ptrinttype:=u32inttype;
{$endif cpu64bit}
        { some other definitions }
        voidpointertype.setdef(tpointerdef.create(voidtype));
        charpointertype.setdef(tpointerdef.create(cchartype));
        widecharpointertype.setdef(tpointerdef.create(cwidechartype));
        voidfarpointertype.setdef(tpointerdef.createfar(voidtype));
        cfiletype.setdef(tfiledef.createuntyped);
        cvarianttype.setdef(tvariantdef.create(vt_normalvariant));
        colevarianttype.setdef(tvariantdef.create(vt_olevariant));

        if target_info.system=system_x86_64_win64 then
          pbestrealtype:=@s64floattype;

{$ifdef cpufpemu}
        { Normal types }
        (* we use the same types as without emulator, the only
          difference is that direct calls to the emulator are generated
        if (cs_fp_emulation in aktmoduleswitches) then
          begin
            addtype('Single',s32floattype);
            { extended size is the best real type for the target }
            addtype('Real',s32floattype);
            pbestrealtype:=@s32floattype;
            { extended size is the best real type for the target }
            addtype('Extended',pbestrealtype^);
          end
        else
        *)
{$endif cpufpemu}
          begin
            addtype('Single',s32floattype);
            addtype('Double',s64floattype);
            { extended size is the best real type for the target }
            addtype('Extended',pbestrealtype^);
            addtype('Real',s64floattype);
          end;
{$ifdef x86}
        if target_info.system<>system_x86_64_win64 then
          adddef('Comp',tfloatdef.create(s64comp));
{$endif x86}
        addtype('Currency',s64currencytype);
        addtype('Pointer',voidpointertype);
{$ifdef x86}
        addtype('FarPointer',voidfarpointertype);
{$endif x86}
        addtype('ShortString',cshortstringtype);
{$ifdef support_longstring}
        addtype('LongString',clongstringtype);
{$endif support_longstring}
        addtype('AnsiString',cansistringtype);
        addtype('WideString',cwidestringtype);
        addtype('Boolean',booltype);
        addtype('ByteBool',booltype);
        adddef('WordBool',torddef.create(bool16bit,0,1));
        adddef('LongBool',torddef.create(bool32bit,0,1));
        addtype('Byte',u8inttype);
        addtype('ShortInt',s8inttype);
        addtype('Word',u16inttype);
        addtype('SmallInt',s16inttype);
        addtype('LongWord',u32inttype);
        addtype('LongInt',s32inttype);
        addtype('QWord',u64inttype);
        addtype('Int64',s64inttype);
        addtype('Char',cchartype);
        addtype('WideChar',cwidechartype);
        adddef('Text',tfiledef.createtext);
        adddef('TypedFile',tfiledef.createtyped(voidtype));
        addtype('Variant',cvarianttype);
        addtype('OleVariant',colevarianttype);
        { Internal types }
        addtype('$undefined',cundefinedtype);
        addtype('$formal',cformaltype);
        addtype('$void',voidtype);
        addtype('$byte',u8inttype);
        addtype('$shortint',s8inttype);
        addtype('$word',u16inttype);
        addtype('$smallint',s16inttype);
        addtype('$ulong',u32inttype);
        addtype('$longint',s32inttype);
        addtype('$qword',u64inttype);
        addtype('$int64',s64inttype);
        addtype('$char',cchartype);
        addtype('$widechar',cwidechartype);
        addtype('$shortstring',cshortstringtype);
        addtype('$longstring',clongstringtype);
        addtype('$ansistring',cansistringtype);
        addtype('$widestring',cwidestringtype);
        addtype('$openshortstring',openshortstringtype);
        addtype('$boolean',booltype);
        addtype('$void_pointer',voidpointertype);
        addtype('$char_pointer',charpointertype);
        addtype('$widechar_pointer',widecharpointertype);
        addtype('$void_farpointer',voidfarpointertype);
        addtype('$openchararray',openchararraytype);
        addtype('$file',cfiletype);
        addtype('$variant',cvarianttype);
        addtype('$olevariant',cvarianttype);
        addtype('$s32real',s32floattype);
        addtype('$s64real',s64floattype);
        addtype('$s80real',s80floattype);
        addtype('$s64currency',s64currencytype);
        { Add a type for virtual method tables }
        hrecst:=trecordsymtable.create(aktpackrecords);
        vmttype.setdef(trecorddef.create(hrecst));
        pvmttype.setdef(tpointerdef.create(vmttype));
        hrecst.insertfield(tfieldvarsym.create('$parent',vs_value,pvmttype,[]));
        hrecst.insertfield(tfieldvarsym.create('$length',vs_value,s32inttype,[]));
        hrecst.insertfield(tfieldvarsym.create('$mlength',vs_value,s32inttype,[]));
        vmtarraytype.setdef(tarraydef.create(0,1,s32inttype));
        tarraydef(vmtarraytype.def).setelementtype(voidpointertype);
        hrecst.insertfield(tfieldvarsym.create('$__pfn',vs_value,vmtarraytype,[]));
        addtype('$__vtbl_ptr_type',vmttype);
        addtype('$pvmt',pvmttype);
        vmtarraytype.setdef(tarraydef.create(0,1,s32inttype));
        tarraydef(vmtarraytype.def).setelementtype(pvmttype);
        addtype('$vtblarray',vmtarraytype);
        { Add a type for methodpointers }
        hrecst:=trecordsymtable.create(1);
        hrecst.insertfield(tfieldvarsym.create('$proc',vs_value,voidpointertype,[]));
        hrecst.insertfield(tfieldvarsym.create('$self',vs_value,voidpointertype,[]));
        methodpointertype.setdef(trecorddef.create(hrecst));
        addtype('$methodpointer',methodpointertype);
        symtablestack.pop(systemunit);
      end;


    procedure load_intern_types;
      {
        Load all default definitions for consts from the system unit
      }

        procedure loadtype(const s:string;var t:ttype);
        var
          srsym : ttypesym;
        begin
          srsym:=search_system_type(s);
          t:=srsym.restype;
        end;

      var
        oldcurrentmodule : tmodule;
      begin
        oldcurrentmodule:=current_module;
        current_module:=nil;
        loadtype('byte',u8inttype);
        loadtype('shortint',s8inttype);
        loadtype('word',u16inttype);
        loadtype('smallint',s16inttype);
        loadtype('ulong',u32inttype);
        loadtype('longint',s32inttype);
        loadtype('qword',u64inttype);
        loadtype('int64',s64inttype);
        loadtype('undefined',cundefinedtype);
        loadtype('formal',cformaltype);
        loadtype('void',voidtype);
        loadtype('char',cchartype);
        loadtype('widechar',cwidechartype);
        loadtype('shortstring',cshortstringtype);
        loadtype('longstring',clongstringtype);
        loadtype('ansistring',cansistringtype);
        loadtype('widestring',cwidestringtype);
        loadtype('openshortstring',openshortstringtype);
        loadtype('openchararray',openchararraytype);
        loadtype('s32real',s32floattype);
        loadtype('s64real',s64floattype);
        loadtype('s80real',s80floattype);
        loadtype('s64currency',s64currencytype);
        loadtype('boolean',booltype);
        loadtype('void_pointer',voidpointertype);
        loadtype('char_pointer',charpointertype);
        loadtype('widechar_pointer',widecharpointertype);
        loadtype('void_farpointer',voidfarpointertype);
        loadtype('file',cfiletype);
        loadtype('pvmt',pvmttype);
        loadtype('vtblarray',vmtarraytype);
        loadtype('__vtbl_ptr_type',vmttype);
        loadtype('variant',cvarianttype);
        loadtype('olevariant',colevarianttype);
        loadtype('methodpointer',methodpointertype);
{$ifdef cpu64bit}
        uinttype:=u64inttype;
        sinttype:=s64inttype;
        ptrinttype:=u64inttype;
{$else cpu64bit}
        uinttype:=u32inttype;
        sinttype:=s32inttype;
        ptrinttype:=u32inttype;
{$endif cpu64bit}
        current_module:=oldcurrentmodule;
      end;


    procedure registernodes;
      {
        Register all possible nodes in the nodeclass array that
        will be used for loading the nodes from a ppu
      }
      begin
        nodeclass[addn]:=caddnode;
        nodeclass[muln]:=caddnode;
        nodeclass[subn]:=caddnode;
        nodeclass[divn]:=cmoddivnode;
        nodeclass[symdifn]:=caddnode;
        nodeclass[modn]:=cmoddivnode;
        nodeclass[assignn]:=cassignmentnode;
        nodeclass[loadn]:=cloadnode;
        nodeclass[rangen]:=crangenode;
        nodeclass[ltn]:=caddnode;
        nodeclass[lten]:=caddnode;
        nodeclass[gtn]:=caddnode;
        nodeclass[gten]:=caddnode;
        nodeclass[equaln]:=caddnode;
        nodeclass[unequaln]:=caddnode;
        nodeclass[inn]:=cinnode;
        nodeclass[orn]:=caddnode;
        nodeclass[xorn]:=caddnode;
        nodeclass[shrn]:=cshlshrnode;
        nodeclass[shln]:=cshlshrnode;
        nodeclass[slashn]:=caddnode;
        nodeclass[andn]:=caddnode;
        nodeclass[subscriptn]:=csubscriptnode;
        nodeclass[derefn]:=cderefnode;
        nodeclass[addrn]:=caddrnode;
        nodeclass[ordconstn]:=cordconstnode;
        nodeclass[typeconvn]:=ctypeconvnode;
        nodeclass[calln]:=ccallnode;
        nodeclass[callparan]:=ccallparanode;
        nodeclass[realconstn]:=crealconstnode;
        nodeclass[unaryminusn]:=cunaryminusnode;
        nodeclass[asmn]:=casmnode;
        nodeclass[vecn]:=cvecnode;
        nodeclass[pointerconstn]:=cpointerconstnode;
        nodeclass[stringconstn]:=cstringconstnode;
        nodeclass[notn]:=cnotnode;
        nodeclass[inlinen]:=cinlinenode;
        nodeclass[niln]:=cnilnode;
        nodeclass[errorn]:=cerrornode;
        nodeclass[typen]:=ctypenode;
        nodeclass[setelementn]:=csetelementnode;
        nodeclass[setconstn]:=csetconstnode;
        nodeclass[blockn]:=cblocknode;
        nodeclass[statementn]:=cstatementnode;
        nodeclass[ifn]:=cifnode;
        nodeclass[breakn]:=cbreaknode;
        nodeclass[continuen]:=ccontinuenode;
        nodeclass[whilerepeatn]:=cwhilerepeatnode;
        nodeclass[forn]:=cfornode;
        nodeclass[exitn]:=cexitnode;
        nodeclass[withn]:=cwithnode;
        nodeclass[casen]:=ccasenode;
        nodeclass[labeln]:=clabelnode;
        nodeclass[goton]:=cgotonode;
        nodeclass[tryexceptn]:=ctryexceptnode;
        nodeclass[raisen]:=craisenode;
        nodeclass[tryfinallyn]:=ctryfinallynode;
        nodeclass[onn]:=connode;
        nodeclass[isn]:=cisnode;
        nodeclass[asn]:=casnode;
        nodeclass[caretn]:=caddnode;
        nodeclass[starstarn]:=caddnode;
        nodeclass[arrayconstructorn]:=carrayconstructornode;
        nodeclass[arrayconstructorrangen]:=carrayconstructorrangenode;
        nodeclass[tempcreaten]:=ctempcreatenode;
        nodeclass[temprefn]:=ctemprefnode;
        nodeclass[tempdeleten]:=ctempdeletenode;
        nodeclass[addoptn]:=caddnode;
        nodeclass[nothingn]:=cnothingnode;
        nodeclass[loadvmtaddrn]:=cloadvmtaddrnode;
        nodeclass[guidconstn]:=cguidconstnode;
        nodeclass[rttin]:=crttinode;
        nodeclass[loadparentfpn]:=cloadparentfpnode;
      end;


    procedure registertais;
      {
        Register all possible tais in the taiclass array that
        will be used for loading the tais from a ppu
      }
      begin
        aiclass[ait_none]:=nil;
        aiclass[ait_align]:=tai_align;
        aiclass[ait_section]:=tai_section;
        aiclass[ait_comment]:=tai_comment;
        aiclass[ait_string]:=tai_string;
        aiclass[ait_instruction]:=taicpu;
        aiclass[ait_datablock]:=tai_datablock;
        aiclass[ait_symbol]:=tai_symbol;
        aiclass[ait_symbol_end]:=tai_symbol_end;
        aiclass[ait_directive]:=tai_directive;
        aiclass[ait_label]:=tai_label;
        aiclass[ait_const]:=tai_const;
        aiclass[ait_real_32bit]:=tai_real_32bit;
        aiclass[ait_real_64bit]:=tai_real_64bit;
        aiclass[ait_real_80bit]:=tai_real_80bit;
        aiclass[ait_comp_64bit]:=tai_comp_64bit;
        aiclass[ait_stab]:=tai_stab;
        aiclass[ait_force_line]:=tai_force_line;
        aiclass[ait_function_name]:=tai_function_name;
{$ifdef alpha}
          { the follow is for the DEC Alpha }
        aiclass[ait_frame]:=tai_frame;
        aiclass[ait_ent]:=tai_ent;
{$endif alpha}
{$ifdef m68k}
{$warning FIXME: tai_labeled_instruction doesn't exists}
//        aiclass[ait_labeled_instruction]:=tai_labeled_instruction;
{$endif m68k}
{$ifdef ia64}
        aiclass[ait_bundle]:=tai_bundle;
        aiclass[ait_stop]:=tai_stop;
{$endif ia64}
{$ifdef SPARC}
//        aiclass[ait_labeled_instruction]:=tai_labeled_instruction;
{$endif SPARC}
        aiclass[ait_cutobject]:=tai_cutobject;
        aiclass[ait_regalloc]:=tai_regalloc;
        aiclass[ait_tempalloc]:=tai_tempalloc;
        aiclass[ait_marker]:=tai_marker;
        aiclass[ait_file]:=tai_file;
        aiclass[ait_loc]:=tai_loc;
      end;

end.
