{
    $Id$
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

procedure insertinternsyms(p : tsymtable);
procedure insert_intern_types(p : tsymtable);

procedure readconstdefs;
procedure createconstdefs;


implementation

uses
  globals,
  symconst,symtype,symsym,symdef,symtable,
  ninl;

procedure insertinternsyms(p : tsymtable);
{
  all intern procedures for the system unit
}
begin
  p.insert(tsyssym.create('Concat',in_concat_x));
  p.insert(tsyssym.create('Write',in_write_x));
  p.insert(tsyssym.create('WriteLn',in_writeln_x));
  p.insert(tsyssym.create('Assigned',in_assigned_x));
  p.insert(tsyssym.create('Read',in_read_x));
  p.insert(tsyssym.create('ReadLn',in_readln_x));
  p.insert(tsyssym.create('Ofs',in_ofs_x));
  p.insert(tsyssym.create('SizeOf',in_sizeof_x));
  p.insert(tsyssym.create('TypeOf',in_typeof_x));
  p.insert(tsyssym.create('Low',in_low_x));
  p.insert(tsyssym.create('High',in_high_x));
  p.insert(tsyssym.create('Seg',in_seg_x));
  p.insert(tsyssym.create('Ord',in_ord_x));
  p.insert(tsyssym.create('Pred',in_pred_x));
  p.insert(tsyssym.create('Succ',in_succ_x));
  p.insert(tsyssym.create('Exclude',in_exclude_x_y));
  p.insert(tsyssym.create('Include',in_include_x_y));
  p.insert(tsyssym.create('Break',in_break));
  p.insert(tsyssym.create('Continue',in_continue));
  p.insert(tsyssym.create('Dec',in_dec_x));
  p.insert(tsyssym.create('Inc',in_inc_x));
  p.insert(tsyssym.create('Str',in_str_x_string));
  p.insert(tsyssym.create('Assert',in_assert_x_y));
  p.insert(tsyssym.create('Val',in_val_x));
  p.insert(tsyssym.create('Addr',in_addr_x));
  p.insert(tsyssym.create('TypeInfo',in_typeinfo_x));
  p.insert(tsyssym.create('SetLength',in_setlength_x));
  p.insert(tsyssym.create('Finalize',in_finalize_x));
  p.insert(tsyssym.create('Length',in_length_x));
  p.insert(tsyssym.create('New',in_new_x));
  p.insert(tsyssym.create('Dispose',in_dispose_x));
end;


procedure insert_intern_types(p : tsymtable);
{
  all the types inserted into the system unit
}

  function addtype(const s:string;const t:ttype):ttypesym;
  begin
    result:=ttypesym.create(s,t);
    p.insert(result);
    { add init/final table if required }
    if t.def.needs_inittable then
     generate_inittable(result);
  end;

  procedure adddef(const s:string;def:tdef);
  var
    t : ttype;
  begin
    t.setdef(def);
    p.insert(ttypesym.create(s,t));
  end;

var
  { several defs to simulate more or less C++ objects for GDB }
  vmttype,
  vmtarraytype : ttype;
  vmtsymtable  : tsymtable;
begin
{ Normal types }
  addtype('Single',s32floattype);
  addtype('Double',s64floattype);
  addtype('Extended',s80floattype);
  addtype('Real',s64floattype);
{$ifdef x86}
  adddef('Comp',tfloatdef.create(s64comp));
{$endif x86}
  addtype('Currency',s64currencytype);
  addtype('Pointer',voidpointertype);
  addtype('FarPointer',voidfarpointertype);
  addtype('ShortString',cshortstringtype);
  addtype('LongString',clongstringtype);
  addtype('AnsiString',cansistringtype);
  addtype('WideString',cwidestringtype);
  addtype('Boolean',booltype);
  addtype('ByteBool',booltype);
  adddef('WordBool',torddef.create(bool16bit,0,1));
  adddef('LongBool',torddef.create(bool32bit,0,1));
  addtype('Char',cchartype);
  addtype('WideChar',cwidechartype);
  adddef('Text',tfiledef.createtext);
  addtype('Cardinal',u32bittype);
  addtype('QWord',cu64bittype);
  addtype('Int64',cs64bittype);
  adddef('TypedFile',tfiledef.createtyped(voidtype));
  addtype('Variant',cvarianttype);
{ Internal types }
  addtype('$formal',cformaltype);
  addtype('$void',voidtype);
  addtype('$byte',u8bittype);
  addtype('$word',u16bittype);
  addtype('$ulong',u32bittype);
  addtype('$longint',s32bittype);
  addtype('$qword',cu64bittype);
  addtype('$int64',cs64bittype);
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
  addtype('$void_farpointer',voidfarpointertype);
  addtype('$openchararray',openchararraytype);
  addtype('$file',cfiletype);
  addtype('$variant',cvarianttype);
  addtype('$s32real',s32floattype);
  addtype('$s64real',s64floattype);
  addtype('$s80real',s80floattype);
  addtype('$s64currency',s64currencytype);
{ Add a type for virtual method tables }
  vmtsymtable:=trecordsymtable.create;
  vmttype.setdef(trecorddef.create(vmtsymtable));
  pvmttype.setdef(tpointerdef.create(vmttype));
  vmtsymtable.insert(tvarsym.create('$parent',pvmttype));
  vmtsymtable.insert(tvarsym.create('$length',s32bittype));
  vmtsymtable.insert(tvarsym.create('$mlength',s32bittype));
  vmtarraytype.setdef(tarraydef.create(0,1,s32bittype));
  tarraydef(vmtarraytype.def).elementtype:=voidpointertype;
  vmtsymtable.insert(tvarsym.create('$__pfn',vmtarraytype));
  addtype('$__vtbl_ptr_type',vmttype);
  addtype('$pvmt',pvmttype);
  vmtarraytype.setdef(tarraydef.create(0,1,s32bittype));
  tarraydef(vmtarraytype.def).elementtype:=pvmttype;
  addtype('$vtblarray',vmtarraytype);
{ Add functions that require compiler magic }
  insertinternsyms(p);
end;


procedure readconstdefs;
{
  Load all default definitions for consts from the system unit
}
begin
  globaldef('byte',u8bittype);
  globaldef('word',u16bittype);
  globaldef('ulong',u32bittype);
  globaldef('longint',s32bittype);
  globaldef('qword',cu64bittype);
  globaldef('int64',cs64bittype);
  globaldef('formal',cformaltype);
  globaldef('void',voidtype);
  globaldef('char',cchartype);
  globaldef('widechar',cwidechartype);
  globaldef('shortstring',cshortstringtype);
  globaldef('longstring',clongstringtype);
  globaldef('ansistring',cansistringtype);
  globaldef('widestring',cwidestringtype);
  globaldef('openshortstring',openshortstringtype);
  globaldef('openchararray',openchararraytype);
  globaldef('s32real',s32floattype);
  globaldef('s64real',s64floattype);
  globaldef('s80real',s80floattype);
  globaldef('s64currency',s64currencytype);
  globaldef('boolean',booltype);
  globaldef('void_pointer',voidpointertype);
  globaldef('char_pointer',charpointertype);
  globaldef('void_farpointer',voidfarpointertype);
  globaldef('file',cfiletype);
  globaldef('pvmt',pvmttype);
  globaldef('variant',cvarianttype);
end;


procedure createconstdefs;
{
  Create all default definitions for consts for the system unit
}
var
  oldregisterdef : boolean;
begin
  { create definitions for constants }
  oldregisterdef:=registerdef;
  registerdef:=false;
  cformaltype.setdef(tformaldef.create);
  voidtype.setdef(torddef.create(uvoid,0,0));
  u8bittype.setdef(torddef.create(u8bit,0,255));
  u16bittype.setdef(torddef.create(u16bit,0,65535));
  u32bittype.setdef(torddef.create(u32bit,0,high(cardinal)));
  s32bittype.setdef(torddef.create(s32bit,low(longint),high(longint)));
  cu64bittype.setdef(torddef.create(u64bit,low(qword),TConstExprInt(high(qword))));
  cs64bittype.setdef(torddef.create(s64bit,low(int64),high(int64)));
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
  openchararraytype.setdef(tarraydef.create(0,-1,s32bittype));
  tarraydef(openchararraytype.def).elementtype:=cchartype;
{$ifdef x86}
  s32floattype.setdef(tfloatdef.create(s32real));
  s64floattype.setdef(tfloatdef.create(s64real));
  s80floattype.setdef(tfloatdef.create(s80real));
{$endif x86}
{$ifdef powerpc}
  s32floattype.setdef(tfloatdef.create(s32real));
  s64floattype.setdef(tfloatdef.create(s64real));
  s80floattype.setdef(tfloatdef.create(s80real));
{$endif powerpc}
{$ifdef sparc}
  s32floattype.setdef(tfloatdef.create(s32real));
  s64floattype.setdef(tfloatdef.create(s64real));
{$endif sparc}
  s64currencytype.setdef(tfloatdef.create(s64currency));
{$ifdef m68k}
  s32floattype.setdef(tfloatdef.create(s32real));
  if (cs_fp_emulation in aktmoduleswitches) then
   begin
     s64floattype.setdef(tfloatdef.create(s32real));
     s80floattype.setdef(tfloatdef.create(s32real)))
   end
  else
   begin
     s64floattype.setdef(tfloatdef.create(s64real));
     s80floattype.setdef(tfloatdef.create(s80real));
   end;
{$endif}
  { some other definitions }
  voidpointertype.setdef(tpointerdef.create(voidtype));
  charpointertype.setdef(tpointerdef.create(cchartype));
  voidfarpointertype.setdef(tpointerdef.createfar(voidtype));
  cfiletype.setdef(tfiledef.createuntyped);
  cvarianttype.setdef(tvariantdef.create);
  registerdef:=oldregisterdef;
end;


end.
{
  $Log$
  Revision 1.30  2002-07-07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.29  2002/07/06 20:18:47  carl
  + more SPARC patches from Mazen

  Revision 1.28  2002/07/04 20:43:02  florian
    * first x86-64 patches

  Revision 1.27  2002/07/01 16:23:54  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.26  2002/05/18 13:34:16  peter
    * readded missing revisions

  Revision 1.25  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.23  2002/05/12 16:53:09  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.22  2002/01/24 12:33:53  jonas
    * adapted ranges of native types to int64 (e.g. high cardinal is no
      longer longint($ffffffff), but just $fffffff in psystem)
    * small additional fix in 64bit rangecheck code generation for 32 bit
      processors
    * adaption of ranges required the matching talgorithm used for selecting
      which overloaded procedure to call to be adapted. It should now always
      select the closest match for ordinal parameters.
    + inttostr(qword) in sysstr.inc/sysstrh.inc
    + abs(int64), sqr(int64), sqr(qword) in systemh.inc/generic.inc (previous
      fixes were required to be able to add them)
    * is_in_limit() moved from ncal to types unit, should always be used
      instead of direct comparisons of low/high values of orddefs because
      qword is a special case

}