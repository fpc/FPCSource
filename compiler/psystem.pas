{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

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
end;


procedure insert_intern_types(p : tsymtable);
{
  all the types inserted into the system unit
}

  procedure addtype(const s:string;const t:ttype);
  begin
    p.insert(ttypesym.create(s,t));
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
  { Add a type for virtual method tables in lowercase }
  { so it isn't reachable!                            }
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
{ Normal types }
  addtype('Single',s32floattype);
  addtype('Double',s64floattype);
  addtype('Extended',s80floattype);
  addtype('Real',s64floattype);
{$ifdef i386}
  adddef('Comp',tfloatdef.create(s64comp));
{$endif}
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
  u32bittype.setdef(torddef.create(u32bit,0,longint($ffffffff)));
  s32bittype.setdef(torddef.create(s32bit,longint($80000000),$7fffffff));
  cu64bittype.setdef(torddef.create(u64bit,0,0));
  cs64bittype.setdef(torddef.create(s64bit,0,0));
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
{$ifdef i386}
  s32floattype.setdef(tfloatdef.create(s32real));
  s64floattype.setdef(tfloatdef.create(s64real));
  s80floattype.setdef(tfloatdef.create(s80real));
{$endif}
{$ifdef m68k}
  s32floattype.setdef(tfloatdef.create(s32real));
  s64floattype.setdef(tfloatdef.create(s64real));
  if (cs_fp_emulation in aktmoduleswitches) then
   s80floattype.setdef(tfloatdef.create(s32real)))
  else
   s80floattype.setdef(tfloatdef.create(s80real));
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
  Revision 1.15  2001-04-13 01:22:13  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.14  2001/04/02 21:20:34  peter
    * resulttype rewrite

  Revision 1.13  2001/03/25 12:40:00  florian
    * cwidechar was loaded with a chardef, fixed

  Revision 1.12  2001/03/22 00:10:58  florian
    + basic variant type support in the compiler

  Revision 1.11  2000/12/07 17:19:43  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.10  2000/11/29 00:30:38  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.9  2000/11/09 17:46:56  florian
    * System.TypeInfo fixed
    + System.Finalize implemented
    + some new keywords for interface support added

  Revision 1.8  2000/10/31 22:02:51  peter
    * symtable splitted, no real code changes

  Revision 1.7  2000/10/21 18:16:12  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.6  2000/10/14 10:14:52  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.5  2000/09/24 15:06:24  peter
    * use defines.inc

  Revision 1.4  2000/08/27 20:19:39  peter
    * store strings with case in ppu, when an internal symbol is created
      a '$' is prefixed so it's not automatic uppercased

  Revision 1.3  2000/08/16 13:06:06  florian
    + support of 64 bit integer constants

  Revision 1.2  2000/07/13 11:32:47  michael
  + removed logs

}
