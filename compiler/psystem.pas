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
  symtable;

procedure insertinternsyms(p : psymtable);
procedure insert_intern_types(p : psymtable);

procedure readconstdefs;
procedure createconstdefs;


implementation

uses
  globtype,globals,symconst,tree;

procedure insertinternsyms(p : psymtable);
{
  all intern procedures for system unit
}
begin
  p^.insert(new(psyssym,init('Concat',in_concat_x)));
  p^.insert(new(psyssym,init('Write',in_write_x)));
  p^.insert(new(psyssym,init('WriteLn',in_writeln_x)));
  p^.insert(new(psyssym,init('Assigned',in_assigned_x)));
  p^.insert(new(psyssym,init('Read',in_read_x)));
  p^.insert(new(psyssym,init('ReadLn',in_readln_x)));
  p^.insert(new(psyssym,init('Ofs',in_ofs_x)));
  p^.insert(new(psyssym,init('SizeOf',in_sizeof_x)));
  p^.insert(new(psyssym,init('TypeOf',in_typeof_x)));
  p^.insert(new(psyssym,init('Low',in_low_x)));
  p^.insert(new(psyssym,init('High',in_high_x)));
  p^.insert(new(psyssym,init('Seg',in_seg_x)));
  p^.insert(new(psyssym,init('Ord',in_ord_x)));
  p^.insert(new(psyssym,init('Pred',in_pred_x)));
  p^.insert(new(psyssym,init('Succ',in_succ_x)));
  p^.insert(new(psyssym,init('Exclude',in_exclude_x_y)));
  p^.insert(new(psyssym,init('Include',in_include_x_y)));
  p^.insert(new(psyssym,init('Break',in_break)));
  p^.insert(new(psyssym,init('Continue',in_continue)));
  p^.insert(new(psyssym,init('Dec',in_dec_x)));
  p^.insert(new(psyssym,init('Inc',in_inc_x)));
  p^.insert(new(psyssym,init('Str',in_str_x_string)));
  p^.insert(new(psyssym,init('Assert',in_assert_x_y)));
  p^.insert(new(psyssym,init('Val',in_val_x)));
  p^.insert(new(psyssym,init('Addr',in_addr_x)));
  p^.insert(new(psyssym,init('TypeInfo',in_typeinfo_x)));
end;


procedure insert_intern_types(p : psymtable);
{
  all the types inserted into the system unit
}
var
  { several defs to simulate more or less C++ objects for GDB }
  vmtdef      : precorddef;
  vmtarraydef : parraydef;
  vmtsymtable : psymtable;
begin
{ Internal types }
  p^.insert(new(ptypesym,initdef('$formal',cformaldef)));
  p^.insert(new(ptypesym,initdef('$void',voiddef)));
  p^.insert(new(ptypesym,initdef('$byte',u8bitdef)));
  p^.insert(new(ptypesym,initdef('$word',u16bitdef)));
  p^.insert(new(ptypesym,initdef('$ulong',u32bitdef)));
  p^.insert(new(ptypesym,initdef('$longint',s32bitdef)));
  p^.insert(new(ptypesym,initdef('$qword',cu64bitdef)));
  p^.insert(new(ptypesym,initdef('$int64',cs64bitdef)));
  p^.insert(new(ptypesym,initdef('$char',cchardef)));
  p^.insert(new(ptypesym,initdef('$widechar',cwidechardef)));
  p^.insert(new(ptypesym,initdef('$shortstring',cshortstringdef)));
  p^.insert(new(ptypesym,initdef('$longstring',clongstringdef)));
  p^.insert(new(ptypesym,initdef('$ansistring',cansistringdef)));
  p^.insert(new(ptypesym,initdef('$widestring',cwidestringdef)));
  p^.insert(new(ptypesym,initdef('$openshortstring',openshortstringdef)));
  p^.insert(new(ptypesym,initdef('$boolean',booldef)));
  p^.insert(new(ptypesym,initdef('$void_pointer',voidpointerdef)));
  p^.insert(new(ptypesym,initdef('$char_pointer',charpointerdef)));
  p^.insert(new(ptypesym,initdef('$void_farpointer',voidfarpointerdef)));
  p^.insert(new(ptypesym,initdef('$openchararray',openchararraydef)));
  p^.insert(new(ptypesym,initdef('$file',cfiledef)));
  p^.insert(new(ptypesym,initdef('$s32real',s32floatdef)));
  p^.insert(new(ptypesym,initdef('$s64real',s64floatdef)));
  p^.insert(new(ptypesym,initdef('$s80real',s80floatdef)));
{$ifdef SUPPORT_FIXED}
  p^.insert(new(ptypesym,initdef('$s32fixed',s32fixeddef)));
{$endif SUPPORT_FIXED}
  { Add a type for virtual method tables in lowercase }
  { so it isn't reachable!                            }
  vmtsymtable:=new(psymtable,init(recordsymtable));
  vmtdef:=new(precorddef,init(vmtsymtable));
  pvmtdef:=new(ppointerdef,initdef(vmtdef));
  vmtsymtable^.insert(new(pvarsym,initdef('$parent',pvmtdef)));
  vmtsymtable^.insert(new(pvarsym,initdef('$length',globaldef('longint'))));
  vmtsymtable^.insert(new(pvarsym,initdef('$mlength',globaldef('longint'))));
  vmtarraydef:=new(parraydef,init(0,1,s32bitdef));
  vmtarraydef^.elementtype.setdef(voidpointerdef);
  vmtsymtable^.insert(new(pvarsym,initdef('$__pfn',vmtarraydef)));
  p^.insert(new(ptypesym,initdef('$__vtbl_ptr_type',vmtdef)));
  p^.insert(new(ptypesym,initdef('$pvmt',pvmtdef)));
  vmtarraydef:=new(parraydef,init(0,1,s32bitdef));
  vmtarraydef^.elementtype.setdef(pvmtdef);
  p^.insert(new(ptypesym,initdef('$vtblarray',vmtarraydef)));
  insertinternsyms(p);
{ Normal types }
  p^.insert(new(ptypesym,initdef('Single',s32floatdef)));
  p^.insert(new(ptypesym,initdef('Double',s64floatdef)));
  p^.insert(new(ptypesym,initdef('Extended',s80floatdef)));
  p^.insert(new(ptypesym,initdef('Real',s64floatdef)));
{$ifdef i386}
  p^.insert(new(ptypesym,initdef('Comp',new(pfloatdef,init(s64comp)))));
{$endif}
  p^.insert(new(ptypesym,initdef('Pointer',voidpointerdef)));
  p^.insert(new(ptypesym,initdef('FarPointer',voidfarpointerdef)));
  p^.insert(new(ptypesym,initdef('ShortString',cshortstringdef)));
  p^.insert(new(ptypesym,initdef('LongString',clongstringdef)));
  p^.insert(new(ptypesym,initdef('AnsiString',cansistringdef)));
  p^.insert(new(ptypesym,initdef('WideString',cwidestringdef)));
  p^.insert(new(ptypesym,initdef('Boolean',booldef)));
  p^.insert(new(ptypesym,initdef('ByteBool',booldef)));
  p^.insert(new(ptypesym,initdef('WordBool',new(porddef,init(bool16bit,0,1)))));
  p^.insert(new(ptypesym,initdef('LongBool',new(porddef,init(bool32bit,0,1)))));
  p^.insert(new(ptypesym,initdef('Char',cchardef)));
  p^.insert(new(ptypesym,initdef('WideChar',cwidechardef)));
  p^.insert(new(ptypesym,initdef('Text',new(pfiledef,inittext))));
  p^.insert(new(ptypesym,initdef('Cardinal',u32bitdef)));
{$ifdef SUPPORT_FIXED}
  p^.insert(new(ptypesym,initdef('Fixed',new(pfloatdef,init(f32bit)))));
  p^.insert(new(ptypesym,initdef('Fixed16',new(pfloatdef,init(f16bit)))));
{$endif SUPPORT_FIXED}
  p^.insert(new(ptypesym,initdef('QWord',cu64bitdef)));
  p^.insert(new(ptypesym,initdef('Int64',cs64bitdef)));
  p^.insert(new(ptypesym,initdef('TypedFile',new(pfiledef,inittypeddef(voiddef)))));
end;


procedure readconstdefs;
{
  Load all default definitions for consts from the system unit
}
begin
  u8bitdef:=porddef(globaldef('byte'));
  u16bitdef:=porddef(globaldef('word'));
  u32bitdef:=porddef(globaldef('ulong'));
  s32bitdef:=porddef(globaldef('longint'));
  cu64bitdef:=porddef(globaldef('qword'));
  cs64bitdef:=porddef(globaldef('int64'));
  cformaldef:=pformaldef(globaldef('formal'));
  voiddef:=porddef(globaldef('void'));
  cchardef:=porddef(globaldef('char'));
  cwidechardef:=porddef(globaldef('char'));
  cshortstringdef:=pstringdef(globaldef('shortstring'));
  clongstringdef:=pstringdef(globaldef('longstring'));
  cansistringdef:=pstringdef(globaldef('ansistring'));
  cwidestringdef:=pstringdef(globaldef('widestring'));
  openshortstringdef:=pstringdef(globaldef('openshortstring'));
  openchararraydef:=parraydef(globaldef('openchararray'));
  s32floatdef:=pfloatdef(globaldef('s32real'));
  s64floatdef:=pfloatdef(globaldef('s64real'));
  s80floatdef:=pfloatdef(globaldef('s80real'));
{$ifdef SUPPORT_FIXED}
  s32fixeddef:=pfloatdef(globaldef('s32fixed'));
{$endif SUPPORT_FIXED}
  booldef:=porddef(globaldef('boolean'));
  voidpointerdef:=ppointerdef(globaldef('void_pointer'));
  charpointerdef:=ppointerdef(globaldef('char_pointer'));
  voidfarpointerdef:=ppointerdef(globaldef('void_farpointer'));
  cfiledef:=pfiledef(globaldef('file'));
  pvmtdef:=ppointerdef(globaldef('pvmt'));
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
  cformaldef:=new(pformaldef,init);
  voiddef:=new(porddef,init(uvoid,0,0));
  u8bitdef:=new(porddef,init(u8bit,0,255));
  u16bitdef:=new(porddef,init(u16bit,0,65535));
  u32bitdef:=new(porddef,init(u32bit,0,$ffffffff));
  s32bitdef:=new(porddef,init(s32bit,$80000000,$7fffffff));
  cu64bitdef:=new(porddef,init(u64bit,0,0));
  cs64bitdef:=new(porddef,init(s64bit,0,0));
  booldef:=new(porddef,init(bool8bit,0,1));
  cchardef:=new(porddef,init(uchar,0,255));
  cwidechardef:=new(porddef,init(uwidechar,0,65535));
  cshortstringdef:=new(pstringdef,shortinit(255));
  { should we give a length to the default long and ansi string definition ?? }
  clongstringdef:=new(pstringdef,longinit(-1));
  cansistringdef:=new(pstringdef,ansiinit(-1));
  cwidestringdef:=new(pstringdef,wideinit(-1));
  { length=0 for shortstring is open string (needed for readln(string) }
  openshortstringdef:=new(pstringdef,shortinit(0));
  openchararraydef:=new(parraydef,init(0,-1,s32bitdef));
  parraydef(openchararraydef)^.elementtype.setdef(cchardef);
{$ifdef i386}
  s32floatdef:=new(pfloatdef,init(s32real));
  s64floatdef:=new(pfloatdef,init(s64real));
  s80floatdef:=new(pfloatdef,init(s80real));
{$endif}
{$ifdef m68k}
  s32floatdef:=new(pfloatdef,init(s32real));
  s64floatdef:=new(pfloatdef,init(s64real));
  if (cs_fp_emulation in aktmoduleswitches) then
   s80floatdef:=new(pfloatdef,init(s32real))
  else
   s80floatdef:=new(pfloatdef,init(s80real));
{$endif}
{$ifdef SUPPORT_FIXED}
  s32fixeddef:=new(pfloatdef,init(f32bit));
{$endif SUPPORT_FIXED}
  { some other definitions }
  voidpointerdef:=new(ppointerdef,initdef(voiddef));
  charpointerdef:=new(ppointerdef,initdef(cchardef));
  voidfarpointerdef:=new(ppointerdef,initfardef(voiddef));
  cfiledef:=new(pfiledef,inituntyped);
  registerdef:=oldregisterdef;
end;


end.
{
  $Log$
  Revision 1.5  2000-09-24 15:06:24  peter
    * use defines.inc

  Revision 1.4  2000/08/27 20:19:39  peter
    * store strings with case in ppu, when an internal symbol is created
      a '$' is prefixed so it's not automatic uppercased

  Revision 1.3  2000/08/16 13:06:06  florian
    + support of 64 bit integer constants

  Revision 1.2  2000/07/13 11:32:47  michael
  + removed logs

}
