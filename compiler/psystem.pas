{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
interface
uses symtable;

procedure insertinternsyms(p : psymtable);
procedure insert_intern_types(p : psymtable);

procedure readconstdefs;
procedure createconstdefs;

implementation

uses
  globtype,globals,tree;

procedure insertinternsyms(p : psymtable);
{
  all intern procedures for system unit
}
begin
  p^.insert(new(psyssym,init('CONCAT',in_concat_x)));
  p^.insert(new(psyssym,init('WRITE',in_write_x)));
  p^.insert(new(psyssym,init('WRITELN',in_writeln_x)));
  p^.insert(new(psyssym,init('ASSIGNED',in_assigned_x)));
  p^.insert(new(psyssym,init('READ',in_read_x)));
  p^.insert(new(psyssym,init('READLN',in_readln_x)));
  p^.insert(new(psyssym,init('OFS',in_ofs_x)));
  p^.insert(new(psyssym,init('SIZEOF',in_sizeof_x)));
  p^.insert(new(psyssym,init('TYPEOF',in_typeof_x)));
  p^.insert(new(psyssym,init('LOW',in_low_x)));
  p^.insert(new(psyssym,init('HIGH',in_high_x)));
  p^.insert(new(psyssym,init('SEG',in_seg_x)));
  p^.insert(new(psyssym,init('ORD',in_ord_x)));
  p^.insert(new(psyssym,init('PRED',in_pred_x)));
  p^.insert(new(psyssym,init('SUCC',in_succ_x)));
  p^.insert(new(psyssym,init('EXCLUDE',in_exclude_x_y)));
  p^.insert(new(psyssym,init('INCLUDE',in_include_x_y)));
  p^.insert(new(psyssym,init('BREAK',in_break)));
  p^.insert(new(psyssym,init('CONTINUE',in_continue)));
  p^.insert(new(psyssym,init('DEC',in_dec_x)));
  p^.insert(new(psyssym,init('INC',in_inc_x)));
  p^.insert(new(psyssym,init('STR',in_str_x_string)));
  p^.insert(new(psyssym,init('ASSERT',in_assert_x_y)));
{$IfnDef OLDVAL}
  p^.insert(new(psyssym,init('VAL',in_val_x)));
{$EndIf OLDVAL}
  p^.insert(new(psyssym,init('ADDR',in_addr_x)));
end;


procedure insert_intern_types(p : psymtable);
{
  all the types inserted into the system unit
}
var
  { several defs to simulate more or less C++ objects for GDB }
  vmtdef      : precdef;
  pvmtdef     : ppointerdef;
  vmtarraydef : parraydef;
  vmtsymtable : psymtable;
begin
{ Internal types }
  p^.insert(new(ptypesym,init('formal',cformaldef)));
  p^.insert(new(ptypesym,init('void',voiddef)));
  p^.insert(new(ptypesym,init('byte',u8bitdef)));
  p^.insert(new(ptypesym,init('word',u16bitdef)));
  p^.insert(new(ptypesym,init('ulong',u32bitdef)));
  p^.insert(new(ptypesym,init('longint',s32bitdef)));
{$ifdef INT64}
  p^.insert(new(ptypesym,init('qword',cu64bitdef)));
  p^.insert(new(ptypesym,init('int64',cs64bitintdef)));
{$endif INT64}
  p^.insert(new(ptypesym,init('char',cchardef)));
  p^.insert(new(ptypesym,init('shortstring',cshortstringdef)));
  p^.insert(new(ptypesym,init('longstring',clongstringdef)));
  p^.insert(new(ptypesym,init('ansistring',cansistringdef)));
  p^.insert(new(ptypesym,init('widestring',cwidestringdef)));
  p^.insert(new(ptypesym,init('openshortstring',openshortstringdef)));
  p^.insert(new(ptypesym,init('boolean',booldef)));
  p^.insert(new(ptypesym,init('void_pointer',voidpointerdef)));
  p^.insert(new(ptypesym,init('char_pointer',charpointerdef)));
  p^.insert(new(ptypesym,init('void_farpointer',voidfarpointerdef)));
  p^.insert(new(ptypesym,init('openchararray',openchararraydef)));
  p^.insert(new(ptypesym,init('file',cfiledef)));
  p^.insert(new(ptypesym,init('s32real',s32floatdef)));
  p^.insert(new(ptypesym,init('s64real',s64floatdef)));
  p^.insert(new(ptypesym,init('s80real',s80floatdef)));
  p^.insert(new(ptypesym,init('s32fixed',s32fixeddef)));
  { Add a type for virtual method tables in lowercase }
  { so it isn't reachable!                            }
  vmtsymtable:=new(psymtable,init(recordsymtable));
  vmtdef:=new(precdef,init(vmtsymtable));
  pvmtdef:=new(ppointerdef,init(vmtdef));
  vmtsymtable^.insert(new(pvarsym,init('parent',pvmtdef)));
  vmtsymtable^.insert(new(pvarsym,init('length',globaldef('longint'))));
  vmtsymtable^.insert(new(pvarsym,init('mlength',globaldef('longint'))));
  vmtarraydef:=new(parraydef,init(0,1,s32bitdef));
  vmtarraydef^.definition := voidpointerdef;
  vmtsymtable^.insert(new(pvarsym,init('__pfn',vmtarraydef)));
  p^.insert(new(ptypesym,init('__vtbl_ptr_type',vmtdef)));
  p^.insert(new(ptypesym,init('pvmt',pvmtdef)));
  vmtarraydef:=new(parraydef,init(0,1,s32bitdef));
  vmtarraydef^.definition := pvmtdef;
  p^.insert(new(ptypesym,init('vtblarray',vmtarraydef)));
  insertinternsyms(p);
{ Normal types }
  p^.insert(new(ptypesym,init('SINGLE',s32floatdef)));
  p^.insert(new(ptypesym,init('DOUBLE',s64floatdef)));
  p^.insert(new(ptypesym,init('EXTENDED',s80floatdef)));
  p^.insert(new(ptypesym,init('REAL',s64floatdef)));
{$ifdef i386}
  p^.insert(new(ptypesym,init('COMP',new(pfloatdef,init(s64comp)))));
{$endif}
  p^.insert(new(ptypesym,init('POINTER',voidpointerdef)));
  p^.insert(new(ptypesym,init('FARPOINTER',voidfarpointerdef)));
{  p^.insert(new(ptypesym,init('STRING',cshortstringdef))); }
  p^.insert(new(ptypesym,init('SHORTSTRING',cshortstringdef)));
  p^.insert(new(ptypesym,init('LONGSTRING',clongstringdef)));
  p^.insert(new(ptypesym,init('ANSISTRING',cansistringdef)));
  p^.insert(new(ptypesym,init('WIDESTRING',cwidestringdef)));
  p^.insert(new(ptypesym,init('BOOLEAN',booldef)));
  p^.insert(new(ptypesym,init('BYTEBOOL',booldef)));
  p^.insert(new(ptypesym,init('WORDBOOL',new(porddef,init(bool16bit,0,1)))));
  p^.insert(new(ptypesym,init('LONGBOOL',new(porddef,init(bool32bit,0,1)))));
  p^.insert(new(ptypesym,init('CHAR',cchardef)));
  p^.insert(new(ptypesym,init('TEXT',new(pfiledef,init(ft_text,nil)))));
  p^.insert(new(ptypesym,init('CARDINAL',u32bitdef)));
  p^.insert(new(ptypesym,init('FIXED',new(pfloatdef,init(f32bit)))));
  p^.insert(new(ptypesym,init('FIXED16',new(pfloatdef,init(f16bit)))));
{$ifdef INT64}
  p^.insert(new(ptypesym,init('QWORD',cu64bitdef)));
  p^.insert(new(ptypesym,init('INT64',cs64bitintdef)));
{$endif INT64}
  p^.insert(new(ptypesym,init('TYPEDFILE',new(pfiledef,init(ft_typed,voiddef)))));
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
{$ifdef INT64}
  cu64bitdef:=porddef(globaldef('qword'));
  cs64bitintdef:=porddef(globaldef('int64'));
{$endif INT64}
  cformaldef:=pformaldef(globaldef('formal'));
  voiddef:=porddef(globaldef('void'));
  cchardef:=porddef(globaldef('char'));
  cshortstringdef:=pstringdef(globaldef('shortstring'));
  clongstringdef:=pstringdef(globaldef('longstring'));
  cansistringdef:=pstringdef(globaldef('ansistring'));
  cwidestringdef:=pstringdef(globaldef('widestring'));
  openshortstringdef:=pstringdef(globaldef('openshortstring'));
  openchararraydef:=parraydef(globaldef('openchararray'));
  s32floatdef:=pfloatdef(globaldef('s32real'));
  s64floatdef:=pfloatdef(globaldef('s64real'));
  s80floatdef:=pfloatdef(globaldef('s80real'));
  s32fixeddef:=pfloatdef(globaldef('s32fixed'));
  booldef:=porddef(globaldef('boolean'));
  voidpointerdef:=ppointerdef(globaldef('void_pointer'));
  charpointerdef:=ppointerdef(globaldef('char_pointer'));
  voidfarpointerdef:=ppointerdef(globaldef('void_farpointer'));
  cfiledef:=pfiledef(globaldef('file'));
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
{$ifdef INT64}
  cu64bitdef:=new(porddef,init(u64bit,0,0));
  cs64bitintdef:=new(porddef,init(s64bitint,0,0));
{$endif INT64}
  booldef:=new(porddef,init(bool8bit,0,1));
  cchardef:=new(porddef,init(uchar,0,255));
  cshortstringdef:=new(pstringdef,shortinit(255));
  { should we give a length to the default long and ansi string definition ?? }
  clongstringdef:=new(pstringdef,longinit(-1));
  cansistringdef:=new(pstringdef,ansiinit(-1));
  cwidestringdef:=new(pstringdef,wideinit(-1));
  { length=0 for shortstring is open string (needed for readln(string) }
  openshortstringdef:=new(pstringdef,shortinit(0));
  openchararraydef:=new(parraydef,init(0,-1,s32bitdef));
  parraydef(openchararraydef)^.definition:=cchardef;
{$ifdef i386}
  s32floatdef:=new(pfloatdef,init(s32real));
  s64floatdef:=new(pfloatdef,init(s64real));
  s80floatdef:=new(pfloatdef,init(s80real));
{$endif}
{$ifdef m68k}
  s32floatdef:=new(pfloatdef,init(s32real))
  s64floatdef:=new(pfloatdef,init(s32real));
  if (cs_fp_emulation in aktmoduleswitches) then
   s80floatdef:=new(pfloatdef,init(s32real))
  else
   s80floatdef:=new(pfloatdef,init(s80real));
{$endif}
  s32fixeddef:=new(pfloatdef,init(f32bit));
  { some other definitions }
  voidpointerdef:=new(ppointerdef,init(voiddef));
  charpointerdef:=new(ppointerdef,init(cchardef));
  voidfarpointerdef:=new(ppointerdef,initfar(voiddef));
  cfiledef:=new(pfiledef,init(ft_untyped,nil));
  registerdef:=oldregisterdef;
end;


end.
{
  $Log$
  Revision 1.23  1999-05-12 00:19:53  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.22  1999/05/06 09:05:23  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.21  1999/04/26 18:28:15  peter
    * better read/write array

  Revision 1.20  1999/04/17 13:12:20  peter
    * addr() internal

  Revision 1.19  1999/04/07 15:31:12  pierre
    * all formaldefs are now a sinlge definition
      cformaldef (this was necessary for double_checksum)
    + small part of double_checksum code

  Revision 1.18  1999/03/26 00:05:40  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.17  1999/03/16 17:52:54  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in cgi386inl: also range checking for subrange types (compile with "-dreadrangecheck")
    * in cgai386: also small fixes to emitrangecheck

  Revision 1.16  1999/03/02 02:56:17  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.15  1998/12/30 22:15:51  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.14  1998/12/11 00:03:40  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.13  1998/12/10 09:47:25  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.12  1998/11/27 14:50:45  peter
    + open strings, $P switch support

  Revision 1.11  1998/11/16 10:18:09  peter
    * fixes for ansistrings

  Revision 1.10  1998/11/09 11:44:36  peter
    + va_list for printf support

  Revision 1.9  1998/11/05 12:02:54  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.8  1998/11/04 10:11:44  peter
    * ansistring fixes

  Revision 1.7  1998/10/05 12:32:48  peter
    + assert() support

  Revision 1.6  1998/09/24 23:49:17  peter
    + aktmodeswitches

  Revision 1.5  1998/08/10 14:50:19  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.4  1998/06/25 14:04:24  peter
    + internal inc/dec

  Revision 1.3  1998/06/04 23:51:55  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.2  1998/06/04 08:23:57  pierre
    * boolean again intern declared (needed to be able to compile
      older RTL's)

  Revision 1.1  1998/06/03 22:49:01  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

}
