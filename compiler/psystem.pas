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

uses tree;

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
  { for testing purpose }
  p^.insert(new(psyssym,init('DECI',in_dec_x)));
  p^.insert(new(psyssym,init('INCI',in_inc_x)));
  p^.insert(new(psyssym,init('STR',in_str_x_string)));
end;


procedure insert_intern_types(p : psymtable);
{
  all the types inserted into the system unit
}
{$ifdef GDB}
var
  { several defs to simulate more or less C++ objects for GDB }
  vmtdef      : precdef;
  pvmtdef     : ppointerdef;
  vmtarraydef : parraydef;
  vmtsymtable : psymtable;
{$endif GDB}
begin
  p^.insert(new(ptypesym,init('longint',s32bitdef)));
  p^.insert(new(ptypesym,init('ulong',u32bitdef)));
  p^.insert(new(ptypesym,init('void',voiddef)));
  p^.insert(new(ptypesym,init('char',cchardef)));
{$ifdef i386}
  p^.insert(new(ptypesym,init('s64real',c64floatdef)));
{$endif i386}
  p^.insert(new(ptypesym,init('s80real',s80floatdef)));
  p^.insert(new(ptypesym,init('cs32fixed',s32fixeddef)));
  p^.insert(new(ptypesym,init('byte',u8bitdef)));
  p^.insert(new(ptypesym,init('string',cstringdef)));
  p^.insert(new(ptypesym,init('longstring',clongstringdef)));
  p^.insert(new(ptypesym,init('ansistring',cansistringdef)));
  p^.insert(new(ptypesym,init('widestring',cwidestringdef)));
  p^.insert(new(ptypesym,init('word',u16bitdef)));
  p^.insert(new(ptypesym,init('boolean',booldef)));
  p^.insert(new(ptypesym,init('void_pointer',voidpointerdef)));
  p^.insert(new(ptypesym,init('file',cfiledef)));
{$ifdef i386}
  p^.insert(new(ptypesym,init('REAL',new(pfloatdef,init(s64real)))));
  p^.insert(new(ptypesym,init('COMP',new(pfloatdef,init(s64bit)))));
  p^.insert(new(ptypesym,init('EXTENDED',new(pfloatdef,init(s80real)))));
{$endif}
{$ifdef m68k}
  { internal definitions }
  p^.insert(new(ptypesym,init('s32real',c64floatdef)));
  { mappings... }
  p^.insert(new(ptypesym,init('REAL',new(pfloatdef,init(s32real)))));
  if (cs_fp_emulation) in aktswitches then
    p^.insert(new(ptypesym,init('DOUBLE',new(pfloatdef,init(s32real)))))
  else
    p^.insert(new(ptypesym,init('DOUBLE',new(pfloatdef,init(s64real)))));
  if (cs_fp_emulation) in aktswitches then
    p^.insert(new(ptypesym,init('EXTENDED',new(pfloatdef,init(s32real)))))
  else
    p^.insert(new(ptypesym,init('EXTENDED',new(pfloatdef,init(s80real)))));
{  p^.insert(new(ptypesym,init('COMP',new(pfloatdef,init(s32real)))));}
{$endif}
  p^.insert(new(ptypesym,init('SINGLE',new(pfloatdef,init(s32real)))));
  p^.insert(new(ptypesym,init('POINTER',new(ppointerdef,init(voiddef)))));
  p^.insert(new(ptypesym,init('STRING',cstringdef)));
  p^.insert(new(ptypesym,init('LONGSTRING',clongstringdef)));
  p^.insert(new(ptypesym,init('ANSISTRING',cansistringdef)));
  p^.insert(new(ptypesym,init('WIDESTRING',cwidestringdef)));
  p^.insert(new(ptypesym,init('BYTEBOOL',new(porddef,init(bool8bit,0,1)))));
  p^.insert(new(ptypesym,init('WORDBOOL',new(porddef,init(bool16bit,0,1)))));
  p^.insert(new(ptypesym,init('LONGBOOL',new(porddef,init(bool32bit,0,1)))));
  p^.insert(new(ptypesym,init('CHAR',new(porddef,init(uchar,0,255)))));
  p^.insert(new(ptypesym,init('TEXT',new(pfiledef,init(ft_text,nil)))));
  p^.insert(new(ptypesym,init('CARDINAL',new(porddef,init(u32bit,0,$ffffffff)))));
  p^.insert(new(ptypesym,init('FIXED',new(pfloatdef,init(f32bit)))));
  p^.insert(new(ptypesym,init('FIXED16',new(pfloatdef,init(f16bit)))));
  p^.insert(new(ptypesym,init('TYPEDFILE',new(pfiledef,init(ft_typed,voiddef)))));
  { !!!!!
  p^.insert(new(ptypesym,init('COMP',new(porddef,init(s64bit,0,0)))));
  p^.insert(new(ptypesym,init('SINGLE',new(porddef,init(s32real,0,0)))));
  p^.insert(new(ptypesym,init('EXTENDED',new(porddef,init(s80real,0,0)))));
  p^.insert(new(ptypesym,init('FILE',new(pfiledef,init(ft_untyped,nil)))));
  }
  { Add a type for virtual method tables in lowercase }
  { so it isn't reachable!                            }
{$ifdef GDB}
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
{$endif GDB}
  insertinternsyms(p);
end;


procedure readconstdefs;
{
  Load all default definitions for consts from the system unit
}
begin
  s32bitdef:=porddef(globaldef('longint'));
  u32bitdef:=porddef(globaldef('ulong'));
  cstringdef:=pstringdef(globaldef('string'));
  clongstringdef:=pstringdef(globaldef('longstring'));
  cansistringdef:=pstringdef(globaldef('ansistring'));
  cwidestringdef:=pstringdef(globaldef('widestring'));
  cchardef:=porddef(globaldef('char'));
{$ifdef i386}
  c64floatdef:=pfloatdef(globaldef('s64real'));
{$endif}
{$ifdef m68k}
  c64floatdef:=pfloatdef(globaldef('s32real'));
{$endif m68k}
  s80floatdef:=pfloatdef(globaldef('s80real'));
  s32fixeddef:=pfloatdef(globaldef('cs32fixed'));
  voiddef:=porddef(globaldef('void'));
  u8bitdef:=porddef(globaldef('byte'));
  u16bitdef:=porddef(globaldef('word'));
  booldef:=porddef(globaldef('boolean'));
  voidpointerdef:=ppointerdef(globaldef('void_pointer'));
  cfiledef:=pfiledef(globaldef('file'));
end;


procedure createconstdefs;
{
  Create all default definitions for consts for the system unit
}
begin
  { create definitions for constants }
  registerdef:=false;
  voiddef:=new(porddef,init(uvoid,0,0));
  u8bitdef:=new(porddef,init(u8bit,0,255));
  u16bitdef:=new(porddef,init(u16bit,0,65535));
  u32bitdef:=new(porddef,init(u32bit,0,$ffffffff));
  s32bitdef:=new(porddef,init(s32bit,$80000000,$7fffffff));
  booldef:=new(porddef,init(bool8bit,0,1));
  cchardef:=new(porddef,init(uchar,0,255));
  cstringdef:=new(pstringdef,init(255));
  { should we give a length to the default long and ansi string definition ?? }
  clongstringdef:=new(pstringdef,longinit(-1));
  cansistringdef:=new(pstringdef,ansiinit(-1));
  cwidestringdef:=new(pstringdef,wideinit(-1));
{$ifdef i386}
  c64floatdef:=new(pfloatdef,init(s64real));
  s80floatdef:=new(pfloatdef,init(s80real));
{$endif}
{$ifdef m68k}
  c64floatdef:=new(pfloatdef,init(s32real));
  if (cs_fp_emulation in aktswitches) then
   s80floatdef:=new(pfloatdef,init(s32real))
  else
   s80floatdef:=new(pfloatdef,init(s80real));
{$endif}
  s32fixeddef:=new(pfloatdef,init(f32bit));
  { some other definitions }
  voidpointerdef:=new(ppointerdef,init(voiddef));
  cfiledef:=new(pfiledef,init(ft_untyped,nil));
end;


end.
{
  $Log$
  Revision 1.1  1998-06-03 22:49:01  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

}
