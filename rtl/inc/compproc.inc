{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains the declarations of internal compiler helper
    routines. That means you can *NOT* call these directly, as they may
    be changed or even removed at any time. The only reason they are
    included in the interface of the system unit, is so that the
    compiler doesn't need special code to access their parameter
    list information etc.

    Note that due to the "compilerproc" directive, it isn't even possible
    to use these routines in your programs.

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ some dummy types necessary to have generic resulttypes for certain compilerprocs }
type
  fpc_big_chararray = array[0..maxlongint] of char;
  fpc_small_set = longint;
  fpc_normal_set = array[0..7] of longint;

{$ifdef hascompilerproc}

{ Needed to solve overloading problem with call from assembler (PFV) }
{$ifdef valuegetmem}
Function  fpc_getmem(size:Longint):pointer;compilerproc;
{$endif}
{$ifdef valuefreemem}
Procedure fpc_freemem(p:pointer);compilerproc;
{$endif valuefreemem}

procedure fpc_Shortstr_SetLength(var s:shortstring;len:StrLenInt); compilerproc;
function fpc_shortstr_to_shortstr(len:longint; const sstr: shortstring): shortstring; compilerproc;
function fpc_shortstr_concat(const s1,s2:shortstring): shortstring; compilerproc;
function fpc_shortstr_compare(const dstr,sstr:shortstring) : longint; compilerproc;
function fpc_pchar_to_shortstr(p:pchar):shortstring; compilerproc;

function fpc_chararray_to_shortstr(const arr: array of char):shortstring; compilerproc;
function fpc_shortstr_to_chararray(arraysize: longint; const src: ShortString): fpc_big_chararray; compilerproc;


function fpc_dynarray_length(p : pointer) : tdynarrayindex; compilerproc;
function fpc_dynarray_high(p : pointer) : tdynarrayindex; compilerproc;
procedure fpc_dynarray_clear(var p : pointer;ti : pointer); compilerproc;
procedure fpc_dynarray_decr_ref(var p : pointer;ti : pointer); compilerproc;
procedure fpc_dynarray_incr_ref(p : pointer); compilerproc;
procedure fpc_dynarray_setlength(var p : pointer;pti : pointer;
  dimcount : dword;dims : pdynarrayindex); compilerproc;
function fpc_dynarray_copy(var p : pointer;ti : pointer;
  dimcount : dword;dims : pdynarrayindex) : pointer; compilerproc;

procedure fpc_ShortStr_Float(d : ValReal;len,fr,rt : longint;var s : shortstring); compilerproc;
procedure fpc_ShortStr_Longint(v : longint;len : longint;var s : shortstring); compilerproc;
procedure fpc_shortstr_cardinal(v : cardinal;len : longint;var s : shortstring); compilerproc;
Function fpc_Val_SInt_ShortStr(DestSize: longint; Const S: ShortString; var Code: ValSInt): ValSInt; compilerproc;
Function fpc_Val_UInt_Shortstr(Const S: ShortString; var Code: ValSInt): ValUInt; compilerproc;
Function fpc_Val_Real_ShortStr(const s : shortstring; var code : ValSInt): ValReal; compilerproc;

Procedure fpc_AnsiStr_Decr_Ref (Var S : Pointer); compilerproc;
Procedure fpc_AnsiStr_Incr_Ref (S : Pointer); compilerproc;
Procedure fpc_AnsiStr_Assign (Var S1 : Pointer;S2 : Pointer); compilerproc;
function fpc_AnsiStr_Concat (const S1,S2 : AnsiString): AnsiString; compilerproc;
{$ifdef EXTRAANSISHORT}
Procedure fpc_AnsiStr_ShortStr_Concat (Var S1: AnsiString; Var S2 : ShortString); compilerproc;
{$endif EXTRAANSISHORT}
function fpc_AnsiStr_To_ShortStr (high_of_res: longint;const S2 : Ansistring): shortstring; compilerproc;
Function fpc_ShortStr_To_AnsiStr (Const S2 : ShortString): ansistring; compilerproc;
Function fpc_Char_To_AnsiStr(const c : Char): AnsiString; compilerproc;
Function fpc_PChar_To_AnsiStr(const p : pchar): ansistring; compilerproc;
Function fpc_CharArray_To_AnsiStr(const arr: array of char): ansistring; compilerproc;
function fpc_ansistr_to_chararray(arraysize: longint; const src: ansistring): fpc_big_chararray; compilerproc;
Function fpc_AnsiStr_Compare(const S1,S2 : AnsiString): Longint; compilerproc;
Procedure fpc_AnsiStr_CheckZero(p : pointer); compilerproc;
Procedure fpc_AnsiStr_CheckRange(len,index : longint); compilerproc;
Procedure fpc_AnsiStr_SetLength (Var S : AnsiString; l : Longint); compilerproc;
{$ifdef EXTRAANSISHORT}
Function fpc_AnsiStr_ShortStr_Compare (Var S1 : Pointer; Var S2 : ShortString): Longint; compilerproc;
{$endif EXTRAANSISHORT}
Procedure fpc_ansistr_Unique(Var S : AnsiString); compilerproc;

Procedure fpc_WideStr_Decr_Ref (Var S : Pointer); compilerproc;
Procedure fpc_WideStr_Incr_Ref (S : Pointer); compilerproc;
function fpc_WideStr_To_ShortStr (high_of_res: longint;const S2 : WideString): shortstring; compilerproc;
Function fpc_ShortStr_To_WideStr (Const S2 : ShortString): WideString; compilerproc;
Function fpc_WideStr_To_AnsiStr (const S2 : WideString): AnsiString; compilerproc;
Function fpc_AnsiStr_To_WideStr (Const S2 : AnsiString): WideString; compilerproc;
Procedure fpc_WideStr_Assign (Var S1 : Pointer;S2 : Pointer); compilerproc;
Function fpc_WideStr_Concat (const S1,S2 : WideString) : WideString; compilerproc;
Function fpc_Char_To_WideStr(const c : Char): WideString; compilerproc;
Function fpc_PChar_To_WideStr(const p : pchar): WideString; compilerproc;
Function fpc_CharArray_To_WideStr(const arr: array of char): WideString; compilerproc;
function fpc_widestr_to_chararray(arraysize: longint; const src: WideString): fpc_big_chararray; compilerproc;
Function fpc_WideStr_Compare(const S1,S2 : WideString): Longint; compilerproc;
Procedure fpc_WideStr_CheckZero(p : pointer); compilerproc;
Procedure fpc_WideStr_CheckRange(len,index : longint); compilerproc;
Procedure fpc_WideStr_SetLength (Var S : WideString; l : Longint); compilerproc;
Procedure fpc_widestr_Unique(Var S : WideString); compilerproc;

Function fpc_Val_Real_AnsiStr(Const S : AnsiString; Var Code : ValSInt): ValReal; compilerproc;
Function fpc_Val_UInt_AnsiStr (Const S : AnsiString; Var Code : ValSInt): ValUInt; compilerproc;
Function fpc_Val_SInt_AnsiStr (DestSize: longint; Const S : AnsiString; Var Code : ValSInt): ValSInt; compilerproc;
Function fpc_Val_qword_AnsiStr (Const S : AnsiString; Var Code : ValSInt): qword;compilerproc;
Function fpc_Val_int64_AnsiStr (Const S : AnsiString; Var Code : ValSInt): Int64; compilerproc;
procedure fpc_AnsiStr_Float(d : ValReal;len,fr,rt : longint;var s : ansistring); compilerproc;
Procedure fpc_AnsiStr_Cardinal(C : Cardinal;Len : Longint; Var S : AnsiString); compilerproc;
Procedure fpc_AnsiStr_Longint(L : Longint; Len : Longint; Var S : AnsiString); compilerproc;

Function fpc_Val_Real_WideStr(Const S : WideString; Var Code : ValSInt): ValReal; compilerproc;
Function fpc_Val_UInt_WideStr (Const S : WideString; Var Code : ValSInt): ValUInt; compilerproc;
Function fpc_Val_SInt_WideStr (DestSize: longint; Const S : WideString; Var Code : ValSInt): ValSInt; compilerproc;
Function fpc_Val_qword_WideStr (Const S : WideString; Var Code : ValSInt): qword; compilerproc;
Function fpc_Val_int64_WideStr (Const S : WideString; Var Code : ValSInt): Int64; compilerproc;
procedure fpc_WideStr_Float(d : ValReal;len,fr,rt : longint;var s : WideString); compilerproc;
Procedure fpc_WideStr_Cardinal(C : Cardinal;Len : Longint; Var S : WideString); compilerproc;
Procedure fpc_WideStr_Longint(L : Longint; Len : Longint; Var S : WideString); compilerproc;

{ from text.inc }
Procedure fpc_Write_End(var f:Text); compilerproc;
Procedure fpc_Writeln_End(var f:Text); compilerproc;
Procedure fpc_Write_Text_ShortStr(Len : Longint;var f : Text;const s : String); compilerproc;
Procedure fpc_Write_Text_Pchar_as_Array(Len : Longint;var f : Text;const s : array of char); compilerproc;
Procedure fpc_Write_Text_PChar_As_Pointer(Len : Longint;var f : Text;p : PChar); compilerproc;
Procedure fpc_Write_Text_AnsiStr (Len : Longint; Var f : Text; S : AnsiString); compilerproc;
{$ifdef HASWIDESTRING}
Procedure fpc_Write_Text_WideStr (Len : Longint; Var f : Text; S : WideString); compilerproc;
{$endif HASWIDESTRING}
Procedure fpc_Write_Text_SInt(Len : Longint;var t : Text;l : ValSInt); compilerproc;
Procedure fpc_Write_Text_UInt(Len : Longint;var t : Text;l : ValUInt); compilerproc;
procedure fpc_write_text_qword(len : longint;var t : text;q : qword); compilerproc;
procedure fpc_write_text_int64(len : longint;var t : text;i : int64); compilerproc;
Procedure fpc_Write_Text_Float(rt,fixkomma,Len : Longint;var t : Text;r : ValReal); compilerproc;
Procedure fpc_Write_Text_Boolean(Len : Longint;var t : Text;b : Boolean); compilerproc;
Procedure fpc_Write_Text_Char(Len : Longint;var t : Text;c : Char); compilerproc;
{$ifdef HASWIDECHAR}
Procedure fpc_Write_Text_WideChar(Len : Longint;var t : Text;c : WideChar); compilerproc;
{$endif HASWIDECHAR}
Procedure fpc_Read_End(var f:Text); compilerproc;
Procedure fpc_ReadLn_End(var f : Text); compilerproc;
Procedure fpc_Read_Text_ShortStr(var f : Text;var s : String); compilerproc;
Procedure fpc_Read_Text_PChar_As_Pointer(var f : Text;var s : PChar); compilerproc;
Procedure fpc_Read_Text_PChar_As_Array(var f : Text;var s : array of char); compilerproc;
Procedure fpc_Read_Text_AnsiStr(var f : Text;var s : AnsiString); compilerproc;
Procedure fpc_Read_Text_Char(var f : Text; var c : char); compilerproc;
Procedure fpc_Read_Text_SInt(var f : Text; var l :ValSInt); compilerproc;
Procedure fpc_Read_Text_UInt(var f : Text; var u :ValUInt); compilerproc;
Procedure fpc_Read_Text_Float(var f : Text; var v :ValReal); compilerproc;
Procedure fpc_Read_Text_QWord(var f : text; var q : qword); compilerproc;
Procedure fpc_Read_Text_Int64(var f : text; var i : int64); compilerproc;

{ from int64.inc }
procedure fpc_shortstr_qword(v : qword;len : longint;var s : shortstring); compilerproc;
procedure fpc_shortstr_int64(v : int64;len : longint;var s : shortstring); compilerproc;
procedure fpc_ansistr_qword(v : qword;len : longint;var s : ansistring); compilerproc;
procedure fpc_ansistr_int64(v : int64;len : longint;var s : ansistring); compilerproc;
Function fpc_val_int64_shortstr(Const S: ShortString; var Code: ValSInt): Int64; compilerproc;
Function fpc_val_qword_shortstr(Const S: ShortString; var Code: ValSInt): QWord; compilerproc;
procedure fpc_widestr_qword(v : qword;len : longint;var s : widestring); compilerproc;
procedure fpc_widestr_int64(v : int64;len : longint;var s : widestring); compilerproc;

function fpc_div_qword(n,z : qword) : qword; compilerproc;
function fpc_mod_qword(n,z : qword) : qword; compilerproc;
function fpc_div_int64(n,z : int64) : int64; compilerproc;
function fpc_mod_int64(n,z : int64) : int64; compilerproc;
function fpc_mul_qword(f1,f2 : qword;checkoverflow : longbool) : qword; compilerproc;
function fpc_mul_int64(f1,f2 : int64;checkoverflow : longbool) : int64; compilerproc;

function fpc_do_is(aclass : tclass;aobject : tobject) : boolean; compilerproc;
function fpc_do_as(aclass : tclass;aobject : tobject): tobject; compilerproc;
procedure fpc_intf_decr_ref(var i: pointer); compilerproc;
procedure fpc_intf_incr_ref(i: pointer); compilerproc;
procedure fpc_intf_assign(var D: pointer; const S: pointer); compilerproc;
procedure fpc_intf_as(var D: pointer; const S: pointer; const iid: TGUID); compilerproc;

Function fpc_PushExceptAddr (Ft: Longint;_buf,_newaddr : pointer): PJmp_buf ; compilerproc;
Procedure fpc_PushExceptObj (Obj : TObject; AnAddr,AFrame : Pointer); compilerproc;
Function fpc_Raiseexception (Obj : TObject; AnAddr,AFrame : Pointer) : TObject; compilerproc;
Procedure fpc_PopAddrStack; compilerproc;
function fpc_PopObjectStack : TObject; compilerproc;
function fpc_PopSecondObjectStack : TObject; compilerproc;
Procedure fpc_ReRaise; compilerproc;
Function fpc_Catches(Objtype : TClass) : TObject; compilerproc;
Procedure fpc_DestroyException(o : TObject); compilerproc;

procedure fpc_help_constructor; compilerproc;
procedure fpc_help_fail; compilerproc;
procedure fpc_help_destructor; compilerproc;
procedure fpc_new_class; compilerproc;
procedure fpc_dispose_class; compilerproc;
procedure fpc_help_fail_class; compilerproc;
procedure fpc_check_object(obj : pointer); compilerproc;
procedure fpc_check_object_ext; compilerproc;

Procedure fpc_Initialize (Data,TypeInfo : pointer); compilerproc;
Procedure fpc_finalize (Data,TypeInfo: Pointer); compilerproc;
Procedure fpc_Addref (Data,TypeInfo : Pointer); compilerproc;
Procedure fpc_DecRef (Data,TypeInfo : Pointer);  compilerproc;
procedure fpc_finalize_array(data,typeinfo : pointer;count,size : longint); compilerproc;

function fpc_set_load_small(l: fpc_small_set): fpc_normal_set; compilerproc;
function fpc_set_create_element(b : byte): fpc_normal_set; compilerproc;
function fpc_set_set_byte(const source: fpc_normal_set; b : byte): fpc_normal_set; compilerproc;
function fpc_set_unset_byte(const source: fpc_normal_set; b : byte): fpc_normal_set; compilerproc;
function fpc_set_set_range(const orgset: fpc_normal_set; l,h : byte): fpc_normal_set; compilerproc;
function fpc_set_in_byte(const p: fpc_normal_set; b: byte): boolean; compilerproc;
function fpc_set_add_sets(const set1,set2: fpc_normal_set): fpc_normal_set; compilerproc;
function fpc_set_mul_sets(const set1,set2: fpc_normal_set): fpc_normal_set; compilerproc;
function fpc_set_sub_sets(const set1,set2: fpc_normal_set): fpc_normal_set; compilerproc;
function fpc_set_symdif_sets(const set1,set2: fpc_normal_set): fpc_normal_set; compilerproc;
function fpc_set_comp_sets(const set1,set2: fpc_normal_set): boolean; compilerproc;
function fpc_set_contains_sets(const set1,set2: fpc_normal_set): boolean; compilerproc;

{$ifdef LARGESETS}
procedure fpc_largeset_set_word(p : pointer;b : word); compilerproc;
procedure fpc_largeset_in_word(p : pointer;b : word); compilerproc;
procedure fpc_largeset_add_sets(set1,set2,dest : pointer;size : longint); compilerproc;
procedure fpc_largeset_sets(set1,set2,dest : pointer;size : longint); compilerproc;
procedure fpc_largeset_sub_sets(set1,set2,dest : pointer;size : longint); compilerproc;
procedure fpc_largeset_symdif_sets(set1,set2,dest : pointer;size : longint); compilerproc;
procedure fpc_largeset_comp_sets(set1,set2 : pointer;size : longint); compilerproc;
procedure fpc_largeset_contains_sets(set1,set2 : pointer; size: longint); compilerproc;
{$endif LARGESETS}

procedure fpc_rangeerror; compilerproc;
procedure fpc_overflow; compilerproc;
procedure fpc_iocheck(addr : longint); compilerproc;

procedure fpc_InitializeUnits; compilerproc;
// not generated by compiler, called directly in system unit
// procedure fpc_FinalizeUnits; compilerproc;

{
Procedure fpc_do_exit; compilerproc;
Procedure fpc_lib_exit; compilerproc;
Procedure fpc_HandleErrorAddrFrame (Errno : longint;addr,frame : longint); compilerproc;
Procedure fpc_HandleError (Errno : longint); compilerproc;
}

procedure fpc_AbstractErrorIntern;compilerproc;
procedure fpc_assert(Const Msg,FName:Shortstring;LineNo,ErrorAddr:Longint); compilerproc;

Procedure fpc_reset_typed(var f : TypedFile;Size : Longint); compilerproc;
Procedure fpc_rewrite_typed(var f : TypedFile;Size : Longint); compilerproc;
Procedure fpc_typed_write(TypeSize : Longint;var f : TypedFile;const Buf); compilerproc;
Procedure fpc_typed_read(TypeSize : Longint;var f : TypedFile;var Buf); compilerproc;

{$endif hascompilerproc}

{
  $Log$
  Revision 1.16  2002-04-25 20:14:56  peter
    * updated compilerprocs
    * incr ref count has now a value argument instead of var

  Revision 1.15  2002/04/24 16:15:35  peter
    * fpc_finalize_array renamed

  Revision 1.14  2002/04/21 18:56:59  peter
    * fpc_freemem and fpc_getmem compilerproc

  Revision 1.13  2002/01/21 20:16:08  peter
    * updated for dynarr:=nil

  Revision 1.12  2001/12/03 21:39:20  peter
    * freemem(var) -> freemem(value)

  Revision 1.11  2001/09/29 21:32:47  jonas
    * almost all second pass typeconvnode helpers are now processor independent
    * fixed converting boolean to int64/qword
    * fixed register allocation bugs which could cause internalerror 10
    * isnode and asnode are completely processor indepent now as well
    * fpc_do_as now returns its class argument (necessary to be able to use it
      properly with compilerproc)

  Revision 1.10  2001/09/05 15:22:09  jonas
    * made multiplying, dividing and mod'ing of int64 and qword processor
      independent with compilerprocs (+ small optimizations by using shift/and
      where possible)

  Revision 1.9  2001/09/04 11:38:55  jonas
    + searchsystype() and searchsystype() functions in symtable
    * changed ninl and nadd to use these functions
    * i386 set comparison functions now return their results in al instead
      of in the flags so that they can be sued as compilerprocs
    - removed all processor specific code from n386add.pas that has to do
      with set handling, it's now all done in nadd.pas
    * fixed fpc_set_contains_sets in genset.inc
    * fpc_set_in_byte is now coded inline in n386set.pas and doesn't use a
      helper anymore
    * some small fixes in compproc.inc/set.inc regarding the declaration of
      internal helper types (fpc_small_set and fpc_normal_set)

  Revision 1.8  2001/09/03 13:27:43  jonas
    * compilerproc implementation of set addition/substraction/...
    * changed the declaration of some set helpers somewhat to accomodate the
      above change
    * i386 still uses the old code for comparisons of sets, because its
      helpers return the results in the flags
    * dummy tc_normal_2_small_set type conversion because I need the original
      resulttype of the set add nodes
    NOTE: you have to start a cycle with 1.0.5!

  Revision 1.7  2001/08/30 15:43:15  jonas
    * converted adding/comparing of strings to compileproc. Note that due
      to the way the shortstring helpers for i386 are written, they are
      still handled by the old code (reason: fpc_shortstr_compare returns
      results in the flags instead of in eax and fpc_shortstr_concat
      has wierd parameter conventions). The compilerproc stuff should work
      fine with the generic implementations though.
    * removed some nested comments warnings

  Revision 1.6  2001/08/29 19:49:04  jonas
    * some fixes in compilerprocs for chararray to string conversions
    * conversion from string to chararray is now also done via compilerprocs

  Revision 1.5  2001/08/28 13:24:47  jonas
    + compilerproc implementation of most string-related type conversions
    - removed all code from the compiler which has been replaced by
      compilerproc implementations (using (ifdef hascompilerproc) is not
      necessary in the compiler)

  Revision 1.4  2001/08/23 14:28:36  jonas
    + tempcreate/ref/delete nodes (allows the use of temps in the
      resulttype and first pass)
    * made handling of read(ln)/write(ln) processor independent
    * moved processor independent handling for str and reset/rewrite-typed
      from firstpass to resulttype pass
    * changed names of helpers in text.inc to be generic for use as
      compilerprocs + added "iocheck" directive for most of them
    * reading of ordinals is done by procedures instead of functions
      because otherwise FPC_IOCHECK overwrote the result before it could
      be stored elsewhere (range checking still works)
    * compilerprocs can now be used in the system unit before they are
      implemented
    * added note to errore.msg that booleans can't be read using read/readln

  Revision 1.3  2001/08/13 12:40:16  jonas
    * renamed some str(x,y) and val(x,y) helpers so the naming scheme is the
      same for all string types
    + added the str(x,y) and val(x,y,z) helpers for int64/qword to
      compproc.inc

  Revision 1.2  2001/08/06 14:13:55  jonas
    * changed buf parameter of fpc_type_write from var to const
    + added names of helpers in typefile.inc to compproc.inc

  Revision 1.1  2001/08/01 15:00:10  jonas
    + "compproc" helpers
    * renamed several helpers so that their name is the same as their
      "public alias", which should facilitate the conversion of processor
      specific code in the code generator to processor independent code
    * some small fixes to the val_ansistring and val_widestring helpers
      (always immediately exit if the source string is longer than 255
       chars)
    * fixed fpc_dynarray_high and fpc_dynarray_length if the dynarray is
      still nil (used to crash, now return resp -1 and 0)

}
