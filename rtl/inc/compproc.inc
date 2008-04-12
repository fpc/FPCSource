{
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
  { normally the array should be maxlongint big, but that will confuse
    the debugger. The compiler will set the correct size of the array
    internally. It's now set to 0..0 because when compiling with -gt,
    the entire array will be trashed, so it must not be defined larger
    than the minimal size (otherwise we can trash other memory) }
{$ifndef FPC_STRTOCHARARRAYPROC}
  fpc_big_chararray = array[0..0] of char;
  fpc_big_widechararray = array[0..0] of widechar;
{$endif ndef FPC_STRTOCHARARRAYPROC}
{$ifdef FPC_NEW_BIGENDIAN_SETS}
  fpc_small_set = bitpacked array[0..31] of 0..1;
  fpc_normal_set = bitpacked array[0..255] of 0..1;
{$else}
  fpc_small_set = longint;
  fpc_normal_set = array[0..7] of longint;
{$endif}
  fpc_normal_set_byte = array[0..31] of byte;
  fpc_normal_set_long = array[0..7] of longint;


{$ifdef FPC_HAS_FEATURE_HEAP}
{ Needed to solve overloading problem with call from assembler (PFV) }
Function  fpc_getmem(size:ptruint):pointer;compilerproc;
Procedure fpc_freemem(p:pointer);compilerproc;
{$endif FPC_HAS_FEATURE_HEAP}

procedure fpc_Shortstr_SetLength(var s:shortstring;len:SizeInt); compilerproc;
{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_shortstr_to_shortstr(len:longint; const sstr: shortstring): shortstring; compilerproc;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_shortstr_to_shortstr(out res:shortstring; const sstr: shortstring); compilerproc;
{$endif FPC_STRTOSHORTSTRINGPROC}

{$ifndef STR_CONCAT_PROCS}
function fpc_shortstr_concat(const s1,s2:shortstring): shortstring; compilerproc;
{$else STR_CONCAT_PROCS}
procedure fpc_shortstr_concat(var dests:shortstring;const s1,s2:shortstring);compilerproc;
procedure fpc_shortstr_concat_multi(var dests:shortstring;const sarr:array of pshortstring);compilerproc;
{$endif STR_CONCAT_PROCS}
procedure fpc_shortstr_append_shortstr(var s1:shortstring;const s2:shortstring); compilerproc;
function fpc_shortstr_compare(const left,right:shortstring) : longint; compilerproc;
function fpc_shortstr_compare_equal(const left,right:shortstring) : longint; compilerproc;

{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_pchar_to_shortstr(p:pchar):shortstring; compilerproc;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_pchar_to_shortstr(out res : shortstring;p:pchar); compilerproc;
{$endif FPC_STRTOSHORTSTRINGPROC}

function fpc_pchar_length(p:pchar):longint; compilerproc;
function fpc_pwidechar_length(p:pwidechar):longint; compilerproc;

{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_chararray_to_shortstr(const arr: array of char; zerobased: boolean = true):shortstring; compilerproc;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_chararray_to_shortstr(out res : shortstring;const arr: array of char; zerobased: boolean = true); compilerproc;
{$endif FPC_STRTOSHORTSTRINGPROC}
{$ifndef FPC_STRTOCHARARRAYPROC}
function fpc_shortstr_to_chararray(arraysize: longint; const src: ShortString): fpc_big_chararray; compilerproc;
{$else ndef FPC_STRTOCHARARRAYPROC}
procedure fpc_shortstr_to_chararray(out res: array of char; const src: ShortString); compilerproc;
{$endif ndef FPC_STRTOCHARARRAYPROC}

Function  fpc_shortstr_Copy(const s:shortstring;index:SizeInt;count:SizeInt):shortstring;compilerproc;
function  fpc_char_copy(c:char;index : SizeInt;count : SizeInt): shortstring;compilerproc;

{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
function fpc_dynarray_copy(psrc : pointer;ti : pointer;
    lowidx,count:tdynarrayindex) : pointer;compilerproc;
function  fpc_dynarray_length(p : pointer) : tdynarrayindex; compilerproc;
function  fpc_dynarray_high(p : pointer) : tdynarrayindex; compilerproc;
procedure fpc_dynarray_clear(var p : pointer;ti : pointer); compilerproc;
procedure fpc_dynarray_decr_ref(var p : pointer;ti : pointer); compilerproc;
procedure fpc_dynarray_incr_ref(p : pointer); compilerproc;
procedure fpc_dynarray_setlength(var p : pointer;pti : pointer; dimcount : dword;dims : pdynarrayindex); compilerproc;
{$endif FPC_HAS_FEATURE_DYNARRAYS}

{ Str() support }
procedure fpc_ShortStr_sint(v : valsint;len : SizeInt;out s : shortstring); compilerproc;
procedure fpc_shortstr_uint(v : valuint;len : SizeInt;out s : shortstring); compilerproc;
{$ifndef FPUNONE}
procedure fpc_ShortStr_Float(d : ValReal;len,fr,rt : SizeInt;out s : shortstring); compilerproc;
{$endif}
procedure fpc_shortstr_enum(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out s:shortstring);compilerproc;
procedure fpc_ShortStr_Currency(c : currency; len,f : SizeInt; out s : shortstring); compilerproc;

procedure fpc_chararray_sint(v : valsint;len : SizeInt;out a : array of char); compilerproc;
procedure fpc_chararray_uint(v : valuint;len : SizeInt;out a : array of char); compilerproc;
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
procedure fpc_AnsiStr_sint(v : valsint; Len : SizeInt; out S : AnsiString); compilerproc;
procedure fpc_AnsiStr_uint(v : valuint;Len : SizeInt; out S : AnsiString); compilerproc;
{$ifndef FPUNONE}
procedure fpc_AnsiStr_Float(d : ValReal;len,fr,rt : SizeInt;out s : ansistring); compilerproc;
{$endif}
{$ifdef FPC_HAS_STR_CURRENCY}
procedure fpc_AnsiStr_Currency(c : currency;len,fr : SizeInt;out s : ansistring); compilerproc;
{$endif FPC_HAS_STR_CURRENCY}
{$endif FPC_HAS_FEATURE_ANSISTRINGS}

{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
procedure fpc_WideStr_sint(v : valsint; Len : SizeInt; out S : WideString); compilerproc;
procedure fpc_WideStr_uint(v : valuint;Len : SizeInt; out S : WideString); compilerproc;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
{$ifndef CPU64}
  procedure fpc_shortstr_qword(v : qword;len : SizeInt;out s : shortstring); compilerproc;
  procedure fpc_shortstr_int64(v : int64;len : SizeInt;out s : shortstring); compilerproc;
  procedure fpc_chararray_qword(v : qword;len : SizeInt;out a : array of char); compilerproc;
  procedure fpc_chararray_int64(v : int64;len : SizeInt;out a : array of char); compilerproc;
  {$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
  procedure fpc_ansistr_qword(v : qword;len : SizeInt;out s : ansistring); compilerproc;
  procedure fpc_ansistr_int64(v : int64;len : SizeInt;out s : ansistring); compilerproc;
  {$endif FPC_HAS_FEATURE_ANSISTRINGS}

  {$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
  procedure fpc_widestr_qword(v : qword;len : SizeInt;out s : widestring); compilerproc;
  procedure fpc_widestr_int64(v : int64;len : SizeInt;out s : widestring); compilerproc;
  {$endif FPC_HAS_FEATURE_WIDESTRINGS}
{$endif CPU64}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
{$ifndef FPUNONE}
procedure fpc_WideStr_Float(d : ValReal;len,fr,rt : SizeInt;out s : WideString); compilerproc;
{$endif}
{$ifdef FPC_HAS_STR_CURRENCY}
procedure fpc_WideStr_Currency(c : Currency;len,fr : SizeInt;out s : WideString);compilerproc;
{$endif FPC_HAS_STR_CURRENCY}
{$endif FPC_HAS_FEATURE_WIDESTRINGS}

{$ifndef FPUNONE}
procedure fpc_chararray_Float(d : ValReal;len,fr,rt : SizeInt;out a : array of char); compilerproc;
{$endif}
{$ifdef FPC_HAS_STR_CURRENCY}
procedure fpc_chararray_Currency(c : Currency;len,fr : SizeInt;out a : array of char);compilerproc;
{$endif FPC_HAS_STR_CURRENCY}

{ Val() support }
{$ifndef FPUNONE}
Function fpc_Val_Real_ShortStr(const s : shortstring; out code : ValSInt): ValReal; compilerproc;
{$endif}
Function fpc_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; out Code: ValSInt): ValSInt; compilerproc;
Function fpc_Val_UInt_Shortstr(Const S: ShortString; out Code: ValSInt): ValUInt; compilerproc;
function fpc_val_enum_shortstr(str2ordindex:pointer;const s:shortstring;out code:valsint):longint; compilerproc;
Function fpc_Val_Currency_ShortStr(const s : shortstring; out Code : ValSInt): currency; compilerproc;
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
{$ifndef FPUNONE}
Function fpc_Val_Real_AnsiStr(Const S : AnsiString; out Code : ValSInt): ValReal; compilerproc;
{$endif}
Function fpc_Val_UInt_AnsiStr (Const S : AnsiString; out Code : ValSInt): ValUInt; compilerproc;
Function fpc_Val_SInt_AnsiStr (DestSize: SizeInt; Const S : AnsiString; out Code : ValSInt): ValSInt; compilerproc;
Function fpc_Val_Currency_AnsiStr(Const S : AnsiString; out Code : ValSInt): Currency; compilerproc;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
{$ifndef FPUNONE}
Function fpc_Val_Real_WideStr(Const S : WideString; out Code : ValSInt): ValReal; compilerproc;
{$endif}
Function fpc_Val_SInt_WideStr (DestSize: SizeInt; Const S : WideString; out Code : ValSInt): ValSInt; compilerproc;
Function fpc_Val_UInt_WideStr (Const S : WideString; out Code : ValSInt): ValUInt; compilerproc;
function fpc_val_enum_widestr(str2ordindex:pointer;const s:widestring;out code:valsint):longint;compilerproc;
Function fpc_Val_Currency_WideStr(Const S : WideString; out Code : ValSInt): Currency; compilerproc;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
{$ifndef CPU64}
Function fpc_val_int64_shortstr(Const S: ShortString; out Code: ValSInt): Int64; compilerproc;
Function fpc_val_qword_shortstr(Const S: ShortString; out Code: ValSInt): QWord; compilerproc;
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Function fpc_Val_qword_AnsiStr (Const S : AnsiString; out Code : ValSInt): qword;compilerproc;
Function fpc_Val_int64_AnsiStr (Const S : AnsiString; out Code : ValSInt): Int64; compilerproc;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
Function fpc_Val_qword_WideStr (Const S : WideString; out Code : ValSInt): qword; compilerproc;
Function fpc_Val_int64_WideStr (Const S : WideString; out Code : ValSInt): Int64; compilerproc;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
{$endif CPU64}

{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Procedure fpc_ansistr_decr_ref (Var S : Pointer); compilerproc;
Procedure fpc_ansistr_incr_ref (S : Pointer); compilerproc;
Procedure fpc_AnsiStr_Assign (Var DestS : Pointer;S2 : Pointer); compilerproc;
{$ifdef STR_CONCAT_PROCS}
Procedure fpc_AnsiStr_Concat (Var DestS : Ansistring;const S1,S2 : AnsiString); compilerproc;
Procedure fpc_AnsiStr_Concat_multi (Var DestS : Ansistring;const sarr:array of Ansistring); compilerproc;
{$else STR_CONCAT_PROCS}
function fpc_AnsiStr_Concat (const S1,S2 : AnsiString): AnsiString; compilerproc;
function fpc_AnsiStr_Concat_multi (const sarr:array of Ansistring): ansistring; compilerproc;
{$endif STR_CONCAT_PROCS}
Procedure fpc_ansistr_append_char(Var S : AnsiString;c : char); compilerproc;
Procedure fpc_ansistr_append_shortstring(Var S : AnsiString;const Str : ShortString); compilerproc;
Procedure fpc_ansistr_append_ansistring(Var S : AnsiString;const Str : AnsiString); compilerproc;
{$ifdef EXTRAANSISHORT}
Procedure fpc_AnsiStr_ShortStr_Concat (Var S1: AnsiString; Var S2 : ShortString); compilerproc;
{$endif EXTRAANSISHORT}
{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_AnsiStr_To_ShortStr (high_of_res: SizeInt;const S2 : Ansistring): shortstring; compilerproc;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_AnsiStr_To_ShortStr (out res : shortstring;const S2 : Ansistring); compilerproc;
{$endif FPC_STRTOSHORTSTRINGPROC}
Function fpc_ShortStr_To_AnsiStr (Const S2 : ShortString): ansistring; compilerproc;
Function fpc_Char_To_AnsiStr(const c : Char): AnsiString; compilerproc;

Function fpc_PChar_To_AnsiStr(const p : pchar): ansistring; compilerproc;
Function fpc_CharArray_To_AnsiStr(const arr: array of char; zerobased: boolean = true): ansistring; compilerproc;
{$ifndef FPC_STRTOCHARARRAYPROC}
function fpc_ansistr_to_chararray(arraysize: SizeInt; const src: ansistring): fpc_big_chararray; compilerproc;
{$else ndef FPC_STRTOCHARARRAYPROC}
procedure fpc_ansistr_to_chararray(out res: array of char; const src: ansistring)compilerproc;
{$endif ndef FPC_STRTOCHARARRAYPROC}
Function fpc_AnsiStr_Compare(const S1,S2 : AnsiString): SizeInt; compilerproc;
Function fpc_AnsiStr_Compare_equal(const S1,S2 : AnsiString): SizeInt; compilerproc;
Procedure fpc_AnsiStr_CheckZero(p : pointer); compilerproc;
Procedure fpc_AnsiStr_CheckRange(len,index : SizeInt); compilerproc;
Procedure fpc_AnsiStr_SetLength (Var S : AnsiString; l : SizeInt); compilerproc;
Function  fpc_ansistr_Copy (Const S : AnsiString; Index,Size : SizeInt) : AnsiString;compilerproc;
{$ifdef EXTRAANSISHORT}
Function fpc_AnsiStr_ShortStr_Compare (Var S1 : Pointer; Var S2 : ShortString): SizeInt; compilerproc;
{$endif EXTRAANSISHORT}
{ pointer argument because otherwise when calling this, we get
  an endless loop since a 'var s: ansistring' must be made
  unique as well                                               }
Function fpc_ansistr_Unique(Var S : Pointer): Pointer; compilerproc;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}

{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
Procedure fpc_WideStr_Decr_Ref (Var S : Pointer); compilerproc;
Procedure fpc_WideStr_Incr_Ref (Var S : Pointer); compilerproc;
{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_WideStr_To_ShortStr (high_of_res: SizeInt;const S2 : WideString): shortstring; compilerproc;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_WideStr_To_ShortStr (out res: ShortString;const S2 : WideString); compilerproc;
{$endif FPC_STRTOSHORTSTRINGPROC}
Function fpc_ShortStr_To_WideStr (Const S2 : ShortString): WideString; compilerproc;
Function fpc_WideStr_To_AnsiStr (const S2 : WideString): AnsiString; compilerproc;
Function fpc_AnsiStr_To_WideStr (Const S2 : AnsiString): WideString; compilerproc;
Procedure fpc_WideStr_Assign (Var S1 : Pointer;S2 : Pointer); compilerproc;
{$ifndef STR_CONCAT_PROCS}
Function fpc_WideStr_Concat (const S1,S2 : WideString) : WideString; compilerproc;
function fpc_WideStr_Concat_multi (const sarr:array of Widestring): widestring; compilerproc;
{$else STR_CONCAT_PROCS}
Procedure fpc_WideStr_Concat (Var DestS : Widestring;const S1,S2 : WideString); compilerproc;
Procedure fpc_WideStr_Concat_multi (Var DestS : Widestring;const sarr:array of Widestring); compilerproc;
{$endif STR_CONCAT_PROCS}
Function fpc_Char_To_WideStr(const c : Char): WideString; compilerproc;
Function fpc_PChar_To_WideStr(const p : pchar): WideString; compilerproc;
Function fpc_CharArray_To_WideStr(const arr: array of char; zerobased: boolean = true): WideString; compilerproc;
{$ifndef FPC_STRTOCHARARRAYPROC}
function fpc_widestr_to_chararray(arraysize: SizeInt; const src: WideString): fpc_big_chararray; compilerproc;
Function fpc_shortstr_to_widechararray(arraysize: SizeInt; const src: ShortString): fpc_big_widechararray; compilerproc;
Function fpc_ansistr_to_widechararray(arraysize: SizeInt; const src: AnsiString): fpc_big_widechararray; compilerproc;
Function fpc_widestr_to_widechararray(arraysize: SizeInt; const src: WideString): fpc_big_widechararray; compilerproc;
{$else ndef FPC_STRTOCHARARRAYPROC}
procedure fpc_widestr_to_chararray(out res: array of char; const src: WideString); compilerproc;
procedure fpc_shortstr_to_widechararray(out res: array of widechar; const src: ShortString); compilerproc;
procedure fpc_ansistr_to_widechararray(out res: array of widechar; const src: AnsiString); compilerproc;
procedure fpc_widestr_to_widechararray(out res: array of widechar; const src: WideString); compilerproc;
{$endif ndef FPC_STRTOCHARARRAYPROC}
{$ifndef FPC_STRTOSHORTSTRINGPROC}
Function fpc_WideCharArray_To_ShortStr(const arr: array of widechar; zerobased: boolean = true): shortstring; compilerproc;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_WideCharArray_To_ShortStr(out res : shortstring;const arr: array of widechar; zerobased: boolean = true); compilerproc;
{$endif FPC_STRTOSHORTSTRINGPROC}
Function fpc_WideCharArray_To_AnsiStr(const arr: array of widechar; zerobased: boolean = true): AnsiString; compilerproc;
Function fpc_WideCharArray_To_WideStr(const arr: array of widechar; zerobased: boolean = true): WideString; compilerproc;
Function fpc_WideStr_Compare(const S1,S2 : WideString): SizeInt; compilerproc;
Function fpc_WideStr_Compare_equal(const S1,S2 : WideString): SizeInt; compilerproc;
Procedure fpc_WideStr_CheckZero(p : pointer); compilerproc;
Procedure fpc_WideStr_CheckRange(len,index : SizeInt); compilerproc;
Procedure fpc_WideStr_SetLength (Var S : WideString; l : SizeInt); compilerproc;
Function  fpc_widestr_Copy (Const S : WideString; Index,Size : SizeInt) : WideString;compilerproc;
{$ifndef FPC_WINLIKEWIDESTRING}
function fpc_widestr_Unique(Var S : Pointer): Pointer; compilerproc;
{$endif FPC_WINLIKEWIDESTRING}
Function fpc_Char_To_WChar(const c : Char): WideChar; compilerproc;
Function fpc_WChar_To_Char(const c : WideChar): Char; compilerproc;
{$ifndef FPC_STRTOSHORTSTRINGPROC}
Function fpc_WChar_To_ShortStr(const c : WideChar): ShortString; compilerproc;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_WChar_To_ShortStr(out res : shortstring;const c : WideChar) compilerproc;
{$endif FPC_STRTOSHORTSTRINGPROC}
Function fpc_WChar_To_AnsiStr(const c : WideChar): AnsiString; compilerproc;
Function fpc_WChar_To_WideStr(const c : WideChar): WideString; compilerproc;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}

{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Function fpc_PWideChar_To_AnsiStr(const p : pwidechar): ansistring; compilerproc;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
Function fpc_PWideChar_To_WideStr(const p : pwidechar): widestring; compilerproc;
{$ifndef FPC_STRTOSHORTSTRINGPROC}
Function fpc_PWideChar_To_ShortStr(const p : pwidechar): shortstring; compilerproc;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_PWideChar_To_ShortStr(out res : shortstring;const p : pwidechar); compilerproc;
{$endif FPC_STRTOSHORTSTRINGPROC}
{$endif FPC_HAS_FEATURE_WIDESTRINGS}

{$ifdef FPC_HAS_FEATURE_TEXTIO}
{ from text.inc }
Function fpc_get_input:PText;compilerproc;
Function fpc_get_output:PText;compilerproc;
Procedure fpc_Write_End(var f:Text); compilerproc;
Procedure fpc_Writeln_End(var f:Text); compilerproc;
Procedure fpc_Write_Text_ShortStr(Len : Longint;var f : Text;const s : String); compilerproc;
Procedure fpc_Write_Text_Pchar_as_Array(Len : Longint;var f : Text;const s : array of char; zerobased: boolean = true); compilerproc;
Procedure fpc_Write_Text_PChar_As_Pointer(Len : Longint;var f : Text;p : PChar); compilerproc;
Procedure fpc_Write_Text_AnsiStr (Len : Longint; Var f : Text; const S : AnsiString); compilerproc;
Procedure fpc_Write_Text_WideStr (Len : Longint; Var f : Text; const S : WideString); compilerproc;
Procedure fpc_Write_Text_SInt(Len : Longint;var t : Text;l : ValSInt); compilerproc;
Procedure fpc_Write_Text_UInt(Len : Longint;var t : Text;l : ValUInt); compilerproc;
{$ifndef CPU64}
procedure fpc_write_text_qword(len : longint;var t : text;q : qword); compilerproc;
procedure fpc_write_text_int64(len : longint;var t : text;i : int64); compilerproc;
{$endif CPU64}
{$ifndef FPUNONE}
Procedure fpc_Write_Text_Float(rt,fixkomma,Len : Longint;var t : Text;r : ValReal); compilerproc;
{$endif}
procedure fpc_write_text_enum(typinfo,ord2strindex:pointer;len:sizeint;var t:text;ordinal:longint); compilerproc;
{$ifdef FPC_HAS_STR_CURRENCY}
Procedure fpc_Write_Text_Currency(fixkomma,Len : Longint;var t : Text;c : Currency); compilerproc;
{$endif FPC_HAS_STR_CURRENCY}
Procedure fpc_Write_Text_Boolean(Len : Longint;var t : Text;b : Boolean); compilerproc;
Procedure fpc_Write_Text_Char(Len : Longint;var t : Text;c : Char); compilerproc;
Procedure fpc_Write_Text_WideChar(Len : Longint;var t : Text;c : WideChar); compilerproc;

function fpc_SetupWriteStr_Shortstr(out s: shortstring): PText; compilerproc;
function fpc_SetupWriteStr_Ansistr(out s: ansistring): PText; compilerproc;
function fpc_SetupWriteStr_Widestr(out s: widestring): PText; compilerproc;

function fpc_SetupReadStr_Shortstr(const s: shortstring): PText; compilerproc;
function fpc_SetupReadStr_Ansistr(const s: ansistring): PText; compilerproc;
function fpc_SetupReadStr_Widestr(const s: widestring): PText; compilerproc;
{$endif FPC_HAS_FEATURE_TEXTIO}

{$ifdef FPC_HAS_FEATURE_VARIANTS}
procedure fpc_variant_copy(d,s : pointer);compilerproc;
procedure fpc_variant_copy_overwrite(source, dest : pointer);compilerproc;
procedure fpc_write_text_variant(Len : Longint;var f : Text;const v : variant); compilerproc;
function fpc_variant_to_dynarray(const v : variant;typeinfo : pointer) : pointer;compilerproc;
function fpc_dynarray_to_variant(dynarr : pointer;typeinfo : pointer) : variant;compilerproc;
function fpc_variant_to_interface(const v : variant) : iinterface;compilerproc;
function fpc_interface_to_variant(const i : iinterface) : variant;compilerproc;
function fpc_variant_to_idispatch(const v : variant) : idispatch;compilerproc;
function fpc_idispatch_to_variant(const i : idispatch) : variant;compilerproc;
procedure fpc_vararray_get(var d : variant;const s : variant;indices : psizeint;len : sizeint);compilerproc;
procedure fpc_vararray_put(var d : variant;const s : variant;indices : psizeint;len : sizeint);compilerproc;
procedure fpc_dispinvoke_variant(dest : pvardata;const source : tvardata;  calldesc : pcalldesc;params : pointer);compilerproc;
{$endif FPC_HAS_FEATURE_VARIANTS}

{$ifdef FPC_HAS_FEATURE_TEXTIO}
Procedure fpc_Read_End(var f:Text); compilerproc;
Procedure fpc_ReadLn_End(var f : Text); compilerproc;
Procedure fpc_Read_Text_ShortStr(var f : Text;out s : String); compilerproc;
Procedure fpc_Read_Text_PChar_As_Pointer(var f : Text; const s : PChar); compilerproc;
Procedure fpc_Read_Text_PChar_As_Array(var f : Text;out s : array of char; zerobased: boolean = false); compilerproc;
Procedure fpc_Read_Text_AnsiStr(var f : Text;out s : AnsiString); compilerproc;
Procedure fpc_Read_Text_Char(var f : Text; out c : char); compilerproc;
Procedure fpc_Read_Text_SInt(var f : Text; out l :ValSInt); compilerproc;
Procedure fpc_Read_Text_UInt(var f : Text; out u :ValUInt); compilerproc;
{$ifndef FPUNONE}
Procedure fpc_Read_Text_Float(var f : Text; out v :ValReal); compilerproc;
{$endif}
procedure fpc_read_text_enum(str2ordindex:pointer;var t:text;out ordinal:longint); compilerproc;
procedure fpc_Read_Text_Currency(var f : Text; out v : Currency); compilerproc;
{$ifndef CPU64}
Procedure fpc_Read_Text_QWord(var f : text; out q : qword); compilerproc;
Procedure fpc_Read_Text_Int64(var f : text; out i : int64); compilerproc;
{$endif CPU64}
{$endif FPC_HAS_FEATURE_TEXTIO}

{$ifdef FPC_INCLUDE_SOFTWARE_MOD_DIV}
function fpc_div_dword(n,z : dword) : dword; compilerproc;
function fpc_mod_dword(n,z : dword) : dword; compilerproc;
function fpc_div_longint(n,z : longint) : longint; compilerproc;
function fpc_mod_longint(n,z : longint) : longint; compilerproc;
{$endif FPC_INCLUDE_SOFTWARE_MOD_DIV}

{ from int64.inc }
function fpc_div_qword(n,z : qword) : qword; compilerproc;
function fpc_mod_qword(n,z : qword) : qword; compilerproc;
function fpc_div_int64(n,z : int64) : int64; compilerproc;
function fpc_mod_int64(n,z : int64) : int64; compilerproc;
function fpc_mul_qword(f1,f2 : qword;checkoverflow : longbool) : qword; compilerproc;
function fpc_mul_int64(f1,f2 : int64;checkoverflow : longbool) : int64; compilerproc;

{$ifdef FPC_INCLUDE_SOFTWARE_SHIFT_INT64}
function fpc_shl_qword(value,shift : qword) : qword; compilerproc;
function fpc_shr_qword(value,shift : qword) : qword; compilerproc;
function fpc_shl_int64(value,shift : int64) : int64; compilerproc;
function fpc_shr_int64(value,shift : int64) : int64; compilerproc;
{$endif  FPC_INCLUDE_SOFTWARE_SHIFT_INT64}

{$ifndef FPUNONE}
function fpc_abs_real(d : ValReal) : ValReal;compilerproc;
function fpc_arctan_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
function fpc_cos_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
function fpc_exp_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
function fpc_frac_real(d : ValReal) : ValReal;compilerproc;
function fpc_int_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
function fpc_ln_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
function fpc_pi_real : ValReal;compilerproc;
function fpc_sin_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
function fpc_sqr_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
function fpc_sqrt_real(d : ValReal) : ValReal;compilerproc;
function fpc_round_real(d : ValReal) : int64;compilerproc;
function fpc_trunc_real(d : ValReal) : int64;compilerproc;
{$endif}

{$ifdef FPC_HAS_FEATURE_CLASSES}
function fpc_do_is(aclass : tclass;aobject : tobject) : boolean; compilerproc;
function fpc_do_as(aclass : tclass;aobject : tobject): tobject; compilerproc;
procedure fpc_intf_decr_ref(var i: pointer); compilerproc;
procedure fpc_intf_incr_ref(i: pointer); compilerproc;
procedure fpc_intf_assign(var D: pointer; const S: pointer); compilerproc;
procedure fpc_intf_assign_by_iid(var D: pointer; const S: pointer; const iid: TGUID); compilerproc;
function  fpc_intf_as(const S: pointer; const iid: TGUID): IInterface; compilerproc;
function fpc_class_as_intf(const S: pointer; const iid: TGUID): IInterface; compilerproc;
procedure fpc_dispatch_by_id(Result: Pointer; const Dispatch: pointer;DispDesc: Pointer; Params: Pointer); compilerproc;
{$endif FPC_HAS_FEATURE_CLASSES}


{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
Function fpc_PushExceptAddr (Ft: Longint;_buf,_newaddr : pointer): PJmp_buf ; compilerproc;
Procedure fpc_PushExceptObj (Obj : TObject; AnAddr,AFrame : Pointer); compilerproc;
Function fpc_Raiseexception (Obj : TObject; AnAddr,AFrame : Pointer) : TObject; compilerproc;
Procedure fpc_PopAddrStack; compilerproc;
function fpc_PopObjectStack : TObject; compilerproc;
function fpc_PopSecondObjectStack : TObject; compilerproc;
Procedure fpc_ReRaise; compilerproc;
Function fpc_Catches(Objtype : TClass) : TObject; compilerproc;
{$endif FPC_HAS_FEATURE_EXCEPTIONS}


{$ifdef FPC_HAS_FEATURE_OBJECTS}
function fpc_help_constructor(_self:pointer;var _vmt:pointer;_vmt_pos:cardinal):pointer;compilerproc;
procedure fpc_help_destructor(_self,_vmt:pointer;vmt_pos:cardinal);compilerproc;
procedure fpc_help_fail(_self:pointer;var _vmt:pointer;vmt_pos:cardinal);compilerproc;
{$endif FPC_HAS_FEATURE_OBJECTS}


{$ifdef dummy}
Procedure fpc_DestroyException(o : TObject); compilerproc;
procedure fpc_check_object(obj:pointer); compilerproc;
procedure fpc_check_object_ext(vmt,expvmt:pointer);compilerproc;
{$endif dummy}


{$ifdef FPC_HAS_FEATURE_RTTI}
Procedure fpc_Initialize (Data,TypeInfo : pointer); compilerproc;
Procedure fpc_finalize (Data,TypeInfo: Pointer); compilerproc;
Procedure fpc_Addref (Data,TypeInfo : Pointer); compilerproc;
Procedure fpc_DecRef (Data,TypeInfo : Pointer);  compilerproc;
procedure fpc_finalize_array(data,typeinfo : pointer;count,size : longint); compilerproc;
Function fpc_Copy (Src, Dest, TypeInfo : Pointer) : SizeInt; compilerproc;
Procedure fpc_Copy_proc (Src, Dest, TypeInfo : Pointer); compilerproc; inline;
{$endif FPC_HAS_FEATURE_RTTI}


{$ifndef FPC_NEW_BIGENDIAN_SETS}
function fpc_set_load_small(l: fpc_small_set): fpc_normal_set; compilerproc;
function fpc_set_create_element(b : byte): fpc_normal_set; compilerproc;
function fpc_set_set_byte(const source: fpc_normal_set; b : byte): fpc_normal_set; compilerproc;
function fpc_set_unset_byte(const source: fpc_normal_set; b : byte): fpc_normal_set; compilerproc;
function fpc_set_set_range(const orgset: fpc_normal_set; l,h : byte): fpc_normal_set; compilerproc;
function fpc_set_add_sets(const set1,set2: fpc_normal_set): fpc_normal_set; compilerproc;
function fpc_set_mul_sets(const set1,set2: fpc_normal_set): fpc_normal_set; compilerproc;
function fpc_set_sub_sets(const set1,set2: fpc_normal_set): fpc_normal_set; compilerproc;
function fpc_set_symdif_sets(const set1,set2: fpc_normal_set): fpc_normal_set; compilerproc;
function fpc_set_comp_sets(const set1,set2: fpc_normal_set): boolean; compilerproc;
function fpc_set_contains_sets(const set1,set2: fpc_normal_set): boolean; compilerproc;
{$endif ndef FPC_NEW_BIGENDIAN_SETS}

{$ifdef FPC_SETBASE_USED}
procedure fpc_varset_load(const l;sourcesize : longint;var dest;size,srcminusdstbase : ptrint); compilerproc;
{$else}
procedure fpc_varset_load(const l;sourcesize : longint;var dest;size : ptrint); compilerproc;
{$endif}
procedure fpc_varset_create_element(b,size : ptrint; var data); compilerproc;
procedure fpc_varset_set(const source;var dest; b,size : ptrint); compilerproc;
procedure fpc_varset_unset(const source;var dest; b,size : ptrint); compilerproc;
procedure fpc_varset_set_range(const orgset; var dest;l,h,size : ptrint); compilerproc;
procedure fpc_varset_add_sets(const set1,set2; var dest;size : ptrint); compilerproc;
procedure fpc_varset_mul_sets(const set1,set2; var dest;size : ptrint); compilerproc;
procedure fpc_varset_sub_sets(const set1,set2; var dest;size : ptrint); compilerproc;
procedure fpc_varset_symdif_sets(const set1,set2; var dest;size : ptrint); compilerproc;
function fpc_varset_comp_sets(const set1,set2;size : ptrint):boolean; compilerproc;
function fpc_varset_contains_sets(const set1,set2;size : ptrint):boolean; compilerproc;

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
procedure fpc_divbyzero; compilerproc;
procedure fpc_overflow; compilerproc;
procedure fpc_iocheck; compilerproc;

procedure fpc_InitializeUnits; compilerproc;
// not generated by compiler, called directly in system unit
// procedure fpc_FinalizeUnits; compilerproc;

{
Procedure fpc_do_exit; compilerproc;
Procedure fpc_lib_exit; compilerproc;
Procedure fpc_HandleErrorAddrFrame (Errno : longint;addr,frame : pointer); compilerproc;
Procedure fpc_HandleError (Errno : longint); compilerproc;
}

procedure fpc_AbstractErrorIntern;compilerproc;
procedure fpc_assert(Const Msg,FName:Shortstring;LineNo:Longint;ErrorAddr:Pointer); compilerproc;

{$ifdef FPC_HAS_FEATURE_FILEIO}
Procedure fpc_reset_typed(var f : TypedFile;Size : Longint); compilerproc;
Procedure fpc_rewrite_typed(var f : TypedFile;Size : Longint); compilerproc;
Procedure fpc_typed_write(TypeSize : Longint;var f : TypedFile;const Buf); compilerproc;
Procedure fpc_typed_read(TypeSize : Longint;var f : TypedFile;out Buf); compilerproc;
{$endif FPC_HAS_FEATURE_FILEIO}

{$ifdef FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}
function fpc_int64_to_double(i: int64): double; compilerproc;
function fpc_qword_to_double(q: qword): double; compilerproc;
{$endif FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}


