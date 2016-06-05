{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl,
    Copyright (c) 2011 by Jonas Maebe,
    members of the Free Pascal development team.

    This file implements support routines for UTF-8 strings with FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ unicodestring is a plain java.lang.String }
{$define FPC_UNICODESTRING_TYPE_DEFINED}

{ helpers for converting between Windows and Java code page identifiers }

{$i jwin2javacharset.inc}

{$define FPC_HAS_DEFAULT_UNICODE_2_ANSI_MOVE}
procedure DefaultUnicode2AnsiMove(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
var
  localencoder: JNCCharsetEncoder;
  inbuf: JNCharBuffer;
  outbuf: JNByteBuffer;
begin
  localencoder:=widestringmanager.encoder.getForCodePage(cp);
  localencoder.reset;
  localencoder.onMalformedInput(JNCCodingErrorAction.fREPLACE);
  localencoder.onUnmappableCharacter(JNCCodingErrorAction.fREPLACE);
  inbuf:=JNCharBuffer.wrap(TJCharArray(source),0,len);
  outbuf:=localencoder.encode(inbuf);
  setlength(dest,outbuf.limit);
  { "The buffer's position will be zero and its limit will *follow* the last
    byte written" -> we already have a terminating zero }
  outbuf.get(TJByteArray(AnsiStringClass(dest).fdata),0,outbuf.limit);
  { already null-terminated because of setlength }
  SetCodePage(dest,cp,false);
end;


{$define FPC_HAS_DEFAULT_ANSI_2_UNICODE}
procedure DefaultAnsi2UnicodeMove(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);
var
  localdecoder: JNCCharsetDecoder;
  inbuf: JNByteBuffer;
  outbuf: JNCharBuffer;
begin
  localdecoder:=widestringmanager.decoder.getForCodePage(cp);
  localdecoder.reset;
  localdecoder.onMalformedInput(JNCCodingErrorAction.fREPLACE);
  localdecoder.onUnmappableCharacter(JNCCodingErrorAction.fREPLACE);
  inbuf:=JNByteBuffer.wrap(TJByteArray(source),0,len);
  outbuf:=localdecoder.decode(inbuf);
  dest:=outbuf.toString;
end;


{
  This file contains the implementation of the UnicodeString type,
  which on the Java platforms is an alias for java.lang.String
}

{$define FPC_HAS_NEW_UNICODESTRING}
Function NewUnicodeString(Len : SizeInt) : JLString;
{
  Allocate a new UnicodeString on the heap.
  initialize it to zero length and reference count 1.
}
var
  data: array of jchar;
begin
  setlength(data,len);
  result:=JLString.create(data);
end;


{ lie, not required }
{$define FPC_HAS_UNICODESTR_DECR_REF}
{$define FPC_HAS_UNICODESTR_INCR_REF}

{$define FPC_HAS_UNICODESTR_TO_SHORTSTR}
procedure fpc_UnicodeStr_To_ShortStr (out res: ShortString;const S2 : UnicodeString); [Public, alias: 'FPC_UNICODESTR_TO_SHORTSTR'];compilerproc;
{
  Converts a UnicodeString to a ShortString;
}
Var
  Size : SizeInt;
  temp : ansistring;
begin
  res:='';
  Size:=Length(S2);
  if Size>0 then
    begin
      temp:=s2;
      res:=temp;
    end;
end;


{$define FPC_HAS_SHORTSTR_TO_UNICODESTR}
Function fpc_ShortStr_To_UnicodeStr (Const S2 : ShortString): UnicodeString;compilerproc;
{
  Converts a ShortString to a UnicodeString;
}
Var
  Size : SizeInt;
begin
  result:='';
  Size:=Length(S2);
  if Size>0 then
    widestringmanager.Ansi2UnicodeMoveProc(PChar(ShortstringClass(@S2).fdata),DefaultSystemCodePage,result,Size);
end;


{$define FPC_HAS_UNICODESTR_TO_ANSISTR}
Function fpc_UnicodeStr_To_AnsiStr (const S2 : UnicodeString{$ifdef FPC_HAS_CPSTRING};cp : TSystemCodePage{$endif FPC_HAS_CPSTRING}): AnsiString; compilerproc;
{
  Converts a UnicodeString to an AnsiString
}
Var
  Size : SizeInt;
begin
  cp:=TranslatePlaceholderCP(cp);
  { avoid codepage conversion -- why isn't the result rawbytestring? }
  pointer(result):=pointer(AnsistringClass.Create(s2,cp));
end;


{$define FPC_HAS_ANSISTR_TO_UNICODESTR}
Function fpc_AnsiStr_To_UnicodeStr (Const S2 : RawByteString): UnicodeString; compilerproc;
{
  Converts an AnsiString to a UnicodeString;
}
Var
  Size : SizeInt;
begin
  if length(s2)=0 then
    result:=''
  else
    result:=AnsistringClass(S2).toString;
end;


{$define FPC_HAS_UNICODESTR_TO_WIDESTR}
Function fpc_UnicodeStr_To_WideStr (const S2 : UnicodeString): WideString; compilerproc;
  begin
    result:=s2;
  end;


{$define FPC_HAS_WIDESTR_TO_UNICODESTR}
Function fpc_WideStr_To_UnicodeStr (Const S2 : WideString): UnicodeString; compilerproc;
  begin
    result:=s2;
  end;


{$define FPC_HAS_PWIDECHAR_TO_UNICODESTR}
Function fpc_PWideChar_To_UnicodeStr(const p : pwidechar): unicodestring; compilerproc;
var
  Size : SizeInt;
begin
  result:='';
  if p=nil then
    exit;
  size:=0;
  while p[size]<>#0 do
    inc(size);
  Setlength(result,Size);
  if Size>0 then
    result:=JLString.Create(TJCharArray(p),0,Size);
end;


{ lie, not used by compiler }
{$define FPC_HAS_PUNICODECHAR_TO_SHORTSTR}

{$define FPC_HAS_PWIDECHAR_TO_ANSISTR}
Function fpc_PWideChar_To_AnsiStr(const p : pwidechar{$ifdef FPC_HAS_CPSTRING};cp : TSystemCodePage{$endif FPC_HAS_CPSTRING}): ansistring; compilerproc;
begin
  result:='';
  if (p=nil) or
     (p^=#0) then
    exit;
  cp:=TranslatePlaceholderCP(cp);
  pointer(result):=pointer(AnsistringClass.Create(unicodestring(p),cp));
end;


{$define FPC_HAS_PWIDECHAR_TO_SHORTSTR}
procedure fpc_PWideChar_To_ShortStr(out res : shortstring;const p : pwidechar); compilerproc;
begin
  res:='';
  if (p=nil) or
     (p^=#0) then
    exit;
  res:=unicodestring(p);
end;

{ lie, not required for JVM target }
{$define FPC_HAS_UNICODESTR_ASSIGN}


{$define FPC_HAS_UNICODESTR_CONCAT}
procedure fpc_UnicodeStr_Concat (var DestS:Unicodestring;const S1,S2 : UnicodeString); compilerproc;
Var
  sb: JLStringBuilder;
begin
  { only assign if s1 or s2 is empty }
  if (length(S1)=0) then
    begin
      DestS:=s2;
      exit;
    end;
  if (length(S2)=0) then
    begin
      DestS:=s1;
      exit;
    end;
  sb:=JLStringBuilder.create(S1);
  sb.append(s2);
  DestS:=sb.toString;
end;


{$define FPC_HAS_UNICODESTR_CONCAT_MULTI}
procedure fpc_UnicodeStr_Concat_multi (var DestS:Unicodestring;const sarr:array of Unicodestring); compilerproc;
Var
  i  : Longint;
  Size,NewSize : SizeInt;
  sb: JLStringBuilder;
begin
  { First calculate size of the result so we can allocate a StringBuilder of
    the right size }
  NewSize:=0;
  for i:=low(sarr) to high(sarr) do
    inc(Newsize,length(sarr[i]));
  sb:=JLStringBuilder.create(NewSize);
  for i:=low(sarr) to high(sarr) do
    begin
      if length(sarr[i])>0 then
        sb.append(sarr[i]);
    end;
  dests:=sb.toString;
end;


{$define FPC_HAS_CHAR_TO_UCHAR}
Function fpc_Char_To_UChar(const c : AnsiChar): UnicodeChar; compilerproc;
var
  arr: array[0..0] of ansichar;
  w: unicodestring;
begin
  arr[0]:=c;
  widestringmanager.Ansi2UnicodeMoveProc(pansichar(@arr),DefaultSystemCodePage,w,1);
  fpc_Char_To_UChar:=w[1];
end;


{$define FPC_HAS_CHAR_TO_UNICODESTR}
Function fpc_Char_To_UnicodeStr(const c : AnsiChar): UnicodeString; compilerproc;
{
  Converts a AnsiChar to a UnicodeString;
}
var
  arr: array[0..0] of ansichar;
begin
  arr[0]:=c;
  widestringmanager.Ansi2UnicodeMoveProc(pansichar(@arr),DefaultSystemCodePage,result,1);
end;


{$define FPC_HAS_UCHAR_TO_CHAR}
Function fpc_UChar_To_Char(const c : UnicodeChar): Char; compilerproc;
{
  Converts a UnicodeChar to a Char;
}
var
  u: unicodestring;
  s: RawByteString;
  arr: array[0..0] of unicodechar;
begin
  arr[0]:=c;
  widestringmanager.Unicode2AnsiMoveProc(punicodechar(@arr), s, DefaultSystemCodePage, 1);
  if length(s)=1 then
    fpc_UChar_To_Char:= s[1]
  else
    fpc_UChar_To_Char:='?';
end;


{ lie, unused for this target since widechar = unicodechar }
{$define FPC_HAS_UCHAR_TO_UNICODESTR}
Function fpc_UChar_To_UnicodeStr(const c : UnicodeChar): UnicodeString; compilerproc;
{
  Converts a UnicodeChar to a UnicodeString;
}
var
  arr: array[0..0] of UnicodeChar;
begin
  arr[0]:=c;
  result:=JLString.create(arr);
end;


{$define FPC_HAS_UCHAR_TO_ANSISTR}
Function fpc_UChar_To_AnsiStr(const c : UnicodeChar{$ifdef FPC_HAS_CPSTRING};cp : TSystemCodePage{$endif FPC_HAS_CPSTRING}): AnsiString; compilerproc;
{
  Converts a UnicodeChar to a AnsiString;
}
var
  u: unicodestring;
  arr: array[0..0] of unicodechar;
begin
  arr[0]:=c;
  cp:=TranslatePlaceholderCP(cp);
  widestringmanager.Unicode2AnsiMoveProc(punicodechar(@arr), RawByteString(fpc_UChar_To_AnsiStr), cp, 1);
end;


{$define FPC_HAS_UCHAR_TO_SHORTSTR}
function fpc_UChar_To_ShortStr(const c : UnicodeChar): shortstring; compilerproc;
{
  Converts a UnicodeChar to a AnsiString;
}
var
  u: unicodestring;
begin
  u:=c;
  result:=u;
end;


{$ifndef FPC_HAS_UCHAR_TO_ANSISTR}
{$define FPC_HAS_UCHAR_TO_ANSISTR}
Function fpc_UChar_To_AnsiStr(const c : UnicodeChar{$ifdef FPC_HAS_CPSTRING};cp : TSystemCodePage{$endif FPC_HAS_CPSTRING}): AnsiString; compilerproc;
{
  Converts a UnicodeChar to a AnsiString;
}
var
  arr: array[0..0] of unicodechar;
{$ifndef FPC_HAS_CPSTRING}
  cp : TSystemCodePage;
{$endif FPC_HAS_CPSTRING}
begin
{$ifndef FPC_HAS_CPSTRING}
  cp:=DefaultSystemCodePage;
{$endif FPC_HAS_CPSTRING}
  cp:=TranslatePlaceholderCP(cp);
  arr[0]:=c;
  widestringmanager.Unicode2AnsiMoveProc(punicodechar(@arr[0]), fpc_UChar_To_AnsiStr, cp, 1);
end;
{$endif FPC_HAS_UCHAR_TO_ANSISTR}


{$define FPC_HAS_PCHAR_TO_UNICODESTR}
Function fpc_PChar_To_UnicodeStr(const p : pchar): UnicodeString; compilerproc;
var
  i, len: longint;
  arr: TAnsiCharArray;
begin
  arr:=TAnsiCharArray(p);
  i:=0;
  while arr[i]<>#0 do
    inc(i);
  if i<>0 then
    widestringmanager.Ansi2UnicodeMoveProc(P,DefaultSystemCodePage,fpc_PChar_To_UnicodeStr,i)
  else
    result:=''
end;


Function real_widechararray_to_unicodestr(const arr: array of widechar; zerobased: boolean): Unicodestring;
var
  i  : SizeInt;
  foundnull  : boolean;
begin
  if (zerobased) then
    begin
      foundnull:=false;
      for i:=low(arr) to high(arr) do
        if arr[i]=#0 then
          begin
            foundnull:=true;
            break;
          end;
      if not foundnull then
        i := high(arr)+1;
    end
  else
    i := high(arr)+1;
  result:=JLString.create(arr,0,i);
end;

{$define FPC_HAS_WIDECHARARRAY_TO_UNICODESTR}
Function fpc_WideCharArray_To_UnicodeStr(const arr: array of widechar; zerobased: boolean = true): UnicodeString; compilerproc;
begin
  result:=real_widechararray_to_unicodestr(arr,zerobased);
end;

{ due to their names, the following procedures should be in wstrings.inc,
  however, the compiler generates code using this functions on all platforms }
{$define FPC_HAS_WIDECHARARRAY_TO_SHORTSTR}
procedure fpc_WideCharArray_To_ShortStr(out res : shortstring;const arr: array of widechar; zerobased: boolean = true);[public,alias:'FPC_WIDECHARARRAY_TO_SHORTSTR']; compilerproc;
begin
  res:=real_widechararray_to_unicodestr(arr,zerobased);
end;


{$define FPC_HAS_WIDECHARARRAY_TO_WIDESTR}
Function fpc_WideCharArray_To_WideStr(const arr: array of widechar; zerobased: boolean = true): WideString; compilerproc;
begin
  result:=real_widechararray_to_unicodestr(arr,zerobased);
end;


{$define FPC_HAS_UNICODESTR_TO_CHARARRAY}
procedure fpc_unicodestr_to_chararray(out res: array of AnsiChar; const src: UnicodeString); compilerproc;
var
  len: longint;
  temp: array of jbyte;
  csname: unicodestring;
begin
  len:=length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len>0 then
    begin
      csname:=win2javacs(DefaultSystemCodePage);
      if csname='<unsupported>' then
        csname:='US-ASCII';
      temp:=JLString(src).getBytes(csname);
      len:=length(temp);
      if len>length(res) then
        len:=length(res);
      JLSystem.ArrayCopy(JLObject(temp),0,JLObject(@res),0,len);
    end;
  if len<=high(res) then
    JUArrays.fill(TJByteArray(@res),len,high(res),0);
end;


function fpc_unicodestr_setchar(const s: UnicodeString; const index: longint; const ch: unicodechar): UnicodeString; compilerproc;
var
  sb: JLStringBuilder;
begin
  sb:=JLStringBuilder.create(s);
  { string indexes are 1-based in Pascal, 0-based in Java }
  sb.setCharAt(index-1,ch);
  result:=sb.toString();
end;


{$define FPC_HAS_ANSISTR_TO_WIDECHARARRAY}
procedure fpc_ansistr_to_widechararray(out res: array of widechar; const src: RawByteString); compilerproc;
var
  len: SizeInt;
  temp: widestring;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    temp:=src;
  len := length(temp);
  if len > high(res)+1 then
    len := high(res)+1;

  JLString(temp).getChars(0,len,res,0);
  JUArrays.fill(res,len,high(res),#0);
end;


{$define FPC_HAS_SHORTSTR_TO_WIDECHARARRAY}
procedure fpc_shortstr_to_widechararray(out res: array of widechar; const src: ShortString); compilerproc;
var
  len: longint;
  temp : unicodestring;
begin
  len := length(src);
  { temp is initialized with an empty string, so no need to convert src in case
    it's also empty}
  if len > 0 then
    temp:=src;
  len := length(temp);
  if len > high(res)+1 then
    len := high(res)+1;

  JLString(temp).getChars(0,len,res,0);
  JUArrays.fill(res,len,high(res),#0);
end;


{$define FPC_HAS_UNICODESTR_TO_WIDECHARARRAY}
procedure fpc_unicodestr_to_widechararray(out res: array of widechar; const src: UnicodeString); compilerproc;
var
  i, len: SizeInt;
begin
  len := length(src);
  if len > length(res) then
    len := length(res);
  JLString(src).getChars(0,len,res,0);
end;


{$define FPC_HAS_UNICODESTR_COMPARE}
Function fpc_UnicodeStr_Compare(const S1,S2 : UnicodeString): SizeInt; compilerproc;
{
  Compares 2 UnicodeStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  MaxI,Temp : SizeInt;
begin
  if JLObject(S1)=JLObject(S2) then
   begin
     result:=0;
     exit;
   end;
  result:=JLString(S1).compareTo(S2);
end;


{$define FPC_HAS_UNICODESTR_COMPARE_EQUAL}
Function fpc_UnicodeStr_Compare_Equal(const S1,S2 : UnicodeString): SizeInt; compilerproc;
{
  Compares 2 UnicodeStrings for equality only;
  The result is
   0 if S1=S2
   <>0 if S1<>S2
}
Var
  MaxI : SizeInt;
begin
  result:=ord(not JLString(S1).equals(JLString(S2)));
end;

{ lie, not required for this target }
{$define FPC_HAS_UNICODESTR_RANGECHECK}


{$define FPC_HAS_UNICODESTR_SETLENGTH}
Procedure fpc_UnicodeStr_SetLength(Var S : UnicodeString; l : SizeInt);[Public,Alias : 'FPC_UNICODESTR_SETLENGTH']; compilerproc;
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
  Returns new val
}
Var
  result: UnicodeString;
  movelen: SizeInt;
  chars: array of widechar;
  strlen: SizeInt;
begin
   if (l>0) then
    begin
      if JLObject(S)=nil then
       begin
         { Need a completely new string...}
         result:=NewUnicodeString(l);
       end
      { no need to create a new string, since Java strings are immutable }
      else
        begin
          strlen:=length(s);
          if l=strlen then
            result:=s
          else if (l<strlen) then
            result:=JLString(s).substring(0,l)
          else
            begin
              setlength(chars,l);
              JLString(s).getChars(0,strlen,chars,0);
              result:=JLString.create(chars,0,l)
            end;
        end
    end
  else
    begin
      result:='';
    end;
  S:=Result;
end;

{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

{$define FPC_HAS_STRING_LEN_TO_WIDECHAR}
function StringToWideChar(const Src : RawByteString;Dest : PWideChar;DestSize : SizeInt) : PWideChar;
  var
    temp: widestring;
    Len: SizeInt;
  begin
    temp:=src;
    Len:=Length(temp);
    if DestSize<=Len then
      Len:=Destsize-1;
    JLString(temp).getChars(0,Len,TJCharArray(Dest),0);
    Dest[Len]:=#0;
    result:=Dest;
  end;


{$define FPC_HAS_UNICODECHAR_LEN_TO_STRING}
function UnicodeCharLenToString(S : PUnicodeChar;Len : SizeInt) : UnicodeString;
  begin
     result:=JLString.Create(TJCharArray(S),0,len);
  end;


{$define FPC_HAS_WIDECHAR_LEN_TO_STRING}
function WideCharLenToString(S : PWideChar;Len : SizeInt) : UnicodeString;
  begin
     result:=JLString.Create(TJCharArray(S),0,len);
  end;


{$define FPC_HAS_UNICODESTR_UNIQUE}
Function fpc_unicodestr_Unique(var S : JLObject): JLObject; compilerproc;
begin
  result:=s;
end;

{ the publicly accessible uniquestring function is declared as
  "external name 'FPC_UNICODESTR_UNIQUE'", which is normally an alias for
  the fpc_unicodestr_Unique compiler proc; since one is a function and the
  other a procedure that sort of hackery doesn't work for the JVM -> create
  a separate procedure for that (since Java strings are immutable, they are
  always unique though) }
procedure FPC_UNICODESTR_UNIQUE(var S : UnicodeString);
begin
  { do nothing }
end;


{$define FPC_HAS_UNICODESTR_COPY}
Function Fpc_UnicodeStr_Copy (Const S : UnicodeString; Index,Size : SizeInt) : UnicodeString;compilerproc;
begin
  dec(index);
  if Index < 0 then
    Index := 0;
  { Check Size. Accounts for Zero-length S, the double check is needed because
    Size can be maxint and will get <0 when adding index }
  if (Size>Length(S)) or
     (Index+Size>Length(S)) then
   Size:=Length(S)-Index;
  If Size>0 then
    result:=JLString(s).subString(Index,Index+Size)
  else
    result:='';
end;


{$define FPC_HAS_POS_UNICODESTR_UNICODESTR}
Function Pos (Const Substr : UnicodeString; Const Source : UnicodeString) : SizeInt;
begin
  Pos:=0;
  if Length(SubStr)>0 then
    Pos:=JLString(Source).indexOf(SubStr)+1;
end;


{ Faster version for a unicodechar alone }
{$define FPC_HAS_POS_UNICODECHAR_UNICODESTR}
Function Pos (c : UnicodeChar; Const s : UnicodeString) : SizeInt;
begin
  Pos:=0;
  if length(S)>0 then
    Pos:=JLString(s).indexOf(ord(c))+1;
end;


{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
{$define FPC_HAS_POS_CHAR_UNICODESTR}
Function Pos (c : AnsiChar; Const s : UnicodeString) : SizeInt;
var
  i: SizeInt;
  wc : unicodechar;
begin
  wc:=c;
  result:=Pos(wc,s);
end;


{$define FPC_HAS_DELETE_UNICODESTR}
Procedure Delete (Var S : UnicodeString; Index,Size: SizeInt);
Var
  LS : SizeInt;
  sb: JLStringBuilder;
begin
  LS:=Length(S);
  if (Index>LS) or (Index<=0) or (Size<=0) then
    exit;

  { (Size+Index) will overflow if Size=MaxInt. }
  if Size>LS-Index then
    Size:=LS-Index+1;
  if Size<=LS-Index then
    begin
      Dec(Index);
      sb:=JLStringBuilder.Create(s);
      sb.delete(index,size);
      s:=sb.toString;
    end
  else
    s:=JLString(s).substring(0,index-1);
end;


{$define FPC_HAS_INSERT_UNICODESTR}
Procedure Insert (Const Source : UnicodeString; Var S : UnicodeString; Index : SizeInt);
var
  Temp : UnicodeString;
  LS : SizeInt;
  sb : JLStringBuilder;
begin
  If Length(Source)=0 then
   exit;
  if index <= 0 then
   index := 1;
  Ls:=Length(S);
  if index > LS then
   index := LS+1;
  Dec(Index);
  sb:=JLStringBuilder.Create(S);
  sb.insert(Index,Source);
  S:=sb.toString;
end;


{$define FPC_HAS_UPCASE_UNICODECHAR}
Function  UpCase(c:UnicodeChar):UnicodeChar;
begin
  result:=JLCharacter.toUpperCase(c);
end;


{$define FPC_HAS_UPCASE_UNICODESTR}
function UpCase(const s : UnicodeString) : UnicodeString;
begin
  result:=JLString(s).toUpperCase;
end;


{$define FPC_HAS_LOWERCASE_UNICODECHAR}
Function  LowerCase(c:UnicodeChar):UnicodeChar;
begin
  result:=JLCharacter.toLowerCase(c);
end;


{$define FPC_HAS_LOWERCASE_UNICODESTR}
function LowerCase(const s : UnicodeString) : UnicodeString;
begin
  result:=JLString(s).toLowerCase;
end;


{$define FPC_HAS_SETSTRING_UNICODESTR_PUNICODECHAR}
Procedure fpc_setstring_unicodestr_pwidechar(Out S : UnicodeString; Buf : PUnicodeChar; Len : SizeInt); compilerproc;
begin
  if assigned(buf) and (Len>0) then
    s:=JLString.Create(TJCharArray(Buf),0,Len)
  else
    s:='';
end;


{$define FPC_HAS_UTF8ENCODE_UNICODESTRING}
function UTF8Encode(const s : UnicodeString) : RawByteString;
  var
    i : SizeInt;
    hs : UTF8String;
    chars: array of widechar;
  begin
    result:='';
    if s='' then
      exit;
    SetLength(hs,length(s)*3);
    chars:=JLString(s).toCharArray;
    i:=UnicodeToUtf8(pchar(hs),length(hs)+1,pwidechar(chars),length(s));
    if i>0 then
      begin
        SetLength(hs,i-1);
        result:=hs;
      end;
  end;


{$define FPC_HAS_UTF8DECODE_UNICODESTRING}
function UTF8Decode(const s : RawByteString): UnicodeString;
  var
    i : SizeInt;
    chars: array of widechar;
  begin
    result:='';
    if s='' then
      exit;
    SetLength(chars,length(s)+1);
    i:=Utf8ToUnicode(pwidechar(chars),length(s)+1,pchar(s),length(s));
    if i>0 then
      result:=JLString.Create(chars,0,i-1);
  end;


{$define FPC_HAS_UCS4STRING_TO_UNICODESTR}

{ concatenates an utf-32 char to a unicodestring. S *must* be unique when entering. }
procedure ConcatUTF32ToUnicodeStr(const nc: UCS4Char; var S: JLStringBuilder; var index: SizeInt);
begin
  { if nc > $ffff, we need two places }
  if (index+ord(nc > $ffff)>s.length) then
    if (s.length < 10*256) then
      s.setLength(s.length+10)
    else
      s.setlength(s.length+s.length shr 8);
  if (nc<$ffff) then
    begin
      s.setCharAt(index-1,unicodechar(nc));
      inc(index);
    end
  else if (dword(nc)<=$10ffff) then
    begin
      s.setCharAt(index-1,unicodechar((nc - $10000) shr 10 + $d800));
      s.setCharAt(index,unicodechar((nc - $10000) and $3ff + $dc00));
      inc(index,2);
    end
  else
    { invalid code point }
    begin
      s.setCharAt(index-1,'?');
      inc(index);
    end;
end;


function UCS4StringToUnicodeString(const s : UCS4String) : UnicodeString;
  var
    i        : SizeInt;
    resindex : SizeInt;
    tmpres: JLStringBuilder;
  begin
    { skip terminating #0 }
    tmpres:=JLStringBuilder.Create(length(s)-1);
    resindex:=1;
    for i:=0 to high(s)-1 do
      ConcatUTF32ToUnicodeStr(s[i],tmpres,resindex);
    { adjust result length (may be too big due to growing }
    { for surrogate pairs)                                }
    tmpres.setLength(resindex-1);
    result:=tmpres.toString;
  end;

procedure UCS4Encode(p: PWideChar; len: sizeint; out res: UCS4String); forward;

{$define FPC_HAS_UCS4STRING_TO_UNICODESTR}
function UnicodeStringToUCS4String(const s : UnicodeString) : UCS4String;
  begin
    UCS4Encode(PWideChar(JLString(s).toCharArray),Length(s),result);
  end;

{$define FPC_HAS_WIDESTR_TO_UCS4STRING}
function WideStringToUCS4String(const s : WideString) : UCS4String;
  begin
    UCS4Encode(PWideChar(JLString(s).toCharArray),Length(s),result);
  end;


{$define FPC_HAS_UCS4STRING_TO_WIDESTR}
function UCS4StringToWideString(const s : UCS4String) : WideString;
  begin
    result:=UCS4StringToUnicodeString(s);
  end;


function StringElementSize(const S : UnicodeString): Word;
  begin
    result:=sizeof(unicodechar);
  end;


function StringRefCount(const S : UnicodeString): SizeInt;
  begin
    if assigned(pointer(s)) then
      result:=1
    else
      result:=0;
  end;


function StringCodePage(const S : UnicodeString): TSystemCodePage;
  begin
    if assigned(pointer(s)) then
      result:=CP_UTF16BE
    else
      result:=DefaultUnicodeCodePage;
  end;

{$define FPC_HAS_TOSINGLEBYTEFILESYSTEMENCODEDFILENAME_UNICODESTRING}
Function ToSingleByteFileSystemEncodedFileName(const Str: UnicodeString): RawByteString;
Begin
  result:=AnsiString(AnsistringClass.Create(Str,DefaultFileSystemCodePage));
End;

{$define FPC_HAS_TOSINGLEBYTEFILESYSTEMENCODEDFILENAME_UNICODECHARARRAY}
Function ToSingleByteFileSystemEncodedFileName(const arr: array of widechar): RawByteString;
Begin
  result:=AnsiString(AnsistringClass.Create(arr,DefaultFileSystemCodePage));
End;


{ *************************************************************************** }
{ ************************* Collator threadvar ****************************** }
{ *************************************************************************** }

function TCollatorThreadVar.InitialValue: JLObject;
  begin
    { get a copy, since we modify the collator (e.g. setting the strength) }
    result:=JTCollator.getInstance.clone
  end;


{ *************************************************************************** }
{ ************************ Helpers for en/decode **************************** }
{ *************************************************************************** }

function GetOrInsertNewEnDecoder(hm: JUWeakHashMap; cp: TSystemCodePage; decoder: boolean): JLObject;
  var
    cs: JNCCharSet;
    replacement: array[0..0] of jbyte;
  begin
    result:=hm.get(JLInteger.valueOf(cp));
    if not assigned(result) then
      begin
        try
          cs:=JNCCharSet.forName(win2javacs(cp));
        except
          { does not exist or not supported, fall back to ASCII like on other
            platforms}
          cs:=JNCCharset.forName('US-ASCII')
        end;
        if decoder then
          begin
            result:=cs.newDecoder;
            JNCCharsetDecoder(result).replaceWith('?');
          end
        else
          begin
            result:=cs.newEncoder;
            replacement[0]:=ord('?');
            JNCCharsetEncoder(result).replaceWith(replacement);
          end;
        { store in weak hashmap for future (possible) reuse }
        hm.put(JLInteger.Create(cp),result);
      end;
  end;

{ *************************************************************************** }
{ ************************** Decoder threadvar ****************************** }
{ *************************************************************************** }

function TCharsetDecoderThreadvar.InitialValue: JLObject;
  begin
    result:=JUWeakHashMap.Create;
  end;


function TCharsetDecoderThreadvar.getForCodePage(cp: TSystemCodePage): JNCCharsetDecoder;
  var
    hm: JUWeakHashMap;
  begin
    hm:=JUWeakHashMap(get);
    result:=JNCCharsetDecoder(GetOrInsertNewEnDecoder(hm,cp,true));
  end;


{ *************************************************************************** }
{ ************************** Encoder threadvar ****************************** }
{ *************************************************************************** }

function TCharsetEncoderThreadvar.InitialValue: JLObject;
  begin
    result:=JUWeakHashMap.Create;
  end;


function TCharsetEncoderThreadvar.getForCodePage(cp: TSystemCodePage): JNCCharsetEncoder;
  var
    hm: JUWeakHashMap;
  begin
    hm:=JUWeakHashMap(get);
    result:=JNCCharsetEncoder(GetOrInsertNewEnDecoder(hm,cp,false));
  end;


{ *************************************************************************** }
{ ************************ TUnicodeStringManager **************************** }
{ *************************************************************************** }

class constructor TUnicodeStringManager.ClassCreate;
  begin
    collator:=TCollatorThreadVar.Create;
    decoder:=TCharsetDecoderThreadVar.Create;
    encoder:=TCharsetEncoderThreadVar.Create;
    DefaultSystemCodePage:=javacs2win(JNCCharset.defaultCharset.name);
    { unknown/unsupported -> default to ASCII (this will be used to parse
      stdin etc, so setting this to utf-8 or so won't help) }
    if DefaultSystemCodePage=65535 then
      DefaultSystemCodePage:=20127;
    DefaultFileSystemCodePage:=DefaultSystemCodePage;
    DefaultRTLFileSystemCodePage:=DefaultFileSystemCodePage;
    DefaultUnicodeCodePage:=CP_UTF16BE;
  end;


procedure TUnicodeStringManager.Wide2AnsiMoveProc(source:pwidechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
  begin
    DefaultUnicode2AnsiMove(source,dest,cp,len);
  end;


procedure TUnicodeStringManager.Ansi2WideMoveProc(source:pchar;cp : TSystemCodePage;var dest:widestring;len:SizeInt);
  begin
    DefaultAnsi2UnicodeMove(source,cp,dest,len);
  end;


function TUnicodeStringManager.UpperWideStringProc(const S: WideString): WideString;
  begin
    result:=upcase(s);
  end;


function TUnicodeStringManager.LowerWideStringProc(const S: WideString): WideString;
  begin
    result:=lowercase(s);
  end;


function TUnicodeStringManager.CompareWideStringProc(const s1, s2 : WideString) : PtrInt;
  var
    localcollator: JTCollator;
  begin
    localcollator:=JTCollator(collator.get);
    localcollator.setStrength(JTCollator.IDENTICAL);
    result:=localcollator.compare(s1,s2);
  end;


function TUnicodeStringManager.CompareTextWideStringProc(const s1, s2 : WideString): PtrInt;
  var
    localcollator: JTCollator;
  begin
    localcollator:=JTCollator(collator.get);
    localcollator.setStrength(JTCollator.TERTIARY);
    result:=localcollator.compare(s1,s2);
  end;


function TUnicodeStringManager.CharLengthPCharProc(const Str: PChar; Index: PtrInt): PtrInt;
  var
    localdecoder: JNCCharsetDecoder;
  begin
    localdecoder:=JNCCharsetDecoder(decoder.get);
    localdecoder.reset;
    localdecoder.onMalformedInput(JNCCodingErrorAction.fREPLACE);
    localdecoder.onUnmappableCharacter(JNCCodingErrorAction.fREPLACE);
    result:=localdecoder.decode(JNByteBuffer.wrap(TJByteArray(Str),Index,length(Str)-Index)).length;
  end;


function TUnicodeStringManager.CodePointLengthProc(const Str: PChar; Index, MaxLookAhead: PtrInt): Ptrint;
  var
    localdecoder: JNCCharsetDecoder;
    inbuf: JNByteBuffer;
    outbuf: JNCharBuffer;
    coderres: JNCCoderResult;
    limit, maxlimit: longint;
  begin
    localdecoder:=JNCCharsetDecoder(decoder.get);
    localdecoder.reset;
    localdecoder.onMalformedInput(JNCCodingErrorAction.fREPORT);
    localdecoder.onUnmappableCharacter(JNCCodingErrorAction.fREPORT);
    localdecoder.reset;
    limit:=0;
    maxlimit:=min(length(Str)-Index,MaxLookAhead);
    { end of pchar? }
    if maxlimit=0 then
      begin
        result:=0;
        exit;
      end;
    inbuf:=JNByteBuffer.wrap(TJByteArray(Str),Index,Index+maxlimit);
    { we will get at most 2 output characters (when decoding from UTF-32 to
      UTF-16) }
    outbuf:=JNCharBuffer.allocate(2);
    { keep trying to decode until we managed to decode one character or
      reached the limit }
    repeat
      inc(limit);
      inbuf.limit(limit);
      coderres:=localdecoder.decode(inbuf,outbuf,true);
    until not coderres.isError or
          (limit=MaxLookAhead);
    if not coderres.isError then
      result:=inbuf.limit
    else
      result:=-1;
  end;


function TUnicodeStringManager.UpperAnsiStringProc(const s : ansistring) : ansistring;
  begin
    result:=UpperWideStringProc(s);
  end;


function TUnicodeStringManager.LowerAnsiStringProc(const s : ansistring) : ansistring;
  begin
    result:=LowerWideStringProc(s);
  end;


function TUnicodeStringManager.CompareStrAnsiStringProc(const S1, S2: ansistring): PtrInt;
  begin
    result:=CompareUnicodeStringProc(S1,S2);
  end;


function TUnicodeStringManager.CompareTextAnsiStringProc(const S1, S2: ansistring): PtrInt;
  begin
    result:=CompareTextUnicodeStringProc(S1,S2);
  end;


function TUnicodeStringManager.StrCompAnsiStringProc(S1, S2: PChar): PtrInt;
  var
    str1,str2: unicodestring;
  begin
    str1:=JLString.Create(TJCharArray(S1),0,length(S1));
    str2:=JLString.Create(TJCharArray(S2),0,length(S2));
    result:=CompareUnicodeStringProc(str1,str2);
  end;


function TUnicodeStringManager.StrICompAnsiStringProc(S1, S2: PChar): PtrInt;
  var
    str1,str2: unicodestring;
  begin
    str1:=JLString.Create(TJCharArray(S1),0,length(S1));
    str2:=JLString.Create(TJCharArray(S2),0,length(S2));
    result:=CompareTextUnicodeStringProc(str1,str2);
  end;


function TUnicodeStringManager.StrLCompAnsiStringProc(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    str1,str2: unicodestring;
  begin
    str1:=JLString.Create(TJCharArray(S1),0,min(length(S1),MaxLen));
    str2:=JLString.Create(TJCharArray(S2),0,min(length(S2),MaxLen));
    result:=CompareUnicodeStringProc(str1,str2);
  end;


function TUnicodeStringManager.StrLICompAnsiStringProc(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    str1,str2: unicodestring;
  begin
    str1:=JLString.Create(TJCharArray(S1),0,min(length(S1),MaxLen));
    str2:=JLString.Create(TJCharArray(S2),0,min(length(S2),MaxLen));
    result:=CompareTextUnicodeStringProc(str1,str2);
  end;


function TUnicodeStringManager.StrLowerAnsiStringProc(Str: PChar): PChar;
  var
    ustr: unicodestring;
  begin
    ustr:=JLString.Create(TJCharArray(Str),0,length(Str));
    result:=PChar(AnsiStringClass(ansistring(LowerWideStringProc(ustr))).fdata);
  end;


function TUnicodeStringManager.StrUpperAnsiStringProc(Str: PChar): PChar;
  var
    ustr: unicodestring;
  begin
    ustr:=JLString.Create(TJCharArray(Str),0,length(Str));
    result:=PChar(AnsiStringClass(ansistring(UpperWideStringProc(ustr))).fdata);
  end;


procedure TUnicodeStringManager.Unicode2AnsiMoveProc(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
  begin
    DefaultUnicode2AnsiMove(source,dest,cp,len);
  end;


procedure TUnicodeStringManager.Ansi2UnicodeMoveProc(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);
  begin
    DefaultAnsi2UnicodeMove(source,cp,dest,len);
  end;


function TUnicodeStringManager.UpperUnicodeStringProc(const S: UnicodeString): UnicodeString;
  begin
    result:=UpperWideStringProc(S);
  end;


function TUnicodeStringManager.LowerUnicodeStringProc(const S: UnicodeString): UnicodeString;
  begin
    result:=LowerWideStringProc(S);
  end;


function TUnicodeStringManager.CompareUnicodeStringProc(const s1, s2 : UnicodeString) : PtrInt;
  begin
    result:=CompareWideStringProc(s1,s2);
  end;


function TUnicodeStringManager.CompareTextUnicodeStringProc(const s1, s2 : UnicodeString): PtrInt;
  begin
    result:=CompareTextWideStringProc(s1,s2);
  end;



procedure initunicodestringmanager;
  begin
    widestringmanager:=TUnicodeStringManager.Create;
  end;

