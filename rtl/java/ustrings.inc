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

{$i wustrings.inc}

{
  This file contains the implementation of the UnicodeString type,
  which on the Java platforms is an alias for java.lang.String
}


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
    result:=unicodestring(JLString.Create(TJByteArray(ShortstringClass(@S2).fdata),0,length(S2)));
end;

Function fpc_UnicodeStr_To_AnsiStr (const S2 : UnicodeString): AnsiString; compilerproc;
{
  Converts a UnicodeString to an AnsiString
}
Var
  Size : SizeInt;
begin
  result:=Ansistring(AnsistringClass.Create(s2));
end;


Function fpc_AnsiStr_To_UnicodeStr (Const S2 : AnsiString): UnicodeString; compilerproc;
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

Function fpc_UnicodeStr_To_WideStr (const S2 : UnicodeString): WideString; compilerproc;
  begin
    result:=s2;
  end;


Function fpc_WideStr_To_UnicodeStr (Const S2 : WideString): UnicodeString; compilerproc;
  begin
    result:=s2;
  end;

function fpc_UnicodeStr_Concat (const S1,S2 : UnicodeString): UnicodeString; compilerproc;
Var
  sb: JLStringBuilder;
begin
  { only assign if s1 or s2 is empty }
  if (length(S1)=0) then
    begin
      result:=s2;
      exit;
    end;
  if (length(S2)=0) then
    begin
      result:=s1;
      exit;
    end;
  sb:=JLStringBuilder.create(S1);
  sb.append(s2);
  result:=sb.toString;
end;


function fpc_UnicodeStr_Concat_multi (const sarr:array of Unicodestring): unicodestring; compilerproc;
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
  result:=sb.toString;
end;


Function fpc_Char_To_UChar(const c : AnsiChar): UnicodeChar; compilerproc;
var
  str: JLString;
  arr: array of jbyte;
begin
  setlength(arr,1);
  arr[0]:=ord(c);
  result:=JLString.create(arr,0,1).charAt(0);
end;



Function fpc_Char_To_UnicodeStr(const c : AnsiChar): UnicodeString; compilerproc;
{
  Converts a AnsiChar to a UnicodeString;
}
var
  str: JLString;
  arr: array of jbyte;
begin
  setlength(arr,1);
  arr[0]:=ord(c);
  result:=JLString.create(arr,0,1);
end;


Function fpc_UChar_To_Char(const c : UnicodeChar): AnsiChar; compilerproc;
{
  Converts a UnicodeChar to a AnsiChar;
}
var
  arrb: array of jbyte;
  arrw: array of jchar;
  str: JLString;
begin
  setlength(arrw,1);
  arrw[0]:=c;
  str:=JLString.create(arrw);
  arrb:=str.getbytes();
  result:=chr(arrb[0]);
end;


Function fpc_WChar_To_UnicodeStr(const c : WideChar): UnicodeString; compilerproc;
{
  Converts a WideChar to a UnicodeString;
}
var
  arrw: array of jchar;
begin
  setlength(arrw,1);
  arrw[0]:=c;
  result:=JLString.create(arrw);
end;


Function fpc_Char_To_WChar(const c : AnsiChar): WideChar; compilerproc;
{
  Converts a AnsiChar to a WideChar;
}
var
  str: JLString;
  arr: array of jbyte;
begin
  setlength(arr,1);
  arr[0]:=ord(c);
  result:=JLString.create(arr,0,1).charAt(0);
end;


Function fpc_WChar_To_Char(const c : WideChar): AnsiChar; compilerproc;
{
  Converts a WideChar to a AnsiChar;
}
var
  arrb: array of jbyte;
  arrw: array of jchar;
begin
  setlength(arrw,1);
  arrw[0]:=c;
  arrb:=JLString.create(arrw).getbytes();
  result:=chr(arrb[0]);
end;

procedure fpc_WChar_To_ShortStr(out res : shortstring;const c : WideChar) compilerproc;
{
  Converts a WideChar to a ShortString;
}
var
  u: unicodestring;
begin
  u:=c;
  res:=u;
end;

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


Function fpc_UChar_To_AnsiStr(const c : UnicodeChar): AnsiString; compilerproc;
{
  Converts a UnicodeChar to a AnsiString;
}
var
  u: unicodestring;
begin
  u:=c;
  result:=u;
end;

(*
Function fpc_PChar_To_UnicodeStr(const p : pchar): UnicodeString; compilerproc;
Var
  L : SizeInt;
begin
  if (not assigned(p)) or (p[0]=#0) Then
  begin
    fpc_pchar_to_unicodestr := '';
    exit;
  end;
  l:=IndexChar(p^,-1,#0);
  widestringmanager.Ansi2UnicodeMoveProc(P,fpc_PChar_To_UnicodeStr,l);
end;
*)

Function fpc_CharArray_To_UnicodeStr(const arr: array of ansichar; zerobased: boolean = true): UnicodeString; compilerproc;
var
  i,j  : SizeInt;
  localarr: array of jbyte;
  foundnull: boolean;
begin
  if (zerobased) then
    begin
      if (arr[0]=#0) Then
      begin
        fpc_chararray_to_unicodestr := '';
        exit;
      end;
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
  setlength(localarr,i);
  for j:=0 to i-1 do
    localarr[j]:=ord(arr[j]);
  result:=JLString.create(localarr,0,i);
end;

(*
function fpc_UnicodeCharArray_To_ShortStr(const arr: array of unicodechar; zerobased: boolean = true): shortstring;[public,alias:'FPC_UNICODECHARARRAY_TO_SHORTSTR']; compilerproc;
var
  l: longint;
 index: longint;
 len: byte;
 temp: ansistring;
 foundnull: boolean;
begin
  l := high(arr)+1;
  if l>=256 then
    l:=255
  else if l<0 then
    l:=0;
  if zerobased then
    begin
      foundnull:=false;
      for index:=low(arr) to l-1 do
        if arr[index]=#0 then
          begin
            foundnull:=true;
            break;
          end;
      if not foundnull then
        len := l
      else
        len := index;
    end
  else
    len := l;
  result:=JLString.create(arr,0,l);
end;


Function fpc_UnicodeCharArray_To_AnsiStr(const arr: array of unicodechar; zerobased: boolean = true): AnsiString; compilerproc;
var
  i  : SizeInt;
begin
  if (zerobased) then
    begin
      i:=IndexWord(arr,high(arr)+1,0);
      if i = -1 then
        i := high(arr)+1;
    end
  else
    i := high(arr)+1;
  SetLength(fpc_UnicodeCharArray_To_AnsiStr,i);
  widestringmanager.Unicode2AnsiMoveProc (punicodechar(@arr),fpc_UnicodeCharArray_To_AnsiStr,i);
end;
*)

Function fpc_UnicodeCharArray_To_UnicodeStr(const arr: array of unicodechar; zerobased: boolean = true): UnicodeString; compilerproc;
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

Function fpc_WideCharArray_To_UnicodeStr(const arr: array of widechar; zerobased: boolean = true): UnicodeString; compilerproc;
begin
  result:=real_widechararray_to_unicodestr(arr,zerobased);
end;

{ due to their names, the following procedures should be in wstrings.inc,
  however, the compiler generates code using this functions on all platforms }
procedure fpc_WideCharArray_To_ShortStr(out res : shortstring;const arr: array of widechar; zerobased: boolean = true);[public,alias:'FPC_WIDECHARARRAY_TO_SHORTSTR']; compilerproc;
begin
  res:=real_widechararray_to_unicodestr(arr,zerobased);
end;

Function fpc_WideCharArray_To_AnsiStr(const arr: array of widechar; zerobased: boolean = true): AnsiString; compilerproc;
begin
  result:=real_widechararray_to_unicodestr(arr,zerobased);
end;

procedure fpc_unicodestr_to_chararray(out res: array of AnsiChar; const src: UnicodeString); compilerproc;
var
  i, len: SizeInt;
  temp: array of jbyte;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    begin
      temp:=JLString(src).getBytes;
      if len > length(temp) then
        len := length(temp);
      for i := 0 to len-1 do
        res[i] := chr(temp[i]);
    end;
end;


procedure fpc_unicodestr_to_unicodechararray(out res: array of unicodechar; const src: UnicodeString); compilerproc;
var
  len: SizeInt;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    begin
      if len > high(res)+1 then
        len := high(res)+1;
      JLString(src).getChars(0,len,res,0);
    end;
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


procedure fpc_ansistr_to_unicodechararray(out res: array of unicodechar; const src: AnsiString); compilerproc;
var
  len: SizeInt;
  temp: unicodestring;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    temp:=src;
  len := length(temp);
  if len > length(res) then
    len := length(res);

  JLString(temp).getChars(0,len,res,0);
  JUArrays.fill(res,len,high(res),#0);
end;

(*
procedure fpc_shortstr_to_unicodechararray(out res: array of unicodechar; const src: ShortString); compilerproc;
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
*)
procedure fpc_ansistr_to_widechararray(out res: array of widechar; const src: AnsiString); compilerproc;
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

procedure fpc_unicodestr_to_widechararray(out res: array of widechar; const src: UnicodeString); compilerproc;
var
  i, len: SizeInt;
begin
  len := length(src);
  if len > length(res) then
    len := length(res);
  JLString(src).getChars(0,len,res,0);
end;


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

function fpc_UnicodeStr_SetLength(const S : UnicodeString; l : SizeInt): UnicodeString; compilerproc;
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
  Returns new val
}
Var
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
end;

{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}
(*
function UnicodeCharToString(S : PUnicodeChar) : AnsiString;
  begin
     result:=UnicodeCharLenToString(s,Length(UnicodeString(s)));
  end;

function StringToUnicodeChar(const Src : AnsiString;Dest : PUnicodeChar;DestSize : SizeInt) : PUnicodeChar;
  var
    temp:unicodestring;
  begin
     widestringmanager.Ansi2UnicodeMoveProc(PChar(Src),temp,Length(Src));
     if Length(temp)<DestSize then
       move(temp[1],Dest^,Length(temp)*SizeOf(UnicodeChar))
     else
       move(temp[1],Dest^,(DestSize-1)*SizeOf(UnicodeChar));

     Dest[DestSize-1]:=#0;

     result:=Dest;

  end;


function WideCharToString(S : PWideChar) : AnsiString;
  begin
     result:=WideCharLenToString(s,Length(WideString(s)));
  end;


function StringToWideChar(const Src : AnsiString;Dest : PWideChar;DestSize : SizeInt) : PWideChar;
  var
    temp:widestring;
  begin
     widestringmanager.Ansi2WideMoveProc(PChar(Src),temp,Length(Src));
     if Length(temp)<DestSize then
       move(temp[1],Dest^,Length(temp)*SizeOf(WideChar))
     else
       move(temp[1],Dest^,(DestSize-1)*SizeOf(WideChar));

     Dest[DestSize-1]:=#0;

     result:=Dest;

  end;


function UnicodeCharLenToString(S : PUnicodeChar;Len : SizeInt) : AnsiString;
  begin
     //SetLength(result,Len);
     widestringmanager.Unicode2AnsiMoveproc(S,result,Len);
  end;


procedure UnicodeCharLenToStrVar(Src : PUnicodeChar;Len : SizeInt;out Dest : AnsiString);
  begin
     Dest:=UnicodeCharLenToString(Src,Len);
  end;


procedure UnicodeCharToStrVar(S : PUnicodeChar;out Dest : AnsiString);
  begin
     Dest:=UnicodeCharToString(S);
  end;


function WideCharLenToString(S : PWideChar;Len : SizeInt) : AnsiString;
  begin
     //SetLength(result,Len);
     widestringmanager.Wide2AnsiMoveproc(S,result,Len);
  end;


procedure WideCharLenToStrVar(Src : PWideChar;Len : SizeInt;out Dest : AnsiString);
  begin
     Dest:=WideCharLenToString(Src,Len);
  end;


procedure WideCharToStrVar(S : PWideChar;out Dest : AnsiString);
  begin
     Dest:=WideCharToString(S);
  end;
*)

Function fpc_unicodestr_Unique(const S : JLObject): JLObject; compilerproc;
begin
  result:=s;
end;


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
    result:=JLString(s).subString(Index,Size)
  else
    result:='';
end;


Function Pos (Const Substr : UnicodeString; Const Source : UnicodeString) : SizeInt;
begin
  Pos:=0;
  if Length(SubStr)>0 then
    Pos:=JLString(Source).indexOf(SubStr)+1;
end;


{ Faster version for a unicodechar alone }
Function Pos (c : UnicodeChar; Const s : UnicodeString) : SizeInt;
begin
  Pos:=0;
  if length(S)>0 then
    Pos:=JLString(s).indexOf(ord(c))+1;
end;

(*
Function Pos (c : AnsiString; Const s : UnicodeString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    result:=Pos(UnicodeString(c),s);
  end;


Function Pos (c : ShortString; Const s : UnicodeString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    result:=Pos(UnicodeString(c),s);
  end;


Function Pos (c : UnicodeString; Const s : AnsiString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    result:=Pos(c,UnicodeString(s));
  end;
*)

{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos (c : AnsiChar; Const s : UnicodeString) : SizeInt;
var
  i: SizeInt;
  wc : unicodechar;
begin
  wc:=c;
  result:=Pos(wc,s);
end;

(*
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
*)

Function  UpCase(c:UnicodeChar):UnicodeChar;
begin
  result:=JLCharacter.toUpperCase(c);
end;


function UpCase(const s : UnicodeString) : UnicodeString;
begin
  result:=JLString(s).toUpperCase;
end;

(*
Procedure SetString (Out S : UnicodeString; Buf : PUnicodeChar; Len : SizeInt);
begin
  SetLength(S,Len);
  If (Buf<>Nil) and (Len>0) then
    Move (Buf[0],S[1],Len*sizeof(UnicodeChar));
end;


Procedure SetString (Out S : UnicodeString; Buf : PChar; Len : SizeInt);
var
  BufLen: SizeInt;
begin
  SetLength(S,Len);
  If (Buf<>Nil) and (Len>0) then
    widestringmanager.Ansi2UnicodeMoveProc(Buf,S,Len);
end;

{$ifndef FPUNONE}
Function fpc_Val_Real_UnicodeStr(Const S : UnicodeString; out Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_UNICODESTR']; compilerproc;
Var
  SS : String;
begin
  fpc_Val_Real_UnicodeStr := 0;
  if length(S) > 255 then
    code := 256
  else
    begin
      SS := S;
      Val(SS,fpc_Val_Real_UnicodeStr,code);
    end;
end;
{$endif}

function fpc_val_enum_unicodestr(str2ordindex:pointer;const s:unicodestring;out code:valsint):longint;compilerproc;

var ss:shortstring;

begin
  if length(s)>255 then
    code:=256
  else
    begin
      ss:=s;
      val(ss,fpc_val_enum_unicodestr,code);
    end;
end;

Function fpc_Val_Currency_UnicodeStr(Const S : UnicodeString; out Code : ValSInt): Currency; [public, alias:'FPC_VAL_CURRENCY_UNICODESTR']; compilerproc;
Var
  SS : String;
begin
  if length(S) > 255 then
    begin
      fpc_Val_Currency_UnicodeStr:=0;
      code := 256;
    end
  else
    begin
      SS := S;
      Val(SS,fpc_Val_Currency_UnicodeStr,code);
    end;
end;


Function fpc_Val_UInt_UnicodeStr (Const S : UnicodeString; out Code : ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_UNICODESTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_UInt_UnicodeStr := 0;
  if length(S) > 255 then
    code := 256
  else
    begin
      SS := S;
      Val(SS,fpc_Val_UInt_UnicodeStr,code);
    end;
end;


Function fpc_Val_SInt_UnicodeStr (DestSize: SizeInt; Const S : UnicodeString; out Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_UNICODESTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_SInt_UnicodeStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
      SS := S;
      fpc_Val_SInt_UnicodeStr := int_Val_SInt_ShortStr(DestSize,SS,Code);
    end;
end;


{$ifndef CPU64}

Function fpc_Val_qword_UnicodeStr (Const S : UnicodeString; out Code : ValSInt): qword; [public, alias:'FPC_VAL_QWORD_UNICODESTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_qword_UnicodeStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_qword_UnicodeStr,Code);
    end;
end;


Function fpc_Val_int64_UnicodeStr (Const S : UnicodeString; out Code : ValSInt): Int64; [public, alias:'FPC_VAL_INT64_UNICODESTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_int64_UnicodeStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_int64_UnicodeStr,Code);
    end;
end;

{$endif CPU64}


{$ifndef FPUNONE}
procedure fpc_UnicodeStr_Float(d : ValReal;len,fr,rt : SizeInt;out s : UnicodeString);compilerproc;
var
  ss : shortstring;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
end;
{$endif}

procedure fpc_unicodestr_enum(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out s:unicodestring);compilerproc;

var ss:shortstring;

begin
  fpc_shortstr_enum(ordinal,len,typinfo,ord2strindex,ss);
  s:=ss;
end;

procedure fpc_unicodestr_bool(b : boolean;len:sizeint;out s:unicodestring);compilerproc;

var ss:shortstring;

begin
  fpc_shortstr_bool(b,len,ss);
  s:=ss;
end;

{$ifdef FPC_HAS_STR_CURRENCY}
procedure fpc_UnicodeStr_Currency(c : Currency;len,fr : SizeInt;out s : UnicodeString);compilerproc;
var
  ss : shortstring;
begin
  str(c:len:fr,ss);
  s:=ss;
end;
{$endif FPC_HAS_STR_CURRENCY}

Procedure fpc_UnicodeStr_SInt(v : ValSint; Len : SizeInt; out S : UnicodeString);compilerproc;
Var
  SS : ShortString;
begin
  Str (v:Len,SS);
  S:=SS;
end;


Procedure fpc_UnicodeStr_UInt(v : ValUInt;Len : SizeInt; out S : UnicodeString);compilerproc;
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
end;


{$ifndef CPU64}

Procedure fpc_UnicodeStr_Int64(v : Int64; Len : SizeInt; out S : UnicodeString);compilerproc;
Var
  SS : ShortString;
begin
  Str (v:Len,SS);
  S:=SS;
end;


Procedure fpc_UnicodeStr_Qword(v : Qword;Len : SizeInt; out S : UnicodeString);compilerproc;
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
end;

{$endif CPU64}
*)

(*
{ converts an utf-16 code point or surrogate pair to utf-32 }
function utf16toutf32(const S: UnicodeString; const index: SizeInt; out len: longint): UCS4Char; [public, alias: 'FPC_UTF16TOUTF32'];
var
  w: unicodechar;
begin
  { UTF-16 points in the range #$0-#$D7FF and #$E000-#$FFFF }
  { are the same in UTF-32                                  }
  w:=s[index];
  if (w<=#$d7ff) or
     (w>=#$e000) then
    begin
      result:=UCS4Char(w);
      len:=1;
    end
  { valid surrogate pair? }
  else if (w<=#$dbff) and
          { w>=#$d7ff check not needed, checked above }
          (index<length(s)) and
          (s[index+1]>=#$dc00) and
          (s[index+1]<=#$dfff) then
      { convert the surrogate pair to UTF-32 }
    begin
      result:=(UCS4Char(w)-$d800) shl 10 + (UCS4Char(s[index+1])-$dc00) + $10000;
      len:=2;
    end
  else
    { invalid surrogate -> do nothing }
    begin
      result:=UCS4Char(w);
      len:=1;
    end;
end;


function UnicodeToUtf8(Dest: PChar; Source: PUnicodeChar; MaxBytes: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    if assigned(Source) then
      Result:=UnicodeToUtf8(Dest,MaxBytes,Source,IndexWord(Source^,-1,0))
    else
      Result:=0;
  end;


function UnicodeToUtf8(Dest: PChar; MaxDestBytes: SizeUInt; Source: PUnicodeChar; SourceChars: SizeUInt): SizeUInt;
  var
    i,j : SizeUInt;
    w : word;
    lw : longword;
    len : longint;
  begin
    result:=0;
    if source=nil then
      exit;
    i:=0;
    j:=0;
    if assigned(Dest) then
      begin
        while (i<SourceChars) and (j<MaxDestBytes) do
          begin
            w:=word(Source[i]);
            case w of
              0..$7f:
                begin
                  Dest[j]:=char(w);
                  inc(j);
                end;
              $80..$7ff:
                begin
                  if j+1>=MaxDestBytes then
                    break;
                  Dest[j]:=char($c0 or (w shr 6));
                  Dest[j+1]:=char($80 or (w and $3f));
                  inc(j,2);
                end;
              $800..$d7ff,$e000..$ffff:
                begin
                  if j+2>=MaxDestBytes then
                    break;
                  Dest[j]:=char($e0 or (w shr 12));
                  Dest[j+1]:=char($80 or ((w shr 6) and $3f));
                  Dest[j+2]:=char($80 or (w and $3f));
                  inc(j,3);
                end;
              $d800..$dbff:
                {High Surrogates}
                begin
                  if j+3>=MaxDestBytes then
                    break;
                  if (i<sourcechars-1) and
                     (word(Source[i+1]) >= $dc00) and
                     (word(Source[i+1]) <= $dfff) then
                    begin
                      lw:=longword(utf16toutf32(Source[i] + Source[i+1], 1, len));
                      Dest[j]:=char($f0 or (lw shr 18));
                      Dest[j+1]:=char($80 or ((lw shr 12) and $3f));
                      Dest[j+2]:=char($80 or ((lw shr 6) and $3f));
                      Dest[j+3]:=char($80 or (lw and $3f));
                      inc(j,4);
                      inc(i);
                    end;
                end;
              end;
            inc(i);
          end;

        if j>SizeUInt(MaxDestBytes-1) then
          j:=MaxDestBytes-1;

        Dest[j]:=#0;
      end
    else
      begin
        while i<SourceChars do
          begin
            case word(Source[i]) of
              $0..$7f:
                inc(j);
              $80..$7ff:
                inc(j,2);
              $800..$d7ff,$e000..$ffff:
                inc(j,3);
              $d800..$dbff:
                begin
                  if (i<sourcechars-1) and
                     (word(Source[i+1]) >= $dc00) and
                     (word(Source[i+1]) <= $dfff) then
                    begin
                      inc(j,4);
                      inc(i);
                    end;
                end;
            end;
            inc(i);
          end;
      end;
    result:=j+1;
  end;


function Utf8ToUnicode(Dest: PUnicodeChar; Source: PChar; MaxChars: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    if assigned(Source) then
      Result:=Utf8ToUnicode(Dest,MaxChars,Source,strlen(Source))
    else
      Result:=0;
  end;


function UTF8ToUnicode(Dest: PUnicodeChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;
  const
    UNICODE_INVALID=63;
  var
    InputUTF8: SizeUInt;
    IBYTE: BYTE;
    OutputUnicode: SizeUInt;
    PRECHAR: SizeUInt;
    TempBYTE: BYTE;
    CharLen: SizeUint;
    LookAhead: SizeUInt;
    UC: SizeUInt;
  begin
    if not assigned(Source) then
      begin
        result:=0;
        exit;
      end;
    result:=SizeUInt(-1);
    InputUTF8:=0;
    OutputUnicode:=0;
    PreChar:=0;
    if Assigned(Dest) Then
      begin
        while (OutputUnicode<MaxDestChars) and (InputUTF8<SourceBytes) do
          begin
            IBYTE:=byte(Source[InputUTF8]);
            if (IBYTE and $80) = 0 then
              begin
                //One character US-ASCII, convert it to unicode
                if IBYTE = 10 then
                  begin
                    If (PreChar<>13) and FALSE then
                      begin
                        //Expand to crlf, conform UTF-8.
                        //This procedure will break the memory alocation by
                        //FPC for the widestring, so never use it. Condition never true due the "and FALSE".
                        if OutputUnicode+1<MaxDestChars then
                          begin
                            Dest[OutputUnicode]:=WideChar(13);
                            inc(OutputUnicode);
                            Dest[OutputUnicode]:=WideChar(10);
                            inc(OutputUnicode);
                            PreChar:=10;
                          end
                        else
                          begin
                            Dest[OutputUnicode]:=WideChar(13);
                            inc(OutputUnicode);
                          end;
                      end
                    else
                      begin
                        Dest[OutputUnicode]:=WideChar(IBYTE);
                        inc(OutputUnicode);
                        PreChar:=IBYTE;
                      end;
                  end
                else
                  begin
                    Dest[OutputUnicode]:=WideChar(IBYTE);
                    inc(OutputUnicode);
                    PreChar:=IBYTE;
                  end;
                inc(InputUTF8);
              end
            else
              begin
                TempByte:=IBYTE;
                CharLen:=0;
                while (TempBYTE and $80)<>0 do
                  begin
                    TempBYTE:=(TempBYTE shl 1) and $FE;
                    inc(CharLen);
                  end;
                //Test for the "CharLen" conforms UTF-8 string
                //This means the 10xxxxxx pattern.
                if SizeUInt(InputUTF8+CharLen-1)>SourceBytes then
                  begin
                    //Insuficient chars in string to decode
                    //UTF-8 array. Fallback to single char.
                    CharLen:= 1;
                  end;
                for LookAhead := 1 to CharLen-1 do
                  begin
                    if ((byte(Source[InputUTF8+LookAhead]) and $80)<>$80) or
                       ((byte(Source[InputUTF8+LookAhead]) and $40)<>$00) then
                      begin
                        //Invalid UTF-8 sequence, fallback.
                        CharLen:= LookAhead;
                        break;
                      end;
                  end;
                UC:=$FFFF;
                case CharLen of
                  1:  begin
                        //Not valid UTF-8 sequence
                        UC:=UNICODE_INVALID;
                      end;
                  2:  begin
                        //Two bytes UTF, convert it
                        UC:=(byte(Source[InputUTF8]) and $1F) shl 6;
                        UC:=UC or (byte(Source[InputUTF8+1]) and $3F);
                        if UC <= $7F then
                          begin
                            //Invalid UTF sequence.
                            UC:=UNICODE_INVALID;
                          end;
                      end;
                  3:  begin
                        //Three bytes, convert it to unicode
                        UC:= (byte(Source[InputUTF8]) and $0F) shl 12;
                        UC:= UC or ((byte(Source[InputUTF8+1]) and $3F) shl 6);
                        UC:= UC or ((byte(Source[InputUTF8+2]) and $3F));
                        if (UC <= $7FF) or (UC >= $FFFE) or ((UC >= $D800) and (UC <= $DFFF)) then
                          begin
                            //Invalid UTF-8 sequence
                            UC:= UNICODE_INVALID;
                          End;
                      end;
                  4:  begin
                        //Four bytes, convert it to two unicode characters
                        UC:= (byte(Source[InputUTF8]) and $07) shl 18;
                        UC:= UC or ((byte(Source[InputUTF8+1]) and $3F) shl 12);
                        UC:= UC or ((byte(Source[InputUTF8+2]) and $3F) shl 6);
                        UC:= UC or ((byte(Source[InputUTF8+3]) and $3F));
                        if (UC < $10000) or (UC > $10FFFF) then
                          begin
                            UC:= UNICODE_INVALID;
                          end
                        else
                          begin
                            { only store pair if room }
                            dec(UC,$10000);
                            if (OutputUnicode<MaxDestChars-1) then
                              begin
                                Dest[OutputUnicode]:=WideChar(UC shr 10 + $D800);
                                inc(OutputUnicode);
                                UC:=(UC and $3ff) + $DC00;
                              end
                            else
                              begin
                                InputUTF8:= InputUTF8 + CharLen;
                                { don't store anything }
                                CharLen:=0;
                              end;
                          end;
                      end;
                  5,6,7:  begin
                            //Invalid UTF8 to unicode conversion,
                            //mask it as invalid UNICODE too.
                            UC:=UNICODE_INVALID;
                          end;
                end;
                if CharLen > 0 then
                  begin
                    PreChar:=UC;
                    Dest[OutputUnicode]:=WideChar(UC);
                    inc(OutputUnicode);
                  end;
                InputUTF8:= InputUTF8 + CharLen;
              end;
          end;
        Result:=OutputUnicode+1;
      end
    else
      begin
        while (InputUTF8<SourceBytes) do
          begin
            IBYTE:=byte(Source[InputUTF8]);
            if (IBYTE and $80) = 0 then
              begin
                //One character US-ASCII, convert it to unicode
                if IBYTE = 10 then
                  begin
                    if (PreChar<>13) and FALSE then
                      begin
                        //Expand to crlf, conform UTF-8.
                        //This procedure will break the memory alocation by
                        //FPC for the widestring, so never use it. Condition never true due the "and FALSE".
                        inc(OutputUnicode,2);
                        PreChar:=10;
                      end
                    else
                      begin
                        inc(OutputUnicode);
                        PreChar:=IBYTE;
                      end;
                  end
                else
                  begin
                    inc(OutputUnicode);
                    PreChar:=IBYTE;
                  end;
                inc(InputUTF8);
              end
            else
              begin
                TempByte:=IBYTE;
                CharLen:=0;
                while (TempBYTE and $80)<>0 do
                  begin
                    TempBYTE:=(TempBYTE shl 1) and $FE;
                    inc(CharLen);
                  end;
                //Test for the "CharLen" conforms UTF-8 string
                //This means the 10xxxxxx pattern.
                if SizeUInt(InputUTF8+CharLen-1)>SourceBytes then
                  begin
                    //Insuficient chars in string to decode
                    //UTF-8 array. Fallback to single char.
                    CharLen:= 1;
                  end;
                for LookAhead := 1 to CharLen-1 do
                  begin
                    if ((byte(Source[InputUTF8+LookAhead]) and $80)<>$80) or
                       ((byte(Source[InputUTF8+LookAhead]) and $40)<>$00) then
                      begin
                        //Invalid UTF-8 sequence, fallback.
                        CharLen:= LookAhead;
                        break;
                      end;
                  end;
                UC:=$FFFF;
                case CharLen of
                  1:  begin
                        //Not valid UTF-8 sequence
                        UC:=UNICODE_INVALID;
                      end;
                  2:  begin
                        //Two bytes UTF, convert it
                        UC:=(byte(Source[InputUTF8]) and $1F) shl 6;
                        UC:=UC or (byte(Source[InputUTF8+1]) and $3F);
                        if UC <= $7F then
                          begin
                            //Invalid UTF sequence.
                            UC:=UNICODE_INVALID;
                          end;
                      end;
                  3:  begin
                        //Three bytes, convert it to unicode
                        UC:= (byte(Source[InputUTF8]) and $0F) shl 12;
                        UC:= UC or ((byte(Source[InputUTF8+1]) and $3F) shl 6);
                        UC:= UC or ((byte(Source[InputUTF8+2]) and $3F));
                        If (UC <= $7FF) or (UC >= $FFFE) or ((UC >= $D800) and (UC <= $DFFF)) then
                          begin
                            //Invalid UTF-8 sequence
                            UC:= UNICODE_INVALID;
                          end;
                      end;
                  4:  begin
                        //Four bytes, convert it to two unicode characters
                        UC:= (byte(Source[InputUTF8]) and $07) shl 18;
                        UC:= UC or ((byte(Source[InputUTF8+1]) and $3F) shl 12);
                        UC:= UC or ((byte(Source[InputUTF8+2]) and $3F) shl 6);
                        UC:= UC or ((byte(Source[InputUTF8+3]) and $3F));
                        if (UC < $10000) or (UC > $10FFFF) then
                          UC:= UNICODE_INVALID
                        else
                          { extra character character }
                          inc(OutputUnicode);
                      end;
                  5,6,7:  begin
                            //Invalid UTF8 to unicode conversion,
                            //mask it as invalid UNICODE too.
                            UC:=UNICODE_INVALID;
                          end;
                end;
                if CharLen > 0 then
                  begin
                    PreChar:=UC;
                    inc(OutputUnicode);
                  end;
                InputUTF8:= InputUTF8 + CharLen;
              end;
          end;
        Result:=OutputUnicode+1;
      end;
  end;


function UTF8Encode(const s : Ansistring) : UTF8String; inline;
  begin
    Result:=UTF8Encode(UnicodeString(s));
  end;


function UTF8Encode(const s : UnicodeString) : UTF8String;
  var
    i : SizeInt;
    hs : UTF8String;
  begin
    result:='';
    if s='' then
      exit;
    SetLength(hs,length(s)*3);
    i:=UnicodeToUtf8(pchar(hs),length(hs)+1,PUnicodeChar(s),length(s));
    if i>0 then
      begin
        SetLength(hs,i-1);
        result:=hs;
      end;
  end;


function UTF8Decode(const s : UTF8String): UnicodeString;
  var
    i : SizeInt;
    hs : UnicodeString;
  begin
    result:='';
    if s='' then
      exit;
    SetLength(hs,length(s));
    i:=Utf8ToUnicode(PUnicodeChar(hs),length(hs)+1,pchar(s),length(s));
    if i>0 then
      begin
        SetLength(hs,i-1);
        result:=hs;
      end;
  end;


function AnsiToUtf8(const s : ansistring): UTF8String;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=Utf8Encode(s);
  end;


function Utf8ToAnsi(const s : UTF8String) : ansistring;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=Utf8Decode(s);
  end;


function UnicodeStringToUCS4String(const s : UnicodeString) : UCS4String;
  var
    i, slen,
    destindex : SizeInt;
    len       : longint;
  begin
    slen:=length(s);
    setlength(result,slen+1);
    i:=1;
    destindex:=0;
    while (i<=slen) do
      begin
        result[destindex]:=utf16toutf32(s,i,len);
        inc(destindex);
        inc(i,len);
      end;
    { destindex <= slen (surrogate pairs may have been merged) }
    { destindex+1 for terminating #0 (dynamic arrays are       }
    { implicitely filled with zero)                            }
    setlength(result,destindex+1);
  end;


{ concatenates an utf-32 char to a unicodestring. S *must* be unique when entering. }
procedure ConcatUTF32ToUnicodeStr(const nc: UCS4Char; var S: UnicodeString; var index: SizeInt);
var
  p : PUnicodeChar;
begin
  { if nc > $ffff, we need two places }
  if (index+ord(nc > $ffff)>length(s)) then
    if (length(s) < 10*256) then
      setlength(s,length(s)+10)
    else
      setlength(s,length(s)+length(s) shr 8);
  { we know that s is unique -> avoid uniquestring calls}
  p:=@s[index];
  if (nc<$ffff) then
    begin
      p^:=unicodechar(nc);
      inc(index);
    end
  else if (dword(nc)<=$10ffff) then
    begin
      p^:=unicodechar((nc - $10000) shr 10 + $d800);
      (p+1)^:=unicodechar((nc - $10000) and $3ff + $dc00);
      inc(index,2);
    end
  else
    { invalid code point }
    begin
      p^:='?';
      inc(index);
    end;
end;


function UCS4StringToUnicodeString(const s : UCS4String) : UnicodeString;
  var
    i        : SizeInt;
    resindex : SizeInt;
  begin
    { skip terminating #0 }
    SetLength(result,length(s)-1);
    resindex:=1;
    for i:=0 to high(s)-1 do
      ConcatUTF32ToUnicodeStr(s[i],result,resindex);
    { adjust result length (may be too big due to growing }
    { for surrogate pairs)                                }
    setlength(result,resindex-1);
  end;


function WideStringToUCS4String(const s : WideString) : UCS4String;
  var
    i, slen,
    destindex : SizeInt;
    len       : longint;
  begin
    slen:=length(s);
    setlength(result,slen+1);
    i:=1;
    destindex:=0;
    while (i<=slen) do
      begin
        result[destindex]:=utf16toutf32(s,i,len);
        inc(destindex);
        inc(i,len);
      end;
    { destindex <= slen (surrogate pairs may have been merged) }
    { destindex+1 for terminating #0 (dynamic arrays are       }
    { implicitely filled with zero)                            }
    setlength(result,destindex+1);
  end;


{ concatenates an utf-32 char to a widestring. S *must* be unique when entering. }
procedure ConcatUTF32ToWideStr(const nc: UCS4Char; var S: WideString; var index: SizeInt);
var
  p : PWideChar;
begin
  { if nc > $ffff, we need two places }
  if (index+ord(nc > $ffff)>length(s)) then
    if (length(s) < 10*256) then
      setlength(s,length(s)+10)
    else
      setlength(s,length(s)+length(s) shr 8);
  { we know that s is unique -> avoid uniquestring calls}
  p:=@s[index];
  if (nc<$ffff) then
    begin
      p^:=widechar(nc);
      inc(index);
    end
  else if (dword(nc)<=$10ffff) then
    begin
      p^:=widechar((nc - $10000) shr 10 + $d800);
      (p+1)^:=widechar((nc - $10000) and $3ff + $dc00);
      inc(index,2);
    end
  else
    { invalid code point }
    begin
      p^:='?';
      inc(index);
    end;
end;


function UCS4StringToWideString(const s : UCS4String) : WideString;
  var
    i        : SizeInt;
    resindex : SizeInt;
  begin
    { skip terminating #0 }
    SetLength(result,length(s)-1);
    resindex:=1;
    for i:=0 to high(s)-1 do
      ConcatUTF32ToWideStr(s[i],result,resindex);
    { adjust result length (may be too big due to growing }
    { for surrogate pairs)                                }
    setlength(result,resindex-1);
  end;


const
  SNoUnicodestrings = 'This binary has no unicodestrings support compiled in.';
  SRecompileWithUnicodestrings = 'Recompile the application with a unicodestrings-manager in the program uses clause.';
*)

function CompareUnicodeString(const s1, s2 : UnicodeString) : PtrInt;
  begin
    widestringmanager.collator.setStrength(JTCollator.IDENTICAL);
    result:=widestringmanager.collator.compare(s1,s2);
  end;


function CompareTextUnicodeString(const s1, s2 : UnicodeString): PtrInt;
  begin
    widestringmanager.collator.setStrength(JTCollator.TERTIARY);
    result:=widestringmanager.collator.compare(s1,s2);
  end;

constructor TUnicodeStringManager.create;
  begin
  end;


procedure initunicodestringmanager;
  begin
    widestringmanager:=TUnicodeStringManager.create;
    widestringmanager.collator:=JTCollator.getInstance;
  end;

