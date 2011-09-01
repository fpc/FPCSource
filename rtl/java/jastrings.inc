{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    This file implements AnsiStrings for FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This will release some functions for special shortstring support }
{ define EXTRAANSISHORT}

constructor AnsistringClass.Create(len: longint);
begin
  { +1 for terminating #0 }
  setlength(fdata,len+1);
end;

constructor AnsistringClass.Create(const arr: array of ansichar; length: longint);
begin
  { make explicit copy so that changing the array afterwards doesn't change
    the string }
  if length=0 then
    begin
      { terminating #0 }
      setlength(fdata,1);
      exit;
    end;
  setlength(fdata,length+1);
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(fdata),0,length);
  // last char is already #0 because of setlength
end;


constructor AnsistringClass.Create(const arr: array of unicodechar);
begin
  if high(arr)=-1 then
    begin
      { terminating #0 }
      setlength(fdata,1);
      exit;
    end;
  fdata:=TAnsiCharArray(JLString.Create(arr).getBytes);
  setlength(fdata,system.length(fdata)+1);
  // last char is already #0 because of setlength
end;


constructor AnsistringClass.Create(const u: unicodestring);
begin
  if system.length(u)=0 then
    begin
      { terminating #0 }
      setlength(fdata,1);
      exit;
    end;
  fdata:=TAnsiCharArray(JLString(u).getBytes);
  setlength(fdata,system.length(fdata)+1);
  // last char is already #0 because of setlength
end;


constructor AnsistringClass.Create(const a: ansistring);
begin
  Create(AnsistringClass(a).fdata,system.length(AnsistringClass(a).fdata)-1);
end;


constructor AnsistringClass.Create(const s: shortstring);
begin
  Create(ShortstringClass(@s).fdata,system.length(ShortstringClass(@s).fdata));
end;


constructor AnsistringClass.Create(ch: ansichar);
begin
  setlength(fdata,2);
  fdata[0]:=ch;
  // last char is already #0 because of setlength
end;


constructor AnsistringClass.Create(ch: unicodechar);
begin
  fdata:=TAnsiCharArray(JLString.Create(ch).getBytes);
  setlength(fdata,system.length(fdata)+1);
  // last char is already #0 because of setlength
end;


class function AnsistringClass.CreateFromLiteralStringBytes(const u: unicodestring): ansistring;
var
  res: AnsistringClass;
  i: longint;
begin
  { used to construct constant ansistrings from Java string constants }
  res:=AnsistringClass.Create;
  { +1 for terminating #0 }
  setlength(res.fdata,system.length(u)+1);
  for i:=1 to system.length(u) do
    res.fdata[i-1]:=ansichar(ord(u[i]));
  result:=ansistring(res);
end;


function AnsistringClass.charAt(index: jint): ansichar;
begin
  { index is already decreased by one, because same calling code is used for
    JLString.charAt() }
  result:=fdata[index];
end;


function AnsistringClass.toUnicodeString: unicodestring;
begin
  result:=UnicodeString(JLString.Create(TJByteArray(fdata),0,system.length(fdata)-1));
end;


function AnsistringClass.toShortstring(maxlen: byte): shortstring;
begin
  result:=pshortstring(ShortstringClass.Create(ansistring(self),maxlen))^;
end;


function AnsistringClass.toString: JLString;
begin
  result:=JLString.Create(TJByteArray(fdata),0,system.length(fdata)-1);
end;

(*
function AnsistringClass.concat(const a: ansistring): ansistring;
var
  newdata: array of ansichar;
  addlen: sizeint;
begin
  addlen:=length(a);
  thislen:=this.length;
  setlength(newdata,addlen+thislen);
  if thislen>0 then
    JLSystem.ArrayCopy(JLObject(fdata),0,JLObject(newdata),0,thislen);
  if addlen>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(a).fdata),0,JLObject(newdata),thislen,addlen);
end;


procedure AnsistringClass.concatmultiple(const arr: array of ansistring): ansistring;
  Var
    i  : longint;
    size, newsize : sizeint;
    curlen, addlen : sizeint
    newdata: array of ansichar;
  begin
    { First calculate size of the result so we can allocate an array of
      the right size }
    NewSize:=0;
    for i:=low(arr) to high(arr) do
      inc(newsize,length(arr[i]));
    setlength(newdata,newsize);
    curlen
    for i:=low(arr) to high(arr) do
      begin
        if length(arr[i])>0 then
          sb.append(arr[i]);
      end;
    DestS:=sb.toString;
end;
*)

function AnsiStringClass.length: jint;
begin
  result:=system.length(fdata)-1;
end;

{****************************************************************************
                    Internal functions, not in interface.
****************************************************************************}

{$ifndef FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}
{$define FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}
procedure fpc_pchar_ansistr_intern_charmove(const src: pchar; const srcindex: sizeint; var dst: ansistring; const dstindex, len: sizeint); {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  JLSystem.arraycopy(JLObject(src),srcindex,JLObject(AnsistringClass(dst).fdata),dstindex,len);
end;
{$endif FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}


{$define FPC_HAS_NEWANSISTR}
Function NewAnsiString(Len : SizeInt) : Pointer;
{
  Allocate a new AnsiString on the heap.
  initialize it to zero length and reference count 1.
}
begin
  result:=AnsistringClass.Create(len);
end;

{ not required }
{$define FPC_HAS_DISPOSE_ANSISTR}
{$define FPC_SYSTEM_HAS_ANSISTR_DECR_REF}
{$define FPC_SYSTEM_HAS_ANSISTR_INCR_REF}
{$define FPC_HAS_ANSISTR_ASSIGN}

{$define FPC_HAS_ANSISTR_CONCAT}
procedure fpc_AnsiStr_Concat (var DestS:ansistring;const S1,S2 : AnsiString); compilerproc;
var
  newdata: array of ansichar;
  thislen, addlen: sizeint;
begin
  thislen:=length(s1);
  addlen:=length(s2);
  { +1 for terminating #0 }
  setlength(newdata,thislen+addlen+1);
  if thislen>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(s1).fdata),0,JLObject(newdata),0,thislen);
  if addlen>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(s2).fdata),0,JLObject(newdata),thislen,addlen);
  dests:=Ansistring(AnsistringClass.Create);
  AnsistringClass(dests).fdata:=newdata;
end;


{$define FPC_HAS_ANSISTR_CONCAT_MULTI}
procedure fpc_AnsiStr_Concat_multi (var DestS:Ansistring;const sarr:array of Ansistring); compilerproc;
  Var
    i  : longint;
    size, newsize : sizeint;
    curlen, addlen, nextlen : sizeint;
    newdata: array of ansichar;
    res : AnsistringClass;
  begin
    { First calculate size of the result so we can allocate an array of
      the right size }
    NewSize:=0;
    for i:=low(sarr) to high(sarr) do
      inc(newsize,length(sarr[i]));
    { +1 for terminating #0 }
    setlength(newdata,newsize+1);
    curlen:=0;
    for i:=low(sarr) to high(sarr) do
      begin
        nextlen:=length(sarr[i]);
        if nextlen>0 then
          begin
            JLSystem.ArrayCopy(JLObject(AnsistringClass(sarr[i]).fdata),0,JLObject(newdata),curlen,nextlen);
            inc(curlen,nextlen);
          end;
      end;
    res:=AnsistringClass.Create;
    res.fdata:=newdata;
    dests:=Ansistring(res);
end;


{$define FPC_HAS_ANSISTR_TO_SHORTSTR}
procedure fpc_AnsiStr_To_ShortStr (out res: shortstring; const S2 : Ansistring); compilerproc;
{
  Converts a AnsiString to a ShortString;
}
Var
  Size : SizeInt;
begin
  if S2='' then
   res:=''
  else
   begin
     Size:=Length(S2);
     If Size>high(res) then
       Size:=high(res);
     JLSystem.ArrayCopy(JLObject(AnsistringClass(S2).fdata),0,JLObject(ShortstringClass(@res).fdata),0,Size);
     setlength(res,Size);
   end;
end;


{$define FPC_HAS_SHORTSTR_TO_ANSISTR}
Function fpc_ShortStr_To_AnsiStr (Const S2 : ShortString): ansistring; compilerproc;
{
  Converts a ShortString to a AnsiString;
}
Var
  Size : SizeInt;
begin
  Size:=Length(S2);
  Setlength(result,Size);
  if Size>0 then
    JLSystem.ArrayCopy(JLObject(ShortstringClass(@S2).fdata),0,JLObject(AnsistringClass(result).fdata),0,Size);
end;


{$define FPC_HAS_CHAR_TO_ANSISTR}
Function fpc_Char_To_AnsiStr(const c : AnsiChar): AnsiString; compilerproc;
{
  Converts a Char to a AnsiString;
}
begin
  result:=ansistring(AnsistringClass.Create(c));
end;


{$define FPC_HAS_PCHAR_TO_ANSISTR}
Function fpc_PChar_To_AnsiStr(const p : pchar): ansistring; compilerproc;
var
  i, len: longint;
  arr: TAnsiCharArray;
begin
  arr:=TAnsiCharArray(p);
  i:=0;
  while arr[i]<>#0 do
    inc(i);
  if i<>0 then
    result:=ansistring(AnsiStringClass.create(arr,i))
  else
    result:=''
end;


{$define FPC_HAS_CHARARRAY_TO_ANSISTR}
Function fpc_CharArray_To_AnsiStr(const arr: array of ansichar; zerobased: boolean = true): ansistring; compilerproc;
var
  i,j  : SizeInt;
  localarr: array of jbyte;
  foundnull: boolean;
  res: AnsistringClass;
begin
  if (zerobased) then
    begin
      if (arr[0]=#0) Then
      begin
        fpc_CharArray_To_AnsiStr := '';
        exit;
      end;
      foundnull:=false;
      j:=0;
      for i:=low(arr) to high(arr) do
        if arr[i]=#0 then
          begin
            foundnull:=true;
            j:=i;
            break;
          end;
      if foundnull then
        begin
          res:=AnsistringClass.Create(arr,j);
          exit;
        end
    end
  else
    begin
      res:=AnsistringClass.Create(arr);
      exit;
    end;
  res:=AnsistringClass.Create;
  { +1 for terminating 0 }
  setlength(res.fdata,high(arr)+2);
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(res.fdata),0,high(arr)+1);
  result:=Ansistring(res);
end;


{$define FPC_HAS_ANSISTR_TO_CHARARRAY}
procedure fpc_ansistr_to_chararray(out res: array of ansichar; const src: ansistring); compilerproc;
var
  len: longint;
begin
  len:=length(src);
  if len>length(res) then
    len:=length(res);
  { make sure we don't try to access element 1 of the ansistring if it's nil }
  if len>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(src).fdata),0,JLObject(@res),0,len);
  if len<=high(res) then
    JUArrays.fill(TJByteArray(@res),len,high(res),0);
end;


function fpc_ansistr_setchar(const s: AnsiString; const index: longint; const ch: ansichar): AnsiString; compilerproc;
var
  res: AnsistringClass;
begin
  res:=AnsistringClass.Create(s);
  res.fdata[index-1]:=ch;
  result:=Ansistring(res);
end;


{$define FPC_HAS_ANSISTR_COMPARE}
Function fpc_AnsiStr_Compare(const S1,S2 : AnsiString): SizeInt;  compilerproc;
{
  Compares 2 AnsiStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  MaxI,Temp, i : SizeInt;
begin
  if JLObject(S1)=JLObject(S2) then
    begin
      result:=0;
      exit;
    end;
  Maxi:=Length(S1);
  temp:=Length(S2);
  If MaxI>Temp then
    MaxI:=Temp;
  if MaxI>0 then
    begin
      for i:=0 to MaxI-1 do
        begin
          result:=ord(AnsistringClass(S1).fdata[i])-ord(AnsistringClass(S2).fdata[i]);
          if result<>0 then
            exit;
        end;
      result:=Length(S1)-Length(S2);
    end
  else
    result:=Length(S1)-Length(S2);
end;


{$define FPC_HAS_ANSISTR_COMPARE_EQUAL}
Function fpc_AnsiStr_Compare_equal(const S1,S2 : AnsiString): SizeInt; compilerproc;
{
  Compares 2 AnsiStrings for equality/inequality only;
  The result is
   0 if S1=S2
   <>0 if S1<>S2
}
Var
  MaxI,Temp : SizeInt;
begin
  if JLObject(S1)=JLObject(S2) then
    begin
      result:=0;
      exit;
    end;
  result:=ord(not JUArrays.equals(TJByteArray(AnsistringClass(S1).fdata),TJByteArray(AnsistringClass(S2).fdata)));
end;


{ not required, the JVM does the range checking for us }
{$define FPC_HAS_ANSISTR_CHECKRANGE}


{$define FPC_HAS_ANSISTR_SETLENGTH}
function fpc_AnsiStr_SetLength (S : AnsiString; l : SizeInt): Ansistring; compilerproc;
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
begin
  if not assigned(AnsistringClass(s)) then
    result:=ansistring(AnsistringClass.Create)
  else
    result:=s;
  { +1 for terminating #0 }
  setlength(AnsistringClass(result).fdata,l+1);
  { null-terminate in case the string became shorter }
  AnsistringClass(result).fdata[l]:=#0;
end;

{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

{ lie, not needed }
{$define FPC_SYSTEM_HAS_TRUELY_ANSISTR_UNIQUE}

{ can't implement reference counting since no control over what javacc-compiled
  code does with ansistrings -> always create a copy }
{$define FPC_SYSTEM_HAS_ANSISTR_UNIQUE}
procedure FPC_ANSISTR_UNIQUE(var s: AnsiString); inline;
begin
  s:=ansistring(AnsistringClass.Create(s));
end;
(*
Function fpc_ansistr_Unique(Var S : jlobject): jlobject; compilerproc;
begin
  s:=AnsistringClass.Create(ansistring(s));
  result:=s;
end;
*)
{$define FPC_HAS_ANSISTR_APPEND_CHAR}
Procedure fpc_ansistr_append_char(Var S : AnsiString;c : ansichar); compilerproc;
var
  curlen: sizeint;
begin
  curlen:=length(s);
  SetLength(s,curlen+1);
  AnsistringClass(s).fdata[curlen]:=c;
end;


{$define FPC_HAS_ANSISTR_APPEND_SHORTSTR}
Procedure fpc_ansistr_append_shortstring(Var S : AnsiString;const Str : ShortString); compilerproc;
var
   ofs : SizeInt;
begin
   if Str='' then
     exit;
   ofs:=Length(S);
   SetLength(S,ofs+length(Str));
   { the pbyte cast avoids an unique call which isn't necessary because SetLength was just called }
   JLSystem.ArrayCopy(JLObject(ShortstringClass(@Str).fdata),0,JLObject(AnsistringClass(S).fdata),ofs,length(Str));
end;


{$define FPC_HAS_ANSISTR_APPEND_ANSISTR}
Procedure fpc_ansistr_append_ansistring(Var S : AnsiString;const Str : AnsiString); compilerproc;
var
  ofs, strlength: longint;
begin
   if Str='' then
     exit;
   strlength:=length(str);
   ofs:=Length(S);
   { no problem if s and str are the same string, because "var" parameters are
     copy-in/out for ansistring }
   SetLength(S,ofs+strlength);
   JLSystem.ArrayCopy(JLObject(AnsistringClass(Str).fdata),0,JLObject(AnsistringClass(S).fdata),ofs,strlength);
end;


{$define FPC_HAS_ANSISTR_COPY}
Function Fpc_Ansistr_Copy (Const S : AnsiString; Index,Size : SizeInt) : AnsiString;compilerproc;
var
  res: AnsistringClass;
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
   begin
     res:=AnsistringClass.Create;
     { +1 for terminating #0 }
     setlength(res.fdata,size+1);
     JLSystem.ArrayCopy(JLObject(AnsistringClass(S).fdata),index,JLObject(res.fdata),0,size);
     result:=ansistring(res);
   end;
  { default function result is empty string }
end;


{$define FPC_HAS_POS_SHORTSTR_ANSISTR}
Function Pos (Const Substr : ShortString; Const Source : AnsiString) : SizeInt;
var
  i,j,k,MaxLen, SubstrLen : SizeInt;
begin
  Pos:=0;
  SubstrLen:=Length(SubStr);
  if SubstrLen>0 then
   begin
     MaxLen:=Length(source)-Length(SubStr);
     i:=0;
     while (i<=MaxLen) do
      begin
        inc(i);
        j:=0;
        k:=i-1;
        while (j<SubstrLen) and
              (ShortStringClass(@SubStr).fdata[j]=AnsistringClass(Source).fdata[k]) do
          begin
            inc(j);
            inc(k);
          end;
        if (j=SubstrLen) then
         begin
           Pos:=i;
           exit;
         end;
      end;
   end;
end;


{$define FPC_HAS_POS_ANSISTR_ANSISTR}
Function Pos (Const Substr : AnsiString; Const Source : AnsiString) : SizeInt;
var
  i,j,k,MaxLen, SubstrLen : SizeInt;
begin
  Pos:=0;
  SubstrLen:=Length(SubStr);
  if SubstrLen>0 then
   begin
     MaxLen:=Length(source)-Length(SubStr);
     i:=0;
     while (i<=MaxLen) do
      begin
        inc(i);
        j:=0;
        k:=i-1;
        while (j<SubstrLen) and
              (AnsistringClass(SubStr).fdata[j]=AnsistringClass(Source).fdata[k]) do
          begin
            inc(j);
            inc(k);
          end;
        if (j=SubstrLen) then
         begin
           Pos:=i;
           exit;
         end;
      end;
   end;
end;


{$define FPC_HAS_POS_ANSICHAR_ANSISTR}
{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos (c : AnsiChar; Const s : AnsiString) : SizeInt;
var
  i: SizeInt;
begin
  for i:=1 to length(s) do
   begin
     if AnsistringClass(s).fdata[i-1]=c then
      begin
        pos:=i;
        exit;
      end;
   end;
  pos:=0;
end;


{$define FPC_HAS_ANSISTR_OF_CHAR}
Function StringOfChar(c : char;l : SizeInt) : AnsiString;
begin
  SetLength(StringOfChar,l);
  FillChar(AnsistringClass(result).fdata,l,c);
end;


{$define FPC_HAS_UPCASE_ANSISTR}
function upcase(const s : ansistring) : ansistring;
var
  u : unicodestring;
begin
  u:=s;
  result:=upcase(u);
end;


{$define FPC_HAS_LOWERCASE_ANSISTR}
function lowercase(const s : ansistring) : ansistring;
var
  u : unicodestring;
begin
  u:=s;
  result:=lowercase(u);
end;

