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

constructor AnsistringClass.Create(const arr: array of ansichar);
begin
  { make explicit copy so that changing the array afterwards doesn't change
    the string }
  if high(arr)=-1 then
    exit;
  setlength(fdata,high(arr)+1);
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(fdata),0,high(arr)+1);
end;


constructor AnsistringClass.Create(const arr: array of unicodechar);
begin
  if high(arr)=-1 then
    exit;
  fdata:=TAnsiCharArray(JLString.Create(arr).getBytes);
end;


constructor AnsistringClass.Create(const u: unicodestring);
begin
  if system.length(u)=0 then
    exit;
  fdata:=TAnsiCharArray(JLString(u).getBytes);
end;


constructor AnsistringClass.Create(const a: ansistring);
begin
  Create(AnsistringClass(a).fdata);
end;


constructor AnsistringClass.Create(ch: ansichar);
begin
  setlength(fdata,1);
  fdata[0]:=ch;
end;


constructor AnsistringClass.Create(ch: unicodechar);
begin
  fdata:=TAnsiCharArray(JLString.Create(ch).getBytes);
end;


class function AnsistringClass.CreateFromLiteralStringBytes(const u: unicodestring): ansistring;
var
  res: AnsistringClass;
  i: longint;
begin
  { used to construct constant ansisitrings for Java string constants }
  res:=AnsistringClass.Create;
  setlength(res.fdata,system.length(u));
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
  result:=UnicodeString(JLString.Create(TJByteArray(fdata)));
end;


function AnsistringClass.toString: JLString;
begin
  result:=JLString.Create(TJByteArray(fdata));
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
  result:=system.length(fdata);
end;

{****************************************************************************
                    Internal functions, not in interface.
****************************************************************************}

function fpc_AnsiStr_Concat (const S1,S2 : AnsiString): ansistring; compilerproc;
var
  newdata: array of ansichar;
  thislen, addlen: sizeint;
begin
  thislen:=length(s1);
  addlen:=length(s2);
  setlength(newdata,thislen+addlen);
  if thislen>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(s1).fdata),0,JLObject(newdata),0,thislen);
  if addlen>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(s2).fdata),0,JLObject(newdata),thislen,addlen);
  result:=Ansistring(AnsistringClass.Create);
  AnsistringClass(result).fdata:=newdata;
end;


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
    setlength(newdata,newsize);
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


(*
{$ifndef FPC_STRTOSHORTSTRINGPROC}

{ the following declaration has exactly the same effect as                   }
{ procedure fpc_AnsiStr_To_ShortStr (Var S1 : ShortString;S2 : Pointer);     }
{ which is what the old helper was, so we don't need an extra implementation }
{ of the old helper (JM)                                                     }
function fpc_AnsiStr_To_ShortStr (high_of_res: SizeInt;const S2 : Ansistring): shortstring;[Public, alias: 'FPC_ANSISTR_TO_SHORTSTR'];  compilerproc;
{
  Converts a AnsiString to a ShortString;
}
Var
  Size : SizeInt;
begin
  if S2='' then
   fpc_AnsiStr_To_ShortStr:=''
  else
   begin
     Size:=Length(S2);
     If Size>high_of_res then
      Size:=high_of_res;
     Move (S2[1],fpc_AnsiStr_To_ShortStr[1],Size);
     byte(fpc_AnsiStr_To_ShortStr[0]):=byte(Size);
   end;
end;

{$else FPC_STRTOSHORTSTRINGPROC}
*)
procedure fpc_AnsiStr_To_ShortStr (out res: shortstring; const S2 : Ansistring);[Public, alias: 'FPC_ANSISTR_TO_SHORTSTR'];  compilerproc;
{
  Converts a AnsiString to a ShortString;
}
(*
Var
  Size : SizeInt;
*)
begin
(*
  if S2='' then
   res:=''
  else
   begin
     Size:=Length(S2);
     If Size>high(res) then
      Size:=high(res);
     Move (S2[1],res[1],Size);
     byte(res[0]):=byte(Size);
   end;
*)
end;
(*
{$endif FPC_STRTOSHORTSTRINGPROC}
*)

Function fpc_ShortStr_To_AnsiStr (Const S2 : ShortString): ansistring; compilerproc;
{
  Converts a ShortString to a AnsiString;
}
(*
Var
  Size : SizeInt;
*)
begin
(*
  Size:=Length(S2);
  Setlength (fpc_ShortStr_To_AnsiStr,Size);
  if Size>0 then
    Move(S2[1],Pointer(fpc_ShortStr_To_AnsiStr)^,Size);
*)
end;


Function fpc_Char_To_AnsiStr(const c : AnsiChar): AnsiString; compilerproc;
{
  Converts a Char to a AnsiString;
}
begin
  result:=ansistring(AnsistringClass.Create(c));
end;

(*
Function fpc_PChar_To_AnsiStr(const p : pchar): ansistring; compilerproc;
Var
  L : SizeInt;
begin
  if (not assigned(p)) or (p[0]=#0) Then
    L := 0
  else
    l:=IndexChar(p^,-1,#0);
  SetLength(fpc_PChar_To_AnsiStr,L);
  if L > 0 then
    Move (P[0],Pointer(fpc_PChar_To_AnsiStr)^,L)
end;
*)


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
      for i:=low(arr) to high(arr) do
        if arr[i]=#0 then
          begin
            foundnull:=true;
            break;
          end;
      if not foundnull then
        begin
          res:=AnsistringClass.Create(arr);
          exit;
        end
    end
  else
    begin
      res:=AnsistringClass.Create(arr);
      exit;
    end;
  res:=AnsistringClass.Create;
  setlength(res.fdata,i);
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(res.fdata),0,i);
  result:=Ansistring(res);
end;

procedure  fpc_ansistr_to_chararray(out res: array of ansichar; const src: ansistring); compilerproc;
var
  i, len: SizeInt;
begin
  len := length(src);
  if len > length(res) then
    len := length(res);
  { make sure we don't try to access element 1 of the ansistring if it's nil }
  if len > 0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(src).fdata),0,JLObject(@res),0,len);
  for i:=len to length(res) do
    res[i]:=#0;
end;


function fpc_ansistr_setchar(const s: AnsiString; const index: longint; const ch: ansichar): AnsiString; compilerproc;
var
  res: AnsistringClass;
begin
  res:=AnsistringClass.Create(s);
  res.fdata[index-1]:=ch;
  result:=Ansistring(res);
end;


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



function fpc_AnsiStr_SetLength (S : AnsiString; l : SizeInt): Ansistring; compilerproc;
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
Var
  lens, lena,
  movelen : SizeInt;
begin
  setlength(AnsistringClass(s).fdata,l);
  result:=s;
end;

{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}
(*
function fpc_truely_ansistr_unique(Var S : Pointer): Pointer;
Var
  SNew : Pointer;
  L    : SizeInt;
begin
  L:=PAnsiRec(Pointer(S)-FirstOff)^.len;
  SNew:=NewAnsiString (L);
  Move (Pointer(S)^,SNew^,L+1);
  PAnsiRec(SNew-FirstOff)^.len:=L;
  fpc_ansistr_decr_ref (Pointer(S));  { Thread safe }
  pointer(S):=SNew;
  pointer(result):=SNew;
end;
*)

(*
{$ifndef FPC_SYSTEM_HAS_ANSISTR_UNIQUE}
// MV: inline the basic checks for case that S is already unique.
// Rest is too complex to inline, so factor that out as a call.
Function fpc_ansistr_Unique(Var S : jlobject): jlobject; compilerproc;
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}
begin
  pointer(result) := pointer(s);
  If Pointer(S)=Nil then
    exit;
  if PAnsiRec(Pointer(S)-Firstoff)^.Ref<>1 then
    result:=fpc_truely_ansistr_unique(s);
end;
{$endif FPC_SYSTEM_HAS_ANSISTR_UNIQUE}
*)

Procedure fpc_ansistr_append_char(Var S : AnsiString;c : ansichar); compilerproc;
var
  curlen: sizeint;
begin
  curlen:=length(s);
  SetLength(s,curlen+1);
  AnsistringClass(s).fdata[curlen]:=c;
end;

Procedure fpc_ansistr_append_shortstring(Var S : AnsiString;const Str : ShortString); compilerproc;
(*
var
   ofs : SizeInt;
*)
begin
(*
   if Str='' then
     exit;
   ofs:=Length(S);
   SetLength(S,ofs+length(Str));
   { the pbyte cast avoids an unique call which isn't necessary because SetLength was just called }
   move(Str[1],(pointer(S)+ofs)^,length(Str));
   PByte(Pointer(S)+length(S))^:=0; { Terminating Zero }
*)
end;

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
     setlength(res.fdata,size);
     JLSystem.ArrayCopy(JLObject(AnsistringClass(S).fdata),index,JLObject(res.fdata),0,size);
     result:=ansistring(res);
   end;
  { default function result is empty string }
end;

(*
Function Pos (Const Substr : ShortString; Const Source : AnsiString) : SizeInt;
var
  i,MaxLen : SizeInt;
  pc : pchar;
begin
  Pos:=0;
  if Length(SubStr)>0 then
   begin
     MaxLen:=Length(source)-Length(SubStr);
     i:=0;
     pc:=@source[1];
     while (i<=MaxLen) do
      begin
        inc(i);
        if (SubStr[1]=pc^) and
           (CompareByte(Substr[1],pc^,Length(SubStr))=0) then
         begin
           Pos:=i;
           exit;
         end;
        inc(pc);
      end;
   end;
end;
*)

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


{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos (c : Char; Const s : AnsiString) : SizeInt;
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

(*
{$ifndef FPUNONE}
Function fpc_Val_Real_AnsiStr(Const S : AnsiString; out Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_ANSISTR']; compilerproc;
Var
  SS : String;
begin
  fpc_Val_Real_AnsiStr := 0;
  if length(S) > 255 then
    code := 256
  else
    begin
      SS := S;
      Val(SS,fpc_Val_Real_AnsiStr,code);
    end;
end;
{$endif}


Function fpc_Val_Currency_AnsiStr(Const S : AnsiString; out Code : ValSInt): Currency; [public, alias:'FPC_VAL_CURRENCY_ANSISTR']; compilerproc;
Var
  SS : String;
begin
  if length(S) > 255 then
    begin
      fpc_Val_Currency_AnsiStr := 0;
      code := 256;
    end
  else
    begin
      SS := S;
      Val(SS,fpc_Val_Currency_AnsiStr,code);
    end;
end;


Function fpc_Val_UInt_AnsiStr (Const S : AnsiString; out Code : ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_UInt_AnsiStr := 0;
  if length(S) > 255 then
    code := 256
  else
    begin
      SS := S;
      Val(SS,fpc_Val_UInt_AnsiStr,code);
    end;
end;


Function fpc_Val_SInt_AnsiStr (DestSize: SizeInt; Const S : AnsiString; out Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_SInt_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       fpc_Val_SInt_AnsiStr := int_Val_SInt_ShortStr(DestSize,SS,Code);
    end;
end;


{$ifndef CPU64}

Function fpc_Val_qword_AnsiStr (Const S : AnsiString; out Code : ValSInt): qword; [public, alias:'FPC_VAL_QWORD_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_qword_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_qword_AnsiStr,Code);
    end;
end;


Function fpc_Val_int64_AnsiStr (Const S : AnsiString; out Code : ValSInt): Int64; [public, alias:'FPC_VAL_INT64_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_int64_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := s;
       Val(SS,fpc_Val_int64_AnsiStr,Code);
    end;
end;

{$endif CPU64}


{$ifndef FPUNONE}
procedure fpc_AnsiStr_Float(d : ValReal;len,fr,rt : SizeInt;out s : ansistring);[public,alias:'FPC_ANSISTR_FLOAT']; compilerproc; {$IFNDEF VER2_0} Inline; {$ENDIF}
var
  ss: ShortString;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
end;
{$endif}

procedure fpc_ansistr_enum(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out s:ansistring);[public,alias:'FPC_ANSISTR_ENUM'];compilerproc; {$IFNDEF VER2_0} Inline; {$ENDIF}

var ss:shortstring;

begin
  fpc_shortstr_enum(ordinal,len,typinfo,ord2strindex,ss);
  s:=ss;
end;


procedure fpc_ansistr_bool(b : boolean;len:sizeint;out s:ansistring);[public,alias:'FPC_ANSISTR_BOOL'];compilerproc; {$IFNDEF VER2_0} Inline; {$ENDIF}
var
  ss:shortstring;
begin
  fpc_shortstr_bool(b,len,ss);
  s:=ss;
end;


function fpc_val_enum_ansistr(str2ordindex:pointer;const s:ansistring;out code:valsint):longint; [public, alias:'FPC_VAL_ENUM_ANSISTR']; compilerproc;

begin
  fpc_val_enum_ansistr:=fpc_val_enum_shortstr(str2ordindex,s,code);
end;


{$ifdef FPC_HAS_STR_CURRENCY}
procedure fpc_AnsiStr_Currency(c : currency;len,fr : SizeInt;out s : ansistring);[public,alias:'FPC_ANSISTR_CURRENCY']; compilerproc; {$IFNDEF VER2_0} Inline; {$ENDIF}
var
  ss: ShortString;
begin
  str(c:len:fr,ss);
  s:=ss;
end;
{$endif FPC_HAS_STR_CURRENCY}

Procedure fpc_AnsiStr_UInt(v : ValUInt;Len : SizeInt; out S : AnsiString);[Public,Alias : 'FPC_ANSISTR_VALUINT']; compilerproc; {$IFNDEF VER2_0} Inline; {$ENDIF}
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
end;



Procedure fpc_AnsiStr_SInt(v : ValSInt;Len : SizeInt; out S : AnsiString);[Public,Alias : 'FPC_ANSISTR_VALSINT']; compilerproc; {$IFNDEF VER2_0} Inline; {$ENDIF}
Var
  SS : ShortString;
begin
  str (v:Len,SS);
  S:=SS;
end;


{$ifndef CPU64}

Procedure fpc_AnsiStr_QWord(v : QWord;Len : SizeInt; out S : AnsiString);[Public,Alias : 'FPC_ANSISTR_QWORD']; compilerproc; {$IFNDEF VER2_0} Inline; {$ENDIF}
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
end;

Procedure fpc_AnsiStr_Int64(v : Int64; Len : SizeInt; out S : AnsiString);[Public,Alias : 'FPC_ANSISTR_INT64']; compilerproc; {$IFNDEF VER2_0} Inline; {$ENDIF}
Var
  SS : ShortString;
begin
  str (v:Len,SS);
  S:=SS;
end;

{$endif CPU64}

Procedure Delete (Var S : AnsiString; Index,Size: SizeInt);
Var
  LS : SizeInt;
begin
  ls:=Length(S);
  If (Index>LS) or (Index<=0) or (Size<=0) then
    exit;
  UniqueString (S);
  If (Size>LS-Index) then   // Size+Index gives overflow ??
     Size:=LS-Index+1;
  If (Size<=LS-Index) then
    begin
      Dec(Index);
      Move(PByte(Pointer(S))[Index+Size],PByte(Pointer(S))[Index],LS-Index-Size+1);
    end;
  Setlength(S,LS-Size);
end;


Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : SizeInt);
var
  Temp : AnsiString;
  LS : SizeInt;
begin
  If Length(Source)=0 then
   exit;
  if index <= 0 then
   index := 1;
  Ls:=Length(S);
  if index > LS then
   index := LS+1;
  Dec(Index);
  Pointer(Temp) := NewAnsiString(Length(Source)+LS);
  SetLength(Temp,Length(Source)+LS);
  If Index>0 then
    move (Pointer(S)^,Pointer(Temp)^,Index);
  Move (Pointer(Source)^,PByte(Temp)[Index],Length(Source));
  If (LS-Index)>0 then
    Move(PByte(Pointer(S))[Index],PByte(temp)[Length(Source)+index],LS-Index);
  S:=Temp;
end;


Function StringOfChar(c : char;l : SizeInt) : AnsiString;
begin
  SetLength(StringOfChar,l);
  FillChar(Pointer(StringOfChar)^,Length(StringOfChar),c);
end;
Procedure SetString (Out S : AnsiString; Buf : PChar; Len : SizeInt); {$IFNDEF VER2_0} Inline; {$ENDIF}
begin
  SetLength(S,Len);
  If (Buf<>Nil) then
    Move (Buf^,Pointer(S)^,Len);
end;

Procedure SetString (Out S : AnsiString; Buf : PWideChar; Len : SizeInt);
begin
  if (Buf<>nil) and (Len>0) then
    widestringmanager.Wide2AnsiMoveProc(Buf,S,Len)
  else
    SetLength(S, Len);
end;
*)

function upcase(const s : ansistring) : ansistring;
var
  i : SizeInt;
  u : unicodestring;
begin
  u:=s;
  result:=upcase(u);
end;


function lowercase(const s : ansistring) : ansistring;
var
  i : SizeInt;
  u : unicodestring;
begin
  u:=s;
  result:=lowercase(u);
end;

