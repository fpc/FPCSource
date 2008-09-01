{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl,
    member of the Free Pascal development team.

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
  and all things that are needed for it.
  UnicodeString is defined as a 'silent' punicodechar :
  a punicodechar that points to :

  @-8  : SizeInt for reference count;
  @-4  : SizeInt for size; size=number of bytes, not the number of chars. Divide or multiply
         with sizeof(UnicodeChar) to convert. This is needed to be compatible with Delphi and
         Windows COM BSTR.
  @    : String + Terminating #0;
  Punicodechar(Unicodestring) is a valid typecast.
  So WS[i] is converted to the address @WS+i-1.

  Constants should be assigned a reference count of -1
  Meaning that they can't be disposed of.
}

Type
  PUnicodeRec = ^TUnicodeRec;
  TUnicodeRec = Packed Record
    Ref : SizeInt;
    Len : SizeInt;
    First : UnicodeChar;
  end;

Const
  UnicodeRecLen = SizeOf(TUnicodeRec);
  UnicodeFirstOff = SizeOf(TUnicodeRec)-sizeof(UnicodeChar);

{
  Default UnicodeChar <-> Char conversion is to only convert the
  lower 127 chars, all others are translated to spaces.

  These routines can be overwritten for the Current Locale
}

procedure DefaultUnicode2AnsiMove(source:punicodechar;var dest:ansistring;len:SizeInt);
var
  i : SizeInt;
begin
  setlength(dest,len);
  for i:=1 to len do
    begin
      if word(source^)<256 then
        dest[i]:=char(word(source^))
      else
        dest[i]:='?';
      inc(source);
    end;
end;


procedure DefaultAnsi2UnicodeMove(source:pchar;var dest:unicodestring;len:SizeInt);
var
  i : SizeInt;
begin
  setlength(dest,len);
  for i:=1 to len do
    begin
      dest[i]:=unicodechar(byte(source^));
      inc(source);
    end;
end;


Procedure GetUnicodeStringManager (Var Manager : TUnicodeStringManager);
begin
  manager:=widestringmanager;
end;


Procedure SetUnicodeStringManager (Const New : TUnicodeStringManager; Var Old: TUnicodeStringManager);
begin
  Old:=widestringmanager;
  widestringmanager:=New;
end;


Procedure SetUnicodeStringManager (Const New : TUnicodeStringManager);
begin
  widestringmanager:=New;
end;


Procedure GetWideStringManager (Var Manager : TUnicodeStringManager);
begin
  manager:=widestringmanager;
end;


Procedure SetWideStringManager (Const New : TUnicodeStringManager; Var Old: TUnicodeStringManager);
begin
  Old:=widestringmanager;
  widestringmanager:=New;
end;


Procedure SetWideStringManager (Const New : TUnicodeStringManager);
begin
  widestringmanager:=New;
end;
         {****************************************************************************
                    Internal functions, not in interface.
****************************************************************************}


procedure UnicodeStringError;
  begin
    HandleErrorFrame(204,get_frame);
  end;


{$ifdef UnicodeStrDebug}
Procedure DumpUnicodeRec(S : Pointer);
begin
  If S=Nil then
    Writeln ('String is nil')
  Else
    Begin
      With PUnicodeRec(S-UnicodeFirstOff)^ do
       begin
         Write   ('(Len:',len);
         Writeln (' Ref: ',ref,')');
       end;
    end;
end;
{$endif}


Function NewUnicodeString(Len : SizeInt) : Pointer;
{
  Allocate a new UnicodeString on the heap.
  initialize it to zero length and reference count 1.
}
Var
  P : Pointer;
begin
  GetMem(P,Len*sizeof(UnicodeChar)+UnicodeRecLen);
  If P<>Nil then
    begin
     PUnicodeRec(P)^.Len:=Len*2;     { Initial length }
     PUnicodeRec(P)^.Ref:=1;         { Initial Refcount }
     PUnicodeRec(P)^.First:=#0;      { Terminating #0 }
     inc(p,UnicodeFirstOff);         { Points to string now }
    end
  else
    UnicodeStringError;
  NewUnicodeString:=P;
end;


Procedure DisposeUnicodeString(Var S : Pointer);
{
  Deallocates a UnicodeString From the heap.
}
begin
  If S=Nil then
    exit;
  Dec (S,UnicodeFirstOff);
  Freemem(S);
  S:=Nil;
end;


Procedure fpc_UnicodeStr_Decr_Ref (Var S : Pointer);[Public,Alias:'FPC_UNICODESTR_DECR_REF']; compilerproc;
{
  Decreases the ReferenceCount of a non constant unicodestring;
  If the reference count is zero, deallocate the string;
}
Type
  pSizeInt = ^SizeInt;
Var
  l : pSizeInt;
Begin
  { Zero string }
  if S=Nil then
    exit;
  { check for constant strings ...}
  l:=@PUnicodeRec(S-UnicodeFirstOff)^.Ref;
  if l^<0 then
    exit;

  { declocked does a MT safe dec and returns true, if the counter is 0 }
  if declocked(l^) then
    { Ref count dropped to zero ...
      ... remove }
  DisposeUnicodeString(S);
end;

{ alias for internal use }
Procedure fpc_UnicodeStr_Decr_Ref (Var S : Pointer);[external name 'FPC_UNICODESTR_DECR_REF'];

Procedure fpc_UnicodeStr_Incr_Ref(S : Pointer);[Public,Alias:'FPC_UNICODESTR_INCR_REF']; compilerproc;
  Begin
    If S=Nil then
      exit;
    { constant string ? }
    If PUnicodeRec(S-UnicodeFirstOff)^.Ref<0 then
      exit;
    inclocked(PUnicodeRec(S-UnicodeFirstOff)^.Ref);
  end;

{ alias for internal use }
Procedure fpc_UnicodeStr_Incr_Ref (S : Pointer);[external name 'FPC_UNICODESTR_INCR_REF'];

{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_UnicodeStr_To_ShortStr (high_of_res: SizeInt;const S2 : UnicodeString): shortstring;[Public, alias: 'FPC_UNICODESTR_TO_SHORTSTR'];  compilerproc;
{
  Converts a UnicodeString to a ShortString;
}
Var
  Size : SizeInt;
  temp : ansistring;
begin
  result:='';
  Size:=Length(S2);
  if Size>0 then
    begin
      If Size>high_of_res then
        Size:=high_of_res;
      widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(S2),temp,Size);
      result:=temp;
    end;
end;
{$else FPC_STRTOSHORTSTRINGPROC}
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
      If Size>high(res) then
        Size:=high(res);
      widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(S2),temp,Size);
      res:=temp;
    end;
end;
{$endif FPC_STRTOSHORTSTRINGPROC}


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
    begin
      widestringmanager.Ansi2UnicodeMoveProc(PChar(@S2[1]),result,Size);
      { Terminating Zero }
      PUnicodeChar(Pointer(fpc_ShortStr_To_UnicodeStr)+Size*sizeof(UnicodeChar))^:=#0;
    end;
end;


Function fpc_UnicodeStr_To_AnsiStr (const S2 : UnicodeString): AnsiString; compilerproc;
{
  Converts a UnicodeString to an AnsiString
}
Var
  Size : SizeInt;
begin
  result:='';
  Size:=Length(S2);
  if Size>0 then
    widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(Pointer(S2)),result,Size);
end;


Function fpc_AnsiStr_To_UnicodeStr (Const S2 : AnsiString): UnicodeString; compilerproc;
{
  Converts an AnsiString to a UnicodeString;
}
Var
  Size : SizeInt;
begin
  result:='';
  Size:=Length(S2);
  if Size>0 then
    widestringmanager.Ansi2UnicodeMoveProc(PChar(S2),result,Size);
end;


Function fpc_UnicodeStr_To_WideStr (const S2 : UnicodeString): WideString; compilerproc;
  begin
    SetLength(Result,Length(S2));
    Move(pointer(S2)^,Pointer(Result)^,Length(S2)*sizeof(WideChar));
  end;


Function fpc_WideStr_To_UnicodeStr (Const S2 : WideString): UnicodeString; compilerproc;
  begin
    SetLength(Result,Length(S2));
    Move(pointer(S2)^,Pointer(Result)^,Length(S2)*sizeof(WideChar));
  end;


Function fpc_PUnicodeChar_To_AnsiStr(const p : punicodechar): ansistring; compilerproc;
var
  Size : SizeInt;
begin
  result:='';
  if p=nil then
    exit;
  Size := IndexWord(p^, -1, 0);
  if Size>0 then
    widestringmanager.Unicode2AnsiMoveProc(P,result,Size);
end;


Function fpc_PUnicodeChar_To_UnicodeStr(const p : punicodechar): unicodestring; compilerproc;
var
  Size : SizeInt;
begin
  result:='';
  if p=nil then
    exit;
  Size := IndexWord(p^, -1, 0);
  Setlength(result,Size);
  if Size>0 then
   begin
      Move(p^,PUnicodeChar(Pointer(result))^,Size*sizeof(UnicodeChar));
      { Terminating Zero }
      PUnicodeChar(Pointer(result)+Size*sizeof(UnicodeChar))^:=#0;
   end;
end;


Function fpc_PWideChar_To_UnicodeStr(const p : pwidechar): unicodestring; compilerproc;
var
  Size : SizeInt;
begin
  result:='';
  if p=nil then
    exit;
  Size := IndexWord(p^, -1, 0);
  Setlength(result,Size);
  if Size>0 then
   begin
      Move(p^,PUnicodeChar(Pointer(result))^,Size*sizeof(UnicodeChar));
      { Terminating Zero }
      PUnicodeChar(Pointer(result)+Size*sizeof(UnicodeChar))^:=#0;
   end;
end;


{$ifndef FPC_STRTOSHORTSTRINGPROC}
Function fpc_PUnicodeChar_To_ShortStr(const p : punicodechar): shortstring; compilerproc;
var
  Size : SizeInt;
  temp: ansistring;
begin
  result:='';
  if p=nil then
    exit;
  Size := IndexWord(p^, $7fffffff, 0);
  if Size>0 then
    begin
      widestringmanager.Unicode2AnsiMoveProc(p,temp,Size);
      result:=temp;
    end;
end;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_PUnicodeChar_To_ShortStr(out res : shortstring;const p : punicodechar); compilerproc;
var
  Size : SizeInt;
  temp: ansistring;
begin
  res:='';
  if p=nil then
    exit;
  Size:=IndexWord(p^, high(PtrInt), 0);
  if Size>0 then
    begin
      widestringmanager.Unicode2AnsiMoveProc(p,temp,Size);
      res:=temp;
    end;
end;
{$endif FPC_STRTOSHORTSTRINGPROC}


{ checked against the ansistring routine, 2001-05-27 (FK) }
Procedure fpc_UnicodeStr_Assign (Var S1 : Pointer;S2 : Pointer);[Public,Alias:'FPC_UNICODESTR_ASSIGN']; compilerproc;
{
  Assigns S2 to S1 (S1:=S2), taking in account reference counts.
}
begin
  If S2<>nil then
    If PUnicodeRec(S2-UnicodeFirstOff)^.Ref>0 then
      inclocked(PUnicodeRec(S2-UnicodeFirstOff)^.ref);
  { Decrease the reference count on the old S1 }
  fpc_unicodestr_decr_ref (S1);
  s1:=s2;
end;


{ alias for internal use }
Procedure fpc_UnicodeStr_Assign (Var S1 : Pointer;S2 : Pointer);[external name 'FPC_UNICODESTR_ASSIGN'];

{$ifndef STR_CONCAT_PROCS}

function fpc_UnicodeStr_Concat (const S1,S2 : UnicodeString): UnicodeString; compilerproc;
Var
  Size,Location : SizeInt;
  pc : punicodechar;
begin
  { only assign if s1 or s2 is empty }
  if (S1='') then
    begin
      result:=s2;
      exit;
    end;
  if (S2='') then
    begin
      result:=s1;
      exit;
    end;
  Location:=Length(S1);
  Size:=length(S2);
  SetLength(result,Size+Location);
  pc:=punicodechar(result);
  Move(S1[1],pc^,Location*sizeof(UnicodeChar));
  inc(pc,location);
  Move(S2[1],pc^,(Size+1)*sizeof(UnicodeChar));
end;


function fpc_UnicodeStr_Concat_multi (const sarr:array of Unicodestring): unicodestring; compilerproc;
Var
  i  : Longint;
  p  : pointer;
  pc : punicodechar;
  Size,NewSize : SizeInt;
begin
  { First calculate size of the result so we can do
    a single call to SetLength() }
  NewSize:=0;
  for i:=low(sarr) to high(sarr) do
    inc(Newsize,length(sarr[i]));
  SetLength(result,NewSize);
  pc:=punicodechar(result);
  for i:=low(sarr) to high(sarr) do
    begin
      p:=pointer(sarr[i]);
      if assigned(p) then
        begin
          Size:=length(unicodestring(p));
          Move(punicodechar(p)^,pc^,(Size+1)*sizeof(UnicodeChar));
          inc(pc,size);
        end;
    end;
end;

{$else STR_CONCAT_PROCS}

procedure fpc_UnicodeStr_Concat (var DestS:Unicodestring;const S1,S2 : UnicodeString); compilerproc;
Var
  Size,Location : SizeInt;
  same : boolean;
begin
  { only assign if s1 or s2 is empty }
  if (S1='') then
    begin
      DestS:=s2;
      exit;
    end;
  if (S2='') then
    begin
      DestS:=s1;
      exit;
    end;
  Location:=Length(S1);
  Size:=length(S2);
  { Use Pointer() typecasts to prevent extra conversion code }
  if Pointer(DestS)=Pointer(S1) then
    begin
      same:=Pointer(S1)=Pointer(S2);
      SetLength(DestS,Size+Location);
      if same then
        Move(Pointer(DestS)^,(Pointer(DestS)+Location*sizeof(UnicodeChar))^,(Size)*sizeof(UnicodeChar))
      else
        Move(Pointer(S2)^,(Pointer(DestS)+Location*sizeof(UnicodeChar))^,(Size+1)*sizeof(UnicodeChar));
    end
  else if Pointer(DestS)=Pointer(S2) then
    begin
      SetLength(DestS,Size+Location);
      Move(Pointer(DestS)^,(Pointer(DestS)+Location*sizeof(UnicodeChar))^,(Size+1)*sizeof(UnicodeChar));
      Move(Pointer(S1)^,Pointer(DestS)^,Location*sizeof(UnicodeChar));
    end
  else
    begin
      DestS:='';
      SetLength(DestS,Size+Location);
      Move(Pointer(S1)^,Pointer(DestS)^,Location*sizeof(UnicodeChar));
      Move(Pointer(S2)^,(Pointer(DestS)+Location*sizeof(UnicodeChar))^,(Size+1)*sizeof(UnicodeChar));
    end;
end;


procedure fpc_UnicodeStr_Concat_multi (var DestS:Unicodestring;const sarr:array of Unicodestring); compilerproc;
Var
  i           : Longint;
  p,pc        : pointer;
  Size,NewLen : SizeInt;
  lowstart    : longint;
  destcopy    : pointer;
  OldDestLen  : SizeInt;
begin
  if high(sarr)=0 then
    begin
      DestS:='';
      exit;
    end;
  destcopy:=nil;
  lowstart:=low(sarr);
  if Pointer(DestS)=Pointer(sarr[lowstart]) then
    inc(lowstart);
  { Check for another reuse, then we can't use
    the append optimization }
  for i:=lowstart to high(sarr) do
    begin
      if Pointer(DestS)=Pointer(sarr[i]) then
        begin
          { if DestS is used somewhere in the middle of the expression,
            we need to make sure the original string still exists after
            we empty/modify DestS.
            This trick only works with reference counted strings. Therefor
            this optimization is disabled for WINLIKEUNICODESTRING }
          destcopy:=pointer(dests);
          fpc_UnicodeStr_Incr_Ref(destcopy);
          lowstart:=low(sarr);
          break;
        end;
    end;
  { Start with empty DestS if we start with concatting
    the first array element }
  if lowstart=low(sarr) then
    DestS:='';
  OldDestLen:=length(DestS);
  { Calculate size of the result so we can do
    a single call to SetLength() }
  NewLen:=0;
  for i:=low(sarr) to high(sarr) do
    inc(NewLen,length(sarr[i]));
  SetLength(DestS,NewLen);
  { Concat all strings, except the string we already
    copied in DestS }
  pc:=Pointer(DestS)+OldDestLen*sizeof(UnicodeChar);
  for i:=lowstart to high(sarr) do
    begin
      p:=pointer(sarr[i]);
      if assigned(p) then
        begin
          Size:=length(unicodestring(p));
          Move(p^,pc^,(Size+1)*sizeof(UnicodeChar));
          inc(pc,size*sizeof(UnicodeChar));
        end;
    end;
  fpc_UnicodeStr_Decr_Ref(destcopy);
end;

{$endif STR_CONCAT_PROCS}

Function fpc_Char_To_UChar(const c : Char): UnicodeChar; compilerproc;
var
  w: unicodestring;
begin
  widestringmanager.Ansi2UnicodeMoveProc(@c, w, 1);
  fpc_Char_To_UChar:= w[1];
end;



Function fpc_Char_To_UnicodeStr(const c : Char): UnicodeString; compilerproc;
{
  Converts a Char to a UnicodeString;
}
begin
  Setlength(fpc_Char_To_UnicodeStr,1);
  fpc_Char_To_UnicodeStr[1]:=c;
  { Terminating Zero }
  PUnicodeChar(Pointer(fpc_Char_To_UnicodeStr)+sizeof(UnicodeChar))^:=#0;
end;


Function fpc_UChar_To_Char(const c : UnicodeChar): Char; compilerproc;
{
  Converts a UnicodeChar to a Char;
}
var
  s: ansistring;
begin
  widestringmanager.Unicode2AnsiMoveProc(@c, s, 1);
  if length(s)=1 then
    fpc_UChar_To_Char:= s[1]
  else
    fpc_UChar_To_Char:='?';
end;


Function fpc_WChar_To_UnicodeStr(const c : WideChar): UnicodeString; compilerproc;
{
  Converts a WideChar to a UnicodeString;
}
begin
  Setlength (Result,1);
  Result[1]:= c;
end;


Function fpc_UChar_To_UnicodeStr(const c : UnicodeChar): UnicodeString; compilerproc;
{
  Converts a UnicodeChar to a UnicodeString;
}
begin
  Setlength (fpc_UChar_To_UnicodeStr,1);
  fpc_UChar_To_UnicodeStr[1]:= c;
end;


Function fpc_UChar_To_AnsiStr(const c : UnicodeChar): AnsiString; compilerproc;
{
  Converts a UnicodeChar to a AnsiString;
}
begin
  widestringmanager.Unicode2AnsiMoveProc(@c, fpc_UChar_To_AnsiStr, 1);
end;


{$ifndef FPC_STRTOSHORTSTRINGPROC}
Function fpc_UChar_To_ShortStr(const c : UnicodeChar): ShortString; compilerproc;
{
  Converts a UnicodeChar to a ShortString;
}
var
  s: ansistring;
begin
  widestringmanager.Unicode2AnsiMoveProc(@c, s, 1);
  fpc_UChar_To_ShortStr:= s;
end;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_UChar_To_ShortStr(out res : shortstring;const c : UnicodeChar) compilerproc;
{
  Converts a UnicodeChar to a ShortString;
}
var
  s: ansistring;
begin
  widestringmanager.Unicode2AnsiMoveProc(@c,s,1);
  res:=s;
end;
{$endif FPC_STRTOSHORTSTRINGPROC}


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


Function fpc_CharArray_To_UnicodeStr(const arr: array of char; zerobased: boolean = true): UnicodeString; compilerproc;
var
  i  : SizeInt;
begin
  if (zerobased) then
    begin
      if (arr[0]=#0) Then
      begin
        fpc_chararray_to_unicodestr := '';
        exit;
      end;
      i:=IndexChar(arr,high(arr)+1,#0);
      if i = -1 then
        i := high(arr)+1;
    end
  else
    i := high(arr)+1;
  SetLength(fpc_CharArray_To_UnicodeStr,i);
  widestringmanager.Ansi2UnicodeMoveProc (pchar(@arr),fpc_CharArray_To_UnicodeStr,i);
end;


{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_UnicodeCharArray_To_ShortStr(const arr: array of unicodechar; zerobased: boolean = true): shortstring;[public,alias:'FPC_UNICODECHARARRAY_TO_SHORTSTR']; compilerproc;
var
  l: longint;
 index: longint;
 len: byte;
 temp: ansistring;
begin
  l := high(arr)+1;
  if l>=256 then
    l:=255
  else if l<0 then
    l:=0;
  if zerobased then
    begin
      index:=IndexWord(arr[0],l,0);
      if (index < 0) then
        len := l
      else
        len := index;
    end
  else
    len := l;
  widestringmanager.Unicode2AnsiMoveProc (punicodechar(@arr),temp,len);
  fpc_UnicodeCharArray_To_ShortStr := temp;
end;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_UnicodeCharArray_To_ShortStr(out res : shortstring;const arr: array of unicodechar; zerobased: boolean = true);[public,alias:'FPC_UNICODECHARARRAY_TO_SHORTSTR']; compilerproc;
var
  l: longint;
  index: ptrint;
  len: byte;
  temp: ansistring;
begin
  l := high(arr)+1;
  if l>=high(res)+1 then
    l:=high(res)
  else if l<0 then
    l:=0;
  if zerobased then
    begin
      index:=IndexWord(arr[0],l,0);
      if index<0 then
        len:=l
      else
        len:=index;
    end
  else
    len:=l;
  widestringmanager.Unicode2AnsiMoveProc (punicodechar(@arr),temp,len);
  res:=temp;
end;
{$endif FPC_STRTOSHORTSTRINGPROC}

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

Function fpc_UnicodeCharArray_To_UnicodeStr(const arr: array of unicodechar; zerobased: boolean = true): UnicodeString; compilerproc;
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
  SetLength(fpc_UnicodeCharArray_To_UnicodeStr,i);
  Move(arr[0], Pointer(fpc_UnicodeCharArray_To_UnicodeStr)^,i*sizeof(UnicodeChar));
end;


Function fpc_WideCharArray_To_UnicodeStr(const arr: array of widechar; zerobased: boolean = true): UnicodeString; compilerproc;
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
  SetLength(fpc_WideCharArray_To_UnicodeStr,i);
  Move(arr[0], Pointer(fpc_WideCharArray_To_UnicodeStr)^,i*sizeof(WideChar));
end;

{$ifndef FPC_STRTOCHARARRAYPROC}

{ inside the compiler, the resulttype is modified to that of the actual }
{ chararray we're converting to (JM)                                    }
function fpc_unicodestr_to_chararray(arraysize: SizeInt; const src: UnicodeString): fpc_big_chararray;[public,alias: 'FPC_UNICODESTR_TO_CHARARRAY']; compilerproc;
var
  len: SizeInt;
  temp: ansistring;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    widestringmanager.unicode2ansimoveproc(punicodechar(@src[1]),temp,len);
  len := length(temp);
  if len > arraysize then
    len := arraysize;
{$r-}
  move(temp[1],fpc_unicodestr_to_chararray[0],len);
  fillchar(fpc_unicodestr_to_chararray[len],arraysize-len,0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;


{ inside the compiler, the resulttype is modified to that of the actual }
{ unicodechararray we're converting to (JM)                                }
function fpc_unicodestr_to_unicodechararray(arraysize: SizeInt; const src: UnicodeString): fpc_big_unicodechararray;[public,alias: 'FPC_UNICODESTR_TO_UNICODECHARARRAY']; compilerproc;
var
  len: SizeInt;
begin
  len := length(src);
  if len > arraysize then
    len := arraysize;
{$r-}
  { make sure we don't try to access element 1 of the ansistring if it's nil }
  if len > 0 then
    move(src[1],fpc_unicodestr_to_unicodechararray[0],len*SizeOf(UnicodeChar));
  fillchar(fpc_unicodestr_to_unicodechararray[len],(arraysize-len)*SizeOf(UnicodeChar),0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;


{ inside the compiler, the resulttype is modified to that of the actual }
{ chararray we're converting to (JM)                                    }
function fpc_ansistr_to_unicodechararray(arraysize: SizeInt; const src: AnsiString): fpc_big_unicodechararray;[public,alias: 'FPC_ANSISTR_TO_UNICODECHARARRAY']; compilerproc;
var
  len: SizeInt;
  temp: unicodestring;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    widestringmanager.ansi2unicodemoveproc(pchar(@src[1]),temp,len);
  len := length(temp);
  if len > arraysize then
    len := arraysize;

{$r-}
  move(temp[1],fpc_ansistr_to_unicodechararray[0],len*sizeof(unicodechar));
  fillchar(fpc_ansistr_to_unicodechararray[len],(arraysize-len)*SizeOf(UnicodeChar),0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;

function fpc_shortstr_to_unicodechararray(arraysize: SizeInt; const src: ShortString): fpc_big_unicodechararray;[public,alias: 'FPC_SHORTSTR_TO_UNICODECHARARRAY']; compilerproc;
var
  len: longint;
  temp : unicodestring;
begin
  len := length(src);
  { make sure we don't access char 1 if length is 0 (JM) }
  if len > 0 then
    widestringmanager.ansi2unicodemoveproc(pchar(@src[1]),temp,len);
  len := length(temp);
  if len > arraysize then
    len := arraysize;
{$r-}
  move(temp[1],fpc_shortstr_to_unicodechararray[0],len*sizeof(unicodechar));
  fillchar(fpc_shortstr_to_unicodechararray[len],(arraysize-len)*SizeOf(UnicodeChar),0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;

{$else ndef FPC_STRTOCHARARRAYPROC}

procedure fpc_unicodestr_to_chararray(out res: array of char; const src: UnicodeString); compilerproc;
var
  len: SizeInt;
  temp: ansistring;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    widestringmanager.unicode2ansimoveproc(punicodechar(@src[1]),temp,len);
  len := length(temp);
  if len > length(res) then
    len := length(res);
{$r-}
  move(temp[1],res[0],len);
  fillchar(res[len],length(res)-len,0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;


procedure fpc_unicodestr_to_unicodechararray(out res: array of unicodechar; const src: UnicodeString); compilerproc;
var
  len: SizeInt;
begin
  len := length(src);
  if len > length(res) then
    len := length(res);
{$r-}
  { make sure we don't try to access element 1 of the ansistring if it's nil }
  if len > 0 then
    move(src[1],res[0],len*SizeOf(UnicodeChar));
  fillchar(res[len],(length(res)-len)*SizeOf(UnicodeChar),0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;


procedure fpc_ansistr_to_unicodechararray(out res: array of unicodechar; const src: AnsiString); compilerproc;
var
  len: SizeInt;
  temp: unicodestring;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    widestringmanager.ansi2unicodemoveproc(pchar(@src[1]),temp,len);
  len := length(temp);
  if len > length(res) then
    len := length(res);

{$r-}
  move(temp[1],res[0],len*sizeof(unicodechar));
  fillchar(res[len],(length(res)-len)*SizeOf(UnicodeChar),0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;

procedure fpc_shortstr_to_unicodechararray(out res: array of unicodechar; const src: ShortString); compilerproc;
var
  len: longint;
  temp : unicodestring;
begin
  len := length(src);
  { make sure we don't access char 1 if length is 0 (JM) }
  if len > 0 then
    widestringmanager.ansi2unicodemoveproc(pchar(@src[1]),temp,len);
  len := length(temp);
  if len > length(res) then
    len := length(res);
{$r-}
  move(temp[1],res[0],len*sizeof(unicodechar));
  fillchar(res[len],(length(res)-len)*SizeOf(UnicodeChar),0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;

{$endif ndef FPC_STRTOCHARARRAYPROC}

Function fpc_UnicodeStr_Compare(const S1,S2 : UnicodeString): SizeInt;[Public,Alias : 'FPC_UNICODESTR_COMPARE']; compilerproc;
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
  if pointer(S1)=pointer(S2) then
   begin
     fpc_UnicodeStr_Compare:=0;
     exit;
   end;
  Maxi:=Length(S1);
  temp:=Length(S2);
  If MaxI>Temp then
   MaxI:=Temp;
  Temp:=CompareWord(S1[1],S2[1],MaxI);
  if temp=0 then
   temp:=Length(S1)-Length(S2);
  fpc_UnicodeStr_Compare:=Temp;
end;

Function fpc_UnicodeStr_Compare_Equal(const S1,S2 : UnicodeString): SizeInt;[Public,Alias : 'FPC_UNICODESTR_COMPARE_EQUAL']; compilerproc;
{
  Compares 2 UnicodeStrings for equality only;
  The result is
   0 if S1=S2
   <>0 if S1<>S2
}
Var
  MaxI : SizeInt;
begin
  if pointer(S1)=pointer(S2) then
    exit(0);
  Maxi:=Length(S1);
  If MaxI<>Length(S2) then
    exit(-1)
  else
    exit(CompareWord(S1[1],S2[1],MaxI));
end;

Procedure fpc_UnicodeStr_CheckZero(p : pointer);[Public,Alias : 'FPC_UNICODESTR_CHECKZERO']; compilerproc;
begin
  if p=nil then
    HandleErrorFrame(201,get_frame);
end;


Procedure fpc_UnicodeStr_CheckRange(len,index : SizeInt);[Public,Alias : 'FPC_UNICODESTR_RANGECHECK']; compilerproc;
begin
  if (index>len div 2) or (Index<1) then
    HandleErrorFrame(201,get_frame);
end;

Procedure fpc_UnicodeStr_SetLength(Var S : UnicodeString; l : SizeInt);[Public,Alias : 'FPC_UNICODESTR_SETLENGTH']; compilerproc;
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
Var
  Temp : Pointer;
  movelen: SizeInt;
begin
   if (l>0) then
    begin
      if Pointer(S)=nil then
       begin
         { Need a complete new string...}
         Pointer(s):=NewUnicodeString(l);
       end
      { windows doesn't support reallocing unicodestrings, this code
        is anyways subject to be removed because unicodestrings shouldn't be
        ref. counted anymore (FK) }
      else
        if (PUnicodeRec(Pointer(S)-UnicodeFirstOff)^.Ref = 1) then
        begin
          Dec(Pointer(S),UnicodeFirstOff);
          if SizeUInt(L*sizeof(UnicodeChar)+UnicodeRecLen)>MemSize(Pointer(S)) then
              reallocmem(pointer(S), L*sizeof(UnicodeChar)+UnicodeRecLen);
          Inc(Pointer(S), UnicodeFirstOff);
        end
      else
        begin
          { Reallocation is needed... }
          Temp:=Pointer(NewUnicodeString(L));
          if Length(S)>0 then
            begin
              if l < succ(length(s)) then
                movelen := l
              { also move terminating null }
              else
                movelen := succ(length(s));
              Move(Pointer(S)^,Temp^,movelen * Sizeof(UnicodeChar));
            end;
          fpc_unicodestr_decr_ref(Pointer(S));
          Pointer(S):=Temp;
        end;
      { Force nil termination in case it gets shorter }
      PWord(Pointer(S)+l*sizeof(UnicodeChar))^:=0;
      PUnicodeRec(Pointer(S)-UnicodeFirstOff)^.Len:=l*sizeof(UnicodeChar);
    end
  else
    begin
      { Length=0 }
      if Pointer(S)<>nil then
        fpc_unicodestr_decr_ref (Pointer(S));
      Pointer(S):=Nil;
    end;
end;

{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

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


Function fpc_unicodestr_Unique(Var S : Pointer): Pointer; [Public,Alias : 'FPC_UNICODESTR_UNIQUE']; compilerproc;
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}
Var
  SNew : Pointer;
  L    : SizeInt;
begin
  pointer(result) := pointer(s);
  If Pointer(S)=Nil then
    exit;
  if PUnicodeRec(Pointer(S)-UnicodeFirstOff)^.Ref<>1 then
   begin
     L:=PUnicodeRec(Pointer(S)-UnicodeFirstOff)^.len div sizeof(UnicodeChar);
     SNew:=NewUnicodeString (L);
     Move (PUnicodeChar(S)^,SNew^,(L+1)*sizeof(UnicodeChar));
     PUnicodeRec(SNew-UnicodeFirstOff)^.len:=L * sizeof(UnicodeChar);
     fpc_unicodestr_decr_ref (Pointer(S));  { Thread safe }
     pointer(S):=SNew;
     pointer(result):=SNew;
   end;
end;


Function Fpc_UnicodeStr_Copy (Const S : UnicodeString; Index,Size : SizeInt) : UnicodeString;compilerproc;
var
  ResultAddress : Pointer;
begin
  ResultAddress:=Nil;
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
     If Index<0 Then
      Index:=0;
     ResultAddress:=Pointer(NewUnicodeString (Size));
     if ResultAddress<>Nil then
      begin
        Move (PUnicodeChar(S)[Index],ResultAddress^,Size*sizeof(UnicodeChar));
        PUnicodeRec(ResultAddress-UnicodeFirstOff)^.Len:=Size*sizeof(UnicodeChar);
        PUnicodeChar(ResultAddress+Size*sizeof(UnicodeChar))^:=#0;
      end;
   end;
  fpc_unicodestr_decr_ref(Pointer(fpc_unicodestr_copy));
  Pointer(fpc_unicodestr_Copy):=ResultAddress;
end;


Function Pos (Const Substr : UnicodeString; Const Source : UnicodeString) : SizeInt;
var
  i,MaxLen : SizeInt;
  pc : punicodechar;
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
           (CompareWord(Substr[1],pc^,Length(SubStr))=0) then
         begin
           Pos:=i;
           exit;
         end;
        inc(pc);
      end;
   end;
end;


{ Faster version for a unicodechar alone }
Function Pos (c : UnicodeChar; Const s : UnicodeString) : SizeInt;
var
  i: SizeInt;
  pc : punicodechar;
begin
  pc:=@s[1];
  for i:=1 to length(s) do
   begin
     if pc^=c then
      begin
        pos:=i;
        exit;
      end;
     inc(pc);
   end;
  pos:=0;
end;


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

{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos (c : Char; Const s : UnicodeString) : SizeInt;
var
  i: SizeInt;
  wc : unicodechar;
  pc : punicodechar;
begin
  wc:=c;
  pc:=@s[1];
  for i:=1 to length(s) do
   begin
     if pc^=wc then
      begin
        pos:=i;
        exit;
      end;
     inc(pc);
   end;
  pos:=0;
end;



Procedure Delete (Var S : UnicodeString; Index,Size: SizeInt);
Var
  LS : SizeInt;
begin
  If Length(S)=0 then
   exit;
  if index<=0 then
   exit;
  LS:=PUnicodeRec(Pointer(S)-UnicodeFirstOff)^.Len div sizeof(UnicodeChar);
  if (Index<=LS) and (Size>0) then
   begin
     UniqueString (S);
     if Size+Index>LS then
      Size:=LS-Index+1;
     if Index+Size<=LS then
      begin
        Dec(Index);
        Move(PUnicodeChar(S)[Index+Size],PUnicodeChar(S)[Index],(LS-Index-Size+1)*sizeof(UnicodeChar));
      end;
     Setlength(s,LS-Size);
   end;
end;


Procedure Insert (Const Source : UnicodeString; Var S : UnicodeString; Index : SizeInt);
var
  Temp : UnicodeString;
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
  Pointer(Temp) := NewUnicodeString(Length(Source)+LS);
  SetLength(Temp,Length(Source)+LS);
  If Index>0 then
    move (PUnicodeChar(S)^,PUnicodeChar(Temp)^,Index*sizeof(UnicodeChar));
  Move (PUnicodeChar(Source)^,PUnicodeChar(Temp)[Index],Length(Source)*sizeof(UnicodeChar));
  If (LS-Index)>0 then
    Move(PUnicodeChar(S)[Index],PUnicodeChar(temp)[Length(Source)+index],(LS-Index)*sizeof(UnicodeChar));
  S:=Temp;
end;


Function  UpCase(c:UnicodeChar):UnicodeChar;
var
  s : UnicodeString;
begin
  s:=c;
  result:=widestringmanager.UpperUnicodeStringProc(s)[1];
end;


function UpCase(const s : UnicodeString) : UnicodeString;
begin
  result:=widestringmanager.UpperUnicodeStringProc(s);
end;


Procedure SetString (Out S : UnicodeString; Buf : PUnicodeChar; Len : SizeInt);
var
  BufLen: SizeInt;
begin
  SetLength(S,Len);
  If (Buf<>Nil) and (Len>0) then
    begin
      BufLen := IndexWord(Buf^, Len+1, 0);
      If (BufLen>0) and (BufLen < Len) then
        Len := BufLen;
      Move (Buf[0],S[1],Len*sizeof(UnicodeChar));
      PUnicodeChar(Pointer(S)+Len*sizeof(UnicodeChar))^:=#0;
    end;
end;


Procedure SetString (Out S : UnicodeString; Buf : PChar; Len : SizeInt);
var
  BufLen: SizeInt;
begin
  SetLength(S,Len);
  If (Buf<>Nil) and (Len>0) then
    begin
      BufLen := IndexByte(Buf^, Len+1, 0);
      If (BufLen>0) and (BufLen < Len) then
        Len := BufLen;
      widestringmanager.Ansi2UnicodeMoveProc(Buf,S,Len);
      //PUnicodeChar(Pointer(S)+Len*sizeof(UnicodeChar))^:=#0;
    end;
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
              else
                begin
                    if j+2>=MaxDestBytes then
                      break;
                    Dest[j]:=char($e0 or (w shr 12));
                    Dest[j+1]:=char($80 or ((w shr 6)and $3f));
                    Dest[j+2]:=char($80 or (w and $3f));
                    inc(j,3);
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
              else
                inc(j,3);
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


function Utf8ToUnicode(Dest: PUnicodeChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;

var
  i,j : SizeUInt;
  w: SizeUInt;
  b : byte;
begin
  if not assigned(Source) then
  begin
    result:=0;
    exit;
  end;
  result:=SizeUInt(-1);
  i:=0;
  j:=0;
  if assigned(Dest) then
    begin
      while (j<MaxDestChars) and (i<SourceBytes) do
        begin
          b:=byte(Source[i]);
          w:=b;
          inc(i);
          // 2 or 3 bytes?
          if b>=$80 then
            begin
              w:=b and $3f;
              if i>=SourceBytes then
                exit;
              // 3 bytes?
              if (b and $20)<>0 then
                begin
                  b:=byte(Source[i]);
                  inc(i);
                  if i>=SourceBytes then
                    exit;
                  if (b and $c0)<>$80 then
                    exit;
                  w:=(w shl 6) or (b and $3f);
                end;
              b:=byte(Source[i]);
              w:=(w shl 6) or (b and $3f);
              if (b and $c0)<>$80 then
                exit;
              inc(i);
            end;
          Dest[j]:=UnicodeChar(w);
          inc(j);
        end;
      if j>=MaxDestChars then j:=MaxDestChars-1;
      Dest[j]:=#0;
    end
  else
    begin
      while i<SourceBytes do
        begin
          b:=byte(Source[i]);
          inc(i);
          // 2 or 3 bytes?
          if b>=$80 then
            begin
              if i>=SourceBytes then
                exit;
              // 3 bytes?
              b := b and $3f;
              if (b and $20)<>0 then
                begin
                  b:=byte(Source[i]);
                  inc(i);
                  if i>=SourceBytes then
                    exit;
                  if (b and $c0)<>$80 then
                    exit;
                end;
              if (byte(Source[i]) and $c0)<>$80 then
                exit;
              inc(i);
            end;
          inc(j);
        end;
    end;
  result:=j+1;
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

const
  SNoUnicodestrings = 'This binary has no unicodestrings support compiled in.';
  SRecompileWithUnicodestrings = 'Recompile the application with a unicodestrings-manager in the program uses clause.';

procedure unimplementedunicodestring;
  begin
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
    If IsConsole then
      begin
      Writeln(StdErr,SNoUnicodestrings);
      Writeln(StdErr,SRecompileWithUnicodestrings);
      end;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
    HandleErrorFrame(233,get_frame);
  end;

{$warnings off}
function GenericUnicodeCase(const s : UnicodeString) : UnicodeString;
  begin
    unimplementedunicodestring;
  end;


function CompareUnicodeString(const s1, s2 : UnicodeString) : PtrInt;
  begin
    unimplementedunicodestring;
  end;


function CompareTextUnicodeString(const s1, s2 : UnicodeString): PtrInt;
  begin
    unimplementedunicodestring;
  end;


{$warnings on}

procedure initunicodestringmanager;
  begin
{$ifndef HAS_WIDESTRINGMANAGER}
    widestringmanager.Unicode2AnsiMoveProc:=@defaultUnicode2AnsiMove;
    widestringmanager.Ansi2UnicodeMoveProc:=@defaultAnsi2UnicodeMove;
    widestringmanager.UpperUnicodeStringProc:=@GenericUnicodeCase;
    widestringmanager.LowerUnicodeStringProc:=@GenericUnicodeCase;
{$endif HAS_WIDESTRINGMANAGER}
    widestringmanager.CompareUnicodeStringProc:=@CompareUnicodeString;
    widestringmanager.CompareTextUnicodeStringProc:=@CompareTextUnicodeString;
  end;

