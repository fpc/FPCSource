{
    $Id$
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

{
  This file contains the implementation of the AnsiString type,
  and all things that are needed for it.
  AnsiString is defined as a 'silent' pchar :
  a pchar that points to :

  @-8  : SizeInt for reference count;
  @-4  : SizeInt for size;
  @    : String + Terminating #0;
  Pchar(Ansistring) is a valid typecast.
  So AS[i] is converted to the address @AS+i-1.

  Constants should be assigned a reference count of -1
  Meaning that they can't be disposed of.
}

Type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = Packed Record
    Ref,
    Len   : SizeInt;
    First : Char;
  end;

Const
  AnsiRecLen = SizeOf(TAnsiRec);
  FirstOff   = SizeOf(TAnsiRec)-1;


{****************************************************************************
                    Internal functions, not in interface.
****************************************************************************}



Function NewAnsiString(Len : SizeInt) : Pointer;
{
  Allocate a new AnsiString on the heap.
  initialize it to zero length and reference count 1.
}
Var
  P : Pointer;
begin
  { request a multiple of 16 because the heap manager alloctes anyways chunks of 16 bytes }
  GetMem(P,Len+AnsiRecLen);
  If P<>Nil then
   begin
     PAnsiRec(P)^.Ref:=1;         { Set reference count }
     PAnsiRec(P)^.Len:=0;         { Initial length }
     PAnsiRec(P)^.First:=#0;      { Terminating #0 }
     inc(p,firstoff);             { Points to string now }
   end;
  NewAnsiString:=P;
end;


Procedure DisposeAnsiString(Var S : Pointer);
{
  Deallocates a AnsiString From the heap.
}
begin
  If S=Nil then
    exit;
  Dec (S,FirstOff);
  FreeMem (S);
  S:=Nil;
end;


Procedure fpc_AnsiStr_Decr_Ref (Var S : Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif}[Public,Alias:'FPC_ANSISTR_DECR_REF'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Decreases the ReferenceCount of a non constant ansistring;
  If the reference count is zero, deallocate the string;
}
Type
  pSizeInt = ^SizeInt;
Var
  l : pSizeInt;
Begin
  { Zero string }
  If S=Nil then exit;
  { check for constant strings ...}
  l:=@PANSIREC(S-FirstOff)^.Ref;
  If l^<0 then exit;

  { declocked does a MT safe dec and returns true, if the counter is 0 }
  If declocked(l^) then
    { Ref count dropped to zero }
    DisposeAnsiString (S);        { Remove...}
{$ifndef decrrefnotnil}
  s:=nil;
{$endif}
end;

{$ifdef hascompilerproc}
{ also define alias for internal use in the system unit }
Procedure fpc_AnsiStr_Decr_Ref (Var S : Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif} [external name 'FPC_ANSISTR_DECR_REF'];
{$endif hascompilerproc}

{$ifdef hascompilerproc}
Procedure fpc_AnsiStr_Incr_Ref (S : Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif}[Public,Alias:'FPC_ANSISTR_INCR_REF'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{$else}
Procedure fpc_AnsiStr_Incr_Ref (Var S : Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif}[Public,Alias:'FPC_ANSISTR_INCR_REF'];
{$endif}
Begin
  If S=Nil then
    exit;
  { Let's be paranoid : Constant string ??}
  If PAnsiRec(S-FirstOff)^.Ref<0 then exit;
  inclocked(PAnsiRec(S-FirstOff)^.Ref);
end;

{$ifdef hascompilerproc}
{ also define alias which can be used inside the system unit }
Procedure fpc_AnsiStr_Incr_Ref (S : Pointer);{$ifndef NOSAVEREGISTERS}saveregisters;{$endif}[external name 'FPC_ANSISTR_INCR_REF'];
{$endif hascompilerproc}

Procedure fpc_AnsiStr_Assign (Var S1 : Pointer;S2 : Pointer);[Public,Alias:'FPC_ANSISTR_ASSIGN'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Assigns S2 to S1 (S1:=S2), taking in account reference counts.
}
begin
  If S2<>nil then
    If PAnsiRec(S2-FirstOff)^.Ref>0 then
      inclocked(PAnsiRec(S2-FirstOff)^.ref);
  { Decrease the reference count on the old S1 }
  fpc_ansistr_decr_ref (S1);
  { And finally, have S1 pointing to S2 (or its copy) }
  S1:=S2;
end;

{$ifdef hascompilerproc}
{ alias for internal use }
Procedure fpc_AnsiStr_Assign (Var S1 : Pointer;S2 : Pointer);[external name 'FPC_ANSISTR_ASSIGN'];
{$endif hascompilerproc}

{$ifdef hascompilerproc}
function fpc_AnsiStr_Concat (const S1,S2 : AnsiString): ansistring; compilerproc;
var
  S3: ansistring absolute result;
{$else hascompilerproc}
Procedure fpc_AnsiStr_Concat (const S1,S2 : ansistring;var S3 : ansistring);[Public, alias: 'FPC_ANSISTR_CONCAT'];
{$endif hascompilerproc}
{
  Concatenates 2 AnsiStrings : S1+S2.
  Result Goes to S3;
}
Var
  Size,Location : SizeInt;
begin
  { only assign if s1 or s2 is empty }
  if (S1='') then
    s3 := s2
  else if (S2='') then
    s3 := s1
  else
    begin
       Size:=length(S2);
       Location:=Length(S1);
       SetLength (S3,Size+Location);
       { the cast to a pointer avoids the unique call }
       { and we don't need an unique call             }
       { because of the SetLength S3 is unique        }
       Move (S1[1],pointer(S3)^,Location);
       Move (S2[1],pointer(pointer(S3)+location)^,Size+1);
    end;
end;


{$ifdef EXTRAANSISHORT}
Procedure AnsiStr_ShortStr_Concat (Var S1: AnsiString; Var S2 : ShortString);
{
  Concatenates a Ansi with a short string; : S2 + S2
}
Var
  Size,Location : SizeInt;
begin
  Size:=Length(S2);
  Location:=Length(S1);
  If Size=0 then
    exit;
  { Setlength takes case of uniqueness
    and alllocated memory. We need to use length,
    to take into account possibility of S1=Nil }
  SetLength (S1,Size+Length(S1));
  Move (S2[1],Pointer(Pointer(S1)+Location)^,Size);
  PByte( Pointer(S1)+length(S1) )^:=0; { Terminating Zero }
end;
{$endif EXTRAANSISHORT}


{ the following declaration has exactly the same effect as                   }
{ procedure fpc_AnsiStr_To_ShortStr (Var S1 : ShortString;S2 : Pointer);     }
{ which is what the old helper was, so we don't need an extra implementation }
{ of the old helper (JM)                                                     }
function fpc_AnsiStr_To_ShortStr (high_of_res: SizeInt;const S2 : Ansistring): shortstring;[Public, alias: 'FPC_ANSISTR_TO_SHORTSTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
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


Function fpc_ShortStr_To_AnsiStr (Const S2 : ShortString): ansistring; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a ShortString to a AnsiString;
}
Var
  Size : SizeInt;
begin
  Size:=Length(S2);
  Setlength (fpc_ShortStr_To_AnsiStr,Size);
  if Size>0 then
    Move(S2[1],Pointer(fpc_ShortStr_To_AnsiStr)^,Size);
end;

{ old style helper }
{$ifndef hascompilerproc}
Procedure fpc_ShortStr_To_AnsiStr (Var S1 : Pointer; Const S2 : ShortString);[Public, alias: 'FPC_SHORTSTR_TO_ANSISTR'];
Var
  Size : SizeInt;
begin
  Size:=Length(S2);
  Setlength (ansistring(s1),Size);
  if Size>0 then
    Move(S2[1],s1^,Size);
end;
{$endif hascompilerproc}

Function fpc_Char_To_AnsiStr(const c : Char): AnsiString; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a Char to a AnsiString;
}
begin
  Setlength (fpc_Char_To_AnsiStr,1);
  PByte(Pointer(fpc_Char_To_AnsiStr))^:=byte(c);
  { Terminating Zero }
  PByte(Pointer(fpc_Char_To_AnsiStr)+1)^:=0;
end;

{ old style helper }
{$ifndef hascompilerproc}
Procedure fpc_Char_To_AnsiStr(var S1 : Pointer; c : Char);[Public, alias: 'FPC_CHAR_TO_ANSISTR'];
begin
  s1 := pointer(fpc_Char_To_AnsiStr(c));
end;
{$endif hascompilerproc}


Function fpc_PChar_To_AnsiStr(const p : pchar): ansistring; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  L : SizeInt;
begin
  if (not assigned(p)) or (p[0]=#0) Then
    { result is automatically set to '' }
    exit;
  l:=IndexChar(p^,-1,#0);
  SetLength(fpc_PChar_To_AnsiStr,L);
  Move (P[0],Pointer(fpc_PChar_To_AnsiStr)^,L)
end;

{ old style helper }
{$ifndef hascompilerproc}
Procedure fpc_PChar_To_AnsiStr(var a : ansistring;p : pchar);[Public,Alias : 'FPC_PCHAR_TO_ANSISTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  pointer(a) := pointer(fpc_PChar_To_AnsiStr(p));
end;
{$endif hascompilerproc}


Function fpc_CharArray_To_AnsiStr(const arr: array of char): ansistring; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  i  : SizeInt;
begin
  if arr[0]=#0 Then
    { result is automatically set to '' }
    exit;
  i:=IndexChar(arr,high(arr)+1,#0);
  if i = -1 then
    i := high(arr)+1;
  SetLength(fpc_CharArray_To_AnsiStr,i);
  Move (arr[0],Pointer(fpc_CharArray_To_AnsiStr)^,i);
end;

{ old style helper }
{$ifndef hascompilerproc}
{ the declaration below is the same as                                              }
{ which is what the old helper was (we need the parameter as "array of char" type   }
{ so we can pass it to the new style helper (JM)                                    }
Procedure fpc_CharArray_To_AnsiStr(var a : ansistring; p: pointer; len: SizeInt);[Public,Alias : 'FPC_CHARARRAY_TO_ANSISTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
var
  src: pchar;
  i: SizeInt;
begin
  src := pchar(p);
  if src[0]=#0 Then
    { result is automatically set to '' }
    begin
      pointer(a) := nil;
      exit;
    end;
  i:=IndexChar(src^,len,#0);
  if i = -1 then
    i := len;
  pointer(a) := NewAnsiString(i);
  Move (src^,a[1],i);
end;
{$endif not hascompilerproc}


{$ifdef hascompilerproc}

{ note: inside the compiler, the resulttype is modified to be the length }
{ of the actual chararray to which we convert (JM)                       }
function fpc_ansistr_to_chararray(arraysize: SizeInt; const src: ansistring): fpc_big_chararray; [public, alias: 'FPC_ANSISTR_TO_CHARARRAY']; compilerproc;
var
  len: SizeInt;
begin
  len := length(src);
  if len > arraysize then
    len := arraysize;
  { make sure we don't try to access element 1 of the ansistring if it's nil }
  if len > 0 then
    move(src[1],fpc_ansistr_to_chararray[0],len);
  fillchar(fpc_ansistr_to_chararray[len],arraysize-len,0);
end;

{$endif hascompilerproc}


Function fpc_AnsiStr_Compare(const S1,S2 : AnsiString): SizeInt;[Public,Alias : 'FPC_ANSISTR_COMPARE'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Compares 2 AnsiStrings;
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
     fpc_AnsiStr_Compare:=0;
     exit;
   end;
  Maxi:=Length(S1);
  temp:=Length(S2);
  If MaxI>Temp then
   MaxI:=Temp;
  Temp:=CompareByte(S1[1],S2[1],MaxI);
  if temp=0 then
   temp:=Length(S1)-Length(S2);
  fpc_AnsiStr_Compare:=Temp;
end;


Procedure fpc_AnsiStr_CheckZero(p : pointer);[Public,Alias : 'FPC_ANSISTR_CHECKZERO'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  if p=nil then
    HandleErrorFrame(201,get_frame);
end;


Procedure fpc_AnsiStr_CheckRange(len,index : SizeInt);[Public,Alias : 'FPC_ANSISTR_RANGECHECK'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  if (index>len) or (Index<1) then
    HandleErrorFrame(201,get_frame);
end;

{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : AnsiString; l : SizeInt);
{$else INTERNSETLENGTH}
Procedure fpc_AnsiStr_SetLength (Var S : AnsiString; l : SizeInt);[Public,Alias : 'FPC_ANSISTR_SETLENGTH'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif INTERNSETLENGTH}
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
Var
  Temp : Pointer;
  movelen : SizeInt;
begin
  if (l>0) then
    begin
      if Pointer(S)=nil then
       begin
         { Need a complete new string...}
         Pointer(s):=NewAnsiString(l);
       end
      else if (PAnsiRec(Pointer(S)-FirstOff)^.Ref = 1) then
        begin
          Dec(Pointer(S),FirstOff);
          if AnsiRecLen+L>MemSize(Pointer(s)) then
            reallocmem(pointer(S),AnsiRecLen+L);
          Inc(Pointer(S),FirstOff);
        end
      else
        begin
          { Reallocation is needed... }
          Temp:=Pointer(NewAnsiString(L));
          if Length(S)>0 then
            begin
              if l < succ(length(s)) then
                movelen := l
              { also move terminating null }
              else movelen := succ(length(s));
              Move(Pointer(S)^,Temp^,movelen);
            end;
          fpc_ansistr_decr_ref(Pointer(S));
          Pointer(S):=Temp;
       end;
      { Force nil termination in case it gets shorter }
      PByte(Pointer(S)+l)^:=0;
      PAnsiRec(Pointer(S)-FirstOff)^.Len:=l;
    end
  else
    begin
      { Length=0 }
      if Pointer(S)<>nil then
       fpc_ansistr_decr_ref (Pointer(S));
      Pointer(S):=Nil;
    end;
end;

{$ifdef EXTRAANSISHORT}
Function fpc_AnsiStr_ShortStr_Compare (Var S1 : Pointer; Var S2 : ShortString): SizeInt;  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Compares a AnsiString with a ShortString;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  i,MaxI,Temp : SizeInt;
begin
  Temp:=0;
  i:=0;
  MaxI:=Length(AnsiString(S1));
  if MaxI>byte(S2[0]) then
    MaxI:=Byte(S2[0]);
  While (i<MaxI) and (Temp=0) do
   begin
     Temp:= PByte(S1+I)^ - Byte(S2[i+1]);
     inc(i);
   end;
  AnsiStr_ShortStr_Compare:=Temp;
end;
{$endif EXTRAANSISHORT}


{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

{$ifndef INTERNLENGTH}
Function Length (Const S : AnsiString) : SizeInt;
{
  Returns the length of an AnsiString.
  Takes in acount that zero strings are NIL;
}
begin
  If Pointer(S)=Nil then
    Length:=0
  else
    Length:=PAnsiRec(Pointer(S)-FirstOff)^.Len;
end;
{$endif INTERNLENGTH}


{$ifdef HASCOMPILERPROC}
{ overloaded version of UniqueString for interface }
Procedure UniqueString(Var S : AnsiString); [external name 'FPC_ANSISTR_UNIQUE'];
Function fpc_ansistr_Unique(Var S : Pointer): Pointer; [Public,Alias : 'FPC_ANSISTR_UNIQUE']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$else}
Procedure UniqueString(Var S : AnsiString); [Public,Alias : 'FPC_ANSISTR_UNIQUE'];
{$endif}
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}
Var
  SNew : Pointer;
  L    : SizeInt;
begin
{$ifdef HASCOMPILERPROC}
  pointer(result) := pointer(s);
{$endif}
  If Pointer(S)=Nil then
    exit;
  if PAnsiRec(Pointer(S)-Firstoff)^.Ref<>1 then
   begin
     L:=PAnsiRec(Pointer(S)-FirstOff)^.len;
     SNew:=NewAnsiString (L);
     Move (Pointer(S)^,SNew^,L+1);
     PAnsiRec(SNew-FirstOff)^.len:=L;
     fpc_ansistr_decr_ref (Pointer(S));  { Thread safe }
     pointer(S):=SNew;
{$ifdef HASCOMPILERPROC}
     pointer(result):=SNew;
{$endif}
   end;
end;

Procedure fpc_ansistr_append_char(Var S : AnsiString;c : char); [Public,Alias : 'FPC_ANSISTR_APPEND_CHAR']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
   SetLength(S,length(S)+1);
   S[length(S)]:=c;
   PByte(Pointer(S)+length(S))^:=0; { Terminating Zero }
end;

Procedure fpc_ansistr_append_shortstring(Var S : AnsiString;Str : ShortString); [Public,Alias : 'FPC_ANSISTR_APPEND_SHORTSTRING']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
   ofs : SizeInt;
begin
   ofs:=Length(S);
   SetLength(S,ofs+length(Str));
   move(Str[1],S[ofs+1],length(Str));
   PByte(Pointer(S)+length(S))^:=0; { Terminating Zero }
end;

Procedure fpc_ansistr_append_ansistring(Var S : AnsiString;Str : AnsiString); [Public,Alias : 'FPC_ANSISTR_APPEND_ANSISTRING']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
   ofs : SizeInt;
begin
   if Str<>'' then
     begin
        ofs:=Length(S);
        SetLength(S,ofs+length(Str));
        move(Str[1],S[ofs+1],length(Str)+1);
     end;
end;

{$ifdef interncopy}
Function Fpc_Ansistr_Copy (Const S : AnsiString; Index,Size : SizeInt) : AnsiString;compilerproc;
{$else}
Function Copy (Const S : AnsiString; Index,Size : SizeInt) : AnsiString;
{$endif}
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
     ResultAddress:=Pointer(NewAnsiString (Size));
     if ResultAddress<>Nil then
      begin
        Move (Pointer(Pointer(S)+index)^,ResultAddress^,Size);
        PAnsiRec(ResultAddress-FirstOff)^.Len:=Size;
        PByte(ResultAddress+Size)^:=0;
      end;
   end;
{$ifdef interncopy}
  Pointer(fpc_ansistr_Copy):=ResultAddress;
{$else}
  Pointer(Copy):=ResultAddress;
{$endif}
end;


Function Pos (Const Substr : AnsiString; Const Source : AnsiString) : SizeInt;
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
           (CompareChar(Substr[1],pc^,Length(SubStr))=0) then
         begin
           Pos:=i;
           exit;
         end;
        inc(pc);
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
  pc : pchar;
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


Function fpc_Val_Real_AnsiStr(Const S : AnsiString; Var Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


Function fpc_Val_UInt_AnsiStr (Const S : AnsiString; Var Code : ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


Function fpc_Val_SInt_AnsiStr (DestSize: SizeInt; Const S : AnsiString; Var Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  fpc_Val_SInt_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       fpc_Val_SInt_AnsiStr := fpc_Val_SInt_ShortStr(DestSize,SS,Code);
    end;
end;


{$ifndef CPU64}

Function fpc_Val_qword_AnsiStr (Const S : AnsiString; Var Code : ValSInt): qword; [public, alias:'FPC_VAL_QWORD_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


Function fpc_Val_int64_AnsiStr (Const S : AnsiString; Var Code : ValSInt): Int64; [public, alias:'FPC_VAL_INT64_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


procedure fpc_AnsiStr_Float(d : ValReal;len,fr,rt : SizeInt;var s : ansistring);[public,alias:'FPC_ANSISTR_FLOAT']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  ss: ShortString;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
end;


{$ifdef STR_USES_VALINT}
Procedure fpc_AnsiStr_UInt(v : ValUInt;Len : SizeInt; Var S : AnsiString);[Public,Alias : 'FPC_ANSISTR_VALUINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$else}
Procedure fpc_AnsiStr_Longword(v : Longword;Len : SizeInt; Var S : AnsiString);[Public,Alias : 'FPC_ANSISTR_LONGWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif}
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
end;



{$ifdef STR_USES_VALINT}
Procedure fpc_AnsiStr_SInt(v : ValSInt;Len : SizeInt; Var S : AnsiString);[Public,Alias : 'FPC_ANSISTR_VALSINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$else}
Procedure fpc_AnsiStr_Longint(v : Longint; Len : SizeInt; Var S : AnsiString);[Public,Alias : 'FPC_ANSISTR_LONGINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif}
Var
  SS : ShortString;
begin
  str (v:Len,SS);
  S:=SS;
end;


{$ifndef CPU64}

Procedure fpc_AnsiStr_QWord(v : QWord;Len : SizeInt; Var S : AnsiString);[Public,Alias : 'FPC_ANSISTR_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
end;



Procedure fpc_AnsiStr_Int64(v : Int64; Len : SizeInt; Var S : AnsiString);[Public,Alias : 'FPC_ANSISTR_INT64']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

Procedure SetString (Var S : AnsiString; Buf : PChar; Len : SizeInt);
begin
  SetLength(S,Len);
  If (Buf<>Nil) then
    begin
      Move (Buf[0],S[1],Len);
    end;
end;


function upcase(const s : ansistring) : ansistring;
var
  i : SizeInt;
begin
  Setlength(result,length(s));
  for i := 1 to length (s) do
    result[i] := upcase(s[i]);
end;


function lowercase(const s : ansistring) : ansistring;
var
  i : SizeInt;
begin
  Setlength(result,length(s));
  for i := 1 to length (s) do
    result[i] := lowercase(s[i]);
end;


{
  $Log$
  Revision 1.49  2004-10-31 16:21:30  peter
    * fix shortstr_to_ansistring for 1.0.x

  Revision 1.48  2004/10/24 20:01:41  peter
    * saveregisters calling convention is obsolete

  Revision 1.47  2004/07/12 17:58:19  peter
    * remove maxlen field from ansistring/widestrings

  Revision 1.46  2004/07/02 21:21:09  peter
    * decr ref doesn't reset pointer
    * finalize resets pointer for astring,wstring

  Revision 1.45  2004/05/29 15:39:08  florian
    * the decr functions set the data now to nil

  Revision 1.44  2004/05/16 16:52:28  peter
    * small fix for 1.0.x

  Revision 1.43  2004/05/01 23:55:18  peter
    * replace strlenint with sizeint

  Revision 1.42  2004/04/29 18:59:43  peter
    * str() helpers now also use valint/valuint
    * int64/qword helpers disabled for cpu64

  Revision 1.41  2004/01/21 22:14:05  peter
    * 1.0.x fix

  Revision 1.40  2004/01/21 22:02:18  peter
    * decrref does not reset always to nil, only when string is disposed.
      the reset to nil for temps is done by the compiler

  Revision 1.39  2003/06/17 19:24:08  jonas
    * fixed conversion of fpc_*str_unique to compilerproc

  Revision 1.38  2003/06/17 16:38:53  jonas
    * fpc_ansistr|widestr_unique is now a function so it can be used as
      compilerproc

  Revision 1.37  2003/05/01 08:05:23  florian
    * started to make the rtl 64 bit save by introducing SizeInt and SizeUInt (similar to size_t of C)

  Revision 1.36  2003/02/26 19:16:55  jonas
    * fixed setstring (+- like suggested by Dimitry Sibiryakov)

  Revision 1.35  2002/12/09 08:33:31  michael
  + Fixed range check error and others in Delete

  Revision 1.34  2002/12/07 14:34:30  carl
    - avoid warnings (add typecast)

  Revision 1.33  2002/10/21 19:52:47  jonas
  Revision 1.1.2.17  2002/12/09 08:32:34  michael
  + Fixed range check error and others in Delete

  Revision 1.1.2.16  2002/10/21 19:30:57  jonas
    * fixed some buffer overflow errors in SetString (both short and
      ansistring versions) (merged)

  Revision 1.32  2002/10/20 12:59:21  jonas
    * fixed ansistring append helpers so they preserve the terminating #0
    * optimized SetLength() so that it uses reallocmem in case the refcount
      of the target string is 1

  Revision 1.31  2002/10/19 17:06:50  michael
  + Added check for nil buffer to setstring

  Revision 1.30  2002/10/17 12:43:00  florian
    + ansistring_append* implemented

  Revision 1.29  2002/10/02 18:21:51  peter
    * Copy() changed to internal function calling compilerprocs
    * FPC_SHORTSTR_COPY renamed to FPC_SHORTSTR_ASSIGN because of the
      new copy functions

  Revision 1.28  2002/09/14 11:20:50  carl
    * Delphi compatibility fix (with string routines)

  Revision 1.27  2002/09/07 21:10:47  carl
    * cardinal -> longword
    - remove some unused routines

  Revision 1.26  2002/09/07 15:07:44  peter
    * old logs removed and tabs fixed

  Revision 1.25  2002/04/26 15:19:05  peter
    * use saveregisters for incr routines, saves also problems with
      the optimizer

  Revision 1.24  2002/04/25 20:14:56  peter
    * updated compilerprocs
    * incr ref count has now a value argument instead of var

  Revision 1.23  2002/01/07 13:23:53  jonas
    * fixed bug in fpc_char_to_ansistr when converting #0 (found by Peter)

}
