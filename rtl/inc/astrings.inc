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

  @-12 : Longint for maximum size;
  @-8  : Longint for size;
  @-4  : Longint for reference count;
  @    : String + Terminating #0;
  Pchar(Ansistring) is a valid typecast.
  So AS[i] is converted to the address @AS+i-1.

  Constants should be assigned a reference count of -1
  Meaning that they can't be disposed of.
}

Type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = Packed Record
    Maxlen,
    len,
    ref   : Longint;
    First : Char;
  end;

Const
  AnsiRecLen = SizeOf(TAnsiRec);
  FirstOff   = SizeOf(TAnsiRec)-1;


{****************************************************************************
                    Internal functions, not in interface.
****************************************************************************}

{$ifdef AnsiStrDebug}
Procedure DumpAnsiRec(S : Pointer);
begin
  If S=Nil then
    Writeln ('String is nil')
  Else
    Begin
      With PAnsiRec(S-Firstoff)^ do
       begin
         Write   ('(Maxlen: ',maxlen);
         Write   (' Len:',len);
         Writeln (' Ref: ',ref,')');
       end;
    end;
end;
{$endif}


Function NewAnsiString(Len : Longint) : Pointer;
{
  Allocate a new AnsiString on the heap.
  initialize it to zero length and reference count 1.
}
Var
  P : Pointer;
begin
  { Also add +1 for a terminating zero }
  GetMem(P,Len+AnsiRecLen);
  If P<>Nil then
   begin
     PAnsiRec(P)^.Maxlen:=Len;    { Maximal length }
     PAnsiRec(P)^.Len:=0;         { Initial length }
     PAnsiRec(P)^.Ref:=1;         { Set reference count }
     PAnsiRec(P)^.First:=#0;      { Terminating #0 }
     P:=P+FirstOff;               { Points to string now }
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
  Dec (Longint(S),FirstOff);
  FreeMem (S);
  S:=Nil;
end;


Procedure fpc_AnsiStr_Decr_Ref (Var S : Pointer);[Public,Alias:'FPC_ANSISTR_DECR_REF'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Decreases the ReferenceCount of a non constant ansistring;
  If the reference count is zero, deallocate the string;
}
Type
  plongint = ^longint;
Var
  l : plongint;
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
  { this pointer is not valid anymore, so set it to zero }
  S:=nil;
end;

{$ifdef hascompilerproc}
{ also define alias for internal use in the system unit }
Procedure fpc_AnsiStr_Decr_Ref (Var S : Pointer); [external name 'FPC_ANSISTR_DECR_REF'];
{$endif hascompilerproc}

Procedure fpc_AnsiStr_Incr_Ref (Var S : Pointer);[Public,Alias:'FPC_ANSISTR_INCR_REF'];  {$ifdef hascompilerproc} compilerproc; {$endif}
Begin
  If S=Nil then
    exit;
  { Let's be paranoid : Constant string ??}
  If PAnsiRec(S-FirstOff)^.Ref<0 then exit;
  inclocked(PAnsiRec(S-FirstOff)^.Ref);
end;

{$ifdef hascompilerproc}
{ also define alias which can be used inside the system unit }
Procedure fpc_AnsiStr_Incr_Ref (Var S : Pointer); [external name 'FPC_ANSISTR_INCR_REF'];
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

Procedure fpc_AnsiStr_Concat (S1,S2 : Pointer;var S3 : Pointer);[Public, alias: 'FPC_ANSISTR_CONCAT'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Concatenates 2 AnsiStrings : S1+S2.
  Result Goes to S3;
}
Var
  Size,Location : Longint;
begin
{ only assign if s1 or s2 is empty }
  if (S1=Nil) then
    fpc_AnsiStr_Assign(S3,S2)
  else
    if (S2=Nil) then
      fpc_AnsiStr_Assign(S3,S1)
  else
    begin
       { create new result }
       fpc_AnsiStr_Decr_Ref(S3);
       Size:=PAnsiRec(S2-FirstOff)^.Len;
       Location:=Length(AnsiString(S1));
       SetLength (AnsiString(S3),Size+Location);
       Move (S1^,S3^,Location);
       Move (S2^,(S3+location)^,Size+1);
    end;
end;


{$ifdef EXTRAANSISHORT}
Procedure AnsiStr_ShortStr_Concat (Var S1: AnsiString; Var S2 : ShortString);
{
  Concatenates a Ansi with a short string; : S2 + S2
}
Var
  Size,Location : Longint;
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


Procedure fpc_AnsiStr_To_ShortStr (Var S1 : ShortString;S2 : Pointer);[Public, alias: 'FPC_ANSISTR_TO_SHORTSTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a AnsiString to a ShortString;
}
Var
  Size : Longint;
begin
  if S2=nil then
   S1:=''
  else
   begin
     Size:=PAnsiRec(S2-FirstOff)^.Len;
     If Size>high(S1) then
      Size:=high(S1);
     Move (S2^,S1[1],Size);
     byte(S1[0]):=Size;
   end;
end;


Procedure fpc_ShortStr_To_AnsiStr (Var S1 : Pointer; Const S2 : ShortString);[Public, alias: 'FPC_SHORTSTR_TO_ANSISTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a ShortString to a AnsiString;
}
Var
  Size : Longint;
begin
  Size:=Length(S2);
  Setlength (AnsiString(S1),Size);
  if Size>0 then
   begin
     Move (S2[1],Pointer(S1)^,Size);
     { Terminating Zero }
     PByte(Pointer(S1)+Size)^:=0;
   end;
end;


Procedure fpc_Char_To_AnsiStr(var S1 : Pointer; c : Char);[Public, alias: 'FPC_CHAR_TO_ANSISTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a ShortString to a AnsiString;
}
begin
  Setlength (AnsiString(S1),1);
  PByte(Pointer(S1))^:=byte(c);
  { Terminating Zero }
  PByte(Pointer(S1)+1)^:=0;
end;


Procedure fpc_PChar_To_AnsiStr(var a : ansistring;p : pchar);[Public,Alias : 'FPC_PCHAR_TO_ANSISTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  L : Longint;
begin
  if pointer(a)<>nil then
    begin
       fpc_AnsiStr_Decr_Ref(Pointer(a));
       pointer(a):=nil;
    end;
  if (not assigned(p)) or (p[0]=#0) Then
    Pointer(a):=nil
  else
    begin
      l:=IndexChar(p^,-1,#0);
      Pointer(a):=NewAnsistring(L);
      SetLength(A,L);
      Move (P[0],Pointer(A)^,L)
    end;
end;


Procedure fpc_CharArray_To_AnsiStr(var a : ansistring;p : pchar;l:longint);[Public,Alias : 'FPC_CHARARRAY_TO_ANSISTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
var
  i  : longint;
begin
  if p[0]=#0 Then
    Pointer(a):=nil
  else
    begin
      i:=IndexChar(p^,L,#0);
      Pointer(a):=NewAnsistring(i);
      SetLength(a,i);
      Move (P[0],Pointer(A)^,i);
    end;
end;


Function fpc_AnsiStr_Compare(S1,S2 : Pointer): Longint;[Public,Alias : 'FPC_ANSISTR_COMPARE'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Compares 2 AnsiStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  MaxI,Temp : Longint;
begin
  if S1=S2 then
   begin
     fpc_AnsiStr_Compare:=0;
     exit;
   end;
  Maxi:=Length(AnsiString(S1));
  temp:=Length(AnsiString(S2));
  If MaxI>Temp then
   MaxI:=Temp;
  Temp:=CompareByte(S1^,S2^,MaxI);
  if temp=0 then
   temp:=Length(AnsiString(S1))-Length(AnsiString(S2));
  fpc_AnsiStr_Compare:=Temp;
end;


Procedure fpc_AnsiStr_CheckZero(p : pointer);[Public,Alias : 'FPC_ANSISTR_CHECKZERO'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  if p=nil then
    HandleErrorFrame(201,get_frame);
end;


Procedure fpc_AnsiStr_CheckRange(len,index : longint);[Public,Alias : 'FPC_ANSISTR_RANGECHECK'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  if (index>len) or (Index<1) then
    HandleErrorFrame(201,get_frame);
end;

{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : AnsiString; l : Longint);
{$else INTERNSETLENGTH}
Procedure fpc_AnsiStr_SetLength (Var S : AnsiString; l : Longint);[Public,Alias : 'FPC_ANSISTR_SETLENGTH'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif INTERNSETLENGTH}
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
Var
  Temp : Pointer;
  movelen: longint;
begin
   if (l>0) then
    begin
      if Pointer(S)=nil then
       begin
         { Need a complete new string...}
         Pointer(s):=NewAnsiString(l);
       end
      else
       If (PAnsiRec(Pointer(S)-FirstOff)^.Maxlen < L) or
          (PAnsiRec(Pointer(S)-FirstOff)^.Ref <> 1) then
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
Function fpc_AnsiStr_ShortStr_Compare (Var S1 : Pointer; Var S2 : ShortString): Longint;  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Compares a AnsiString with a ShortString;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  i,MaxI,Temp : Longint;
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
Function Length (Const S : AnsiString) : Longint;
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


{ overloaded version of UniqueString for interface }
Procedure UniqueString(Var S : AnsiString); [external name 'FPC_ANSISTR_UNIQUE'];

Procedure fpc_ansistr_Unique(Var S : AnsiString); [Public,Alias : 'FPC_ANSISTR_UNIQUE']; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}
Var
  SNew : Pointer;
  L    : Longint;
begin
  If Pointer(S)=Nil then
    exit;
  if PAnsiRec(Pointer(S)-Firstoff)^.Ref<>1 then
   begin
     L:=PAnsiRec(Pointer(S)-FirstOff)^.len;
     SNew:=NewAnsiString (L);
     Move (Pointer(S)^,SNew^,L+1);
     PAnsiRec(SNew-FirstOff)^.len:=L;
     fpc_ansistr_decr_ref (Pointer(S));  { Thread safe }
     Pointer(S):=SNew;
   end;
end;


Function Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;
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
  Pointer(Copy):=ResultAddress;
end;


Function Pos (Const Substr : AnsiString; Const Source : AnsiString) : Longint;
var
  i,MaxLen : StrLenInt;
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
Function Pos (c : Char; Const s : AnsiString) : Longint;
var
  i: longint;
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


Function fpc_Val_SInt_AnsiStr (DestSize: longint; Const S : AnsiString; Var Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

Function fpc_Val_UInt64_AnsiStr (Const S : AnsiString; Var Code : ValSInt): qword; [public, alias:'FPC_VAL_QWORD_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  fpc_Val_UInt64_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_UInt64_AnsiStr,Code);
    end;
end;


Function fpc_Val_SInt64_AnsiStr (Const S : AnsiString; Var Code : ValSInt): Int64; [public, alias:'FPC_VAL_INT64_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  fpc_Val_SInt64_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := s;
       Val(SS,fpc_Val_SInt64_AnsiStr,Code);
    end;
end;

procedure fpc_AnsiStr_Float(d : ValReal;len,fr,rt : longint;var s : ansistring);[public,alias:'FPC_ANSISTR_FLOAT']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  ss: ShortString;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
end;


Procedure fpc_AnsiStr_UInt(C : Cardinal;Len : Longint; Var S : AnsiString);[Public,Alias : 'FPC_ANSISTR_CARDINAL']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  str(C:Len,SS);
  S:=SS;
end;



Procedure fpc_AnsiStr_SInt(L : Longint; Len : Longint; Var S : AnsiString);[Public,Alias : 'FPC_ANSISTR_LONGINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  str (L:Len,SS);
  S:=SS;
end;


Procedure Delete (Var S : AnsiString; Index,Size: Longint);
Var
  LS : Longint;
begin
  If Length(S)=0 then
   exit;
  if index<=0 then
   begin
     inc(Size,index-1);
     index:=1;
   end;
  LS:=PAnsiRec(Pointer(S)-FirstOff)^.Len;
  if (Index<=LS) and (Size>0) then
   begin
     UniqueString (S);
     if Size+Index>LS then
      Size:=LS-Index+1;
     if Index+Size<=LS then
      begin
        Dec(Index);
        Move(PByte(Pointer(S))[Index+Size],PByte(Pointer(S))[Index],LS-Index+1);
      end;
     Setlength(s,LS-Size);
   end;
end;


Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : Longint);
var
  Temp : AnsiString;
  LS : Longint;
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


Function StringOfChar(c : char;l : longint) : AnsiString;
begin
  SetLength(StringOfChar,l);
  FillChar(Pointer(StringOfChar)^,Length(StringOfChar),c);
end;

Procedure SetString (Var S : AnsiString; Buf : PChar; Len : Longint);

begin
  SetLength(S,Len);
  Move (Buf[0],S[1],Len);
end;


function upcase(const s : ansistring) : ansistring;
var
  i : longint;
begin
  Setlength(result,length(s));
  for i := 1 to length (s) do
    result[i] := upcase(s[i]);
end;


function lowercase(const s : ansistring) : ansistring;
var
  i : longint;
begin
  Setlength(result,length(s));
  for i := 1 to length (s) do
    result[i] := lowercase(s[i]);
end;


{
  $Log$
  Revision 1.17  2001-08-01 15:00:10  jonas
    + "compproc" helpers
    * renamed several helpers so that their name is the same as their
      "public alias", which should facilitate the conversion of processor
      specific code in the code generator to processor independent code
    * some small fixes to the val_ansistring and val_widestring helpers
      (always immediately exit if the source string is longer than 255
       chars)
    * fixed fpc_dynarray_high and fpc_dynarray_length if the dynarray is
      still nil (used to crash, now return resp -1 and 0)

  Revision 1.16  2001/07/10 18:04:37  peter
    * merged textfile, readlink and concat ansistring fixes

  Revision 1.15  2001/07/09 21:15:41  peter
    * Length made internal
    * Add array support for Length

  Revision 1.14  2001/07/09 11:41:57  florian
    * another MT fix

  Revision 1.13  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.12  2001/07/04 12:17:09  jonas
    * removed DestSize parameter from declaration of ValAnsiSignedInt64
      (merged)

  Revision 1.11  2001/05/27 14:28:44  florian
    + made the ref. couting MT safe

  Revision 1.10  2001/04/13 18:06:07  peter
    * upcase, lowercase for ansistring

  Revision 1.9  2000/12/10 15:00:14  florian
    * val for int64 hopefully works now correct

  Revision 1.8  2000/12/08 14:04:43  jonas
    + added pos(char,ansistring), because there is also a pos(char,shortstring)
      and without the ansistring version, the shortstring version is always
      called when calling pos(char,pchar), even when using $h+ (because the
      first parameter matches exactly) (merged)

  Revision 1.7  2000/11/06 20:34:24  peter
    * changed ver1_0 defines to temporary defs

  Revision 1.6  2000/10/21 18:20:17  florian
    * a lot of small changes:
       - setlength is internal
       - win32 graph unit extended
       ....

  Revision 1.5  2000/08/29 18:39:42  peter
    * fixed chararray to ansistring (merged)

  Revision 1.4  2000/08/24 07:37:21  jonas
    * fixed bug in setlength (it sometimes read after the end of the heap)
      and small improvement to ansistring_to_chararray conversion (merged
      from fixes branch)

  Revision 1.3  2000/08/09 19:31:18  marco
   * fixes for val(int64 or qword) to ansistring

  Revision 1.2  2000/07/13 11:33:42  michael
  + removed logs
}
