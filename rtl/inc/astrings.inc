{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Michael Van Canneyt,
    member of the Free Pascal development team.

    This file implements AnsiStrings for FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

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
  FreeMem (S,PAnsiRec(S)^.Maxlen+AnsiRecLen);
  S:=Nil;
end;


Procedure AnsiStr_Decr_Ref (Var S : Pointer);[Public,Alias:'FPC_ANSISTR_DECR_REF'];
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
  Dec(l^);
  If l^=0 then
    { Ref count dropped to zero }
    DisposeAnsiString (S);        { Remove...}
  { this pointer is not valid anymore, so set it to zero }
  S:=nil;
end;


Procedure AnsiStr_Incr_Ref (Var S : Pointer);[Public,Alias:'FPC_ANSISTR_INCR_REF'];
Begin
  If S=Nil then
    exit;
  { Let's be paranoid : Constant string ??}
  If PAnsiRec(S-FirstOff)^.Ref<0 then exit;
  Inc(PAnsiRec(S-FirstOff)^.Ref);
end;


Procedure AnsiStr_Assign (Var S1 : Pointer;S2 : Pointer);[Public,Alias:'FPC_ANSISTR_ASSIGN'];
{
  Assigns S2 to S1 (S1:=S2), taking in account reference counts.
}
begin
  If S2<>nil then
    If PAnsiRec(S2-FirstOff)^.Ref>0 then
      Inc(PAnsiRec(S2-FirstOff)^.ref);
  { Decrease the reference count on the old S1 }
  ansistr_decr_ref (S1);
  { And finally, have S1 pointing to S2 (or its copy) }
  S1:=S2;
end;


Procedure AnsiStr_Concat (S1,S2 : Pointer;var S3 : Pointer);[Public, alias: 'FPC_ANSISTR_CONCAT'];
{
  Concatenates 2 AnsiStrings : S1+S2.
  Result Goes to S3;
}
Var
  Size,Location : Longint;

begin
  if S3<>nil then
    begin
       AnsiStr_Decr_Ref(S3);
       S3:=nil;
    end;

  if (S1=Nil) then
    AnsiStr_Assign(S3,S2)
  else
    if (S2=Nil) then
      AnsiStr_Assign(S3,S1)
  else
    begin
       Size:=PAnsiRec(S2-FirstOff)^.Len;
       Location:=Length(AnsiString(S1));
       SetLength (AnsiString(S3),Size+Location);
       Move (S1^,S3^,Location);
       Move (S2^,(S3+location)^,Size+1);
    end;
end;


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


Procedure AnsiStr_To_ShortStr (Var S1 : ShortString;S2 : Pointer);[Public, alias: 'FPC_ANSISTR_TO_SHORTSTR'];
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


Procedure ShortStr_To_AnsiStr (Var S1 : Pointer; Const S2 : ShortString);[Public, alias: 'FPC_SHORTSTR_TO_ANSISTR'];
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


Procedure Char_To_AnsiStr(var S1 : Pointer; c : Char);[Public, alias: 'FPC_CHAR_TO_ANSISTR'];
{
  Converts a ShortString to a AnsiString;
}
begin
  Setlength (AnsiString(S1),1);
  PByte(Pointer(S1))^:=byte(c);
  { Terminating Zero }
  PByte(Pointer(S1)+1)^:=0;
end;


Procedure PChar_To_AnsiStr(var a : ansistring;p : pchar);[Public,Alias : 'FPC_PCHAR_TO_ANSISTR'];
Var
  L : Longint;
begin
  if pointer(a)<>nil then
    begin
       AnsiStr_Decr_Ref(Pointer(a));
       pointer(a):=nil;
    end;
  if (not assigned(p)) or (p[0]=#0) Then
    Pointer(a):=nil
  else
    begin
      //!! Horribly inneficient, but I see no other way...
      L:=1;
      While P[l]<>#0 do
        inc (l);
      Pointer(a):=NewAnsistring(L);
      SetLength(A,L);
      Move (P[0],Pointer(A)^,L)
    end;
end;


Procedure CharArray_To_AnsiStr(var a : ansistring;p : pchar;l:longint);[Public,Alias : 'FPC_CHARARRAY_TO_ANSISTR'];
var
  i  : longint;
  hp : pchar;
begin
  if p[0]=#0 Then
    Pointer(a):=nil
  else
    begin
      Pointer(a):=NewAnsistring(L);
      hp:=p;
      i:=0;
      while (i<l) and (hp^<>#0) do
       begin
         inc(hp);
         inc(i);
       end;
      SetLength(A,i);
      Move (P[0],Pointer(A)^,i)
    end;
end;


Function AnsiStr_Compare(S1,S2 : Pointer): Longint;[Public,Alias : 'FPC_ANSISTR_COMPARE'];
{
  Compares 2 AnsiStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  i,MaxI,Temp : Longint;
begin
  i:=0;
  Maxi:=Length(AnsiString(S1));
  temp:=Length(AnsiString(S2));
  If MaxI>Temp then
   MaxI:=Temp;
  Temp:=0;
  While (i<MaxI) and (Temp=0) do
   begin
     Temp:= PByte(S1+I)^ - PByte(S2+i)^;
     inc(i);
   end;
  if temp=0 then
   temp:=Length(AnsiString(S1))-Length(AnsiString(S2));
  AnsiStr_Compare:=Temp;
end;


Procedure AnsiStr_CheckZero(p : pointer);[Public,Alias : 'FPC_ANSISTR_CHECKZERO'];
begin
  if p=nil then
    HandleErrorFrame(201,get_frame);
end;


Procedure AnsiStr_CheckRange(len,index : longint);[Public,Alias : 'FPC_ANSISTR_RANGECHECK'];
begin
  if (index>len) or (Index<1) then
    HandleErrorFrame(201,get_frame);
end;


Function AnsiStr_ShortStr_Compare (Var S1 : Pointer; Var S2 : ShortString): Longint;
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


{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

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


Procedure SetLength (Var S : AnsiString; l : Longint);
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
Var
  Temp : Pointer;
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
            Move(Pointer(S)^,Temp^,L);
          ansistr_decr_ref(Pointer(S));
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
       ansistr_decr_ref (Pointer(S));
      Pointer(S):=Nil;
    end;
end;


Procedure UniqueAnsiString(Var S : AnsiString); [Public,Alias : 'FPC_ANSISTR_UNIQUE'];
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}
Var
  SNew : Pointer;
begin
  If Pointer(S)=Nil then
    exit;
  if PAnsiRec(Pointer(S)-Firstoff)^.Ref<>1 then
    begin
    SNew:=NewAnsiString (PAnsiRec(Pointer(S)-FirstOff)^.len);
    Move (Pointer(S)^,SNew^,PAnsiRec(Pointer(S)-FirstOff)^.len+1);
    PAnsiRec(SNew-FirstOff)^.len:=PAnsiRec(Pointer(S)-FirstOff)^.len;
    ansistr_decr_ref (Pointer(S));  { Thread safe }
    Pointer(S):=SNew;
    end;
end;


Function Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;
var
  ResultAddress : Pointer;
begin
  ResultAddress:=Nil;
  dec(index);
  { Check Size. Accounts for Zero-length S }
  if Length(S)<Index+Size then
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
  i,j : longint;
  e : boolean;
  S : AnsiString;
  se : Pointer;
begin
  i := 0;
  j := 0;
  e := true;
  if Length(SubStr)=0 then e := false;
  while (e) and (i <= length (Source) - length (substr)) do
   begin
     inc (i);
     S:=copy(Source,i,length(Substr));
     Se:=pointer(SubStr);
     if AnsiStr_Compare(se,Pointer(S))=0 then
      begin
        j := i;
        e := false;
      end;
   end;
  pos := j;
end;


{$IfDef ValInternCompiled}

Function ValAnsiFloat(Const S : AnsiString; Var Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_ANSISTR'];
Var
  SS : String;
begin
  AnsiStr_To_ShortStr(SS,Pointer(S));
  ValAnsiFloat := ValFloat(SS,Code);
end;


Function ValAnsiUnsigendInt (Const S : AnsiString; Code : ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_ANSISTR'];
Var
  SS : ShortString;
begin
  AnsiStr_To_ShortStr(SS,Pointer(S));
  ValAnsiUnsigendInt := ValUnsignedInt(SS,Code);
end;


Function ValAnsiSignedInt (DestSize: longint; Const S : AnsiString; Var Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_ANSISTR'];
Var
  SS : ShortString;
begin
  AnsiStr_To_ShortStr (SS,Pointer(S));
  ValAnsiSignedInt := ValSignedInt(DestSize,SS,Code);
end;


{$IfDef SUPPORT_FIXED}
Function ValAnsiFixed(Const S : AnsiString; Var Code : ValSint): ValReal; [public, alias:'FPC_VAL_FIXED_ANSISTR'];
Var
  SS : String;
begin
  AnsiStr_To_ShortStr (SS,Pointer(S));
  ValAnsiFixed := Fixed(ValFloat(SS,Code));
end;
{$EndIf SUPPORT_FIXED}


{$Else ValInternCompiled}

Procedure Val (Const S : AnsiString; var R : real; Var Code : Integer);
Var
  SS : String;
begin
 AnsiStr_To_ShortStr (SS,Pointer(S));
 Val(SS,R,Code);
end;


{
Procedure Val (var S : AnsiString; var D : Double; Var Code : Integer);

Var SS : ShortString;

begin
 AnsiStr_To_ShortStr (SS,S);
 Val(SS,D,Code);
end;
}


Procedure Val (Const S : AnsiString; var E : Extended; Code : Integer);

Var SS : ShortString;

begin
 AnsiStr_To_ShortStr (SS,Pointer(S));
 Val(SS,E,Code);
end;



Procedure Val (Const S : AnsiString; var C : Cardinal; Code : Integer);

Var SS : ShortString;

begin
 AnsiStr_To_ShortStr (SS,Pointer(S));
 Val(SS,C,Code);
end;



Procedure Val (Const S : AnsiString; var L : Longint; Var Code : Integer);

Var SS : ShortString;

begin
 AnsiStr_To_ShortStr (SS,Pointer(S));
 Val(SS,L,Code);
end;



Procedure Val (Const S : AnsiString; var W : Word; Var Code : Integer);

Var SS : ShortString;

begin
 AnsiStr_To_ShortStr (SS,Pointer(S));
 Val(SS,W,Code);
end;



Procedure Val (Const S : AnsiString; var I : Integer; Var Code : Integer);

Var SS : ShortString;

begin
 AnsiStr_To_ShortStr (SS,Pointer(S));
 Val(SS,I,Code);
end;



Procedure Val (Const S : AnsiString; var B : Byte; Var Code : Integer);

Var SS : ShortString;

begin
 AnsiStr_To_ShortStr (SS,Pointer(S));
 Val(SS,B,Code);
end;



Procedure Val (Const S : AnsiString; var SI : ShortInt; Var Code : Integer);

Var SS : ShortString;

begin
 AnsiStr_To_ShortStr (SS,Pointer(S));
 Val(SS,SI,Code);
end;

{$EndIf ValInternCompiled}



{$ifdef INTERNDOUBLE}

procedure AnsiStr_Float(d : ValReal;len,fr,rt : longint;var s : ansistring);[public,alias:'FPC_ANSISTR_FLOAT'];
var
  ss : shortstring;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
end;

{$else INTERNDOUBLE}


Procedure ACoStr (Co : Comp;Len,fr: Longint; Var S : AnsiString);
  [Public,Alias : 'FPC_'+{$ifdef NOSTRANSI}'ANSISTR'{$else}'STRANSI_'{$endif}+'COMP'];

Var SS : ShortString;

begin
 ShortStr_comp (Co,Len,fr,SS);
 S:=SS;
end;


Procedure ASiStr (Si : Single;Len,fr: Longint; Var S : AnsiString);
  [Public,Alias : 'FPC_'+{$ifdef NOSTRANSI}'ANSISTR'{$else}'STRANSI'{$endif}+'_SINGLE'];

Var SS : ShortString;

begin
 ShortStr_Single (Si,Len,fr,SS);
 S:=SS;
end;

{$IfDef Support_Fixed}
Procedure AFiStr (fi : Comp;Len,fr: Longint; Var S : AnsiString);
  [Public,Alias : 'FPC_'+{$ifdef NOSTRANSI}'ANSISTR'{$else}'STRANSI'{$endif}+'_FIXED'];

Var SS : ShortString;

begin
 ShortStr_Fixed (fi,Len,fr,SS);
 S:=SS;
end;
{$EndIf Support_Fixed}


Procedure ARStr (D : Real;Len,fr: Longint; Var S : AnsiString);
  [Public,Alias : 'FPC_'+{$ifdef NOSTRANSI}'ANSISTR'{$else}'STRANSI'{$endif}+'_REAL'];

Var SS : ShortString;

begin
 ShortStr_real (D,Len,fr,SS);
 S:=SS;
end;


Procedure AEStr (E : Extended;Len,Fr: Longint; Var S : AnsiString);
  [Public,Alias : 'FPC_'+{$ifdef NOSTRANSI}'ANSISTR'{$else}'STRANSI'{$endif}+'_EXTENDED'];

Var SS : ShortString;

begin
 ShortStr_Extended (E,Len,fr,SS);
 S:=SS;
end;


{$endif INTERNDOUBLE}


Procedure ACStr (C : Cardinal;Len : Longint; Var S : AnsiString);
  [Public,Alias : 'FPC_'+{$ifdef NOSTRANSI}'ANSISTR'{$else}'STRANSI'{$endif}+'_CARDINAL'];

Var SS : ShortString;

begin
  int_str_cardinal(C,Len,SS);
  S:=SS;
end;



Procedure ALStr (L : Longint; Len : Longint; Var S : AnsiString);
  [Public,Alias : 'FPC_'+{$ifdef NOSTRANSI}'ANSISTR'{$else}'STRANSI'{$endif}+'_LONGINT'];

Var SS : ShortString;

begin
 int_Str_Longint (L,Len,SS);
 S:=SS;
end;


Procedure Delete (Var S : AnsiString; Index,Size: Longint);

Var LS : Longint;

begin
  If Length(S)=0 then exit;
  if index<=0 then
    begin
    Size:=Size+index-1;
    index:=1;
    end;
  LS:=PAnsiRec(Pointer(S)-FirstOff)^.Len;
  if (Index<=LS) and (Size>0) then
    begin
    UniqueAnsiString (S);
    if Size+Index>LS then
      Size:=LS-Index+1;
    if Index+Size<=LS then
      begin
      Dec(Index);
      Move(PByte(Pointer(S))[Index+Size],
           PByte(Pointer(S))[Index],LS-Index+1);
      end;
    Setlength(s,LS-Size);
    end;
end;

Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : Longint);

var Temp : AnsiString;
    LS : Longint;

begin
  If Length(Source)=0 then exit;
  if index <= 0 then index := 1;
  Ls:=Length(S);
  if index > LS then index := LS+1;
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


{
  $Log$
  Revision 1.28.2.1  1999-06-14 00:39:07  peter
    * setlength finally fixed when l < length(s)

  Revision 1.28  1999/06/09 23:00:16  peter
    * small ansistring fixes
    * val_ansistr_sint destsize changed to longint
    * don't write low/hi ascii with -al

  Revision 1.27  1999/06/05 20:48:56  michael
  Copy checks index now for negative values.

  Revision 1.26  1999/05/31 20:37:39  peter
    * fixed decr_ansistr which didn't set s to nil

  Revision 1.25  1999/05/17 22:41:24  florian
    * small fixes for the new ansistring temp. management

  Revision 1.24  1999/05/17 21:52:35  florian
    * most of the Object Pascal stuff moved to the system unit

  Revision 1.23  1999/05/06 09:05:11  peter
    * generic write_float str_float

  Revision 1.22  1999/04/22 10:51:17  peter
    * fixed pchar 2 ansi

  Revision 1.21  1999/04/13 09:02:06  michael
  + 1 byte too much allocated in new_ansiStringastrings.inc

  Revision 1.20  1999/04/09 07:33:15  michael
  * More fixes and optimizing for ansistr_concat

  Revision 1.19  1999/04/08 15:57:53  peter
    + subrange checking for readln()

  Revision 1.18  1999/04/08 10:19:55  peter
    * fixed concat when s1 or s2 was nil

  Revision 1.17  1999/04/06 11:23:58  peter
    * fixed insert on last char
    * saver chararray 2 ansi

  Revision 1.16  1999/04/06 10:06:51  michael
  * Fixed chararray to ansistring conversion

  Revision 1.15  1999/04/01 22:00:48  peter
    * universal names for str/val (ansistr instead of stransi)
    * '1.' support for val() this is compatible with tp7

  Revision 1.14  1999/03/16 17:49:40  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in text.inc: changed RTE 106 when read integer values are out of bounds to RTE 201
    * in systemh.inc: disabled "support_fixed" for the i386 because it gave internal errors,

  Revision 1.13  1999/03/02 18:24:51  peter
    * function names cleanup
    + chararray -> ansistring

  Revision 1.12  1999/03/01 15:41:02  peter
    * use external names
    * removed all direct assembler modes

  Revision 1.11  1999/02/04 14:55:42  michael
  * Fixed pos

  Revision 1.10  1999/02/04 10:49:21  florian
    + routines for range checking added

  Revision 1.9  1999/02/02 11:37:34  peter
    * fixed ansi2short

  Revision 1.8  1999/01/06 14:48:43  michael
  + Implemented more str() functions

  Revision 1.7  1999/01/06 13:03:39  peter
    * fixed str() and made it working

  Revision 1.6  1999/01/06 12:25:02  florian
    * naming for str(...) routines inserted
    * don't know what in int64 changed

  Revision 1.5  1998/12/15 22:43:01  peter
    * removed temp symbols

  Revision 1.4  1998/11/18 10:56:46  michael
  + Fixed pchar2ansi

  Revision 1.3  1998/11/17 12:16:07  michael
  + Fixed copy. Now reference count is correct

  Revision 1.2  1998/11/17 11:33:22  peter
    + several checks for empty string

  Revision 1.1  1998/11/17 10:34:18  michael
  + renamed from astrings.pp to astrings.inc

  Revision 1.34  1998/11/17 00:41:11  peter
    * renamed string functions

  Revision 1.33  1998/11/16 15:42:04  peter
    + char2ansi

  Revision 1.32  1998/11/16 11:11:47  michael
  + Fix for Insert and Delete functions

  Revision 1.31  1998/11/13 14:37:11  michael
  + Insert procedure corrected

  Revision 1.30  1998/11/05 14:20:36  peter
    * removed warnings

  Revision 1.29  1998/11/04 20:34:04  michael
  + Removed ifdef useansistrings

  Revision 1.28  1998/11/04 15:39:44  michael
  + Small fixes to assign and add

  Revision 1.27  1998/11/04 10:20:48  peter
    * ansistring fixes

  Revision 1.26  1998/11/02 09:46:12  michael
  + Fix for assign of null string

  Revision 1.25  1998/10/30 21:42:48  michael
  Fixed assignment of NIL string.

  Revision 1.24  1998/10/22 11:32:23  michael
  + AssignAnsistring no longer copies constant ansistrings;
  + CompareAnsiString is now faster (1 call to length less)
  + UniqueAnsiString is fixed.

  Revision 1.23  1998/10/21 23:01:54  michael
  + Some more corrections

  Revision 1.22  1998/10/21 09:03:11  michael
  + more fixes so it compiles

  Revision 1.21  1998/10/21 08:56:58  michael
  + Fix so it compiles

  Revision 1.20  1998/10/21 08:38:46  florian
    * ansistringconcat fixed

  Revision 1.19  1998/10/20 12:46:11  florian
    * small fixes to ansicompare

  Revision 1.18  1998/09/28 14:02:34  michael
  + AnsiString changes

  Revision 1.17  1998/09/27 22:44:50  florian
    * small fixes
    * made UniqueAnsistring public
    * ...

  Revision 1.16  1998/09/20 17:49:08  florian
    * some ansistring fixes

  Revision 1.15  1998/09/19 08:33:17  florian
    * some internal procedures take now an pointer instead of a
      ansistring

  Revision 1.14  1998/09/14 10:48:14  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.13  1998/08/23 20:58:51  florian
    + rtti for objects and classes
    + TObject.GetClassName implemented

  Revision 1.12  1998/08/22 09:32:12  michael
  + minor fixes typos, and ansi2pchar

  Revision 1.11  1998/08/08 12:28:10  florian
    * a lot small fixes to the extended data type work

  Revision 1.10  1998/07/29 21:44:34  michael
  + Implemented reading/writing of ansistrings

  Revision 1.9  1998/07/20 23:36:56  michael
  changes for ansistrings

  Revision 1.8  1998/07/13 21:19:09  florian
    * some problems with ansi string support fixed

  Revision 1.7  1998/07/06 14:29:08  michael
  + Added Public,Alias directives for some calls

  Revision 1.6  1998/06/25 08:41:44  florian
    * better rtti

  Revision 1.5  1998/06/12 07:39:13  michael
  + Added aliases for Incr/Decr ref.

  Revision 1.4  1998/06/08 19:35:02  michael
  Some changes to integrate in system unit

  Revision 1.3  1998/06/08 12:38:22  michael
  Implemented rtti, inserted ansistrings again

  Revision 1.2  1998/05/12 10:42:44  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
