{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ ---------------------------------------------------------------------
   This file implements AnsiStrings for FPC
  ---------------------------------------------------------------------}


{
  This file contains the implementation of the LongString type,
  and all things that are needed for it.
  AnsiSTring is defined as a 'silent' pchar :
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


Function  NewAnsiString (Len : Longint) : Pointer; forward;
Procedure DisposeAnsiString (Var S : Pointer); forward;
Procedure Decr_Ansi_Ref (Var S : Pointer); forward;
Procedure Incr_Ansi_Ref (Var S : Pointer); forward;
Procedure AssignAnsiString (Var S1 : Pointer; S2 : Pointer); forward;
Function  Ansi_String_Concat (S1,S2 : Pointer): Pointer; forward;
Procedure Ansi_ShortString_Concat (Var S1: AnsiString; Var S2 : ShortString); forward;
Procedure Ansi_To_ShortString (Var S1 : ShortString; S2 : Pointer; maxlen : longint); forward;
Procedure Short_To_AnsiString (Var S1 : Pointer; Const S2 : ShortString); forward;
Procedure Char_To_AnsiString(var S1 : Pointer; c : Char); forward;
Function  AnsiCompare (S1,S2 : Pointer): Longint; forward;
Function  AnsiCompare (var S1 : Pointer; Var S2 : ShortString): Longint; forward;
Procedure SetCharAtIndex (Var S : AnsiString; Index : Longint; C : CHar); forward;

Type
  TAnsiRec = Packed Record
    Maxlen, len, ref :  Longint;
    First : Char;
  end;
  PAnsiRec = ^TAnsiRec;

Const
  AnsiRecLen = SizeOf(TAnsiRec);
  FirstOff   = SizeOf(TAnsiRec)-1;


{ ---------------------------------------------------------------------
  Internal functions, not in interface.
  ---------------------------------------------------------------------}

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


Function NewAnsiString(Len : Longint) : Pointer;
{
  Allocate a new AnsiString on the heap.
  initialize it to zero length and reference count 1.
}
Var
  P : Pointer;
begin
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
  If S=Nil then exit;
  Dec (Longint(S),FirstOff);
  FreeMem (S,PAnsiRec(S)^.Maxlen+AnsiRecLen);
  S:=Nil;
end;


Procedure Decr_Ansi_Ref (Var S : Pointer);[Public,Alias:{$ifdef NEWSTRNAMES}'FPC_ANSISTR_DECR_REF'{$else}'FPC_DECR_ANSI_REF'{$endif}];
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
end;


Procedure Incr_Ansi_Ref (Var S : Pointer);[Public,Alias:{$ifdef NEWSTRNAMES}'FPC_ANSISTR_INCR_REF'{$else}'FPC_INCR_ANSI_REF'{$endif}];
Begin
  If S=Nil then
    exit;
  { Let's be paranoid : Constant string ??}
  If PAnsiRec(S-FirstOff)^.Ref<0 then exit;
  Inc(PAnsiRec(S-FirstOff)^.Ref);
end;


Procedure UniqueAnsiString (Var S : AnsiString); [Public,Alias : 'FPC_ANSISTR_UNIQUE'];
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
    Decr_Ansi_Ref (Pointer(S));  { Thread safe }
    Pointer(S):=SNew;
    end;
end;


Procedure AssignAnsiString (Var S1 : Pointer;S2 : Pointer);[Public,Alias:{$ifdef NEWSTRNAMES}'FPC_ANSISTR_ASSIGN'{$else}'FPC_ASSIGN_ANSI_STRING'{$endif}];
{
  Assigns S2 to S1 (S1:=S2), taking in account reference counts.
}
begin
  If S2<>nil then
    If PAnsiRec(S2-FirstOff)^.Ref>0 then
      Inc(PAnsiRec(S2-FirstOff)^.ref);
  { Decrease the reference count on the old S1 }
  Decr_Ansi_Ref (S1);
  { And finally, have S1 pointing to S2 (or its copy) }
  S1:=S2;
end;


function Ansi_String_Concat (S1,S2 : Pointer) : pointer;[Public, alias: 'FPC_ANSISTR_CONCAT'];
{
  Concatenates 2 AnsiStrings : S1+S2.
  Result Goes to S3;
}
Var
  Size,Location : Longint;
  S3 : pointer;
begin
  if (S1=Nil) then
     AssignAnsiString(S3,S2)
  else
    begin
       S3:=Nil;
       Size:=PAnsiRec(S2-FirstOff)^.Len;
       Location:=Length(AnsiString(S1));
       { Setlength takes case of uniqueness
         and allocated memory. We need to use length,
         to take into account possibility of S1=Nil }
       SetLength (AnsiString(S3),Size+Location);
       Move (S1^,S3^,PAnsiRec(S1-FirstOff)^.Len);
       Move (S2^,(S3+location)^,Size+1);
    end;
  Ansi_String_Concat:=S3;
end;


Procedure Ansi_ShortString_Concat (Var S1: AnsiString; Var S2 : ShortString);
{
  Concatenates a Ansi with a short string; : S2 + S2
}
Var
  Size,Location : Longint;
begin
  Size:=Length(S2);
  Location:=Length(S1);
  If Size=0 then exit;
  { Setlength takes case of uniqueness
    and alllocated memory. We need to use length,
    to take into account possibility of S1=Nil }
  SetLength (S1,Size+Length(S1));
  Move (S2[1],Pointer(Pointer(S1)+Location)^,Size);
  PByte( Pointer(S1)+length(S1) )^:=0; { Terminating Zero }
end;


Procedure Ansi_To_ShortString (Var S1 : ShortString;S2 : Pointer; Maxlen : Longint);[Public, alias: 'FPC_ANSISTR_TO_SHORTSTR'];
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
     If Size>maxlen then
      Size:=maxlen;
     Move (S2^,S1[1],Size);
     byte(S1[0]):=Size;
   end;
end;


Procedure Short_To_AnsiString (Var S1 : Pointer; Const S2 : ShortString);[Public, alias: 'FPC_SHORTSTR_TO_ANSISTR'];
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


Procedure Char_To_AnsiString(var S1 : Pointer; c : Char);[Public, alias: 'FPC_CHAR_TO_ANSISTR'];
{
  Converts a ShortString to a AnsiString;
}
begin
  Setlength (AnsiString(S1),1);
  PByte(Pointer(S1))^:=byte(c);
  { Terminating Zero }
  PByte(Pointer(S1)+1)^:=0;
end;


Procedure PChar2Ansi(var a : ansistring;p : pchar);[Public,Alias : 'FPC_PCHAR_TO_ANSISTR'];
begin
  { !!!!!!!!! needs to be fixed (FK) }
  if p[0]=#0 Then
    Pointer(a):=nil
  else
    Pointer(a):=p;
end;

{ the compiler generates inline code for that

Const
  EmptyChar : char = #0;
Function Ansi2pchar (S : Pointer) : Pchar; [Alias : 'FPC_ANSISTR_TO_PCHAR'];
begin
  If S<>Nil then
    Ansi2Pchar:=S
  else
    Ansi2Pchar:=@emptychar;
end;
}

{ stupid solution, could be done with public,name in later versions }
{$ASMMODE DIRECT}
procedure dummy;assembler;
  asm
     .globl FPC_EMPTYCHAR
     FPC_EMPTYCHAR:
     .byte 0
  end;
{$ASMMODE ATT}


Function AnsiCompare(S1,S2 : Pointer): Longint;[Public,Alias : 'FPC_ANSISTR_COMPARE'];
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
  AnsiCompare:=Temp;
end;



Function AnsiCompare (Var S1 : Pointer; Var S2 : ShortString): Longint;
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
 if MaxI>byte(S2[0]) then MaxI:=Byte(S2[0]);
 While (i<MaxI) and (Temp=0) do
   begin
   Temp:= PByte(S1+I)^ - Byte(S2[i+1]);
   inc(i);
   end;
 AnsiCompare:=Temp;
end;


{ Not used, can be removed. }
Procedure SetCharAtIndex (Var S : AnsiString; Index : Longint; C : CHar);
begin
  if Index<=Length(S) then
    begin
    UniqueAnsiString(S);
    Pbyte(Pointer(S)+index-1)^:=Byte(C);
    end;
end;

{ ---------------------------------------------------------------------
   Public functions, In interface.
  ---------------------------------------------------------------------}

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
   If (Pointer(S)=Nil) and (l>0) then
    begin
    { Need a complete new string...}
    Pointer(s):=NewAnsiString(l);
    PAnsiRec(Pointer(S)-FirstOff)^.Len:=l;
    PAnsiRec(Pointer(S)-FirstOff)^.MaxLen:=l;
    PByte (Pointer(S)+l)^:=0;
    end
  else if l>0 then
    begin
    If (PAnsiRec(Pointer(S)-FirstOff)^.Maxlen < L) or
       (PAnsiRec(Pointer(S)-FirstOff)^.Ref <> 1) then
      begin
      { Reallocation is needed... }
      Temp:=Pointer(NewAnsiString(L));
      if Length(S)>0 then
        Move (Pointer(S)^,Temp^,Length(S)+1);
      Decr_Ansi_ref (Pointer(S));
      Pointer(S):=Temp;
      end
    else
      //!! Force nil termination in case it gets shorter
      PByte(Pointer(S)+l)^:=0;
    PAnsiRec(Pointer(S)-FirstOff)^.Len:=l;
    end
  else
    { Length=0 }
    begin
    Decr_Ansi_Ref (Pointer(S));
    Pointer(S):=Nil;
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
    ResultAddress:=Pointer(NewAnsiString (Size));
    if ResultAddress<>Nil then
      begin
      Move (Pointer(Pointer(S)+index)^,ResultAddress^,Size);
      PAnsiRec(ResultAddress-FirstOff)^.Len:=Size;
      PByte(ResultAddress+Size)^:=0;
      end;
    end;
  Copy:=AnsiString(ResultAddress);
end;



Function Pos (Const Substr : AnsiString; Const Source : AnsiString) : Longint;

var i,j : longint;
    e : boolean;
    s,se : Pointer;

begin
 i := 0;
 j := 0;
 e := true;
 if Length(SubStr)=0 then e := false;
 while (e) and (i <= length (Source) - length (substr)) do
   begin
   inc (i);
   S:=Pointer(copy(Source,i,length(Substr)));
   Se:=pointer(substr);
    if AnsiCompare(se,S)=0 then
     begin
     j := i;
     e := false;
     end;
   DisposeAnsiString(S);
   end;
 pos := j;
end;



Procedure Val (Const S : AnsiString; var R : real; Var Code : Integer);

Var SS : String;

begin
 Ansi_To_ShortString (SS,Pointer(S),255);
 Val(SS,R,Code);
end;


{
Procedure Val (var S : AnsiString; var D : Double; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 Val(SS,D,Code);
end;
}


Procedure Val (Const S : AnsiString; var E : Extended; Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,Pointer(S),255);
 Val(SS,E,Code);
end;



Procedure Val (Const S : AnsiString; var C : Cardinal; Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,Pointer(S),255);
 Val(SS,C,Code);
end;



Procedure Val (Const S : AnsiString; var L : Longint; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,Pointer(S),255);
 Val(SS,L,Code);
end;



Procedure Val (Const S : AnsiString; var W : Word; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,Pointer(S),255);
 Val(SS,W,Code);
end;



Procedure Val (Const S : AnsiString; var I : Integer; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,Pointer(S),255);
 Val(SS,I,Code);
end;



Procedure Val (Const S : AnsiString; var B : Byte; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,Pointer(S),255);
 Val(SS,B,Code);
end;



Procedure Val (Const S : AnsiString; var SI : ShortInt; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,Pointer(S),255);
 Val(SS,SI,Code);
end;

(*
Procedure Str (Const R : Real;Len,fr : Longint; Const S : AnsiString);

Var SS : ShortString;

begin
 {int_Str_Real (R,Len,fr,SS);}
 Short_To_AnsiString (Pointer(S),SS);
end;


{
Procedure Str (Var D : Double;Len,fr: Longint; Var S : AnsiString);

Var SS : ShortString;

begin
 {int_Str_Double (D,Len,fr,SS);}
 Short_To_AnsiString (S,SS);
end;
}


Procedure Str (E : Extended;Lenf,Fr: Longint; Var S : AnsiString);

Var SS : ShortString;

begin
 {int_Str_Extended (E,Len,fr,SS);}
 Short_To_AnsiString (S,SS);
end;



Procedure Str (C : Cardinal;Len : Longint; Var S : AnsiString);

begin
end;



Procedure Str (L : Longint; Len : Longint; Var S : AnsiString);

Var SS : ShortString;

begin
 {int_Str_Longint (L,Len,fr,SS);}
 Short_To_AnsiString (S,SS);
end;



Procedure Str (Var W : Word;Len : Longint; Var S : AnsiString);

begin
end;



Procedure Str (Var I : Integer;Len : Longint; Var S : AnsiString);

begin
end;



Procedure Str (Var B : Byte; Len : Longint; Var S : AnsiString);

begin
end;



Procedure Str (Var SI : ShortInt; Len : Longint; Var S : AnsiString);

begin
end;
*)


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
  If (LS-Index)>1 then
    Move(PByte(Pointer(S))[Index],PByte(temp)[Length(Source)+index],LS-Index);
  S:=Temp;
end;


{
  $Log$
  Revision 1.2  1998-11-17 11:33:22  peter
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
