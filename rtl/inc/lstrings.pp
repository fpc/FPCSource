{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This file contains the implementation of the LongString type,
  and all things that are needed for it.
  LongSTring is defined as a 'silent' pchar :
  a pchar that points to :

  @   : Longint for size
  @+4 : Unused byte;
  @+5 : String;
   So LS[i] is converted to the address @LS+4+i.

  pchar[0]-pchar[3] : Longint Size
  pchar [4] : Unused
  pchar[5] : String;

}

{$ifdef lstrings_unit}
{ Compile as a separate unit - development only}
unit lstrings;

Interface

Type longstring = pchar;
     ShortString = string;

{ Internal functions, will not appear in systemh.inc }

Function  NewLongString (Len : Longint) : LongString;
Procedure DisposeLongString (Var S : LongString; Len : Longint);
Procedure Long_String_Concat (Var S1 : LongString; Const S2 : LongString; maxlen : Longint);
Procedure Long_ShortString_Concat (Var S1: LongString; Const S2 : ShortString; maxlen : Longint);
Procedure Long_To_ShortString (Var S1 : ShortString; Const S2 : LongString; Maxlen : Longint);
Procedure Short_To_LongString (Var S1 : LongString; Const S2 : ShortString; Maxlen : Longint);
Function  LongCompare (Const S1,S2 : Longstring): Longint;
Function  LongCompare (Const S1 : LongString; Const S2 : ShortString): Longint;

{ Public functions, Will end up in systemh.inc }

Procedure SetLength (Var S : LongString; l : Longint);
Procedure Write_Text_LongString (Len : Longint; T : Textrec; Var S : LongString);
Function  Length (Const S : LongString) : Longint;
Function  Copy (Const S : LongString; Index,Size : Longint) : LongString;
Function  Pos (Const Substr : LongString; Const Source : Longstring) : Longint;
Procedure Insert (Const Source : LongString; Var S : LongString; Index : Longint);
Procedure Delete (Var S : LongString; Index,Size: Longint);
Procedure Val (Const S : LongString; var R : real; Var Code : Integer);
{Procedure Val (Const S : LongString; var D : Double; Var Code : Integer);}
Procedure Val (Const S : LongString; var E : Extended; Code : Integer);
Procedure Val (Const S : LongString; var C : Cardinal; Code : Integer);
Procedure Val (Const S : LongString; var L : Longint; Var Code : Integer);
Procedure Val (Const S : LongString; var W : Word; Var Code : Integer);
Procedure Val (Const S : LongString; var I : Integer; Var Code : Integer);
Procedure Val (Const S : LongString; var B : Byte; Var Code : Integer);
Procedure Val (Const S : LongString; var SI : ShortInt; Var  Code : Integer);
Procedure Str (Const R : Real;Len, fr : longint; Var S : LongString);
{Procedure Str (Const D : Double;Len,fr : longint; Var S : LongString);}
Procedure Str (Const E : Extended;Len,fr : longint; Var S : LongString);
Procedure Str (Const C : Cardinal;len : Longint; Var S : LongString);
Procedure Str (Const L : LongInt;len : longint; Var S : LongString);
Procedure Str (Const W : Word;len : longint; Var S : LongString);
Procedure Str (Const I : Integer;len : Longint; Var S : LongString);
Procedure Str (Const B : Byte; Len : longint; Var S : LongString);
Procedure Str (Const SI : ShortInt; Len : longint; Var S : LongString);

Implementation

{$endif}

Type PLongint = ^Longint;

{ ---------------------------------------------------------------------
  Internal functions, not in interface.
  ---------------------------------------------------------------------}

Function  NewLongString (Len : Longint) : LongString;
{
  Allocate a new string on the heap.
  initialize it to zero length
}
Var P : Pointer;

begin
  GetMem(P,Len+5);
  If P<>Nil then
     begin
     PLongint(P)^:=0;
     pchar(P+4)^:=#0;
     end;
  NewLongString:=P;
end;



Procedure DisposeLongString (Var S : LongString; Len : Longint);
{
  DeAllocates a LongString From the heap.
}
begin
  FreeMem (Pointer(S),Len+5);
end;



Procedure Long_String_Concat (Var S1 : LongString; Const S2 : LongString; maxlen : Longint);
{
  Concatenates 2 LongStrings : S1+S2
  If maxlen<>-1 then the result has maximal length maxlen.
}
Var Size : Longint;

begin
  Size:=PLongint(S2)^;
  If maxlen<>-1 then
    if Size+PLongint(S1)^>MaxLen then
      Size:=Maxlen-PLongint(S1)^;
  If Size<=0 then exit;
  Move (pchar(S2)[5],pchar(S1)[PLongint(S1)^+5],Size);
  PLongint(S1)^:=PLongint(S1)^+Size;
end;



Procedure Long_ShortString_Concat (Var S1: LongString; Const S2 : ShortString; maxlen : Longint);
{
  Concatenates a long with a short string; : S2 + S2
  If maxlen<>-1 then the result has maximal length maxlen.
}
Var Size : Longint;

begin
  Size:=Byte(S2[0]);
  if MaxLen<>-1 then
    if Size+PLongint(S1)^>Maxlen then
      Size:=Maxlen-PLongint(S1)^;
  If Size<=0 then exit;
  Move (S2[1],Pchar(S1)[PLongint(S1)^+5],Size);
  PLongint(S1)^:=PLongint(S1)^+Size;
end;



Procedure Long_To_ShortString (Var S1 : ShortString; Const S2 : LongString; Maxlen : Longint);
{
 Converts a LongString to a longstring;
 if maxlen<>-1, the resulting string has maximal length maxlen
 else a default length of 255 is taken.
}
Var Size : Longint;

begin
  Size:=PLongint(S2)^;
  if maxlen=-1 then maxlen:=255;
  If Size>maxlen then Size:=maxlen;
  Move (Pchar(S2)[5],S1[1],Size);
  S1[0]:=chr(Size);
end;



Procedure Short_To_LongString (Var S1 : LongString; Const S2 : ShortString; Maxlen : Longint);
{
 Converts a ShortString to a LongString;
 if maxlen<>-1 then the resulting string has length maxlen.
}
Var Size : Longint;

begin
  Size:=Byte(S2[0]);
  if maxlen=-1 then maxlen:=255;
  If Size>maxlen then Size:=maxlen;
  Move (S2[1],pchar(S1)[5],Size);
  PLongint(S1)^:=Size;
end;



Function LongCompare (Const S1,S2 : Longstring): Longint;
{
  Compares 2 longStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var i,MaxI,Temp : Longint;

begin
 Temp:=0;
 i:=1;
 MaxI:=PLongint(S1)^;
 if MaxI>PLOngint(S2)^ then MaxI:=PLongint(S2)^;
 While (i<=MaxI) and (Temp=0) do
   begin
   Temp:= Byte( Pchar(S1)[i+4] ) - Byte( Pchar(S2)[I+4] );
   inc(i);
   end;
 if temp=0 then temp:=Plongint(S1)^-PLongint(S2)^;
 LongCompare:=Temp;
end;



Function LongCompare (Const S1 : LongString; Const S2 : ShortString): Longint;
{
  Compares a longString with a ShortString;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var i,MaxI,Temp : Longint;

begin
 Temp:=0;
 i:=1;
 MaxI:=PLongint(S1)^;
 if MaxI>byte(S2[0]) then MaxI:=Byte(S2[0]);
 While (i<=MaxI) and (Temp=0) do
   begin
   Temp:=(Byte(Pchar(S1)[i+4])-Byte(S2[I]));
   inc(i);
   end;
 LongCompare:=Temp;
end;



Procedure Write_Text_LongString (Len : Longint; T : TextRec; Var S : LongString);
{
 Writes a LongString to the Text file T
}
begin
end;


{ ---------------------------------------------------------------------
   Public functions, In interface.
  ---------------------------------------------------------------------}

Function Length (Const S : LongString) : Longint;

begin
  Length:=PLongint(S)^;
end;



Procedure SetLength (Var S : LongString; l : Longint);

begin
  PLongint(S)^:=l;
end;

Function Copy (Const S : LongString; Index,Size : Longint) : LongString;

var ResultAddress : pchar;

begin
  ResultAddress:=NewLongString (Size);
  if ResultAddress=Nil then
    {We're in deep shit here !!}
    exit;
  dec(index);
  if PLongint(S)^<Index+Size then
    Size:=PLongint(S)^-Index;
  if Size>0 then
    Move (Pchar(S)[Index+5],ResultAddress[5],Size)
  Else
    Size:=0;
  PLongint(ResultAddress)^:=Size;
  Copy:=ResultAddress
end;



Function Pos (Const Substr : LongString; Const Source : Longstring) : Longint;

var i,j : longint;
    e : boolean;
    s : longstring;

begin
 i := 0;
 j := 0;
 e := true;
 if Plongint(substr)^=0 then e := false;
 while (e) and (i <= length (Source) - length (substr)) do
   begin
   inc (i);
   s :=copy(Source,i,length(Substr));
   if LongCompare(substr,s)=0 then
     begin
     j := i;
     e := false;
     end;
   DisposeLongString(s,length(Substr));
   end;
 pos := j;
end;



Procedure Val (Const S : LongString; var R : real; Var Code : Integer);

Var SS : String;

begin
 Long_To_ShortString (SS,S,255);
 System.Val(SS,R,Code);
end;


{
Procedure Val (Const S : LongString; var D : Double; Var Code : Integer);

Var SS : ShortString;

begin
 Long_To_ShortString (SS,S,255);
 Val(SS,D,Code);
end;
}


Procedure Val (Const S : LongString; var E : Extended; Code : Integer);

Var SS : ShortString;

begin
 Long_To_ShortString (SS,S,255);
 System.Val(SS,E,Code);
end;



Procedure Val (Const S : LongString; var C : Cardinal; Code : Integer);

Var SS : ShortString;

begin
 Long_To_ShortString (SS,S,255);
 System.Val(SS,C,Code);
end;



Procedure Val (Const S : LongString; var L : Longint; Var Code : Integer);

Var SS : ShortString;

begin
 Long_To_ShortString (SS,S,255);
 System.Val(SS,L,Code);
end;



Procedure Val (Const S : LongString; var W : Word; Var Code : Integer);

Var SS : ShortString;

begin
 Long_To_ShortString (SS,S,255);
 System.Val(SS,W,Code);
end;



Procedure Val (Const S : LongString; var I : Integer; Var Code : Integer);

Var SS : ShortString;

begin
 Long_To_ShortString (SS,S,255);
 System.Val(SS,I,Code);
end;



Procedure Val (Const S : LongString; var B : Byte; Var Code : Integer);

Var SS : ShortString;

begin
 Long_To_ShortString (SS,S,255);
 System.Val(SS,B,Code);
end;



Procedure Val (Const S : LongString; var SI : ShortInt; Var Code : Integer);

Var SS : ShortString;

begin
 Long_To_ShortString (SS,S,255);
 System.Val(SS,SI,Code);
end;


Procedure Str (Const R : Real;Len,fr : Longint; Var S : LongString);

Var SS : ShortString;

begin
 {int_Str_Real (R,Len,fr,SS);}
 Short_To_LongString (S,SS,255);
end;


{
Procedure Str (Const D : Double;Len,fr: Longint; Var S : LongString);

Var SS : ShortString;

begin
 {int_Str_Double (D,Len,fr,SS);}
 Short_To_LongString (S,SS,255);
end;
}


Procedure Str (Const E : Extended;Lenf,Fr: Longint; Var S : LongString);

Var SS : ShortString;

begin
 {int_Str_Extended (E,Len,fr,SS);}
 Short_To_LongString (S,SS,255);
end;



Procedure Str (Const C : Cardinal;Len : Longint; Var S : LongString);

begin
end;



Procedure Str (Const L : Longint; Len : Longint; Var S : LongString);

Var SS : ShortString;

begin
 {int_Str_Longint (L,Len,fr,SS);}
 Short_To_LongString (S,SS,255);
end;



Procedure Str (Const W : Word;Len : Longint; Var S : LongString);

begin
end;



Procedure Str (Const I : Integer;Len : Longint; Var S : LongString);

begin
end;



Procedure Str (Const B : Byte; Len : Longint; Var S : LongString);

begin
end;



Procedure Str (Const SI : ShortInt; Len : Longint; Var S : LongString);

begin
end;



Procedure Delete (Var S : LongString; Index,Size: Longint);

begin
  if index<=0 then
    begin
    Size:=Size+index-1;
    index:=1;
    end;
  if (Index<=PLongint(s)^) and (Size>0) then
    begin
    if Size+Index>PLongint(s)^ then
      Size:=PLongint(s)^-Index+1;
    PLongint(s)^:=PLongint(s)^-Size;
    if Index<=Length(s) then
      Move(pchar(s)[Index+Size+4],pchar(s)[Index+4],Length(s)-Index+1);
    end;
end;

Procedure Insert (Const Source : LongString; Var S : LongString; Index : Longint);

var s3,s4 : pchar;

begin
  if index <= 0 then index := 1;
  s3 := longString(copy (s, index, length(s)));
  if index > PLongint(s)^ then index := PLongint(S)^+1;
  PLongint(s)^ := index - 1;
  s4 :=Pchar ( NewLongString (Plongint(Source)^) );
  Long_String_Concat(LongString(s4),Source,-1);
  Long_String_Concat(LongString(S4),LongString(s3),-1);
  Long_String_Concat(S,LongString(S4),-1);
  DisposeLongstring(LongString(S3),PLongint(S3)^);
  DisposeLongString(LongString(S4),PLongint(S4)^);
end;

{$ifdef lstrings_unit}
end.
