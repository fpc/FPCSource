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
   This units implements AnsiStrings for FPC
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
{$ifdef astrings_unit} 
{ Compile as a separate unit - development only}
unit astrings;

Interface 

Type AnsiString = Pointer;
     ShortString = string;

{$i textrec.inc}

{ Internal functions, will not appear in systemh.inc }

Function  NewAnsiString (Len : Longint) : AnsiString;
Procedure DisposeAnsiString (Var S : AnsiString);
Procedure Decr_Ansi_Ref (Var S : AnsiString);
Procedure Incr_Ansi_Ref (Var S : AnsiString);
Procedure AssignAnsiString (Var S1 : AnsiString; S2 : AnsiString); 
Procedure Ansi_String_Concat (Var S1 : AnsiString; Const S2 : AnsiString);
Procedure Ansi_ShortString_Concat (Var S1: AnsiString; Const S2 : ShortString);
Procedure Ansi_To_ShortString (Var S1 : ShortString; Const S2 : AnsiString; maxlen : longint);
Procedure Short_To_AnsiString (Var S1 : AnsiString; Const S2 : ShortString);
Function  AnsiCompare (Const S1,S2 : AnsiString): Longint;
Function  AnsiCompare (Const S1 : AnsiString; Const S2 : ShortString): Longint;
Procedure SetCharAtIndex (Var S : AnsiString; Index : Longint; C : CHar);

{ Public functions, Will end up in systemh.inc }

Procedure SetLength (Var S : AnsiString; l : Longint);
Procedure UniqueAnsiString (Var S : AnsiString);
Procedure Write_Text_AnsiString (Len : Longint; T : Textrec; Var S : AnsiString);
Function  Length (Const S : AnsiString) : Longint;
Function  Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;
Function  Pos (Const Substr : AnsiString; Const Source : AnsiString) : Longint;
Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : Longint);
Procedure Delete (Var S : AnsiString; Index,Size: Longint);
Procedure Val (Const S : AnsiString; var R : real; Var Code : Integer);
{Procedure Val (Const S : AnsiString; var D : Double; Var Code : Integer);}
Procedure Val (Const S : AnsiString; var E : Extended; Code : Integer);
Procedure Val (Const S : AnsiString; var C : Cardinal; Code : Integer);
Procedure Val (Const S : AnsiString; var L : Longint; Var Code : Integer);
Procedure Val (Const S : AnsiString; var W : Word; Var Code : Integer);
Procedure Val (Const S : AnsiString; var I : Integer; Var Code : Integer);
Procedure Val (Const S : AnsiString; var B : Byte; Var Code : Integer);
Procedure Val (Const S : AnsiString; var SI : ShortInt; Var  Code : Integer);
Procedure Str (Const R : Real;Len, fr : longint; Var S : AnsiString);
{Procedure Str (Const D : Double;Len,fr : longint; Var S : AnsiString);}
Procedure Str (Const E : Extended;Len,fr : longint; Var S : AnsiString);
Procedure Str (Const C : Cardinal;len : Longint; Var S : AnsiString);
Procedure Str (Const L : LongInt;len : longint; Var S : AnsiString);
Procedure Str (Const W : Word;len : longint; Var S : AnsiString);
Procedure Str (Const I : Integer;len : Longint; Var S : AnsiString);
Procedure Str (Const B : Byte; Len : longint; Var S : AnsiString);
Procedure Str (Const SI : ShortInt; Len : longint; Var S : AnsiString);

Implementation

{$endif}
{$PACKRECORDS 1}
Type TAnsiRec = Record
      Maxlen, len, ref :  Longint;
      First : Char;
     end;
     PAnsiRec = ^TAnsiRec;
     
     PLongint = ^Longint;
     PByte = ^Byte;
     
Const AnsiRecLen = SizeOf(TAnsiRec);
      FirstOff   = SizeOf(TAnsiRec)-1;
      
{ ---------------------------------------------------------------------
  Internal functions, not in interface.
  ---------------------------------------------------------------------}


Procedure DumpAnsiRec ( S : Ansistring);

begin
  If Pointer(S)=Nil then
    Writeln ('String is nil')
  Else
    Begin
    Dec (Longint(S),FirstOff);
    With PansiRec(S)^ do
      begin
      Writeln ('MAxlen : ',maxlen);
      Writeln ('Len    : ',len);
      Writeln ('Ref    : ',ref);
      end;  
    end;
end;



Function  NewAnsiString (Len : Longint) : AnsiString;
{
  Allocate a new AnsiString on the heap.
  initialize it to zero length and reference count 1.
}
Var P : Pointer;

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

Procedure Decr_Ansi_Ref (Var S : AnsiString);
{
 Decreases the ReferenceCount of a non constant ansistring; 
 If the reference count is zero, deallocate the string;
}
Begin
  If Pointer(S)=Nil then exit; { Zero string }
  { check for constant strings ...}
  If PansiRec(Pointer(S)-FirstOff)^.Ref<0 then exit; 
  Dec(PAnsiRec(Pointer(S)-FirstOff)^.Ref);
  If PAnsiRec(Pointer(S)-FirstOff)^.Ref=0 then 
    { Ref count dropped to zero } 
    DisposeAnsiString (S);        { Remove...}
end;

Procedure Incr_Ansi_Ref (Var S : AnsiString);

Begin
  If Pointer(S)=Nil then exit;
  { Let's be paranoid : Constant string ??}
  If PansiRec(Pointer(S)-FirstOff)^.Ref<0 then exit; 
  inc(PAnsiRec(Pointer(S)-FirstOff)^.Ref);
end;

Procedure UniqueAnsiString (Var S : AnsiString);
{
  Make sure reference count of S is 1, 
  using copy-on-write semantics.
}

Var SNew : Pointer;

begin
  If Pointer(S)=Nil then exit;
  if PAnsiRec(Pointer(S)-Firstoff)^.Ref>1 then
    begin
    SNew:=Pointer(NewAnsiString (PAnsiRec(Pointer(S)-FirstOff)^.len));
    Move (Pointer(S)^,SNew^,PAnsiRec(Pointer(S)-FirstOff)^.len+1);
    PAnsiRec(SNew-8)^.len:=PAnsiRec(Pchar(S)-FirstOff)^.len;
    Decr_Ansi_Ref (S);  { Thread safe }
    Pchar(S):=Pchar(SNew);
    end;
end;


Procedure DisposeAnsiString (Var S : AnsiString);
{
  Deallocates a AnsiString From the heap.
}
begin
  If Pointer(S)=Nil then exit;
  Dec (Longint(S),FirstOff);
  FreeMem (S,PAnsiRec(Pointer(S))^.Maxlen+AnsiRecLen);
  Pointer(S):=Nil;
end;

Procedure AssignAnsiString (Var S1 : AnsiString; S2 : AnsiString); 
{
 Assigns S2 to S1 (S1:=S2), taking in account reference counts.
 If S2 is a constant string, a new S1 is allocated on the heap.
}
Var Temp : Pointer;

begin
  If Pointer(S2)<>nil then
    begin
    If PAnsiRec(Pointer(S2)-FirstOff)^.Ref<0 then
      begin
      { S2 is a constant string, Create new string with copy. } 
      Temp:=Pointer(NewAnsiString(PansiRec(Pointer(S2)-FirstOff)^.Len));
      Move (Pointer(S2)^,Temp^,PAnsiRec(Pointer(S2)-FirstOff)^.len+1);
      PAnsiRec(Temp-FirstOff)^.Len:=PAnsiRec(Pointer(S2)-FirstOff)^.len;
      S2:=Temp;
      end
    else
      Inc(PAnsiRec(Pointer(S2)-FirstOff)^.ref)
    end;
  { Decrease the reference count on the old S1 }
  Decr_Ansi_Ref (S1);
  { And finally, have S1 pointing to S2 (or its copy) }
  Pointer(S1):=Pointer(S2);
end;

Procedure Ansi_String_Concat (Var S1 : AnsiString; Const S2 : AnsiString);
{
  Concatenates 2 AnsiStrings : S1+S2. 
  Result Goes to S1;
}
Var Size,Location : Longint;

begin
  if Pointer(S2)=Nil then exit;
  if (Pointer(S1)=Nil) then
     AssignAnsiString(S1,S2)
  else
    begin
    Size:=PAnsiRec(Pointer(S2)-FirstOff)^.Len;
    Location:=Length(S1);
    { Setlength takes case of uniqueness 
      and alllocated memory. We need to use length, 
      to take into account possibility of S1=Nil }
    SetLength (S1,Size+Location); 
    Move (Pointer(S2)^,Pointer(Pointer(S1)+location)^,Size+1);
    end;
end;



Procedure Ansi_ShortString_Concat (Var S1: AnsiString; Const S2 : ShortString);
{
  Concatenates a Ansi with a short string; : S2 + S2
}

Var Size,Location : Longint;

begin
  Size:=byte(S2[0]);
  Location:=Length(S1);
  If Size=0 then exit;
    { Setlength takes case of uniqueness 
      and alllocated memory. We need to use length, 
      to take into account possibility of S1=Nil }
  SetLength (S1,Size+Length(S1)); 
  Move (S2[1],Pointer(Pointer(S1)+Location)^,Size);
  PByte( Pointer(S1)+length(S1) )^:=0; { Terminating Zero }
end;



Procedure Ansi_To_ShortString (Var S1 : ShortString; Const S2 : AnsiString; Maxlen : Longint);
{
 Converts a AnsiString to a ShortString;
 if maxlen<>-1, the resulting string has maximal length maxlen
 else a default length of 255 is taken. 
}
Var Size : Longint;

begin
  Size:=PAnsiRec(Pointer(S2)-FirstOff)^.Len;
  if maxlen=-1 then maxlen:=255;
  If Size>maxlen then Size:=maxlen;
  Move (Pointer(S2)^,S1[1],Size);
  byte(S1[0]):=Size;
end;



Procedure Short_To_AnsiString (Var S1 : AnsiString; Const S2 : ShortString);
{
 Converts a ShortString to a AnsiString;
}

Var Size : Longint;

begin
  Size:=Byte(S2[0]);
  Setlength (S1,Size);
  Move (S2[1],Pointer(S1)^,Size);
  PByte(Pointer(S1)+Size)^:=0; { Terminating Zero }
end;



Function AnsiCompare (Const S1,S2 : AnsiString): Longint;
{
  Compares 2 AnsiStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var i,MaxI,Temp : Longint;

begin
 Temp:=0;
 i:=0;
 MaxI:=Length(S1);
 if MaxI>Length(S2) then MaxI:=Length(S2);
 While (i<MaxI) and (Temp=0) do
   begin
   Temp:= PByte(Pointer(S1)+I)^ - PByte(Pointer(S2)+i)^;
   inc(i);
   end;
 if temp=0 then temp:=Length(S1)-Length(S2);
 AnsiCompare:=Temp; 
end;



Function AnsiCompare (Const S1 : AnsiString; Const S2 : ShortString): Longint;
{
  Compares a AnsiString with a ShortString;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var i,MaxI,Temp : Longint;

begin
 Temp:=0;
 i:=0;
 MaxI:=Length(S1);
 if MaxI>byte(S2[0]) then MaxI:=Byte(S2[0]);
 While (i<MaxI) and (Temp=0) do
   begin
   Temp:= PByte(Pointer(S1)+I)^ - Byte(S2[i+1]);
   inc(i);
   end;
 AnsiCompare:=Temp; 
end;



Procedure Write_Text_AnsiString (Len : Longint; T : TextRec; Var S : AnsiString);
{
 Writes a AnsiString to the Text file T
}
begin
end;

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
Var Temp : Pointer;

begin
  If (S=Nil) and (l>0) then
    begin
    { Need a complete new string...}
    S:=NewAnsiString(l);
    PAnsiRec(Pointer(S)-FirstOff)^.Len:=l;
    PAnsiRec(Pointer(S)-FirstOff)^.Len:=l;
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
        Move (S^,Temp^,Length(S)+1);
      Decr_Ansi_ref (S);
      S:=Temp;
      end;
    PAnsiRec(Pointer(S)-FirstOff)^.Len:=l
    end 
  else
    { Length=0 }
    begin  
    Decr_Ansi_Ref (S);
    S:=Nil;
    end;
end;

Function Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;

var ResultAddress : Pointer;

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
  Copy:=ResultAddress
end;



Function Pos (Const Substr : AnsiString; Const Source : AnsiString) : Longint;

var i,j : longint;
    e : boolean;
    s : Pointer;
    
begin
 i := 0;
 j := 0;
 e := true;
 if Plongint(substr)^=0 then e := false;
 while (e) and (i <= length (Source) - length (substr)) do
   begin
   inc (i);
   S:=Pointer(copy(Source,i,length(Substr)));
   if AnsiCompare(substr,s)=0 then
     begin
     j := i;
     e := false;
     end;
   DisposeAnsiString(AnsiString(S));
   end;
 pos := j;
end;



Procedure Val (Const S : AnsiString; var R : real; Var Code : Integer);

Var SS : String;
    
begin
 Ansi_To_ShortString (SS,S,255);
 System.Val(SS,R,Code);
end;


{
Procedure Val (Const S : AnsiString; var D : Double; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 Val(SS,D,Code);
end;
}


Procedure Val (Const S : AnsiString; var E : Extended; Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 System.Val(SS,E,Code);
end;



Procedure Val (Const S : AnsiString; var C : Cardinal; Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 System.Val(SS,C,Code);
end;



Procedure Val (Const S : AnsiString; var L : Longint; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 System.Val(SS,L,Code);
end;



Procedure Val (Const S : AnsiString; var W : Word; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 System.Val(SS,W,Code);
end;



Procedure Val (Const S : AnsiString; var I : Integer; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 System.Val(SS,I,Code);
end;



Procedure Val (Const S : AnsiString; var B : Byte; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 System.Val(SS,B,Code);
end;



Procedure Val (Const S : AnsiString; var SI : ShortInt; Var Code : Integer);

Var SS : ShortString;

begin
 Ansi_To_ShortString (SS,S,255);
 System.Val(SS,SI,Code);
end;


Procedure Str (Const R : Real;Len,fr : Longint; Var S : AnsiString);

Var SS : ShortString;

begin
 {int_Str_Real (R,Len,fr,SS);}
 Short_To_AnsiString (S,SS);
end;


{
Procedure Str (Const D : Double;Len,fr: Longint; Var S : AnsiString);

Var SS : ShortString;

begin
 {int_Str_Double (D,Len,fr,SS);}
 Short_To_AnsiString (S,SS);
end;
}


Procedure Str (Const E : Extended;Lenf,Fr: Longint; Var S : AnsiString);

Var SS : ShortString;

begin
 {int_Str_Extended (E,Len,fr,SS);}
 Short_To_AnsiString (S,SS);
end;



Procedure Str (Const C : Cardinal;Len : Longint; Var S : AnsiString);

begin
end;



Procedure Str (Const L : Longint; Len : Longint; Var S : AnsiString);

Var SS : ShortString;

begin
 {int_Str_Longint (L,Len,fr,SS);}
 Short_To_AnsiString (S,SS);
end;



Procedure Str (Const W : Word;Len : Longint; Var S : AnsiString);

begin
end;



Procedure Str (Const I : Integer;Len : Longint; Var S : AnsiString);

begin
end;



Procedure Str (Const B : Byte; Len : Longint; Var S : AnsiString);

begin
end;



Procedure Str (Const SI : ShortInt; Len : Longint; Var S : AnsiString);

begin
end;



Procedure Delete (Var S : AnsiString; Index,Size: Longint);

begin
  if index<=0 then
    begin
    Size:=Size+index-1;
    index:=1;
    end;
  if (Index<=length(s)) and (Size>0) then
    begin
    UniqueAnsiString (S);
    if Size+Index>Length(S) then
      Size:=Length(s)-Index+1;
    Setlength(s,Length(s)-Size);
    if Index<=Length(s) then
      Move(Pointer(Pointer(S)+Index+Size-1)^,
           Pointer(Pointer(s)+Index-1)^,Length(s)-Index+2)
     else
       Pbyte(Pointer(S)+Length(S))^:=0;
    end;
end;

Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : Longint);

var s3,s4 : Pointer;

begin
  If Length(Source)=0 then exit;
  if index <= 0 then index := 1;
  s3 := Pointer(copy(s,index,length(s)));
  if index > Length(s) then 
    index := Length(S)+1;
  SetLength(s,index - 1);
  s4 := Pointer ( NewAnsiString(PansiRec(Pointer(Source)-Firstoff)^.len) );
  Ansi_String_Concat(AnsiString(s4),Source);
  if S4<>Nil then
  Ansi_String_Concat(AnsiString(S4),AnsiString(s3));
  Ansi_String_Concat(S,AnsiString(S4));
  Decr_ansi_ref (AnsiString(S3));
  Decr_ansi_ref (AnsiString(S4));
end;

{$ifdef astrings_unit}
end.
{$endif}

{
  $Log$
  Revision 1.2  1998-05-12 10:42:44  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
