{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl,
    member of the Free Pascal development team.

    This file implements support routines for WideStrings with FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This file contains the implementation of the WideString type,
  and all things that are needed for it.
  WideString is defined as a 'silent' pwidechar :
  a pwidechar that points to :

  @-12 : Longint for maximum size;
  @-8  : Longint for size;
  @-4  : Longint for reference count;
  @    : String + Terminating #0;
  Pwidechar(Widestring) is a valid typecast.
  So WS[i] is converted to the address @WS+i-1.

  Constants should be assigned a reference count of -1
  Meaning that they can't be disposed of.
}

Type
  PWideRec = ^TWideRec;
  TWideRec = Packed Record
    Maxlen,
    len,
    ref   : Longint;
    First : WideChar;
  end;

Const
  WideRecLen = SizeOf(TWideRec);
  WideFirstOff = SizeOf(TWideRec)-1;

{
Procedure UniqueWideString(Var S : WideString); [Public,Alias : 'FPC_WIDESTR_UNIQUE'];
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}

begin
end;
}



{****************************************************************************
                    Internal functions, not in interface.
****************************************************************************}

{$ifdef WideStrDebug}
Procedure DumpWideRec(S : Pointer);
begin
  If S=Nil then
    Writeln ('String is nil')
  Else
    Begin
      With PWideRec(S-WideFirstOff)^ do
       begin
         Write   ('(Maxlen: ',maxlen);
         Write   (' Len:',len);
         Writeln (' Ref: ',ref,')');
       end;
    end;
end;
{$endif}


Function NewWideString(Len : Longint) : Pointer;
{
  Allocate a new WideString on the heap.
  initialize it to zero length and reference count 1.
}
Var
  P : Pointer;
begin
  { Also add +1 for a terminating zero }
  GetMem(P,Len+Len+WideRecLen);
  If P<>Nil then
   begin
     PWideRec(P)^.Maxlen:=Len;    { Maximal length }
     PWideRec(P)^.Len:=0;         { Initial length }
     PWideRec(P)^.Ref:=1;         { Set reference count }
     PWideRec(P)^.First:=#0;      { Terminating #0 }
     P:=P+WideFirstOff;               { Points to string now }
   end;
  NewWideString:=P;
end;


Procedure DisposeWideString(Var S : Pointer);
{
  Deallocates a WideString From the heap.
}
begin
  If S=Nil then
    exit;
  Dec (Longint(S),WideFirstOff);
  FreeMem (S);
  S:=Nil;
end;


Procedure WideStr_Decr_Ref (Var S : Pointer);[Public,Alias:'FPC_WIDESTR_DECR_REF'];
{
  Decreases the ReferenceCount of a non constant widestring;
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
  l:=@PWIDEREC(S-WideFirstOff)^.Ref;
  If l^<0 then exit;
  Dec(l^);
  If l^=0 then
    { Ref count dropped to zero }
    DisposeWideString (S);        { Remove...}
  { this pointer is not valid anymore, so set it to zero }
  S:=nil;
end;


Procedure WideStr_Incr_Ref (Var S : Pointer);[Public,Alias:'FPC_WIDESTR_INCR_REF'];
Begin
  If S=Nil then
    exit;
  { Let's be paranoid : Constant string ??}
  If PWideRec(S-WideFirstOff)^.Ref<0 then exit;
  Inc(PWideRec(S-WideFirstOff)^.Ref);
end;


Procedure WideStr_Assign (Var S1 : Pointer;S2 : Pointer);[Public,Alias:'FPC_WIDESTR_ASSIGN'];
{
  Assigns S2 to S1 (S1:=S2), taking in account reference counts.
}
begin
  If S2<>nil then
    If PWideRec(S2-WideFirstOff)^.Ref>0 then
      Inc(PWideRec(S2-WideFirstOff)^.ref);
  { Decrease the reference count on the old S1 }
  widestr_decr_ref (S1);
  { And finally, have S1 pointing to S2 (or its copy) }
  S1:=S2;
end;


Procedure WideStr_Concat (S1,S2 : Pointer;var S3 : Pointer);[Public, alias: 'FPC_WIDESTR_CONCAT'];
{
  Concatenates 2 WideStrings : S1+S2.
  Result Goes to S3;
}
Var
  Size,Location : Longint;
begin
{ create new result }
  if S3<>nil then
    WideStr_Decr_Ref(S3);
{ only assign if s1 or s2 is empty }
  if (S1=Nil) then
    WideStr_Assign(S3,S2)
  else
    if (S2=Nil) then
      WideStr_Assign(S3,S1)
  else
    begin
       Size:=PWideRec(S2-WideFirstOff)^.Len;
       Location:=Length(WideString(S1));
       SetLength (WideString(S3),Size+Location);
       Move (S1^,S3^,Location);
       Move (S2^,(S3+location)^,Size+1);
    end;
end;


(* !!!:
Procedure Char_To_WideStr(var S1 : Pointer; c : Char);[Public, alias: 'FPC_CHAR_TO_WIDESTR'];
{
  Converts a ShortString to a WideString;
}
begin
  Setlength (WideString(S1),1);
  PByte(Pointer(S1))^:=byte(c);
  { Terminating Zero }
  PByte(Pointer(S1)+1)^:=0;
end;


Procedure PChar_To_WideStr(var a : widestring;p : pchar);[Public,Alias : 'FPC_PCHAR_TO_WIDESTR'];
Var
  L : Longint;
begin
  if pointer(a)<>nil then
    begin
       WideStr_Decr_Ref(Pointer(a));
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
      Pointer(a):=NewWidestring(L);
      SetLength(A,L);
      Move (P[0],Pointer(A)^,L)
    end;
end;


Procedure CharArray_To_WideStr(var a : widestring;p : pchar;l:longint);[Public,Alias : 'FPC_CHARARRAY_TO_WIDESTR'];
var
  i  : longint;
  hp : pchar;
begin
  if p[0]=#0 Then
    Pointer(a):=nil
  else
    begin
      Pointer(a):=NewWidestring(L);
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
*)


Function WideStr_Compare(S1,S2 : Pointer): Longint;[Public,Alias : 'FPC_WIDESTR_COMPARE'];
{
  Compares 2 WideStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  i,MaxI,Temp : Longint;
begin
  i:=0;
  Maxi:=Length(WideString(S1));
  temp:=Length(WideString(S2));
  If MaxI>Temp then
   MaxI:=Temp;
  Temp:=0;
  While (i<MaxI) and (Temp=0) do
   begin
     Temp:= PWord(S1+I)^ - PWord(S2+i)^;
     inc(i);
   end;
  if temp=0 then
   temp:=Length(WideString(S1))-Length(WideString(S2));
  WideStr_Compare:=Temp;
end;


Procedure WideStr_CheckZero(p : pointer);[Public,Alias : 'FPC_WIDESTR_CHECKZERO'];
begin
  if p=nil then
    HandleErrorFrame(201,get_frame);
end;


Procedure WideStr_CheckRange(len,index : longint);[Public,Alias : 'FPC_WIDESTR_RANGECHECK'];
begin
  if (index>len) or (Index<1) then
    HandleErrorFrame(201,get_frame);
end;


{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

Function Length (Const S : WideString) : Longint;
{
  Returns the length of an WideString.
  Takes in acount that zero strings are NIL;
}
begin
  If Pointer(S)=Nil then
    Length:=0
  else
    Length:=PWideRec(Pointer(S)-WideFirstOff)^.Len;
end;


Procedure SetLength (Var S : WideString; l : Longint);
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
         Pointer(s):=NewWideString(l);
       end
      else
       If (PWideRec(Pointer(S)-WideFirstOff)^.Maxlen < L) or
          (PWideRec(Pointer(S)-WideFirstOff)^.Ref <> 1) then
        begin
          { Reallocation is needed... }
          Temp:=Pointer(NewWideString(L));
          if Length(S)>0 then
            Move(Pointer(S)^,Temp^,L+L);
          ansistr_decr_ref(Pointer(S));
          Pointer(S):=Temp;
       end;
      { Force nil termination in case it gets shorter }
      PByte(Pointer(S)+l)^:=0;
      PWideRec(Pointer(S)-WideFirstOff)^.Len:=l;
    end
  else
    begin
      { Length=0 }
      if Pointer(S)<>nil then
       ansistr_decr_ref (Pointer(S));
      Pointer(S):=Nil;
    end;
end;


Procedure UniqueString(Var S : WideString); [Public,Alias : 'FPC_WIDESTR_UNIQUE'];
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}
Var
  SNew : Pointer;
begin
  If Pointer(S)=Nil then
    exit;
  if PWideRec(Pointer(S)-WideFirstOff)^.Ref<>1 then
   begin
     SNew:=NewWideString (PWideRec(Pointer(S)-WideFirstOff)^.len);
     Move (Pointer(S)^,SNew^,(PWideRec(Pointer(S)-WideFirstOff)^.len+1)*2);
     PWideRec(SNew-WideFirstOff)^.len:=PWideRec(Pointer(S)-WideFirstOff)^.len;
     ansistr_decr_ref (Pointer(S));  { Thread safe }
     Pointer(S):=SNew;
   end;
end;


Function Copy (Const S : WideString; Index,Size : Longint) : WideString;
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
     ResultAddress:=Pointer(NewWideString (Size));
     if ResultAddress<>Nil then
      begin
        Move (Pointer(Pointer(S)+index)^,ResultAddress^,Size*2);
        PWideRec(ResultAddress-WideFirstOff)^.Len:=Size;
        PWord(ResultAddress+Size*2)^:=0;
      end;
   end;
  Pointer(Copy):=ResultAddress;
end;


Function Pos (Const Substr : WideString; Const Source : WideString) : Longint;
var
  substrlen,
  maxi,
  i,j : longint;
  e   : boolean;
  S   : WideString;
  se  : Pointer;
begin
  i := 0;
  j := 0;
  substrlen:=Length(SubStr);
  maxi:=length(source)-substrlen;
  e:=(substrlen>0);
  while (e) and (i <= maxi) do
   begin
     inc (i);
{!!!:     if Source[i]=SubStr[1] then
      begin
        S:=copy(Source,i,substrlen);
        Se:=pointer(SubStr);
        if WideStr_Compare(se,Pointer(S))=0 then
         begin
           j := i;
           break;
         end;
      end;}
   end;
  pos := j;
end;


Procedure Delete (Var S : WideString; Index,Size: Longint);
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
  LS:=PWideRec(Pointer(S)-WideFirstOff)^.Len;
  if (Index<=LS) and (Size>0) then
   begin
     UniqueString (S);
     if Size+Index>LS then
      Size:=LS-Index+1;
     if Index+Size<=LS then
      begin
        Dec(Index);
        Move(PByte(Pointer(S))[Index+Size],PByte(Pointer(S))[Index],(LS-Index+1)*2);
      end;
     Setlength(s,LS-Size);
   end;
end;


Procedure Insert (Const Source : WideString; Var S : WideString; Index : Longint);
var
  Temp : WideString;
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
  Pointer(Temp) := NewWideString(Length(Source)+LS);
  SetLength(Temp,Length(Source)+LS);
  If Index>0 then
    move (Pointer(S)^,Pointer(Temp)^,Index*2);
  Move (Pointer(Source)^,PByte(Temp)[Index],Length(Source)*2);
  If (LS-Index)>0 then
    Move(PByte(Pointer(S))[Index],PByte(temp)[Length(Source)+index],(LS-Index)*2);
  S:=Temp;
end;


{!!!:Procedure SetString (Var S : WideString; Buf : PWideChar; Len : Longint);

begin
  SetLength(S,Len);
  Move (Buf[0],S[1],Len*2);
end;}


{
  $Log$
  Revision 1.3  2000-08-08 22:12:36  sg
  * Implemented WideString helper functions (but they are not tested yet
    due to the lack of full compiler support for WideString/WideChar!)

  Revision 1.2  2000/07/13 11:33:46  michael
  + removed logs
 
}
