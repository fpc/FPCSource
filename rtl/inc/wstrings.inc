{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by Florian Klaempfl,
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
  WideFirstOff = SizeOf(TWideRec)-sizeof(WideChar);


{
  Default WideChar <-> Char conversion is to only convert the
  lower 127 chars, all others are translated to spaces.

  These routines can be overwritten for the Current Locale
}

procedure Wide2AnsiMove(source:pwidechar;dest:pchar;len:longint);
var
  i : longint;
begin
  for i:=1to len do
   begin
     if word(source^)<128 then
      dest^:=char(word(source^))
     else
      dest^:=' ';
     inc(dest);
     inc(source);
   end;
end;


procedure Ansi2WideMove(source:pchar;dest:pwidechar;len:longint);
var
  i : longint;
begin
  for i:=1to len do
   begin
     if byte(source^)<128 then
      dest^:=widechar(byte(source^))
     else
      dest^:=' ';
     inc(dest);
     inc(source);
   end;
end;


Type
  TWide2AnsiMove=procedure(source:pwidechar;dest:pchar;len:longint);
  TAnsi2WideMove=procedure(source:pchar;dest:pwidechar;len:longint);
Const
  Wide2AnsiMoveProc:TWide2AnsiMove=@Wide2AnsiMove;
  Ansi2WideMoveProc:TAnsi2WideMove=@Ansi2WideMove;

(*
Procedure UniqueWideString(Var S : WideString); [Public,Alias : 'FPC_WIDESTR_UNIQUE'];
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}

begin
end;
*)


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
     inc(p,WideFirstOff);         { Points to string now }
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

  { declocked does a MT safe dec and returns true, if the counter is 0 }
  If declocked(l^) then
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
  inclocked(PWideRec(S-WideFirstOff)^.Ref);
end;


Procedure WideStr_To_ShortStr (Var S1 : ShortString;S2 : Pointer);[Public, alias: 'FPC_WIDESTR_TO_SHORTSTR'];
{
  Converts a WideString to a ShortString;
}
Var
  Size : Longint;
begin
  if S2=nil then
   S1:=''
  else
   begin
     Size:=PWideRec(S2-FirstOff)^.Len;
     If Size>high(S1) then
      Size:=high(S1);
     Wide2AnsiMoveProc(PWideChar(S2),PChar(@S1[1]),Size);
     byte(S1[0]):=Size;
   end;
end;


Procedure ShortStr_To_WideStr (Var S1 : Pointer; Const S2 : ShortString);[Public, alias: 'FPC_SHORTSTR_TO_WIDESTR'];
{
  Converts a ShortString to a WideString;
}
Var
  Size : Longint;
begin
  Size:=Length(S2);
  Setlength (WideString(S1),Size);
  if Size>0 then
   Ansi2WideMoveProc(PChar(@S2[1]),PWideChar(S1),Size);
end;


Procedure WideStr_To_AnsiStr (Var S1 : Pointer;S2 : Pointer);[Public, alias: 'FPC_WIDESTR_TO_ANSISTR'];
{
  Converts a WideString to an AnsiString
}
Var
  Size : Longint;
begin
  if s2=nil then
    s1:=nil
  else
    begin
       Size:=Length(WideString(S2));
       Setlength (AnsiString(S1),Size);
       if Size>0 then
        begin
          Wide2AnsiMoveProc(PWideChar(S2),PChar(S1),Size);
          { Terminating Zero }
          PChar(S1+Size)^:=#0;
        end;
    end;
end;


Procedure AnsiStr_To_WideStr (Var S1 : Pointer; Const S2 : Pointer);[Public, alias: 'FPC_ANSISTR_TO_WIDESTR'];
{
  Converts an AnsiString to a WideString;
}
Var
  Size : Longint;
begin
   if s2=nil then
     s1:=nil
   else
     begin
       Size:=Length(AnsiString(S2));
       Setlength (WideString(S1),Size);
       if Size>0 then
        begin
          Ansi2WideMoveProc(PChar(S2),PWideChar(S1),Size);
          { Terminating Zero }
          PWideChar(S1+Size*sizeof(WideChar))^:=#0;
        end;
     end;
end;


{ checked against the ansistring routine, 2001-05-27 (FK) }
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

{ checked against the ansistring routine, 2001-05-27 (FK) }
Procedure WideStr_Concat (S1,S2 : Pointer;var S3 : Pointer);[Public, alias: 'FPC_WIDESTR_CONCAT'];
{
  Concatenates 2 WideStrings : S1+S2.
  Result Goes to S3;
}
Var
  Size,Location : Longint;
begin
{ only assign if s1 or s2 is empty }
  if (S1=Nil) then
    WideStr_Assign(S3,S2)
  else
    if (S2=Nil) then
      WideStr_Assign(S3,S1)
  else
    begin
      { create new result }
       if S3<>nil then
         WideStr_Decr_Ref(S3);
       Size:=PWideRec(S2-WideFirstOff)^.Len;
       Location:=Length(WideString(S1));
       SetLength (WideString(S3),Size+Location);
       Move (S1^,S3^,Location*sizeof(WideChar));
       Move (S2^,(S3+location*sizeof(WideChar))^,(Size+1)*sizeof(WideChar));
    end;
end;


Procedure Char_To_WideStr(var S1 : Pointer; c : Char);[Public, alias: 'FPC_CHAR_TO_WIDESTR'];
{
  Converts a Char to a WideString;
}
begin
  Setlength (WideString(S1),1);
  PWideChar(S1)^:=c;
  { Terminating Zero }
  PWideChar(S1+sizeof(WideChar))^:=#0;
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
      l:=IndexChar(p^,-1,#0);
      Pointer(a):=NewWidestring(L);
      SetLength(A,L);
      Ansi2WideMoveProc(P,PWideChar(A),L);
    end;
end;


Procedure CharArray_To_WideStr(var a : widestring;p : pchar;l:longint);[Public,Alias : 'FPC_CHARARRAY_TO_WIDESTR'];
var
  i  : longint;
begin
  if p[0]=#0 Then
    Pointer(a):=nil
  else
    begin
      i:=IndexChar(p^,L,#0);
      Pointer(a):=NewWidestring(i);
      SetLength(a,i);
      Ansi2WideMoveProc(P,PWideChar(A),i);
    end;
end;


Function WideStr_Compare(S1,S2 : Pointer): Longint;[Public,Alias : 'FPC_WIDESTR_COMPARE'];
{
  Compares 2 WideStrings;
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
     WideStr_Compare:=0;
     exit;
   end;
  Maxi:=Length(WideString(S1));
  temp:=Length(WideString(S2));
  If MaxI>Temp then
   MaxI:=Temp;
  Temp:=CompareWord(S1^,S2^,MaxI);
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

{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : WideString; l : Longint);
{$else INTERNSETLENGTH}
Procedure WideStr_SetLength (Var S : WideString; l : Longint);[Public,Alias : 'FPC_WIDESTR_SETLENGTH'];
{$endif INTERNSETLENGTH}
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
            Move(Pointer(S)^,Temp^,L*sizeof(WideChar));
          WideStr_decr_ref(Pointer(S));
          Pointer(S):=Temp;
       end;
      { Force nil termination in case it gets shorter }
      PWideChar(Pointer(S)+l*sizeof(WideChar))^:=#0;
      PWideRec(Pointer(S)-WideFirstOff)^.Len:=l;
    end
  else
    begin
      { Length=0 }
      if Pointer(S)<>nil then
       WideStr_decr_ref (Pointer(S));
      Pointer(S):=Nil;
    end;
end;




{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

{$ifndef INTERNLENGTH}
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
{$endif INTERNLENGTH}


Procedure UniqueString(Var S : WideString); [Public,Alias : 'FPC_WIDESTR_UNIQUE'];
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
  if PWideRec(Pointer(S)-WideFirstOff)^.Ref<>1 then
   begin
     L:=PWideRec(Pointer(S)-WideFirstOff)^.len;
     SNew:=NewWideString (L);
     Move (PWideChar(S)^,SNew^,(L+1)*sizeof(WideChar));
     PWideRec(SNew-WideFirstOff)^.len:=L;
     widestr_decr_ref (Pointer(S));  { Thread safe }
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
        Move (PWideChar(S)[Index],ResultAddress^,Size*sizeof(WideChar));
        PWideRec(ResultAddress-WideFirstOff)^.Len:=Size;
        PWideChar(ResultAddress+Size*sizeof(WideChar))^:=#0;
      end;
   end;
  Pointer(Copy):=ResultAddress;
end;


Function Pos (Const Substr : WideString; Const Source : WideString) : Longint;
var
  i,MaxLen : StrLenInt;
  pc : pwidechar;
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


{ Faster version for a widechar alone }
Function Pos (c : WideChar; Const s : WideString) : Longint;
var
  i: longint;
  pc : pwidechar;
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


{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos (c : Char; Const s : WideString) : Longint;
var
  i: longint;
  wc : widechar;
  pc : pwidechar;
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
        Move(PWideChar(S)[Index+Size],PWideChar(S)[Index],(LS-Index+1)*sizeof(WideChar));
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
    move (PWideChar(S)^,PWideChar(Temp)^,Index*sizeof(WideChar));
  Move (PWideChar(Source)^,PWideChar(Temp)[Index],Length(Source)*sizeof(WideChar));
  If (LS-Index)>0 then
    Move(PWideChar(S)[Index],PWideChar(temp)[Length(Source)+index],(LS-Index)*sizeof(WideChar));
  S:=Temp;
end;


{!!!:Procedure SetString (Var S : WideString; Buf : PWideChar; Len : Longint);

begin
  SetLength(S,Len);
  Move (Buf[0],S[1],Len*2);
end;}


Function ValWideFloat(Const S : WideString; Var Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_WIDESTR'];
Var
  SS : String;
begin
  WideStr_To_ShortStr(SS,Pointer(S));
  ValWideFloat := ValFloat(SS,Code);
end;


Function ValWideUnsignedInt (Const S : WideString; Var Code : ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_WIDESTR'];
Var
  SS : ShortString;
begin
  WideStr_To_ShortStr(SS,Pointer(S));
  ValWideUnsignedInt := ValUnsignedInt(SS,Code);
end;


Function ValWideSignedInt (DestSize: longint; Const S : WideString; Var Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_WIDESTR'];
Var
  SS : ShortString;
begin
  ValWideSignedInt:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       WideStr_To_ShortStr (SS,Pointer(S));
       ValWideSignedInt := ValSignedInt(DestSize,SS,Code);
    end;
end;

Function ValWideUnsignedint64 (Const S : WideString; Var Code : ValSInt): qword; [public, alias:'FPC_VAL_QWORD_WIDESTR'];
Var
  SS : ShortString;
begin
  ValWideUnsignedInt64:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       WideStr_To_ShortStr(SS,Pointer(S));
       ValWideUnsignedInt64 := ValQWord(SS,Code);
    end;
end;


Function ValWideSignedInt64 (Const S : WideString; Var Code : ValSInt): Int64; [public, alias:'FPC_VAL_INT64_WIDESTR'];
Var
  SS : ShortString;
begin
  ValWideSignedInt64:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       WideStr_To_ShortStr (SS,Pointer(S));
       ValWideSignedInt64 := valInt64(SS,Code);
    end;
end;


procedure WideStr_Float(d : ValReal;len,fr,rt : longint;var s : WideString);[public,alias:'FPC_WIDESTR_FLOAT'];
var
  ss : shortstring;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
end;


Procedure WideStr_Cardinal(C : Cardinal;Len : Longint; Var S : WideString);[Public,Alias : 'FPC_WIDESTR_CARDINAL'];
Var
  SS : ShortString;
begin
  int_str_cardinal(C,Len,SS);
  S:=SS;
end;



Procedure WideStr_Longint(L : Longint; Len : Longint; Var S : WideString);[Public,Alias : 'FPC_WIDESTR_LONGINT'];
Var
  SS : ShortString;
begin
  int_Str_Longint (L,Len,SS);
  S:=SS;
end;



{
  $Log$
  Revision 1.10  2001-07-16 12:33:08  jonas
    * fixed wrong public alieases for val(widestring,...)

  Revision 1.9  2001/07/09 21:15:41  peter
    * Length made internal
    * Add array support for Length

  Revision 1.8  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.7  2001/05/27 14:28:03  florian
    + some procedures added

  Revision 1.6  2000/11/06 23:17:15  peter
    * removed some warnings

  Revision 1.5  2000/11/06 20:34:24  peter
    * changed ver1_0 defines to temporary defs

  Revision 1.4  2000/10/21 18:20:17  florian
    * a lot of small changes:
       - setlength is internal
       - win32 graph unit extended
       ....

  Revision 1.3  2000/08/08 22:12:36  sg
  * Implemented WideString helper functions (but they are not tested yet
    due to the lack of full compiler support for WideString/WideChar!)

  Revision 1.2  2000/07/13 11:33:46  michael
  + removed logs
}
