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
  for i:=1 to len do
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
  for i:=1 to len do
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
  l : Longint;
begin
  { request a multiple of 16 because the heap manager alloctes anyways chunks of 16 bytes }
  L := (Len*sizeof(WideChar)+WideRecLen+15) and (not 15);
  GetMem(P,l);
  If P<>Nil then
   begin
     PWideRec(P)^.Maxlen:=(l-WideRecLen) div sizeof(WideChar);    { Maximal length }
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
  Dec (S,WideFirstOff);
  FreeMem (S);
  S:=Nil;
end;


Procedure fpc_WideStr_Decr_Ref (Var S : Pointer);saveregisters;[Public,Alias:'FPC_WIDESTR_DECR_REF']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

{$ifdef hascompilerproc}
{ alias for internal use }
Procedure fpc_WideStr_Decr_Ref (Var S : Pointer);saveregisters;[external name 'FPC_WIDESTR_DECR_REF'];
{$endif compilerproc}

{$ifdef hascompilerproc}
Procedure fpc_WideStr_Incr_Ref (S : Pointer);saveregisters;[Public,Alias:'FPC_WIDESTR_INCR_REF']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$else}
Procedure fpc_WideStr_Incr_Ref (Var S : Pointer);saveregisters;[Public,Alias:'FPC_WIDESTR_INCR_REF'];
{$endif compilerproc}
Begin
  If S=Nil then
    exit;
  { Let's be paranoid : Constant string ??}
  If PWideRec(S-WideFirstOff)^.Ref<0 then exit;
  inclocked(PWideRec(S-WideFirstOff)^.Ref);
end;

{$ifdef hascompilerproc}
{ alias for internal use }
Procedure fpc_WideStr_Incr_Ref (S : Pointer);saveregisters;[external name 'FPC_WIDESTR_INCR_REF'];
{$endif compilerproc}

function fpc_WideStr_To_ShortStr (high_of_res: longint;const S2 : WideString): shortstring;[Public, alias: 'FPC_WIDESTR_TO_SHORTSTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a WideString to a ShortString;
}
Var
  Size : Longint;
begin
  if S2='' then
   fpc_WideStr_To_ShortStr:=''
  else
   begin
     Size:=Length(S2);
     If Size>high_of_res then
      Size:=high_of_res;
     Wide2AnsiMoveProc(PWideChar(S2),PChar(@fpc_WideStr_To_ShortStr[1]),Size);
     byte(fpc_WideStr_To_ShortStr[0]):=byte(Size);
   end;
end;


Function fpc_ShortStr_To_WideStr (Const S2 : ShortString): WideString; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a ShortString to a WideString;
}
Var
  Size : Longint;
begin
  Size:=Length(S2);
  Setlength (fpc_ShortStr_To_WideStr,Size);
  if Size>0 then
    begin
      Ansi2WideMoveProc(PChar(@S2[1]),PWideChar(Pointer(fpc_ShortStr_To_WideStr)),Size);
      { Terminating Zero }
      PWideChar(Pointer(fpc_ShortStr_To_WideStr)+Size*sizeof(WideChar))^:=#0;
    end;
end;

{ old style helper }
{$ifndef hascompilerproc}

Procedure fpc_ShortStr_To_WideStr (Var S1 : Pointer; Const S2 : ShortString);[Public, alias: 'FPC_SHORTSTR_TO_WIDESTR'];
begin
  s1 := pointer(fpc_ShortStr_To_WideStr(s2));
end;
{$endif hascompilerproc}

Function fpc_WideStr_To_AnsiStr (const S2 : WideString): AnsiString; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a WideString to an AnsiString
}
Var
  Size : Longint;
begin
  if s2='' then
    exit;
  Size:=Length(WideString(S2));
  Setlength (fpc_WideStr_To_AnsiStr,Size);
  if Size>0 then
   begin
     Wide2AnsiMoveProc(PWideChar(Pointer(S2)),PChar(Pointer(fpc_WideStr_To_AnsiStr)),Size);
     { Terminating Zero }
     PChar(Pointer(fpc_WideStr_To_AnsiStr)+Size)^:=#0;
   end;
end;

{ old style helper }
{$ifndef hascompilerproc}
Procedure fpc_WideStr_To_AnsiStr (Var S1 : Pointer;const S2 : WideString);[Public, alias: 'FPC_WIDESTR_TO_ANSISTR'];
begin
  s1 := pointer(fpc_WideStr_To_AnsiStr(s2));
end;
{$endif hascompilerproc}


Function fpc_AnsiStr_To_WideStr (Const S2 : AnsiString): WideString; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts an AnsiString to a WideString;
}
Var
  Size : Longint;
begin
   if s2='' then
     exit;
   Size:=Length(S2);
   Setlength (result,Size);
   if Size>0 then
    begin
      Ansi2WideMoveProc(PChar(S2),PWideChar(Pointer(result)),Size);
      { Terminating Zero }
      PWideChar(Pointer(result)+Size*sizeof(WideChar))^:=#0;
    end;
end;

{ compilers with widestrings should have compiler procs }
Function fpc_PWideChar_To_AnsiStr(const p : pwidechar): ansistring; compilerproc;
var
  Size : longint;
begin
  if p=nil then
   exit;
  Size := IndexWord(p^, $7fffffff, 0);
  Setlength (result,Size);
  if Size>0 then
   begin
     Wide2AnsiMoveProc(P,PChar(Pointer(result)),Size);
     { Terminating Zero }
     PChar(Pointer(result)+Size)^:=#0;
   end;
end;


Function fpc_PWideChar_To_WideStr(const p : pwidechar): widestring; compilerproc;
var
  Size : longint;
begin
  if p=nil then
   exit;
  Size := IndexWord(p^, $7fffffff, 0);
  Setlength (result,Size);
  if Size>0 then
   begin
      Move(p^,PWideChar(Pointer(result))^,Size*sizeof(WideChar));
      { Terminating Zero }
      PWideChar(Pointer(result)+Size*sizeof(WideChar))^:=#0;
   end;
end;


Function fpc_PWideChar_To_ShortStr(const p : pwidechar): shortstring; compilerproc;
var
  Size : longint;
begin
  if p=nil then
   begin
     fpc_PWideChar_To_ShortStr:='';
     exit;
   end;
  Size := IndexWord(p^, $7fffffff, 0);
  Setlength (result,Size+1);
  if Size>0 then
   begin
     If Size>255 then
      Size:=255;
     Wide2AnsiMoveProc(p,PChar(@result[1]),Size);
     byte(result[0]):=byte(Size);
   end;
end;


{ old style helper }
{$ifndef hascompilerproc}
Procedure fpc_AnsiStr_To_WideStr (Var S1 : Pointer; Const S2 : AnsiString);[Public, alias: 'FPC_ANSISTR_TO_WIDESTR'];
begin
  s1 := pointer(fpc_AnsiStr_To_WideStr(s2));
end;
{$endif hascompilerproc}


{ checked against the ansistring routine, 2001-05-27 (FK) }
Procedure fpc_WideStr_Assign (Var S1 : Pointer;S2 : Pointer);[Public,Alias:'FPC_WIDESTR_ASSIGN']; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Assigns S2 to S1 (S1:=S2), taking in account reference counts.
}
begin
  If S2<>nil then
    If PWideRec(S2-WideFirstOff)^.Ref>0 then
      Inc(PWideRec(S2-WideFirstOff)^.ref);
  { Decrease the reference count on the old S1 }
  fpc_widestr_decr_ref (S1);
  { And finally, have S1 pointing to S2 (or its copy) }
  S1:=S2;
end;

{$ifdef hascompilerproc}
{ alias for internal use }
Procedure fpc_WideStr_Assign (Var S1 : Pointer;S2 : Pointer);[external name 'FPC_WIDESTR_ASSIGN'];
{$endif hascompilerproc}

{ checked against the ansistring routine, 2001-05-27 (FK) }
{$ifdef hascompilerproc}
function fpc_WideStr_Concat (const S1,S2 : WideString): WideString; compilerproc;
var
  S3: WideString absolute result;
{$else hascompilerproc}
Procedure fpc_WideStr_Concat (S1,S2 : WideString;var S3 : WideString);[Public, alias: 'FPC_WIDESTR_CONCAT'];
{$endif hascompilerproc}
{
  Concatenates 2 WideStrings : S1+S2.
  Result Goes to S3;
}
Var
  Size,Location : Longint;
begin
{ only assign if s1 or s2 is empty }
  if (S1='') then
    S3 := S2
  else
    if (S2='') then
      S3 := S1
  else
    begin
       { create new result }
       Size:=Length(S2);
       Location:=Length(S1);
       SetLength (S3,Size+Location);
       Move (S1[1],S3[1],Location*sizeof(WideChar));
       Move (S2[1],S3[location+1],(Size+1)*sizeof(WideChar));
    end;
end;


Function fpc_Char_To_WideStr(const c : Char): WideString; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Converts a Char to a WideString;
}
begin
  if c = #0 then
    { result is automatically set to '' }
    exit;
  Setlength (fpc_Char_To_WideStr,1);
  fpc_Char_To_WideStr[1]:=c;
  { Terminating Zero }
  PWideChar(Pointer(fpc_Char_To_WideStr)+sizeof(WideChar))^:=#0;
end;

{ old style helper }
{$ifndef hascompilerproc}
Procedure fpc_Char_To_WideStr(var S1 : Pointer; c : Char);[Public, alias: 'FPC_CHAR_TO_WIDESTR'];
begin
  s1 := pointer(fpc_Char_To_WideStr(c));
end;
{$endif hascompilerproc}


Function fpc_PChar_To_WideStr(const p : pchar): WideString; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  L : Longint;
begin
  if (not assigned(p)) or (p[0]=#0) Then
    { result is automatically set to '' }
    exit;
  l:=IndexChar(p^,-1,#0);
  SetLength(fpc_PChar_To_WideStr,L);
  Ansi2WideMoveProc(P,PWideChar(Pointer(fpc_PChar_To_WideStr)),l);
end;

{ old style helper }
{$ifndef hascompilerproc}

Procedure fpc_PChar_To_WideStr(var a : WideString;p : pchar);[Public,Alias : 'FPC_PCHAR_TO_WIDESTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  pointer(a) := pointer(fpc_PChar_To_WideStr(p));
end;
{$endif hascompilerproc}

Function fpc_CharArray_To_WideStr(const arr: array of char): WideString; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  i  : longint;
begin
  if arr[0]=#0 Then
    { result is automatically set to '' }
    exit;
  i:=IndexChar(arr,high(arr)+1,#0);
  if i = -1 then
    i := high(arr)+1;
  SetLength(fpc_CharArray_To_WideStr,i);
  Ansi2WideMoveProc (pchar(@arr),PWideChar(Pointer(fpc_CharArray_To_WideStr)),i);
end;

{ old style helper }
{$ifndef hascompilerproc}
Procedure fpc_CharArray_To_WideStr(var a : WideString; p: pointer; len: longint); [Public,Alias : 'FPC_CHARARRAY_TO_WIDESTR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
var
  src: pchar;
  i: longint;
begin
  src := pchar(p);
  if src[0]=#0 Then
    begin
      pointer(a) := nil;
      exit;
    end;
  i:=IndexChar(src^,len,#0);
  if i = -1 then
    i := len;
  pointer(a) := NewWideString(i);
  Ansi2WideMoveProc (src,PWideChar(Pointer(@a[1])),i);
end;
{$endif not hascompilerproc}

{$ifdef hascompilerproc}
{ inside the compiler, the resulttype is modified to that of the actual }
{ chararray we're converting to (JM)                                    }
function fpc_widestr_to_chararray(arraysize: longint; const src: WideString): fpc_big_chararray;[public,alias: 'FPC_WIDESTR_TO_CHARARRAY']; compilerproc;
var
  len: longint;
begin
  len := length(src);
  if len > arraysize then
    len := arraysize;
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    wide2ansimoveproc(pwidechar(@src[1]),pchar(@fpc_widestr_to_chararray[0]),len);
  fillchar(fpc_widestr_to_chararray[len],arraysize-len,0);
end;
{$endif hascompilerproc}

Function fpc_WideStr_Compare(const S1,S2 : WideString): Longint;[Public,Alias : 'FPC_WIDESTR_COMPARE']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
  if pointer(S1)=pointer(S2) then
   begin
     fpc_WideStr_Compare:=0;
     exit;
   end;
  Maxi:=Length(S1);
  temp:=Length(S2);
  If MaxI>Temp then
   MaxI:=Temp;
  Temp:=CompareWord(S1[1],S2[1],MaxI);
  if temp=0 then
   temp:=Length(S1)-Length(S2);
  fpc_WideStr_Compare:=Temp;
end;


Procedure fpc_WideStr_CheckZero(p : pointer);[Public,Alias : 'FPC_WIDESTR_CHECKZERO']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  if p=nil then
    HandleErrorFrame(201,get_frame);
end;


Procedure fpc_WideStr_CheckRange(len,index : longint);[Public,Alias : 'FPC_WIDESTR_RANGECHECK']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  if (index>len) or (Index<1) then
    HandleErrorFrame(201,get_frame);
end;

{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : WideString; l : Longint);
{$else INTERNSETLENGTH}
Procedure fpc_WideStr_SetLength (Var S : WideString; l : Longint);[Public,Alias : 'FPC_WIDESTR_SETLENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif INTERNSETLENGTH}
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
Var
  Temp : Pointer;
  movelen, NewLen: longint;
begin
   if (l>0) then
    begin
      if Pointer(S)=nil then
       begin
         { Need a complete new string...}
         Pointer(s):=NewWideString(l);
       end
      else if (PWideRec(Pointer(S)-WideFirstOff)^.Ref = 1) then
        begin
          if (PWideRec(Pointer(S)-WideFirstOff)^.Maxlen < L) then
            begin
              Dec(Pointer(S),WideFirstOff);
              NewLen := (L*sizeof(WideChar)+WideRecLen+15) and (not 15);
              reallocmem(pointer(S), NewLen);
              PAnsiRec(S)^.MaxLen := (NewLen - WideRecLen) div sizeof(WideChar);
              Inc(Pointer(S), WideFirstOff);
            end;
          PWideRec(Pointer(S)-WideFirstOff)^.Len := L;
          PWord(Pointer(S)+L*sizeof(WideChar))^:=0;
        end
      else
        begin
          { Reallocation is needed... }
          Temp:=Pointer(NewWideString(L));
          if Length(S)>0 then
            begin
              if l < succ(length(s)) then
                movelen := l
              { also move terminating null }
              else movelen := succ(length(s));
              Move(Pointer(S)^,Temp^,movelen * Sizeof(WideChar));
            end;
          fpc_widestr_decr_ref(Pointer(S));
          Pointer(S):=Temp;
       end;
      { Force nil termination in case it gets shorter }
      PWord(Pointer(S)+l*sizeof(WideChar))^:=0;
      PWideRec(Pointer(S)-FirstOff)^.Len:=l;
    end
  else
    begin
      { Length=0 }
      if Pointer(S)<>nil then
       fpc_widestr_decr_ref (Pointer(S));
      Pointer(S):=Nil;
    end;
end;




{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

function WideCharToString(S : PWideChar) : AnsiString;
  begin
     result:=WideCharLenToString(s,Length(WideString(s)));
  end;

function StringToWideChar(const Src : AnsiString;Dest : PWideChar;DestSize : LongInt) : PWideChar;
  begin
     if Length(Src)<DestSize then
       Ansi2WideMoveProc(PChar(Src),Dest,Length(Src))
     else
       Ansi2WideMoveProc(PChar(Src),Dest,DestSize);
     result:=Dest;
  end;

function WideCharLenToString(S : PWideChar;Len : LongInt) : AnsiString;
  begin
     SetLength(result,Len);
     Wide2AnsiMove(S,PChar(result),Len);
  end;

procedure WideCharLenToStrVar(Src : PWideChar;Len : LongInt;var Dest : AnsiString);
  begin
     Dest:=WideCharLenToString(Src,Len);
  end;

procedure WideCharToStrVar(S : PWideChar;var Dest : AnsiString);
  begin
     Dest:=WideCharToString(S);
  end;


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


{ overloaded version of UniqueString for interface }
procedure UniqueString(Var S : WideString); [external name 'FPC_WIDESTR_UNIQUE'];

Function fpc_widestr_Unique(Var S : Pointer): Widestring; [Public,Alias : 'FPC_WIDESTR_UNIQUE']; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}
Var
  SNew : Pointer;
  L    : Longint;
begin
  pointer(result) := pointer(s);
  If Pointer(S)=Nil then
    exit;
  if PWideRec(Pointer(S)-WideFirstOff)^.Ref<>1 then
   begin
     L:=PWideRec(Pointer(S)-WideFirstOff)^.len;
     SNew:=NewWideString (L);
     Move (PWideChar(S)^,SNew^,(L+1)*sizeof(WideChar));
     PWideRec(SNew-WideFirstOff)^.len:=L;
     fpc_widestr_decr_ref (Pointer(S));  { Thread safe }
     pointer(S):=SNew;
     pointer(result):=SNew;
   end;
end;


{$ifdef interncopy}
Function Fpc_WideStr_Copy (Const S : WideString; Index,Size : Longint) : WideString;compilerproc;
{$else}
Function Copy (Const S : WideString; Index,Size : Longint) : WideString;
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
     ResultAddress:=Pointer(NewWideString (Size));
     if ResultAddress<>Nil then
      begin
        Move (PWideChar(S)[Index],ResultAddress^,Size*sizeof(WideChar));
        PWideRec(ResultAddress-WideFirstOff)^.Len:=Size;
        PWideChar(ResultAddress+Size*sizeof(WideChar))^:=#0;
      end;
   end;
{$ifdef interncopy}
  Pointer(fpc_widestr_Copy):=ResultAddress;
{$else}
  Pointer(Copy):=ResultAddress;
{$endif}
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
   exit;
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


Procedure SetString (Var S : WideString; Buf : PWideChar; Len : Longint);
var
  BufLen: longint;
begin
  SetLength(S,Len);
  If (Buf<>Nil) and (Len>0) then
    begin
      BufLen := IndexWord(Buf^, Len+1, 0);
      If (BufLen>0) and (BufLen < Len) then
        Len := BufLen;
      Move (Buf[0],S[1],Len*sizeof(WideChar));
      PWideChar(Pointer(S)+Len*sizeof(WideChar))^:=#0;
    end;
end;


Procedure SetString (Var S : WideString; Buf : PChar; Len : Longint);
var
  BufLen: longint;
begin
  SetLength(S,Len);
  If (Buf<>Nil) and (Len>0) then
    begin
      BufLen := IndexByte(Buf^, Len+1, 0);
      If (BufLen>0) and (BufLen < Len) then
        Len := BufLen;
      Ansi2WideMoveProc(Buf,PWideChar(S),Len);
      PWideChar(Pointer(S)+Len*sizeof(WideChar))^:=#0;
    end;
end;


Function fpc_Val_Real_WideStr(Const S : WideString; Var Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_WIDESTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : String;
begin
  fpc_Val_Real_WideStr := 0;
  if length(S) > 255 then
    code := 256
  else
    begin
      SS := S;
      Val(SS,fpc_Val_Real_WideStr,code);
    end;
end;


Function fpc_Val_UInt_WideStr (Const S : WideString; Var Code : ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_WIDESTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  fpc_Val_UInt_WideStr := 0;
  if length(S) > 255 then
    code := 256
  else
    begin
      SS := S;
      Val(SS,fpc_Val_UInt_WideStr,code);
    end;
end;


Function fpc_Val_SInt_WideStr (DestSize: longint; Const S : WideString; Var Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_WIDESTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  fpc_Val_SInt_WideStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
      SS := S;
      fpc_Val_SInt_WideStr := fpc_Val_SInt_ShortStr(DestSize,SS,Code);
    end;
end;

Function fpc_Val_qword_WideStr (Const S : WideString; Var Code : ValSInt): qword; [public, alias:'FPC_VAL_QWORD_WIDESTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  fpc_Val_qword_WideStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_qword_WideStr,Code);
    end;
end;


Function fpc_Val_int64_WideStr (Const S : WideString; Var Code : ValSInt): Int64; [public, alias:'FPC_VAL_INT64_WIDESTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  fpc_Val_int64_WideStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_int64_WideStr,Code);
    end;
end;


procedure fpc_WideStr_Float(d : ValReal;len,fr,rt : longint;var s : WideString);[public,alias:'FPC_WIDESTR_FLOAT']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  ss : shortstring;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
end;


Procedure fpc_WideStr_Longword(C : Longword;Len : Longint; Var S : WideString);[Public,Alias : 'FPC_WIDESTR_CARDINAL']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  str(C:Len,SS);
  S:=SS;
end;



Procedure fpc_WideStr_Longint(L : Longint; Len : Longint; Var S : WideString);[Public,Alias : 'FPC_WIDESTR_LONGINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
Var
  SS : ShortString;
begin
  Str (L:Len,SS);
  S:=SS;
end;



{
  $Log$
  Revision 1.30  2003-06-17 16:38:53  jonas
    * fpc_{ansistr|widestr}_unique is now a function so it can be used as
      compilerproc

  Revision 1.29  2003/05/01 08:05:23  florian
    * started to make the rtl 64 bit save by introducing SizeInt and SizeUInt (similar to size_t of C)

  Revision 1.28  2002/12/29 16:59:17  peter
    * implemented some more conversions

  Revision 1.27  2002/12/15 22:33:12  peter
    * SetString(WideString,[PChar|PWideChar],Len) added

  Revision 1.26  2002/12/14 19:16:45  sg
  * Ported improvements from the AnsiString equivalents to NewWideString and
    fpc_WideStr_SetLength

  Revision 1.25  2002/12/07 14:35:34  carl
    - avoid warnings (add typecast)

  Revision 1.24  2002/10/10 16:08:50  florian
    + several widestring/pwidechar related helpers added

  Revision 1.23  2002/10/02 18:21:52  peter
    * Copy() changed to internal function calling compilerprocs
    * FPC_SHORTSTR_COPY renamed to FPC_SHORTSTR_ASSIGN because of the
      new copy functions

  Revision 1.22  2002/09/26 21:50:38  florian
    + some WideString<->AnsiString conversion functions added

  Revision 1.21  2002/09/14 11:20:50  carl
    * Delphi compatibility fix (with string routines)

  Revision 1.20  2002/09/07 21:16:45  carl
    * cardinal -> longword

  Revision 1.19  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.18  2002/07/29 21:28:17  florian
    * several fixes to get further with linux/ppc system unit compilation

  Revision 1.17  2002/04/26 15:19:05  peter
    * use saveregisters for incr routines, saves also problems with
      the optimizer

  Revision 1.16  2002/04/25 20:14:57  peter
    * updated compilerprocs
    * incr ref count has now a value argument instead of var

}
