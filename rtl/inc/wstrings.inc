{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    This file implements support routines for WideStrings/Unicode with FPC

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

  @-8  : SizeInt for reference count;
  @-4  : SizeInt for size; size=number of bytes, not the number of chars. Divide or multiply
         with sizeof(WideChar) to convert. This is needed to be compatible with Delphi and
         Windows COM BSTR.
  @    : String + Terminating #0;
  Pwidechar(Widestring) is a valid typecast.
  So WS[i] is converted to the address @WS+i-1.

  Constants should be assigned a reference count of -1
  Meaning that they can't be disposed of.
}

Type
  PWideRec = ^TWideRec;
  TWideRec = Packed Record
    Ref,
    Len   : SizeInt;
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

procedure DefaultWide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
var
  i : SizeInt;
begin
  //writeln('in widetoansimove');
  setlength(dest,len);
  for i:=1 to len do
   begin
     if word(source^)<256 then
      dest[i]:=char(word(source^))
     else
      dest[i]:='?';
     //inc(dest);
     inc(source);
   end;
end;


procedure DefaultAnsi2WideMove(source:pchar;var dest:widestring;len:SizeInt);
var
  i : SizeInt;
begin
  //writeln('in ansitowidemove');
  setlength(dest,len);
  for i:=1 to len do
   begin
//     if byte(source^)<128 then
      dest[i]:=widechar(byte(source^));
//     else
//      dest^:=' ';
     //inc(dest);
     inc(source);
   end;
end;

Procedure GetWideStringManager (Var Manager : TWideStringManager);
begin
  manager:=widestringmanager;
end;


Procedure SetWideStringManager (Const New : TWideStringManager; Var Old: TWideStringManager);
begin
  Old:=widestringmanager;
  widestringmanager:=New;
end;

Procedure SetWideStringManager (Const New : TWideStringManager);
begin
  widestringmanager:=New;
end;

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


procedure WideStringError;
  begin
    HandleErrorFrame(204,get_frame);
  end;


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


Function NewWideString(Len : SizeInt) : Pointer;
{
  Allocate a new WideString on the heap.
  initialize it to zero length and reference count 1.
}
Var
  P : Pointer;
begin
{$ifdef MSWINDOWS}
  if winwidestringalloc then
    P:=SysAllocStringLen(nil,Len*sizeof(WideChar)+WideRecLen)
  else
{$endif MSWINDOWS}
    GetMem(P,Len*sizeof(WideChar)+WideRecLen);
  If P<>Nil then
    begin
     PWideRec(P)^.Len:=0;         { Initial length }
     PWideRec(P)^.Ref:=1;         { Set reference count }
     PWideRec(P)^.First:=#0;      { Terminating #0 }
     inc(p,WideFirstOff);         { Points to string now }
    end
  else
    WideStringError;
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
{$ifdef MSWINDOWS}
  if winwidestringalloc then
    SysFreeString(S)
  else
{$endif MSWINDOWS}
    FreeMem (S);
  S:=Nil;
end;


Procedure fpc_WideStr_Decr_Ref (Var S : Pointer);[Public,Alias:'FPC_WIDESTR_DECR_REF']; compilerproc;
{
  Decreases the ReferenceCount of a non constant widestring;
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
  l:=@PWIDEREC(S-WideFirstOff)^.Ref;
  If l^<0 then exit;

  { declocked does a MT safe dec and returns true, if the counter is 0 }
  If declocked(l^) then
    { Ref count dropped to zero }
    DisposeWideString (S);        { Remove...}
end;

{ alias for internal use }
Procedure fpc_WideStr_Decr_Ref (Var S : Pointer);[external name 'FPC_WIDESTR_DECR_REF'];

Procedure fpc_WideStr_Incr_Ref (S : Pointer);[Public,Alias:'FPC_WIDESTR_INCR_REF']; compilerproc;
Begin
  If S=Nil then
    exit;
  { Let's be paranoid : Constant string ??}
  If PWideRec(S-WideFirstOff)^.Ref<0 then exit;
  inclocked(PWideRec(S-WideFirstOff)^.Ref);
end;

{ alias for internal use }
Procedure fpc_WideStr_Incr_Ref (S : Pointer);[external name 'FPC_WIDESTR_INCR_REF'];

function fpc_WideStr_To_ShortStr (high_of_res: SizeInt;const S2 : WideString): shortstring;[Public, alias: 'FPC_WIDESTR_TO_SHORTSTR'];  compilerproc;
{
  Converts a WideString to a ShortString;
}
Var
  Size : SizeInt;
  temp : ansistring;
begin
  if S2='' then
   fpc_WideStr_To_ShortStr:=''
  else
   begin
     Size:=Length(S2);
     If Size>high_of_res then
       Size:=high_of_res;
     widestringmanager.Wide2AnsiMoveProc(PWideChar(S2),temp,Size);
     fpc_WideStr_To_ShortStr:=temp;
   end;

end;


Function fpc_ShortStr_To_WideStr (Const S2 : ShortString): WideString;compilerproc;
{
  Converts a ShortString to a WideString;
}
Var
  Size : SizeInt;
begin
  Size:=Length(S2);
  //Setlength (fpc_ShortStr_To_WideStr,Size);
  if Size>0 then
    begin
			widestringmanager.Ansi2WideMoveProc(PChar(@S2[1]),fpc_ShortStr_To_WideStr,Size);
      { Terminating Zero }
			PWideChar(Pointer(fpc_ShortStr_To_WideStr)+Size*sizeof(WideChar))^:=#0;
    end;
end;


Function fpc_WideStr_To_AnsiStr (const S2 : WideString): AnsiString; compilerproc;
{
  Converts a WideString to an AnsiString
}
Var
  Size : SizeInt;
begin
  if s2='' then
    exit;
  Size:=Length(WideString(S2));
//  Setlength (fpc_WideStr_To_AnsiStr,Size);
  if Size>0 then
   begin
     widestringmanager.Wide2AnsiMoveProc(PWideChar(Pointer(S2)),fpc_WideStr_To_AnsiStr,Size);
     { Terminating Zero }
//     PChar(Pointer(fpc_WideStr_To_AnsiStr)+Size)^:=#0;
   end;
end;


Function fpc_AnsiStr_To_WideStr (Const S2 : AnsiString): WideString; compilerproc;
{
  Converts an AnsiString to a WideString;
}
Var
  Size : SizeInt;
begin
   if s2='' then
     exit;
   Size:=Length(S2);
  // Setlength (result,Size);
   if Size>0 then
    begin
      widestringmanager.Ansi2WideMoveProc(PChar(S2),result,Size);
      { Terminating Zero }
    //  PWideChar(Pointer(result)+Size*sizeof(WideChar))^:=#0;
    end;
end;

{ compilers with widestrings should have compiler procs }
Function fpc_PWideChar_To_AnsiStr(const p : pwidechar): ansistring; compilerproc;
var
  Size : SizeInt;
begin
  if p=nil then
   exit;
  Size := IndexWord(p^, -1, 0);
 // Setlength (result,Size);
  if Size>0 then
   begin
     widestringmanager.Wide2AnsiMoveProc(P,result,Size);
     { Terminating Zero }
   //  PChar(Pointer(result)+Size)^:=#0;
   end;
end;


Function fpc_PWideChar_To_WideStr(const p : pwidechar): widestring; compilerproc;
var
  Size : SizeInt;
begin
  if p=nil then
   exit;
  Size := IndexWord(p^, -1, 0);
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
  Size : SizeInt;
  temp: ansistring;
begin
  if p=nil then
   begin
     fpc_PWideChar_To_ShortStr:='';
     exit;
   end;
  Size := IndexWord(p^, $7fffffff, 0);
//  Setlength (result,Size+1);
  if Size>0 then
   begin
//     If Size>255 then
//      Size:=255;
     widestringmanager.Wide2AnsiMoveProc(p,temp,Size);
//     byte(result[0]):=byte(Size);
   end;
  result := temp
end;



{ checked against the ansistring routine, 2001-05-27 (FK) }
Procedure fpc_WideStr_Assign (Var S1 : Pointer;S2 : Pointer);[Public,Alias:'FPC_WIDESTR_ASSIGN']; compilerproc;
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

{ alias for internal use }
Procedure fpc_WideStr_Assign (Var S1 : Pointer;S2 : Pointer);[external name 'FPC_WIDESTR_ASSIGN'];

{ checked against the ansistring routine, 2001-05-27 (FK) }
function fpc_WideStr_Concat (const S1,S2 : WideString): WideString; compilerproc;
var
  S3: WideString absolute result;
{
  Concatenates 2 WideStrings : S1+S2.
  Result Goes to S3;
}
Var
  Size,Location : SizeInt;
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


Function fpc_Char_To_WideStr(const c : WideChar): WideString; compilerproc;
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


Function fpc_PChar_To_WideStr(const p : pchar): WideString; compilerproc;
Var
  L : SizeInt;
begin
  if (not assigned(p)) or (p[0]=#0) Then
    { result is automatically set to '' }
    exit;
  l:=IndexChar(p^,-1,#0);
  //SetLength(fpc_PChar_To_WideStr,L);
  widestringmanager.Ansi2WideMoveProc(P,fpc_PChar_To_WideStr,l);
end;

Function fpc_CharArray_To_WideStr(const arr: array of char): WideString; compilerproc;
var
  i  : SizeInt;
begin
  if arr[0]=#0 Then
    { result is automatically set to '' }
    exit;
  i:=IndexChar(arr,high(arr)+1,#0);
  if i = -1 then
    i := high(arr)+1;
  SetLength(fpc_CharArray_To_WideStr,i);
  widestringmanager.Ansi2WideMoveProc (pchar(@arr),fpc_CharArray_To_WideStr,i);
end;

function fpc_WideCharArray_To_ShortStr(const arr: array of widechar): shortstring;[public,alias:'FPC_WIDECHARARRAY_TO_SHORTSTR']; compilerproc;
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
  index:=IndexWord(arr[0],l,0);
  if (index < 0) then
    len := l
  else
    len := index;
  widestringmanager.Wide2AnsiMoveProc (pwidechar(@arr),temp,len);
  fpc_WideCharArray_To_ShortStr := temp;
  //fpc_WideCharArray_To_ShortStr[0]:=chr(len);
end;

Function fpc_WideCharArray_To_AnsiStr(const arr: array of widechar): AnsiString; compilerproc;
var
  i  : SizeInt;
begin
  if arr[0]=#0 Then
    { result is automatically set to '' }
    exit;
  i:=IndexWord(arr,high(arr)+1,0);
  if i = -1 then
    i := high(arr)+1;
  SetLength(fpc_WideCharArray_To_AnsiStr,i);
  widestringmanager.Wide2AnsiMoveProc (pwidechar(@arr),fpc_WideCharArray_To_AnsiStr,i);
end;

Function fpc_WideCharArray_To_WideStr(const arr: array of widechar): WideString; compilerproc;
var
  i  : SizeInt;
begin
  if arr[0]=#0 Then
    { result is automatically set to '' }
    exit;
  i:=IndexWord(arr,high(arr)+1,0);
  if i = -1 then
    i := high(arr)+1;
  SetLength(fpc_WideCharArray_To_WideStr,i);
  Move(pwidechar(@arr)^, PWideChar(Pointer(@fpc_WideCharArray_To_WideStr[1]))^,i*sizeof(WideChar));
  { Terminating Zero }
  PWideChar(Pointer(@fpc_WideCharArray_To_WideStr[1])+i*sizeof(WideChar))^:=#0;
end;

{ inside the compiler, the resulttype is modified to that of the actual }
{ chararray we're converting to (JM)                                    }
function fpc_widestr_to_chararray(arraysize: SizeInt; const src: WideString): fpc_big_chararray;[public,alias: 'FPC_WIDESTR_TO_CHARARRAY']; compilerproc;
var
  len: SizeInt;
  temp: ansistring;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    widestringmanager.wide2ansimoveproc(pwidechar(@src[1]),temp,len);
  len := length(temp);
  if len > arraysize then
    len := arraysize;
  move(temp[1],fpc_widestr_to_chararray[0],len);
  fillchar(fpc_widestr_to_chararray[len],arraysize-len,0);
end;

{ inside the compiler, the resulttype is modified to that of the actual }
{ widechararray we're converting to (JM)                                }
function fpc_widestr_to_widechararray(arraysize: SizeInt; const src: WideString): fpc_big_widechararray;[public,alias: 'FPC_WIDESTR_TO_WIDECHARARRAY']; compilerproc;
var
  len: SizeInt;
begin
  len := length(src);
  if len > arraysize then
    len := arraysize;
  { make sure we don't try to access element 1 of the ansistring if it's nil }
  if len > 0 then
    move(src[1],fpc_widestr_to_widechararray[0],len*SizeOf(WideChar));
  fillchar(fpc_widestr_to_widechararray[len],(arraysize-len)*SizeOf(WideChar),0);
end;

{ inside the compiler, the resulttype is modified to that of the actual }
{ chararray we're converting to (JM)                                    }
function fpc_ansistr_to_widechararray(arraysize: SizeInt; const src: AnsiString): fpc_big_widechararray;[public,alias: 'FPC_ANSISTR_TO_WIDECHARARRAY']; compilerproc;
var
  len: SizeInt;
  temp: widestring;
begin
  len := length(src);
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    widestringmanager.ansi2widemoveproc(pchar(@src[1]),temp,len);
  len := length(temp);
  if len > arraysize then
    len := arraysize;

  move(temp[1],fpc_ansistr_to_widechararray[0],len*sizeof(widechar));
  fillchar(fpc_ansistr_to_widechararray[len],(arraysize-len)*SizeOf(WideChar),0);
end;

function fpc_shortstr_to_widechararray(arraysize: SizeInt; const src: ShortString): fpc_big_widechararray;[public,alias: 'FPC_SHORTSTR_TO_WIDECHARARRAY']; compilerproc;
var
  len: longint;
  temp : widestring;
begin
  len := length(src);
  { make sure we don't access char 1 if length is 0 (JM) }
  if len > 0 then
    widestringmanager.ansi2widemoveproc(pchar(@src[1]),temp,len);
  len := length(temp);
  if len > arraysize then
    len := arraysize;
  move(temp[1],fpc_shortstr_to_widechararray[0],len*sizeof(widechar));
  fillchar(fpc_shortstr_to_widechararray[len],(arraysize-len)*SizeOf(WideChar),0);
end;

Function fpc_WideStr_Compare(const S1,S2 : WideString): SizeInt;[Public,Alias : 'FPC_WIDESTR_COMPARE']; compilerproc;
{
  Compares 2 WideStrings;
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


Procedure fpc_WideStr_CheckZero(p : pointer);[Public,Alias : 'FPC_WIDESTR_CHECKZERO']; compilerproc;
begin
  if p=nil then
    HandleErrorFrame(201,get_frame);
end;


Procedure fpc_WideStr_CheckRange(len,index : SizeInt);[Public,Alias : 'FPC_WIDESTR_RANGECHECK']; compilerproc;
begin
  if (index>len) or (Index<1) then
    HandleErrorFrame(201,get_frame);
end;

Procedure fpc_WideStr_SetLength (Var S : WideString; l : SizeInt);[Public,Alias : 'FPC_WIDESTR_SETLENGTH']; compilerproc;
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
         Pointer(s):=NewWideString(l);
       end
      { windows doesn't support reallocing widestrings, this code
        is anyways subject to be removed because widestrings shouldn't be
        ref. counted anymore (FK) }
      else if
{$ifdef MSWINDOWS}
              not winwidestringalloc and
{$endif MSWINDOWS}
              (PWideRec(Pointer(S)-WideFirstOff)^.Ref = 1) then
        begin
          Dec(Pointer(S),WideFirstOff);
          if L*sizeof(WideChar)+WideRecLen>MemSize(Pointer(S)) then
              reallocmem(pointer(S), L*sizeof(WideChar)+WideRecLen);
          Inc(Pointer(S), WideFirstOff);
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
      PWideRec(Pointer(S)-FirstOff)^.Len:=l*sizeof(WideChar);
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

function StringToWideChar(const Src : AnsiString;Dest : PWideChar;DestSize : SizeInt) : PWideChar;
  var
    temp:widestring;
  begin
     widestringmanager.Ansi2WideMoveProc(PChar(Src),temp,Length(Src));
     if Length(temp)<DestSize then
       move(temp[1],Dest^,Length(temp)*SizeOf(WideChar))
     else
       move(temp[1],Dest^,(DestSize-1)*SizeOf(WideChar));

     Dest[DestSize-1]:=#0;

     result:=Dest;

  end;

function WideCharLenToString(S : PWideChar;Len : SizeInt) : AnsiString;
  begin
     //SetLength(result,Len);
     widestringmanager.Wide2AnsiMoveproc(S,result,Len);
  end;

procedure WideCharLenToStrVar(Src : PWideChar;Len : SizeInt;out Dest : AnsiString);
  begin
     Dest:=WideCharLenToString(Src,Len);
  end;

procedure WideCharToStrVar(S : PWideChar;out Dest : AnsiString);
  begin
     Dest:=WideCharToString(S);
  end;




Function fpc_widestr_Unique(Var S : Pointer): Pointer; [Public,Alias : 'FPC_WIDESTR_UNIQUE']; compilerproc;
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
  if PWideRec(Pointer(S)-WideFirstOff)^.Ref<>1 then
   begin
     L:=PWideRec(Pointer(S)-WideFirstOff)^.len div sizeof(WideChar);
     SNew:=NewWideString (L);
     Move (PWideChar(S)^,SNew^,(L+1)*sizeof(WideChar));
     PWideRec(SNew-WideFirstOff)^.len:=L * sizeof(WideChar);
     fpc_widestr_decr_ref (Pointer(S));  { Thread safe }
     pointer(S):=SNew;
     pointer(result):=SNew;
   end;
end;


Function Fpc_WideStr_Copy (Const S : WideString; Index,Size : SizeInt) : WideString;compilerproc;
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
        PWideRec(ResultAddress-WideFirstOff)^.Len:=Size*sizeof(WideChar);
        PWideChar(ResultAddress+Size*sizeof(WideChar))^:=#0;
      end;
   end;
  Pointer(fpc_widestr_Copy):=ResultAddress;
end;


Function Pos (Const Substr : WideString; Const Source : WideString) : SizeInt;
var
  i,MaxLen : SizeInt;
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
Function Pos (c : WideChar; Const s : WideString) : SizeInt;
var
  i: SizeInt;
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


Function Pos (c : WideChar; Const s : AnsiString) : SizeInt;
var
  i: SizeInt;
  pc : pchar;
begin
  pc:=@s[1];
  for i:=1 to length(s) do
   begin
     if widechar(pc^)=c then
      begin
        pos:=i;
        exit;
      end;
     inc(pc);
   end;
  pos:=0;
end;


Function Pos (c : AnsiString; Const s : WideString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    result:=Pos(WideString(c),s);
  end;


Function Pos (c : ShortString; Const s : WideString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    result:=Pos(WideString(c),s);
  end;


Function Pos (c : WideString; Const s : AnsiString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    result:=Pos(c,WideString(s));
  end;

{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos (c : Char; Const s : WideString) : SizeInt;
var
  i: SizeInt;
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



Procedure Delete (Var S : WideString; Index,Size: SizeInt);
Var
  LS : SizeInt;
begin
  If Length(S)=0 then
   exit;
  if index<=0 then
   exit;
  LS:=PWideRec(Pointer(S)-WideFirstOff)^.Len div sizeof(WideChar);
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


Procedure Insert (Const Source : WideString; Var S : WideString; Index : SizeInt);
var
  Temp : WideString;
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
  Pointer(Temp) := NewWideString(Length(Source)+LS);
  SetLength(Temp,Length(Source)+LS);
  If Index>0 then
    move (PWideChar(S)^,PWideChar(Temp)^,Index*sizeof(WideChar));
  Move (PWideChar(Source)^,PWideChar(Temp)[Index],Length(Source)*sizeof(WideChar));
  If (LS-Index)>0 then
    Move(PWideChar(S)[Index],PWideChar(temp)[Length(Source)+index],(LS-Index)*sizeof(WideChar));
  S:=Temp;
end;


function UpCase(const s : WideString) : WideString;
begin
  result:=widestringmanager.UpperWideStringProc(s);
end;


Procedure SetString (Out S : WideString; Buf : PWideChar; Len : SizeInt);
var
  BufLen: SizeInt;
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


Procedure SetString (Out S : WideString; Buf : PChar; Len : SizeInt);
var
  BufLen: SizeInt;
begin
  SetLength(S,Len);
  If (Buf<>Nil) and (Len>0) then
    begin
      BufLen := IndexByte(Buf^, Len+1, 0);
      If (BufLen>0) and (BufLen < Len) then
        Len := BufLen;
      widestringmanager.Ansi2WideMoveProc(Buf,S,Len);
      //PWideChar(Pointer(S)+Len*sizeof(WideChar))^:=#0;
    end;
end;


Function fpc_Val_Real_WideStr(Const S : WideString; out Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_WIDESTR']; compilerproc;
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


Function fpc_Val_UInt_WideStr (Const S : WideString; out Code : ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_WIDESTR']; compilerproc;
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


Function fpc_Val_SInt_WideStr (DestSize: SizeInt; Const S : WideString; out Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_WIDESTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_SInt_WideStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
      SS := S;
      fpc_Val_SInt_WideStr := int_Val_SInt_ShortStr(DestSize,SS,Code);
    end;
end;


{$ifndef CPU64}

Function fpc_Val_qword_WideStr (Const S : WideString; out Code : ValSInt): qword; [public, alias:'FPC_VAL_QWORD_WIDESTR']; compilerproc;
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


Function fpc_Val_int64_WideStr (Const S : WideString; out Code : ValSInt): Int64; [public, alias:'FPC_VAL_INT64_WIDESTR']; compilerproc;
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

{$endif CPU64}


procedure fpc_WideStr_Float(d : ValReal;len,fr,rt : SizeInt;out s : WideString);compilerproc;
var
  ss : shortstring;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
end;


Procedure fpc_WideStr_SInt(v : ValSint; Len : SizeInt; out S : WideString);compilerproc;
Var
  SS : ShortString;
begin
  Str (v:Len,SS);
  S:=SS;
end;


Procedure fpc_WideStr_UInt(v : ValUInt;Len : SizeInt; out S : WideString);compilerproc;
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
end;


{$ifndef CPU64}

Procedure fpc_WideStr_Int64(v : Int64; Len : SizeInt; out S : WideString);compilerproc;
Var
  SS : ShortString;
begin
  Str (v:Len,SS);
  S:=SS;
end;


Procedure fpc_WideStr_Qword(v : Qword;Len : SizeInt; out S : WideString);compilerproc;
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
end;

{$endif CPU64}

function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    if assigned(Source) then
      Result:=UnicodeToUtf8(Dest,MaxBytes,Source,IndexWord(Source^,-1,0))
    else
      Result:=0;
  end;


function UnicodeToUtf8(Dest: PChar; MaxDestBytes: SizeUInt; Source: PWideChar; SourceChars: SizeUInt): SizeUInt;
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

        if j>MaxDestBytes-1 then
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
          end;
      end;
    result:=j+1;
  end;


function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    if assigned(Source) then
      Result:=Utf8ToUnicode(Dest,MaxChars,Source,strlen(Source))
    else
      Result:=0;
  end;


function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;

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
          Dest[j]:=WideChar(w);
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


function UTF8Encode(const s : WideString) : UTF8String;
  var
    i : SizeInt;
    hs : UTF8String;
  begin
    result:='';
    if s='' then
      exit;
    SetLength(hs,length(s)*3);
    i:=UnicodeToUtf8(pchar(hs),length(hs)+1,PWideChar(s),length(s));
    if i>0 then
      begin
        SetLength(hs,i-1);
        result:=hs;
      end;
  end;


function UTF8Decode(const s : UTF8String): WideString;
  var
    i : SizeInt;
    hs : WideString;
  begin
    result:='';
    if s='' then
      exit;
    SetLength(hs,length(s));
    i:=Utf8ToUnicode(PWideChar(hs),length(hs)+1,pchar(s),length(s));
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


function WideStringToUCS4String(const s : WideString) : UCS4String;
  var
    i : SizeInt;
  begin
    setlength(result,length(s)+1);
    for i:=1 to length(s) do
      result[i-1]:=UCS4Char(s[i]);
    result[length(s)]:=UCS4Char(0);
  end;


function UCS4StringToWideString(const s : UCS4String) : WideString;
  var
    i : SizeInt;
  begin
    setlength(result,length(s)-1);
    for i:=1 to length(s)-1 do
      result[i]:=WideChar(s[i-1]);
  end;


procedure unimplementedwidestring;
  begin
    HandleErrorFrame(215,get_frame);
  end;


function GenericWideCase(const s : WideString) : WideString;
  begin
    unimplementedwidestring;
  end;


function CompareWideString(const s1, s2 : WideString) : PtrInt;
  begin
    unimplementedwidestring;
  end;


function CompareTextWideString(const s1, s2 : WideString): PtrInt;
  begin
    unimplementedwidestring;
  end;


function CharLengthPChar(const Str: PChar): PtrInt;
  begin
    unimplementedwidestring;
  end;

procedure initwidestringmanager;
  begin
    fillchar(widestringmanager,sizeof(widestringmanager),0);
    widestringmanager.Wide2AnsiMoveProc:=@defaultWide2AnsiMove;
    widestringmanager.Ansi2WideMoveProc:=@defaultAnsi2WideMove;
    widestringmanager.UpperWideStringProc:=@GenericWideCase;
    widestringmanager.LowerWideStringProc:=@GenericWideCase;
    widestringmanager.CompareWideStringProc:=@CompareWideString;
    widestringmanager.CompareTextWideStringProc:=@CompareTextWideString;
    widestringmanager.CharLengthPCharProc:=@CharLengthPChar;
  end;


