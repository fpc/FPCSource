{
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

{$define FPC_HAS_TRANSLATEPLACEHOLDERCP}
function TranslatePlaceholderCP(cp: TSystemCodePage): TSystemCodePage; {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  TranslatePlaceholderCP:=cp;
  case cp of
    CP_OEMCP,
    CP_ACP:
      TranslatePlaceholderCP:=DefaultSystemCodePage;
  end;
end;


constructor AnsistringClass.Create(len: longint; cp: TSystemCodePage);
begin
  fElementSize:=1;
  { +1 for terminating #0 }
  setlength(fdata,len+1);
  fCodePage:=cp;
end;

constructor AnsistringClass.Create(const arr: array of ansichar; length: longint; cp: TSystemCodePage);
begin
  fElementSize:=1;
  fCodePage:=cp;
  { make explicit copy so that changing the array afterwards doesn't change
    the string }
  if length=0 then
    begin
      { terminating #0 }
      setlength(fdata,1);
      exit;
    end;
  setlength(fdata,length+1);
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(fdata),0,length);
  // last char is already #0 because of setlength
end;


constructor AnsistringClass.Create(const arr: array of unicodechar; cp: TSystemCodePage);
var
  temp: RawByteString;
begin
  fElementSize:=1;
  fCodePage:=cp;
  if high(arr)=-1 then
    begin
      { terminating #0 }
      setlength(fdata,1);
      exit;
    end;
  widestringmanager.Unicode2AnsiMoveProc(punicodechar(@arr),temp,cp,system.length(arr));
  fdata:=AnsistringClass(temp).fdata;
  // last char is already #0 because of Unicode2AnsiMoveProc()
end;


constructor AnsistringClass.Create(const u: unicodestring; cp: TSystemCodePage);
var
  temp: RawByteString;
begin
  fElementSize:=1;
  fCodePage:=cp;
  if system.length(u)=0 then
    begin
      { terminating #0 }
      setlength(fdata,1);
      exit;
    end;
  widestringmanager.Unicode2AnsiMoveProc(punicodechar(JLString(u).toCharArray),temp,cp,system.length(u));
  fdata:=AnsistringClass(temp).fdata;
  // last char is already #0 because of Unicode2AnsiMoveProc()
end;


constructor AnsistringClass.Create(const u: unicodestring);
begin
  { for use in Java code }
  Create(u,DefaultSystemCodePage);
end;


constructor AnsistringClass.Create(const a: RawByteString; cp: TSystemCodePage);
begin
  Create(AnsistringClass(a).fdata,system.length(AnsistringClass(a).fdata)-1,cp);
end;


constructor AnsistringClass.Create(const s: shortstring; cp: TSystemCodePage);
begin
  Create(ShortstringClass(@s).fdata,system.length(s),cp);
end;


constructor AnsistringClass.Create(ch: ansichar; cp: TSystemCodePage);
var
  arr: array[0..0] of ansichar;
begin
  fElementSize:=1;
  fCodePage:=cp;
  setlength(fdata,2);
  fdata[0]:=ch;
  // last char is already #0 because of setlength
end;


constructor AnsistringClass.Create(ch: unicodechar; cp: TSystemCodePage);
var
  temp: RawByteString;
  arr: array[0..0] of unicodechar;
begin
  fElementSize:=1;
  fCodePage:=cp;
  arr[0]:=ch;
  widestringmanager.Unicode2AnsiMoveProc(punicodechar(@arr),temp,cp,system.length(arr));
  fdata:=AnsistringClass(temp).fdata;
end;


class function AnsistringClass.CreateFromLiteralStringBytes(const u: unicodestring; cp: TSystemCodePage): RawByteString;
var
  res: AnsistringClass;
  i: longint;
begin
  { used to construct constant ansistrings from Java string constants }
  res:=AnsistringClass.Create(system.length(u),cp);
  for i:=1 to system.length(u) do
    res.fdata[i-1]:=ansichar(ord(u[i]));
  result:=ansistring(res);
end;


function AnsistringClass.charAt(index: jint): ansichar;
begin
  { index is already decreased by one, because same calling code is used for
    JLString.charAt() }
  result:=fdata[index];
end;


function AnsistringClass.toUnicodeString: unicodestring;
begin
  widestringmanager.Ansi2UnicodeMoveProc(pchar(fdata),TranslatePlaceholderCP(fCodePage),result,system.length(fdata)-1);
end;


function AnsistringClass.toShortstring(maxlen: byte): shortstring;
begin
  ShortstringClass(@result).copyFromAnsiCharArray(fData,maxlen);
end;


function AnsistringClass.toString: JLString;
begin
  result:=JLString(toUnicodeString);
end;

(*
function AnsistringClass.concat(const a: ansistring): ansistring;
var
  newdata: array of ansichar;
  addlen: sizeint;
begin
  addlen:=length(a);
  thislen:=this.length;
  setlength(newdata,addlen+thislen);
  if thislen>0 then
    JLSystem.ArrayCopy(JLObject(fdata),0,JLObject(newdata),0,thislen);
  if addlen>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(a).fdata),0,JLObject(newdata),thislen,addlen);
end;


procedure AnsistringClass.concatmultiple(const arr: array of ansistring): ansistring;
  Var
    i  : longint;
    size, newsize : sizeint;
    curlen, addlen : sizeint
    newdata: array of ansichar;
  begin
    { First calculate size of the result so we can allocate an array of
      the right size }
    NewSize:=0;
    for i:=low(arr) to high(arr) do
      inc(newsize,length(arr[i]));
    setlength(newdata,newsize);
    curlen
    for i:=low(arr) to high(arr) do
      begin
        if length(arr[i])>0 then
          sb.append(arr[i]);
      end;
    DestS:=sb.toString;
end;
*)

function AnsiStringClass.length: jint;
begin
  result:=system.length(fdata)-1;
end;


function AnsistringClass.codePage: TSystemCodePage;
begin
  result:=fCodePage;
end;


function AnsistringClass.elementSize: Word;
begin
  result:=fElementSize;
end;


class function AnsistringClass.internChars(const a: Ansistring): TAnsiCharArray;
begin
  if a<>'' then
    result:=AnsistringClass(a).fdata
  else
    { empty pchar: array with one element that is #0 }
    setlength(result,1);
end;


{****************************************************************************
                    Internal functions, not in interface.
****************************************************************************}

{$ifndef FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}
{$define FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}
procedure fpc_pchar_ansistr_intern_charmove(const src: pchar; const srcindex: sizeint; var dst: rawbytestring; const dstindex, len: sizeint); {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif} {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  JLSystem.arraycopy(JLObject(src),srcindex,JLObject(AnsistringClass(dst).fdata),dstindex,len);
end;
{$endif FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}


{$ifndef FPC_HAS_PCHAR_PCHAR_INTERN_CHARMOVE}
{$define FPC_HAS_PCHAR_PCHAR_INTERN_CHARMOVE}
procedure fpc_pchar_pchar_intern_charmove(const src: pchar; const srcindex: sizeint; const dst: pchar; const dstindex, len: sizeint); {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif} {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  JLSystem.arraycopy(JLObject(src),srcindex,JLObject(dst),dstindex,len);
end;
{$endif FPC_HAS_PCHAR_PCHAR_INTERN_CHARMOVE}


{$ifndef FPC_HAS_SHORTSTR_ANSISTR_INTERN_CHARMOVE}
{$define FPC_HAS_SHORTSTR_ANSISTR_INTERN_CHARMOVE}
procedure fpc_shortstr_ansistr_intern_charmove(const src: shortstring; const srcindex: sizeint; var dst: rawbytestring; const dstindex, len: sizeint); {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif} {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  JLSystem.arraycopy(JLObject(ShortStringClass(@src).fdata),srcindex-1,JLObject(AnsistringClass(dst).fdata),dstindex,len);
end;
{$endif FPC_HAS_SHORTSTR_ANSISTR_INTERN_CHARMOVE}


{$define FPC_HAS_NEWANSISTR}
Function NewAnsiString(Len : SizeInt) : Pointer;
{
  Allocate a new AnsiString on the heap.
  initialize it to zero length and reference count 1.
}
begin
  result:=AnsistringClass.Create(len,DefaultSystemCodePage);
end;

{ not required }
{$define FPC_SYSTEM_HAS_ANSISTR_DECR_REF}
{$define FPC_SYSTEM_HAS_ANSISTR_INCR_REF}
{$define FPC_HAS_ANSISTR_ASSIGN}


{$ifndef FPC_HAS_ANSISTR_CONCAT_COMPLEX}
{$define FPC_HAS_ANSISTR_CONCAT_COMPLEX}
{ keeps implicit try..finally block out from primary control flow }
procedure ansistr_concat_complex(var DestS: RawByteString; const S1,S2: RawByteString; cp: TSystemCodePage);
var
  U: UnicodeString;
begin
  U:=UnicodeString(S1)+UnicodeString(S2);
  widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(JLString(U).toCharArray),DestS,cp,Length(U));
end;
{$endif FPC_HAS_ANSISTR_CONCAT_COMPLEX}


{$define FPC_HAS_ANSISTR_TO_ANSISTR}
Function fpc_AnsiStr_To_AnsiStr (const S : RawByteString;cp : TSystemCodePage): RawByteString; compilerproc;
{
  Converts an AnsiString to an AnsiString taking code pages into care
}
Var
  Size : SizeInt;
  temp : UnicodeString;
  orgcp: TSystemCodePage;
begin
  result:='';
  Size:=Length(S);
  if Size>0 then
    begin
      cp:=TranslatePlaceholderCP(cp);
      orgcp:=TranslatePlaceholderCP(StringCodePage(S));
      if (orgcp=cp) or (orgcp=CP_NONE) then
        begin
          result:=RawByteString(AnsistringClass.Create(S,cp));
        end
      else
        begin
          temp:=UnicodeString(S);
          Size:=Length(temp);
          widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(JLString(temp).toCharArray),result,cp,Size);
        end;
    end;
end;

Function fpc_AnsiStr_To_AnsiStr (const S : RawByteString;cp : TSystemCodePage): RawByteString; [external name 'fpc_ansistr_to_ansistr'];

{$define FPC_HAS_ANSISTR_CONCAT_MULTI}
procedure fpc_AnsiStr_Concat_multi (var DestS:RawByteString;const sarr:array of RawByteString{$ifdef FPC_HAS_CPSTRING};cp : TSystemCodePage{$endif FPC_HAS_CPSTRING}); compilerproc;
Var
  lowstart,
  nonemptystart,
  i           : Longint;
  p           : pointer;
  Size,NewLen,
  OldDestLen  : SizeInt;
  destcopy    : RawByteString;
  U           : UnicodeString;
  DestCP      : TSystemCodePage;
  tmpCP       : TSystemCodePage;
  sameCP      : Boolean;
begin
  if high(sarr)=0 then
    begin
      DestS:='';
      exit;
    end;
{$ifdef FPC_HAS_CPSTRING}
  DestCP:=cp;
  if DestCp=CP_NONE then
    DestCP:=DefaultSystemCodePage;
{$else FPC_HAS_CPSTRING}
  DestCP:=StringCodePage(DestS);
{$endif FPC_HAS_CPSTRING}
  lowstart:=low(sarr);
  { skip empty strings }
  while (lowstart<=high(sarr)) and
        (sarr[lowstart]='') do
    inc(lowstart);
  if lowstart>high(sarr) then
    begin
      DestS:=''; { All source strings empty }
      exit;
    end;
  DestCP:=TranslatePlaceholderCP(DestCP);
  sameCP:=true;
  tmpCP:=TranslatePlaceholderCP(StringCodePage(sarr[lowstart]));
  for i:=lowstart+1 to high(sarr) do
    begin
      { ignore the code page of empty strings, it will always be
        DefaultSystemCodePage but it doesn't matter for the outcome }
      if (sarr[i]<>'') and
         (tmpCP<>TranslatePlaceholderCP(StringCodePage(sarr[i]))) then
        begin
          sameCP:=false;
          break;
        end;
    end;
  if not sameCP then
    begin
      U:='';
      for i:=lowstart to high(sarr) do
        if sarr[i]<>'' then
          U:=U+UnicodeString(sarr[i]);

      DestS:='';
      widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(JLString(U).toCharArray),DestS,DestCP,Length(U));
      exit;
    end;
  {$ifdef FPC_HAS_CPSTRING}
    { if the result is rawbytestring and all strings have the same code page,
      keep that code page }
    if cp=CP_NONE then
      DestCP:=tmpCP;
  {$endif FPC_HAS_CPSTRING}

  nonemptystart:=lowstart;
  { Check for another reuse, then we can't use
    the append optimization }
  if DestS<>'' then
    begin
      if Pointer(DestS)=Pointer(sarr[lowstart]) then
        inc(lowstart);
      for i:=lowstart to high(sarr) do
        begin
          if Pointer(DestS)=Pointer(sarr[i]) then
            begin
              { if DestS is used somewhere in the middle of the expression,
                we need to make sure the original string still exists after
                we empty/modify DestS -- not necessary on JVM platform, ansistrings
                are not explicitly refrence counted there }
              lowstart:=nonemptystart;
              break;
            end;
        end;
    end;
  { Start with empty DestS if we start with concatting
    the first (non-empty) array element }
  if lowstart=nonemptystart then
    DestS:='';
  OldDestLen:=length(DestS);
  { Calculate size of the result so we can do
    a single call to SetLength() }
  NewLen:=0;
  for i:=nonemptystart to high(sarr) do
    inc(NewLen,length(sarr[i]));
  SetLength(DestS,NewLen);
  { Concat all strings, except the string we already
    copied in DestS }
  NewLen:=OldDestLen;
  for i:=lowstart to high(sarr) do
    begin
      p:=pointer(sarr[i]);
      if assigned(p) then
        begin
          Size:=length(ansistring(p));
          fpc_pchar_pchar_intern_charmove(pchar(ansistring(p)),0,pchar(DestS),NewLen,Size+1);
          inc(NewLen,size);
        end;
    end;
  if NewLen<>0 then
    begin
      SetCodePage(DestS,tmpCP,False);
      SetCodePage(DestS,DestCP,True);
    end;
end;


{$define FPC_HAS_ANSISTR_TO_SHORTSTR}
procedure fpc_AnsiStr_To_ShortStr (out res: shortstring; const S2 : RawByteString);[Public, alias: 'FPC_ANSISTR_TO_SHORTSTR'];  compilerproc;
{
  Converts a AnsiString to a ShortString;
}
Var
  Size : SizeInt;
begin
  if S2='' then
   res:=''
  else
   begin
     Size:=Length(S2);
     If Size>high(res) then
      Size:=high(res);
     if Size>0 then
       JLSystem.ArrayCopy(JLObject(AnsistringClass(S2).fdata),0,JLObject(ShortstringClass(@res).fdata),0,Size);
     setlength(res,size);
   end;
end;


{$define FPC_HAS_PCHAR_TO_ANSISTR}
Function fpc_PChar_To_AnsiStr(const p : PAnsiChar{$ifdef FPC_HAS_CPSTRING};cp : TSystemCodePage{$endif FPC_HAS_CPSTRING}): RawByteString; compilerproc;
Var
  L : SizeInt;
{$ifndef FPC_HAS_CPSTRING}
  cp : TSystemCodePage;
{$endif FPC_HAS_CPSTRING}
begin
  if (not assigned(p)) or (p[0]=#0) Then
    L := 0
  else
    L:=IndexChar(Arr1jbyte(p),-1,#0);
  SetLength(fpc_PChar_To_AnsiStr,L);
  if L > 0 then
    begin
{$ifdef FPC_HAS_CPSTRING}
      cp:=TranslatePlaceholderCP(cp);
{$else FPC_HAS_CPSTRING}
      cp:=DefaultSystemCodePage;
{$endif FPC_HAS_CPSTRING}
      fpc_pchar_ansistr_intern_charmove(p,0,fpc_PChar_To_AnsiStr,0,L);
      SetCodePage(fpc_PChar_To_AnsiStr,cp,False);
    end;
end;

{$define FPC_HAS_ANSISTR_TO_CHARARRAY}
procedure  fpc_ansistr_to_chararray(out res: array of AnsiChar; const src: RawByteString); compilerproc;
var
  len: longint;
begin
  len:=length(src);
  if len>length(res) then
    len:=length(res);
  { make sure we don't try to access element 1 of the ansistring if it's nil }
  if len>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(src).fdata),0,JLObject(@res),0,len);
  if len<=high(res) then
    JUArrays.fill(TJByteArray(@res),len,high(res),0);
end;


function fpc_ansistr_setchar(const s: RawByteString; const index: longint; const ch: ansichar): RawByteString; compilerproc;
var
  res: AnsistringClass;
begin
  res:=AnsistringClass.Create(s,AnsistringClass(s).fCodePage);
  res.fdata[index-1]:=ch;
  result:=Ansistring(res);
end;


{$define FPC_HAS_ANSISTR_COMPARE}
Function fpc_AnsiStr_Compare(const S1,S2 : RawByteString): SizeInt;[Public,Alias : 'FPC_ANSISTR_COMPARE'];  compilerproc;
{
  Compares 2 AnsiStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  MaxI,Temp, i : SizeInt;
  cp1,cp2 : TSystemCodePage;
  r1,r2 : RawByteString;
begin
  if JLObject(S1)=JLObject(S2) then
    begin
      result:=0;
      exit;
    end;
  if (pointer(S1)=nil) then
    begin
      result:=-Length(S2);
      exit;
    end;
  if (pointer(S2)=nil) then
    begin
      result:=Length(S1);
      exit;
    end;
  cp1:=TranslatePlaceholderCP(StringCodePage(S1));
  cp2:=TranslatePlaceholderCP(StringCodePage(S2));
  if cp1=cp2 then
    begin
      Maxi:=Length(S1);
      temp:=Length(S2);
      If MaxI>Temp then
        MaxI:=Temp;
      for i:=0 to MaxI-1 do
        begin
          result:=ord(AnsistringClass(S1).fdata[i])-ord(AnsistringClass(S2).fdata[i]);
          if result<>0 then
            exit;
        end;
      result:=Length(S1)-Length(S2);
    end
  else
    begin
      r1:=S1;
      r2:=S2;
      //convert them to utf8 then compare
      SetCodePage(r1,65001);
      SetCodePage(r2,65001);
      Result:=fpc_AnsiStr_Compare(r1,r2);
    end;
end;


{$define FPC_HAS_ANSISTR_COMPARE_EQUAL}
Function fpc_AnsiStr_Compare_equal(const S1,S2 : RawByteString): SizeInt; compilerproc;
{
  Compares 2 AnsiStrings for equality/inequality only;
  The result is
   0 if S1=S2
   <>0 if S1<>S2
}
Var
  MaxI,Temp : SizeInt;
  cp1,cp2 : TSystemCodePage;
  r1,r2 : RawByteString;
begin
  if JLObject(S1)=JLObject(S2) then
    begin
      result:=0;
      exit;
    end;
  { don't compare strings if one of them is empty }
  if (length(S1)=0) then
    begin
      { in the JVM, one string may be nil and the other may be empty -> the jlobject()
        equals check may have failed even if both strings are technically empty }
      result:=ord(length(S2)<>0);
      exit;
    end;
  if (length(S2)=0) then
    begin
      { length(S1)<>0, we checked that above }
      result:=1;
      exit;
    end;
  cp1:=TranslatePlaceholderCP(StringCodePage(S1));
  cp2:=TranslatePlaceholderCP(StringCodePage(S2));
  if cp1=cp2 then
    begin
      r1:=s1;
      r2:=s2;
    end
  else
    begin
      r1:=S1;
      r2:=S2;
      //convert them to utf8 then compare
      SetCodePage(r1,65001);
      SetCodePage(r2,65001);
    end;
  result:=ord(not JUArrays.equals(TJByteArray(AnsistringClass(r1).fdata),TJByteArray(AnsistringClass(r2).fdata)))
end;


{ not required, the JVM does the range checking for us }
{$define FPC_HAS_ANSISTR_RANGECHECK}


{$define FPC_HAS_ANSISTR_SETLENGTH}
Procedure fpc_AnsiStr_SetLength (Var S : RawByteString; l : SizeInt{$ifdef FPC_HAS_CPSTRING};cp : TSystemCodePage{$endif FPC_HAS_CPSTRING});[Public,Alias : 'FPC_ANSISTR_SETLENGTH'];  compilerproc;
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
var
  oldlen: longint;
  result: RawByteString;
begin
  cp:=TranslatePlaceholderCP(cp);
  { no explicit reference counting possible -> can't reuse S because we don't
    know how many references exist to it }
  result:=RawByteString(AnsistringClass.Create(l,cp));
  oldlen:=length(s);
  if l>oldlen then
    l:=oldlen;
  if l>0 then
    JLSystem.ArrayCopy(JLObject(AnsistringClass(S).fdata),0,JLObject(AnsistringClass(result).fdata),0,l);
  S:=result;
end;

{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

{ lie, not needed }
{$define FPC_SYSTEM_HAS_TRUELY_ANSISTR_UNIQUE}

{ can't implement reference counting since no control over what javacc-compiled
  code does with ansistrings -> always create a copy }
{$define FPC_SYSTEM_HAS_ANSISTR_UNIQUE}
procedure FPC_ANSISTR_UNIQUE(var s: AnsiString); inline;
begin
  s:=ansistring(AnsistringClass.Create(s,AnsiStringClass(s).fCodePage));
end;


{$define FPC_HAS_ANSISTR_COPY}
Function Fpc_Ansistr_Copy(Const S : RawByteString; Index,Size : SizeInt): RawByteString;compilerproc;
var
  res: AnsistringClass;
begin
  result:='';
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
     res:=AnsistringClass.Create;
     AnsistringClass(res).fcodepage:=AnsistringClass(S).fcodepage;
     { +1 for terminating #0 }
     setlength(res.fdata,size+1);
     JLSystem.ArrayCopy(JLObject(AnsistringClass(S).fdata),index,JLObject(res.fdata),0,size);
     result:=ansistring(res);
   end;
end;


{$define FPC_HAS_POS_SHORTSTR_ANSISTR}
Function Pos(Const Substr : ShortString; Const Source : RawByteString) : SizeInt;
var
  i,j,k,MaxLen, SubstrLen : SizeInt;
begin
  Pos:=0;
  SubstrLen:=Length(SubStr);
  if SubstrLen>0 then
   begin
     MaxLen:=Length(source)-Length(SubStr);
     i:=0;
     while (i<=MaxLen) do
      begin
        inc(i);
        j:=0;
        k:=i-1;
        while (j<SubstrLen) and
              (ShortStringClass(@SubStr).fdata[j]=AnsistringClass(Source).fdata[k]) do
          begin
            inc(j);
            inc(k);
          end;
        if (j=SubstrLen) then
         begin
           Pos:=i;
           exit;
         end;
      end;
   end;
end;


{$define FPC_HAS_POS_ANSISTR_ANSISTR}
Function Pos(Const Substr : RawByteString; Const Source : RawByteString) : SizeInt;
var
  i,j,k,MaxLen, SubstrLen : SizeInt;
begin
  Pos:=0;
  SubstrLen:=Length(SubStr);
  if SubstrLen>0 then
   begin
     MaxLen:=Length(source)-Length(SubStr);
     i:=0;
     while (i<=MaxLen) do
      begin
        inc(i);
        j:=0;
        k:=i-1;
        while (j<SubstrLen) and
              (AnsistringClass(SubStr).fdata[j]=AnsistringClass(Source).fdata[k]) do
          begin
            inc(j);
            inc(k);
          end;
        if (j=SubstrLen) then
         begin
           Pos:=i;
           exit;
         end;
      end;
   end;
end;


{$define FPC_HAS_POS_ANSICHAR_ANSISTR}
{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos(c : AnsiChar; Const s : RawByteString) : SizeInt;
var
  i: SizeInt;
begin
  for i:=1 to length(s) do
   begin
     if AnsistringClass(s).fdata[i-1]=c then
      begin
        pos:=i;
        exit;
      end;
   end;
  pos:=0;
end;


{$define FPC_HAS_ANSISTR_OF_CHAR}
Function StringOfChar(c : Ansichar;l : SizeInt) : AnsiString;
begin
  SetLength(StringOfChar,l);
  FillChar(AnsistringClass(result).fdata,l,c);
end;


{$define FPC_HAS_UPCASE_ANSISTR}
function upcase(const s : ansistring) : ansistring;
var
  u : unicodestring;
begin
  u:=s;
  result:=upcase(u);
end;


{$define FPC_HAS_LOWERCASE_ANSISTR}
function lowercase(const s : ansistring) : ansistring;
var
  u : unicodestring;
begin
  u:=s;
  result:=lowercase(u);
end;

{$define FPC_HAS_ANSISTR_STRINGCODEPAGE}
function StringCodePage(const S: RawByteString): TSystemCodePage; overload;
  begin
    if assigned(pointer(S)) then
       Result:=AnsistringClass(S).fCodePage
    else
      Result:=DefaultSystemCodePage;
  end;


{$define FPC_HAS_ANSISTR_STRINGELEMENTSIZE}
function StringElementSize(const S: RawByteString): Word; overload;
  begin
    if assigned(Pointer(S)) then
      Result:=AnsistringClass(S).fElementSize
    else
      Result:=SizeOf(AnsiChar);
  end;


{$define FPC_HAS_ANSISTR_STRINGREFCOUNT}
function StringRefCount(const S: RawByteString): SizeInt; overload;
  begin
    if assigned(Pointer(S)) then
      Result:=1
    else
      Result:=0;
  end;


{$define FPC_HAS_ANSISTR_SETCODEPAGE}
procedure SetCodePage(var s : RawByteString; CodePage : TSystemCodePage; Convert : Boolean = True);
  begin
    if not assigned(Pointer(S)) or (StringCodePage(S)=CodePage) then
      exit
    else if (AnsistringClass(S).length<>0) and
        Convert then
      begin
        s:=fpc_AnsiStr_To_AnsiStr(s,CodePage);
      end
    else
      begin
        UniqueString(s);
        AnsistringClass(S).fCodePage:=CodePage;
      end;
  end;


