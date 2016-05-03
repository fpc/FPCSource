{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005, 2011 by Florian Klaempfl, Jonas Maebe
    members of the Free Pascal development team.

    This file implements support routines for Shortstrings with FPC/JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

constructor ShortstringClass.Create(const arr: array of ansichar; maxlen: byte);
begin
  copyFromAnsiCharArray(arr,maxlen);
end;


constructor ShortstringClass.Create(const arr: array of unicodechar; maxlen: byte);
begin
  if high(arr)=-1 then
    begin
      setlength(fdata,maxlen);
      exit;
    end;
  fdata:=TAnsiCharArray(JLString.Create(arr).getBytes);
  setlength(fdata,maxlen);
  curlen:=min(high(fdata)+1,maxlen);
end;


constructor ShortstringClass.Create(const u: unicodestring; maxlen: byte);
var
  temp: RawByteString;
begin
  if system.length(u)=0 then
    begin
      setlength(fdata,maxlen);
      exit;
    end;
  widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(JLString(u).toCharArray),temp,DefaultSystemCodePage,system.length(u));
  curlen:=min(system.length(temp),maxlen);
  fdata:=AnsistringClass(temp).fdata;
  setlength(fdata,maxlen);
end;


constructor ShortstringClass.Create(const a: ansistring; maxlen: byte);
var
  alen: jint;
begin
  setlength(fdata,maxlen);
  alen:=system.length(a);
  if alen=0 then
    exit;
  curlen:=min(alen,maxlen);
  JLSystem.ArrayCopy(JLObject(AnsistringClass(a).fdata),0,JLObject(fdata),0,curlen);
end;


constructor ShortstringClass.Create(const s: shortstring; maxlen: byte);overload;
begin
  setlength(fdata,maxlen);
  if system.length(s)=0 then
    exit;
  curlen:=min(system.length(s),maxlen);
  JLSystem.ArrayCopy(JLObject(ShortstringClass(@s).fdata),0,JLObject(fdata),0,min(system.length(s),maxlen));
end;


constructor ShortstringClass.Create(ch: ansichar; maxlen: byte);overload;
begin
  setlength(fdata,maxlen);
  fdata[0]:=ch;
  curlen:=1;
end;


constructor ShortstringClass.Create(ch: unicodechar; maxlen: byte);overload;
begin
  fdata:=TAnsiCharArray(JLString.Create(ch).getBytes);
  curlen:=min(system.length(fdata),maxlen);
  setlength(fdata,maxlen);
end;


class function ShortstringClass.CreateEmpty(maxlen: byte): ShortstringClass;
begin
  result:=ShortstringClass.Create;
  setlength(result.fdata,maxlen);
end;


class function ShortstringClass.CreateFromLiteralStringBytes(const u: unicodestring): shortstring;
var
  i: longint;
begin
  { used to construct constant shortstrings from Java string constants }
  ShortstringClass(@result).curlen:=min(system.length(u),255);
  setlength(ShortstringClass(@result).fdata,ShortstringClass(@result).curlen);
  for i:=1 to ShortstringClass(@result).curlen do
    ShortstringClass(@result).fdata[i-1]:=ansichar(ord(u[i]));
end;


procedure ShortstringClass.FpcDeepCopy(dest: ShortstringClass);
var
  destmaxlen,
  copylen: longint;
begin
  dest.curlen:=curlen;
  copylen:=system.length(fdata);
  destmaxlen:=system.length(dest.fdata);
  if copylen>destmaxlen then
    begin
      copylen:=destmaxlen;
      dest.curlen:=destmaxlen;
    end;
  if copylen>0 then
    JLSystem.ArrayCopy(JLObject(fdata),0,JLObject(dest.fdata),0,copylen);
end;


procedure ShortstringClass.copyFromAnsiCharArray(const arr: array of ansichar; maxlen: byte);
begin
  setlength(fdata,maxlen);
  if high(arr)=-1 then
    exit;
  curlen:=min(high(arr)+1,maxlen);
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(fdata),0,curlen);
end;


procedure ShortstringClass.setChar(index: jint; char: ansichar);
begin
  { index is 1-based here }

  { support accessing the length byte }
  if index=0 then
    curlen:=ord(char)
  else
    fdata[index-1]:=char;
end;


function ShortstringClass.charAt(index: jint): ansichar;
begin
  { index is already decreased by one, because same calling code is used for
    JLString.charAt() }

  { support accessing the length byte }
  if (index=-1) then
    result:=ansichar(curlen)
  else
    result:=fdata[index];
end;


function ShortstringClass.toUnicodeString: unicodestring;
begin
  result:=UnicodeString(toString);
end;


function ShortstringClass.toAnsistring: ansistring;
begin
  result:=ansistring(AnsistringClass.Create(pshortstring(self)^,DefaultSystemCodePage));
end;


function ShortstringClass.toString: JLString;
begin
  if curlen<>0 then
    result:=JLString.Create(TJByteArray(fdata),0,curlen)
  else
    result:='';
end;


function ShortstringClass.clone: JLObject;
begin
  result:=ShortstringClass.Create(pshortstring(self)^,system.length(fdata));
end;


function ShortstringClass.length: jint;
begin
  result:=curlen;
end;


class function AnsiCharArrayClass.CreateFromLiteralStringBytes(const u: unicodestring): TAnsiCharArray;
var
  i: longint;
begin
  { used to construct constant chararrays from Java string constants }
  setlength(result,length(u)+1);
  for i:=1 to length(u) do
    result[i-1]:=ansichar(ord(u[i]));
  result[length(u)]:=#0;
end;


{$define FPC_HAS_SHORTSTR_SHORTSTR_INTERN_CHARMOVE}
procedure fpc_shortstr_shortstr_intern_charmove(const src: shortstring; const srcindex: byte; var dst: shortstring; const dstindex, len: byte); {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  JLSystem.arraycopy(JLObject(ShortstringClass(@src).fdata),srcindex-1,JLObject(ShortstringClass(@dst).fdata),dstindex-1,len);
end;

{$define FPC_HAS_SHORTSTR_CHARARRAY_INTERN_CHARMOVE}
procedure fpc_shortstr_chararray_intern_charmove(const src: shortstring; out dst: array of char; const len: sizeint); {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  JLSystem.arraycopy(JLObject(ShortstringClass(@src).fdata),0,JLObject(@dst),0,len);
end;



{$define FPC_HAS_CHAR_TO_SHORTSTR}
function fpc_Char_To_ShortStr(const c : AnsiChar): shortstring; compilerproc;
{
  Converts an AnsiChar to a ShortString;
}
begin
  setlength(result,1);
  ShortstringClass(@result).fdata[0]:=c;
end;



{$define FPC_HAS_SHORTSTR_POS_SHORTSTR}
Function Pos (Const Substr : Shortstring; Const s : Shortstring) : SizeInt;
var
  i,j,k,MaxLen, SubstrLen : SizeInt;
begin
  Pos:=0;
  SubstrLen:=Length(SubStr);
  if SubstrLen>0 then
   begin
     MaxLen:=Length(s)-Length(SubStr);
     i:=0;
     while (i<=MaxLen) do
      begin
        inc(i);
        j:=0;
        k:=i-1;
        while (j<SubstrLen) and
              (ShortstringClass(@SubStr).fdata[j]=ShortstringClass(@s).fdata[k]) do
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


{$define FPC_HAS_SHORTSTR_POS_CHAR}
{Faster when looking for a single char...}
function pos(c:char;const s:shortstring):SizeInt;
var
  i : SizeInt;
begin
  for i:=0 to length(s)-1 do
   begin
     if ShortStringClass(@s).fdata[i]=c then
       begin
         pos:=i+1;
         exit;
       end;
   end;
  pos:=0;
end;


{$define FPC_UPCASE_SHORTSTR}
function upcase(const s : shortstring) : shortstring;
var
  u : unicodestring;
begin
  u:=s;
  result:=upcase(u);
end;


{$define FPC_UPCASE_CHAR}
Function  upCase(c:Char):Char;
var
  u : unicodestring;
  s: ansistring;
begin
  u:=c;
  s:=upcase(u);
  c:=s[1];
end;


{$define FPC_LOWERCASE_SHORTSTR}
function lowercase(const s : shortstring) : shortstring;
var
  u : unicodestring;
begin
  u:=s;
  result:=lowercase(u);
end;


{$define FPC_LOWERCASE_CHAR}
Function  lowerCase(c:Char):Char; overload;
var
  u : unicodestring;
  s: ansistring;
begin
  u:=c;
  s:=lowercase(u);
  c:=s[1];
end;


{ defined as external aliases to the int64 versions }
{$define FPC_HAS_QWORD_OCT_SHORTSTR}
{$define FPC_HAS_QWORD_BIN_SHORTSTR}
{$define FPC_HAS_QWORD_HEX_SHORTSTR}

{$define FPC_HAS_HEXSTR_POINTER_SHORTSTR}
function hexstr(val : pointer) : shortstring;
begin
  hexstr:=hexstr(JLObject(val).hashCode,sizeof(pointer)*2);
end;


{$define FPC_HAS_SPACE_SHORTSTR}
function space (b : byte): shortstring;
begin
  setlength(result,b);
  if b>0 then
    JUArrays.fill(TJByteArray(ShortstringClass(@result).fdata),0,b,ord(' '))
end;


{*****************************************************************************
                              Str() Helpers
*****************************************************************************}

{ this is a bit of a hack: 'public name' aliases don't work yet on the JVM
  target, so manually add the redirection from FPC_VAL_SINT_SHORTSTR
  to the fpc_Val_SInt_ShortStr compilerproc since compilerprocs are all lower
  case }
{$ifndef FPC_HAS_INT_VAL_SINT_SHORTSTR}
{$define FPC_HAS_INT_VAL_SINT_SHORTSTR}
{ we need this for fpc_Val_SInt_Ansistr and fpc_Val_SInt_WideStr because }
{ we have to pass the DestSize parameter on (JM)                         }
Function int_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; out Code: ValSInt): ValSInt; [external name 'fpc_val_sint_shortstr'];
{$endif FPC_HAS_INT_VAL_SINT_SHORTSTR}


{$define FPC_HAS_SETSTRING_SHORTSTR}
Procedure fpc_setstring_shortstr(Out S : Shortstring; Buf : PChar; Len : SizeInt); compilerproc;
begin
  If Len > High(S) then
    Len := High(S);
  SetLength(S,Len);
  If Buf<>Nil then
    begin
      JLSystem.arraycopy(JLObject(Buf),0,JLObject(ShortstringClass(@S).fdata),0,len);
    end;
end;


{$define FPC_HAS_COMPARETEXT_SHORTSTR}
function ShortCompareText(const S1, S2: shortstring): SizeInt;
var
  c1, c2: Byte;
  i: Integer;
  L1, L2, Count: SizeInt;
  P1, P2: PChar;
begin
  L1 := Length(S1);
  L2 := Length(S2);
  if L1 > L2 then
    Count := L2
  else
    Count := L1;
  i := 0;
  P1 := @ShortstringClass(@S1).fdata[0];
  P2 := @ShortstringClass(@S2).fdata[0];
  c1 := 0;
  c2 := 0;
  while i < count do
  begin
    c1 := byte(p1[i]);
    c2 := byte(p2[i]);
    if c1 <> c2 then
    begin
      if c1 in [97..122] then
        Dec(c1, 32);
      if c2 in [97..122] then
        Dec(c2, 32);
      if c1 <> c2 then
        Break;
    end;
    Inc(I);
  end;
  if i < count then
    ShortCompareText := c1 - c2
  else
    ShortCompareText := L1 - L2;
end;


{ not based on Delphi-style rtti }
{$define FPC_STR_ENUM_INTERN}
function fpc_shortstr_enum_intern(enum: JLEnum; len:sizeint;out s:shortstring): longint;
begin
  s:=enum.toString;
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

