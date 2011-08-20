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
  setlength(fdata,maxlen);
  if high(arr)=-1 then
    exit;
  curlen:=min(high(arr)+1,maxlen);
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(fdata),0,curlen);
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
begin
  if system.length(u)=0 then
    begin
      setlength(fdata,maxlen);
      exit;
    end;
  fdata:=TAnsiCharArray(JLString(u).getBytes);
  setlength(fdata,maxlen);
  curlen:=min(high(fdata)+1,maxlen);
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
  result:=ansistring(AnsistringClass.Create(pshortstring(self)^));
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


procedure fpc_Shortstr_SetLength(var s:shortstring;len:SizeInt); compilerproc;
begin
  if len>255 then
    len:=255;
  ShortstringClass(@s).curlen:=len;
end;


procedure fpc_Char_To_ShortStr(out res : shortstring;const c : AnsiChar) compilerproc;
{
  Converts a WideChar to a ShortString;
}

begin
  setlength(res,1);
  ShortstringClass(@res).fdata[0]:=c;
end;


Function  fpc_shortstr_Copy(const s:shortstring;index:SizeInt;count:SizeInt):shortstring;compilerproc;
begin
  if count<0 then
   count:=0;
  if index>1 then
   dec(index)
  else
   index:=0;
  if index>length(s) then
   count:=0
  else
   if count>length(s)-index then
    count:=length(s)-index;
  ShortstringClass(@result).curlen:=count;
  JLSystem.ArrayCopy(JLObject(ShortstringClass(@s).fdata),index,JLObject(ShortstringClass(@result).fdata),0,count);
end;


function  fpc_char_copy(c:AnsiChar;index : SizeInt;count : SizeInt): shortstring;compilerproc;
begin
  if (index=1) and (Count>0) then
   fpc_char_Copy:=c
  else
   fpc_char_Copy:='';
end;


function upcase(const s : shortstring) : shortstring;
var
  u : unicodestring;
begin
  u:=s;
  result:=upcase(u);
end;


Function  upCase(c:Char):Char;
var
  u : unicodestring;
  s: ansistring;
begin
  u:=c;
  s:=upcase(u);
  c:=s[1];
end;


function lowercase(const s : shortstring) : shortstring;
var
  u : unicodestring;
begin
  u:=s;
  result:=lowercase(u);
end;


Function  lowerCase(c:Char):Char; overload;
var
  u : unicodestring;
  s: ansistring;
begin
  u:=c;
  s:=lowercase(u);
  c:=s[1];
end;


Function Pos (Const Substr : Shortstring; Const Source : Shortstring) : SizeInt;
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
              (ShortstringClass(@SubStr).fdata[j]=ShortstringClass(@Source).fdata[k]) do
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


{ Faster version for a char alone. Must be implemented because   }
{ pos(c: char; const s: shortstring) also exists, so otherwise   }
{ using pos(char,pchar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos (c : AnsiChar; Const s : ShortString) : SizeInt;
var
  i: SizeInt;
begin
  for i:=1 to length(s) do
   begin
     if ShortstringClass(@s).fdata[i-1]=c then
      begin
        pos:=i;
        exit;
      end;
   end;
  pos:=0;
end;


function space (b : byte): shortstring;
begin
  setlength(result,b);
  if b>0 then
    JUArrays.fill(TJByteArray(ShortstringClass(@result).fdata),0,b,ord(' '))
end;


{*****************************************************************************
                              Str() Helpers
*****************************************************************************}


procedure fpc_shortstr_SInt(v : valSInt;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_SINT']; compilerproc;
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

procedure fpc_shortstr_UInt(v : valUInt;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_UINT']; compilerproc;
begin
  int_str_unsigned(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

procedure fpc_shortstr_qword(v : qword;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_QWORD']; compilerproc;
begin
  int_str_unsigned(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


procedure fpc_shortstr_int64(v : int64;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_INT64'];  compilerproc;
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{ lie, implemented internally in the compiler }
{$define FPC_SHORTSTR_ENUM_INTERN}
