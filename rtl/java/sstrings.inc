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
begin
  { should only be called for shortstrings of the same maximum length }
  dest.curlen:=curlen;
  JLSystem.ArrayCopy(JLObject(fdata),0,JLObject(dest.fdata),0,system.length(fdata));
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
  result:=UnicodeString(JLString.Create(TJByteArray(fdata)));
end;


function ShortstringClass.toAnsistring: ansistring;
begin
  result:=ansistring(AnsistringClass.Create(pshortstring(self)^));
end;


function ShortstringClass.toString: JLString;
begin
  if curlen<>0 then
    result:=JLString.Create(TJByteArray(fdata),0,curlen-1)
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


class function AnsiCharArrayClass.CreateFromLiteralStringBytes(const u: unicodestring; maxlen: byte): TAnsiCharArray;
var
  i: longint;
begin
  { used to construct constant chararrays from Java string constants }
  setlength(result,system.length(u));
  for i:=1 to system.length(u) do
    result[i-1]:=ansichar(ord(u[i]));
end;


procedure fpc_Shortstr_SetLength(var s:shortstring;len:SizeInt); compilerproc;
begin
  if len>255 then
    len:=255;
  ShortstringClass(@s).curlen:=len;
end;


procedure fpc_shortstr_to_shortstr(out res:shortstring; const sstr: shortstring); compilerproc;
var
  len: longint;
begin
  len:=length(sstr);
  if len>high(res) then
    len:=high(res);
  ShortstringClass(@res).curlen:=len;
  JLSystem.ArrayCopy(JLObject(ShortstringClass(@sstr).fdata),0,JLObject(ShortstringClass(@res).fdata),0,len);
end;


procedure fpc_shortstr_concat(var dests:shortstring;const s1,s2:shortstring);compilerproc;
var
  tmpres: ShortstringClass;
  s1l, s2l: longint;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if (s1l+s2l)>high(dests) then
    begin
      if s1l>high(dests) then
        s1l:=high(dests);
      s2l:=high(dests)-s1l;
    end;
  if ShortstringClass(@dests)=ShortstringClass(@s1) then
    JLSystem.ArrayCopy(JLObject(ShortstringClass(@s2).fdata),0,JLObject(ShortstringClass(@dests).fdata),s1l,s2l)
  else if ShortstringClass(@dests)=ShortstringClass(@s2) then
    begin
      JLSystem.ArrayCopy(JLObject(ShortstringClass(@dests).fdata),0,JLObject(ShortstringClass(@dests).fdata),s1l,s2l);
      JLSystem.ArrayCopy(JLObject(ShortstringClass(@s1).fdata),0,JLObject(ShortstringClass(@dests).fdata),0,s1l);
    end
  else
    begin
      JLSystem.ArrayCopy(JLObject(ShortstringClass(@s1).fdata),0,JLObject(ShortstringClass(@dests).fdata),0,s1l);
      JLSystem.ArrayCopy(JLObject(ShortstringClass(@s2).fdata),0,JLObject(ShortstringClass(@dests).fdata),s1l,s2l)
    end;
  ShortstringClass(@dests).curlen:=s1l+s2l;
end;


procedure fpc_shortstr_concat_multi(var dests:shortstring;const sarr:array of ShortstringClass);compilerproc;
var
  s2l : byte;
  LowStart,i,
  Len : longint;
  needtemp : boolean;
  tmpstr  : shortstring;
  p,pdest  : ShortstringClass;
begin
  if high(sarr)=0 then
    begin
      DestS:='';
      exit;
    end;
  lowstart:=low(sarr);
  if ShortstringClass(@DestS)=sarr[lowstart] then
    inc(lowstart);
  { Check for another reuse, then we can't use
    the append optimization and need to use a temp }
  needtemp:=false;
  for i:=lowstart to high(sarr) do
    begin
      if ShortstringClass(@DestS)=sarr[i] then
        begin
          needtemp:=true;
          break;
        end;
    end;
  if needtemp then
    begin
      lowstart:=low(sarr);
      tmpstr:='';
      pdest:=ShortstringClass(@tmpstr)
    end
  else
    begin
      { Start with empty DestS if we start with concatting
        the first array element }
      if lowstart=low(sarr) then
        DestS:='';
      pdest:=ShortstringClass(@DestS);
    end;
  { Concat all strings, except the string we already
    copied in DestS }
  Len:=pdest.curlen;
  for i:=lowstart to high(sarr) do
    begin
      p:=sarr[i];
      if assigned(p) then
        begin
          s2l:=p.curlen;
          if Len+s2l>high(dests) then
            s2l:=high(dests)-Len;
          JLSystem.ArrayCopy(JLObject(p.fdata),0,JLObject(pdest.fdata),len,s2l);
          inc(Len,s2l);
        end;
    end;
  pdest.curlen:=len;
  if needtemp then
    DestS:=TmpStr;
end;


procedure fpc_shortstr_append_shortstr(var s1:shortstring;const s2:shortstring); compilerproc;
var
  s1l, s2l : integer;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(s1) then
    s2l:=high(s1)-s1l;
  JLSystem.ArrayCopy(JLObject(ShortstringClass(@s2).fdata),0,JLObject(ShortstringClass(@s1).fdata),s1l,s2l);
  s1[0]:=chr(s1l+s2l);
end;


function fpc_shortstr_compare(const left,right:shortstring) : longint; compilerproc;
Var
  MaxI,Temp, i : SizeInt;
begin
  if ShortstringClass(@left)=ShortstringClass(@right) then
    begin
      result:=0;
      exit;
    end;
  Maxi:=Length(left);
  temp:=Length(right);
  If MaxI>Temp then
    MaxI:=Temp;
  if MaxI>0 then
    begin
      for i:=0 to MaxI-1 do
        begin
          result:=ord(ShortstringClass(@left).fdata[i])-ord(ShortstringClass(@right).fdata[i]);
          if result<>0 then
            exit;
        end;
      result:=Length(left)-Length(right);
    end
  else
    result:=Length(left)-Length(right);
end;


function fpc_shortstr_compare_equal(const left,right:shortstring) : longint; compilerproc;
Var
  MaxI,Temp : SizeInt;
begin
  if ShortstringClass(@left)=ShortstringClass(@right) then
    begin
      result:=0;
      exit;
    end;
  result:=ord(not JUArrays.equals(TJByteArray(ShortstringClass(@left).fdata),TJByteArray(ShortstringClass(@right).fdata)));
end;


procedure fpc_chararray_to_shortstr(out res : shortstring;const arr: array of AnsiChar; zerobased: boolean = true); compilerproc;
var
 l: longint;
 index: longint;
 len: byte;
 foundnull: boolean;
begin
  l:=high(arr)+1;
  if l>=high(res)+1 then
    l:=high(res)
  else if l<0 then
    l:=0;
  if zerobased then
    begin
      foundnull:=false;
      for index:=low(arr) to l-1 do
        if arr[index]=#0 then
          begin
            foundnull:=true;
            break;
          end;
      if not foundnull then
        len:=l
      else
        len:=index;
    end
  else
    len:=l;
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(ShortstringClass(@res).fdata),0,len);
  ShortstringClass(@res).curlen:=len;
end;


procedure fpc_shortstr_to_chararray(out res: array of AnsiChar; const src: ShortString); compilerproc;
var
  len: longint;
begin
  len:=length(src);
  if len>length(res) then
    len:=length(res);
  { make sure we don't access char 1 if length is 0 (JM) }
  if len>0 then
    JLSystem.ArrayCopy(JLObject(ShortstringClass(@src).fdata),0,JLObject(@res),0,len);
  JUArrays.fill(TJByteArray(@res),len,high(res),0);
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


function lowercase(const s : shortstring) : shortstring;
var
  u : unicodestring;
begin
  u:=s;
  result:=lowercase(u);
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


