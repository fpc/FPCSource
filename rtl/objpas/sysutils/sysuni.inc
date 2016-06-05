{
    *********************************************************************
    Copyright (C) 2002-2005 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    *********************************************************************
}

function Trim(const S: unicodestring): unicodestring;
  var 
    Ofs, Len: sizeint;
  begin
    len := Length(S);
    while (Len>0) and (S[Len]<=' ') do
     dec(Len);
    Ofs := 1;
    while (Ofs<=Len) and (S[Ofs]<=' ') do
      Inc(Ofs);
    result := Copy(S, Ofs, 1 + Len - Ofs);
  end;
	

{ TrimLeft returns a copy of S with all blank characters on the left stripped off  }
function TrimLeft(const S: unicodestring): unicodestring;
  var 
    i,l:sizeint;
  begin
    l := length(s);
    i := 1;
    while (i<=l) and (s[i]<=' ') do
      inc(i);
    Result := copy(s, i, l);
  end;
	

{ TrimRight returns a copy of S with all blank characters on the right stripped off  }
function TrimRight(const S: unicodestring): unicodestring;
  var 
	l:sizeint;
  begin
    l := length(s);
    while (l>0) and (s[l]<=' ') do
      dec(l);
    result := copy(s,1,l);
  end;


Function InternalChangeCase(Const S : UnicodeString; const Chars: TSysCharSet; const Adjustment: Longint): UnicodeString;
  var
    i : Integer;
    P : PWideChar;
    Unique : Boolean;
  begin
    Result := S;
    if Result='' then
      exit;
    Unique:=false;
    P:=PWideChar(Result);
    for i:=1 to Length(Result) do
      begin
        if CharInSet(P^,Chars) then
          begin
            if not Unique then
              begin
                UniqueString(Result);
                p:=@Result[i];
                Unique:=true;
              end;
            P^:=WideChar(Ord(P^)+Adjustment);
          end;
        Inc(P);
      end;
  end;


{   UpperCase returns a copy of S where all lowercase characters ( from a to z )
    have been converted to uppercase   }
Function UpperCase(Const S : UnicodeString) : UnicodeString;
  begin
    Result:=InternalChangeCase(S,['a'..'z'],-32);
  end;


{   LowerCase returns a copy of S where all uppercase characters ( from A to Z )
    have been converted to lowercase  }
Function Lowercase(Const S : UnicodeString) : UnicodeString;
  begin
    Result:=InternalChangeCase(S,['A'..'Z'],32);
  end;


function UnicodeUpperCase(const s : UnicodeString) : UnicodeString;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.UpperUnicodeStringProc(s);
  end;


function UnicodeLowerCase(const s : UnicodeString) : UnicodeString;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.LowerUnicodeStringProc(s);
  end;


function UnicodeCompareStr(const s1, s2 : UnicodeString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareUnicodeStringProc(s1,s2,[]);
  end;


function UnicodeSameStr(const s1, s2 : UnicodeString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareUnicodeStringProc(s1,s2,[])=0;
  end;


function UnicodeCompareText(const s1, s2 : UnicodeString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareUnicodeStringProc(s1,s2,[coIgnoreCase]);
  end;
  
  
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareUnicodeStringProc(s1,s2,[coIgnoreCase])=0;
  end;
  

{ we've no templates, but with includes we can simulate this :) }
{$macro on}
{$define INWIDEFORMAT}
{$define TFormatString:=unicodestring}
{$define TFormatChar:=unicodechar}

Function UnicodeFormat (Const Fmt : UnicodeString; const Args : Array of const; Const FormatSettings: TFormatSettings) : UnicodeString;
{$i sysformt.inc}

{$undef TFormatString}
{$undef TFormatChar}
{$undef INWIDEFORMAT}
{$macro off}

Function UnicodeFormat (Const Fmt : UnicodeString; const Args : Array of const) : UnicodeString;
begin
  Result:=UnicodeFormat(Fmt,Args,DefaultFormatSettings);
end;


Function UnicodeFormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const; Const FormatSettings: TFormatSettings) : Cardinal;
Var 
  S,F : UnicodeString;
begin
  Setlength(F,fmtlen);
  if fmtlen > 0 then
    Move(fmt,F[1],fmtlen*sizeof(Unicodechar));
  S:=UnicodeFormat (F,Args);
  If Cardinal(Length(S))<Buflen then
    Result:=Length(S)
  else
    Result:=Buflen;
  Move(S[1],Buffer,Result);
end;

Function UnicodeFormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const) : Cardinal;

begin
  Result:=UnicodeFormatBuf(Buffer,BufLEn,Fmt,FmtLen,Args,DefaultFormatSettings);
end;


Procedure UnicodeFmtStr(Var Res: UnicodeString; Const Fmt : UnicodeString; Const args: Array of const; Const FormatSettings: TFormatSettings);
begin
  Res:=UnicodeFormat(fmt,Args);
end;


Procedure UnicodeFmtStr(Var Res: UnicodeString; Const Fmt : UnicodeString; Const args: Array of const);
begin
  UnicodeFmtStr(Res,Fmt,Args,DefaultFormatSettings);
end;


function StrMove(dest,source : PWideChar;l : SizeInt) : PWideChar; overload;
 begin
   move(source^,dest^,l*2);
   Result:=dest;
 end;


function StrPLCopy(Dest: PWideChar; const Source: UnicodeString; MaxLen: SizeUInt): PWideChar; overload;
var Len: SizeUInt;
begin
  Len := length(Source);
  if Len > MaxLen then
    Len := MaxLen;
  Move(Source[1], Dest^, Len*sizeof(WideChar));
  Dest[Len] := #0;
  StrPLCopy := Dest;
end;


function StrPCopy(Dest: PWideChar; const Source: UnicodeString): PWideChar; overload;
begin
  StrPCopy := StrPLCopy(Dest, Source, length(Source));
end;


function StrScan(P: PWideChar; C: WideChar): PWideChar;
Var
   count: SizeInt;
Begin
 count := 0;
 { As in Borland Pascal, if looking for NULL return null }
 if C = #0 then
 begin
   StrScan := @(P[StrLen(P)]);
   exit;
 end;
 { Find first matching character of Ch in Str }
 while P[count] <> #0 do
 begin
   if C = P[count] then
    begin
        StrScan := @(P[count]);
        exit;
    end;
   Inc(count);
 end;
 { nothing found. }
 StrScan := nil;
end;


function StrPas(Str: PWideChar): UnicodeString;overload;
begin
  Result:=Str;
end;


function strecopy(dest,source : pwidechar) : pwidechar;
  var
    counter: sizeint;
  begin
    counter := indexword(source^,-1,0);
    { counter+1 will move zero terminator }
    move(source^,dest^,(counter+1)*2);
    result:=dest+counter;
  end;


function strend(p : pwidechar) : pwidechar;
  begin
    result:=p+indexword(p^,-1,0);
  end;


function strcat(dest,source : pwidechar) : pwidechar;
  begin
    strcopy(strend(dest),source);
    strcat:=dest;
  end;


function strcomp(str1,str2 : pwidechar) : SizeInt;
  var
    counter: sizeint;
    c1, c2: widechar;
  begin
    counter:=0;
    repeat
      c1:=str1[counter];
      c2:=str2[counter];
      inc(counter);
    until (c1<>c2) or
          (c1=#0) or
          (c2=#0);
    strcomp:=ord(c1)-ord(c2);
  end;


function strlcomp(str1,str2 : pwidechar;l : SizeInt) : SizeInt;
  var
    counter: sizeint;
    c1, c2: widechar;
  begin
    if l = 0 then
      begin
        strlcomp := 0;
        exit;
      end;
    counter:=0;
    repeat
      c1:=str1[counter];
      c2:=str2[counter];
      inc(counter);
   until (c1<>c2) or (counter>=l) or
         (c1=#0) or (c2=#0);
    strlcomp:=ord(c1)-ord(c2);
  end;


{ the str* functions are not supposed to support internationalisation;
  system.upcase(widechar) does support it (although this is
  Delphi-incompatible) }
function simplewideupcase(w: widechar): widechar;
  begin
    if w in ['a'..'z'] then
      result:=widechar(ord(w)-32)
    else
      result:=w;
  end;


function stricomp(str1,str2 : pwidechar) : SizeInt;
  var
   counter: sizeint;
   c1, c2: widechar;
  begin
    counter := 0;
    c1:=simplewideupcase(str1[counter]);
    c2:=simplewideupcase(str2[counter]);
    while c1=c2 do
      begin
        if (c1=#0) or (c2=#0) then break;
        inc(counter);
        c1:=simplewideupcase(str1[counter]);
        c2:=simplewideupcase(str2[counter]);
      end;
    stricomp:=ord(c1)-ord(c2);
  end;


function strlcat(dest,source : pwidechar;l : SizeInt) : pwidechar;
  var
    destend : pwidechar;
  begin
    destend:=strend(dest);
    dec(l,destend-dest);
    if l>0 then
      strlcopy(destend,source,l);
    strlcat:=dest;
  end;


function strrscan(p : pwidechar;c : widechar) : pwidechar;
  var
   count: sizeint;
   index: sizeint;
  begin
    count:=strlen(p);
    { As in Borland Pascal , if looking for NULL return null }
    if c=#0 then
      begin
        strrscan:=@(p[count]);
        exit;
      end;
    dec(count);
    for index:=count downto 0 do
      begin
        if c=p[index] then
          begin
            strrscan:=@(p[index]);
            exit;
          end;
      end;
    { nothing found. }
    strrscan:=nil;
  end;


function strlower(p : pwidechar) : pwidechar;
  var
   counter: SizeInt;
   c: widechar;
  begin
    counter:=0;
    repeat
      c:=p[counter];
      if c in [#65..#90] then
        p[counter]:=widechar(ord(c)+32);
      inc(counter);
    until c=#0;
    strlower:=p;
  end;


function strupper(p : pwidechar) : pwidechar;
  var
   counter: SizeInt;
   c: widechar;
  begin
    counter:=0;
    repeat
      c:=p[counter];
      if c in [#97..#122] then
        p[counter]:=widechar(ord(c)-32);
      inc(counter);
    until c=#0;
    strupper:=p;
  end;


function strlicomp(str1,str2 : pwidechar;l : SizeInt) : SizeInt;
  var
   counter: sizeint;
   c1, c2: char;
  begin
    counter := 0;
    if l=0 then
      begin
        strlicomp := 0;
        exit;
      end;
    repeat
      c1:=simplewideupcase(str1[counter]);
      c2:=simplewideupcase(str2[counter]);
      if (c1=#0) or (c2=#0) then break;
      inc(counter);
    until (c1<>c2) or (counter>=l);
    strlicomp:=ord(c1)-ord(c2);
  end;


function strpos(str1,str2 : pwidechar) : pwidechar;
  var
    p : pwidechar;
    lstr2 : SizeInt;
  begin
    strpos:=nil;
    if (str1=nil) or (str2=nil) then
      exit;
    p:=strscan(str1,str2^);
    if p=nil then
       exit;
    lstr2:=strlen(str2);
    while p<>nil do
      begin
        if strlcomp(p,str2,lstr2)=0 then
          begin
             strpos:=p;
             exit;
          end;
        inc(p);
        p:=strscan(p,str2^);
      end;
  end;


function strnew(p : pwidechar) : pwidechar; overload;
  var
    len: sizeint;
  begin
    len:=strlen(p)+1;
    result:=WideStrAlloc(Len);
    if result<>nil then
      strmove(result,p,len);
  end;


function WideStrAlloc(Size: cardinal): PWideChar;
  begin
    getmem(result,size*2+sizeof(cardinal));
    PCardinal(result)^:=size*2+sizeof(cardinal);
    result:=PWideChar(PByte(result)+sizeof(cardinal));
  end;

function StrBufSize(str: pwidechar): cardinal;
  begin
    if assigned(str) then
      result:=(PCardinal(PByte(str)-sizeof(cardinal))^)-sizeof(cardinal)
    else
      result := 0;
  end;

procedure StrDispose(str: pwidechar);
begin
  if assigned(str) then
   begin
     str:=PWideChar(PByte(str)-sizeof(cardinal));
     freemem(str,PCardinal(str)^);
   end;
end;



function BytesOf(const Val: UnicodeString): TBytes;
begin
  Result:=TEncoding.Default.GetBytes(Val);
end;


function BytesOf(const Val: WideChar): TBytes; overload;
begin
  Result:=TEncoding.Default.GetBytes(Val);
end;

function StringOf(const Bytes: TBytes): UnicodeString;
begin
  Result:=TEncoding.Default.GetString(Bytes);
end;


function WideBytesOf(const Value: UnicodeString): TBytes;
var
  Len:Integer;
begin
  Len:=Length(Value)*SizeOf(UnicodeChar);
  SetLength(Result,Len);
  if Len>0 then
    Move(Value[1],Result[0],Len);
end;

function WideStringOf(const Value: TBytes): UnicodeString;
var
  Len:Integer;
begin
  Len:=Length(Value) div SizeOf(UnicodeChar);
  SetLength(Result,Len);
  if Len>0 then
    Move(Value[0],Result[1],Len*SizeOf(UnicodeChar));
end;

function ByteLength(const S: UnicodeString): Integer;
begin
  Result:=Length(S)*SizeOf(UnicodeChar);
end;

{$macro on}
{$define INUNICODESTRINGREPLACE}
{$define SRString:=UnicodeString}
{$define SRUpperCase:=WideUppercase}
function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
{$i syssr.inc}

{$undef INUNICODESTRINGREPLACE}
{$undef SRString}
{$undef SRUpperCase}
