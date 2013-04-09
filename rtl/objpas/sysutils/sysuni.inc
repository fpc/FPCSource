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
    result:=widestringmanager.CompareUnicodeStringProc(s1,s2);
  end;


function UnicodeSameStr(const s1, s2 : UnicodeString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareUnicodeStringProc(s1,s2)=0;
  end;


function UnicodeCompareText(const s1, s2 : UnicodeString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareTextUnicodeStringProc(s1,s2);
  end;
  
  
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareTextUnicodeStringProc(s1,s2)=0;
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


function StrPLCopy(Dest: PWideChar; const Source: UnicodeString; MaxLen: SizeInt): PWideChar; overload;
var Len: SizeInt;
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


function strnew(p : PWideChar) : PWideChar; overload;
var
  len : SizeInt;
begin
  Result:=nil;
  if (p=nil) or (p^=#0) then
   exit;
  len:=strlen(p)+1;
  Result:=PWideChar(StrAlloc(Len*2));
  if Result<>nil then
   strmove(Result,p,len);
end;

function StrPas(Str: PWideChar): UnicodeString;overload;
begin
  Result:=Str;
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
