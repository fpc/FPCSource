{
    *********************************************************************
    Copyright (C) 2002-2005 by Florian Klaempfl

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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


