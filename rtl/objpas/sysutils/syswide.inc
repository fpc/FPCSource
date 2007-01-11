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

function Trim(const S: widestring): widestring;
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
	

{   TrimLeft returns a copy of S with all blank characters on the left stripped off  }
function TrimLeft(const S: widestring): widestring;
	var 
	  i,l:sizeint;
	begin
	  l := length(s);
	  i := 1;
	  while (i<=l) and (s[i]<=' ') do
	   inc(i);
	  Result := copy(s, i, l);
	end;
	

{   TrimRight returns a copy of S with all blank characters on the right stripped off  }
function TrimRight(const S: widestring): widestring;
	var 
		l:sizeint;
	begin
	  l := length(s);
	  while (l>0) and (s[l]<=' ') do
	   dec(l);
	  result := copy(s,1,l);
	end;


function WideUpperCase(const s : WideString) : WideString;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.UpperWideStringProc(s);
  end;


function WideLowerCase(const s : WideString) : WideString;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.LowerWideStringProc(s);
  end;


function WideCompareStr(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2);
  end;


function WideSameStr(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2)=0;
  end;


function WideCompareText(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareTextWideStringProc(s1,s2);
  end;
  
  
function WideSameText(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareTextWideStringProc(s1,s2)=0;
  end;
  

{ we've no templates, but with includes we can simulate this :) }
{$macro on}
{$define INWIDEFORMAT}
{$define TFormatString:=widestring}
{$define TFormatChar:=widechar}

Function WideFormat (Const Fmt : WideString; const Args : Array of const; Const FormatSettings: TFormatSettings) : WideString;
{$i sysformt.inc}

{$undef TFormatString}
{$undef TFormatChar}
{$undef INWIDEFORMAT}
{$macro off}

Function WideFormat (Const Fmt : WideString; const Args : Array of const) : WideString;

begin
  Result:=WideFormat(Fmt,Args,DefaultFormatSettings);
end;



Function WideFormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const; Const FormatSettings: TFormatSettings) : Cardinal;
Var 
  S,F : WideString;
begin
  Setlength(F,fmtlen);
  if fmtlen > 0 then
    Move(fmt,F[1],fmtlen*sizeof(Widechar));
  S:=WideFormat (F,Args);
  If Cardinal(Length(S))<Buflen then
    Result:=Length(S)
  else
    Result:=Buflen;
  Move(S[1],Buffer,Result);
end;

Function WideFormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const) : Cardinal;

begin
  Result:=WideFormatBuf(Buffer,BufLEn,Fmt,FmtLen,Args,DefaultFormatSettings);
end;

Procedure WideFmtStr(Var Res: WideString; Const Fmt : WideString; Const args: Array of const; Const FormatSettings: TFormatSettings);

begin
  Res:=WideFormat(fmt,Args);
end;

Procedure WideFmtStr(Var Res: WideString; Const Fmt : WideString; Const args: Array of const);

begin
  WideFmtStr(Res,Fmt,Args,DefaultFormatSettings);
end;


