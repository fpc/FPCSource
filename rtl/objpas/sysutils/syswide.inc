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
    result:=widestringmanager.CompareWideStringProc(s1,s2,[]);
  end;


function WideSameStr(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2,[])=0;
  end;


function WideCompareText(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2,[coIgnoreCase]);
  end;
  
  
function WideSameText(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2,[coIgnoreCase])=0;
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

function StrCopy(Dest, Source: PWideChar): PWideChar; overload;
var
  counter : SizeInt;
begin
  counter := 0;
  while Source[counter] <> #0 do
  begin
    Dest[counter] := widechar(Source[counter]);
    Inc(counter);
  end;
  { terminate the string }
  Dest[counter] := #0;
  StrCopy := Dest;
end;

function StrLCopy(Dest,Source: PWideChar; MaxLen: SizeInt): PWideChar; overload;
var
  counter: SizeInt;
begin
  counter := 0;
  while (Source[counter] <> #0) and (counter < MaxLen) do
  begin
    Dest[counter] := widechar(Source[counter]);
    Inc(counter);
  end;
  { terminate the string }
  Dest[counter] := #0;
  StrLCopy := Dest;
end;

Function CharInSet(Ch:WideChar;Const CSet : TSysCharSet) : Boolean;
begin
  result:=(Ch<=#$FF) and (ansichar(byte(ch)) in CSet);
end;

{$macro on}
{$define INWIDESTRINGREPLACE}
{$define SRString:=WideString}
{$define SRUpperCase:=WideUppercase}
function WideStringReplace(const S, OldPattern, NewPattern: WideString;  Flags: TReplaceFlags): WideString;
{$i syssr.inc}

{$undef INWIDESTRINGREPLACE}
{$undef SRString}
{$undef SRUpperCase}
