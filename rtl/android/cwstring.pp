{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by Yury Sidorov,
    member of the Free Pascal development team.

    Wide string support for Android

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

{$mode objfpc}
{$inline on}
{$implicitexceptions off}

unit cwstring;

interface

procedure SetCWidestringManager;

implementation

procedure Wide2AnsiMove(source:pwidechar; var dest:RawByteString; cp:TSystemCodePage; len:SizeInt);
var
  i : SizeInt;
  hs : RawByteString;
begin
  dest:='';
  if len = 0 then
    exit;
  if (cp = CP_UTF8) or (cp = CP_ACP) then
    begin
      // Only UTF-8 is supported for Android
      SetLength(hs,len*3);
      i:=UnicodeToUtf8(pchar(hs),length(hs)+1,source,len);
      if i > 0 then
        begin
          SetLength(hs,i-1);
          dest:=hs;
        end;
    end
  else
    DefaultUnicode2AnsiMove(source,dest,DefaultSystemCodePage,len);
end;

procedure Ansi2WideMove(source:pchar; cp:TSystemCodePage; var dest:widestring; len:SizeInt);
var
  i : SizeInt;
  hs : UnicodeString;
begin
  // Only UTF-8 is supported for Android
  dest:='';
  if len = 0 then
    exit;
  if (cp = CP_UTF8) or (cp = CP_ACP) then
    begin
      SetLength(hs,len);
      i:=Utf8ToUnicode(PUnicodeChar(hs),length(hs)+1,pchar(source),len);
      if i>0 then
        begin
          SetLength(hs,i-1);
          dest:=hs;
        end
      else
        dest:='';
    end
  else
    DefaultAnsi2UnicodeMove(source,DefaultSystemCodePage,dest,len);
end;

function UpperWideString(const s : WideString) : WideString;
begin
  // Not implemented
  Result:=UpCase(AnsiString(s));
end;

function LowerWideString(const s : WideString) : WideString;
begin
  // Not implemented
  Result:=LowerCase(AnsiString(s));
end;

function _CompareStr(const S1, S2: ansistring): Integer;
var count, count1, count2: integer;
begin
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := CompareByte(PChar(S1)^,PChar(S2)^,Count);
  if result=0 then
    result:=Count1-Count2;
end;

function CompareWideString(const s1, s2 : WideString) : PtrInt;
begin
  // Not implemented
  Result:=_CompareStr(s1, s2);
end;

function CompareTextWideString(const s1, s2 : WideString): PtrInt;
begin
  // Not implemented
  Result:=_CompareStr(LowerCase(AnsiString(s1)), LowerCase(AnsiString(s2)));
end;

function UpperAnsiString(const s : AnsiString) : AnsiString;
begin
  // Not implemented
  Result:=UpCase(s);
end;

function LowerAnsiString(const s : AnsiString) : AnsiString;
begin
  // Not implemented
  Result:=LowerCase(s);
end;

function CharLengthPChar(const Str: PChar): PtrInt;
begin
  // Not implemented
  Result:=Length(Str);
end;

function CodePointLength(const Str: PChar; maxlookahead: ptrint): PtrInt;
begin
  // Not implemented
  Result:=Length(Str);
end;

function CompareStrAnsiString(const s1, s2: ansistring): PtrInt;
begin
  // Not implemented
  Result:=_CompareStr(s1, s2);
end;

function StrCompAnsi(s1,s2 : PChar): PtrInt;
begin
  // Not implemented
  Result:=_CompareStr(s1, s2);
end;

function AnsiCompareText(const S1, S2: ansistring): PtrInt;
begin
  // Not implemented
  Result:=_CompareStr(LowerCase(s1), LowerCase(s2));
end;

function AnsiStrIComp(S1, S2: PChar): PtrInt;
begin
  // Not implemented
  Result:=_CompareStr(LowerCase(s1), LowerCase(s2));
end;

function AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
begin
  // Not implemented
  Result:=_CompareStr(Copy(s1, 1, MaxLen), Copy(s2, 1, MaxLen));
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
begin
  // Not implemented
  Result:=_CompareStr(LowerCase(Copy(s1, 1, MaxLen)), LowerCase(Copy(s2, 1, MaxLen)));
end;

function AnsiStrLower(Str: PChar): PChar;
var
  temp: ansistring;
begin
  // Not implemented
  temp:=loweransistring(str);
  Move(PChar(temp)^, Str, Length(temp));
  Result:=Str;
end;

function AnsiStrUpper(Str: PChar): PChar;
var
  temp: ansistring;
begin
  // Not implemented
  temp:=upperansistring(str);
  Move(PChar(temp)^, Str, Length(temp));
  Result:=Str;
end;

function GetStandardCodePage(const stdcp: TStandardCodePageEnum): TSystemCodePage;
begin
  Result := CP_UTF8; // Android has only UTF-8
end;

{$ifdef FPC_HAS_CPSTRING}
{$i textrec.inc}
procedure SetStdIOCodePage(var T: Text); inline;
begin
  case TextRec(T).Mode of
    fmInput:TextRec(T).CodePage:=GetStandardCodePage(scpConsoleInput);
    fmOutput:TextRec(T).CodePage:=GetStandardCodePage(scpConsoleOutput);
  end;
end;

procedure SetStdIOCodePages; inline;
begin
  SetStdIOCodePage(Input);
  SetStdIOCodePage(Output);
  SetStdIOCodePage(ErrOutput);
  SetStdIOCodePage(StdOut);
  SetStdIOCodePage(StdErr);
end;
{$endif FPC_HAS_CPSTRING}

Procedure SetCWideStringManager;
Var
  CWideStringManager : TUnicodeStringManager;
begin
  CWideStringManager:=widestringmanager;
  With CWideStringManager do
    begin
      Wide2AnsiMoveProc:=@Wide2AnsiMove;
      Ansi2WideMoveProc:=@Ansi2WideMove;

      UpperWideStringProc:=@UpperWideString;
      LowerWideStringProc:=@LowerWideString;

      CompareWideStringProc:=@CompareWideString;
      CompareTextWideStringProc:=@CompareTextWideString;

      CharLengthPCharProc:=@CharLengthPChar;
      CodePointLengthProc:=@CodePointLength;

      UpperAnsiStringProc:=@UpperAnsiString;
      LowerAnsiStringProc:=@LowerAnsiString;
      CompareStrAnsiStringProc:=@CompareStrAnsiString;
      CompareTextAnsiStringProc:=@AnsiCompareText;
      StrCompAnsiStringProc:=@StrCompAnsi;
      StrICompAnsiStringProc:=@AnsiStrIComp;
      StrLCompAnsiStringProc:=@AnsiStrLComp;
      StrLICompAnsiStringProc:=@AnsiStrLIComp;
      StrLowerAnsiStringProc:=@AnsiStrLower;
      StrUpperAnsiStringProc:=@AnsiStrUpper;
      { Unicode }
      Unicode2AnsiMoveProc:=@Wide2AnsiMove;
      Ansi2UnicodeMoveProc:=@Ansi2WideMove;
      UpperUnicodeStringProc:=@UpperWideString;
      LowerUnicodeStringProc:=@LowerWideString;
      CompareUnicodeStringProc:=@CompareWideString;
      CompareTextUnicodeStringProc:=@CompareTextWideString;
      { CodePage }
      GetStandardCodePageProc:=@GetStandardCodePage;
    end;
  SetUnicodeStringManager(CWideStringManager);
end;

{$ifndef android}
var
  iconvlib:TLibHandle;
{$endif android}

initialization
  SetCWideStringManager;
  { set the DefaultSystemCodePage }
  DefaultSystemCodePage:=GetStandardCodePage(scpAnsi);
  SetStdIOCodePages;

finalization

end.
