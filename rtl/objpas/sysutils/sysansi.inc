{%MainUnit sysutils.pp}
{
    *********************************************************************
    Copyright (C) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function AnsiCompareFileName(const S1, S2: string): SizeInt;

begin
  If FileNameCaseSensitive then
    Result:=AnsiCompareStr(S1,S2) // Compare case sensitive
  else
    Result:=AnsiCompareText(S1,S2); // Compare case insensitive. No MBCS yet.
end;

Function SameFileName(const S1, S2: string): Boolean;

begin
  Result:=AnsiCompareFileName(S1,S2)=0;
end;

Function AnsiLowerCaseFileName(const S: string): string;

begin
  Result:=AnsiLowerCase(S); // No locale support or MBCS yet.
end;

Function AnsiUpperCaseFileName(const S: string): string;

begin
  Result:=AnsiUpperCase(S); // No locale support or MBCS yet.
end;

Function AnsiPos(const Substr, S: string): SizeInt;

begin
  Result:=Pos(Substr,S); // No MBCS yet.
end;

Function AnsiStrPos(Str, SubStr: PChar): PChar;

begin
  Result:=StrPos(Str,Substr);
end;

Function AnsiStrRScan(Str: PChar; Chr: Char): PChar;

begin
  Result:=StrRScan(Str,Chr);
end;

Function AnsiStrScan(Str: PChar; Chr: Char): PChar;

begin
  Result:=StrScan(Str,Chr);
end;

Function HashName(Name: PAnsiChar): LongWord;
Var
  thehash,g,I : LongWord;
begin
  thehash:=0;
  For I:=1 to Length(Name) do { 0 terminated }
    begin
      thehash:=thehash shl 4;
      inc(theHash,Ord(UpCase(Name[i])));
      g:=thehash and LongWord($f shl 28);
      if g<>0 then
        begin
          thehash:=thehash xor (g shr 24);
          thehash:=thehash xor g;
        end;
    end;
  If theHash=0 then
    HashName:=$ffffffff
  else
    HashName:=TheHash;
end;

function BytesOf(const Val: RawByteString): TBytes;
var
  Len:Integer;
begin
  Len:=Length(Val);
  SetLength(Result,Len);
  if Len>0 then
    Move(Val[1],Result[0],Len);
end;

function BytesOf(const Val: AnsiChar): TBytes;
begin
  SetLength(Result,1);
  Result[0]:=Byte(Val);
end;

Function CharInSet(Ch:AnsiChar;Const CSet : TSysCharSet) : Boolean; inline;
begin
  result:=ch in CSet;
end;
