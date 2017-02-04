program cp;

{$mode objfpc}
{$h+}
{$codepage utf8}

uses
  SysUtils
  {$ifdef unix}, cwstring
  {$endif};

type
  string1252 = type ansistring(1252);

function StrToHex(S: RawByteString): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    Result := Result + IntToHex(Ord(S[i]),2);
    if i < Length(S) then Result := Result + #32;
  end;
end;

function StrToHex(S: UnicodeString): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    Result := Result + IntToHex(Word(S[i]),4);
    if i < Length(S) then Result := Result + #32;
  end;
end;

function DebugString(S: RawByteString): String;
begin
  Result := Format('[%.5d] %s',[StringCodePage(S),StrToHex(S)]);
end;

function DebugString(S: UnicodeString): String;
begin
  Result := StrToHex(S);
end;

procedure Test(VarName: String; S: RawByteString; Expected: String);
begin
  write(VarName,': ',DebugString(S));
  if StrToHex(S) = Expected then
    writeln(': Ok')
  else
    writeln(': FAIL: Expected: ',Expected);
end;

procedure Test(VarName: String; S: UnicodeString; Expected: String);
begin
  write(VarName,': ',DebugString(S));
  if StrToHex(S) = Expected then
    writeln(': Ok')
  else
    writeln(': FAIL: Expected: ',Expected);
end;


var
  s: string;
  s1252: string1252;
  sutf8: utf8string;
  us: UnicodeString;

begin
  writeln('DefaultSystemCodePage = ',DefaultSystemCodePage);
  sutf8 := #$E2#$82#$AC; //eur symbol in UTF8 encoding
  Test('sutf8',sutf8,'E2 82 AC');

  s := sutf8;
  SetCodePage(RawBytestring(s), 1252, true);
  Test('s    ', s, '80');

  s1252 := sutf8;
  Test('s1252', s1252, '80');

  us := UnicodeString(sutf8);
  Test('us   ', us, '20AC');

  us := UnicodeString(s1252);
  Test('us   ', us, '20AC');
end.

