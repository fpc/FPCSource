program postest;

{$mode objfpc}{$H+}

var
  wchar: WideChar;
  wstring: WideString;

begin
  wchar := 'a';
  wstring := 'badc';
  writeln(Pos(wchar, wstring));
end.