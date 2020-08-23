program example;

{$mode objfpc}{$H+}

procedure foo(out c: char); inline;
begin
  c := #32;
end;

var s: String;

begin
  s:=#42;
  foo(s[1]);
  Writeln(ord(s[1]));
end.
