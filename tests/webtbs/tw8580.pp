program project3;

{$mode objfpc}{$H+}

type
  TUTF8Char = String[7];

var
  t: widestring;
  UTF8Char: TUTF8Char;

begin
  t := 'test';
  UTF8Char := TUTF8Char(T);
  writeln(UTF8Char);
end.
