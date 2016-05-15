program error;

{$mode Delphi}

uses sysutils;

type a = 1..MaxInt;

var b: a;
    c: integer;

begin
 b := 3;
 c := -5 div b;
 writeln(c);
end.
