{$mode TP}
uses ub0339;
type
  r = packed record
        Foo : Boolean;
        Bar : (No, Yes);
        Baz : 0 .. 3;
        Qux : -1 .. 0;
        Fred : 1 .. 7
      end;
begin
  Writeln ('AAA: Size of packed record r = ', SizeOf (r), ' bytes.');
  Writeln ('AAA: Size of packed record r2 = ', SizeOf (r2), ' bytes.');
  PrintSize;
end.
