{$mode FPC}
unit ub0339;
interface
type
  r2 = packed record
        Foo : Boolean;
        Bar : (No, Yes);
        Baz : 0 .. 3;
        Qux : -1 .. 0;
        Fred : 1 .. 7
      end;
  procedure PrintSize;
implementation
  procedure PrintSize;
  begin
    Writeln ('BBB: Size of packed record r2 = ', SizeOf (r2), ' bytes.')
  end;
begin end.
