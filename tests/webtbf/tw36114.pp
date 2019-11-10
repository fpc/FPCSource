{ %FAIL }

program tw36114;

{$mode objfpc}{$H+}{$J-}
type
  TTestme=class
  strict private
  const
  {$push}{$writeableconst off} // superfluous but also doesn't work
    c_one:integer = 1;
    c_two:integer = 10;
  {$pop}
  public
  class property one:integer read c_one;
  class property two:integer read c_two;
  end;

begin
  TTestme.c_one := 1000;
  writeln(TTestme.One);
end.
