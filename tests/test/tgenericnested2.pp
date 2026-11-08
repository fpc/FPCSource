{ %FAIL }
{ A type nested in two specializations is the same type only where the specializations are equal both ways:
  an array over a narrower subrange is not an array over a wider one }
program tgenericnested2;

{$mode delphi}

type
  TNarrow = 1..3;
  TWide = 1..100;

  TBox<T> = class
  public type
    TArr = array[T] of Byte;
  end;

var
  lNarrow: TBox<TNarrow>.TArr;
  lWide: TBox<TWide>.TArr;
begin
  lWide := lNarrow;
end.
