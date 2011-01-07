{ %fail }
program tgeneric26;

{$mode objfpc}{$H+}
type
  generic TRecArr<T> = array[0..1] of record
    case enum:(one, two, three) of
      one: (F: T); // can't use type parameter here
      two: (Z: T);
      three: (Y: T);
  end;

begin
end.

