program tgeneric27;

{ check that specialization does not add enum members to the static symtable and reuses the generic enum definintion }

{$mode objfpc}{$H+}
type
  generic TRecArr<T> = array[0..1] of record
    case enum:(one, two, three) of
      one: (F: Integer);
      two: (Z: Byte);
      three: (Y: PChar);
  end;

var
  A: specialize TRecArr<Integer>;
  B: specialize TRecArr<String>;
begin
  A[0].enum := one;
  B[0].enum := one;
  if A[0].enum <> B[0].enum then
    halt(1);
end.

