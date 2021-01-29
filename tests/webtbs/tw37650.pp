{ %NORUN }

program tw37650;

{$mode objfpc}

type
  generic TMyClass<const U: Integer> = class
    type TKey = String[U];
  end;

generic procedure Test<const U: Integer>;
type
  TKey = String[U];
begin
end;

type
  TMyClass12 = specialize TMyClass<12>;
begin
  specialize Test<12>;
end.
