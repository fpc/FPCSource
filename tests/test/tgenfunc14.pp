{ %FAIL }

unit tgenfunc14;

{$mode objfpc}

{ constraints must not be repeated in the definition }

interface

generic procedure Test<T: class>;

implementation

generic procedure Test<T: class>;
begin

end;

end.

