{ %fail }

unit tw19213;

{$mode objfpc}

interface

implementation

class operator + (a, b: String): Integer;
begin
  Result := Length(a) + Length(b);
end;

end.
