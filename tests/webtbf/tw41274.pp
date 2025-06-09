{ %fail }
{$mode objfpc}
type
  tt = class
    ff: byte;
    property NewInstance: byte read ff;
    constructor Create;
  end;

constructor tt.Create;
begin // Error: Internal error 200305108
end;

begin
end.
