{ %fail }

{$mode objfpc}
type
  tc = class
    procedure test(var b: byte);
    procedure test(const b: array of byte);
  end;

procedure tc.test(var b: byte);
begin
end;

procedure tc.test(const b: array of byte);
begin
end;

begin
end.
