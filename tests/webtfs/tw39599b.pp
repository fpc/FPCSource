{ %fail }
program project1;
{$mode objfpc}
type
  TMyClass = class(TObject)
  type
    procedure A; virtual; abstract; // should not be allowed
  end;
begin
end.
