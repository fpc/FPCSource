{ %norun }

{$mode objfpc}
type
  TMyObject = class
  public
    constructor Create(ar: array of TMyObject);
  end;

constructor TMyObject.Create(ar: array of TMyObject);
begin
end;

begin
  TMyObject.Create([nil]);
end.

