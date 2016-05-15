{ %norun }

{$mode objfpc}
type
  TMyObject = class;
  TArr = array of TMyObject;
  TMyObject = class
  public
    constructor Create(ar: array of TMyObject); overload;
    constructor Create(ar: TArr); overload;
  end;

constructor TMyObject.Create(ar: array of TMyObject);
begin
end;

constructor TMyObject.Create(ar: Tarr);
begin
end;

begin
  TMyObject.Create([nil]);
end.

