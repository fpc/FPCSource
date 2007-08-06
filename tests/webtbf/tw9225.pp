{ %fail }
{$mode objfpc}{$H+}

type
  generic TGenMap<T> = class(TObject)
    function Add(const AKey: T): T;
  end;

function TGenMap.Add(const AKey: T): T;
begin
end;

var
  a: TGenMap;
begin
  a := TGenMap.Create;

  a.Add(5);

  a.Free;
end.
