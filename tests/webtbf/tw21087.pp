{ %fail }

{$mode objfpc}{$H+}
uses
  Classes;

type
  TMyItem = class(TObject)
  end;

  TMyList = class(tfplist)
    function GetItem(const I: Integer): TMyItem;
    procedure SetItem(const I: Integer; const Item: TMyItem);
  public
    property Items[I: Integer]: TMyItem read GetItem write SetItem; default;
  end;

function TMyList.GetItem(const I: Integer): TMyItem;
begin
  Result := TMyItem(inherited Items[I]);
end;

procedure TMyList.SetItem(const I: Integer; const Item: TMyItem);
begin
  (inherited Items[I]) := Item;
end;

var
  I1, I2: TMyItem;
  L: TMyList;
begin
  try
    I1 := TMyItem.Create;
    I2 := TMyItem.Create;
    L := TMyList.Create;

    L.Add(I1);
    L[0] := I2;
    Assert(L[0] = I2);
  finally
    I1.Free;
    I2.Free;
    L.Free;
  end;
end.
