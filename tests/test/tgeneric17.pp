{$mode objfpc}{$H+}

type
  generic TGListItem<T> = class(TObject)
  public
    FNext: TGListItem;
    procedure Assign(Source: TGListItem);
  end;

procedure TGListItem.Assign(Source: TGListItem)
begin
  FNext := Source;
end;

type
  TIntListItem = specialize TGListItem<Integer>;

var
  A, B: TIntListItem;
begin
  A := TIntListItem.Create;
  B := TIntListItem.Create;
  A.Assign(B);
  if A.FNext <> B then
    halt(1);
end.
