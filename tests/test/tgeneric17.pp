{$mode objfpc}{$H+}

type
  generic TGListItem<T> = class(TObject)
  public
    FNext: specialize TGListItem<T>;
    procedure Assign(Source: specialize TGListItem<T>);
  end;

procedure TGListItem<T>.Assign(Source: specialize TGListItem<T>);
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
