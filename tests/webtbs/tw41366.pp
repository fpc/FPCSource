unit tw41366;

{$mode objfpc}

interface

type
  TItem = record
    Flag: Boolean;
  end;
  TItemArray = array of TItem;

procedure Foo(const Items: TItemArray);

implementation

procedure Foo(const Items: TItemArray);
begin
  Items[0].Flag := (@Items <> Nil);
end;

end.
