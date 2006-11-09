{$mode objfpc}

type
  generic TMap<TK, TD> = class(TObject)
    Key: TK;
    Data: TD;
    procedure Add(const AKey: TK; const AData: TD);
  end;

procedure TMap.Add(const AKey: TK; const AData: TD);
begin
  Key := AKey;
  Data := AData;
end;

type
  TMyStringList = specialize TMap<string, TObject>;

var
  slist: TMyStringList;
begin
  slist := TMyStringList.Create;
  slist.Add('test', slist);
  if slist.Key <> 'test' then
    halt(1);
  if slist.Data <> slist then
    halt(1);
  slist.Free;
end.
