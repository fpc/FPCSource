uses gmap, gutil;

type lesslli=specialize TLess<longint>;
     maplli=specialize TMap<longint, longint, lesslli>;

var data:maplli; i:longint; iterator:maplli.TMSet.PNode;

begin
  data:=maplli.Create;

  for i:=0 to 10 do
    data[i]:=10*i;

  {Iteration through elements}
  iterator:=data.Min;
  while iterator<>nil do begin
    writeln(iterator^.Data.Key, ' ', iterator^.Data.Value);
    iterator:=data.next(iterator);
  end;

  writeln(data.FindLess(7)^.Data.Value);

  data.Destroy;
end.
