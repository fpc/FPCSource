uses gmap, gutil;

type lesslli=specialize TLess<longint>;
     maplli=specialize TMap<longint, longint, lesslli>;

var data:maplli; i:longint; iterator:maplli.TIterator;
    pair : maplli.TPair;
begin
  data:=maplli.Create;

  for i:=0 to 10 do
    data[i]:=10*i;

  writeln(data[7]);
  data[7] := 42;

  {Iteration through elements with write access}
  iterator:=data.Min;
  repeat
    writeln(iterator.Key, ' ', iterator.Value);
    iterator.Value := 47;
  until not iterator.next;
  iterator.Destroy;

  // using for..in to check everything changed to 47. For in is shorter and autoallocated, but can't write to cells via iterator.
  for pair in data.min do
    writeln('Min: ',pair.Key, ' ', pair.Value);         

  iterator := data.FindLess(7);
  writeln(iterator.Value);
  iterator.Destroy;

  data.Destroy;
end.
