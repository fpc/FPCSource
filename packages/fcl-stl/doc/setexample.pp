uses gset, gutil;

type lesslli=specialize TLess<longint>;
     setlli=specialize TSet<longint, lesslli>;

var data:setlli; i:longint; iterator:setlli.TIterator;

begin
  data:=setlli.Create;

  for i:=0 to 10 do
    data.insert(i);

  {Iteration through elements}
  iterator:=data.Min;
  repeat
    writeln(iterator.Data);
  until not iterator.next;
  {Don't forget to destroy iterator}
  iterator.Destroy;

  iterator := data.FindLess(7);
  writeln(iterator.Data);
  iterator.Destroy;

  data.Destroy;
end.
