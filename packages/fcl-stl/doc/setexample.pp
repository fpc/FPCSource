uses gset, gutil;

type lesslli=specialize TLess<longint>;
     setlli=specialize TSet<longint, lesslli>;

var data:setlli; i:longint; iterator:setlli.PNode;

begin
  data:=setlli.Create;

  for i:=0 to 10 do
    data.insert(i);

  {Iteration through elements}
  iterator:=data.Min;
  while iterator<>nil do begin
    writeln(iterator^.Data);
    iterator:=data.next(iterator);
  end;

  writeln(data.FindLess(7)^.Data);

  data.Destroy;
end.
