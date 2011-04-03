uses gqueue;

type queuelli = specialize TQueue<longint>;

var data:queuelli; i:longint;

begin
  data:=queuelli.Create;
  for i:=1 to 10 do
    data.Push(10*i);
  while not data.IsEmpty do begin
    writeln(data.Front);
    data.Pop;
  end;

  data.Destroy;
end.
