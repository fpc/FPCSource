uses gstack;

type stacklli = specialize TStack<longint>;

var data:stacklli; i:longint;

begin
  data:=stacklli.Create;
  for i:=1 to 10 do
    data.Push(10*i);
  while not data.IsEmpty do begin
    writeln(data.Top);
    data.Pop;
  end;

  data.Destroy;
end.
