uses gdeque;

type TDequelli = specialize TDeque<longint>;

var Buffer:TDequelli; i:longint;

begin
  Buffer := TDequelli.Create;
  {Push 5 elements at the end of array}
  for i:=1 to 5 do
    Buffer.PushBack(i);
  {change 3rd element to 47}
  Buffer[2] := 47;
  {pop last element}
  Buffer.PopBack;
  {push 3 element to front}
  for i:=1 to 3 do
    Buffer.PushFront(i*10);
  {print all elements}
  for i:=0 to Buffer.Size-1 do
    writeln(Buffer[i]);

  Buffer.Destroy;
end.
