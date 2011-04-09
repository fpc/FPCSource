uses gvector;

type TVectorlli = specialize TVector<longint>;

var Buffer:TVectorlli; i:longint;

begin
  Buffer := TVectorlli.Create;
  {Push 5 elements at the end of array}
  for i:=1 to 5 do
    Buffer.PushBack(i);
  {change 3rd element to 47}
  Buffer[2] := 47;
  {pop last element}
  Buffer.PopBack;
  {print all elements}
  for i:=0 to Buffer.Size-1 do
    writeln(Buffer[i]);

  Buffer.Destroy;
end.
