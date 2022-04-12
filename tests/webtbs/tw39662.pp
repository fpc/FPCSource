uses Generics.Collections;
  
var
  Queue: specialize TQueue<Integer>;
  I, J: Integer;
begin
  Queue := specialize TQueue<Integer>.Create;
 
  for I := 0 to 15 do
    Queue.Enqueue(I);
  for I := 1 to 10000 do
    begin
      for J := 1 to 15 do
        Queue.Dequeue;
      for J := 1 to 15 do
        Queue.Enqueue(J);
    end;
 
  WriteLn(Queue.Capacity);
  { avoid too large capacities }
  if Queue.Capacity>64 then
    halt(1);
  Queue.Free;
end.
