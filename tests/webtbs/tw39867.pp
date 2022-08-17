uses 
  math;
  
begin
  writeln(tanh(-354));
  if tanh(-354)<>-1 then
    halt(1);
  writeln(tanh(-355));
  if tanh(-355)<>-1 then
    halt(1);
end.
