program bug1_empty;
{$apptype console}
var
  w: WideString;
begin
  w := 'toto';
  if length(w) <> 4 then
    Halt(1);
  //writeln(ErrOutput, 'length=', length(w));
end.
