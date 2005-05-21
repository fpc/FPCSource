Program Test;
{$X-}

Function TestFunc : Boolean;
var b : Boolean;
begin
  TestFunc := True;
  b := True;
  if b then
  begin
    exit;
  end;
end;

begin
  writeln(3 xor 1);
  if TestFunc then
  begin
    writeln('Yo');
  end;
end.
