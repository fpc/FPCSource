{ Old file: tbs0290.pp }
{ problem with storing hex numbers in integers }

{ $R+ would give compile time errors }
{$R-}

var i,j : integer;

begin
  { the following line gives a warning and $ffff is changed to $7fff!}
  i := $ffff;
  if i <> $ffff then
    begin
      Writeln('i:=$ffff loads ',i,'$7fff if i is integer !');
    end;
  j := 65535;
  if j <> 65535 then
    begin
      Writeln('j:=65535 loads ',j,' if j is integer !');
    end;
  if ($ffff=65535) and (i<>j) then
    begin
      Writeln('i and j are different !!!');
      Halt(1);
    end;
end.
