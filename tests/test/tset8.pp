program tset8;

{$packset 1}

procedure CheckIn(C: Char);
begin
  if (C < 'a') or (C > 'z') then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

procedure CheckOut(C: Char);
begin
  if (C >= 'a') and (C <= 'z') then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

var
  C: Char;
begin
  for C := #0 to #255 do
  begin
    if C in ['a'..'z'] then
      CheckIn(C)
    else
      CheckOut(C);
  end;
end.
