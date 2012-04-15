{$Q-}
{$R-}

// tests whether the rol/ror operations properly mask out the shift count
procedure do_error(i : integer);
  begin
    writeln('Error: ',i);
    halt(1);
  end;

var
  b1 : byte;
  w1 : word;
  d1 : dword;
  q1 : qword;
begin
  b1:=rorbyte(2,15);
  if b1<>4 then
    do_error(1000);

  w1:=rorword(1,29);
  if w1<>$8 then
    do_error(1001);

  d1:=rordword($400,60);
  if d1<>$4000 then
    do_error(1002);

  q1:=rorqword($80000000000,125);
  if q1<>$400000000000 then
    do_error(1003);

  b1 := rolbyte($81,14);
  if (b1 <> $60) then
    do_error(2000);

  w1:=rolword($8001,22);
  if w1<>$60 then
    do_error(2001);

  d1:=roldword($80000001,38);
  if d1<>$60 then
    do_error(2002);

  q1:=rolqword(qword($8000000000000001),70);
  if q1<>$60 then
    do_error(2003);

  writeln('ok');
end.
