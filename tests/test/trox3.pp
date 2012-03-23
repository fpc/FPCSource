{$Q-}
{$R-}

procedure do_error(i : integer);
  begin
    writeln('Error: ',i);
    halt(1);
  end;

var
  b1,b2 : byte;
  w1 : word;
  d1 : dword;
  q1 : qword;
begin
  b1:=rorbyte(rorbyte(rorbyte(1),2),3);
  if b1<>4 then
    do_error(1000);

  w1:=rorword(rorword(rorword(1),2),3);
  if w1<>$400 then
    do_error(1001);

  d1:=rordword(rordword(rordword(1),2),3);
  if d1<>$4000000 then
    do_error(1002);

  q1:=rorqword(rorqword(rorqword(1),2),3);
  if q1<>$400000000000000 then
    do_error(1003);

  b1:=rolbyte(rolbyte(rolbyte(1),2),3);
  if b1<>$40 then
    do_error(2000);

  w1:=rolword(rolword(rolword($8001),2),3);
  if w1<>$60 then
    do_error(2001);

  d1:=roldword(roldword(roldword($80000001),2),3);
  if d1<>$60 then
    do_error(2002);

  q1:=rolqword(rolqword(rolqword($8000000000000001),2),3);
  if q1<>$60 then
    do_error(2003);

  writeln('ok');
end.
