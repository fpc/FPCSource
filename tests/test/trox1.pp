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
  b1:=1;
  b2:=3;
  b1:=ror(b1);
  b1:=ror(b1,2);
  b1:=ror(b1,b2);
  if b1<>4 then
    do_error(1000);

  w1:=1;
  b2:=3;
  w1:=ror(w1);
  w1:=ror(w1,2);
  w1:=ror(w1,b2);
  if w1<>$400 then
    do_error(1001);

  d1:=1;
  b2:=3;
  d1:=ror(d1);
  d1:=ror(d1,2);
  d1:=ror(d1,b2);
  if d1<>$4000000 then
    do_error(1002);

  q1:=1;
  b2:=3;
  q1:=ror(q1);
  q1:=ror(q1,2);
  q1:=ror(q1,b2);
  if q1<>$400000000000000 then
    do_error(1003);

  b1:=1;
  b2:=3;
  b1:=rol(b1);
  b1:=rol(b1,2);
  b1:=rol(b1,b2);
  if b1<>$40 then
    do_error(2000);

  w1:=$8001;
  b2:=3;
  w1:=rol(w1);
  w1:=rol(w1,2);
  w1:=rol(w1,b2);
  if w1<>$60 then
    do_error(2001);

  d1:=$80000001;
  b2:=3;
  d1:=rol(d1);
  d1:=rol(d1,2);
  d1:=rol(d1,b2);
  if d1<>$60 then
    do_error(2002);

  q1:=$8000000000000001;
  b2:=3;
  q1:=rol(q1);
  q1:=rol(q1,2);
  q1:=rol(q1,b2);
  if q1<>$60 then
    do_error(2003);

  writeln('ok');
end.
