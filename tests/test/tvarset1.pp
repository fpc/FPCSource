{$packset 1}
type
  tset8 = set of 0..63;

procedure do_error(l : longint);

  begin
     writeln('Error near number ',l);
     halt(1);
  end;

var
  set8_1,set8_2,set8_3 : tset8;
  i,j : longint;
begin
  if sizeof(tset8)<>8 then
    do_error(1);

  set8_1:=[42];
  set8_2:=set8_1;
  if set8_1<>set8_2 then
    do_error(2);

  set8_2:=[41,42];
  if set8_1=set8_2 then
    do_error(3);

  if set8_1>=set8_2 then
    do_error(4);

  set8_1:=[21..50];
  if set8_1<=set8_2 then
    do_error(5);

  set8_1:=[1];
  set8_2:=[2];
  set8_3:=set8_1+set8_2;
  if set8_3<>[1,2] then
    do_error(6);

  i:=55;
  set8_1:=[i];
  if set8_1<>[55] then
    do_error(7);

  i:=55;
  j:=60;
  set8_1:=[i,j];
  if set8_1<>[55,60] then
    do_error(8);

  i:=55;
  j:=60;
  set8_1:=[i..j];
  if set8_1<>[55..60] then
    do_error(9);
end.
