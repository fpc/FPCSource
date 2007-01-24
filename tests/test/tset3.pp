{$packset 1}
type
  tmini = 0..7;
  tminiset = set of tmini;


procedure do_error(w : word);
  begin
    writeln('Error: ',w);
    halt(1);
  end;

var
  s1,s2,s3 : tminiset;
  b : byte;
  m : tmini;
begin
  s1:=[];
  if s1<>[] then
    do_error(1);

  s1:=[1];
  if s1<>[1] then
    do_error(2);

  s2:=[2,3];
  if s2<>[2,3] then
    do_error(3);

  b:=6;
  s3:=[b,7];
  if s3<>[6,7] then
    do_error(4);

  s1:=s1+s2;
  if s1<>[1..3] then
    do_error(5);

  s2:=s1;

  if not(s1=s2) then
    do_error(6);

  s3:=[4];

  include(s1,4);
  if s1<>[1..4] then
    do_error(7);

  s2:=s1;

  exclude(s1,4);
  if s1<>[1..3] then
    do_error(8);

  s2:=s2-s3;
  if s1<>s2 then
    do_error(9);

  b:=4;
  include(s1,b);
  if s1<>[1..4] then
    do_error(10);

  s2:=s2+[b];
  if s1<>s2 then
    do_error(11);

  s2:=s1;
  m:=3;
  s1:=s1-[m];
  exclude(s2,m);
  if s1<>s2 then
    do_error(12);

  writeln('ok');
end.
