{%OPT=-glh}
program tautom;

type  wstr_varnt_record=record
        s:widestring;
        v:variant;
      end;

      wstr_varnt_object=object
        s:wstr_varnt_record;
      end;

      wstr_array1=array[0..99] of wstr_varnt_record;
      wstr_array2=array[0..99] of wstr_varnt_object;


procedure do_test;

var a,b:wstr_array1;
    c,d:wstr_array2;
    i:0..99;


begin
  for i:=low(a) to high(a) do
    begin
      a[i].s:='Ninja';
      a[i].v:='Samurai';
    end;

  b:=a;

  for i:=low(a) to high(a) do
    begin
      if a[i].s<>'Ninja' then
        halt(255);
      if b[i].s<>'Ninja' then
        halt(255);
      if a[i].v<>'Samurai' then
        halt(255);
      if b[i].v<>'Samurai' then
        halt(255);
    end;

  for i:=0 to 99 do
    begin
      c[i].s.s:=a[i].s;
      c[i].s.v:=a[i].v;
    end;

  d:=c;
  for i:=low(d) to high(d) do
    begin
      if c[i].s.s<>'Ninja' then
        halt(255);
      if d[i].s.s<>'Ninja' then
        halt(255);
      if c[i].s.v<>'Samurai' then
        halt(255);
      if d[i].s.v<>'Samurai' then
        halt(255);
    end;
end;


var before,after:sizeuint;

begin
  with getfpcheapstatus do
    before:=currheapused;
  writeln('Used heap before ',before);

  do_test;

  with getfpcheapstatus do
    after:=currheapused;
  writeln('Used heap after ',after);
  if before<>after then
    exitcode:=255;
end.
