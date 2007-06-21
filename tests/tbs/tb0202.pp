{ Old file: tbs0238.pp }
{ Internal error 432645 (from Frank MCCormick, mailinglist 24/2) OK 0.99.11 (PM) }

program test1;

           {compiles under TPC - PPC386 gives internal error}

Type str1=string[160];

var
   fileof  :file of str1;
   lol   :array[1..8] of str1;
   nu,n:integer;
   i,tt    :str1;
   ul   :text;
   a: str1;


procedure test;


begin
   for nu:=1 to 8 do read(fileof,lol[nu]);
   writeln('File contents');
   for nu:=4 to 8 do writeln(lol[nu]);
end;


begin
  assign(fileof,'tbs0238.tmp');
  rewrite(fileof);
  a:='dummy string !!';
  for nu:=1 to 8 do write(fileof,a);
  close(fileof);
  reset(fileof);
  test;
  close(fileof);
  erase(fileof);
end.
