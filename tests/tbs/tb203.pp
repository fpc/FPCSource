{ Old file: tbs0233.pp }
{ Problem with enum sets in args                       OK 0.99.11 (PFV) }

program except_test;

type byteset = set of byte;
     enumset = set of (zero,one,two,three);

function test(s : byteset) : boolean;
begin
  test:=false;
  if 0 in s then
    begin
       Writeln('Contains zero !');
       test:=true;
    end;
end;

function testenum(s : enumset) : boolean;
begin
  testenum:=false;

  if zero in s then
    begin
       Writeln('Contains zero !');
       testenum:=true;
    end;
end;

begin
  if test([1..5,8]) then halt(1);
  if not test([0,8,15]) then halt(1);
  if not testenum([zero,two]) then halt(1);
end.
