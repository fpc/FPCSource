{ Old file: tbs0046.pp }
{  problems with sets with values over 128 due to sign extension (already fixed ) but also for SET_IN_BYTE }

program test;

{$R-}

type byteset = set of byte;
     bl = record i,j : longint;
          end;
const set1 : byteset = [1,50,220];
      set2 : byteset = [55];
var i : longint;
    b : bl;

    function bi : longint;

    begin
       bi:=b.i;
    end;

begin
set1:=set1+set2;
writeln('set 1 = [1,50,55,220]');
i:=50;
if i in set1 then
  writeln(i,' is in set1');
i:=220;
if i in set1 then
  writeln(i,' is in set1');
i:=$100+220;
if i in set1 then
  writeln(i,' is in set1');
i:=-35;
if i in set1 then
  writeln(i,' is in set1');
b.i:=50;
i:=$100+220;
if i in [50,220] then
  writeln(i,' is in [50,220]');
if Bi in [50,220] then
  writeln(b.i,' is in [50,220]');
b.i:=220;
if bi in [50,220] then
  writeln(b.i,' is in [50,220]');
B.i:=-36;
if bi in [50,220] then
  writeln(B.i,' is in [50,220]');
end.
