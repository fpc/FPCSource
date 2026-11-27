{$mode objfpc}
program tset9;
uses
  sysutils;
type
  t1000 = set of 0..1000;
  tbyteset = set of byte;
  t010 = 0..10;
var
  s, s2 : t1000;
  bs : tbyteset;
  i, j, k : longint;
  w : word;
  b : byte;
  sr : t010;
  e : longint;
  cnt : longint;

procedure fail(n : longint);
  begin
    writeln('fail ',n);
    halt(n);
  end;

function pass(const x : t1000) : boolean;
  begin
    result:=(300 in x) and (310 in x) and not(44 in x);
  end;

begin
  i:=300; j:=310; k:=999;
  s:=[i,j];
  if not((300 in s) and (310 in s)) or (44 in s) or (54 in s) then fail(1);
  s:=[i,k];
  if not((300 in s) and (999 in s)) or (44 in s) or (231 in s) then fail(2);
  s:=[i..j];
  if not((300 in s) and (305 in s) and (310 in s)) or (299 in s) or (311 in s) or (44 in s) then fail(3);
  if not pass([i,j]) then fail(4);
  s:=[];
  s:=s+[i];
  if not(300 in s) or (44 in s) then fail(5);
  s:=s+[i..j];
  if not(310 in s) then fail(6);
  s:=s-[j];
  if (310 in s) or not(309 in s) then fail(7);
  s:=s*[i,j];
  if s<>[300] then fail(8);
  { in operator }
  w:=300;
  if not(w in [i,j]) then fail(9);
  if not(i in [k,j,i]) then fail(10);
  { constructors without context }
  s2:=[i]+[j];
  if s2<>[300,310] then fail(13);
  cnt:=0;
  for e in [i,j] do
    begin
      if (e<>300) and (e<>310) then fail(14);
      inc(cnt);
    end;
  if cnt<>2 then fail(15);
  { mixed element types }
  b:=5;
  s:=[b,i];
  if s<>[5,300] then fail(16);
  s:=[300,b];
  if s<>[5,300] then fail(17);
  sr:=7;
  s:=[sr,i];
  if s<>[7,300] then fail(18);
  { comparisons }
  if [i]=[44] then fail(19);
  if not([i]<=s) then fail(20);
  { high end }
  i:=1000;
  s:=[i];
  if not(1000 in s) then fail(21);
  { byte sets keep their behaviour }
  b:=3; i:=3;
  bs:=[b,i];
  if bs<>[3] then fail(22);
  bs:=bs+[i+1];
  if bs<>[3,4] then fail(23);
  i:=995; j:=1000;
  s:=[i..j];
  if not((995 in s) and (1000 in s)) or (994 in s) then fail(28);
  { empty range, the other operand must be copied }
  s2:=[1,2];
  i:=5; j:=4;
  s:=s2+[i..j];
  if s<>[1,2] then fail(30);
  { no context: up to 65535 }
  i:=65535; j:=40000;
  if not(i in [i,j]) or not(j in [j]) then fail(31);
  { in place addition }
  s:=[];
  i:=300;
  s:=s+[i];
  i:=1000;
  s:=s+[i];
  if s<>[300,1000] then fail(32);
  { range constructors get the set type from the context }
  i:=300; j:=302;
  s:=[1]+[i..j];
  if s<>[1,300..302] then fail(35);
  s:=[i..j]*s;
  if s<>[300..302] then fail(36);
  s:=s-[i..i];
  if s<>[301,302] then fail(37);
  w:=301;
  if not(w in [i..j]) then fail(38);
  b:=44;
  i:=40; j:=50;
  if not(b in [i..j]) then fail(39);
  bs:=[];
  bs:=bs+[i..j];
  if bs<>[40..50] then fail(40);
  cnt:=0;
  for e in [i..j] do
    inc(cnt);
  if cnt<>11 then fail(41);
  i:=303; j:=305;
  if not pass([300..302,i..j,310]) then fail(42);
  { range checking }
{$push}{$R+}
  i:=1001;
  try
    s:=[i];
    fail(33);
  except
    on ERangeError do
      ;
  end;
  i:=256;
  try
    bs:=[i];
    fail(34);
  except
    on ERangeError do
      ;
  end;
{$pop}
  writeln('ok');
end.
