{$mode macpas}

{$r-}
{$q-}

procedure testlongintrot;
const
  haltoffset = 0;
var
  l : longint;
begin
  l := 1;
  l := brotl(l,1);
  if (l <> 2) then
    halt(1+haltoffset);
  l := brotr(l,1);
  if (l <> 1) then
    halt(2+haltoffset);

  l := longint($80000001);
  l := brotl(l,2);
  if (l <> 6) then
    halt(3+haltoffset);
  l := brotr(l,3);
  if (l <> longint($c0000000)) then
    halt(4+haltoffset);

  l := brotr(l,2);
  // "longint($c0000000) shr 2" is evaluated using 64 bit :/
  if (l <> (longint(cardinal($c0000000) shr 2))) then
    halt(5+haltoffset);
end;


procedure testcardinalrot;
const
  haltoffset = 5;
var
  l : cardinal;
begin
  l := 1;
  l := brotl(l,1);
  if (l <> 2) then
    halt(1+haltoffset);
  l := brotr(l,1);
  if (l <> 1) then
    halt(2+haltoffset);

  l := $80000001;
  l := brotl(l,2);
  if (l <> 6) then
    halt(3+haltoffset);
  l := brotr(l,3);
  if (l <> $c0000000) then
    halt(4+haltoffset);

  l := brotr(l,2);
  if (l <> (cardinal($c0000000) shr 2)) then
    halt(5+haltoffset);
end;


procedure testint64rot;
const
  haltoffset = 10;
var
  l : int64;
begin
  l := 1;
  l := brotl(l,1);
  if (l <> 2) then
    halt(1+haltoffset);
  l := brotr(l,1);
  if (l <> 1) then
    halt(2+haltoffset);

  l := $80000001;
  l := brotl(l,2);
  if (l <> $200000004) then
    halt(3+haltoffset);
  l := brotr(l,3);
  if (l <> int64($8000000040000000)) then
    halt(4+haltoffset);

  l := brotr(l,2);
  if (l <> (int64($8000000040000000) shr 2)) then
    halt(5+haltoffset);
end;


procedure testqwordrot;
const
  haltoffset = 15;
var
  l : qword;
begin
  l := 1;
  l := brotl(l,1);
  if (l <> 2) then
    halt(1+haltoffset);
  l := brotr(l,1);
  if (l <> 1) then
    halt(2+haltoffset);

  l := $80000001;
  l := brotl(l,2);
  if (l <> $200000004) then
    halt(3+haltoffset);
  l := brotr(l,3);
  if (l <> qword($8000000040000000)) then
    halt(4+haltoffset);

  l := brotr(l,2);
  if (l <> (qword($8000000040000000) shr 2)) then
    halt(5+haltoffset);
end;


procedure testlongintnot;
const
  haltoffset = 20;
var
  l, j : longint;
begin
  l := low(longint);
  for j := 1 to (maxlongint div 13579) do
    begin
      if not(l) <> bnot(l) then
        halt(haltoffset+1);
      inc(l,13579*2);
    end;
end;


procedure testcardinalnot;
const
  haltoffset = 21;
var
  l, j : cardinal;
begin
  l := 0;
  for j := 1 to (maxlongint div 13579) do
    begin
      if not(l) <> bnot(l) then
        halt(haltoffset+1);
      inc(l,13579*2);
    end;
end;


procedure testint64not;
const
  haltoffset = 22;
var
  l, j : int64;
begin
  l := low(int64);
  j := 1;
  repeat
    if not(l) <> bnot(l) then
      halt(haltoffset+1);
    inc(l,int64(13579)*high(longint)*2);
    inc(j);
  until (j = (high(int64) div (int64(13579) * high(longint))));
end;


procedure testqwordnot;
const
  haltoffset = 22;
var
  l, j : qword;
begin
  l := 0;
  j := 1;
  repeat
    if not(l) <> bnot(l) then
      halt(haltoffset+1);
    inc(l,int64(13579)*high(longint)*2);
    inc(j);
  until (j = (high(int64) div (int64(13579) * high(longint))));
end;


begin
  testlongintrot;
  testcardinalrot;
  testint64rot;
  testqwordrot;

  testlongintnot;
  testcardinalnot;
  testint64not;
  testqwordnot;
end.