{$mode objfpc}
{$h+}
{$codepage utf8}
program defaulting;


var
  a: integer = 0;
  b: integer = default(integer);

function c (d: integer = 0): integer;
  begin
    result := d;
  end;

procedure g (h: integer = 0);
  var
    i: integer = default(integer);
  begin
    writeln(h, i);
    if h<>0 then
      halt(1);
    if i<>0 then
      halt(2);
  end;

procedure j (k: integer = default(integer));
  var
    l: integer = default(integer);
  begin
    writeln(k, l);
    if k<>0 then
      halt(3);
    if l<>0 then
      halt(4);
  end;

function e (f: integer = default(integer)): integer;
  begin
    result := f;
  end;

function m (n: string = default(string)): string;
  var
    o: string = '1';
    p: string = default(string);
    q: string = '';
    r: string = '2';
  begin
    result := n + o + p + q + r;
  end;

begin
  writeln(a, b, c);
  if a<>0 then
    halt(5);
  if b<>0 then
    halt(6);
  if c<>0 then
    halt(7);
  g;
  j;
  writeln(m);
  if m<>'12' then
    halt(8);
end.

