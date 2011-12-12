program tvarpara;

{$mode objfpc}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

procedure test(var c: char);
begin
  if c<>'a' then
    halt(1);
  c:='b';
end;

procedure test(var c: widechar);
begin
  if c<>'a' then
    halt(2);
  c:='b';
end;

procedure test(var i: int64);
begin
end;

var
  l: longint;
function f: longint;
begin
  result:=l;
  inc(l);
end;

var
  c: char;
  w: widechar;
  a: ansistring;
  u: unicodestring;
  s: shortstring;
begin
  c:='a';
  test(c);
  if c<>'b' then
    halt(3);
  a:='abc';
  test(a[1]);
  if a<>'bbc' then
    begin
      u:=a;
      jlsystem.fout.println(length(a));
      jlsystem.fout.println(length(u));
      jlsystem.fout.println(a=u);
      jlsystem.fout.println(unicodestring(a));
      jlsystem.fout.println(unicodestring(ansistringclass(a).toString));
      halt(4);
    end;
  s:='cba';
  test(s[3]);
  if s<>'cbb' then
    begin
      jlsystem.fout.println(unicodestring(s));
      halt(5);
    end;
  w:='a';
  test(w);
  if w<>'b' then
   halt(6);
  u:='bac';
  l:=2;
  test(u[f]);
  if u<>'bbc' then
    halt(7);
  if l<>3 then
    halt(8);
end.
