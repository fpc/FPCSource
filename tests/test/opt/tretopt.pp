{$mode objfpc}

type
  pshortstring=^shortstring;

  tr = record
    a,b,c,d,e: shortstring;
  end;

  ta = array[0..5] of shortstring;

  tc = record
    p: pointer;
  end;

var
  p: pointer;


function f1(p: pchar): tr;
begin
  fillchar(result,sizeof(tr),0);
  if (p^<>'x') then
    halt(1);
  f1.a:=p^;
end;


function f2(var s: shortstring): tr;
begin
  fillchar(result,sizeof(tr),0);
  if (s<>'x') then
    halt(2);
  f2.a:=s;
end;


function f3(const s: shortstring): tr;
begin
  fillchar(result,sizeof(tr),0);
  if (s<>'x') then
    halt(3);
  f3.a:=s;
end;


function f4(const t: tr): tr;
begin
  fillchar(result,sizeof(tr),0);
  if (t.a<>'x') then
    halt(4);
  f4:=t;
end;



function f5(p: pchar): ta;
begin
  fillchar(result,sizeof(result),0);
  if (p^<>'x') then
    halt(5);
  result[3]:=p^;
end;


function f6(var s: shortstring): ta;
begin
  fillchar(result,sizeof(result),0);
  if (s<>'x') then
    halt(6);
  result[3]:=s;
end;


function f7(const s: shortstring): ta;
begin
  fillchar(result,sizeof(result),0);
  if (s<>'x') then
    halt(7);
  result[3]:=s;
end;


function f8(const t: ta): ta;
begin
  fillchar(result,sizeof(result),0);
  if (t[3]<>'x') then
    halt(8);
  result:=t;
end;


procedure temp;
begin
  if (pshortstring(p)^<>'x') then
    halt(9);
end;

function f9: tr;
begin
  fillchar(result,sizeof(result),0);
  temp;
  result.a:='x';
end;

procedure testrec;
var
  t: tr;
begin
  t.a:='x';
  t:=f1(@t.a[1]);
  t:=f2(t.a);
  t:=f3(t.a);
  t:=f4(t);
  p:=@t.a;
  t:=f9;
end;


procedure testarr;
var
  t: ta;
begin
  t[3]:='x';
  t:=f5(@t[3][1]);
  t:=f6(t[3]);
  t:=f7(t[3]);
  t:=f8(t);
end;

begin
  testrec;
  testarr;
end.
