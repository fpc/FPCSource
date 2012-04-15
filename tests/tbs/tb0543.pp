{$R-}
{$Q-}

procedure check(l: longint; v,c: int64);
begin
  if (v<>c) then
    begin
      writeln('error near ',l);
      halt(l);
    end;
end;

var
  l1,l2,l3: longint;
  c1,c2,c3: cardinal;
  i: int64;
begin
  l1:=low(longint);
  l2:=-2;
  c1:=$80000000;
  c2:=cardinal(-2);


  l3:=$80000000 div l2;
  writeln(l3);
  check(1,l3,-1073741824);
  c3:=$80000000 div l2;
  writeln(c3);
  check(2,c3,3221225472);
  i:=$80000000 div l2;
  writeln(i);
  check(3,i,-1073741824);

  l3:=c1 div -2;
  writeln(l3);
  check(4,l3,-1073741824);
  c3:=c1 div -2;
  writeln(c3);
  check(5,c3,3221225472);
  i:=c1 div -2;
  writeln(i);
  check(6,i,-1073741824);

  l3:=c1 div l2;
  writeln(l3);
  check(7,l3,-1073741824);
  c3:=c1 div l2;
  writeln(c3);
  check(8,c3,3221225472);
  i:=c1 div l2;
  writeln(i);
  check(9,i,-1073741824);


  l3:=l1 div c2;
  writeln(l3);
  check(10,l3,0);
  c3:=l1 div c2;
  check(11,c3,0);
  writeln(c3);
  i:=l1 div c2;
  writeln(i);
  check(12,i,0);

  l3:=l1 div cardinal(-2);
  writeln(l3);
  check(13,l3,0);
  c3:=l1 div cardinal(-2);
  writeln(c3);
  check(14,c3,0);
  i:=l1 div cardinal(-2);
  writeln(i);
  check(15,i,0);

  l3:=low(longint) div c2;
  writeln(l3);
  check(16,l3,0);
  c3:=low(longint) div c2;
  writeln(c3);
  check(17,c3,0);
  i:=low(longint) div c2;
  writeln(i);
  check(18,i,0);

end.

