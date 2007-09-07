var
  l: longint;
  c: cardinal;
  i: int64;
  q: qword;

procedure ts1(const res1, res2, res3, res4: string);
var
  r: packed record
    s: string[1];
    b1,b2,b3,b4: byte;
  end;
begin
  with r do
    begin
      b1:=0;
      b2:=0;
      b3:=0;
      b4:=0;
      str(l,s);
      if (res1<>s) or
         (b1<>0) or
         (b2<>0) or
         (b3<>0) or
         (b4<>0) then
        halt(1);

      str(c,s);
      if (res2<>s) or
         (b1<>0) or
         (b2<>0) or
         (b3<>0) or
         (b4<>0) then
        halt(2);

      str(i,s);
      if (res3<>s) or
         (b1<>0) or
         (b2<>0) or
         (b3<>0) or
         (b4<>0) then
        halt(3);

      str(q,s);
      if (res4<>s) or
         (b1<>0) or
         (b2<>0) or
         (b3<>0) or
         (b4<>0) then
        halt(4);
    end;
end;



procedure ts3(const res1, res2, res3, res4: string);
var
  r: packed record
    s: string[3];
    b1,b2,b3,b4: byte;
  end;
begin
  with r do
    begin
      b1:=0;
      b2:=0;
      b3:=0;
      b4:=0;
      str(l,s);
      if (res1<>s) or
         (b1<>0) or
         (b2<>0) or
         (b3<>0) or
         (b4<>0) then
        halt(1);

      str(c,s);
      if (res2<>s) or
         (b1<>0) or
         (b2<>0) or
         (b3<>0) or
         (b4<>0) then
        halt(2);

      str(i,s);
      if (res3<>s) or
         (b1<>0) or
         (b2<>0) or
         (b3<>0) or
         (b4<>0) then
        halt(3);

      str(q,s);
      if (res4<>s) or
         (b1<>0) or
         (b2<>0) or
         (b3<>0) or
         (b4<>0) then
        halt(4);
    end;
end;


begin
  l:=high(longint);
  c:=high(cardinal);
  i:=high(int64);
  q:=high(qword);
  ts1('2','4','9','1');
  ts3('214','429','922','184');
  l:=low(longint)+1;
  c:=high(cardinal)-1;
  i:=low(int64)+1;
  q:=high(qword)-1;
  ts1('-','4','-','1');
  ts3('-21','429','-92','184');
end.
