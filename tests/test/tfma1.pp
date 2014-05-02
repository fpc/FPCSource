{ %CPU=i386,x86_64 }
{ %OPT=-Cfavx2 -Cpcoreavx2 }
uses
  cpu;
var
  d0,d1,d2,d3 : double;
  s0,s1,s2,s3 : single;

procedure testsingle;
  var
    l0,l1,l2,l3 : single;
  begin
    l1:=2;
    l2:=3;
    l3:=4;
    s0:=0;

    l0:=fma(l1,l2,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(l1+1.0,l2,l3);
    writeln(l0);
    if l0<>13.0 then
      halt(1);

    l0:=fma(l1,l1+1.0,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(s1,l2,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(l1,s2,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(l1,l2,s3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(s1,s2,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(s1,l2,s3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(l1,s2,s3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    { first operand negative }
    l0:=fma(-l1,l2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-(l1+1.0),l2,l3);
    writeln(l0);
    if l0<>-5.0 then
      halt(1);

    l0:=fma(-l1,l1+1.0,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-s1,l2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-l1,s2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-l1,l2,s3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-s1,s2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-s1,l2,s3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-l1,s2,s3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    { second operand negative }
    l0:=fma(l1,-l2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(l1+1.0,-l2,l3);
    writeln(l0);
    if l0<>-5.0 then
      halt(1);

    l0:=fma(l1,-(l1+1.0),l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(s1,-l2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(l1,-s2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(l1,-l2,s3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(s1,-s2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(s1,-l2,s3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(l1,-s2,s3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    { third operand negative }
    l0:=fma(l1,l2,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(l1+1.0,l2,-l3);
    writeln(l0);
    if l0<>5.0 then
      halt(1);

    l0:=fma(l1,l1+1.0,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(s1,l2,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(l1,s2,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(l1,l2,-s3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(s1,s2,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(s1,l2,-s3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(l1,s2,-s3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    { first and third operand negative }
    l0:=fma(-l1,l2,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-(l1+1.0),l2,-l3);
    writeln(l0);
    if l0<>-13.0 then
      halt(1);

    l0:=fma(-l1,l1+1.0,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-s1,l2,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-l1,s2,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-l1,l2,-s3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-s1,s2,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-s1,l2,-s3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-l1,s2,-s3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);
  end;


procedure testdouble;
  var
    l0,l1,l2,l3 : double;
  begin
    l1:=2;
    l2:=3;
    l3:=4;
    d0:=0;

    l0:=fma(l1,l2,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(l1+1.0,l2,l3);
    writeln(l0);
    if l0<>13.0 then
      halt(1);

    l0:=fma(l1,l1+1.0,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(d1,l2,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(l1,d2,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(l1,l2,d3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(d1,d2,l3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(d1,l2,d3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    l0:=fma(l1,d2,d3);
    writeln(l0);
    if l0<>10.0 then
      halt(1);

    { first operand negative }
    l0:=fma(-l1,l2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-(l1+1.0),l2,l3);
    writeln(l0);
    if l0<>-5.0 then
      halt(1);

    l0:=fma(-l1,l1+1.0,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-d1,l2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-l1,d2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-l1,l2,d3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-d1,d2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-d1,l2,d3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(-l1,d2,d3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    { second operand negative }
    l0:=fma(l1,-l2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(l1+1.0,-l2,l3);
    writeln(l0);
    if l0<>-5.0 then
      halt(1);

    l0:=fma(l1,-(l1+1.0),l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(d1,-l2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(l1,-d2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(l1,-l2,d3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(d1,-d2,l3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(d1,-l2,d3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    l0:=fma(l1,-d2,d3);
    writeln(l0);
    if l0<>-2.0 then
      halt(1);

    { third operand negative }
    l0:=fma(l1,l2,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(l1+1.0,l2,-l3);
    writeln(l0);
    if l0<>5.0 then
      halt(1);

    l0:=fma(l1,l1+1.0,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(d1,l2,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(l1,d2,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(l1,l2,-d3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(d1,d2,-l3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(d1,l2,-d3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    l0:=fma(l1,d2,-d3);
    writeln(l0);
    if l0<>2.0 then
      halt(1);

    { first and third operand negative }
    l0:=fma(-l1,l2,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-(l1+1.0),l2,-l3);
    writeln(l0);
    if l0<>-13.0 then
      halt(1);

    l0:=fma(-l1,l1+1.0,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-d1,l2,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-l1,d2,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-l1,l2,-d3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-d1,d2,-l3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-d1,l2,-d3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);

    l0:=fma(-l1,d2,-d3);
    writeln(l0);
    if l0<>-10.0 then
      halt(1);
  end;

begin
  if AVXSupport and FMASupport then
    begin
      d1:=2;
      d2:=3;
      d3:=4;
      d0:=fma(d1,d2,d3);
      writeln(d0);
      if d0<>10.0 then
        halt(1);

      s1:=2;
      s2:=3;
      s3:=4;
      s0:=fma(s1,s2,s3);
      writeln(s0);
      if s0<>10.0 then
        halt(1);

      testsingle;
      testdouble;

      writeln('ok');
    end
  else
    writeln('Skipped because not supported by the CPU');
end.
