program ttrig;

{$modeswitch exceptions}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define writeln:=JLSystem.fout.println}

procedure do_error(i : longint);
  begin
//    writeln('Error near ',i);
    raise JLException.create('Error near '+UnicodeString(JLInteger.valueOf(i).toString));
  end;

var
  s0,s1,s2 : single;


begin
  writeln('--- Testing single functions ---');

  // 0.0
  s0:=0.0;

  s1:=sin(s0);
  if s1<>0.0 then
    do_error(1);

  s1:=cos(s0);
  if s1<>1.0 then
    do_error(2);

  s1:=arctan(s0);
  if s1<>0.0 then
    do_error(3);

  // pi/2
  s2:=pi/2;

  s1:=sin(s2);
  if s1<>1.0 then
    do_error(100);

  s1:=cos(s2);
  { with single precision, the result is -4.371138829E-08 }
  if abs(s1-0.0)>4.371138829E-08 then
    do_error(101);
end.
