{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3093 }
{ Submitted by "Dail Singleton" on  2004-05-08 }
{ e-mail: fdails@hal-pc.org }
{$asmmode intel}
var
  d1,d2,d3 : double;
begin
  d1:=1;
  d2:=2;
  asm
    fld d1
    fld d2
    fdivr st(1),st
    fst d3
  end;
  if d3<>2 then
    begin
      writeln(d3);
      halt(1);
    end;
end.
