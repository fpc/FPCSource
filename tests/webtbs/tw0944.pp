{ %CPU=i386 }
{$ifdef TP}
{$N+}
{$endif TP}
PROGRAM fadd_bug;
VAR x,y,z,t: double;
BEGIN
x:=4.5;
y:=5.5;
{$ifndef TP}
{$asmmode intel}
{$endif TP}
asm
  fld x
  fld y
  fadd
  fstp z
end;
t:=x+y;
if (z<>10.0) or (z<>t) then
  begin
    Writeln('Error in FADD handling');
    Halt(1);
  end
else
  Writeln('FADD assembler instruction works');
END.
